package io.chrisdavenport.natchezhttp4sotel

import cats._
import cats.syntax.all._
import cats.effect.{Trace => _, _}
import natchez._
import munit.CatsEffectSuite
import cats.data.Kleisli
import org.http4s._
import org.http4s.client._

class ClientMiddewareTests extends TraceSuite {
  traceTest(
    "ClientMiddleware",
    new TraceTest {
      def program[F[_]: Async: Trace] = {
        val fakeClient = Client.fromHttpApp(HttpApp{(req: Request[F]) => req.body.compile.drain.as(Response[F](Status.Ok))})
        val tracedClient = ClientMiddleware.default[F].build(fakeClient)

        tracedClient.run(Request[F](Method.GET)).use{ resp =>
          val fk = resp.attributes.lookup(ClientMiddleware.Keys.spanKey[F]).get

          fk(Trace[F].put("mainUse" -> "test")) >>
          Trace[F].put("mainUseNoFk" -> "test") >>
          resp.body.compile.drain
        }
      }

      def expectedHistory = List(
        (Lineage.Root, NatchezCommand.CreateRootSpan("root", Kernel(Map()), Span.Options.Defaults)),
        (Lineage.Root, NatchezCommand.CreateSpan("Http Client - GET", None, Span.Options.Defaults)),
        (Lineage.Root / "Http Client - GET", NatchezCommand.Put(List(
          "span.kind" -> "client",
          "http.method" -> "GET",
          "http.url" -> "/",
          "http.target" -> "/",
          "http.host" -> "localhost"
        ))),
        (Lineage.Root / "Http Client - GET", NatchezCommand.AskKernel(Kernel(Map.empty))),
        (Lineage.Root / "Http Client - GET", NatchezCommand.Put(List("exit.case" -> "succeeded"))),
        (Lineage.Root / "Http Client - GET", NatchezCommand.Put(List(
          "http.status_code" -> 200,
          "http.flavor" -> "1.1"
        ))),
        (Lineage.Root / "Http Client - GET", NatchezCommand.Put(List("mainUse" -> "test"))),
        (Lineage.Root, NatchezCommand.Put(List("mainUseNoFk" -> "test"))),

        (Lineage.Root, NatchezCommand.ReleaseSpan("Http Client - GET")),

        // (Lineage.Root, NatchezCommand.CreateSpan("spanR", None, Span.Options.Defaults)),
        // (Lineage.Root, NatchezCommand.CreateSpan("span", None, Span.Options.Defaults)),
        // (Lineage.Root / "spanR", NatchezCommand.Put(List("question" -> "ultimate"))),
        // (Lineage.Root / "span", NatchezCommand.Put(List("answer" -> 42))),
        // (Lineage.Root, NatchezCommand.ReleaseSpan("span")),
        // (Lineage.Root, NatchezCommand.ReleaseSpan("spanR")),
        (Lineage.Root, NatchezCommand.ReleaseRootSpan("root"))
      )
    }

  )

}

trait InMemorySuite extends CatsEffectSuite {
  type Lineage = InMemory.Lineage
  val Lineage = InMemory.Lineage
  type NatchezCommand = InMemory.NatchezCommand
  val NatchezCommand = InMemory.NatchezCommand
}

trait TraceSuite extends InMemorySuite {
  val defaultRootName = "root"
  trait TraceTest {
    def program[F[_]: Async: Trace]: F[Unit]

    def expectedHistory: List[(Lineage, NatchezCommand)]
  }

  def traceTest(name: String, tt: TraceTest): Unit = {
    test(s"$name - Kleisli")(
      testTraceKleisli(tt.program[Kleisli[IO, Span[IO], *]](implicitly, _), tt.expectedHistory)
    )
    test(s"$name - IOLocal")(testTraceIoLocal(tt.program[IO](implicitly, _), tt.expectedHistory))
  }

  def testTraceKleisli(
      traceProgram: Trace[Kleisli[IO, Span[IO], *]] => Kleisli[IO, Span[IO], Unit],
      expectedHistory: List[(Lineage, NatchezCommand)]
  ): IO[Unit] = testTrace[Kleisli[IO, Span[IO], *]](
    traceProgram,
    root => IO.pure(Trace[Kleisli[IO, Span[IO], *]] -> (k => k.run(root))),
    expectedHistory
  )

  def testTraceIoLocal(
      traceProgram: Trace[IO] => IO[Unit],
      expectedHistory: List[(Lineage, NatchezCommand)]
  ): IO[Unit] = testTrace[IO](traceProgram, Trace.ioTrace(_).map(_ -> identity), expectedHistory)

  def testTrace[F[_]](
      traceProgram: Trace[F] => F[Unit],
      makeTraceAndResolver: Span[IO] => IO[(Trace[F], F[Unit] => IO[Unit])],
      expectedHistory: List[(Lineage, NatchezCommand)]
  ): IO[Unit] =
    InMemory.EntryPoint.create[IO].flatMap { ep =>
      val traced = ep.root(defaultRootName).use { r =>
        makeTraceAndResolver(r).flatMap { case (traceInstance, resolve) =>
          resolve(traceProgram(traceInstance))
        }
      }
      traced *> ep.ref.get.map { history =>
        assertEquals(history.toList, expectedHistory)
      }
    }
}