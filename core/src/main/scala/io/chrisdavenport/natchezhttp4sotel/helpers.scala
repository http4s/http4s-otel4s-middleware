package io.chrisdavenport.natchezhttp4sotel

import org.typelevel.otel4s.trace.TracerProvider
import org.typelevel.otel4s.TextMapGetter

private[natchezhttp4sotel] object helpers {

  implicit val textMapGetter: TextMapGetter[org.http4s.Headers] = new TextMapGetter[org.http4s.Headers] {

    def get(carrier: org.http4s.Headers, key: String): Option[String] =
      carrier.get(org.typelevel.ci.CIString(key)).map(_.head.value)
    def keys(carrier: org.http4s.Headers): List[String] = carrier.headers.map(_.name.toString)
  }


  import org.typelevel.otel4s.trace.Tracer
  import cats.data.OptionT

  def liftImplicit[F[_]](tracer: Tracer[F]): Tracer[OptionT[F, *]] = new Tracer[OptionT[F, *]]{
    /** As seen from class $anon, the missing signatures are as follows.
    *  For convenience, these are usable as stub implementations.
    */
    def childScope[A]
    (parent: org.typelevel.otel4s.trace.SpanContext)
      (fa: cats.data.OptionT[F, A]): cats.data.OptionT[F, A] = ???
    def currentSpanContext:
    cats.data.OptionT[F, Option[org.typelevel.otel4s.trace.SpanContext]] = ???
    def joinOrRoot[A, C]
    (carrier: C)
      (fa: cats.data.OptionT[F, A])
        (implicit evidence$1: org.typelevel.otel4s.TextMapGetter[C]):
          cats.data.OptionT[F, A] = ???
    override def meta: org.typelevel.otel4s.trace.Tracer.Meta[cats.data.OptionT[F, *]] = ???
    def noopScope[A](fa: cats.data.OptionT[F, A]): cats.data.OptionT[F, A] = ???
    def rootScope[A](fa: cats.data.OptionT[F, A]): cats.data.OptionT[F, A] = ???
    override def spanBuilder(name: String):org.typelevel.otel4s.trace.SpanBuilder.Aux[cats.data.OptionT[F, *],
        org.typelevel.otel4s.trace.Span[cats.data.OptionT[F, *]]
      ] = ???
  }

  def provider[F[_]](t: TracerProvider[F]): TracerProvider[OptionT[F, *]] = new TracerProvider[OptionT[F, *]] {
    def tracer(name: String): org.typelevel.otel4s.trace.TracerBuilder[cats.data.OptionT[F,*]] = ???
  }







  import java.io.{OutputStream, FilterOutputStream, ByteArrayOutputStream, PrintStream}


  def listStackTrace(e: Throwable): List[String] = {
    e.getStackTrace().toList.map(
      element => element.toString()
    )
  }

  def printStackTrace(e: Throwable): String = {
    val baos = new ByteArrayOutputStream
    val fs   = new AnsiFilterStream(baos)
    val ps   = new PrintStream(fs, true, "UTF-8")
    e.printStackTrace(ps)
    ps.close
    fs.close
    baos.close
    new String(baos.toByteArray, "UTF-8")
  }

  /** Filter ANSI codes out of an OutputStream. */
  private class AnsiFilterStream(os: OutputStream) extends FilterOutputStream(os) {
    case class State(apply: Int => State)

    val S: State = State {
      case 27 => I0
      case _  => F
    }

    val F: State = State(_ => F)

    val T: State = State(_ => T)

    val I0: State = State {
      case '[' => I1
      case _   => F
    }

    val I1: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
              => I2
      case _   => F
    }

    val I2: State = State {
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
              => I2
      case ';' => I1
      case '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' |
          'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\'| ']' |
          '^' | '_' | '`' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' |
          'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '{' |
          '|' | '}' | '~'
              => T // end of ANSI escape
      case _   => F
    }

    // Strategy is, accumulate values as long as we're in a non-terminal state, then either discard
    // them if we reach T (which means we accumulated an ANSI escape sequence) or print them out if
    // we reach F.

    private var stack: List[Int] = Nil
    private var state: State     = S // Start

    override def write(n: Int): Unit =
      state.apply(n) match {

        case F =>
          stack.foldRight(())((c, _) => super.write(c))
          super.write(n)
          stack = Nil
          state = S

        case T =>
          stack = Nil
          state = S

        case s =>
          stack = n :: stack
          state = s

      }

  }
}