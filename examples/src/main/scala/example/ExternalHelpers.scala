package example

import cats.effect.{IOLocal, LiftIO, MonadCancelThrow}
import cats.mtl.Local
import cats.syntax.functor._
import cats.{Applicative, Functor}

object ExternalHelpers {

  def local[F[_]: LiftIO: MonadCancelThrow, A](value: A): F[Local[F, A]] =
    LiftIO[F].liftIO(IOLocal(value)).map(localForIoLocal(_))

  def localForIoLocal[F[_]: MonadCancelThrow: LiftIO, E](
      ioLocal: IOLocal[E]
  ): Local[F, E] =
    new Local[F, E] {
      def applicative =
        Applicative[F]
      def ask[E2 >: E] =
        Functor[F].widen[E, E2](ioLocal.get.to[F])
      def local[A](fa: F[A])(f: E => E): F[A] =
        MonadCancelThrow[F].bracket(ioLocal.modify(e => (f(e), e)).to[F])(_ =>
          fa
        )(ioLocal.set(_).to[F])
    }
}