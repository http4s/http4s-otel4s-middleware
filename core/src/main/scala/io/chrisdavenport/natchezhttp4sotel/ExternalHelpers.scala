package io.chrisdavenport.natchezhttp4sotel

import cats.syntax.all._
import cats.effect._
import cats.mtl.Local
import cats._
import org.typelevel.vault.Vault

object ExternalHelpers {

  def localVault[F[_]: LiftIO: MonadCancelThrow]: F[Local[F, Vault]] = {
    LiftIO[F].liftIO(IOLocal(Vault.empty)).map(localForIoLocal(_))
  }

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