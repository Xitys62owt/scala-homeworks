package ru.mipt.fp.resilience

import cats.MonadError
import ru.mipt.fp.utils.Timer

import scala.concurrent.duration.FiniteDuration

import cats.syntax.flatMap._

trait Retry[F[_], E]:
  def retry[A](operation: F[A])(canRetry: E => Boolean)(count: Int, delay: FiniteDuration): F[A]

object Retry:
  private class Impl[F[_]: Timer, E](using
    MonadError[F, E]
  ) extends Retry[F, E]:
    override def retry[A](
      operation: F[A]
    )(canRetry: E => Boolean)(count: Int, delay: FiniteDuration): F[A] =
      MonadError[F, E].handleErrorWith(operation) { error =>
        if canRetry(error) && count > 0 then Timer[F].sleep(delay) >> retry(operation)(canRetry)(count - 1, delay)
        else MonadError[F, E].raiseError(error)
      }

  def apply[F[_]: Timer, E](using
    MonadError[F, E]
  ): Retry[F, E] = new Impl[F, E]
