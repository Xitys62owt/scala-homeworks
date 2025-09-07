package ru.mipt.fp.resilience

import cats.MonadError
import ru.mipt.fp.utils.Cache

import scala.annotation.nowarn

/**
 * Интерфейс для фоллбэк-кэша
 */
trait FallbackCache[F[_], K, V]:
  def withFallback(key: K)(operation: K => F[V]): F[V]

object FallbackCache:
  @nowarn
  private class Impl[F[_], K, V, E](cache: Cache[F, K, V])(using monadError: MonadError[F, E])
    extends FallbackCache[F, K, V]:
    def withFallback(key: K)(operation: K => F[V]): F[V] = ???

  def apply[F[_], K, V, E](cache: Cache[F, K, V])(using monadError: MonadError[F, E]): FallbackCache[F, K, V] =
    new Impl(cache)
