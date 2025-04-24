package ru.mipt.fp.utils

import scala.concurrent.duration.FiniteDuration

trait Timer[F[_]]:
  def sleep(duration: FiniteDuration): F[Unit]

object Timer:
  inline def apply[F[_]: Timer]: Timer[F] = summon[Timer[F]]
