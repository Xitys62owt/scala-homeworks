package ru.mipt.fp.masking

/** Тайпкласс для маскирования данных
  */
trait Masking[T]:
  def mask(t: T): T

  extension (t: T) def masked: T = mask(t)
