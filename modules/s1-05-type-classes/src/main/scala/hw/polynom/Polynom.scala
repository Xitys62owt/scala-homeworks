package hw.polynom

/**
 Структура, представляющая собой многочлен:
 a_n * x ^ n + a_(n-1) * x ^ (n - 1) + .... + a_1 * x + a_0
 где a_n, a_(n-1), ..., a_1, a_0 - элементы (коэффициенты многочлена) из множества A
 */
case class Polynom[A](coeffs: List[A]):

  /**
   Реализовать красивый toString, строка на выходе должна быть похожа на многочлен
   */
  override def toString: String =
    coeffs.zipWithIndex
      .collect {
        case (c, i) if c != 0 => s"${c}x^$i"
      }
      .reverse
      .mkString(" + ")

  /**
   Реализовать разность многочленов над полем F

   Например:
   Для поля вещественных чисел:
   (x^3 + 5x + 1) - (2x^5 + x^3 - 3x^2 - 1) === -2x^5 + 3x^2 + 5x + 2
   Для поля остатков по модулю 5:
   (x^3 + 4x + 1) - (2x^5 + x^3 - 3x^2 - x - 1) === 3x^5 + 3x^2 + 2
   */
  infix def minus(other: Polynom[A])(implicit F: Field[A]): Polynom[A] =
    val maxLength = math.max(coeffs.length, other.coeffs.length)
    val newCoeffs = (0 until maxLength)
      .map { i =>
        val aInd = coeffs.length - 1 - i
        val bInd = other.coeffs.length - 1 - i
        val a = if (aInd >= 0) coeffs(aInd) else F.zero
        val b = if (bInd >= 0) other.coeffs(bInd) else F.zero
        F.add(a, F.addInv(b))
      }
      .toList
      .reverse
    Polynom(newCoeffs)

  /** Опциональное задание со звездочкой.
   * Реализовать евклидово деление двух многочленов над полем F. Метод должен возвращать пару из частного и остатка от деления.
   * Можно реализовать например делением в столбик.
   */
  infix def div(other: Polynom[A])(using field: Field[A]): (Polynom[A], Polynom[A]) = ???

  /**
   Опциональное задание со звездочкой.
   Реализовать вычисление наибольшего общего делителя многочленов над полем F
   Результат нормализовать (старший коэффициэнт должен быть 1)

   Например:
   Для поля вещественных чисел:
   gcd(
   x^4 - x^3 + 2x^2 + 8x - 10,
      x^5 + 3x^3 + 10x^2 + 2x + 10
   ) === x^3 + 2x + 10

   Для поля остатков по модулю 7:
   gcd(
   x^4 + 6x^3 + 2x^2 + x + 4,
      x^5 + 3x^3 + 3x^2 + 2x + 3
   ) === x^3 + 2x + 3

   При реализации используйте рекурсивный алгоритм Евклида для поиска наибольшего общего делителя
   */
  infix def gcd(other: Polynom[A])(using F: Field[A]): Polynom[A] = ???

object Polynom:

  /**
   Конструктор Polynom из коэффициэнтов многочлена:
   from(3, 0, 2) === 3x^2 + 2
     from(5, 1, 2, 7, 1) === 5x^4 + x^3 + 2x^2 + 7x + 1
   */
  def from[A](xs: A*): Polynom[A] = Polynom(xs.toList)
