package hw.polynom

class PolynomSpec extends munit.FunSuite:

  test("subtract polynoms for real numbers and boolean field"):
    assertEquals(
      Polynom.from[BigDecimal](3, 2, 1) minus Polynom.from(2, 1, 0),
      Polynom.from[BigDecimal](1, 1, 1)
    )

    assertEquals(
      Polynom.from[Double](4, 1) minus Polynom.from(6, -1.5, 0.1, 2),
      Polynom.from[Double](-6, 1.5, 3.9, -1)
    )

    assertEquals(
      Polynom.from(true) minus Polynom.from(true, true, true, true),
      Polynom.from(true, true, true, false)
    )

  test("subtract polynoms for finite primary field"):
    given Field[Int] = Field.pField(7)

    assertEquals(
      Polynom.from(3, 2, 1) minus Polynom.from(2, 1, 0),
      Polynom.from(1, 1, 1)
    )

    assertEquals(
      Polynom.from(1, 1, 0) minus Polynom.from(2, 1, 0),
      Polynom.from(6, 0, 0)
    )

    assertEquals(
      Polynom.from(3, 2, 3, 2) minus Polynom.from(6, 5, 1),
      Polynom.from(3, 3, 5, 1)
    )

  // Тесты ниже следует закомментировать, если не хочешь реализовывать опциональные задания

  test("div should eval division for polynoms on real field"):
    assertEquals(
      Polynom
        .from[BigDecimal](1, -1, 2, 8, -10)
        .div(
          Polynom.from[BigDecimal](1, 0)
        ),
      (Polynom.from[BigDecimal](1, -1, 2, 8), Polynom.from[BigDecimal](-10))
    )
    assertEquals(
      Polynom
        .from[BigDecimal](1, -1, 2, 8, -10)
        .div(
          Polynom.from(1, 2)
        ),
      (Polynom.from[BigDecimal](1, -3, 8, -8), Polynom.from[BigDecimal](6))
    )

  test("gcd should eval gcd for polynoms on fractional field"):
    assertEquals(
      Polynom
        .from[BigDecimal](1, -1, 2, 8, -10)
        .gcd(
          Polynom.from(1, 0, 3, 10, 2, 10)
        ),
      Polynom.from[BigDecimal](1, 0, 2, 10)
    )

  test("gcd should eval gcd for polynoms on finite field prime sized"):
    given Field[Int] = Field.pField(7)
    assertEquals(
      Polynom
        .from[Int](1, 6, 2, 1, 4)
        .gcd(
          Polynom.from(1, 0, 3, 3, 2, 3)
        ),
      Polynom.from(1, 0, 2, 3)
    )
