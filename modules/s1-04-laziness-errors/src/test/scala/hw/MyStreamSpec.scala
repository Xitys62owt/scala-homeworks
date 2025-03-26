package hw

class MyStreamSpec extends munit.FunSuite:
  // Бесконечная последовательность чисел Фибоначчи
  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34...
  val fibonacci: MyStream[Int] = {
    def nextValue(prePrev: Int, prev: Int): MyStream[Int] = {
      val current = prePrev + prev
      MyStream(current, nextValue(prev, current))
    }

    MyStream(0, MyStream(1, nextValue(0, 1)))
  }

  test("filter"):
    assertEquals(fibonacci.filter(_ % 2 == 0).take(4), List(0, 2, 8, 34))

  test("dropWhile"):
    assertEquals(fibonacci.dropWhile(_ < 10).take(3), List(13, 21, 34))

  test("flatMap"):
    val result = fibonacci
      .flatMap { a => MyStream(a, MyStream(a, MyStream(a, Empty))) }
      .take(15)

    assertEquals(result, List(0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3))
