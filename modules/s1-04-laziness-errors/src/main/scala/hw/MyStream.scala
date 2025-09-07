package hw

/**
 * MyStream - ленивый список, аналогичный LazyList в Scala
 */
sealed trait MyStream[+A]:
  import MyStream.*

  def take(n: Int): List[A] = takeInner(this, n, Nil).reverse

  /* Реализуйте метод фильтрации ленивого списка. Результатом тоже должен быть ленивый список */
  def filter(predicate: A => Boolean): MyStream[A] =
    this match
      case NonEmpty(head, tail) =>
        if (predicate(head()))
          MyStream(head(), tail().filter(predicate))
        else
          tail().filter(predicate)
      case Empty => Empty

  /* Реализуйте метод, который лениво отбросит все элементы, пока выполняется условие на элемент */
  def dropWhile(predicate: A => Boolean): MyStream[A] =
    this match
      case NonEmpty(head, tail) =>
        if (predicate(head()))
          tail().dropWhile(predicate)
        else
          this
      case Empty => Empty

  /* Реализуйте метод flatMap, аналогичный такому у списка. Метод должен работать лениво */
  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    this match
      case NonEmpty(head, tail) => concatStreams(f(head()), tail().flatMap(f))
      case Empty                => Empty

  // str1 вызваем by value, str2 - by name
  def concatStreams[A](str1: MyStream[A], str2: => MyStream[A]): MyStream[A] =
    str1 match
      case NonEmpty(head, tail) => MyStream(head(), concatStreams(tail(), str2))
      case Empty                => str2

case object Empty extends MyStream[Nothing]

object MyStream {
  // case object Empty extends MyStream[Nothing]
  // мешается, ибо определено повторно три строчки выше
  case class NonEmpty[A] private[MyStream] (h: () => A, t: () => MyStream[A]) extends MyStream[A]

  def apply[A](h: => A, t: => MyStream[A]): MyStream[A] =
    NonEmpty(() => h, () => t)

  @annotation.tailrec
  private def takeInner[A](stream: MyStream[A], n: Int, acc: List[A]): List[A] =
    stream match {
      case NonEmpty(h, t) if n > 0 => takeInner(t(), n - 1, h() :: acc)
      case _                       => acc
    }
}
