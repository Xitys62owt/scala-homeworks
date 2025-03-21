package hw.segmenttree

import cats.{Monoid, Semigroup}
import cats.syntax.semigroup.*

/**
 * Дерево отрезков
 *
 * Необходимо реализовать структуру данных -- дерево отрезков (подробнее можно прочитать на хабре https://habr.com/ru/articles/808511/)
 * Дерево отрезков позволяет эффективно (логарифмическая сложность) считать результат ассоциативной функции на отрезке и обновлять значения в массиве.
 *
 * Задание отличается от задания по коллекциям тем, что необходимо использовать
 * соответствующие тайпклассы Monoid и Semigroup из библиотеки cats, а также их синтаксис
 */
sealed trait SegmentTree[A: Monoid]:
  /**
   * Предпосчитанный результат выполнения операции
   */
  def value: A

  /**
   * начало интервала исходной последователености
   * корорый "связан" с этим деревом
   */
  def startIdx: Int

  /**
   * конец интервала исходной последователености
   * корорый "связан" с этим деревом
   */
  def endIdx: Int

  /**
   * 2.
   * (задача 1 на построение дерева ниже, рекомендуется начать с нее)
   *
   * Реализовать метод update который обновляет эелемент с индеком idx на значение a
   * Должен иметь логарифмическую сложность от размера исходной последовательности
   *
   * Не обязательно использовать хвостовую рекурсию / трамполайнинг, считаем, что при желании вы сможете
   * доработать любой стэко-небезопасный метод через трамполйанинг.
   *
   * Пример:
   *  Если дерево построено на последовательности (5, 2, 4, 1, 6, 7)
   *  То:
   *    update(3, 0) должен будет вернуть дерево:
   *
   *                     24
   *                 /       \
   *               11         13
   *             /   \      /    \
   *            7     4   13     0
   *          /  \   / \   / \   / \
   *         5    2 4   0 6   7 0   0
   */
  def update(idx: Int, a: A): SegmentTree[A] =
    this match
      case Leaf(idx, value) => Leaf(startIdx, a)
      case Node(value, startIdx, endIdx, left, right) =>
        val mid = (startIdx + endIdx) / 2
        val l = if (idx <= mid) left.update(idx, a) else left
        val r = if (idx > mid) right.update(idx, a) else right
        val nvalue = l.value |+| r.value
        Node(nvalue, startIdx, endIdx, l, r)

  /**
   * 3.
   *
   * Реализовать метод calc который считает результат функции (которая была указано неявно при создании дерева)
   * на отрезке [from, to] (оба конца включительно) используя частичные результаты посчитанные в узлах дерева.
   *
   * Должен иметь логарифмическую сложность от размера интервала.
   *
   * Не обязательно использовать хвостовую рекурсию / трамполайнинг, считаем, что при желании вы сможете
   * доработать любой стэко-небезопасный метод через трамполйанинг.
   *
   * Пример:
   *  SegmentTree(5, 2, 4, 1, 6, 7).calc(1, 5) = 20
   *  SegmentTree(5, 2, 4, 1, 6, 7).calc(1, 4) = 13
   */
  def calc(from: Int, to: Int): A =
    this match
      case Node(value, startIdx, endIdx, left, right) if from <= startIdx && to >= endIdx => value
      case Node(_, startIdx, endIdx, left, right) =>
        val lValue = if (from <= left.endIdx) left.calc(from, to) else Monoid[A].empty
        val rValue = if (to >= right.startIdx) right.calc(from, to) else Monoid[A].empty
        lValue |+| rValue
      case Leaf(valueIdx, value) if valueIdx >= from && valueIdx <= to => value
      case _                                                           => Monoid[A].empty

// можно добавлять параметры, в том числе неявные
case class Leaf[A: Monoid](idx: Int, value: A) extends SegmentTree[A]:
  val startIdx = idx
  val endIdx = idx

// можно добавлять параметры, в том числе неявные
case class Node[A: Monoid](
  value: A,
  startIdx: Int,
  endIdx: Int,
  left: SegmentTree[A],
  right: SegmentTree[A]
) extends SegmentTree[A]

object SegmentTree:
  /**
   * 1.
   *
   * Реализовать метод apply, который строит дерево из последовательности элементов
   *
   * Пример:
   *   Для последовательности SegmentTree(5, 2, 4, 1, 6, 7)
   *   (монойд по для Int по умолчанию по сложению)
   *   Должно быть построено дерево:
   *
   *                     25
   *                 /       \
   *               12         13
   *             /   \      /    \
   *            7     5    13     0
   *          /  \   / \   / \   / \
   *         5    2 4   1 6   7 0   0
   */

  def apply[A: Monoid](values: A*): SegmentTree[A] =
    val newSize = math.pow(2, math.ceil(math.log(values.length) / math.log(2))).toInt
    val newSeq = values.toSeq ++ Seq.fill(newSize - values.length)(Monoid[A].empty)
    buildTree(newSeq.toIndexedSeq, 0, newSize - 1)

  // тупанул в прошлый раз, функцию-то брал из 3 дз
  def buildTree[A: Monoid](values: IndexedSeq[A], startIdx: Int, endIdx: Int): SegmentTree[A] =
    if (startIdx == endIdx) {
      Leaf(startIdx, values(startIdx))
    } else {
      val mid = (startIdx + endIdx) / 2
      val l = buildTree(values, startIdx, mid)
      val r = buildTree(values, mid + 1, endIdx)
      val value = l.value |+| r.value
      Node(value, startIdx, endIdx, l, r)
    }

  /**
   * 4.
   *
   * Реализовать метод fromSemigroup, который будет строить дерево из последовательности элементов типа A,
   * для которого нет инстанса монойда, а есть только инстанс полугруппы
   *
   * Пример:
   *  given Semigroup[Int] with // явно указываем монойд полугруппу для Int как max
   *    def combine(l: Int, r: Int): Int = l max r
   *
   *  SegmentTree(5, 2, 4, 1, 6, 7).max()
   */
  def fromSemigroup[A: Semigroup](values: A*): SegmentTree[Option[A]] =
    given monoid: Monoid[Option[A]] with
      val empty: Option[A] = None
      def combine(l: Option[A], r: Option[A]): Option[A] =
        (l, r) match
          case (None, None)       => None
          case (Some(x), Some(y)) => Some(x |+| y)
          case (x, None)          => x
          case (None, y)          => y

    val newSize = math.pow(2, math.ceil(math.log(values.length) / math.log(2))).toInt
    val newSeq = values.map(Some(_)).toSeq ++ Seq.fill(newSize - values.length)(monoid.empty)
    buildTreeOpt(newSeq.toIndexedSeq, 0, newSize - 1)

  def buildTreeOpt[A: Semigroup](
    values: IndexedSeq[Option[A]],
    startIdx: Int,
    endIdx: Int
  ): SegmentTree[Option[A]] =
    if (startIdx == endIdx) {
      Leaf(startIdx, values(startIdx))
    } else {
      val mid = (startIdx + endIdx) / 2
      val l = buildTreeOpt(values, startIdx, mid)
      val r = buildTreeOpt(values, mid + 1, endIdx)
      val value = l.value |+| r.value
      Node(value, startIdx, endIdx, l, r)
    }

  /**
 * 5.
 *
 * Добавить метод-расширение чтобы можно было вызывать update(idx, v: A) на деревьях Segment[Option[A]]
 * (то есть не оборачивая руками значение в Some).
 *
 * Пример:
 *
 *  val tree = SegmentTree.fromSemigroup(1, 2, 3, 4, 5)
 *  tree.update(2, 0) // должно компилироваться и работать как tree.update(2, Some(0))
 */

  extension [A: Semigroup](tree: SegmentTree[Option[A]])
    def update(idx: Int, value: A): SegmentTree[Option[A]] = tree.update(idx, Some(value))

/**
 * 6. Как будет выглядеть код, если вам потребуется считать результаты нескольких операций
 *    на одном одной и той же послежовательности элементов?
 *
 *    Потребуется ли делать дополнятельную структуру данных? Или расширять текущую?
 */

// Так же, как в третьей домашке, у нас в дерево вписана операция
// (т.е. во всех нодах лежит combine(l, r)). Таким образом, для разных операций всее-таки
// будут созданы разные деревья, хотя в код это добавлять не нужно (просто передать моноидом или полугруппой)
// Хотя если нам заранее известно, какие операции необходимо поддерживать, мы можем в нодах
// (и листьях, которые заполнены monoid.empty, если требуется) хранить список значений для каждой операции
