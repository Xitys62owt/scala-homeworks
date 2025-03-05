package hw

trait Monoid[A]:
  def empty: A
  def combine(l: A, r: A): A

object Monoid:
  val intSum = new Monoid[Int]:
    def empty: Int = 0
    def combine(l: Int, r: Int): Int = l + r

  val stringConcat = new Monoid[String]:
    def empty: String = ""
    def combine(l: String, r: String): String = l + r

/**
  * II. Дерево отрезков
  *
  * Необходимо реализовать структуру данных -- дерево отрезков (подробнее можно прочитать на хабре https://habr.com/ru/articles/808511/)
  * Дерево отрезков позволяет эффективно (логарифмическая сложность) считать результат ассоциативной функции на отрезке и обновлять значения в массиве.
  *
  */
sealed trait SegmentTree[A]:
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
    * aссоциативная операция и нейтральный элемент
    */
  def monoid: Monoid[A]

  /**
  * II. 2.
  *
  * Реализовать метод update который обновляет эелемент с индеком idx на значение a
  * Должен иметь логарифмическую сложность от размера исходной последовательности
  *
  * Не обязательно использовать хвостовую рекурсию (на лекции по ленивости будем разбирать как такие вызовы делать стэкобезопасными).
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
      case Leaf(idx, value, _) => Leaf(startIdx, a, monoid)
      case Node(value, startIdx, endIdx, left, right, monoid) =>
        val mid = (startIdx + endIdx) / 2
        val l = if (idx <= mid) left.update(idx, a) else left
        val r = if (idx > mid) right.update(idx, a) else right
        val nvalue = monoid.combine(l.value, r.value)
        Node(nvalue, startIdx, endIdx, l, r, monoid)

  /**
  * II. 3.
  *
  * Реализовать метод calc который считает результат функции (которая была указано неявно при создании дерева)
  * на отрезке [from, to] (оба конца включительно) используя частичные результаты посчитанные в узлах дерева.
  *
  * Должен иметь логарифмическую сложность от размера интервала.
  *
  * Не обязательно использовать хвостовую рекурсию (на лекции по ленивости будем разбирать как такие вызовы делать стэкобезопасными).
  *
  * Пример:
  *  SegmentTree(5, 2, 4, 1, 6, 7)(Monoid.intSum).calc(1, 5) = 20
  *  SegmentTree(5, 2, 4, 1, 6, 7)(Monoid.intSum).calc(1, 4) = 13
  */
  def calc(from: Int, to: Int): A =
    this match
      case Node(value, startIdx, endIdx, left, right, _) if from <= startIdx && to >= endIdx => value
      case Node(_, startIdx, endIdx, left, right, _) =>
        val lValue = if (from <= left.endIdx) left.calc(from, to) else monoid.empty
        val rValue = if (to >= right.startIdx) right.calc(from, to) else monoid.empty
        monoid.combine(lValue, rValue)
      case Leaf(valueIdx, value, _) if valueIdx >= from && valueIdx <= to => value
      case _                                                              => monoid.empty

case class Leaf[A](idx: Int, value: A, monoid: Monoid[A]) extends SegmentTree[A]:
  val startIdx = idx
  val endIdx = idx

case class Node[A](
  value: A,
  startIdx: Int,
  endIdx: Int,
  left: SegmentTree[A],
  right: SegmentTree[A],
  monoid: Monoid[A]
) extends SegmentTree[A]

object SegmentTree:
  /**
   * II. 1.
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
  def apply[A](values: A*)(monoid: Monoid[A]): SegmentTree[A] =
    val newSize = math.pow(2, math.ceil(math.log(values.length) / math.log(2))).toInt
    val newSeq = values.toSeq ++ Seq.fill(newSize - values.length)(monoid.empty)
    buildTree(newSeq.toIndexedSeq, 0, newSize - 1, monoid)

  def buildTree[A](values: IndexedSeq[A], startIdx: Int, endIdx: Int, monoid: Monoid[A]): SegmentTree[A] =
    if (startIdx == endIdx) {
      Leaf(startIdx, values(startIdx), monoid)
    } else {
      val mid = (startIdx + endIdx) / 2
      val l = buildTree(values, startIdx, mid, monoid)
      val r = buildTree(values, mid + 1, endIdx, monoid)
      val value = monoid.combine(l.value, r.value)
      Node(value, startIdx, endIdx, l, r, monoid)
    }

  /**
    * II. 4. Вопрос: как будет выглядеть код, если вам потребуется считать результаты нескольких операций
    *    на одной одной и той же последовательности элементов?
    *
    *    Потребуется ли делать дополнительную структуру данных? Или расширять текущую?
    */

  // В целом, у нас в структуре вписана используемая операция, т.е. если дерево построено для суммы,
  // то мы в не-листьях храним сумму элементов-детей. Если мы знаем, какие операции нам надо поддерживать,
  // то можно строить дерево сразу для нескольких операций (и обновлять для нескольких), храня в родителях
  // что-то вроде мапы или массива, где мы знаем какой индекс за какую операцию отвечает.
  // Правда эта реализация, по сути, почти ничем не будет отличаться (в смысле времени и памяти)
  // от просто нескольких деревьев, а также будет работать только если нейтральный элемент наших ассоциативных
  // операций совпадает. Иначе же для построения дерева мы будем давать на вход массивы с разными
  // хвостами (условно нули на конце для суммы и единички для умножения), тогда уже придется разбить на несколько
  // структур. Но с нашей хорошей "шаблонной" реализацией ничего нового дописывать не придется.
