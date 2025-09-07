package hw

/**   I. Упражнения
  *
  * Задания в этом файле необходимо решать используя иммутабельные коллекции,
  * т.е. scala.collection._ и scala.collection.immutable._
  */

/** 1.
  *
  * Реализуйте метод removeMostFrequent. В списке чисел нужно найти число с
  * самым большим числом повторений, и вернуть новый список без этого числа Если
  * есть несколько разных чисел с одинаковой (максимальной) частотой, то удалить
  * их все
  */
def removeMostFrequent(numbers: Seq[Int]): Seq[Int] =
  val freq = numbers.groupBy(identity).view.mapValues(_.size)
  numbers.filter(num => freq.getOrElse(num, 0) < freq.values.maxOption.getOrElse(0))

/** 2.
  *
  * Реализуйте метод smoothNumbers. Для каждого элемента списка, нужно заменить
  * его на среднее арифметическое этого элемента и двух соседних Если какого-то
  * из соседних элементов нет, то среднее необходимо считать не по 3, а по 2 или
  * 1 значению.
  */
def smoothNumbers(numbers: Seq[Int]): Seq[Double] =
  numbers.indices.map { i =>
    val neighbors = Seq(
      Some(numbers(i)),
      if (i > 0) Some(numbers(i - 1)) else None,
      if (i < numbers.length - 1) Some(numbers(i + 1)) else None
    ).flatten
    neighbors.sum.toDouble / neighbors.size
  }

case class User(
  lastName: String,
  firstName: String,
  middleName: String,
  age: Int
)

/** 3.
  *
  * Реализуйте метод sortUsers. Есть список людей (фамилия, имя, отчество,
  * возраст) Нужно отсортировать его в следующем порядке: фамилия (лекс) ->
  * возраст (по убыванию) -> имя (лекс) -> отчество (лекс)
  */
def sortUsers(users: Seq[User]): Seq[User] =
  users.sortBy(user => (user.lastName, -user.age, user.firstName, user.middleName))

/** 4.
  *
  * Релизовать ленивый бесконечный список, состоящий из степеней двойки
  *
  * powersOfTwo = 2 #:: 4 #:: 8 #:: 16 #:: 32 ...
  */
val powersOfTwo: LazyList[BigInt] =
  LazyList.from(1).map(n => BigInt(2).pow(n))

/** 5.
  *
  * С помощью Решета Эратосфена реализовать ленивый бесконечный список,
  * состоящий из простых чисел sieveEratosthene = 2 #:: 3 #:: 5 #:: 7 #:: 11 #::
  * 13 #:: 17 ...
  *
  * В этой задаче не требуется оптимальный алгоритм, ожидается что хотя бы
  * вычисление первой 1000 простых чисел будет корректно работать.
  */
val sieveEratosthene: LazyList[Int] =
  def sieve(list: LazyList[Int]): LazyList[Int] =
    list.head #:: sieve(list.tail.filter(_ % list.head != 0))
  2 #:: sieve(LazyList.from(3, 2).filter(_ % 2 != 0))
