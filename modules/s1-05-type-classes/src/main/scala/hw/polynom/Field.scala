package hw.polynom

/**
 Реализовать тайпкласс Field
 Поле - множество F, на котором:

 - определена операция сложения (+)
 - есть нейтральный элемент (0) относительно операции сложения
 - для любого элемента множества определен обратный элемент относительно сложения
 - определена операция умножения (*)
 - есть нейтральный элемент (1) относительно операции умножения (на F\{0})
 - для любого элемента кроме 0 определен обратный относительно операции умножения

 И выполнены законы:
 1) (F, +, 0) - коммутативная группа
 2) (F \ {0}, *, 1) - коммутативная группа
 3) дистрибутивность умножения относительно сложения:
 forall a, b, c: F => (a + b) * c = (a * c) + (b * c)
 */
trait Field[F]:
  // названия методов
  def zero: F
  def one: F
  def add(a: F, b: F): F
  def mul(a: F, b: F): F
  def addInv(a: F): F
  def mulInv(x: F): Option[F]

  //
  // Реализовать синтаксис для операций из тайпкласса Field
  //
  extension (a: F)
    def +(b: F): F = add(a, b)
    def -(b: F): F = add(a, addInv(b))
    def *(b: F): F = mul(a, b)
    def /(b: F): Option[F] =
      mulInv(b) match
        case Some(invB) => Some(mul(a, invB))
        case _          => None

end Field

object Field:

  /**
   Реализовать summoner для Field
   После реализации вместо `summon[Field[Foo]]` можно будет писать просто `Field[Foo]`
   */
  def apply[F](using field: Field[F]): Field[F] = field

  /**
   Реализовать инстанс поля для вещественных чисел
   На основе тайпкласса Fractional из стандартной библиотеки выводить инстанс Field

   Чтобы в скоупе всегда были инстансы
   Field[Double], Field[Float], Field[BigDecimal]
   */
  given [A](using fractional: Fractional[A]): Field[A] = new Field[A] {
    def zero: A = fractional.zero
    def one: A = fractional.one
    def add(a: A, b: A): A = fractional.plus(a, b)
    def mul(a: A, b: A): A = fractional.times(a, b)
    def addInv(a: A): A = fractional.negate(a)
    def mulInv(a: A): Option[A] =
      if (fractional.equiv(a, fractional.zero)) then None
      else Some(fractional.div(fractional.one, a))
  }

  /**
   Релизовать инстанс поля для множества из двух элементов {true, false}
   Операция сложения - "исключающее или"
   Операция умножения - "и"
   */
  given Field[Boolean] with
    def zero: Boolean = false
    def one: Boolean = true
    def add(a: Boolean, b: Boolean): Boolean = a ^ b
    def mul(a: Boolean, b: Boolean): Boolean = a && b
    def addInv(a: Boolean): Boolean = a
    def mulInv(a: Boolean): Option[Boolean] =
      if a then Some(true)
      else None

  /**
   Реализовать метод создающий конечное поле остатков по модулю p, где p - простое
   На входе: p - простое
   1) не надо проверять что на входе действительно простое число
   2) если передать составное число в pField, то на выходе может быть любая некорректная структура
   */
  def pField(p: Int): Field[Int] =
    new Field[Int]:
      def zero: Int = 0
      def one: Int = 1
      def add(a: Int, b: Int): Int = (a + b) % p
      def mul(a: Int, b: Int): Int = (a * b) % p
      def addInv(a: Int): Int = (p - a) % p
      def mulInv(a: Int): Option[Int] =
        if a == 0 then None
        else
          (1 until p).find(x => (a * x) % p == 1) match
            case Some(x) => Some(x)
            case None    => None
