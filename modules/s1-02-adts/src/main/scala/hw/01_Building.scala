package hw

/**
  * I. Здание и ADT
  *
  *   1. Отразить в ADT следующую предметную область:
  *      - Здание. У здания есть строковый адрес и этажи (ссылка на 1-й этаж)
  *      - Этаж бывает жилым, чердаком или коммерческим
  *      - У каждого жилого этажа есть 2 постояльца и лестница на следующий этаж
  *        (просто ссылка на этаж)
  *      - У каждого коммерческого этажа есть несколько заведений (минимум 1) и
  *        лестница на следующий этаж (да, в этом доме можно открыть свою
  *        кальянную на 5 этаже, даже если всего этажей 10 :kekw:)
  *      - У заведения есть название
  *      - Чердак может быть обычным, либо тоже коммерческим, но только с 1
  *        заведением.
  *      - У постояльца есть возраст и пол (м/ж).
  *
  *   2. Реализовать функцию fold аккумулирующую результат во время обхода здания.
  *      На каждом этаже аккумулируемое значение пересчитывается с помощью функции f,
  *      параметрами которой служат текущее значение аккумулятора и этаж. Первый
  *      параметр - это текущий аккумулятор. Второй \- текущий этаж. Здание
  *      необходимо обходить снизу вверх. Аккумулятор изначально равен accumulator
  *
  *   3. Реализовать остальные функции с помощью fold
  *
  *   4. Напишите тесты в Building.test.scala. На функцию fold отдельные тесты
  *      писать не обязательно
  */

case class Resident(age: Int, gender: Gender)
sealed trait Gender
case object Male extends Gender
case object Female extends Gender

sealed trait Floor {
  def next: Option[Floor]
} // вынес, т.к. building в строчке ниже уже должен знать про floor
case class Building(address: String, firstFloor: Floor)

case class Business(name: String)
case class ResidentialFloor(residents: List[Resident], next: Option[Floor]) extends Floor
case class CommercialFloor(businesses: List[Business], next: Option[Floor]) extends Floor
case class Attic(business: Option[Business], next: Option[Floor]) extends Floor
// вроде как у чердака ссылки быть не должно, но ссылка по умолчанию есть у этажа, и остается у чердака

object Building:
  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом этаже
    * с начальным аккумулятором [[accumulator]]
    */
  def fold[T](building: Building, accumulator: T)(f: (T, Floor) => T): T = 
    // чтобы передавать этаж, а не здание
    def loop(floor: Option[Floor], acc: T): T = 
      floor match
        case Some(floor) => loop(floor.next, f(acc, floor))
        case None => acc

    loop(Some(building.firstFloor), accumulator)

  /** Подсчитывает количество этажей, на которых живет хотя бы один мужчина
    * старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int = 
    fold(building, 0) { (acc, floor) =>
      floor match
        case ResidentialFloor(residents, _) 
          if residents.exists(r => r.gender == Male && r.age >= olderThan) => acc + 1
        case _ => acc
    }

  /** Находит наибольший возраст женьщины, проживающей в здании. Используйте
    * [[fold]]
    */
  def womanMaxAge(building: Building): Option[Int] = 
    fold(building, Option.empty[Int]) { (maxAgeOpt, floor) =>
      floor match
        case ResidentialFloor(residents, _) =>
          val femaleAges = residents.filter(_.gender == Female).map(_.age)
          if (femaleAges.isEmpty) maxAgeOpt
          else 
            maxAgeOpt match
              case None => Some(femaleAges.max)
              case Some(currentMax) => Some(math.max(currentMax, femaleAges.max)) 
        case _ => maxAgeOpt
    }

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = 
    fold(building, 0) { (acc, floor) =>
      floor match
        case CommercialFloor(businesses, _) => acc + businesses.size
        case Attic(Some(business), _) => acc + 1
        case _ => acc
    }

  /** Находит среднее кол-во коммерческих заведений в зданиях. Используйте [[fold]] */
  def countCommercialAvg(building: List[Building]): Double = 
    if (building.isEmpty) 0.0
    else building.map(countCommercial).sum.toDouble / building.size
