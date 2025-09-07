package hw

/** III. Сервис лояльности
  *
  * Разрабатываем сервис лояльности/уровней обслуживания для клиентов компании.
  * Уровни лояльности реализованы с помощью иерархии классов, начиная с самого
  * простого Economy (родительский класс для всех остальных) Чем ниже мы
  * продвигаемся по потомкам от Economy тем круче класс. ServiceLevelAdvance
  * необходим для продвижения клиента от менее крутых к более крутым уровням.
  */
class Economy
class UpgradedEconomy extends Economy
class Special1b extends UpgradedEconomy
class ExtendedEconomy extends Economy
class Business_ extends ExtendedEconomy
// в первой задаче был свой бизнес, ругается на то что класс имеет то же название
class Elite extends Business_
class Platinum extends Business_

/** Модифицируйте код таким образом, чтобы ServiceLevelAdvance был привязан к
  * уровню обслуживания, а ServiceLevelAdvance.advance мог только повышать
  * уровень обслуживания. Подсказку, как именно модифицировать, вы найдете в
  * тестах.
  */
class ServiceLevelAdvance[T <: Economy] {
  def advance[U <: T]: ServiceLevelAdvance[U] = new ServiceLevelAdvance[U]
}
