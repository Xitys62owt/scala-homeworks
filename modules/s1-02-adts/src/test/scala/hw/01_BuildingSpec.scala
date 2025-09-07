package hw

val building = Building("Fiztekh-Space", // вдохновлено названием станции метро Физтех на англе
    CommercialFloor(
      List(Business("Fiztekh.Coffee_Tochka"), Business("Fiztekh.Stolovaya"), Business("Fiztekh.Kniga")),
      Some(CommercialFloor(
        List(Business("Fiztekh.Formula_Shaurmy"), Business("Fiztekh.Divanchiki_GK")),
        Some(CommercialFloor(
          List(Business("Fiztekh.Botalka")),
          Some(ResidentialFloor(
            List(Resident(26, Male), Resident(22, Male)),
            Some(ResidentialFloor(
              List(Resident(19, Male), Resident(21, Male)),
              Some(ResidentialFloor(
                List(Resident(22, Male), Resident(17, Male)),
                Some(ResidentialFloor(
                  List(Resident(25, Female), Resident(21, Female)),
                  Some(ResidentialFloor(
                    List(Resident(99, Female), Resident(99, Male)),
                    Some(Attic(None, None))
                  ))
                ))
              ))
            ))
          ))
        ))
      ))
    )
  )

val saray = Building("saray", Attic(None, None))

class BuildingSpec extends munit.FunSuite:
  test("countOldManFloors should return the number of men older than the specified age"):
    assertEquals(Building.countOldManFloors(building, 99), 1)

  test("countOldManFloors should return 0 if there are no men older than the specified age"):
    assertEquals(Building.countOldManFloors(building, 100), 0)

  test("countOldManFloors should return 0 if there are no men at all in the building"):
    assertEquals(Building.countOldManFloors(saray, -100), 0)

  test("womanMaxAge should find age of the oldest woman in the building"):
    assertEquals(Building.womanMaxAge(building), Some(99))

  test("womanMaxAge should return None if there are no women in the building"):
    assertEquals(Building.womanMaxAge(saray), None)

  test("countCommercial should return number of commercial establishments in the building"):
    assertEquals(Building.countCommercial(building), 6)

  test("countCommercial should return 0 if there are no commercial establishments in the building"):
    assertEquals(Building.countCommercial(saray), 0)
