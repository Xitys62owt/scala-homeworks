package hw

class ServiceLevelAdvanceSpec extends munit.FunSuite:

  test("ServiceLevelAdvance should correctly determine ServiceLevel type"):
    assertEquals(
      compileErrors(
        """
          val special1bLevel: ServiceLevelAdvance[Special1b] =
            new ServiceLevelAdvance[Economy]
              .advance[UpgradedEconomy]
              .advance[Special1b]
        """
      ),
      ""
    )
    assertEquals(
      compileErrors(
        """
          val specialPlLevel: ServiceLevelAdvance[Platinum] =
            new ServiceLevelAdvance[Economy]
              .advance[Business_]
              .advance
        """
      ),
      ""
    )

  test("SchoolClass should implement compile time checking"):
    assert(
      compileErrors(
        """
          val eliteLevel =
            new ServiceLevelAdvance[Economy]
              .advance[Elite]
              .advance[Business_]
        """
      ).nonEmpty
    )
