package hw

class SchoolClassSpec extends munit.FunSuite:

  test("SchoolClass should correctly determine School Class type"):
    assertEquals(
      compileErrors(
        """
          val geniusAndEnlightenedClass: SchoolClass[KnowNothing] =
            new SchoolClass(Seq(new Genius))
              .accept(Seq(new Enlightened))
              .accept(Seq(new Normal))
              .accept(Seq(new PoorlyEducated))
              .accept(Seq(new KnowSomething))
              .accept(Seq(new Aggressive))
              .accept(Seq(new KnowNothing))
         """
      ),
      ""
    )
    assertEquals(
      compileErrors(
        """
          val geniusAndEnlightenedClass: SchoolClass[KnowSomething] =
            new SchoolClass(Nil)
              .accept(Seq(new Enlightened))
              .accept(Seq(new KnowSomething))
        """
      ),
      ""
    )

  test("SchoolClass should implement compile time checking"):
    assert(
      compileErrors(
        """
          val geniusAndEnlightenedClass: SchoolClass[Genius] =
            new SchoolClass(Seq(new Genius))
              .accept(Seq(new KnowNothing))
        """
      ).nonEmpty
    )
