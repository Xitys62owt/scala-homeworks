package hw

class Spec01 extends munit.FunSuite:
  test("1. swap"):
    assertEquals(swap(0), 0L)
    assertEquals(swap(10000), 1L)
    assertEquals(swap(42), 24L)
    assertEquals(swap(34879347593470L), 7439574397843L)
    assertEquals(swap(9876567891L), 1987656789L)

  test("2. selfRepeat"):
    assertEquals(selfRepeat(5), 3)
    assertEquals(selfRepeat(10), 5)
    assertEquals(selfRepeat(15), 6)
    assertEquals(selfRepeat(21), 7)
    assertEquals(selfRepeat(30), 8)
    assertEquals(selfRepeat(100), 14)
    assertEquals(selfRepeat(1000), 45)
    assertEquals(selfRepeat(10000), 141)
    assertEquals(selfRepeat(100000), 447)

  test("3. fibDiv"):
    assertEquals(fibDiv(3, 3), 144L)
    assertEquals(fibDiv(13, 3), 10946L)
    assertEquals(fibDiv(15, 3), 1548008755920L)
    assertEquals(fibDiv(7, 5), 102334155L)
    assertEquals(fibDiv(10, 10), 1233533820207347330L)
    assertEquals(fibDiv(100, 2), 4832170346816893000L)

  val treeExample =
    Tree.Node(
      10,
      Tree.Node(
        1,
        Tree.Leaf(13),
        Tree.Node(
          42,
          Tree.Node(
            32,
            Tree.Leaf(41),
            Tree.Leaf(0)
          ),
          Tree.Leaf(14)
        )
      ),
      Tree.Leaf(-3)
    )

  test("4. 1) Tree: max"):
    assertEquals(maxT(treeExample), 42)
    assertEquals(maxT(Tree.Leaf(-10)), -10)

  val treeExamplePlus1 =
    Tree.Node(
      11,
      Tree.Node(
        2,
        Tree.Leaf(14),
        Tree.Node(
          43,
          Tree.Node(
            33,
            Tree.Leaf(42),
            Tree.Leaf(1)
          ),
          Tree.Leaf(15)
        )
      ),
      Tree.Leaf(-2)
    )

  test("4. 2) Tree: map"):
    assertEquals(mapT(treeExample, _ + 1), treeExamplePlus1)
    assertEquals(mapT(Tree.Leaf(-10), _ => 0), Tree.Leaf(0))
