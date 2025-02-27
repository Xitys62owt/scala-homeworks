package hw

class SegmentTreeSpec extends munit.FunSuite:
  test("segment tree should keep the order of elements"):
    val words = List("segment", "trees", "are", "awesome")
    val segTree = SegmentTree(words*)(Monoid.stringConcat)

    assertEquals(
      segTree.value,
      words.mkString("")
    )

  test("segment trees should properly build"):
    val monoid = new Monoid[Int]:
      def empty: Int = 0
      def combine(l: Int, r: Int): Int = l + r

    val tree = SegmentTree(23, 753, 547, 846, 583, 14, 321, 50, -93, 334)(Monoid.intSum)
    assertEquals(
      tree.value,
      3378
    )

    assertEquals(
      tree.calc(2, 6),
      2311
    )

    assertEquals(
      tree.calc(4, 10),
      1209
    )

    assertEquals(
      tree.calc(10, 200),
      0
    )
