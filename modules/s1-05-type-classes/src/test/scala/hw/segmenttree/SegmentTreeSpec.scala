package hw.segmenttree

import cats.Semigroup

class SegmentTreeSpec extends munit.FunSuite:
  test("segment tree should keep the order of elements"):
    val words = List("segment", "trees", "are", "awesome")
    val segTree = SegmentTree(words*)

    assertEquals(
      segTree.value,
      words.mkString("")
    )

  test("segment trees should properly build for monoids"):
    val tree = SegmentTree(23, 753, 547, 846, 583, 14, 321, 50, -93, 334)
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

    assertEquals(
      tree.update(0, 754).calc(0, 1),
      1507
    )

    assertEquals(
      tree.update(5, -3378 + 14).value,
      0
    )

  test("segment tree should properly build for semigroups"):
    given Semigroup[Int] with
      def combine(l: Int, r: Int): Int = l max r

    val tree = SegmentTree.fromSemigroup(23, 753, 547, 846, 583, 14, 321, 50, -93, 334)
    assertEquals(
      tree.value,
      Some(846)
    )

    assertEquals(
      tree.calc(2, 6),
      Some(846)
    )

    assertEquals(
      tree.calc(4, 10),
      Some(583)
    )

    assertEquals(
      tree.calc(10, 200),
      None
    )

    assertEquals(
      tree.update(0, 754).calc(0, 1), // тут нужен метод расширение из пункта 5
      Some(754)
    )

    assertEquals(
      tree.update(5, 847).value,
      Some(847)
    )
