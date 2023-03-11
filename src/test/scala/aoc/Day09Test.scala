package aoc

import hedgehog.munit.HedgehogSuite

class Day09Test extends HedgehogSuite {
  val inputs =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.split("\n")
  val inputs2 =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin.split("\n")


  test("tail visit count part1") {
    val river = new River(2).moveAll(inputs)
    val count = river.tailVisitCount()

    println(count)
    withMunitAssertions { assertions =>
      assertions.assertEquals(count, 13)
    }
  }

  test("tail visit count part2") {
    val river = new River(10).moveAll(inputs2)
    val count = river.tailVisitCount()

    println(count)
    withMunitAssertions { assertions =>
      assertions.assertEquals(count, 36)
    }
  }
}

/**
 * 배운점
 * Math.abs(num) -> (num).abs 도 가능
 * array pattern-match  `_*` 로 나머지 표현 가능
 */
