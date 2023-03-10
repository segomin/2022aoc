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


  test("visit short part1") {
    val river = River.of(inputs)
    val count = river.countTailVisit()
    println(count)
  }

  test("visit long part2") {
  }
}

