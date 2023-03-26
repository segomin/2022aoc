package aoc.day20

import hedgehog.munit.HedgehogSuite

class Day12Test extends HedgehogSuite {
  val inputs =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin

  test("part1") {
    val heightmap = inputs.split("\n").map(str => str.toArray)
    val part1 = Day12.startAgain(heightmap)

    println(part1)
    withMunitAssertions { assertions =>
      assertions.assertEquals(part1, 31)
    }
  }
}
