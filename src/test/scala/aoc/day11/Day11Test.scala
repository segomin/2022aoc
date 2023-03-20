package aoc.day11

import aoc.day20.Day11
import hedgehog.munit.HedgehogSuite

class Day11Test extends HedgehogSuite {
  val inputs =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin

  test("monkey business part1") {
    val part1 = Day11.monkeyBusiness(inputs, 20, 3);
    println(part1)
    withMunitAssertions { assertions =>
      assertions.assertEquals(part1, 10605L)
    }
  }
  test("monkey business part2") {
    val part2 = Day11.monkeyBusiness(inputs, 10000);
    println(part2)
    withMunitAssertions { assertions =>
      assertions.assertEquals(part2, 2713310158L)
    }
  }
}
