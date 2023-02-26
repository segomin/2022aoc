package aoc

import hedgehog.munit.HedgehogSuite

class Day08Test extends HedgehogSuite {
  val inputs =
    """30373
      |25512
      |65332
      |32549
      |35390""".stripMargin.split("\n")


  test("count visible tree") {
    val board = Board.of(inputs)
    val mid = Point(board.width / 2, board.height / 2)

    board.searchFrom(mid)
    val count = board.countAllVisible()

    withMunitAssertions { assertions =>
      assertions.assertEquals(count, 21)
    }
  }

  test("get the most view point") {
    val countBoard = Board.of(inputs)
    val first = Point(0, 0)

    countBoard.countFrom(first)
    val count = countBoard.getMaxVisibleTreeCount()

    withMunitAssertions { assertions =>
      assertions.assertEquals(count, 8)
    }
  }
}

/**
 * 회고: 생각보다 구현에서 자꾸 실수를 해서 삽질하게됨
 * 스칼라 스럽게 side-effect 없이 구현하는건 아직 어려운듯
 */