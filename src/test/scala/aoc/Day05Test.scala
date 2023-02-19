package aoc

import aoc.Day05.{doMove, parseStack, splitInput}
import hedgehog.munit.HedgehogSuite
import hedgehog._

class Day05Test extends HedgehogSuite{

  val inputs = """    [D]\u0020\u0020\u0020\u0020
                 |[N] [C]\u0020\u0020\u0020\u0020
                 |[Z] [M] [P]
                 | 1   2   3\u0020
                 |
                 |move 1 from 2 to 1
                 |move 3 from 1 to 3
                 |move 2 from 2 to 1
                 |move 1 from 1 to 2""".stripMargin.split("\n")


  test("move crate") {
    val (stackLines, actionLines) = splitInput(inputs)
    val stacks = parseStack(stackLines)
    doMove(actionLines, act => stacks.move(act))

    println(stacks, stacks.getAllHead().mkString)
    withMunitAssertions { assertions =>
      assertions.assertEquals(stacks.getAllHead(), List("C", "M", "Z"))
    }
  }

  test("move crate assembly") {
    val (stackLines, actionLines) = splitInput(inputs)
    val stacks = parseStack(stackLines)
    doMove(actionLines, act => stacks.moveAssemble(act))

    println(stacks, stacks.getAllHead().mkString)
    withMunitAssertions { assertions =>
      assertions.assertEquals(stacks.getAllHead(), List("M", "C", "D"))
    }
  }
}

/**
 * 배운점
 * scala 에 transpose 메소드 (2d array 를 pivot 할 수 있는 기능)
 * int 를 사용해서 loop 를 돌리는 방법 `(0 unit number).foreach` 혹은 for(i <- 0 until number)
 * regex: `new scala.util.matching.Regex("[0-9]")` == `"[0-9]".r`
 */