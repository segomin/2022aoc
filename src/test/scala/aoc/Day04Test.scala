package aoc

import hedgehog.munit.HedgehogSuite
import hedgehog._

class Day04Test extends HedgehogSuite{

  val inputs = """2-4,6-8
                 |2-3,4-5
                 |5-7,7-9
                 |2-8,3-7
                 |14-50,14-50
                 |4-17,4-18
                 |4-90,4-91
                 |2-52,7-80
                 |6-6,4-6
                 |2-6,4-8""".stripMargin.split("\n")

  def isContain(outer: (Int, Int), inner: (Int, Int)): Boolean = {
    if (outer._1 > inner._1) return isContain(inner, outer)
    if (outer._2 < inner._2) return false
    true
  }

  test("countFullyContained") {
    val ret: Int = inputs.map(ln => Day04.calcIfContains(ln)).count(a => a)
    println(ret)

    ret ==== 5
    withMunitAssertions { assertions =>
      assertions.assertEquals(ret, 5)
    }
  }
}
