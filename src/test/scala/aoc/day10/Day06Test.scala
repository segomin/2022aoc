package aoc.day10

import aoc.day10.Day06.findUniqueIndex
import hedgehog.munit.HedgehogSuite

import scala.collection.mutable

class Day06Test extends HedgehogSuite {

  val inputs = """bvwbjplbgvbhsrlpgdmjqwftvncz
                 |nppdvjthqldpwncqszvftbrmjlhg
                 |nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
                 |zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
                 |""".stripMargin.split("\n")

  test("test uniq seq index") {
    inputs.foreach(input => {
      val inputChs = input.toCharArray
      val ret = findUniqueIndex(inputChs, new mutable.Queue[Char], 0)
      println(ret)
    })
  }
}

/**
 * 배운점. String 에는 toCharArray 라는 편한 method 가 있었음
 */
