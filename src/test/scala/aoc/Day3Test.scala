package aoc

import aoc.Day03.{calcDuplicateChar, calcDuplicateCharIn3, charToPoint}
import hedgehog.munit.HedgehogSuite

class Day3Test extends HedgehogSuite {

  val inputs =
    """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw""".split("\n")

  test("firstRound") {
    val sum = inputs.flatMap(input => {
      calcDuplicateChar(input)
    }).foldLeft(0)((acc, ch) => {
      acc + charToPoint(ch)
    })
    println(sum)
  }

  val inputs2 =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin

  test("secondRound") {
    val grouped = inputs2.split("\n").grouped(3)
    grouped.foreach(r => {
      val ret = calcDuplicateCharIn3(r)
      println(ret, ret.map(_.toHexString), ret.map(_.toChar))
    })
  }
}

