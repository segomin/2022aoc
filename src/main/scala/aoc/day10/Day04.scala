package aoc.day10

import scala.io.Source

object Day04 extends App {

  val filename = "src/main/scala/aoc/Day04.input"
  val lines = Source.fromFile(filename).getLines()

  def isContain(outer: (Int, Int), inner: (Int, Int)): Boolean = {
    if (outer._1 > inner._1) return isContain(inner, outer)
    if (outer._1 == inner._1 && outer._2 < inner._2) return isContain(inner, outer)
    if (outer._2 < inner._2) return false
    true
  }

  def isOverlaps(left: (Int, Int), right: (Int, Int)): Boolean = {
    if (left._1 > right._1) return isOverlaps(right, left)
    if (left._1 == right._1) return true
    if (left._2 < right._1) return false
    true
  }

  def calcIf(line: String)(func: ((Int, Int), (Int, Int)) => Boolean) = {
    val ret: ((Int, Int), (Int, Int)) = line.split(',').map(_.split('-') match {
      case Array(a, b) => (a.toInt, b.toInt)
    }) match {
      case Array(a, b) => (a, b)
    }
    func(ret._1, ret._2)
  }

//  val ret: Int = lines.map(calcIf(_)(isContain)).count(a => a)
  val ret: Int = lines.map(calcIf(_)(isOverlaps)).count(a => a)
  println(ret) // supposed to be 524
}
