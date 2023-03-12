package aoc.day10

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object Day03 extends App {
  @tailrec
  private def assignIndex(input: String, arr: Array[Boolean]): Array[Boolean] = input match {
    case h s_+: ts => {
      arr(h - 'A') = true
      assignIndex(ts, arr)
    }
    case _ => arr
  }

  def calcDuplicateChar(input: String): Option[Int] = {
    val mid = input.length / 2
    val lefts = assignIndex(input.substring(0, mid), new Array[Boolean]('z' - 'A' + 1))
    val rights = assignIndex(input.substring(mid), new Array[Boolean]('z' - 'A' + 1))

    ((lefts zip rights) zipWithIndex)
      .find(a => a._1._1 && a._1._2)
      .map(a => a._2 + 'A')
  }

  def calcDuplicateCharIn3(inputs: Seq[String]): Option[Int] = {
    val lefts = assignIndex(inputs(0), new Array[Boolean]('z' - 'A' + 1))
    val middles = assignIndex(inputs(1), new Array[Boolean]('z' - 'A' + 1))
    val rights = assignIndex(inputs(2), new Array[Boolean]('z' - 'A' + 1))

    (lefts.lazyZip(middles).lazyZip(rights) zipWithIndex)
      .find(a => a._1._1 && a._1._2 && a._1._3)
      .map(a => a._2 + 'A')
  }

  /**
   * [a-z] : 1 ~
   * [A-Z] : 27 ~
   */
  def charToPoint(char: Int): Int = if (char >= 'a' && char <= 'z') (char - 'a') + 1 else (char - 'A') + 27

  private object s_+: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map {
      (_, s.tail)
    }
  }

  val filename = "src/main/scala/aoc/Day03.input"
  val lines = Source.fromFile(filename).getLines

  val sum = lines.flatMap(input => {
    calcDuplicateChar(input)
  }).foldLeft(0)((acc, ch) => {
    acc + Day03.charToPoint(ch)
  })
  println(sum)

  val filename2 = "src/main/scala/aoc/Day03-2.input"
  val lines2 = Source.fromFile(filename2).getLines
  val sum2 = lines2.grouped(3).flatMap(input => {
    calcDuplicateCharIn3(input)
  }).foldLeft(0)((acc, ch) => {
    acc + Day03.charToPoint(ch)
  })
  println(sum2)
}

// 배운점
// Array 초기화 하는 방법
// iterator 의 grouped 메소드
// String 의 pattern matching
