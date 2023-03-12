package aoc.day10

import scala.io.Source

object Day01 extends App {

  val filename = "src/main/scala/aoc/Day01.input"

  def sumEach(xs: List[String]) = {
    def sumBy(xs: List[String], acc: Int): List[Int] = xs match {
      case (h :: t) if h != "" => sumBy(t, acc + h.toInt)
      case (h :: t) if h == "" => acc :: sumBy(t, 0)
      case Nil => acc :: Nil
    }

//    sumBy(xs, 0).max
    sumBy(xs, 0).sortWith(_ > _).take(3).sum
  }

  val result = sumEach(Source.fromFile(filename).getLines.map(a => a.trim).toList)

  print(result)

}