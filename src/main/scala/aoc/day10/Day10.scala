package aoc.day10

import scala.io.Source

object Day10 extends App {
  val filename = "src/main/scala/aoc/day10/Day10.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines.toArray

  def interpretCmds(inputs: Array[String]) = {
    inputs
      .flatMap(input => input.split(" "))
      .foldLeft(Seq(1))((acc, instruction) => {
        instruction match {
          case "noop" => acc :+ acc.last
          case "addx" => acc :+ acc.last
          case cmd => acc :+ (acc.last + cmd.toInt)
        }
      })
  }

  def isPoint(tick: Int) = ((tick - 20) % 40 == 0)

  def sumPart1(signals: Seq[Int]) = signals.zipWithIndex
    .filter { case (_, idx) => Day10.isPoint(idx + 1) }
    .map { case (sig, idx) => sig * (idx + 1) }.sum

  def drawPixels(signals: Seq[Int]) = {
    val WIDTH = 40
    val HEIGHT = 6
    val overlaps = signals.zipWithIndex
      .filter { case (sig, idx) => List(sig - 1, sig, sig + 1).contains(idx % WIDTH) }
      .map { case (_, idx) => idx }.toSet

    (0 until HEIGHT)
      .map(row => (0 until WIDTH)
        .map(col => if (overlaps.contains(row * WIDTH + col)) "#" else "."))
      .map(row => {
        row.map(col => col).mkString
      }).mkString("\n")
  }

  val signals = interpretCmds(inputs)
  val part1 = sumPart1(signals)
  println(part1)
  val part2 = drawPixels(signals)
  println(part2)
}

