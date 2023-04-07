package aoc.day20

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  val filename = "src/main/scala/aoc/day20/Day14.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines

  val inputs_ =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.split("\n")

  val pointsLines: List[List[Point]] = inputs.map(line => Point.ofList(line.split(" -> "))).toList

  val start = Point(500, 0)

  val part1Board = drawBoardPart1(pointsLines)
  while (dropSand(part1Board, start)) {}
  val part1 = countSand(part1Board)

  val part2Board = drawBoardPart2(pointsLines)
  while (dropSand(part2Board, start)) {}
  val part2 = countSand(part2Board)

  println(part1)
  println(part2)

  private def drawBoardPart1(pointsLines: List[List[Point]]): Array[Array[Char]] = {
    val board = mkBoard(pointsLines)
    drawLine(board, pointsLines)
    board
  }
  private def drawBoardPart2(pointsLines: List[List[Point]]): Array[Array[Char]] = {
    val board = mkBoardPart2(pointsLines)
    drawLine(board, pointsLines)
    drawLine(board, List(List(Point(0, board.length - 1), Point(board(0).length - 1, board.length - 1))))
    board
  }

  private def countSand(board: Array[Array[Char]]) = board.map(line => line.count(_ == 'o')).sum

  @tailrec
  private def dropSand(board: Array[Array[Char]], start: Point): Boolean = {
    val down = start.down()
    val left = down.left()
    val right = down.right()

    def isDroppable(point: Point) = {
      if (point.x < board(0).length && point.x >= 0 && point.y < board.length && point.y >= 0 )
        board(point.y)(point.x) == '.'
      else false
    }

    def isEnd(point: Point): Boolean = point.y == board.length - 1


    val next = if (isDroppable(down)) down
    else if (isDroppable(left)) left
    else if (isDroppable(right)) right
    else start

    if (next == start) {
      if (isDroppable(start)) {
        if (isEnd(start))
          return false

        board(start.y)(start.x) = 'o'
        return true
      }
      return false
    }

    dropSand(board, next)
  }

  def mkBoard(pointsLines: List[List[Point]]): Array[Array[Char]] = {
    val (x: Int, y: Int) = getMax(pointsLines)
    Array.tabulate(y, x)((_, _) => '.')
  }

  def mkBoardPart2(pointsLines: List[List[Point]]): Array[Array[Char]] = {
    val (x: Int, y: Int) = getMax(pointsLines)
    val bottomY = y + 2
    Array.tabulate(bottomY, x * 2)((_, _) => '.')
  }

  private def getMax(pointsLines: List[List[Point]]) = {
    val maxX = pointsLines.maxBy(lst => lst.maxBy(_.x).x).maxBy(_.x).x + 1
    val maxY = pointsLines.maxBy(lst => lst.maxBy(_.y).y).maxBy(_.y).y + 1
    (maxX, maxY)
  }

  def drawLine(board: Array[Array[Char]], pointsLines: List[List[Point]]) = {
    pointsLines.foreach(points => points.sliding(2).foreach(from2 => drawPath(board, from2.head, from2.last)))
  }

  def drawPath(board: Array[Array[Char]], from: Point, to: Point): Unit = {
    if (from.x == to.x) {
      if (from.y > to.y) {
        return drawPath(board, to, from)
      }
      (from.y to to.y).foreach(y => board(y)(to.x) = '#')
    } else {
      if (from.x > to.x) {
        return drawPath(board, to, from)
      }
      (from.x to to.x).foreach(x => board(to.y)(x) = '#')
    }
  }
}

case class Point(x: Int, y: Int) {
  def down() = Point(x, y + 1)
  def left()  = Point(x - 1, y)
  def right()  = Point(x + 1, y)
}

object Point {
  def ofList(xys: Array[String]): List[Point] = xys.map(xy => Point.of(xy.split(","))).toList
  def of(xy: Array[String]): Point = Point(xy(0).toInt, xy(1).toInt)
}