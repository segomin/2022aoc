package aoc

import scala.collection.mutable
import scala.io.Source

class Stacks(val stacks: List[mutable.Stack[String]]) {
  def getAllHead() = stacks.map(s => s.head)

  def moveAssemble(action: Action): Unit = {
    val from = stacks(action.from - 1)
    val to = stacks(action.to - 1)
    val crates = (0 until action.count).map(_ => from.pop())
    to.pushAll(crates.reverse)
  }

  def move(action: Action): Unit = {
    val from = stacks(action.from - 1)
    val to = stacks(action.to - 1)
    val crates = (0 until action.count).map(_ => from.pop())
    to.pushAll(crates)
  }
}

class Action(val count: Int, val from: Int, val to: Int)

object Stacks {
  private val character = "[A-Z]".r
  def from(arr: Array[String]): mutable.Stack[String] = {
    arr.filter(character.matches)
      .foldLeft(new mutable.Stack[String]()) { (acc, item) => acc.push(item) }
  }
}

object Action {
  private val actionReg = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r

  def unapply(arg: String): Option[(Int, Int, Int)] = {
    if (actionReg.matches(arg)) {
      val actionReg(count, from, to) = arg
      return Option(count.toInt, from.toInt, to.toInt)
    }
    None
  }

  val none = new Action(0, 1, 1)
}

object Day05 extends App {

  val filename = "src/main/scala/aoc/Day05.input"
  val lines = Source.fromFile(filename).getLines()

  def splitInput(lines: Array[String]) = {
    val idxLine = new scala.util.matching.Regex("[ 0-9]+")
    val index = lines.indexWhere(line => idxLine.matches(line))
    lines.splitAt(index + 1)
  }

  /**
   * @param stackParts
   * 0 = "    [D]    "
   * 1 = "[N] [C]    "
   * 2 = "[Z] [M] [P]"
   * 3 = " 1   2   3 "
   */
  def parseStack(stackParts: Array[String]): Stacks = {
    val pivoted = stackParts.reverse.map(a => a.split("")).transpose
    val number = "[0-9]".r
    val sss = pivoted.filter(a => number.matches(a(0)))
      .map(a => Stacks.from(a)).toList

    new Stacks(sss)
  }

  def doMove(actionLines: Array[String], func: Action => Unit): Unit = {
    val actions = actionLines.map {
      case Action(count, from, to) => new Action(count, from, to)
      case _ => Action.none
    }
    actions.foreach(action => func(action))
  }

  val (stackLines, actionLines) = splitInput(lines.toArray)
  val stacks = parseStack(stackLines)
//  doMove(actionLines, action => stacks.move(action))
  doMove(actionLines, action => stacks.moveAssemble(action))

  val heads = stacks.getAllHead();
  println(heads.fold("")((acc, ch) => acc + ch))
}
