package aoc.day20

import scala.collection.mutable
import scala.io.Source

object Day11 extends App {

  val filename = "src/main/scala/aoc/day20/Day11.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines.mkString("\n")

  def monkeyBusiness(input: String, round: Int, divideBy: Int = 1): Long = {
    val monkeys = MonkeyParser.parseMonkeys(input)
    val monkeyMap = monkeys.map(monkey => monkey.id -> monkey).toMap;

    for (i <- 1 to round) {
      monkeys.foreach { monkey =>
        monkey.throwItems(divideBy, (to, worry) =>
          monkeyMap.getOrElse(to, throw new RuntimeException("")).appendItem(worry)
        )
      }

      if (List(1, 20, 1000, 2000, 3000, 4000).contains(i)) {
        println(s"== After round $i ==")
        monkeys.foreach(monkey => println(s"Monkey ${monkey.id} inspected items ${monkey.inspectCount} times."))
        println()
      }
    }

    monkeys
      .map(monkey => monkey.inspectCount)
      .sortWith { (left, right) => left > right }.take(2)
      .product
  }

  val part1 = monkeyBusiness(inputs, 20, 3)
  println(part1);
  val part2 = monkeyBusiness(inputs, 10000)
  println(part2);
}

case class Monkey(id: Int, queue: mutable.Queue[Long], operation: (Long) => Long, divisibleBy: Long, trueTarget: Int, falseTarget: Int) {
  var inspectCount: Long = 0

  def appendItem(worry: Long): Unit = queue.addOne(worry)

  def throwItems(divideBy: Int, throwTo: (Int, Long) => Unit) = {
    while (queue.nonEmpty) {
      val item = queue.dequeue()
      inspectCount = inspectCount + 1
      val worry = operation(item) / divideBy
      val target = if (worry % divisibleBy == 0) trueTarget else falseTarget
      throwTo(target, worry)
    }
  }
}

object MonkeyParser {
  def opFun(op: String)(left: Long, right: Long): Long = op match {
    case "+" => left + right
    case "*" => left * right
  }

  def parseOperation(operation: String): Long => Long = operation.split(" ") match {
    case Array(left, op, right) if left == "old" && right == "old" => (old: Long) => opFun(op)(old, old)
    case Array(left, op, right) if left == "old" && right != "old" => (old: Long) => opFun(op)(old, right.toInt)
  }

  def parseMonkey(input: String): Monkey = {
    val monkeyRegex =
      """Monkey (\d+):
        |\s+Starting items: ([0-9, ]+)
        |\s+Operation: new = (.+)
        |\s+Test: divisible by (\d+)
        |\s+If true: throw to monkey (\d+)
        |\s+If false: throw to monkey (\d+)""".stripMargin.r
    input match {
      case monkeyRegex(id, items, operation, divisibleBy, trueMonkey, falseMonkey) =>
        Monkey(
          id.toInt,
          mutable.Queue(items.split(", ").map(_.toLong).toList: _*),
          parseOperation(operation),
          divisibleBy.toLong,
          trueMonkey.toInt,
          falseMonkey.toInt
        )
    }
  }

  def parseMonkeys(input: String): List[Monkey] = {
    val monkeyStrings = input.split("\n\n")
    monkeyStrings.map(parseMonkey).toList
  }
}
