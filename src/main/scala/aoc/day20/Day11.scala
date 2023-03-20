package aoc.day20

import scala.collection.mutable
import scala.io.Source

object Day11 extends App {

  val filename = "src/main/scala/aoc/day20/Day11.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines.mkString("\n")

  def monkeyBusiness(input: String, round: Int, reduceBy: Int = 1): Long = {
    val monkeys = MonkeyParser.parseMonkeys(input)
    val monkeyMap: Map[Int, Monkey] = monkeys.map(monkey => monkey.id -> monkey).toMap;

    for (_ <- 1 to round) {
      monkeys.foreach { monkey =>
        monkey.throwItems(reduceBy, (to, worry) =>
          monkeyMap(to).appendItem(worry)
        )
      }

//      if (List(1, 20, 1000, 2000, 3000, 4000).contains(i)) {
//        println(s"== After round $i ==")
//        monkeys.foreach(monkey => println(s"Monkey ${monkey.id} inspected items ${monkey.inspectCount} times. ${monkey.getQueue()}"))
//        println()
//      }
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

class Item(var worries: Map[Int, Int]) {
  def of(monkeyId: Int): Int = worries(monkeyId)
}

object Item {
  private val divisibleMap: mutable.Map[Int, Int] = mutable.Map()

  def init(init: Int) = {
    val worries = divisibleMap.map { case (id, _) => (id, init) }.toMap
    new Item(worries)
  }

  def from(item: Item, operate: Int => Int, reduceBy: Int) = {
    val worries = divisibleMap.map { case (id, divisible) =>
      if (reduceBy == 1)
        (id, operate(item.of(id)) % divisible)
      else
        (id, operate(item.of(id)) / reduceBy)
    }.toMap

    new Item(worries)
  }

  def addDivisible(monkeyId: Int, divisibleBy: Int): Unit = {
    divisibleMap.put(monkeyId, divisibleBy)
  }
}

case class Monkey(id: Int, queue: mutable.Queue[Item], operation: (Int) => Int, divisibleBy: Int, trueTarget: Int, falseTarget: Int) {
  var inspectCount: Long = 0
  def appendItem(worry: Item): Unit = queue.addOne(worry)

  def throwItems(reduceBy: Int, throwTo: (Int, Item) => Unit) = {
    while (queue.nonEmpty) {
      val item = queue.dequeue()
      inspectCount = inspectCount + 1
      val worry = Item.from(item, operation, reduceBy)
      val target = if (worry.of(id) % divisibleBy == 0) trueTarget else falseTarget
      throwTo(target, worry)
    }
  }

  def getQueue() = queue.map(item => item.of(id)).mkString(",")
}

object MonkeyParser {
  val monkeyRegex =
    """Monkey (\d+):
      |\s+Starting items: ([0-9, ]+)
      |\s+Operation: new = (.+)
      |\s+Test: divisible by (\d+)
      |\s+If true: throw to monkey (\d+)
      |\s+If false: throw to monkey (\d+)""".stripMargin.r

  def parseMonkeys(input: String): List[Monkey] = {
    val monkeyStrings = input.split("\n\n")
    monkeyStrings.foreach(parseDivisiblesFirst)
    monkeyStrings.map(parseMonkeySecond).toList
  }

  def parseDivisiblesFirst(input: String): Unit = {
    input match {
      case monkeyRegex(id, items, operation, divisibleBy, trueMonkey, falseMonkey) =>
        Item.addDivisible(id.toInt, divisibleBy.toInt)
    }
  }

  def parseMonkeySecond(input: String): Monkey = {
    input match {
      case monkeyRegex(id, items, operation, divisibleBy, trueMonkey, falseMonkey) =>
        Monkey(
          id.toInt,
          mutable.Queue(items.split(", ").map(value => Item.init(value.toInt)).toList: _*),
          parseOperation(operation),
          divisibleBy.toInt,
          trueMonkey.toInt,
          falseMonkey.toInt
        )
    }
  }

  def parseOperation(operation: String): Int => Int = operation.split(" ") match {
    case Array(left, op, right) if left == "old" && right == "old" => (old: Int) => opFun(op)(old, old)
    case Array(left, op, right) if left == "old" && right != "old" => (old: Int) => opFun(op)(old, right.toInt)
  }

  def opFun(op: String)(left: Int, right: Int): Int = op match {
    case "+" => left + right
    case "*" => left * right
  }
}
