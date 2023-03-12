package aoc

import scala.io.Source


trait Hand {
  val shapeOfScore: Int

  def drawingToMe(): Hand

  def losingToMe(): Hand

  def winToMe(): Hand

  def engage(opponent: Hand): Int = opponent match {
    case LostTo() => this.shapeOfScore + 0
    case DrawTo() => this.shapeOfScore + 3
    case WinTo() => this.shapeOfScore + 6
  }

  object LostTo {
    def unapply(opponent: Hand): Boolean = winToMe() == opponent
  }

  object DrawTo {
    def unapply(opponent: Hand): Boolean = drawingToMe() == opponent
  }

  object WinTo {
    def unapply(opponent: Hand): Boolean = losingToMe() == opponent
  }
}

object Rock extends Hand {
  override val shapeOfScore: Int = 1

  override def losingToMe(): Hand = Scissors

  override def winToMe(): Hand = Paper

  override def drawingToMe(): Hand = Rock
}

object Paper extends Hand {
  override val shapeOfScore: Int = 2

  override def losingToMe(): Hand = Rock

  override def winToMe(): Hand = Scissors

  override def drawingToMe(): Hand = Paper
}

object Scissors extends Hand {
  override val shapeOfScore: Int = 3

  override def losingToMe(): Hand = Paper

  override def winToMe(): Hand = Rock

  override def drawingToMe(): Hand = Scissors
}

object Day02 extends App {

  val filename = "src/main/scala/aoc/Day02.input"

  def getScores(xs: List[String]): Int = xs
    .map(x => x.split(" "))
    .filter(em => em.length == 2)
    .foldLeft(0) { (acc, em) =>
      em match {
        case Array(abc, xyz) =>
          val elf = getElfs(abc)
          val mine = getOpponentOf(xyz, elf)
          acc + mine.engage(elf)
//          acc + getMine(xyz).engage(getElfs(abc)) xyz as my rock,paper,scissors
      }
    }

  val result = getScores(Source.fromFile(filename).getLines.map(a => a.trim).toList)

  print(result)

  def getElfs(abc: String) = abc match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
  }

  def getMine(xyz: String) = xyz match {
    case "X" => Rock
    case "Y" => Paper
    case "Z" => Scissors
  }

  def getOpponentOf(of: String, opp: Hand) = of match {
    case "X" => opp.losingToMe()
    case "Y" => opp.drawingToMe()
    case "Z" => opp.winToMe()
  }
}