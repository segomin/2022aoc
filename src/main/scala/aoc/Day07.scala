package aoc

import scala.io.Source

object Day07 extends App {
  val filename = "src/main/scala/aoc/Day07.input"
  val lines = Source.fromFile(filename).getLines()

  trait FD {
    val dir: Dir
    val name: String
  }
  case class File(dir: Dir, name: String, size: Int) extends FD
  case class Dir(dir: Dir, name: String, var fds: List[FD]) extends FD {
    def setFile(size: String, name: String): Dir = {
      val file = fds.find(fd => fd.name == name)
      // 스칼라 스럽지는 않지만,
      if (file.isEmpty) {
        val newFile = File(this, name, size.toInt)
        this.fds = this.fds.appended(newFile)
      }
      this
    }

    def cd(name: String): Dir = {
      if (name == "/") return getRoot()

      if (name == "..") return dir

      val fd = fds.find(fd => fd.name == name).getOrElse( {
        val newFolder = Dir(this, name, List.empty)
        this.fds = this.fds.appended(newFolder)
        newFolder
      })

      fd.asInstanceOf[Dir]
    }

    def getRoot(): Dir = {
      def getRoot(folder: Dir): Dir = {
        if (folder.dir != null)
          getRoot(folder.dir)
        else
          folder
      }
      getRoot(this)
    }
  }

  def mkTree(lines: Iterator[String]): Dir = {
    lines.foldLeft(Dir(null, "/", List.empty)) { (dir: Dir, line: String) => {
      val console = line.split(" ")
      val folder: Dir = console match {
        case Array("$", "cd", name) => dir.cd(name)
        case Array("$", "ls") => dir // ignore cmd
        case Array("dir", _) => dir // ignore dir (will create when `$ cd <name>`)
        case Array(size, name) => dir.setFile(size, name)
        case _ => dir
      }
      folder
    }
    }
  }

  def getDirSize(dir: Dir) = {
    def dirSize(fd: FD, accSize: Int): Int = {
      fd match {
        case File(_, _, size) => size + accSize
        case Dir(_, _, fds) => fds.foldLeft(0) { (acc, fd) => dirSize(fd, acc) } + accSize
        case _ => accSize
      }
    }
    dirSize(dir, 0)
  }

  def getDirSizeList(dir: Dir): List[Int] = {
    var list: List[Int] = List.empty
    // 각 dir 의 size 를 list 에 넣는 좋은 방법이 있을텐데
    def calcCurrentSize(dir: Dir): Int = {
      val (dirs: List[Dir], files: List[File]) = dir.fds.partition(_.isInstanceOf[Dir])
      val curFilesSize: Int = files.map(f => f.size).sum
      val subs = dirs.map(subDir => calcCurrentSize(subDir)).sum
      list = list :+ (subs + curFilesSize)
      subs + curFilesSize
    }
    calcCurrentSize(dir)

    list
  }

  def sumOfUnder100(dir: Dir): Int = {
    getDirSizeList(dir).filter(_ < 100_000).sum
  }

  def minimumSizeToDelete(root: Dir): Int = {
    val sizeList = Day07.getDirSizeList(root)
    val total = Day07.getDirSize(root)
    val reqSize = 30_000_000 - (70_000_000 - total)
    sizeList.filter(_ > reqSize).min
  }

  val root: Dir = Day07.mkTree(lines).getRoot()
  val part1: Int = Day07.sumOfUnder100(root)
  println(part1)

  val part2: Int = Day07.minimumSizeToDelete(root)
  println(part2)
}
