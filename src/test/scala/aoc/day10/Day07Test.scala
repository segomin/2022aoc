package aoc.day10

import aoc.day10.Day07.Dir
import hedgehog.munit.HedgehogSuite

class Day07Test extends HedgehogSuite {
  val inputs =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin.split("\n")


  test("test sum of under 100000 size dir") {

    val root: Dir = Day07.mkTree(inputs.iterator).getRoot()

    val sumDir = Day07.sumOfUnder100(root)

    println(root.name, sumDir)

    withMunitAssertions { assertions =>
      assertions.assertEquals(sumDir, 95437)
    }
  }

  test("test get minimum delete directory") {
    val root: Dir = Day07.mkTree(inputs.iterator).getRoot()
    val answer: Int = Day07.minimumSizeToDelete(root)

    withMunitAssertions { assertions =>
      assertions.assertEquals(answer, 24933642)
    }
  }
}

/**
 * 배운점. 문제를 끝까지 잘 읽어야 함 ;;;
 * 집중력이 흐려지기 전에 문제를 풀자 ;;;
 */
