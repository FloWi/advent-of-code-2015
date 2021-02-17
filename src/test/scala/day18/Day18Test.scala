package day18

import day18._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object Day18TestData {}

class Day18Part1Test extends AnyFunSuite with Matchers {

  val testInput = s"""
Initial state:
.#.#.#
...##.
#....#
..#...
#.#..#
####..

After 1 step:
..##..
..##.#
...##.
......
#.....
#.##..

After 2 steps:
..###.
......
..###.
......
.#....
.#....

After 3 steps:
...#..
......
...#..
..##..
......
......

After 4 steps:
......
......
..##..
..##..
......
......
"""
  val states = testInput
    .split("\n\n")
    .toList
    .map(_.trim)
    .map(_.linesIterator.drop(1).toList)

  test("parse state") {
    val actual = Day18.parseState(states.head)

    actual.count(_._2) shouldBe 3 + 2 + 2 + 1 + 3 + 4
    actual.size shouldBe 36

    Day18.showState(actual) shouldBe states.head
  }

  test("neighbors") {
    Day18.neighbors.distinct.size shouldBe 8

    val state = Day18.parseState(states.head)
    Day18.calcNeighbors(state, (0, 0)).size shouldBe 3 //top-left-corner
    Day18.calcNeighbors(state, (1, 0)).size shouldBe 5 //top-row
    Day18.calcNeighbors(state, (1, 1)).size shouldBe 8 //somewhere in the middle
  }

  test("calcNumberOfLitNeighbors") {
    val state = Day18.parseState(states.head)
    Day18.calcNumberOfLitNeighbors(state, (0, 0)) shouldBe 1 //top-left-corner
    Day18.calcNumberOfLitNeighbors(state, (2, 0)) shouldBe 3
  }

  test("example 1") {

    val initialState = Day18.parseState(states.head)
    Day18.calcStateAfter(initialState, 1) shouldBe Day18.parseState(states(1))
    Day18.calcStateAfter(initialState, 2) shouldBe Day18.parseState(states(2))
    Day18.calcStateAfter(initialState, 3) shouldBe Day18.parseState(states(3))
    Day18.calcStateAfter(initialState, 4) shouldBe Day18.parseState(states(4))

    println(states)

  }

}

class Day18Part2Test extends AnyFunSuite with Matchers {

  val testInput = s"""
Initial state:
##.#.#
...##.
#....#
..#...
#.#..#
####.#

After 1 step:
#.##.#
####.#
...##.
......
#...#.
#.####

After 2 steps:
#..#.#
#....#
.#.##.
...##.
.#..##
##.###

After 3 steps:
#...##
####.#
..##.#
......
##....
####.#

After 4 steps:
#.####
#....#
...#..
.##...
#.....
#.#..#

After 5 steps:
##.###
.##..#
.##...
.##...
#.#...
##...#
"""
  val states = testInput
    .split("\n\n")
    .toList
    .map(_.trim)
    .map(_.linesIterator.drop(1).toList)

  test("example 1") {

    val initialState = Day18.parseState(states.head)
    Day18.calcStateAfter(initialState, 1, cornerLampsAlwaysOn = true) shouldBe Day18.parseState(states(1))
    Day18.calcStateAfter(initialState, 2, cornerLampsAlwaysOn = true) shouldBe Day18.parseState(states(2))
    Day18.calcStateAfter(initialState, 3, cornerLampsAlwaysOn = true) shouldBe Day18.parseState(states(3))
    Day18.calcStateAfter(initialState, 4, cornerLampsAlwaysOn = true) shouldBe Day18.parseState(states(4))
  }
}
