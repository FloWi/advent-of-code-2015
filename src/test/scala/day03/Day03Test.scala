package day03

import day03._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day03Part1Test extends AnyFunSuite with Matchers {
//> delivers presents to 2 houses: one at the starting location, and one to the east.
  //^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
  //^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
  test("example 1") {
    part1.solve(">") shouldBe 2
  }
  test("example 2") {
    part1.solve("^>v<") shouldBe 4

  }
  test("example 3") {
    part1.solve("^v^v^v^v^v") shouldBe 2
  }

}

class Day03Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    part2.solve("^v") shouldBe 3
  }
  test("example 2") {
    part2.solve("^>v<") shouldBe 3

  }
  test("example 3") {
    part2.solve("^v^v^v^v^v") shouldBe 11
  }

}
