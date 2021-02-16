package day17

import day17.Day17Data.input
import day17._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object Day17Data {
  val input = "20,15,10,5,5".split(",").toList
}

class Day17Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    part1.solve(input, 25) shouldBe 4
  }

}

class Day17Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    part2.solve(input, 25) shouldBe 3
  }
}
