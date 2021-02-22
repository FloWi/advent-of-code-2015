package day20

import day20._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day20Part1Test extends AnyFunSuite with Matchers {

  test("my example 1") {
    part1.findLowestHouseNumber(minAmountOfPresents = 100) shouldBe 6
    part1.findLowestHouseNumber(minAmountOfPresents = 75) shouldBe 6
  }

}

class Day20Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
