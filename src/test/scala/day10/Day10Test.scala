package day10

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day10Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    Day10.lookAndSay("1") shouldBe "11" // one one
  }

  test("example 2") {
    Day10.lookAndSay("11") shouldBe "21" //two ones
  }

  test("example 3") {
    Day10.lookAndSay("21") shouldBe "1211" //one two and one one
  }

  test("example 4") {
    Day10.lookAndSay("1211") shouldBe "111221" //one one, one two, two ones
  }

  test("example 5") {
    Day10.lookAndSay("111221") shouldBe "312211" // three 1s, two 2s, and one 1.
  }

}

class Day10Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
