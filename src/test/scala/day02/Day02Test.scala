package day02

import day02._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import day02.Day02.Prism

class Day02Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {

    Day02.parse("2x3x4") shouldBe Prism(2, 3, 4)
    Day02.calculateWrappingPaper(Day02.parse("2x3x4")) shouldBe 58
  }

  test("example 2") {

    Day02.parse("1x1x10") shouldBe Prism(1, 1, 10)
    Day02.calculateWrappingPaper(Day02.parse("1x1x10")) shouldBe 43
  }

}

class Day02Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    Day02.calculateRibbon(Day02.parse("2x3x4")) shouldBe 34

  }
  test("example 2") {
    Day02.calculateRibbon(Day02.parse("1x1x10")) shouldBe 14

  }

}
