package day04

import day04.Day04.solve
import day04._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day04Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    solve("abcdef", "00000") shouldBe 609043
  }
  test("example 2") {
    solve("pqrstuv", "00000") shouldBe 1048970
  }

}

class Day04Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
