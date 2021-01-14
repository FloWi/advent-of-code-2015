package day01

import day01._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day01Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    part1.solve("(())") shouldBe 0
    part1.solve("()()") shouldBe 0
  }

  test("example 2") {
    part1.solve("(((") shouldBe 3
    part1.solve("(()(()(") shouldBe 3
  }
  test("example 3") {
    part1.solve("))(((((") shouldBe 3
  }
  test("example 4") {
    part1.solve("())") shouldBe -1
    part1.solve("))(") shouldBe -1

  }
  test("example 5") {
    part1.solve(")))") shouldBe -3
    part1.solve(")())())") shouldBe -3
  }

}

class Day01Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {
    // ) causes him to enter the basement at character position 1.
    part2.solve(")") shouldBe 1

  }
  test("example 2") {
    // ()()) causes him to enter the basement at character position 5.
    part2.solve("()())") shouldBe 5
  }

}
