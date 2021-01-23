package day16

import day16.Day16.Aunt
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day16Part1Test extends AnyFunSuite with Matchers {

  test("parse") {
    val input = "Sue 500: perfumes: 4, cars: 9, trees: 4"

    Day16.parse(input) shouldBe Aunt(500, Map("perfumes" -> 4, "cars" -> 9, "trees" -> 4))

  }

  test("example 1") {}

}

class Day16Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
