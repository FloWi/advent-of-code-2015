package day12

import day12.part1._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day12Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    solve(List("[1,2,3]")) shouldBe 6
  }

  test("example 2") {
    solve(List("""{"a":2,"b":4}""")) shouldBe 6
  }

  test("example 3") {
    solve(List("""[[3]]""")) shouldBe 3
  }

  test("example 4") {
    solve(List("""{"a":{"b":4},"c":-1}""")) shouldBe 3
  }

}

class Day12Part2Test extends AnyFunSuite with Matchers {

  /*
[1,2,3] still has a sum of 6.
[1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
{"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
[1,"red",5] has a sum of 6, because "red" in an array has no effect.
   */

  test("example 1") {
    part2.solve(List("[1,2,3]")) shouldBe 6
  }

  test("example 2") {
    part2.solve(List("""[1,{"c":"red","b":2},3]""")) shouldBe 4
  }

  test("example 3") {
    part2.solve(List("""{"d":"red","e":[1,2,3,4],"f":5}""")) shouldBe 0
  }

  test("example 4") {
    part2.solve(List("""[1,"red",5]""")) shouldBe 6
  }

}
