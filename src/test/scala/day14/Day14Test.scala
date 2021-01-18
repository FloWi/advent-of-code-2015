package day14

import day14.Day14._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class Day14Part1Test extends AnyFunSuite with Matchers {

  val input = """
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
""".trim.split("\n").toList

  test("parse") {
    input.map(Day14.parse) should contain theSameElementsAs List(
      Reindeer("Comet", 14, 10.seconds, 127.seconds),
      Reindeer("Dancer", 16, 11.seconds, 162.seconds)
    )
  }

  test("example 1 after 1000s") {
    val reindeers = input.map(Day14.parse)
    reindeers.map(calcDistanceAfter(_, 1000)) should contain theSameElementsInOrderAs List(1120, 1056)
  }

  test("solve") {
    part1.solve(input, 1000) shouldBe 1120
  }

}

class Day14Part2Test extends AnyFunSuite with Matchers {

  val input = """
Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
""".trim.split("\n").toList

  test("example 1 after 1000s") {
    val reindeers = input.map(Day14.parse)
    calcPoints(reindeers, 1000) shouldBe Map(
      "Comet" -> 312,
      "Dancer" -> 689
    )
  }

  test("solve") {
    part2.solve(input, 1000) shouldBe 689
  }

}
