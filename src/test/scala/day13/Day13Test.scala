package day13

import day13.Day13.solve
import day13._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day13Part1Test extends AnyFunSuite with Matchers {

  val input = """
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
""".trim.split("\n").toList

  test("parser") {
    Day13.parse("Alice would gain 54 happiness units by sitting next to Bob.") shouldBe ("Alice", ("Bob", 54))
  }

  test("calculate happiness score of one setup") {
    val seating = "Alice,Bob,Carol,David".split(",").toVector
    val happinessMap = part1.calculateHappinessMap(input)
    Day13.calculateHappinessScore(seating, happinessMap) shouldBe 330
  }

  test("example 1") {

    solve(part1.calculateHappinessMap(input)) shouldBe 330

  }

}

class Day13Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}

/*
Alice
Bob
Carol
David
Eric
Frank
George
Mallory
 */
