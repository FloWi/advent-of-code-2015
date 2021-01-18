package day09

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day09Part1Test extends AnyFunSuite with Matchers {
  val input = """
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
""".trim.split("\n").toList

  test("parsing") {

    input.map(Day09.parse) should contain theSameElementsAs List(
      ("London", "Dublin", 464),
      ("London", "Belfast", 518),
      ("Dublin", "Belfast", 141)
    )
  }

  test("distances map") {
    val distances = input.map(Day09.parse)
    val distancesMap = Day09.createDistanceMap(distances)
    val actual = distancesMap.flatMap { case (from, toMap) => toMap.map { case (to, distance) => (from, to, distance) } }
    actual should contain theSameElementsAs List(
      ("London", "Dublin", 464),
      ("London", "Belfast", 518),
      ("Dublin", "Belfast", 141),
      ("Dublin", "London", 464),
      ("Belfast", "London", 518),
      ("Belfast", "Dublin", 141)
    )
  }

  test("all routes") {
    val distances = input.map(Day09.parse)
    val distancesMap = Day09.createDistanceMap(distances)
    val routes = Day09.calculateRoutes(distancesMap)

    routes should contain theSameElementsAs List(
      (List("Dublin", "London", "Belfast"), 982),
      (List("London", "Dublin", "Belfast"), 605),
      (List("London", "Belfast", "Dublin"), 659),
      (List("Dublin", "Belfast", "London"), 659),
      (List("Belfast", "Dublin", "London"), 605),
      (List("Belfast", "London", "Dublin"), 982)
    )
  }

  test("example 1") {
    part1.solve(input) shouldBe 605
  }

}

class Day09Part2Test extends AnyFunSuite with Matchers {

  val input = """
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
""".trim.split("\n").toList

  test("example 1") {
    part2.solve(input) shouldBe 982
  }

}
