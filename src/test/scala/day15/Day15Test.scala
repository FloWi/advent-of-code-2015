package day15

import day15.Day15Data._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object Day15Data {
  val input = """
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
""".trim.split("\n").toList

  def testCombinations = {
    for {
      i <- 1.to(100)
      j <- 1.to(100 - i)
      if i + j == 100
    } yield Vector(i, j)

  }

}

class Day15Part1Test extends AnyFunSuite with Matchers {

  test("parse") {
    Day15.parseIngredients(input) shouldBe List(
      "Butterscotch" -> List("capacity" -> -1, "durability" -> -2, "flavor" -> 6, "texture" -> 3, "calories" -> 8),
      "Cinnamon" -> List("capacity" -> 2, "durability" -> 3, "flavor" -> -2, "texture" -> -1, "calories" -> 3)
    )
  }

  test("ranges") {
    val combinations = Day15.combinations

    combinations.size shouldBe 156849
  }

  test("calculating score") {
    val combination = Vector(44, 56)
    val ingredientList = Day15.parseIngredients(input)
    val flipped = Day15.flipList(ingredientList)
    val score = Day15.calculateScoreForCombination(flipped, combination)
    score shouldBe 62842880
  }

  test("solve excercise 1") {
    part1.solve(input, testCombinations) shouldBe 62842880
  }

}

class Day15Part2Test extends AnyFunSuite with Matchers {

  test("solve excercise 1") {
    part2.solve(input, testCombinations) shouldBe 57600000
  }

}
