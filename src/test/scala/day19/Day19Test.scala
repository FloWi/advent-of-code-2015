package day19

import day19._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day19Part1Test extends AnyFunSuite with Matchers {

  val testInput = """
H => HO
H => OH
O => HH

HOH
  """.trim

  test("example 1 - parsing") {

    val parsed = Day19.parse(testInput)

    parsed.replacementRules shouldBe Map(
      "H" -> List("HO", "OH"),
      "O" -> List("HH")
    )

    parsed.startingMolecule shouldBe "HOH"
  }

  test("example 1") {

    val parsed = Day19.parse(testInput)

    Day19.findDistinctMolecules(parsed) should contain theSameElementsAs List("HOOH", "HOHO", "OHOH", "HOOH", "HHHH").distinct
    Day19.findDistinctMolecules(parsed).size shouldBe 4
  }

  test("example 2") {

    val parsed = Day19.parse(testInput)

    Day19.findDistinctMolecules(parsed.copy(startingMolecule = "HOHOHO")).size shouldBe 7

  }
}

class Day19Part2Test extends AnyFunSuite with Matchers {

  val testInput = """
e => H
e => O
H => HO
H => OH
O => HH

HOH
  """.trim

  test("example 1") {

    val parsed = Day19.parse(testInput)

    parsed.replacementRules shouldBe Map(
      "e" -> List("H", "O"),
      "H" -> List("HO", "OH"),
      "O" -> List("HH")
    )

    /*
    starts from e and work your way towards the goal

    e --> H --> OH --> HHH --> dead end - too long
    e --> O --> HH --> HOH --> success
     */

    parsed.startingMolecule shouldBe "HOH"

    part2.findWayFromElectronToStartingMolecule(parsed)
    part2.findWayFromElectronToStartingMolecule(parsed) shouldBe 3
  }
}
