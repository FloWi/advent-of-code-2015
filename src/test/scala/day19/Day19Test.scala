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

  test("example 1") {

    val parsed = Day19.parse(testInput)

    parsed.replacementRules shouldBe Map(
      "H" -> List("HO", "OH"),
      "O" -> List("HH")
    )

    parsed.startingMolecule shouldBe "HOH"

  }

}

class Day19Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
