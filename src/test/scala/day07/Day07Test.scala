package day07

import Day07._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day07Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    val input = """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
    """

    val expected: Map[String, UShort] = Map(
      "d" -> 72,
      "e" -> 507,
      "f" -> 492,
      "g" -> 114,
      "h" -> 65412,
      "i" -> 65079,
      "x" -> 123,
      "y" -> 456
    ).mapValues(UShort.apply).toMap

    val circuit = createCircuit(input.trim.split("\n").toList)
    val actual = runCircuit(circuit)
    actual shouldBe expected

  }

  test("parse instruction ProvideValue") {
    parseInstruction("123 -> x") shouldBe ProvideValue("x", UShort(123))
  }

  test("parse instruction Not") {
    parseInstruction("NOT x -> h") shouldBe Not("x", "h")
  }

  test("parse instruction And") {
    parseInstruction("x AND y -> d") shouldBe And("x", "y", "d")
  }

  test("parse instruction AndWithConstant") {
    parseInstruction("1 AND y -> d") shouldBe AndWithConstant(UShort(1), "y", "d")
  }

  test("parse instruction OR") {
    parseInstruction("x OR y -> d") shouldBe Or("x", "y", "d")
  }

  test("parse instruction LShift") {
    parseInstruction("x LSHIFT 2 -> f") shouldBe LShift("x", 2, "f")
  }

  test("parse instruction RShift") {
    parseInstruction("x RSHIFT 2 -> f") shouldBe RShift("x", 2, "f")
  }

  test("parse instruction PassThrough") {
    parseInstruction("lx -> a") shouldBe PassThrough("lx", "a")
  }

}

class Day07Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
