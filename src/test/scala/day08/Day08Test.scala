package day08

import helper.Helper.source
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day08Part1Test extends AnyFunSuite with Matchers {

  val input = source(Some("src/test/resources/day08-example.txt"))
    .getLines()
    .filterNot(_.isEmpty())
    .toList

  test("example") {
    part1.solve(input) shouldBe 23 - 11
  }

  test("example 1") {
    part1.solve(input.take(1)) shouldBe 2 - 0
  }
  test("example 2") {
    part1.solve(input.slice(1, 2)) shouldBe 5 - 3
  }

  test("example 3") {
    part1.solve(input.slice(2, 3)) shouldBe 10 - 7
  }

  test("example 4") {
    part1.solve(input.slice(3, 4)) shouldBe 6 - 1
  }

  test("surrounded by") {
    import cats.parse.{Parser => P}

    val word = P.rep(P.charIn('a' to 'z')).map(_.mkString)
    val p = word.surroundedBy(P.char('"'))
    p.parse(""""aaa"""") shouldBe Right(("", "aaa"))
  }

  test("hexcode") {
    val input = """"v\x27""""
    val actual = part1.parser.parse(input)

    actual shouldBe Right(("", "v'"))

  }

}

class Day08Part2Test extends AnyFunSuite with Matchers {

  val input = source(Some("src/test/resources/day08-example.txt"))
    .getLines()
    .filterNot(_.isEmpty())
    .toList

  test("example") {
    part2.solve(input) shouldBe 42 - 23
  }

  test("example 1") {
    part2.solve(input.take(1)) shouldBe 6 - 2
  }
  test("example 2") {
    part2.solve(input.slice(1, 2)) shouldBe 9 - 5
  }

  test("example 3") {
    part2.solve(input.slice(2, 3)) shouldBe 16 - 10
  }

  test("example 4") {
    part2.solve(input.slice(3, 4)) shouldBe 11 - 6
  }

}
