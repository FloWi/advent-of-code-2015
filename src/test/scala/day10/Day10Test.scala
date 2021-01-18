package day10

import day10.Day10._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Day

class Day10Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {
    lookAndSay("1") shouldBe "11" // one one
  }

  test("example 2") {
    lookAndSay("11") shouldBe "21" //two ones
  }

  test("example 3") {
    lookAndSay("21") shouldBe "1211" //one two and one one
  }

  test("example 4") {
    lookAndSay("1211") shouldBe "111221" //one one, one two, two ones
  }

  test("example 5") {
    lookAndSay("111221") shouldBe "312211" // three 1s, two 2s, and one 1.
  }

}

class Day10Part2Test extends AnyFunSuite with Matchers {

  test("test if running in parallel would be possible") {

    val input = "    13211321322113311213211331121113122112132113121113222112"
    val inputLeft = "1321132132211331121321133"
    val inputRight = "                        1121113122112132113121113222112"

    lookAndSay(inputLeft.trim) + lookAndSay(inputRight.trim) shouldBe lookAndSay(input.trim)
  }
}
