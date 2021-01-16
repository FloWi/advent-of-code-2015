package day05

import day05._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day05Part1Test extends AnyFunSuite with Matchers {

  test("example 1") {

    /*
    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
    aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.
     */

    Day05.isNiceString("ugknbfddgicrmopn") shouldBe true
    Day05.isNiceString("aaa") shouldBe true
    Day05.isNiceString("jchzalrnumimnmhp") shouldBe false
    Day05.isNiceString("haegwjzuvuyypxyu") shouldBe false
    Day05.isNiceString("dvszwmarrgswjxmb") shouldBe false
  }

}

class Day05Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
