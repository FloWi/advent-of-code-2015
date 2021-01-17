package day05

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

    part1.isNiceString("ugknbfddgicrmopn") shouldBe true
    part1.isNiceString("aaa") shouldBe true
    part1.isNiceString("jchzalrnumimnmhp") shouldBe false
    part1.isNiceString("haegwjzuvuyypxyu") shouldBe false
    part1.isNiceString("dvszwmarrgswjxmb") shouldBe false
  }

}

class Day05Part2Test extends AnyFunSuite with Matchers {

  test("find indexes of pattern") {
    part2.findIndexesOfPattern("xyxy", "xy") shouldBe List(0, 2)
  }

  test("example 1") {
    part2.isNiceString("qjhvhtzxzqqjkmpb") shouldBe true
    part2.isNiceString("xxyxx") shouldBe true
    part2.isNiceString("uurcxstgmygtbstg") shouldBe false
    part2.isNiceString("ieodomkazucvgmuy") shouldBe false

  }

}
