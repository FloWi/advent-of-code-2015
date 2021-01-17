package day06

import day06.Day06._
import day06.part1._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day06Part1Test extends AnyFunSuite with Matchers {
  /*
    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
   */

  test("example 1") {}

  test("parse instruction") {
    Day06.parseInstruction("turn on 0,0 through 2,2") shouldBe TurnOn(Pos(0, 0), Pos(2, 2))
    Day06.parseInstruction("turn off 0,0 through 2,2") shouldBe TurnOff(Pos(0, 0), Pos(2, 2))
    Day06.parseInstruction("toggle 0,0 through 2,2") shouldBe Toggle(Pos(0, 0), Pos(2, 2))
  }

  test("turn on") {
    val grid = createGrid(3, 3, false)
    applyInstruction(grid, TurnOn(Pos(0, 0), Pos(0, 0)))
    grid.flatten.count(identity) shouldBe 1
  }

  test("toggle") {
    val grid = createGrid(3, 3, false)
    applyInstruction(grid, Toggle(Pos(0, 0), Pos(0, 0)))
    grid.flatten.count(identity) shouldBe 1
    applyInstruction(grid, Toggle(Pos(0, 0), Pos(0, 0)))
    grid.flatten.count(identity) shouldBe 0
  }
  test("have you tried turning it on and off again") {
    val grid = createGrid(3, 3, false)
    applyInstruction(grid, TurnOn(Pos(0, 0), Pos(0, 0)))
    applyInstruction(grid, TurnOff(Pos(0, 0), Pos(0, 0)))
    grid.flatten.count(identity) shouldBe 0
  }

}

class Day06Part2Test extends AnyFunSuite with Matchers {

  test("example 1") {}

}
