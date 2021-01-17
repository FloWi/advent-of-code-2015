package day06
import day06.Day06._
import helper.Helper._

import scala.reflect.ClassTag

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val grid = createGrid(width = 1000, height = 1000, default = false)
    val instructions = input.map(parseInstruction)

    instructions.foreach(applyInstruction(grid, _))
    val solution = grid.map(line => line.count(identity)).sum

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def applyInstruction(grid: Array[Array[Boolean]], instruction: Instruction): Unit = {
    instruction match {
      case TurnOn(start, endInclusive) =>
        for {
          x <- start.x.to(endInclusive.x)
          y <- start.y.to(endInclusive.y)
        } grid(y)(x) = true
      case TurnOff(start, endInclusive) =>
        for {
          x <- start.x.to(endInclusive.x)
          y <- start.y.to(endInclusive.y)
        } grid(y)(x) = false
      case Toggle(start, endInclusive) =>
        for {
          x <- start.x.to(endInclusive.x)
          y <- start.y.to(endInclusive.y)
        } grid(y)(x) = !grid(y)(x)
    }
  }
}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val grid = createGrid(width = 1000, height = 1000, default = 0)

    val instructions = input.map(parseInstruction)

    instructions.foreach(applyInstruction(grid, _))
    val solution = grid.map(line => line.sum).sum

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def applyInstruction(grid: Array[Array[Int]], instruction: Instruction): Unit = {
    val (lightnessIncreate, start, end) = instruction match {
      case TurnOn(start, endInclusive) =>
        (1, start, endInclusive)
      case TurnOff(start, endInclusive) =>
        (-1, start, endInclusive)
      case Toggle(start, endInclusive) =>
        (2, start, endInclusive)
    }

    for {
      x <- start.x.to(end.x)
      y <- start.y.to(end.y)
    } {
      grid(y)(x) += lightnessIncreate
      if (grid(y)(x) < 0) grid(y)(x) = 0
    }
  }

}

object Day06 {

  def createGrid[T: ClassTag](width: Int, height: Int, default: T): Array[Array[T]] = {
    Array.fill(height)(Array.fill(width)(default))
  }

  sealed trait Instruction

  case class Pos(x: Int, y: Int)

  case class TurnOn(start: Pos, endInclusive: Pos) extends Instruction
  case class TurnOff(start: Pos, endInclusive: Pos) extends Instruction
  case class Toggle(start: Pos, endInclusive: Pos) extends Instruction

  def parseCoords(str: String): (Pos, Pos) = {
    val List(start, end) = str.split(" through ").toList.map(parseCoord)
    (start, end)
  }

  def parseCoord(str: String): Pos = {
    val List(x, y) = str.split(",").toList
    Pos(x.toInt, y.toInt)
  }

  def parseInstruction(str: String): Instruction = {
    if (str.startsWith("turn on ")) {
      val (start, end) = parseCoords(str.replace("turn on ", ""))

      TurnOn(start, end)

    } else if (str.startsWith("turn off ")) {
      val (start, end) = parseCoords(str.replace("turn off ", ""))
      TurnOff(start, end)

    } else if (str.startsWith("toggle ")) {
      val (start, end) = parseCoords(str.replace("toggle ", ""))
      Toggle(start, end)

    } else ???
  }
}
