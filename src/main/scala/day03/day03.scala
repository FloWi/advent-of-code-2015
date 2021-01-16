package day03
import cats.instances.char
import day03.Day03.Pos
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input.head)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(line: String): Int = {
    val (lastPos, visited) = line.toCharArray
      .foldLeft((Pos(0, 0), Set(Pos(0, 0)))) { case ((currentPos, pastPositions), char) =>
        val offset = char match {
          case '^' => Pos(0, 1)
          case 'v' => Pos(0, -1)
          case '<' => Pos(-1, 0)
          case '>' => Pos(1, 0)
        }

        val newPos = currentPos + offset
        (newPos, pastPositions.+(newPos))
      }

    visited.size
  }

}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input.head)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(line: String): Int = {
    val (_, _, visited) = line.toCharArray.zipWithIndex
      .foldLeft((Pos(0, 0), Pos(0, 0), Set(Pos(0, 0)))) { case ((santaPos, roboSantaPos, pastPositions), (char, index)) =>
        val offset = char match {
          case '^' => Pos(0, 1)
          case 'v' => Pos(0, -1)
          case '<' => Pos(-1, 0)
          case '>' => Pos(1, 0)
        }

        val (newSantaPos, newRoboSantaPos) = if (index % 2 == 0) {
          (santaPos + offset, roboSantaPos)

        } else {
          (santaPos, roboSantaPos + offset)
        }

        (newSantaPos, newRoboSantaPos, pastPositions.+(newSantaPos).+(newRoboSantaPos))
      }

    visited.size
  }

}

object Day03 {
  case class Pos(x: Int, y: Int) {
    def +(other: Pos): Pos = {
      Pos(x + other.x, y + other.y)
    }
  }

}
