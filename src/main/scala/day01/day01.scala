package day01
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    assert(input.length == 1)
    val solution = solve(input.head)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(line: String): Int = {
    val open = line.count(_ == '(')
    val closed = line.count(_ == ')')
    open - closed
  }

}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    assert(input.length == 1)
    val solution = solve(input.head)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(line: String): Int = {

    // Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1).
    // The first character in the instructions has position 1, the second character has position 2, and so on.
    line.toList
      .scanLeft((0, 0)) { case ((acc, pos), cur) =>
        val value =
          if (cur == '(') 1
          else -1
        (acc + value, pos + 1)
      }
      .find(_._1 == -1)
      .get
      ._2

  }
}
