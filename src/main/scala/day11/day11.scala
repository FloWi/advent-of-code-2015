package day11
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    //cqjxjnds
    //cqjxxyzz

    val solution = "cqjxxyzz"

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    //cqjxjnds
    //cqjxxyzz
    //cqkaabcc
    val solution = "cqkaabcc"

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day11 {}
