package day19
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = ???

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

    val solution = ???

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day19 {

  case class ParseResult(replacementRules: List[(String, String)], startingMolecule: String)

  def parse(input: String): ParseResult = {
    val List(replacementRulesString, startingMoleculeString) = input.split("\n\n").toList.map(_.trim)

    val replacementMap = replacementRulesString.linesIterator.map { line =>
      val List(k, v) = line.split(" => ").toList
      k -> v
    }.toList

    ParseResult(replacementMap, startingMoleculeString)
  }

}
