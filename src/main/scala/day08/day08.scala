package day08
import cats.parse.{Parser1, Parser => P}
import helper.Helper._

object part1 {
  def solve(input: List[String]): Int = {

    val results = input.map(inp => (inp, parser.parse(inp)))

    results.foreach(println)

    val resultLengths = results.map {
      case (_, Right((_, res))) =>
        res.length
      case (input, Left(error)) =>
        println(s"\nparsing error. \nInput: $input")
        println(s"Error: $error")

        0

    }

    val inputLengths = input.map(_.length)

    inputLengths.sum - resultLengths.sum
  }

  val parser: P[String] = {

    val backslash: Parser1[String] = P.char('\\').string.backtrack
    val hexDigit = P.charIn('a'.to('f') ++ ('0' to '9')).backtrack
    val twoDigits = (hexDigit ~ hexDigit).map { case (c1, c2) =>
      val intCode = Integer.parseInt(s"$c1$c2", 16)
      new String(Character.toChars(intCode))
    }.backtrack

    val doubleQuote = P.string1(P.char('"')).backtrack

    val hexCode: Parser1[String] = (P.char('x') *> twoDigits).backtrack
    val escaped: Parser1[String] = (P.char('\\') *> P.oneOf(List(backslash, hexCode, doubleQuote))).backtrack

    val word = P.rep1(P.charIn('a' to 'z'), 1).map(_.toList.mkString).backtrack

    val parser: Parser1[String] = P.oneOf1(List(escaped, word)).backtrack

    val p: P[String] = P.rep(parser).map(_.mkString).surroundedBy(P.char('"'))

    p

  }

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input)

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

object Day08 {}
