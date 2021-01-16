package day05
import day05.Day05._
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = input.count(isNiceString)

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

object Day05 {

  def isNiceString(line: String): Boolean = {
    /*
It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
     */

    val vowels = "aeiou".toCharArray.toSet
    !line.contains("ab") &&
    !line.contains("cd") &&
    !line.contains("pq") &&
    !line.contains("xy") &&
    line.count(vowels.contains) >= 3 && {
      line.toCharArray
        .sliding(2)
        .exists(chars => chars(0) == chars(1))
    }

  }

}
