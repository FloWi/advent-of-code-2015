package day05
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

object part2 {

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

  def isNiceString(line: String): Boolean = {
    /*
       - it contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
       - it contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.     */

    val twoLetterPairs = line.sliding(2).toSet
    val indexesByTwoLetterPair = twoLetterPairs.map(pair => (pair, findIndexesOfPattern(line, pair)))
    val relevantTwoLetterPairs = indexesByTwoLetterPair.filter(_._2.length > 1)
    val enoughDistancedTwoLetterPairs = relevantTwoLetterPairs.filter { case (_, l) => (l.max - l.min) >= 2 }

    val containsTwoLetterPair = enoughDistancedTwoLetterPairs.nonEmpty

    val containsTripleWithMatchingHeadAndLast = line.sliding(3).exists { str =>
      str.head == str.last
    }

    containsTwoLetterPair && containsTripleWithMatchingHeadAndLast

  }

  def findIndexesOfPattern(line: String, pattern: String): List[Int] = {

    def helper(currentIndex: Int, acc: List[Int]): List[Int] = {
      if (line.isEmpty) acc.reverse
      else {
        val idx = line.indexOf(pattern, currentIndex)
        if (idx < 0) {
          acc.reverse
        } else {
          val rest = line.substring(idx + pattern.length)
          helper(idx + pattern.length, idx :: acc)
        }
      }
    }
    helper(0, List.empty)
  }
}
