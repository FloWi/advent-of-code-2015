package day17
import day17.Day17.getCombinations
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, eggnogAmount = 150)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], eggnogAmount: Int): Int = {

    val validCombinations = getCombinations(input, eggnogAmount)
    validCombinations.size
  }
}

object part2 {

  /*
  Find the minimum number of containers that can exactly fit all 150 liters of eggnog.
  How many different ways can you fill that number of containers and still hold exactly 150 litres?

  In the example above, the minimum number of containers was two.
  There were three ways to use that many containers, and so the answer there would be 3.
   */

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, eggnogAmount = 150)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], eggnogAmount: Int): Int = {
    val combinations = getCombinations(input, eggnogAmount)
    val minimumNumberOfContainers = combinations.minBy(_.size).size

    val relevantCombinationsOfThisSize = combinations.filter(_.size == minimumNumberOfContainers)
    relevantCombinationsOfThisSize.size

  }

}

object Day17 {

  def getCombinations(input: List[String], eggnogAmount: Int): List[List[Int]] = {
    val containers = input.map(_.toInt).sorted.reverse

    getCombinations(containers, eggnogAmount, Nil)
  }

  def getCombinations(numbers: List[Int], target: Int, partial: List[Int], results: List[List[Int]] = List.empty): List[List[Int]] = {

    val s = partial.sum

    //check if the partial sum == target
    if (s == target) {
      println(s"sum(${partial.mkString(", ")} == $target)")
      partial :: results
    } else if (s > target) {
      results
    } else {
      numbers.indices.toList.flatMap { i =>
        val n = numbers(i)
        val remaining = numbers.drop(i + 1)
        getCombinations(remaining, target, n :: partial, results)
      }
    }
  }

}
