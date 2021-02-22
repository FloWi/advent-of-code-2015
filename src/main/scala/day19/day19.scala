package day19
import day19.Day19.ParseResult
import helper.Helper._

import scala.annotation.tailrec
import scala.util.Random

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      //.filterNot(_.isEmpty())
      .toList

    val instructions = Day19.parse(input.mkString("\n"))

    val solution = Day19.findDistinctMolecules(instructions).size

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      //.filterNot(_.isEmpty())
      .toList

    val instructions = Day19.parse(input.mkString("\n"))

    val solution = findWayFromElectronToStartingMolecule(instructions)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  /*
e => H
e => O
H => HO
H => OH
O => HH
HOH

translated python-solution of what-a-baller
https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cu5b/

replace backwards until you found your destination
HOH --> HH -> O -> e
   */

  private def translationFromPythonPart2(parsed: ParseResult): Int = {

    val rules = parsed.replacementRules
    val rulesBackward: List[(String, String)] = rules.toList.flatMap { case (pattern, list) => list.map(replacement => (replacement, pattern)) }

    var target = parsed.startingMolecule
    var myRules = Random.shuffle(rulesBackward)
    var part2 = 0
    while (target != "e") {
      val tmp = target

      myRules.foreach { case (b, a) =>
        if (target.contains(b)) {
          val old = target
          val idx = target.indexOf(b)
          println(s"applied reverse rule $b -> $a at index $idx")
          target = target.replaceFirst(b, a)
          println(s"old target: $old")
          println(s"new target: $target\n")
          part2 += 1
        }
      }
      println(s"target: $target -- applied all rules.\n")

      if (tmp == target) {

        println(s"target: '$target' - no change found - starting over")
        target = parsed.startingMolecule
        part2 = 0
        myRules = Random.shuffle(myRules)
      }
    }
    part2
  }

  def findWayFromElectronToStartingMolecule(parsed: ParseResult): Int = {

    val rules = parsed.replacementRules
    val rulesBackward: List[(String, String)] = rules.toList.flatMap { case (pattern, list) => list.map(replacement => (replacement, pattern)) }

    @tailrec
    def helper(current: String, rules: List[(String, String)], depth: Int): Int = {
      if (current == "e") {
        depth
      } else {
        val (moleculeAfterAllRules, replacementCnt) = rules.foldLeft((current, 0)) { case ((str, replacementCnt), (b, a)) =>
          if (str.contains(b)) {
            (str.replaceFirst(b, a), replacementCnt + 1)
          } else (str, replacementCnt)
        }

        if (moleculeAfterAllRules == current) {
          //dead-end - start over with different rule-order
          helper(parsed.startingMolecule, Random.shuffle(rules), 0)
        } else {
          helper(moleculeAfterAllRules, rules, depth + replacementCnt)
        }
      }
    }

    helper(parsed.startingMolecule, rulesBackward, 0)
  }
}

object Day19 {

  def findDistinctMolecules(replacementRules: Map[String, List[String]], startingMolecule: String): List[String] = {
    val result = replacementRules.toList.flatMap { case (pattern, replacements) =>
      val matches = pattern.r.findAllMatchIn(startingMolecule).toList
      replacements.flatMap { replacement =>
        matches.map { m =>
          val before = startingMolecule.substring(0, m.start)
          val after = startingMolecule.substring(m.`end`)
          val result = before + replacement + after
          result
        }
      }
    }

    result.distinct
  }
  def findDistinctMolecules(parsed: ParseResult): List[String] = {
    val ParseResult(replacementRules, startingMolecule) = parsed
    findDistinctMolecules(replacementRules, startingMolecule)

  }

  case class ParseResult(replacementRules: Map[String, List[String]], startingMolecule: String)

  def parse(input: String): ParseResult = {
    val List(replacementRulesString, startingMoleculeString) = input.split("\n\n").toList.map(_.trim)

    val replacementMap: Map[String, List[String]] = replacementRulesString.linesIterator
      .map { line =>
        val List(k, v) = line.split(" => ").toList
        k -> v
      }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .toMap

    ParseResult(replacementMap, startingMoleculeString)
  }

}
