package day13
import com.typesafe.scalalogging.LazyLogging
import day13.Day13.solve
import helper.Helper._

object part1 extends LazyLogging {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(calculateHappinessMap(input))

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def calculateHappinessMap(input: List[String]): Map[String, Map[String, Int]] = {
    val happinessScores = input.map(Day13.parse)
    val initialMap = happinessScores.map(_._1).distinct.map(name => name -> Map.empty[String, Int]).toMap

    happinessScores.foldLeft(initialMap) { case (acc, (p1, (p2, score))) =>
      val map = acc(p1)
      acc.updated(p1, map.updated(p2, score))
    }
  }
}

object part2 extends LazyLogging {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(calculateHappinessMap(input))

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def calculateHappinessMap(input: List[String]): Map[String, Map[String, Int]] = {
    val happinessScores = input.map(Day13.parse)
    val people = happinessScores.map(_._1)
    val updatedHappinessScores = happinessScores ::: people.flatMap { p =>
      List(
        ("me", (p, 0)),
        (p, ("me", 0))
      )
    }

    val initialMap = updatedHappinessScores.map(_._1).distinct.map(name => name -> Map.empty[String, Int]).toMap

    updatedHappinessScores.foldLeft(initialMap) { case (acc, (p1, (p2, score))) =>
      val map = acc(p1)
      acc.updated(p1, map.updated(p2, score))
    }
  }
}

object Day13 extends LazyLogging {

  def parse(line: String): (String, (String, Int)) = {

    //Alice would gain 54 happiness units by sitting next to Bob.

    val tokens = line.split("[\\s.]")
    val p1 = tokens(0)
    val gainOrLose = tokens(2)
    val score = tokens(3).toInt
    val p2 = tokens(10)

    (p1, (p2, score * (if (gainOrLose == "gain") 1 else -1)))

  }

  def solve(happinessMap: Map[String, Map[String, Int]]): Int = {

    val allSetups: Vector[Vector[String]] =
      happinessMap.keySet.toVector.permutations.toVector

    allSetups.zipWithIndex.map { case (setup, idx) =>
      if (idx % 1000 == 0) {
        logger.debug(s"checking setup #$idx of ${allSetups.length}")
      }
      calculateHappinessScore(setup, happinessMap)
    }.max

  }

  def calculateHappinessScore(seating: Vector[String], happinessMap: Map[String, Map[String, Int]]): Int = {
    seating.indices.map { i =>
      val leftNeighborIdx = if (i == 0) seating.size - 1 else i - 1
      val rightNeighborIdx = if (i == seating.size - 1) 0 else i + 1

      val leftNeighbor = seating(leftNeighborIdx)
      val me = seating(i)
      val rightNeighbor = seating(rightNeighborIdx)

      val myDiff = happinessMap(me)(leftNeighbor) + happinessMap(me)(rightNeighbor)
      myDiff
    }.sum
  }

}
