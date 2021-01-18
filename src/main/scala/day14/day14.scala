package day14
import helper.Helper._

import scala.concurrent.duration.FiniteDuration

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, 2503)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], seconds: Int): Long = {
    val winner = input
      .map(Day14.parse)
      .map(reindeer => (reindeer, Day14.calcDistanceAfter(reindeer, seconds)))
      .maxBy(_._2)

    winner._2
  }

}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input, 2503)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }

  def solve(input: List[String], seconds: Int): Long = {
    val reindeers = input
      .map(Day14.parse)

    val points = Day14.calcPoints(reindeers, seconds)
    points.values.max
  }
}

object Day14 {

  case class Reindeer(name: String, maxSpeedKmPerS: Int, activeDuration: FiniteDuration, restDuration: FiniteDuration) {}

  def parse(line: String): Reindeer = {
    import scala.concurrent.duration._
    //Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    val tokens = line.split(" ")
    val name = tokens(0)
    val maxSpeed = tokens(3)
    val activeDuration = tokens(6)
    val restDuration = tokens(13)

    Reindeer(name, maxSpeed.toInt, activeDuration.toInt.seconds, restDuration.toInt.seconds)
  }

  def calcDistanceAfter(reindeer: Reindeer, seconds: Int): Long = {
    val Reindeer(_, maxSpeedKmPerS, activeDuration, restDuration) = reindeer
    val wakeSleepCycleLength = activeDuration + restDuration

    val numberOfSleepWakeCycles = seconds / wakeSleepCycleLength.toSeconds
    val rest = seconds - numberOfSleepWakeCycles * wakeSleepCycleLength.toSeconds

    val restSeconds = if (rest > activeDuration.toSeconds) activeDuration.toSeconds else rest
    val movingSeconds = numberOfSleepWakeCycles * activeDuration.toSeconds + restSeconds
    val distance = maxSpeedKmPerS * movingSeconds
    distance
  }

  def calcPoints(reindeers: List[Reindeer], seconds: Int): Map[String, Int] = {

    1.to(seconds)
      .foldLeft(reindeers.map(r => r.name -> 0).toMap) { case (acc, second) =>
        val distances = reindeers.map(r => (r, calcDistanceAfter(r, second)))
        val max = distances.map(_._2).max

        val winners = distances.filter(_._2 == max)

        winners.foldLeft(acc) { case (map, (r, _)) =>
          map.updatedWith(r.name)(_.map(_ + 1))
        }

      }

  }

}
