package day09
import helper.Helper._

object part1 {

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

  def solve(input: List[String]) = {
    // only eight cities
    // 8! --> 40320 routes
    // no need for better tsp algorithm - brute force is fast enough
    val distances = input.map(Day09.parse)
    val distancesMap = Day09.createDistanceMap(distances)
    val routes = Day09.calculateRoutes(distancesMap)

    val shortestRoute = routes.minBy(_._2)
    shortestRoute._2
  }
}

object part2 {

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

  def solve(input: List[String]) = {
    val distances = input.map(Day09.parse)
    val distancesMap = Day09.createDistanceMap(distances)
    val routes = Day09.calculateRoutes(distancesMap)

    val longestRoute = routes.maxBy(_._2)
    longestRoute._2
  }
}

object Day09 {

  def parse(line: String): (String, String, Int) = {
    //London to Dublin = 464
    import cats.parse.{Numbers, Parser => P}
    val word = P.ignoreCaseCharIn('a'.to('z')).rep1(1).string

    val cities = P.rep1Sep(word, 2, P.string(" to "))

    val distanceParser = ((cities <* P.string(" = ")) ~ Numbers.digits).map { case (nel, distance) =>
      val List(from, to) = nel.toList
      (from, to, distance.toInt)
    }

    val result = distanceParser.parseAll(line)

    result match {
      case Left(value)  => throw new RuntimeException(s"Error during parsing: $value")
      case Right(value) => value
    }
  }

  def createDistanceMap(distances: List[(String, String, Int)]): Map[String, Map[String, Int]] = {

    val distancesBothDirections = distances ++ distances.map { case (from, to, distance) =>
      (to, from, distance)
    }
    val map = distancesBothDirections.map(_._1).distinct.map(from => from -> Map.empty[String, Int]).toMap

    distancesBothDirections.foldLeft(map) { case (acc, (from, to, distance)) =>
      val updatedFromMap = acc(from).updated(to, distance)
      acc.updated(from, updatedFromMap)
    }
  }

  def calculateRouteLength(distanceMap: Map[String, Map[String, Int]], cities: List[String]): Int = {
    cities
      .sliding(2)
      .map { pair =>
        val List(from, to) = pair
        distanceMap(from)(to)
      }
      .sum
  }

  def calculateRoutes(distanceMap: Map[String, Map[String, Int]]): List[(List[String], Int)] = {
    val permutations = distanceMap.keySet.toList.permutations.toList

    permutations.map { cities =>
      (cities, calculateRouteLength(distanceMap, cities))
    }

  }

}
