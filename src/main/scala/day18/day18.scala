package day18
import helper.Helper._

object part1 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val initial = Day18.parseState(input)

    val finalState = Day18.calcStateAfter(initial, 100)

    val solution = finalState.count(_._2)

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

    val initial = Day18.parseState(input)

    val finalState = Day18.calcStateAfter(initial, 100, cornerLampsAlwaysOn = true)

    val solution = finalState.count(_._2)

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day18 {

  type Location = (Int, Int)

  def parseState(lines: List[String]): Map[Location, Boolean] = {

    val state: Map[Location, Boolean] = lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex
        .map { case (char, x) =>
          (x, y) -> (char == '#')
        }
    }.toMap

    state
  }

  val neighbors = List(
    (0, 1), //North
    (1, 1), //NorthEast
    (1, 0), //East
    (1, -1), //SouthEast
    (0, -1), //South
    (-1, -1), //SouthWest
    (-1, 0), //West
    (-1, 1) //NorthWest
  )

  def calcNeighbors(state: Map[Location, Boolean], location: Location): List[Location] = {

    neighbors.flatMap { case (dX, dY) =>
      val (x, y) = location
      val neighborLocation = (x + dX, y + dY)

      if (state.keySet.contains(neighborLocation)) {
        List(neighborLocation)
      } else {
        Nil
      }
    }
  }

  def calcNumberOfLitNeighbors(state: Map[Location, Boolean], location: Location): Int = {
    val neighbors = calcNeighbors(state, location)

    neighbors.map(state.apply).count(identity)
  }

  def calcNewState(state: Map[Location, Boolean]): Map[Location, Boolean] = {

    state.map {
      case (loc, true) =>
        //A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise
        val cnt = calcNumberOfLitNeighbors(state, loc)
        loc -> (cnt == 2 || cnt == 3)

      case (loc, false) =>
        //A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise
        loc -> (calcNumberOfLitNeighbors(state, loc) == 3)
    }
  }

  def turnOnCornerLamps(state: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
    val maxX = state.keys.map(_._1).max
    val maxY = state.keys.map(_._2).max

    state
      .updated((0, 0), true)
      .updated((0, maxY), true)
      .updated((maxX, 0), true)
      .updated((maxX, maxY), true)
  }

  def showState(newState: Map[(Int, Int), Boolean]): List[String] = {
    val maxX = newState.keys.map(_._1).max
    val maxY = newState.keys.map(_._2).max

    0.to(maxY)
      .map { y =>
        0.to(maxX)
          .map { x =>
            val loc = (x, y)
            val lightState = newState(loc)
            if (lightState) "#" else "."
          }
          .mkString
      }
      .toList
  }

  def calcNewState(lines: List[String]): List[String] = {

    val state = parseState(lines)
    val newState = calcNewState(state)

    showState(newState)
  }

  def calcStateAfter(state: Map[(Int, Int), Boolean], n: Int, cornerLampsAlwaysOn: Boolean = false): Map[(Int, Int), Boolean] = {

    1.to(n).foldLeft(state) { case (s, _) =>
      val updated = calcNewState(s)
      if (cornerLampsAlwaysOn) turnOnCornerLamps(updated)
      else updated
    }
  }

}
