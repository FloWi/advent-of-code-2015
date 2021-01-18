package day10
import com.typesafe.scalalogging.LazyLogging
import day10.Day10.solve
import helper.Helper._

object part1 extends LazyLogging {

  //13:06:28.598 [main] DEBUG day10.Day10$ - playing round #1
  //13:06:30.602 [main] DEBUG day10.Day10$ - played round #40. Result length: 492982

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input.head, 40)

    logger.info(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object part2 extends LazyLogging {

  //13:06:28.598 [main] DEBUG day10.Day10$ - playing round #1
  //13:06:30.602 [main] DEBUG day10.Day10$ - played round #40. Result length: 492982
  //13:07:24.612 [main] DEBUG day10.Day10$ - played round #50. Result length: 6989950

  //13:09:34.972 [main] DEBUG day10.Day10$ - played round #49. Result length: 5361162
  //13:09:51.617 [main] DEBUG day10.Day10$ - played round #50. Result length: 6989950

  //John Conway showed that you can expand subparts of the game in parallel, if you split it at a group-break.
  //so, the input for round 50 was 5361162 chars long and solving it took 16s

  //let's say we split it in 10 parts à ~500k chars
  // one 500k part solve happened in round #41 and took around 350ms. Since it can run in parallel we could solve it in 350ms instead of

  //13:08:55.813 [main] DEBUG day10.Day10$ - played round #40. Result length: 492982
  //13:08:56.170 [main] DEBUG day10.Day10$ - played round #41. Result length: 643280

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = solve(input.head, 50)

    logger.info(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day10 extends LazyLogging {

  def solve(input: String, numberOfTurns: Int): Int = {
    val gameOutput = 1
      .to(numberOfTurns)
      .foldLeft(input) { case (currentString, roundNo) =>
        logger.debug(s"playing round #$roundNo")
        val roundResult = Day10.lookAndSay(currentString)
        logger.debug(s"played round #$roundNo. Result length: ${roundResult.length}")
        roundResult
      }

    gameOutput.length
  }

  def lookAndSay(str: String): String = {

    val groupBreaks = str
      .sliding(2)
      .toList
      .zipWithIndex
      .flatMap { case (list, index) =>
        if (list.distinct.length == 1) List.empty
        else List(index)
      }

    /*
111221133

0    1
1    1
2    1  <-
3    2
4    2  <-
5    1
6    1  <-
7    3
8    3  <-

group breaks: 2, 4, 6

groups:
0-2  length: 3
3-4  length: 2
5-6  length: 2
7-8  length: 2
     */

    //println("group breaks")
    //groupBreaks.foreach(println)

    val groups = (-1 :: groupBreaks ::: List(str.length - 1))
      .sliding(2)
      .toList
      .map { case List(from, to) =>
        (from + 1, to)
      }
      .map { case (from, to) =>
        (from, to, to - from + 1, str(from))
      }

    //println("groups")
    //groups.foreach(println)

    groups.map { case (_, _, qty, char) =>
      s"$qty$char"
    }.mkString

  }

}
