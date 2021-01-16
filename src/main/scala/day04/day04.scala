package day04
import day04.Day04._
import helper.Helper._

import java.security.MessageDigest
import com.typesafe.scalalogging.LazyLogging

object part1 extends LazyLogging {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    logger.info("starting search for correct md5-hash")
    val solution = solveNonPar(input.head, "00000")

    //runtime: 4.3s with lazyList
    //runtime: 2.15s with lazyList but only first 3 bytes
    //runtime: 1.8s with lazyList flatMap but only first 3 bytes
    //runtime: 0:10min with scala parallel lazyList but only first 3 bytes
    logger.info(s"Solution for ${getCallingMainClass.getCanonicalName}: $solution")
  }

}

object part2 extends LazyLogging {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    logger.info("starting search for correct md5-hash")
    val solution = solve(input.head, "000000")

    //runtime: 1:35min with scala lazyList
    //runtime: 0:23min with scala lazyList but only first 3 bytes
    //runtime: 0:21min with scala lazyList flatMap but only first 3 bytes
    //runtime: 0:07min with scala parallel lazyList but only first 3 bytes
    logger.info(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day04 {

  import scala.collection.parallel.CollectionConverters._

  def solve(secretKey: String, md5BeginningToMatch: String): Int = {
    val numberOfBytesToCheck = math.ceil(md5BeginningToMatch.length.toDouble / 2).toInt //two chars per bytes in hex encoding
    LazyList
      .range(0, 10 * 1000 * 1000)
      .par
      .flatMap { i =>
        val stringToHash = s"$secretKey$i"
        val md5Hash = MessageDigest.getInstance("MD5").digest(stringToHash.getBytes).take(numberOfBytesToCheck).map("%02X".format(_)).mkString
        if (md5Hash.startsWith(md5BeginningToMatch)) List(i) else List.empty
      }
      .take(1)
      .head
  }

  def solveNonPar(secretKey: String, md5BeginningToMatch: String): Int = {
    val numberOfBytesToCheck = math.ceil(md5BeginningToMatch.length.toDouble / 2).toInt //two chars per bytes in hex encoding
    LazyList
      .range(0, 10 * 1000 * 1000)
      .flatMap { i =>
        val stringToHash = s"$secretKey$i"
        val md5Hash = MessageDigest.getInstance("MD5").digest(stringToHash.getBytes).take(numberOfBytesToCheck).map("%02X".format(_)).mkString
        if (md5Hash.startsWith(md5BeginningToMatch)) List(i) else List.empty
      }
      .take(1)
      .head
  }

}
