package day12
import helper.Helper._
import io.circe.Json
import io.circe.Json.{JArray, JNumber}

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

  def solve(lines: List[String]): Int = {
    lines.map { line =>
      io.circe.parser.parse(line) match {
        case Right(json) =>
          reduce(json)
      }
    }.sum
  }

  def reduce(json: Json): Int = {
    if (json.isArray) {
      json.asArray.get.map(reduce).sum
    } else if (json.isNumber) {
      json.asNumber.get.toInt.get
    } else if (json.isObject) {
      val obj = json.asObject.get
      obj.values.map(reduce).sum
    } else {
      throw new RuntimeException(s"unrecognized type. Json: $json")
    }
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

  def solve(lines: List[String]): Int = {
    lines.map { line =>
      io.circe.parser.parse(line) match {
        case Right(json) =>
          reduce(json)
      }
    }.sum
  }

  def reduce(json: Json): Int = {
    if (json.isArray) {
      json.asArray.get.map(reduce).sum
    } else if (json.isNumber) {
      json.asNumber.get.toInt.get
    } else if (json.isString) {
      0
    } else if (json.isObject) {
      val obj = json.asObject.get
      if (obj.values.filter(_.isString).map(_.asString.get).toList.contains("red")) {
        0
      } else {
        obj.values.map(reduce).sum
      }
    } else {
      throw new RuntimeException(s"unrecognized type. Json: $json")
    }
  }
}

object Day12 {}
