package day07
import day07.Day07.{createCircuit, runCircuit}
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

  def solve(lines: List[String]): UShort = {
    val circuit = createCircuit(lines)
    val result = runCircuit(circuit)
    result("a")
  }
}

object part2 {

  def main(args: Array[String]) = {

    val input = source(args.headOption)
      .getLines()
      .filterNot(_.isEmpty())
      .toList

    val solution = ???

    println(
      s"Solution for ${getCallingMainClass.getCanonicalName}: $solution"
    )
  }
}

object Day07 {
  sealed trait Instruction {
    def wire: String
    def requiredInput: List[String]
  }
  case class ProvideValue(wire: String, value: UShort) extends Instruction {
    val requiredInput = List.empty[String]
  }

  case class Not(inputWire: String, wire: String) extends Instruction {
    val requiredInput = List(inputWire)
  }

  case class And(inputWire1: String, inputWire2: String, wire: String) extends Instruction {
    val requiredInput = List(inputWire1, inputWire2)
  }

  case class AndWithConstant(value: UShort, inputWire: String, wire: String) extends Instruction {
    val requiredInput = List(inputWire)
  }

  case class Or(inputWire1: String, inputWire2: String, wire: String) extends Instruction {
    val requiredInput = List(inputWire1, inputWire2)
  }

  case class LShift(inputWire: String, shift: Int, wire: String) extends Instruction {
    val requiredInput = List(inputWire)
  }

  case class RShift(inputWire: String, shift: Int, wire: String) extends Instruction {
    val requiredInput = List(inputWire)
  }

  case class PassThrough(inputWire: String, wire: String) extends Instruction {
    val requiredInput = List(inputWire)
  }

  def createCircuit(lines: List[String]): Map[String, Instruction] = {
    lines.map { line =>
      val instruction = parseInstruction(line)
      instruction.wire -> instruction
    }.toMap
  }

  def resolveValue(circuit: Map[String, Instruction], resolved: Map[String, UShort], instruction: Instruction): UShort = {
    instruction match {
      case ProvideValue(wire, value) =>
        value

      case PassThrough(inputWire, wire) =>
        resolved(inputWire)

      case Not(inputWire, wire) =>
        val input = resolved(inputWire)
        (~input)

      case And(inputWire1, inputWire2, wire) =>
        val input1 = resolved(inputWire1)
        val input2 = resolved(inputWire2)
        (input1 & input2)

      case AndWithConstant(constant, inputWire, wire) =>
        val input = resolved(inputWire)
        (constant & input)

      case Or(inputWire1, inputWire2, wire) =>
        val input1 = resolved(inputWire1)
        val input2 = resolved(inputWire2)
        (input1 | input2)

      case LShift(inputWire, shift, wire) =>
        val input = resolved(inputWire)
        (input << shift)

      case RShift(inputWire, shift, wire) =>
        val input = resolved(inputWire)
        (input >> shift)

    }
  }

  def runCircuit(circuit: Map[String, Instruction]): Map[String, UShort] = {

    def helper(acc: Map[String, UShort]): Map[String, UShort] = {
      val nonResolved = circuit.filterNot(kv => acc.contains(kv._1))
      if (nonResolved.isEmpty)
        acc
      else {
        val resolvable = nonResolved.filter { case (_, inst) => inst.requiredInput.forall(wire => acc.contains(wire)) }

        val newlyResolved = resolvable.map { case (wire, instr) =>
          val value: UShort = resolveValue(circuit, acc, instr)
          wire -> value
        }

        helper(acc ++ newlyResolved)
      }
    }

    helper(Map.empty)

  }

  def parseInstruction(str: String): Instruction = {
    if (str.head.isDigit) {
      if (str.contains("1 AND")) {
        //1 AND cx -> cy
        val tokens = str.split(" ")
        AndWithConstant(UShort(1), tokens(2), tokens(4))
      } else {
        val List(value, wire) = str.split(" -> ").toList
        ProvideValue(wire, UShort(value.toShort))
      }
    } else {
      val tokens = str.split(" ")
      if (str.startsWith("NOT")) {
        //NOT x -> h
        Not(tokens(1), tokens(3))
      } else if (str.contains("AND")) {
        //x AND y -> d
        And(tokens(0), tokens(2), tokens(4))
      } else if (str.contains("LSHIFT")) {
        //x LSHIFT 2 -> f
        LShift(tokens(0), tokens(2).toInt, tokens(4))
      } else if (str.contains("RSHIFT")) {
        //x RSHIFT 2 -> f
        RShift(tokens(0), tokens(2).toInt, tokens(4))
      } else if (str.contains("OR")) {
        //x OR y -> d
        Or(tokens(0), tokens(2), tokens(4))
      } else {
        //lx -> a
        PassThrough(tokens(0), tokens(2))
      }

    }
  }

}
