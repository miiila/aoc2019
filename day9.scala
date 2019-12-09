import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day9{

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("./day9.input")
    val inputs = ArrayBuffer(source.getLines().next().split(',').map(_.toLong): _*)
    inputs ++= Array.tabulate(1000)(_ => 0.toLong)

    // First
    intCodeComputer(inputs, 0, List(1), 0)

    // Second
    intCodeComputer(inputs, 0, List(2), 0)
  }

  @scala.annotation.tailrec
  def intCodeComputer(code: ArrayBuffer[Long], pointer: Long, inputs: List[Long], relativeBase: Long): Long = {
    val current = pointer.toInt
    val ins = parseInstruction(code(current))
    ins._4 match {
      case 1 => {
        val res = code(getParamAddress(ins._3, code, current + 1, relativeBase)) + code(getParamAddress(ins._2, code, current + 2, relativeBase))
        val s = code.splitAt(getParamAddress(ins._1, code, current + 3, relativeBase))
        intCodeComputer({
          s._1 :+ res.toLong
        } ++ s._2.tail, current + 4, inputs, relativeBase)
      }
      case 2 => {
        val res = code(getParamAddress(ins._3, code, current + 1, relativeBase)) * code(getParamAddress(ins._2, code, current + 2, relativeBase))
        val s = code.splitAt(getParamAddress(ins._1, code, current + 3, relativeBase))
        intCodeComputer({
                          s._1 :+ res.toLong
                        } ++ s._2.tail, current + 4, inputs, relativeBase)
      }
      case 3 => {
        val s = code.splitAt(code(current+3).toInt)
        intCodeComputer({
                          s._1 :+ inputs.head
                        } ++ s._2.tail, current + 2, inputs.tail, relativeBase)
      }
      case 4 => {
        println(code(getParamAddress(ins._3, code, current + 1, relativeBase)))
        intCodeComputer(code, current + 2, inputs, relativeBase)
      }
      case 5 => {
        val next = if (code(getParamAddress(ins._3, code, current + 1, relativeBase)) != 0) code(getParamAddress(ins._2, code, current + 2, relativeBase)) else current + 3
        intCodeComputer(code, next, inputs, relativeBase)
      }
      case 6 => {
        val next = if (code(getParamAddress(ins._3, code, current + 1, relativeBase)) == 0) code(getParamAddress(ins._2, code, current + 2, relativeBase)) else current + 3
        intCodeComputer(code, next, inputs, relativeBase)
      }
      case 7 => {
        val res = if (code(getParamAddress(ins._3, code, current + 1, relativeBase)) < code(getParamAddress(ins._2, code, current + 2, relativeBase))) 1 else 0
        val s = code.splitAt(getParamAddress(ins._1, code, current + 3, relativeBase))
        intCodeComputer({
                          s._1 :+ res.toLong
                        } ++ s._2.tail, current + 4, inputs, relativeBase)
      }
      case 8 => {
        val res = if (code(getParamAddress(ins._3, code, current + 1, relativeBase)) == code(getParamAddress(ins._2, code, current + 2, relativeBase))) 1 else 0
        val s = code.splitAt(getParamAddress(ins._1, code, current + 3, relativeBase))
        intCodeComputer({
                          s._1 :+ res.toLong
                        } ++ s._2.tail, current + 4, inputs, relativeBase)
      }
      case 9 => {
        val p = code(getParamAddress(ins._3, code, current + 1, relativeBase))
        intCodeComputer(code, current + 2, inputs, relativeBase + p)
      }
      case 99 => -1
    }
  }

  def getParamAddress(mode: Long, inputs: ArrayBuffer[Long], index: Long, relativeBase: Long): Int = {
    mode match {
      case 0 => inputs(index.toInt).toInt
      case 1 => index.toInt
      case 2 => {inputs(index.toInt) + relativeBase}.toInt
    }
  }

  def parseInstruction(i: Long): (Long, Long, Long, Long) = {
    (i / 10000, i / 1000 % 10, i / 100 % 10, i % 100)
  }

}