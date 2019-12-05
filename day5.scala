import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("./day5.input")
    val inputs = source.getLines().next().split(',').map(_.toInt)

    processInput(inputs, 0)
    processInputSecond(inputs, 0)

  }

  // First
  def processInput(inputs: Array[Int], current: Int): Int = {
    val ins = parseInstruction(inputs(current))
    ins._4 match {
      case 1 => {
        val res = getParamByMode(ins._3, inputs, current+1) + getParamByMode(ins._2, inputs, current+2)
        val s = inputs.splitAt(inputs(current+3))
        processInput({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 2 => {
        val res = getParamByMode(ins._3, inputs, current+1) * getParamByMode(ins._2, inputs, current+2)
        val s = inputs.splitAt(inputs(current+3))
        processInput({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 3 => {
        val s = inputs.splitAt(inputs(current+1))
        // ...by running an input instruction - provide it 1
        processInput({s._1 :+ 1} ++ s._2.tail, current + 2)
      }
      case 4 => {
        println(getParamByMode(ins._3, inputs, current+1))
        processInput(inputs, current + 2)
      }
      case 99 =>
        inputs(0)
    }
  }

  // Second
  // TODO: Check orElse or more composition, it's too late now :-)
  def processInputSecond(inputs: Array[Int], current: Int): Unit = {
    val ins = parseInstruction(inputs(current))
    ins._4 match {
      case 1 => {
        val res = getParamByMode(ins._3, inputs, current+1) + getParamByMode(ins._2, inputs, current+2)
        val s = inputs.splitAt(inputs(current+3))
        processInputSecond({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 2 => {
        val res = getParamByMode(ins._3, inputs, current+1) * getParamByMode(ins._2, inputs, current+2)
        val s = inputs.splitAt(inputs(current+3))
        processInputSecond({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 3 => {
        val s = inputs.splitAt(inputs(current+1))
        // when the TEST diagnostic program runs its input instruction to get the ID of the system to test, provide it 5
        processInputSecond({s._1 :+ 5} ++ s._2.tail, current + 2)
      }
      case 4 => {
        println(getParamByMode(ins._3, inputs, current+1))
        processInputSecond(inputs, current + 2)
      }
      case 5 => {
        val next = if (getParamByMode(ins._3, inputs, current+1) != 0) getParamByMode(ins._2, inputs, current+2) else current + 3
        processInputSecond(inputs, next)
      }
      case 6 => {
        val next = if (getParamByMode(ins._3, inputs, current+1) == 0) getParamByMode(ins._2, inputs, current+2) else current + 3
        processInputSecond(inputs, next)
      }
      case 7 => {
        val res = if (getParamByMode(ins._3, inputs, current+1) < getParamByMode(ins._2, inputs, current+2)) 1 else 0
        val s = inputs.splitAt(inputs(current+3))
        processInputSecond({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 8 => {
        val res = if (getParamByMode(ins._3, inputs, current+1) == getParamByMode(ins._2, inputs, current+2)) 1 else 0
        val s = inputs.splitAt(inputs(current+3))
        processInputSecond({s._1 :+ res} ++ s._2.tail, current + 4)
      }
      case 99 => ()
    }
  }

  def getParamByMode(mode: Int, inputs: Array[Int], index: Int): Int = {
    if (mode == 0) inputs(inputs(index)) else inputs(index)
  }

  def parseInstruction(i: Int): (Int, Int, Int, Int) = {
    (i/10000,i/1000%10,i/100%10,i%100)
  }

}