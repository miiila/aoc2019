import scala.io.Source

object Day7 {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("./day7.input")
    val inputs = source.getLines().next().split(',').map(_.toInt)

    // First
    println((0 to 4).permutations.map(amplifiers(_, inputs)).reduce(math.max))

    def amplifiers(settings: IndexedSeq[Int], inputs: Array[Int]): Int = {
      settings.foldLeft(0)((acc, setting) => {
        intCodeComputer(inputs, 0, List(setting, acc))
      })
    }

    def intCodeComputer(inputs: Array[Int], current: Int, functionInputs: List[Int]): Int = {
      val ins = parseInstruction(inputs(current))
      ins._4 match {
        case 1 => {
          val res = getParamByMode(ins._3, inputs, current + 1) + getParamByMode(ins._2, inputs, current + 2)
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputer({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 2 => {
          val res = getParamByMode(ins._3, inputs, current + 1) * getParamByMode(ins._2, inputs, current + 2)
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputer({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 3 => {
          val s = inputs.splitAt(inputs(current + 1))
          intCodeComputer({
            s._1 :+ functionInputs.head
          } ++ s._2.tail, current + 2, functionInputs.tail)
        }
        case 4 => {
          return getParamByMode(ins._3, inputs, current + 1)

        }
        case 5 => {
          val next = if (getParamByMode(ins._3, inputs, current + 1) != 0) getParamByMode(ins._2, inputs, current + 2) else current + 3
          intCodeComputer(inputs, next, functionInputs)
        }
        case 6 => {
          val next = if (getParamByMode(ins._3, inputs, current + 1) == 0) getParamByMode(ins._2, inputs, current + 2) else current + 3
          intCodeComputer(inputs, next, functionInputs)
        }
        case 7 => {
          val res = if (getParamByMode(ins._3, inputs, current + 1) < getParamByMode(ins._2, inputs, current + 2)) 1 else 0
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputer({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 8 => {
          val res = if (getParamByMode(ins._3, inputs, current + 1) == getParamByMode(ins._2, inputs, current + 2)) 1 else 0
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputer({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 99 => -1
      }
    }

    def getParamByMode(mode: Int, inputs: Array[Int], index: Int): Int = {
      if (mode == 0) inputs(inputs(index)) else inputs(index)
    }

    def parseInstruction(i: Int): (Int, Int, Int, Int) = {
      (i / 10000, i / 1000 % 10, i / 100 % 10, i % 100)
    }



    // Second
    println((5 to 9).permutations.map(amplifiersLoop(_, inputs)).reduce(math.max))

    def amplifiersLoop(settings: IndexedSeq[Int], inputs: Array[Int]): Int = {
      var last_code = 0
      var inputSettings = Array(
        (inputs, 0, settings(0), 0),
        (inputs, 0, settings(1), -1),
        (inputs, 0, settings(2), -1),
        (inputs, 0, settings(3), -1),
        (inputs, 0, settings(4), -1),
      )

      while (last_code != 99) {
        for {
          i <- 0 to 4
        } {
          val res = intCodeComputerWithLoop(inputSettings(i)._1, inputSettings(i)._2, List(inputSettings(i)._3, inputSettings(i)._4))
          if (res.lastCode != 99) {
            inputSettings(i) = (res.inputs, res.pointer, inputSettings(i)._3, inputSettings(i)._4)
            if (inputSettings((i+1)%5)._4 == -1 ) {
              inputSettings((i+1)%5) = (inputSettings((i+1)%5)._1, inputSettings((i+1)%5)._2, inputSettings((i+1)%5)._3, res.output)
            } else {
              inputSettings((i+1)%5) = (inputSettings((i+1)%5)._1, inputSettings((i+1)%5)._2, res.output, res.output)
            }
          }
          last_code = res.lastCode
        }
      }

      inputSettings.map(_._3).reduce(math.max)
    }

    case class intCodeResult(output: Int, inputs: Array[Int], pointer: Int, lastCode: Int)

    def intCodeComputerWithLoop(inputs: Array[Int], current: Int, functionInputs: List[Int]): intCodeResult = {
      val ins = parseInstruction(inputs(current))
      ins._4 match {
        case 1 => {
          val res = getParamByMode(ins._3, inputs, current + 1) + getParamByMode(ins._2, inputs, current + 2)
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputerWithLoop({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 2 => {
          val res = getParamByMode(ins._3, inputs, current + 1) * getParamByMode(ins._2, inputs, current + 2)
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputerWithLoop({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 3 => {
          val s = inputs.splitAt(inputs(current + 1))
          intCodeComputerWithLoop({
            s._1 :+ functionInputs.head
          } ++ s._2.tail, current + 2, functionInputs.tail)
        }
        case 4 => {
          return intCodeResult(getParamByMode(ins._3, inputs, current + 1), inputs, current + 2, 4)
        }
        case 5 => {
          val next = if (getParamByMode(ins._3, inputs, current + 1) != 0) getParamByMode(ins._2, inputs, current + 2) else current + 3
          intCodeComputerWithLoop(inputs, next, functionInputs)
        }
        case 6 => {
          val next = if (getParamByMode(ins._3, inputs, current + 1) == 0) getParamByMode(ins._2, inputs, current + 2) else current + 3
          intCodeComputerWithLoop(inputs, next, functionInputs)
        }
        case 7 => {
          val res = if (getParamByMode(ins._3, inputs, current + 1) < getParamByMode(ins._2, inputs, current + 2)) 1 else 0
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputerWithLoop({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 8 => {
          val res = if (getParamByMode(ins._3, inputs, current + 1) == getParamByMode(ins._2, inputs, current + 2)) 1 else 0
          val s = inputs.splitAt(inputs(current + 3))
          intCodeComputerWithLoop({
            s._1 :+ res
          } ++ s._2.tail, current + 4, functionInputs)
        }
        case 99 => return intCodeResult(-1, inputs, current, 99)
      }
    }





  }
}