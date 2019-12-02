import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("./day2.input")
    val inputs = source.getLines().next().split(',').map(_.toInt)
    // first
    // replace position 1 with the value 12 and replace position 2 with the value 2
    inputs(1) = 12
    inputs(2) = 2
    println(processInput(inputs, 0))
    // Second
    for {
      x <- 0 to 99
      y <- 0 to 99
    } {
      inputs(1) = x
      inputs(2) = y
      if (processInput(inputs, 0) == 19690720) {
        println(100 * x + y)
        return
      }
    }
  }

  @annotation.tailrec
  def processInput(inputs: Array[Int], current: Int): Int = {
    val s = inputs.splitAt(inputs(current+3))
    val params = (inputs(inputs(current+1)), inputs(inputs(current+2)))
    inputs(current) match {
      case 1 => {
        val res = params._1 + params._2
        processInput({s._1 :+ res} ++ s._2.slice(1, s._2.length), current + 4)
      }
      case 2 => {
        val res = params._1 * params._2
        processInput({s._1 :+ res} ++ s._2.slice(1, s._2.length), current + 4)
      }
      case 99 =>
        inputs(0)
    }
  }
}
