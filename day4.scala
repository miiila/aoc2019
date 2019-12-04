object Day4 {
  def main(args: Array[String]): Unit = {

    val convert = (i: Int) => {
      i.toString.map(_.asDigit).toList
    }

    val isPair = (i: List[Int]) => {
      i.zip(i.tail).exists((pair) => pair._1 == pair._2)
    }

    val isRaising = (i: List[Int]) => {
      i.zip(i.tail).map((pair) => pair._1 <= pair._2).reduce(_ && _)
    }

    val isPassword = (i: Int) => {
      convert.andThen(isPair)(i) && convert.andThen(isRaising)(i)
    }

    val input = 158126 to 624574

    // First
    println(input.map(isPassword).filter(_ == true).length)

    // Second
    val hasOnePair = (i: List[Int]) => {
      val pairs = i.zip(i.tail).filter((pair) => pair._1 == pair._2)
      pairs.groupBy(_._1).exists(_._2.length == 1)
    }

    val isPasswordSecond = (i: Int) => {
      convert.andThen(hasOnePair)(i) && convert.andThen(isRaising)(i)
    }

    println(input.map(isPasswordSecond).filter(_ == true).length)

  }
}