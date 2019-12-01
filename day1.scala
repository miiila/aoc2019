import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    var modules = Source.fromFile("./day1.input").getLines.toList.map(x => x.toInt)
    modules = modules.map(x => (x / 3) - 2)
    // First
    println(modules.reduce((a, b) => a + b))
    // Second
    println(modules.map(fuel_rec).reduce(_ + _))
  }

  def fuel_rec(f: Int): Int = {
    if (f/3 < 3) f else f + fuel_rec((f / 3) - 2)
  }
}
