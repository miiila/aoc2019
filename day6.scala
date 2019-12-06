import scala.io.Source

object Day6 {

  def main(args: Array[String]): Unit = {
    val orbits = Source.fromFile("./day6.input").getLines.toList.map(_.split(')')).map((a) => (a(0),a(1)))

    // First
    def countOrbits (orbits: List[(String, String)], current: Int)(orbit: String): Int = {
      val toCount = orbits.filter(_._1 == orbit).map(_._2)
      current + toCount.map(countOrbits(orbits, current + 1)).sum
    }

    println(countOrbits(orbits, 0)("COM"))

    // Second
    def traverseOrbits (orbits: List[(String, String)], current: Int, visited: Set[String])(orbit: String): Int = {
      val toCount = {orbits.filter(_._2 == orbit).map(_._1) ++ orbits.filter(_._1 == orbit).map(_._2)}.filterNot(visited)
      if (orbit == "SAN") current-1 else toCount.map(traverseOrbits(orbits, current + 1, visited + orbit)).sum
    }

    println(traverseOrbits(orbits, -1, Set())("YOU"))
  }
}