import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    val maps = Source.fromFile("./day10.input").getLines.toList.map(_.zipWithIndex.filter(_._1 == '#')).zipWithIndex.filter(_._1.nonEmpty).flatMap(i=> i._1.map(j => (j._2, i._2)))
    // First
    var (visible, station) = maps.map(a => visibleAsteroids(a, maps)).max
    println(visible)

    // Second
    val mapFromStation = maps.map(vectorDiff(station, _)).filterNot(_ == (0,0)).sorted
    val anglesAndMagns = mapFromStation.map(findAngleAndMagnitute)
    val layeredAsteroids = mapFromStation.zip(anglesAndMagns).groupBy(_._2._1).values.flatMap(_.sorted.zipWithIndex.map((a) => (a._1._1, a._1._2._1, a._2))).toList
    val vapeOrder = layeredAsteroids.sortBy(_._2).sortBy(_._3).map((a) => (station._1 + a._1._1, station._2 - a._1._2))
    print(vapeOrder(199)._1*100 + vapeOrder(199)._2)
  }

  def findAngleAndMagnitute(asteroid: (Int, Int)): (Double, Double) = {
    val magn = Math.sqrt(asteroid._1*asteroid._1 + asteroid._2*asteroid._2)
    val rad = Math.atan2(asteroid._1.toDouble, asteroid._2.toDouble)
    val angle = if (rad.toDegrees < 0) 360 + rad.toDegrees else rad.toDegrees
    (angle, magn);
  }

  def visibleAsteroids(asteroid: (Int, Int), asteroidMap: List[(Int, Int)]): (Int, (Int, Int)) = {
    val visible = asteroidMap.map(b => vectorDiff(asteroid, b)).map(findLowestTerm).toSet.size - 1
    (visible, asteroid)
  }

  def vectorDiff(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    (b._1-a._1, a._2-b._2)
  }

  def findLowestTerm(a: (Int, Int)): (Int, Int) = {
    a match {
      case (0, _) => (0, math.signum(a._2))
      case (_, 0) => (math.signum(a._1), 0)
      case _ =>
        val g = gcd(math.abs(a._1), math.abs(a._2))
        (a._1 / g, a._2/g)
    }
  }

  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = {
    if(b == 0) a else gcd(b, a%b)
  }

}