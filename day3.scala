import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day3 {

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("./day3.input")
    val inputs = source.getLines.toList.map(_.split(',').map(x => (x.slice(0, 1), x.slice(1, x.length).toInt)))

    def moveRight(distance: Int, start: (Int, Int)): IndexedSeq[(Int, Int)] = {
      val (start_x, start_y) = start
      for {
        x <- (start_x to start_x + distance)
      } yield {
        Tuple2(x, start_y)
      }
    }

    def moveLeft(distance: Int, start: (Int, Int)): IndexedSeq[(Int, Int)] = {
      val (start_x, start_y) = start
      for {
        x <- (start_x to start_x - distance by -1)
      } yield {
        Tuple2(x, start_y)
      }
    }

    def moveUp(distance: Int, start: (Int, Int)): IndexedSeq[(Int, Int)] = {
      val (start_x, start_y) = start
      for {
        y <- (start_y to start_y + distance)
      } yield {
        Tuple2(start_x, y)
      }
    }

    def moveDown(distance: Int, start: (Int, Int)): IndexedSeq[(Int, Int)] = {
      val (start_x, start_y) = start
      for {
        y <- (start_y to start_y - distance by -1)
      } yield {
        Tuple2(start_x, y)
      }
    }

    // First

    case class VisitedPath(path: List[(Int, Int)], end: (Int, Int))

    def getVisitedPoints(path: (String, Int), start: (Int, Int)): VisitedPath = {
      val (direction, value) = path
      val visitedPath = direction match {
        case "R" => moveRight(value, start)
        case "L" => moveLeft(value, start)
        case "U" => moveUp(value, start)
        case "D" => moveDown(value, start)
      }

      VisitedPath(visitedPath.toList, visitedPath.last)
    }

    val wires = for {
      wire <- inputs
    } yield {
      var start = (0, 0)
      wire.map((i) => {
        val visitedPath = getVisitedPoints(i, start)
        start = visitedPath.end
        visitedPath.path.toSet
      }).reduce(_ ++ _)
    }

    val intersections = {
      wires(0).intersect(wires(1)) - Tuple2(0, 0)
    }
    println(intersections.map((a: (Int, Int)) => math.abs(a._1) + math.abs(a._2)).reduce(math.min))

    // Second
    case class VisitedPathWithDistance(path: List[(Int, Int, Int)], end: (Int, Int), distance: Int)

    def getVisitedPointsWithDistance(path: (String, Int), start: (Int, Int), currentDistance: Int): VisitedPathWithDistance = {
      var dist = currentDistance
      val visitedPath = getVisitedPoints(path, start)
      VisitedPathWithDistance(
        visitedPath.path.map(
          (t) => {
            dist += 1
            (t._1, t._2, dist)
          }),
        visitedPath.end, dist
      )
    }

    val wiresAgain = for {
      wire <- inputs
    } yield {
      var start = (0, 0)
      var distance = 0
      wire.map((i) => {
        val visitedPath = getVisitedPointsWithDistance(i, start, distance-1)
        start = visitedPath.end
        distance = visitedPath.distance
        visitedPath.path
      }).reduce(_ ++ _)
    }

    val res = for {
      i <- intersections
    } yield {
     wiresAgain.map(_.find((w) => w._1 == i._1 && w._2 == i._2)).map(_.map(_._3).get).reduce(_ + _)
    }

    print(res.reduce(math.min))
  }
}