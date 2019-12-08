import scala.io.Source

object Day8 {

  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("./day8.input").getLines().next()

    // First
    val layersFlat = source.grouped(25*6).toBuffer
    val lestZerosLayerI = (layersFlat.map(_.count(_ == '0')).zipWithIndex.reduce((left, right) => if (left._1 < right._1) left else right))._2
    println(layersFlat(lestZerosLayerI).count(_ == '1') * layersFlat(lestZerosLayerI).count(_ == '2'))

    // Second
    val layers = source.grouped(25).grouped(6).toList
    val pic = layers.reduceRight((layer, res) => {
      layer.zip(res).map((s) => s._1.zip(s._2).map(comparePixels).mkString)
    })

    pic.map(_.map((c) => if (c == '0') ' ' else '*')).map(println)
  }

  def comparePixels(p: (Char, Char)): Char = {
    if (p._1 == '2') p._2 else p._1
  }
}