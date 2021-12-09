import java.lang.System
import scala.io.Source

object Day9:
  val floor = Array.ofDim[Int](100, 100)

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day9").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def countLowPoints(data: List[List[Int]]): Int = {
    data.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(num => {
      val x = num._2
      val y = row._2
      val value = num._1
      if (
        (x == 0 || data(y)(x - 1) > value) &&
          (x == row._1.length - 1 || data(y)(x + 1) > value) &&
          (y == 0 || data(y - 1)(x) > value) &&
          (y == data.length - 1 || data(y + 1)(x) > value)) value + 1 else 0
    })).sum
  }

  def calcBasin(x: Int, y: Int): Int = {
    var cnt = 0
    if (x > 0 && floor(y)(x - 1) != 9 && floor(y)(x - 1) != -1){
      floor(y)(x - 1) = -1
      cnt += 1
      cnt += calcBasin(x - 1, y)
    }
    if (x < floor(y).length - 1 && floor(y)(x + 1) != 9 && floor(y)(x + 1) != -1){
      floor(y)(x + 1) = -1
      cnt += 1
      cnt += calcBasin(x + 1, y)
    }
    if (y > 0 && floor(y - 1)(x) != 9 && floor(y - 1)(x) != -1){
      floor(y - 1)(x) = -1
      cnt += 1
      cnt += calcBasin(x, y - 1)
    }
    if (y < floor.length - 1 && floor(y + 1)(x) != 9 && floor(y + 1)(x) != -1){
      floor(y + 1)(x) = -1
      cnt += 1
      cnt += calcBasin(x, y + 1)
    }
    cnt
  }

  def findBasins(data: List[List[Int]]): List[Int] = {
    data.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(num => {
      val x = num._2
      val y = row._2
      val value = num._1
      if (
          (x == 0 || data(y)(x - 1) > value) &&
          (x == row._1.length - 1 || data(y)(x + 1) > value) &&
          (y == 0 || data(y - 1)(x) > value) &&
          (y == data.length - 1 || data(y + 1)(x) > value)) calcBasin(x, y) else 0
    }))
  }



  def run(): Unit =
    val data: List[List[Int]] = loadFile().map(l => l.split("").map(_.toInt).toList)
    println(countLowPoints(data))



  def runb(): Unit =
    loadFile().zipWithIndex.foreach(line => {
      floor(line._2) = line._1.split("").map(_.toInt)
    })
    val data: List[List[Int]] = loadFile().map(l => l.split("").map(_.toInt).toList)
    println(findBasins(data).sorted.reverse.take(3).product)

