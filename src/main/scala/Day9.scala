import java.lang.System
import scala.io.Source

object Day9:
  case class FloorBoard(value: Int, var counted: Boolean)

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

  def isOk(x: Int, y: Int, map: List[List[FloorBoard]]): Boolean =
    val ok = y >= 0 && y < map.length && x >= 0 && x < map(y).length && !map(y)(x).counted && map(y)(x).value != 9
    if (ok) map(y)(x).counted = true
    ok

  def calcBasin(x: Int, y: Int, map: List[List[FloorBoard]]): Int = {
    var cnt = 0
    if (isOk(x - 1, y, map)) cnt += calcBasin(x - 1, y, map) + 1
    if (isOk(x + 1, y, map)) cnt += calcBasin(x + 1, y, map) + 1
    if (isOk(x, y - 1, map)) cnt += calcBasin(x, y - 1, map) + 1
    if (isOk(x, y + 1, map)) cnt += calcBasin(x, y + 1, map) + 1
    cnt
  }

  def findBasins(data: List[List[FloorBoard]]): List[Int] =
    data.zipWithIndex.flatMap(row => row._1.filter(fb => fb.value != 9).zipWithIndex.map(num => calcBasin(num._2, row._2, data)))

  def run(): Unit =
    val data: List[List[Int]] = loadFile().map(l => l.split("").map(_.toInt).toList)
    println(countLowPoints(data))


  def runb(): Unit =
    val data: List[List[FloorBoard]] = loadFile().map(l => l.split("").map(n => FloorBoard(n.toInt, false)).toList)
    println(findBasins(data).sorted.reverse.take(3).product)

