import java.lang.System
import scala.io.Source

object Day5:
  case class Coordinate(x: Int, y: Int)
  case class Line(start: Coordinate, end: Coordinate)

  private var map: Array[Array[Int]] = Array.ofDim(1000, 1000)
  def resetMap = map = Array.ofDim(1000, 1000)

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day5").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def toCoord(string: String) : Coordinate = {
    val spl = string.split(",")
    Coordinate(spl(0).toInt, spl(1).toInt)
  }

  def isHor(l: Line): Boolean = l.start.y == l.end.y
  def isVert(l: Line): Boolean = l.start.x == l.end.x

  def drawLineOnMap(from: Coordinate, to: Coordinate, xChange: Int => Int, yChange: Int => Int ) : Unit =
    var x = from.x
    var y = from.y
    while(x != to.x || y != to.y) {
      map(x)(y) += 1
      x = xChange(x)
      y = yChange(y)
    }
    map(x)(y) += 1

  def incr(x: Int): Int = x + 1
  def decr(x: Int): Int = x - 1
  def eq(x: Int): Int = x
  private def changeFunc(start: Int, end: Int) = if (start < end) incr else if (start > end) decr else eq

  def drawLine(line: Line): Unit = {
    val xChangeFunc = changeFunc(line.start.x, line.end.x)
    val yChangeFunc = changeFunc(line.start.y, line.end.y)
    drawLineOnMap(line.start, line.end, xChangeFunc, yChangeFunc)
  }

  def run(): Unit = {
    resetMap
    val data = loadFile()

    val lines = data.map(str => str.split(" -> ").map(toCoord)).map(c => Line(c(0), c(1)))

    lines.filter(l => isHor(l) || isVert(l)).foreach(drawLine)

    println(map.flatten.count(x => x > 1))
  }

  def runb(): Unit =
    resetMap
    val data = loadFile()

    val lines = data.map(str => str.split(" -> ").map(toCoord)).map(c => Line(c(0), c(1)))

    lines.foreach(drawLine)

    println(map.flatten.count(x => x > 1))
