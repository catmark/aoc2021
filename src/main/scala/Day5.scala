import Day5.drawHor

import java.lang.System
import scala.io.Source

object Day5:
  case class Coordinate(x: Int, y: Int)
  case class Line(start: Coordinate, end: Coordinate)

  private val map: Array[Array[Int]] = Array.ofDim(1000, 1000)


  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day5").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def toCoord(string: String) : Coordinate = {
    val spl = string.split(",")
    Coordinate(spl(0).toInt, spl(1).toInt)
  }

  def drawHor(from: Coordinate, to: Coordinate): Unit = {
    (from.y to to.y).foreach(y => {
      map(from.x)(y) += 1;
    })
  }

  def drawVer(from: Coordinate, to: Coordinate): Unit = {
    (from.x to to.x).foreach(x => {
      map(x)(from.y) += 1;
    })
  }

  def drawLine(line: Line): Unit = {
    if (line.start.x == line.end.x) {
      if (line.start.y < line.end.y) drawHor(line.start, line.end) else drawHor(line.end, line.start)
    }
    if (line.start.y == line.end.y) {
      if (line.start.x < line.end.x) drawVer(line.start, line.end) else drawVer(line.end, line.start)
    }
  }

  def drawIncline(from: Coordinate, to: Coordinate): Unit = {
    (from.x to to.x).zip(from.y to to.y).foreach((x,y) => {
      map(x)(y) += 1;
    })
  }

  def drawDecline(from: Coordinate, to: Coordinate): Unit = {
    (from.x to to.x).zip(from.y to to.y by -1).foreach((x,y) => {
      map(x)(y) += 1;
    })
  }

  def drawLineB(line: Line): Unit = {
    if (line.start.x == line.end.x) {
      if (line.start.y < line.end.y) drawHor(line.start, line.end) else drawHor(line.end, line.start)
    }
    else if (line.start.y == line.end.y) {
      if (line.start.x < line.end.x) drawVer(line.start, line.end) else drawVer(line.end, line.start)
    }
    else {
      val slope = (line.end.y - line.start.y) / (line.end.x - line.start.x)
      if (slope == 1) {
        if (line.start.x < line.end.x) drawIncline(line.start, line.end) else drawIncline(line.end, line.start)
      }
      if (slope == -1) {
        if (line.start.x < line.end.x) drawDecline(line.start, line.end) else drawDecline(line.end, line.start)
      }
    }
  }

  def run(): Unit = {
    val data = loadFile()

    val lines = data.map(str => str.split(" -> ").map(toCoord)).map(c => Line(c(0), c(1)))

    lines.foreach(drawLine)

    println(map.flatten.count(x => x > 1))
  }

  def runb(): Unit =
    val data = loadFile()

    val lines = data.map(str => str.split(" -> ").map(toCoord)).map(c => Line(c(0), c(1)))

    lines.foreach(drawLineB)

    println(map.flatten.count(x => x > 1))
