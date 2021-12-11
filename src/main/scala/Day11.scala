import java.lang.System
import scala.io.Source

object Day11:
  case class Octopus(var value: Int, var hasFlashed: Boolean)

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day11").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def incrementAll(data: List[List[Octopus]]) = data.foreach(_.foreach(_.value += 1))

  def resetAll(data: List[List[Octopus]]) = data.foreach(_.filter(_.hasFlashed).foreach(o => {
    o.value = 0
    o.hasFlashed = false
  }))

  def isOk(row: Int, col: Int, map: List[List[Octopus]]): Boolean = row >= 0 && row < map.length && col >= 0 && col < map(row).length

  def neighbours(row: Int, col: Int, grid: List[List[Octopus]]): Unit =
    (row - 1 to row + 1).foreach(r => {
      (col - 1 to col + 1).filter(c => isOk(r, c, grid)).foreach(c => grid(r)(c).value += 1)
    })

  def nextStep(data: List[List[Octopus]]): Int = {
    resetAll(data)
    incrementAll(data)

    var flashCount = 0
    while(
      data.zipWithIndex.flatMap(row => row._1.zipWithIndex.filter(o => o._1.value > 9 && !o._1.hasFlashed).map(o => {
        flashCount += 1
        o._1.hasFlashed = true
        neighbours(row._2, o._2, data)
      })).nonEmpty) {}
    flashCount
  }

  def countFlashes(data: List[List[Octopus]]): Int = (0 until 10).map(round => {
    nextStep(data)
  }).sum

  def findSync(data: List[List[Octopus]]): Int =
    for (i <- 1 until 1000) {
      nextStep(data)
      if (data.forall(row => row.forall(_.hasFlashed))) return i
    }
    0

  def run(): Unit =
    val data: List[List[Octopus]] = loadFile().map(line => line.split("").map(c => Octopus(c.toInt, false)).toList)
    println(countFlashes(data))


  def runb(): Unit =
    val data: List[List[Octopus]] = loadFile().map(line => line.split("").map(c => Octopus(c.toInt, false)).toList)
    println(findSync(data))


