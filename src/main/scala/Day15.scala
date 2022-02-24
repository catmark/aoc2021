import java.lang.System
import javax.swing.ViewportLayout
import scala.io.Source

object Day15:
  case class Value(var value: Int, var isFinal: Boolean)

  case class Coordinate(x: Int, y: Int)

  var processing: Map[Coordinate, Int] = Map.empty
  
  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day15").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def updateNeighbour(grid: List[List[Value]], x: Int, y: Int, value: Int) =
    if (y >= 0 && y < grid.length && x >= 0 && x < grid.head.length && !grid(y)(x).isFinal) {
      processing.get(Coordinate(x, y)) match {
        case Some(existingValue) =>
          if (existingValue > value + grid(y)(x).value) {
            processing = processing.updated(Coordinate(x, y), value + grid(y)(x).value)
          }
        case None =>
          processing = processing.updated(Coordinate(x, y), value + grid(y)(x).value)
      }
    }

  def updateNeighbours(grid: List[List[Value]], coord: Coordinate, value: Int) = {
    updateNeighbour(grid, coord.x - 1, coord.y, value)
    updateNeighbour(grid, coord.x + 1, coord.y, value)
    updateNeighbour(grid, coord.x, coord.y - 1, value)
    updateNeighbour(grid, coord.x, coord.y + 1, value)
  }

  private def shortestRoute(grid: List[List[Value]]): Int =
    println(processing.size)
    val (minCoord, minVal) = processing.minBy(_._2)
    if (minCoord.y == grid.length - 1 && minCoord.x == grid.head.length - 1)
      return minVal
    processing = processing.removed(minCoord)
    grid(minCoord.y)(minCoord.x).isFinal = true
    updateNeighbours(grid, minCoord, minVal)
    shortestRoute(grid: List[List[Value]])

  def run(): Unit =
    val grid = loadFile().map(line => line.split("").map(d => Value(d.toInt, false)).toList)
    processing = Map(Coordinate(0,0) -> 0)
    println(shortestRoute(grid))

  def runb(): Unit =
    val grid = loadFile().map(line => line.split("").map(_.toInt).toList)
    var bigGrid = grid.map(line =>
      (0 until 5).flatMap(i => line.map(v => if (v + i > 9) Value((v + i)%9, false) else Value(v + i, false))).toList
    )
    (1 until 5).foreach(i => {
      bigGrid = bigGrid.appendedAll(bigGrid.take(grid.length).map(line =>
        line.map(v => if (v.value + i > 9) Value((v.value + i)%9, false) else Value(v.value + i, false))
      ))
    })
    processing = Map(Coordinate(0,0) -> 0)
    println(shortestRoute(bigGrid))



