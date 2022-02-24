import java.lang.System
import scala.io.Source

object Day20:
  type Grid = IndexedSeq[IndexedSeq[Char]]

  val algo = loadFile().head

  def loadFile() = Source.fromFile("src/main/scala/input/day20").getLines().toIndexedSeq.map(_.trim())

  def neighbour(y: Int, x: Int, grid: Grid, round: Int): Char = {
    if (y < 0 || y >= grid.length || x < 0 || x >= grid.head.length)
      if (round % 2 == 1) '.' else '#'
    else
      grid(y)(x)
  }

  def calculateChar(y: Int, x: Int, grid: Grid, round: Int): Char =
    val binary: String = (y - 1 to y + 1).flatMap(yy => {
      (x - 1 to x + 1).map(xx => {
        neighbour(yy, xx, grid, round)
      })
    }).foldLeft("")((s,c) => s + c)
    algo(Integer.parseInt(binary.replace('#', '1').replace('.', '0'), 2))


  def enhance(grid: Grid, round: Int): Grid =
    (-2 until grid.length + 2).map(y => {
      (-2 until grid.head.length + 2).map(x => {
        calculateChar(y, x, grid, round)
      })
    })

  def run(): Unit = {
    var grid = loadFile().tail.filterNot(_.isEmpty).map(_.split("").map(_.head).toIndexedSeq)

    grid = enhance(grid, 1)
    grid = enhance(grid, 2)


    println(grid.flatten.count(_.==('#')))
  }

  def runb(): Unit = {
    var grid = loadFile().tail.filterNot(_.isEmpty).map(_.split("").map(_.head).toIndexedSeq)

    (1 to 50).foreach(v =>
      grid = enhance(grid, v)
    )

    println(grid.flatten.count(_.==('#')))
  }


