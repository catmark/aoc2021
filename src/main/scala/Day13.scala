import java.lang.System
import scala.io.Source

object Day13:
  enum FoldDirection:
    case X, Y

  case class Coord(x: Int, y: Int)
  case class FoldLine(direction: FoldDirection, value: Int)

  def loadCoordinates(): List[String] = Source.fromFile("src/main/scala/input/day13").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def loadFolds(): List[String] = Source.fromFile("src/main/scala/input/day13b").getLines().toList.filter(str => str.nonEmpty).map(_.trim())


  def nextFold(data: Array[Array[Int]], fold: FoldLine): Array[Array[Int]] = {
    data.indices.foreach(y => {
      data(y).indices.foreach(x => {
        fold.direction match {
          case FoldDirection.X =>
            if (x > fold.value && data(y)(x) == 1) {
              data(y)(fold.value - (x - fold.value)) = 1
              data(y)(x) = 0
            }
          case FoldDirection.Y =>
            if (y > fold.value && data(y)(x) == 1) {
              data(fold.value - (y - fold.value))(x) = 1
              data(y)(x) = 0
            }
        }
      })
    })
    data
  }

  def readGrid(): Array[Array[Int]] = {
    val coords: List[Coord] = loadCoordinates().map(line =>
      val spl = line.split(",").map(_.toInt)
      Coord(spl(0), spl(1))
    )
    val maxX = coords.map(c => c.x).max + 1
    val maxY = coords.map(c => c.y).max + 1
    val data = Array.fill(maxY, maxX)(0)
    coords.foreach(c => data(c.y)(c.x) = 1)
    data
  }


  def run(): Unit =
    val grid = readGrid()
    val folds = loadFolds().map(line => {
      val spl = line.substring(11).split("=");
      val direction = if (spl(0) == "x") FoldDirection.X else FoldDirection.Y;
      FoldLine(direction, spl(1).toInt)
    })
    println(nextFold(grid, folds.head).flatten.sum)
    println(grid.flatten.sum)

  def runb(): Unit =
    val grid = readGrid()
    val folds = loadFolds().map(line => {
      val spl = line.substring(11).split("=");
      val direction = if (spl(0) == "x") FoldDirection.X else FoldDirection.Y;
      FoldLine(direction, spl(1).toInt)
    })
    folds.foreach(fold => nextFold(grid, fold))
    grid.filter(row => row.contains(1)).foreach(row =>
      row.take(40).map(v => if (v == 1) print('#') else print(' '))
      println()
    )





