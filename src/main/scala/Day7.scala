import java.lang.System
import scala.io.Source

object Day7:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day7").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def bestPosition(fuelCostFunc: (Int, Int) => Int): Long =
    val data = loadFile()

    val crabs = data.head.split(",").map(_.toInt)
    val max = crabs.max

    (0 to max).map(pos => crabs.map(c => fuelCostFunc(pos, c)).sum).min

  def run(): Unit = println(bestPosition((x, y) => Math.abs(x - y)))

  def runb(): Unit = println(bestPosition((x, y) => (Math.abs(x - y)* Math.abs(x - y) + Math.abs(x - y))/2 ))

