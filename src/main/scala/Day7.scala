import java.lang.System
import scala.io.Source

object Day7:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day7").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def bestPosition(fuelCostFunc: Int => Int): Long =
    val crabs = loadFile().head.split(",").map(_.toInt)

    (0 to crabs.max).map(pos => crabs.map(c => fuelCostFunc(Math.abs(pos - c))).sum).min

  def run(): Unit = println(bestPosition(d => d))

  def runb(): Unit = println(bestPosition(d => (d*d+d)/2))

