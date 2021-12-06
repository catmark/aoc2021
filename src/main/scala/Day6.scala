import Day5.drawHor

import java.lang.System
import scala.io.Source

object Day6:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day6").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def passDay(fishes: collection.mutable.Map[Int, Long]): Unit = {
    val newFishes = fishes(0)
    fishes.foreach((age, amount) => fishes(age) = if (age == 8) newFishes else fishes(age + 1))
    fishes(6) += newFishes
  }

  def run(): Unit = {
    val data = loadFile()

    val fishes: collection.mutable.Map[Int, Long] = collection.mutable.Map().withDefaultValue(0)

    data.head.split(",").map(_.toInt).foreach(fishes(_) += 1)
    fishes(0) = 0
    fishes(7) = 0
    fishes(8) = 0

    (1 to 256).foreach(_ => passDay(fishes))

    println(fishes.values.sum)
  }

  def runb(): Unit =
    val data = loadFile()

