import java.lang.System
import scala.io.Source

object Day6:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day6").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def passDay(fishes: collection.mutable.Map[Int, Long]): Unit = {
    val newFishes = fishes(0)
    fishes.foreach((age, amount) => fishes(age) = if (age == 8) newFishes else fishes(age + 1))
    fishes(6) += newFishes
  }

  def countFish(days: Int): Long =
    val data = loadFile()

    val fishes: collection.mutable.Map[Int, Long] = collection.mutable.Map().withDefaultValue(0)

    data.head.split(",").map(_.toInt).foreach(fishes(_) += 1)
    fishes(0) = 0
    fishes(7) = 0
    fishes(8) = 0
    (1 to days).foreach(_ => passDay(fishes))
    fishes.values.sum

  def run(): Unit = println(countFish(80))

  def runb(): Unit = println(countFish(256))

