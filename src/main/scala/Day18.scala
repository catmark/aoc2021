import java.lang.System
import scala.io.Source

object Day18:


  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day18").getLines().filterNot(_.isEmpty).toList

  def add(f1: FishNumber, f2: FishNumber): FishNumber = {
    val f = FishNumber.add(f1, f2)

    while(f.explode(1) || f.split()) {
      FishNumber.exploded = false
      FishNumber.addToRight = None
    }
    f
  }


  def run(): Unit = {
    val fishNumbers = loadFile().map(x => Input(0, x)).map(FishNumber.create)
    println(fishNumbers.length)
    println(fishNumbers.reduce(add))

    println(add(fishNumbers.head, fishNumbers(1)))
  }

  def runb(): Unit = {}


