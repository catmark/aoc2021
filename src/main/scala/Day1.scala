import scala.io.Source

object Day1:
  def loadFile() = {
    Source.fromFile("src/main/scala/input/day1").getLines().map(_.toInt).toList
  }

  def run(): Unit = {
    val data = loadFile();
    val count = data.zipWithIndex.filter((value, i) => i > 0 && value > data(i - 1)).length
    println(count)
  }

  def runb(): Unit = {
    val data = loadFile();
    val count = data.zipWithIndex.filter((value, i) => i > 2 && value > data(i - 3)).length
    println(count)
  }

