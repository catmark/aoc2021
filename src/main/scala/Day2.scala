import scala.io.Source


object Day2:
  def loadFile() = {
    Source.fromFile("src/main/scala/input/day2").getLines().toList
  }

  def run(): Unit = {
    val data = loadFile();
    var depth, horizontal, aim = 0;

    data.map(_.split(" ")).map(x => (x(0), x(1).toInt)).foreach((dir, cnt) => {
      dir match {
        case "forward" => {horizontal += cnt; depth += aim * cnt}
        case "down" => aim += cnt
        case "up" => aim -= cnt
      }
    })
    println(horizontal * depth)
  }

