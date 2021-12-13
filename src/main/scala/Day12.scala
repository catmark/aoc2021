import java.lang.System
import scala.io.Source

object Day12:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day12").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def connectCaves =
    var caves: Map[String, Cave] = Map.empty
    loadFile().foreach(line => {
      val spl = line.split("-")
      if (!caves.contains(spl(0))) caves += (spl(0) -> Cave(spl(0), List.empty))
      if (!caves.contains(spl(1))) caves += (spl(1) -> Cave(spl(1), List.empty))
      caves(spl(0)).addConnections(caves(spl(1)))
      caves(spl(1)).addConnections(caves(spl(0)))
    })
    caves

  def run(): Unit =
    val caves = connectCaves
    println(caves("start").findPaths(List.empty))


  def runb(): Unit =
    val caves = connectCaves
    println(caves("start").findPathsB(List.empty, false))



