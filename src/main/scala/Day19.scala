import java.lang.System
import scala.io.Source

object Day19:


  def loadFile() = Source.fromFile("src/main/scala/input/day19").getLines().toList.map(_.trim())

  def scanners(): List[Scanner]= {
    var input = loadFile()

    var scanners: List[Scanner] = List.empty
    while(input.nonEmpty) {
      scanners = scanners.appended(Scanner.from(input.takeWhile(_.nonEmpty).tail))
      input = input.dropWhile(_.nonEmpty).drop(1)
    }
    scanners
  }

  def scannerDistance(s1: Scanner, s2: Scanner) = (s1.location.x - s2.location.x).abs + (s1.location.y - s2.location.y).abs + (s1.location.z - s2.location.z).abs


  def run(): Unit = {
    val ss = scanners()
    ss.head.add()

    while(!ss.forall(_.added)) {
      ss.filterNot(_.added).foreach(s => s.compare())
    }
    println(Scanner.set.size)
  }

  def runb(): Unit = {
    val ss = scanners()
    ss.head.add()

    while(!ss.forall(_.added)) {
      ss.filterNot(_.added).foreach(s => s.compare())
    }
    println(ss.flatMap(s1 => ss.map(s2 => scannerDistance(s1, s2))).max)
  }


