import java.lang.System
import scala.io.Source

object Day8:
  case class Code(input: List[String], output: List[String])

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day8").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def countEasy(data: List[String]) = data.map(_.split("""\|""")(1).split(" ").count(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)).sum

  def toCode(line: String): Code =
    val d = line.split("""\|""")
    Code(
      d(0).split(" ").filter(_.nonEmpty).toList.map(s => s.split("").sorted.reduceLeft(_ + _)),
      d(1).split(" ").filter(_.nonEmpty).toList.map(s => s.split("").sorted.reduceLeft(_ + _))
    )

  def containsAll(str: String, toFind: String) : Boolean = toFind.split("").forall(l => str.contains(l))


  def decode(code: Code): Int =
    val mapping = collection.mutable.Map[Int, String]()
    code.input.sortWith((a,b) => if (a.length == 5) false else a.length < b.length).foreach(str => {
      str.length match {
        case 2 => mapping += (1 -> str)
        case 4 => mapping += (4 -> str)
        case 3 => mapping += (7 -> str)
        case 7 => mapping += (8 -> str)
        case 6 if containsAll(str, mapping(4)) => mapping += (9 -> str)
        case 6 if containsAll(str, mapping(1)) => mapping += (0 -> str)
        case 6 => mapping += (6 -> str)
        case 5 if containsAll(str, mapping(1)) => mapping += (3 -> str)
        case 5 if containsAll(mapping(6), str) => mapping += (5 -> str)
        case 5 => mapping += (2 -> str)
        case _ =>
      }
    })
    val lookup = mapping.toList
    var result = 0
    code.output.map(str => lookup.find(x => x._2 == str).get._1).foreach(v => result = result * 10 + v)
    result

  def run(): Unit =
    val data = loadFile()
    println(countEasy(data))

  def runb(): Unit =
    val start = System.currentTimeMillis()
    val codes = loadFile().map(toCode)
    println(codes.map(decode).sum)
    println(System.currentTimeMillis() - start)

