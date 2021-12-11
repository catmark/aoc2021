import java.lang.System
import scala.io.Source

object Day10:
  val matches: Map[Char, Char] = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val scores: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val fixScores: Map[Char, Int] = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)


  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day10").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def isOpening(c: Char): Boolean = matches.contains(c)

  def syntaxError(line: String): Int = {
    var stack: List[Char] = List()
    line.toList.foreach(br => {
      if (isOpening(br)) {stack = stack.prepended(br)}
      else if (br == matches(stack.head)) {stack = stack.tail}
      else return scores(br)
    })
    0
  }

  def fixStack(stack: List[Char]): Long = stack.foldLeft(0L)((ans, c) => ans*5 + fixScores(matches(c)))

  def fixString(line: String): Long = {
    var stack: List[Char] = List()
    line.toList.foreach(br => {
      if (isOpening(br)) {stack = stack.prepended(br)}
      else if (br == matches(stack.head)) {stack = stack.tail}
      else return 0
    })
    fixStack(stack)
  }

  def run(): Unit = println(loadFile().map(syntaxError).sum)

  def runb(): Unit =
    val scores = loadFile().map(fixString).filter(_.!=(0)).sorted
    println(scores(scores.length / 2))

