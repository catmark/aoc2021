import java.lang.System
import scala.io.Source

object Day14:

  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day14").getLines().toList.filter(str => str.nonEmpty).map(_.trim())

  def grow(polymer: String, rules: Map[String, String]): String = {
    val x = (1 until polymer.length).map(i => {
      if (i == polymer.length - 1)
        polymer(i - 1) + rules(polymer.substring(i-1, i+1)) + polymer(i)
      else
        polymer(i - 1) + rules(polymer.substring(i-1, i+1))
    })
    x.foldLeft("")(_ + _)
  }

  def count(polymer: String): Int = {
    val charCount = polymer.split("").groupBy(c => c).values.map(_.length)
    charCount.max - charCount.min
  }

  def growSmart(charPairs: Map[String, Long], rules: Map[String, String]): Map[String, Long] = {
    var newCharPairs: Map[String, Long] = Map.empty.withDefaultValue(0)
    charPairs.foreach(pair => {
      val insert = rules(pair._1)
      newCharPairs = newCharPairs.updatedWith(pair._1.substring(0, 1) + insert) {
        case old: scala.Some[Long] => old.map(v => v + pair._2)
        case _: scala.None.type => Some(pair._2)
      }
      newCharPairs = newCharPairs.updatedWith(insert + pair._1.substring(1, 2)) {
        case old: scala.Some[Long] => old.map(v => v + pair._2)
        case _: scala.None.type => Some(pair._2)
      }
    })
    newCharPairs
  }

  def answer(charPairs: Map[String, Long]): Long = {
    var charCount: Map[String, Long] = Map.empty.withDefaultValue(0)
    charPairs.foreach(pair => pair._1.split("").foreach(c => charCount = charCount.updated(c, charCount(c) + pair._2)))

    (charCount.values.max + 1) / 2 - (charCount.values.min + 1) / 2
  }

  def run(): Unit =
    val file = loadFile()
    var polymer = file.head
    val rules: Map[String, String] = file.tail.map(line => {
      val spl = line.split(" -> ")
      spl(0) -> spl(1)
    }).toMap
    (0 until 10).foreach(_ => {
      polymer = grow(polymer, rules)
    })
    println(count(polymer))

  def runb(): Unit =
    val file = loadFile()

    val polymerStr = file.head
    var polymer: Map[String, Long] = Map.empty.withDefaultValue(0)
    (1 until polymerStr.length).foreach(i =>
      polymer = polymer.updatedWith(polymerStr.substring(i - 1, i + 1)) {
        case x: scala.Some[Long] => Some(x.get + 1)
        case _: scala.None.type => Some(1)
      }
    )
    val rules: Map[String, String] = file.tail.map(line => {
      val spl = line.split(" -> ")
      spl(0) -> spl(1)
    }).toMap
    val start = System.currentTimeMillis()

    (0 until 10).foreach(_ => {
      polymer = growSmart(polymer, rules)
    })
    println(answer(polymer))
    println(System.currentTimeMillis() - start)





