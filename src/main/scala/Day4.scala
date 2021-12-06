import java.lang.System
import scala.io.Source

object Day4:
  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day4").getLines().toList.filter(str => str.nonEmpty)

  def run(): Unit = {
    val data = loadFile()
    val numbers: List[Int] = data.head.split(',').toList.filter(_.nonEmpty).map(_.toInt);

    val bingocards = data.drop(1).dropWhile(str=> str.isEmpty).grouped(5).map(strings => BingoCard(strings)).toList

    numbers.foreach(num => {
      bingocards.foreach(b =>
        b.crossNumber(num).foreach(ans => {
          println(ans)
          return
        })
      )
    })
  }

  def runb(): Unit =
    val data = loadFile();
    val numbers: List[Int] = data.head.split(',').toList.filter(_.nonEmpty).map(_.toInt);

    val bingocards = data.drop(1).dropWhile(_.isEmpty).grouped(5).map(BingoCard(_)).toList
    var solved = 0

    numbers.foreach(num => {
      bingocards.foreach(b =>
        b.crossNumber(num).foreach(ans => {
          solved += 1;
          if (solved == bingocards.length) println(ans)
        })
      )
    })
