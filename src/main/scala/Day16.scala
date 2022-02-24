import java.lang.System
import scala.io.Source

object Day16:


  def loadFile() = Source.fromFile("src/main/scala/input/day16").getLines().next().flatMap(c =>
    val i = Integer.parseInt(c.toString, 16)
    "%4s".format(i.toBinaryString).replace(' ', '0')
  )


  def run(): Unit = {
    val input = Input(0, loadFile())
    val root = Packet.make(input)
    println(root.totalVersion)
  }

  def runb(): Unit =
    val input = Input(0, loadFile())
    val root = Packet.make(input)
    println(root.calculate)


