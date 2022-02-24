import java.lang.System
import scala.io.Source

object Day17:
  private val leftBorder = 195
  private val rightBorder = 238
  private val topBorder = -67
  private val bottomBorder = -93


  def inTarget(x: Int, y: Int): Boolean = x >= leftBorder && x <= rightBorder && y >= bottomBorder && y <= topBorder

  def inRange(xSpeed: Int, ySpeed: Int): Boolean =
    var xs = xSpeed
    var ys = ySpeed
    var x, y = 0
    while(x <= rightBorder && y >= bottomBorder) {
      x += xs
      y += ys
      if (xs > 0) xs -= 1
      ys -= 1
      if (inTarget(x,y)) return true
    }
    false

  def run(): Unit = {}

  def runb(): Unit =
    val minX = 20
    val maxY = 92
    println((minX to rightBorder).map(x => (bottomBorder to maxY).count(y => inRange(x, y))).sum)






