import scala.io.Source

object Day3:
  def loadFile(): List[String] = Source.fromFile("src/main/scala/input/day3").getLines().toList.filter(str => str.nonEmpty)

  def hasOneAt(i: Int)(s: String): Boolean = s(i) == '1'

  def run(): Unit = {
    val data = loadFile()
    var g, e = 0

    data.map(_.toList).transpose.map(f => f.count(_.==('1')) > f.length/2).foreach(one => { g <<= 1; e <<= 1; if (one) g += 1 else e += 1})
    println(g * e)

//    for (i <- 0 to 11) {
//      if (data.count(hasOneAt(i)) > data.length/2 )
//        g += 1 << (11- i)
//      else
//        e += 1 << (11- i)
//    }
//    println(g * e)
  }

  def filterData(data: List[String], op: (Int,Int) => Boolean): Int = {
    var d = data
    for (i <- 0 to 11) {
      d = if (op(d.count(hasOneAt(i)), d.length/2))
        d.filter(hasOneAt(i))
      else
        d.filterNot(hasOneAt(i))
      if (d.length == 1) return Integer.parseInt(d.head, 2)
    };0
  }

  def runb(): Unit =
    val data = loadFile();
    println(filterData(data, _ >= _ ) * filterData(data, _ < _))

