
class Packet(version: Int, _type: Int) {
  var subpackets: List[Packet] = List.empty
  var literal = 0L

  def totalVersion : Int = subpackets.map(_.totalVersion).sum + version

  def compare(f: (Long, Long) => Boolean): Long = if (f(subpackets(0).calculate, subpackets(1).calculate)) 1 else 0

  def calculate: Long =
    _type match {
      case 0 => subpackets.map(_.calculate).sum
      case 1 => subpackets.map(_.calculate).product
      case 2 => subpackets.map(_.calculate).min
      case 3 => subpackets.map(_.calculate).max
      case 4 => literal
      case 5 => compare(_ > _)
      case 6 => compare(_ < _)
      case 7 => compare(_ == _)
      case default => 0
    }
}

object Packet {
  def make(input: Input) : Packet = {
    val version = Integer.parseInt(input.next(3), 2)
    val _type = Integer.parseInt(input.next(3), 2)
    val p = Packet(version, _type)
    if (_type == 4)
      p.literal = findLiteral(input)
    else
      p.subpackets = findSubpackets(input)
    p
  }

  def findLiteral(input: Input): Long = {
    var literalStr: String = ""

    while(true) {
      val s = input.next(5)
      literalStr += s.substring(1)
      if (s.startsWith("0"))
        return BigInt(literalStr, 2).toLong
    }
    0
  }

  def findSubpackets(input: Input): List[Packet] = {
    if (input.next(1) == "0") {
      val totalLength = Integer.parseInt(input.next(15), 2)
      val startIndex = input.index
      var packets: List[Packet] = List.empty
      while (input.index < startIndex + totalLength) {
        packets = packets.appended(make(input))
      }
      packets
    } else {
      val totalSubs = Integer.parseInt(input.next(11), 2)
      (0 until totalSubs).map(_ => make(input)).toList
    }
  }
}
