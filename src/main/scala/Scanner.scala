import Scanner.{Coordinate, set}

class Scanner(beacons: List[Coordinate]) {
  var added = false;
  var location : Coordinate = Coordinate(0,0,0)

  def add() =
    Scanner.set ++= beacons
    added = true

  val permutations: List[Coordinate => Coordinate] = List(
    c => Coordinate(c.x, c.y, c.z),
    c => Coordinate(c.x, -c.y, -c.z),
    c => Coordinate(-c.x, c.y, -c.z),
    c => Coordinate(-c.x, -c.y, c.z),
    c => Coordinate(c.x, c.z, -c.y),
    c => Coordinate(c.x, -c.z, c.y),
    c => Coordinate(-c.x, c.z, c.y),
    c => Coordinate(-c.x, -c.z, -c.y),
    c => Coordinate(c.y, c.z, c.x),
    c => Coordinate(c.y, -c.z, -c.x),
    c => Coordinate(-c.y, c.z, -c.x),
    c => Coordinate(-c.y, -c.z, c.x),
    c => Coordinate(c.y, c.x, -c.z),
    c => Coordinate(c.y, -c.x, c.z),
    c => Coordinate(-c.y, c.x, c.z),
    c => Coordinate(-c.y, -c.x, -c.z),
    c => Coordinate(c.z, c.x, c.y),
    c => Coordinate(c.z, -c.x, -c.y),
    c => Coordinate(-c.z, c.x, -c.y),
    c => Coordinate(-c.z, -c.x, c.y),
    c => Coordinate(c.z, c.y, -c.x),
    c => Coordinate(c.z, -c.y, c.x),
    c => Coordinate(-c.z, c.y, c.x),
    c => Coordinate(-c.z, -c.y, -c.x)
  )

  def subtract(c1: Coordinate, c2: Coordinate): Coordinate = Coordinate(c1.x - c2.x, c1.y - c2.y, c1.z - c2.z)

  def comparePerm(coords: List[Coordinate]): Boolean = {
    var distances: Map[Coordinate, Int] = Map.empty.withDefaultValue(0)

    set.foreach(c1 => {
      coords.foreach(c2 => {
        val diff = subtract(c2, c1)
        distances = distances.updatedWith(diff)(v => Some(v.getOrElse(0) + 1))
        if (distances(diff) >= 12)
          Scanner.set ++= coords.map(c => subtract(c, diff))
          this.added = true
          this.location = diff
          return true
      })
    })
    false
  }

  def compare(): Unit = {
    permutations.foreach(p =>
      val permuted = beacons.map(p)
      if (comparePerm(permuted)) return
    )
  }
}

object Scanner {
  case class Coordinate(x: Int, y: Int, z: Int)

  var set: Set[Coordinate] = Set.empty


  def from(input: List[String]): Scanner = {
    Scanner(input.map(line => {
      val spl = line.split(",").map(_.toInt)
      Coordinate(spl(0), spl(1), spl(2))
    }))
  }
}

