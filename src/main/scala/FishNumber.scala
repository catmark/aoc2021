class FishNumber(var left: Either[FishNumber, Int], var right: Either[FishNumber, Int]) {

  override def toString: String =
    var str: String = "["
    left match {
      case Left(fv) => str += fv.toString
      case Right(i) => str += i
    }
    str += ","
    right match {
      case Left(fv) => str += fv.toString
      case Right(i) => str += i
    }
    str += "]"
    str

  def addRight(value: Int, location: String): Unit =
    if (location == "left") {
      left match {
        case Right(i) =>
          left = Right(i + value)
        case Left(_) =>
      }
    } else {
      right match {
        case Right(i) =>
          right = Right(i + value)
        case Left(_) =>
      }
    }

  def split(): Boolean =
    left match {
      case Left(fn) => if (fn.split()) return true
      case Right(i) if i >= 10 =>
        left = Left(FishNumber(Right(i / 2), Right((i + 1) / 2)))
        return true
      case default =>
    }
    right match {
      case Left(fn) => if (fn.split()) return true
      case Right(i) if i >= 10 =>
        right = Left(FishNumber(Right(i / 2), Right((i + 1) / 2)))
        return true
      case default =>
    }
    false

  def explode(level: Int): Boolean = {
    var exploded = false
//    var leftie = leftNeighbour
    left match {
      case Left(value) if level == 4 && !FishNumber.exploded =>
//        println("Exploding left" + toString)
        exploded = true
        FishNumber.exploded = true
        FishNumber.leftNeighbour.foreach(leftNb => leftNb._1.addRight(value.left.right.get, leftNb._2))
        FishNumber.addToRight = Some(value.right.right.get)
        left = Right(0)
      case Left(value) =>
        exploded = exploded | value.explode(level + 1)
      case Right(i) =>
//        println("Checking whether to add " + i + " and " + FishNumber.addToRight)
        FishNumber.addToRight match {
          case Some(v) =>
//            println("Adding " + i + " to " + v)
            left = Right(i + v)
            FishNumber.addToRight = None
          case None => FishNumber.leftNeighbour = Some(this, "left")
        }
    }

    right match {
      case Left(value) if level == 4 && !FishNumber.exploded =>
//        println("Exploding right" + toString)
        exploded = true
        FishNumber.exploded = true
        FishNumber.leftNeighbour.foreach(leftNb => leftNb._1.addRight(value.left.right.get, leftNb._2))
        FishNumber.addToRight = Some(value.right.right.get)
        right = Right(0)
      case Left(value) =>
        exploded = exploded | value.explode(level + 1)
      case Right(i) =>
        FishNumber.addToRight match {
          case Some(v) =>
            right = Right(i + v)
            FishNumber.addToRight = None
          case None => FishNumber.leftNeighbour = Some(this, "right")
        }
    }
    exploded
  }
}

object FishNumber {
  var exploded = false
  var addToRight: Option[Int] = None
  var leftNeighbour: Option[(FishNumber, String)] = None

  def create(str: Input): FishNumber = {
    str.next(1)

    val left = str.peek() match {
      case c if c.isDigit => Right(str.next(1).toInt)
      case default => Left(FishNumber.create(str))
    }
    str.next(1)

    val right = str.peek() match {
      case c if c.isDigit => Right(str.next(1).toInt)
      case default => Left(FishNumber.create(str))
    }
    str.next(1)
    FishNumber(left, right)
  }

  def add(f1: FishNumber, f2: FishNumber) = FishNumber(Left(f1), Left(f2))
}
