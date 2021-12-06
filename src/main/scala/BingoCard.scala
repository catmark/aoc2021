class BingoCard(strings: List[String]) {
  private var done = false
  private var card: List[List[Int]] = strings.map(line => line.trim.split(' ').filter(_.nonEmpty).map(_.toInt).toList)

  def crossNumber(drawnNumber: Int): Option[Int] = {
    card = card.map(
      row => row.map(
        num => if (num == drawnNumber) -1 else num
      )
    )
    if (hasWon && !done) Some(calculateAnswer(drawnNumber)) else None
  }

  def hasWon: Boolean = {
    def check(card: List[List[Int]]): Boolean = card.exists(row => row.forall(_.==(-1)));
    check(card) || check(card.transpose)
  }

  def calculateAnswer(drawnNumber: Int): Int = {
    done = true
    card.flatten.filterNot(_.==(-1)).sum * drawnNumber
  }
}
