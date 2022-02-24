class Input(var index: Int, string: String) {
  def next(count: Int): String = {
    val substr = string.substring(index, index + count)
    index += count
    substr
  }
  def peek(): Char = string(index)
}
