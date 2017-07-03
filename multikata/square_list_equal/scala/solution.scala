object Solution {
  def comp(a: List[Int], b: List[Int]): Boolean = {
    a.sorted.map(scala.math.pow(_, 2).toInt) == b.sorted
  }
}
