package lesson3

object FrogJmp {
  def solution(x: Int, y: Int, d: Int): Int = {
    val res = (y - x) / d
    if ((y - x) % d == 0) res
    else res + 1
  }
}
