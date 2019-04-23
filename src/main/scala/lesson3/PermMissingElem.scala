package lesson3

object PermMissingElem {
  def solution(a: Array[Int]): Int = {
    val total = (1 to a.length + 1).sum
    val actualTotal = a.sum

    total - actualTotal
  }
}
