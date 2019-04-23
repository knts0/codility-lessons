package lesson2

object OddOccurrencesInArray {
  def solution(a: Array[Int]): Int = {
    val sortedArray = a.sorted
    var i = 0
    var res = 0
    while (i < sortedArray.length) {
      val num = sortedArray(i)
      var cnt = 0
      while (i < sortedArray.length && sortedArray(i) == num) {
        i += 1
        cnt += 1
      }
      if (cnt % 2 == 1) res = num
    }
    res
  }
}
