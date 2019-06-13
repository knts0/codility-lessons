package lesson9

// result1: https://app.codility.com/demo/results/trainingZC3DCY-8TJ/
// result2: https://app.codility.com/demo/results/training3ZWPBY-UTK/

object MaxProfit {
  def solution(a: Array[Int]): Int = {
    if (a.isEmpty) return 0

    var min = a(0)
    var max = 0
    a.tail.foreach { v =>
      val profit = v - min
      if (profit > max) max = profit

      if (v < min) {
        min = v
      }
    }
    max
  }
}
