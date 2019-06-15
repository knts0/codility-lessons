package lesson9

// result: https://app.codility.com/demo/results/trainingPXF9Q6-E3G/

object MaxSliceSum {
  def solution(a: Array[Int]): Int = {
    var ans = −(1L << 40)
    var currentMaxSum = −(1L << 40)
    a.foreach { v =>
      currentMaxSum = Math.max(v, currentMaxSum + v)
      ans = Math.max(ans, currentMaxSum)
    }
    ans
  }
}
