package lesson3

import scala.math.abs

object TapeEquilibrium {
  def solution(a: Array[Int]): Int = {
    var accSum = Array.fill(a.length)(0)

    a.zipWithIndex.foldLeft(0) { (acc, v) => {
      val sum = acc + v._1
      accSum(v._2) = sum
      sum
    }}

    var res = 1000000
    for (i <- 1 to a.length - 1) {
      val left = accSum(i - 1)
      val right = accSum(a.length - 1) - accSum(i - 1)
      if (res > abs(left - right)) res = abs(left -right)
    }

    res
  }
}
