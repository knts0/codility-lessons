package lesson3

import scala.math.abs

object TapeEquilibrium {

  // 累積和accSumをあらかじめ求めておく方法
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

  // 累積和使わない
  // あらかじめ総和を出しておいて、配列を前から走査していけば、
  // 2つに分けた配列のそれぞれの総和の差の絶対値を求めることができる
  // result: https://app.codility.com/demo/results/trainingYZJSXQ-FY7/
  def solution2(a: Array[Int]): Int = {
    val total = a.sum
    var result = 1000000000
    a.reduceLeft { (acc, v) =>
      val part1 = acc
      val part2 = total - acc
      result = math.min(result, math.abs(part1 - part2))
      acc + v
    }
    result
  }
}
