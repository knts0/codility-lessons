package lesson6

import scala.annotation.tailrec

// result1: https://app.codility.com/demo/results/trainingT3WRBG-VV8/
// result2: https://app.codility.com/demo/results/training2BGMW8-JMY/ オーバーフローを直した

object NumberOfDiscIntersections {

  @tailrec
  def upperBound(ary: Seq[Long], l: Int, r:Int, x: Long): Int = {
    if (r - l <= 1) {
      if (ary(l) > x) l else r
    } else if (ary((l + r) / 2) > x) {
      upperBound(ary, l, (l + r) / 2, x)
    } else {
      upperBound(ary, (l + r) / 2, r, x)
    }
  }

  def solution(a: Array[Int]): Int = {
    val edges = a.zipWithIndex.map{ case (radius, center) => (center.toLong - radius.toLong, center.toLong + radius.toLong) }.sortBy(_._1)
    var notIntersects = 0L
    val lefts = edges.map(_._1)
    edges.zipWithIndex.foreach { case ((left, right), idx) =>
      val bound = upperBound(lefts, 0, a.length, right)
      var cnt = a.length - bound
      notIntersects += cnt
    }
    val ans = a.length.toLong * (a.length.toLong - 1) / 2 - notIntersects
    if (ans > 10000000L) -1 else ans.toInt
  }
}
