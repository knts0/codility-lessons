package lesson4

import scala.collection.JavaConverters._

object MaxCounters {
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    var max_counter_val = 0
    var max = 0
    var cnt = scala.collection.mutable.HashMap.empty[Int, Int]

    a.foreach { v =>
      if (v == n + 1) {
        max_counter_val += max
        max = 0
        cnt = scala.collection.mutable.HashMap.empty[Int, Int]
      } else {
        if (cnt.contains(v - 1)) cnt(v - 1) += 1
        else cnt += (v - 1) -> 1
        if (max < cnt(v - 1)) max = cnt(v - 1)
      }
    }

    val ary = Array.fill(n)(0)
    ary.zipWithIndex.map(v => v._1 + max_counter_val + (if (cnt.contains(v._2)) cnt(v._2) else 0))
  }
}
