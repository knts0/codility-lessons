package lesson4

object PermCheck {
  def solution(a: Array[Int]): Int = {
    var cnt = collection.mutable.HashMap.empty[Int, Int]

    a.foreach { v =>
      if (cnt.contains(v)) cnt(v) += 1
      else cnt += v -> 1
    }

    var res = 1
    cnt.foreach { v =>
      if (v._1 > a.length || v._2 != 1) res = 0
    }
    res
  }
}
