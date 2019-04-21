package lesson4

class FrogRiverOne {
  def solution(x: Int, a: Array[Int]): Int = {
    var times = Array.fill(100010)(-1)

    a.zipWithIndex.foreach { v =>
      if (times(v._1) == -1) times(v._1) = v._2
    }

    var check = true
    for (i <- 1 to x) {
      if (times(i) < 0) check = false
    }
    if (check) times.slice(1, x + 1).max
    else -1
  }
}
