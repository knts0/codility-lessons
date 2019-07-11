package lesson4

class FrogRiverOne {
  // 各1〜xの位置には最短何秒後に葉が落ちるのかを求め、それらの最大値が答えとなる
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

  // ↑のコードを簡潔にしようと頑張った
  // result: https://app.codility.com/demo/results/trainingYQ3X2V-E66/
  def solution2(x: Int, a: Array[Int]): Int = {
    // 各1〜xの位置に、最短何秒後に葉が落ちるのかを格納した配列 (0-indexed)
    val earliestTimes = a.zipWithIndex.foldLeft(Array.fill(x)(-1)) { case (acc, (v, idx)) =>
      if (v <= x && acc(v - 1) == -1) acc(v - 1) = idx
      acc
    }
    earliestTimes.foldLeft(-1) { (acc, v) =>
      if (v == -1) return -1
      math.max(acc, v)
    }
  }
}
