package lesson4

// Boolean配列をmark用意して、各正整数が出現していったかどうかを記録、最後にそれを前から見ていく方針
// MissingIntergerの方のコードはmarkで1,000,010個分初期化するためtimeout errorになっていたよう。
// そもそも配列の長さは最大で10^5であるため、答えは最大でも1000001。よって1,000,010の確保は不要だった。
// （codility、timeoutがシビア）

object MissingInteger {
  def solution(a: Array[Int]): Int = {
    val mark = Array.fill(1000010)(false)

    a.foreach { v =>
      if (v > 0) mark(v) = true
    }

    mark.zipWithIndex.foreach { v =>
      if (v._2 > 0 && !v._1) return v._2
    }
    return 0
  }
}

object MissingInteger2 {
  def solution(a: Array[Int]): Int = {
    val mark = Array.fill(100010)(false)

    a.foreach { v =>
      if (v > 0 && v <= 100000) mark(v) = true
    }

    mark.zipWithIndex.foreach { v =>
      if (v._2 > 0 && !v._1) return v._2
    }
    return 0
  }
}

// HashSet使う、immutableに書く
// missing integerの候補としては1〜100001で十分だった（回答では1000001までとってる）
// result: https://app.codility.com/demo/results/trainingKGPTJJ-U74/
object MissingInteger3 {
  def solution(a: Array[Int]): Int = {
    val st = a.foldLeft(scala.collection.immutable.HashSet.empty[Int]) { (acc, v) =>
      if (acc.contains(v)) acc else acc + v
    }
    (1 to 1000001).foldLeft(0) { (acc, v) =>
      if (acc == 0 && !st.contains(v)) v else acc
    }
  }
}
