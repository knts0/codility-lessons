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

// returnを書かないようにする
object MissingInteger3 {
  def solution(a: Array[Int]): Int = {
    val mark = Array.fill(100010)(false)

    a.foreach { v =>
      if (v > 0 && v <= 100000) mark(v) = true
    }

    var answerAppeared = false // 答えが現れたかどうか
    var ans = 0 // 答え
    mark.zipWithIndex.foreach { v =>
      if (v._2 > 0 && !v._1 && !answerAppeared) {
        ans = v._2
        answerAppeared = true
      }
    }
    ans
  }
}
