package lesson3

object PermMissingElem {
  // O(1)解法
  // 1〜N+1を足した数 - 配列の総和 = 足りない数　になる
  def solution(a: Array[Int]): Int = {
    val total = (1 to a.length + 1).sum
    val actualTotal = a.sum

    total - actualTotal
  }

  // 愚直にループするO(N)解でも間に合う
  // Scalaっぽくvarを使わず書いてみる
  // result: https://app.codility.com/demo/results/trainingEEPYQD-PGR/
  def solution2(a: Array[Int]): Int = {
    val flags = a.foldLeft(Array.fill(a.length + 1)(false)) { (acc, v) =>
      acc(v - 1) = true
      acc
    }
    flags.zipWithIndex.foldLeft(0) { case (acc, (flag, idx)) =>
      if (flag) acc else idx + 1
    }
  }
}
