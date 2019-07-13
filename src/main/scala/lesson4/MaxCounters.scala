package lesson4

object MaxCounters {
  // HashMapで各数が何回increaseされたかカウントしておく。
  // またそのHashMapに入っているうちで最もincreaseされた回数が多いものをmaxとして保持する。
  // max counterの場合には、max_counter_valにmaxを足してHashMapとmaxをリセットする。
  // こうすれば、最終的なmax_counter_valにHashMapに格納された回数を足したものを配列で返したのが答えになる。
  // （HashMapでなくArrayを使うと、ループの中でのリセット操作にO(N)かかるので全体計算量がO(N^2)となり間に合わない）
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

  // ↑をなるべくvar使わず書いてみる
  // result: https://app.codility.com/demo/results/trainingW9MXXT-UZC/
  import scala.collection._
  def solution2(n: Int, a: Array[Int]): Array[Int] = {
    val (cntMap, maxTmp, maxNum) = a.foldLeft((mutable.HashMap.empty[Int, Int], 0, 0)) { case ((cnt, maxTmp, maxNum), v) =>
      if (v == n + 1) {
        (mutable.HashMap.empty[Int, Int], 0, maxNum + maxTmp)
      } else {
        if (cnt.contains(v)) cnt(v) += 1 else cnt += v -> 1
        (cnt, math.max(cnt(v), maxTmp), maxNum)
      }
    }
    val ary = Array.fill(n)(0)
    ary.zipWithIndex.map { case (v, idx) => v + maxNum + cntMap.getOrElse(idx + 1, 0) }
  }
}
