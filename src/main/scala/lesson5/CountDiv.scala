package lesson5

import scala.math._

// bが最大で2,000,000,000なので、O(N)はだめでO(1)にする必要がある->数学的に求める
// a以上でkで割り切れる数minDivisableByKを求め
// minDivisableByK <= bならばminDivisable以上b以下のkで割り切れる数が答え
// そうでないなら答えは0
// 結果：https://app.codility.com/demo/results/trainingY3T2U6-KNC/
// 反省：最初凡ミスで、toDoubleを忘れていて通らなくて時間をだいぶ食ってしまった

object CountDiv {
  def solution(a: Int, b: Int, k: Int): Int = {
    val minDivisableByK = ceil(a.toDouble / k).toInt * k
    if (minDivisableByK <= b) return (b - minDivisableByK) / k + 1
    else return 0
  }
}
