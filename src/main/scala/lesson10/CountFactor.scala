package lesson10

object CountFactor {
  // result: https://app.codility.com/demo/results/trainingXBAPTF-SAT/

  // √n以下の正の整数xに対して、nを割り切ることができるか調べる
  // 割り切ることができる場合 n = x * aと表されるが a != xの場合のみ+2, a == xの場合+1していく
  // 桁オーバーフロー回避のためnやxはLongに変換する

  def solution(n: Int): Int = {
    var x = 1L
    var res = 0
    while (x * x <= n.toLong) {
      if (n.toLong % x == 0) {
        res += 1
        if (n.toLong / x != x) res += 1
      }
      x += 1
    }
    res
  }
}
