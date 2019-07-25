package lesson10

object Peaks {
  // result: https://app.codility.com/demo/results/training23T7AB-PRN/
  def solution(a: Array[Int]): Int = {
    val n = a.length

    // a.lengthの約数を列挙する（分割するブロック数の候補）
    val factors = scala.collection.mutable.ArrayBuffer.empty[Int]

    var i = 1
    while(i * i <= n) {
      if (n % i == 0) {
        factors += i
        if(i * i != n) factors += n / i
      }
      i += 1
    }
    val sortedFactors = factors.sorted

    // peakを数えたもの
    // peaksCnt(i + 1) は、aの0番目〜i番目までのpeakの数　
    val peaksCnt = Array.fill(n + 1)(0)
    (2 to n - 1).foldLeft(0) { (acc, idx) =>
      val cnt = if (a(idx - 2) < a(idx - 1) && a(idx - 1) > a(idx)) acc + 1 else acc

      peaksCnt(idx) = cnt
      cnt
    }
    peaksCnt(n) = peaksCnt(n - 1)

    // 約数をループして、分割されたblockのすべてにpeakが存在するかチェックする
    var result = 0
    sortedFactors.foreach { blockNum =>
      val numPerBlock = n / blockNum  // 1ブロックあたりの個数
    val isAllBlockHasPeak = (0 to blockNum - 1).forall { v => peaksCnt((v + 1) * numPerBlock) - peaksCnt(v * numPerBlock) > 0 }
      if (isAllBlockHasPeak) result = blockNum
    }
    result
  }
}
