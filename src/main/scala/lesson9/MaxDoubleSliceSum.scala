package lesson9

// 1回目は100%じゃなかったです。結果のURLメモるのを忘れました…
// 2回目: https://app.codility.com/demo/results/trainingGS62VP-STC/

object MaxDoubleSliceSum {

  // you can write to stdout for debugging purposes, e.g.
  // println("this is a debug message")

  // Yは1〜N-2の値を取りうる。Y = iのとき
  // leftSum = A[X + 1] + A[X + 2] + ... + A[i − 1] = (i - 1までの累積和) - (Xまでの累積和)
  // rightSum = A[i + 1] + A[Y + 2] + ... + A[Z − 1] = (Z - 1までの累積和) - (iまでの累積和)
  // のいずれも最大となるようにX, Zを選べば良い = 0 〜 i - 1までの累積和で最小値、i 〜 Z-1までの累積和で最大値を選ぶ。
  // そうしてできたleftSum + rightSumの最大値をYを変化させながら探していく

  // 以下の前処理をあらかじめやっておけば計算量がO(N)となり間に合う
  // 1. 配列aの各iまでの累積和accSumsを求める
  // 2. minAccSums(i): 0 ≦ k ≦ iのaccSum(k)のうち最小値 leftSumの最大を求めるのに使う
  // 3. maxAccSums(N - 2 - j): j ≦ k ≦ N - 2のaccSum(k)のうち最大値 rightSumの最大を求めるのに使う

  def solution(a: Array[Int]): Int = {
    val accSums = a.scanLeft(0) { (accSum, v) =>
      val newAccSum = accSum + v
      newAccSum
    }.tail
    val minAccSums = accSums.scanLeft(1000000100) { (min, v) =>
      math.min(min, v)
    }.tail
    val maxAccSums = accSums.reverse.tail.scanLeft(-1000000100) { (max, v) =>
      math.max(max, v)
    }.tail

    // Yを1〜N-2まで変化させる
    (1 to a.length - 2).foldLeft(-1000000100) { (acc, y) =>
      val leftSum = accSums(y - 1) - minAccSums(y - 1)
      val rightSum = maxAccSums(a.length - 2 - y) - accSums(y)
      math.max(acc, leftSum + rightSum)
    }
  }
}
