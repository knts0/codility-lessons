package lesson1

object BinaryGap {
  def solution(n: Int): Int = {
    val binaryString = Integer.toBinaryString(n).toSeq

    var i = 0
    var res = 0
    while(i < binaryString.length) {
      var cnt = 0
      if (binaryString(i) == '1') {
        i += 1
        while (i < binaryString.length && binaryString(i) == '0') {
          i += 1
          cnt += 1
        }
        if (i < binaryString.length && binaryString(i) == '1') res = Seq(cnt, res).max
      } else {
        i += 1
      }

    }

    res
  }

  // 最初のコードは汚かったので直しました。。
  // 二進数表示の文字列は1... というように必ず1から始まるので
  // 0の場合はcntを増やす、1の場合はcntとansのうち大きい方をとる
  // としていけばansに答えが入る
  // result: https://app.codility.com/demo/results/training87UHY9-6VA/
  def solution2(n: Int): Int = {
    val binaryString = Integer.toBinaryString(n).toSeq

    var ans = 0
    var cnt = 0
    binaryString.foreach { v =>
      if (v == '1') {
        ans = math.max(ans, cnt)
        cnt = 0
      } else {
        cnt += 1
      }
    }
    ans
  }
}
