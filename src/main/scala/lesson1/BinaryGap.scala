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
}
