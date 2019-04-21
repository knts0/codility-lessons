package lesson5

object GenomicRangeQuery {
  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    var cnt = Array.fill(s.length, 4)(0)
    val mapping = Map[Char, Int] ( 'A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3 )
    s.zipWithIndex.foldLeft(Array.fill(4)(0)) { (acc, chr) =>
      var newAcc = acc
      newAcc(mapping(chr._1)) += 1
      cnt(chr._2) = newAcc
      newAcc
    }

    var ans = Array.fill(p.length)(0)
    for (i <- 0 to p.length - 1) {
      val start = p(i)
      val end = q(i)
      val tempCnt = (0 to 3).map { v =>
        cnt(end)(v) - (if (start > 0) cnt(start - 1)(v) else 0)
      }
      ans(i) = tempCnt.indexWhere(_ > 0) + 1
    }
    ans
  }
}
