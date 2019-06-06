package lesson5

object GenomicRangeQuery {
  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    // cnt(i)(j) に、文字列sの添字iまでのDNA jの数の累積和が入るようにする
    var cnt = Array.fill(s.length, 4)(0)
    val mapping = Map[Char, Int] ( 'A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3 )
    s.zipWithIndex.foldLeft(Array.fill(4)(0)) { (acc, chr) =>
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

object GenomicRangeQuery2 {
  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    val mapping = Map[Char, Int] ( 'A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3 )
    // cnt(i + 1)(j) に、文字列sの添字iまでのDNA jの数の累積和が入るようにする
    // cnt(0)(j)は0
    val cnt = s.zipWithIndex.foldLeft(Array.fill(s.length + 1, 4)(0)) { (acc, chr) =>
      acc(chr._2 + 1)(mapping(chr._1)) = acc(chr._2)(mapping(chr._1)) + 1
      acc
    }

    var ans = Array.fill(p.length)(0)
    for (i <- 0 to p.length - 1) {
      val start = p(i)
      val end = q(i)
      val tempCnt = (0 to 3).map { v =>
        cnt(end + 1)(v) - cnt(start)(v)
      }
      ans(i) = tempCnt.indexWhere(_ > 0) + 1
    }
    ans
  }
}
