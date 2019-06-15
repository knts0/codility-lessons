package lesson8

// result: https://app.codility.com/demo/results/trainingKEHTED-K6D/

object EquiLeader {
  def getLeader(ary: Array[Int]): Int = {
    var cnt = scala.collection.mutable.Map.empty[Int, Int]
    ary.foreach { v =>
      if (cnt.contains(v)) cnt(v) += 1
      else cnt += (v -> 1)

      if (cnt(v) * 2 > ary.length) return v
    }
    -1000000010
  }

  def solution(a: Array[Int]): Int = {
    val leader = getLeader(a)
    if (leader == -1000000010) {
      0
    } else {
      var cnt = Array.fill(a.length + 1)(0)
      a.zipWithIndex.foreach { case (v, idx) =>
        cnt(idx + 1) = cnt(idx) + (if (v == leader) 1 else 0)
      }

      var ans = 0
      a.zipWithIndex.foreach { case (v, idx) =>
        val left = cnt(idx + 1)
        val right = cnt(a.length) - cnt(idx + 1)
        if (left * 2 > idx + 1 && right * 2 > a.length - idx - 1) ans += 1
      }
      ans
    }
  }
}
