package lesson8

// result1: https://app.codility.com/demo/results/trainingQER9PC-5CB/
// result2(.toDoubleをやめた): https://app.codility.com/demo/results/trainingRG99D3-GCP/

class Dominator {
  def solution(a: Array[Int]): Int = {
    var cnt = scala.collection.mutable.Map.empty[Int, Int]
    a.zipWithIndex.foreach { v =>
      if (cnt.contains(v._1)) cnt(v._1) += 1
      else cnt += (v._1 -> 1)

      if (cnt(v._1) * 2 > a.length) return v._2
    }
    -1
  }
}
