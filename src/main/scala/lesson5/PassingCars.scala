package lesson5

object PassingCars {
  def solution(a: Array[Int]): Int = {
    var accSum = Array.fill(a.length)(0)
    a.zipWithIndex.foldLeft(0) { (acc, v) =>
      var nextAcc = acc
      if (v._1 == 1) nextAcc += v._1
      accSum(v._2) = nextAcc
      nextAcc
    }
    var res = 0L
    a.zipWithIndex.foreach { v =>
      if (v._1 == 0) {
        res += accSum(a.length - 1) - accSum(v._2)
      }
    }
    if (res > 1000000000L) -1
    else res.toInt
  }
}
