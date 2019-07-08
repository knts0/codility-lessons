package lesson2

object OddOccurrencesInArray {
  def solution(a: Array[Int]): Int = {
    val sortedArray = a.sorted
    var i = 0
    var res = 0
    while (i < sortedArray.length) {
      val num = sortedArray(i)
      var cnt = 0
      while (i < sortedArray.length && sortedArray(i) == num) {
        i += 1
        cnt += 1
      }
      if (cnt % 2 == 1) res = num
    }
    res
  }

  // ScalaっぽくfoldLeftで書いてみました
  // result: https://app.codility.com/demo/results/trainingTDVTWY-S5D/
  def solution2(a: Array[Int]): Int = {
    val cnt = a.foldLeft(scala.collection.mutable.HashMap.empty[Int, Int]) { (acc, v) =>
      val num = if (acc.contains(v)) acc(v) else 0
      acc += (v -> (num + 1))
    }
    cnt.foldLeft(0) { (acc, v) =>
      if (v._2 % 2 == 1) v._1 else acc
    }
  }

  // ↑上のコードをimmutableなHashMapにすると速度がでなくなる
  // result: https://app.codility.com/demo/results/trainingUPASHC-SJ8/
  def solution3(a: Array[Int]): Int = {
    val cnt = a.foldLeft(scala.collection.immutable.HashMap.empty[Int, Int]) { (acc, v) =>
      val num = if (acc.contains(v)) acc(v) else 0
      acc + (v -> (num + 1))
    }
    cnt.foldLeft(0) { (acc, v) =>
      if (v._2 % 2 == 1) v._1 else acc
    }
  }
}
