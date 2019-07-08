package lesson2

object CyclicRotationSolution {
  def solution(a: Array[Int], k: Int): Array[Int] = {
    val res = new Array[Int](a.length)

    a.length match {
      case 0 =>
      case _ => a.zipWithIndex.foreach(v => {
        res((v._2 + k) % a.length) = v._1
      })
    }
    res
  }

  // Scalaっぽくコレクションメソッドを使って書いてみる
  // result: https://app.codility.com/demo/results/trainingXB2HQW-J2F/
  def solution2(a: Array[Int], k: Int): Array[Int] = {
    val shift = if (a.length > 0) k % a.length else 0

    a.takeRight(shift) ++ a.take(a.length - shift)
  }
}
