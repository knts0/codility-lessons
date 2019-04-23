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
}
