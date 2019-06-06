package lesson6

object Distinct {
  def solution(a: Array[Int]): Int = {
    // シンプルにSetに入れていく
    var st = scala.collection.mutable.Set.empty[Int]
    a.foreach { v =>
      st.add(v)
    }
    st.size
  }
}
