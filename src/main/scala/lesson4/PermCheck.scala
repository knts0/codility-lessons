package lesson4

object PermCheck {
  // HashMapに各数が何回登場したかをカウントしていって、
  // 登場回数が1回以外の場合や、Nより大きい数が登場していた場合0を返すようにする
  def solution(a: Array[Int]): Int = {
    var cnt = collection.mutable.HashMap.empty[Int, Int]

    a.foreach { v =>
      if (cnt.contains(v)) cnt(v) += 1
      else cnt += v -> 1
    }

    var res = 1
    cnt.foreach { v =>
      if (v._1 > a.length || v._2 != 1) res = 0
    }
    res
  }

  // 配列aの数をHashSetに格納していく。もしすでにHashSetに存在するならば、配列aには同じ数が2つ以上含まれることになる。
  // 配列aに同じ数は含まれず、かつ、配列aの最大値がNであればpermutationである。
  // がんばってfoldLeftで書いてみた
  // result: https://app.codility.com/demo/results/trainingNJ2F2Q-KBE/
  def solution2(a: Array[Int]): Int = {
    var st = collection.mutable.HashSet.empty[Int]

    val (isUnique, max) = a.foldLeft((true, 0)) { case ((isUnique, max), v) =>
      val nextUnique = !st.contains(v)
      st += v
      (isUnique && nextUnique, math.max(max, v))
    }
    if (isUnique && max == a.length) 1
    else 0
  }
}
