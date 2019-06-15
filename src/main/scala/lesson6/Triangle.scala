package lesson6

// 配列の要素3つの組み合わせをすべてチェックするとO(N^3)となるのでそれ以外の方法を考える。
// a ≦ b ≦ cを満たす数があった時にそれらがTriangularでなければ、
// a, b, cのうち2つと、d < a や c < e をみたす数との組もTriangularにならないので、
// 配列をソートし連続する3つの組がTriangularかをチェックしていけばよい

// result1: https://app.codility.com/demo/results/trainingCFPHHB-MPB/ オーバーフローの考慮漏れ
// result2: https://app.codility.com/demo/results/trainingQ4SPCS-JRE/ 配列aのIntをLongに変換する。（こうしなくても不等式を変えて差をとって比較すればIntのままでも行けたが）

object Triangle {
  def solution(a: Array[Int]): Int = {
    if (a.length < 3) return 0
    val itr = a.sorted.map(_.toLong).sliding(3)
    while(itr.hasNext) {
      val partial = itr.next
      if (partial(0) + partial(1) > partial(2) && partial(1) + partial(2) > partial(0) && partial(2) + partial(0) > partial(1)) return 1
    }
    0
  }
}
