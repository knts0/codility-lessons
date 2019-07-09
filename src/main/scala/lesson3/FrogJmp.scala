package lesson3

// X, Y and D are integers within the range [1..1,000,000,000];
// の制約があるので、O(1)の解法でないとだめ

object FrogJmp {
  def solution(x: Int, y: Int, d: Int): Int = {
    val res = (y - x) / d
    if ((y - x) % d == 0) res
    else res + 1
  }

  // よりシンプルに、ceilを使って書く
  // https://app.codility.com/demo/results/trainingDJP28N-ZPG/
  def solution2(x: Int, y: Int, d: Int): Int = {
    math.ceil((y - x).toDouble / d).toInt
  }
}
