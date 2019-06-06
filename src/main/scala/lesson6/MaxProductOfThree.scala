package lesson6

object MaxProductOfThree {
  def solution(a: Array[Int]): Int = {
    // 結果：https://app.codility.com/demo/results/trainingQKVZCZ-DNZ/

    // 最大は
    // 1. 正数が3つ以上ある場合以下のいずれか
    // 　　- 正数のうち最大の3つの積
    //　　 - （負数が2つ以上ある場合）負数のうち最小の2つと、正数の最大値の積
    // 2. 正数が2つ or 1つの場合
    //　　 - （負数が2つ以上ある場合）負数のうち最小の2つと、正数の最大値の積
    //　　 - （負数が2つない場合）ソートして上から3つを掛け合わせる
    // 3. 上記以外の場合は上から3つをかける
    val sorted = a.sorted
    val positive = a.sorted.filter(_ > 0)
    val negative = a.sorted.filter(_ < 0)
    if (positive.length >= 3) {
      if (negative.length >= 2) {
        return Seq(positive.takeRight(3).product, positive.takeRight(1).head * negative.take(2).product).max
      } else {
        return positive.takeRight(3).product
      }
    } else if (positive.length >= 1) {
      if (negative.length >= 2) {
        return Seq(positive.takeRight(3).product, positive.takeRight(1).head * negative.take(2).product).max
      } else {
        return sorted.takeRight(3).product
      }
    } else {
      return sorted.takeRight(3).product
    }
  }
}
