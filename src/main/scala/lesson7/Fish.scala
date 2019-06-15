package lesson7

// aを先頭から見ていき、
// 1. b(i) == 1ならstackに積む
// 2. b(i) == 0なら、stackにすでに積まれている魚に必ず出会うため、これらとサイズを比較する。
//    stackの魚のうち、魚iよりもサイズが大きいものが存在したら、魚iは生き残らない
//    stackに積まれたすべての魚より魚iが大きい場合は魚iが生き残る

// result: https://app.codility.com/demo/results/trainingCTUK5C-BKS/
// 疑問：これで計算量がO(N)な理由がわからない

object Fish {
  def solution(a: Array[Int], b: Array[Int]): Int = {
    val aliveDowns = new scala.collection.mutable.Stack[Int]
    var aliveUpsCnt = 0

    a.zip(b).foreach { case (size, direction) =>
      if (direction == 1) {
        aliveDowns.push(size)
      } else {
        var finished = false
        while (!aliveDowns.isEmpty && !finished) {
          val down = aliveDowns.top
          if (down > size) {
            finished = true
          } else {
            aliveDowns.pop()
          }
        }
        if (aliveDowns.isEmpty) aliveUpsCnt += 1
      }
    }
    aliveUpsCnt + aliveDowns.size
  }
}
