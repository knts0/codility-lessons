package lesson7

object StoneWall {
  // result: https://app.codility.com/demo/results/trainingUEHQYR-VDN/
  def solution(h: Array[Int]): Int = {
    var cnt = 0
    val stack = new scala.collection.mutable.Stack[Int]()

    h.foreach { v =>
      var finished = false
      while (!stack.isEmpty && !finished) {
        if (stack.top < v) {
          finished = true
          stack.push(v)
        } else if (stack.top == v) {
          finished = true
        } else {
          stack.pop()
          cnt += 1
        }
      }
      if (stack.isEmpty) stack.push(v)
    }

    cnt + stack.size
  }

  // アルゴリズム改善
  // result: https://app.codility.com/demo/results/trainingMGSC8Q-H2S/
  def solution2(h: Array[Int]): Int = {
    val stack = new scala.collection.mutable.Stack[Int]()

    h.foldLeft(0) { (acc, v) =>
      while (!stack.isEmpty && stack.top > v) {
        stack.pop()
      }

      if (stack.isEmpty || stack.top < v) {
        stack.push(v)
        acc + 1
      } else {
        acc
      }
    }
  }

}
