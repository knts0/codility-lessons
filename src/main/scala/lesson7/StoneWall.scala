package lesson7

// result: https://app.codility.com/demo/results/trainingUEHQYR-VDN/

object StoneWall {
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

}
