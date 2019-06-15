package lesson7

// Bracketsと同じやり方でいける
// result: https://app.codility.com/demo/results/trainingWKGPVA-B4P/

object Nesting {
  def solution(s: String): Int = {
    // write your code in Scala 2.12
    val stack = new scala.collection.mutable.Stack[Char]()
    s.foreach { v =>
      if (v == '(') {
        stack.push(v)
      } else {
        if (stack.nonEmpty) stack.pop()
        else return 0
      }
    }
    if (stack.size == 0) return 1
    else return 0
  }
}
