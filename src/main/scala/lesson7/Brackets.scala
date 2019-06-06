package lesson7

object Brackets {
  // https://app.codility.com/demo/results/trainingFR4HDA-U9W/

  def solution(s: String): Int = {
    val stack = new scala.collection.mutable.Stack[Char]()
    val chars = Map(
      '(' -> true, '[' -> true, '{' -> true,
      ')' -> false, ']' -> false, '}' -> false
    )
    val pairs = Map(
      '(' -> ')', '[' -> ']', '{' -> '}'
    )
    s.foreach { v =>
      if (chars(v)) {
        stack.push(v)
      } else {
        val chrOpt = stack.headOption
        chrOpt match {
          case Some(chr) if pairs(chr) == v =>
          case _                            => return 0
        }
        stack.pop()
      }
    }
    if (stack.size == 0) return 1
    else return 0
  }
}
