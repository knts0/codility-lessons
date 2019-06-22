package lesson10

object MinPerimeterRectangle {
  // result: https://app.codility.com/demo/results/training3G4QRK-KB2/

  def solution(n: Int): Int = {
    var x = 1
    var res = 2000000100
    while (x * x <= n) {
      if (n % x == 0) {
        val a = x
        val b = n / x
        res = math.min(2 * (a + b), res)
      }
      x += 1
    }
    res
  }
}
