package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], balance: Int): Int =
      if (chars.isEmpty || balance < 0) balance
      else {
        val x :: xs = chars
        x match {
          case '(' => f(xs, balance + 1)
          case ')' => f(xs, balance - 1)
          case _ => f(xs, balance)
        }
      }
    f(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money <= 0 || coins.isEmpty) 0
    else {
      val x :: xs = coins
      countChange(money - x, coins) + countChange(money, xs)
    }
}