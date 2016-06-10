package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || c >= r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isParenthesesBalanced(balanceOfParentheses: Int, chars: List[Char]): Boolean = {
      if (balanceOfParentheses < 0)
        return false

      chars match {
        case Nil => balanceOfParentheses == 0
        case x :: tail => {
          val parenthesesFactor = if (x == '(') 1 else if (x == ')') -1 else 0
          isParenthesesBalanced(balanceOfParentheses + parenthesesFactor, chars.tail)
        }
      }
    }

    isParenthesesBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        count(money - coins.head, coins) + count(money, coins.tail)
    }

    if (money == 0) 0 else count(money, coins)
  }
}
