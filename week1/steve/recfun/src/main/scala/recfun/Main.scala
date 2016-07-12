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
    if(c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = bal(chars, 0) == 0

  private def bal(chars: List[Char], counter: Int): Int = chars match {
    case head :: tail =>
      val result = counter + (head match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      })
      if (result < 0) result
      else bal(tail, result)
    case Nil => counter
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else coins match {
      case head :: tail => countChange(money - head, coins) + countChange(money, tail)
      case Nil => 0
    }
  }

}
