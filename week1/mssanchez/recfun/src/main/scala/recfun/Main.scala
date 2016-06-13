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
      if(c == 0 || c == r || r == 0)
        1
      else
        pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(acc: Int, chars: List[Char]): Boolean = chars match {
        case Nil => acc == 0
        case h :: tail =>
          h match {
            case '(' => balance(acc + 1, tail)
            case ')' => if(acc == 0) false else balance(acc - 1, tail)
            case _ => balance(acc, tail)
          }
      }

      if(chars.isEmpty)
        true
      else
        balance(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

  }
