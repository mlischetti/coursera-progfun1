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
      if(c <= 0 || r <= 0 || c == r) 1
      else pascal(c,r-1) + pascal(c-1,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      isBalanced(chars, 0)
    }

    def isBalanced(chars: List[Char], open: Int): Boolean = {
      if(open < 0) false
      else{
        chars match {
          case Nil => true
          case head :: tail => head match {
            case '(' => isBalanced(tail,open + 1)
            case ')' => isBalanced(tail, open - 1)
            case _ => isBalanced(tail, open)
          }
        }
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
