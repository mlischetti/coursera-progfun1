package recfun

import scala.collection.mutable.Stack

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
      if(c == r || c == 0)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val parentheses = Stack[Char]()
      filterParentheses(chars, parentheses).isEmpty

    }

    def filterParentheses(chars: List[Char], parentheses: Stack[Char]): Stack[Char] = {
      if (chars.nonEmpty) {
        chars.head match {
          case '(' =>
            parentheses.push(chars.head)
            filterParentheses(chars.tail, parentheses)

          case ')' =>
            if (parentheses.nonEmpty){
              parentheses.pop()
              filterParentheses(chars.tail, parentheses)
            }
            else
              parentheses.push('n')

          case _ => filterParentheses(chars.tail, parentheses)
        }
      }

      parentheses
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(wallet: Int, lstCoins: List[Int]): Int ={
        if (wallet == 0)
          1
        else if (wallet < 0 || lstCoins.isEmpty)
          0
        else
          count(wallet - lstCoins.head, lstCoins) + count(wallet, lstCoins.tail)
      }

      count(money, coins)
    }


  }
