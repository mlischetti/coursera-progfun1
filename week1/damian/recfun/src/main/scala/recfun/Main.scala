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

  //TODO: to correct
    def balance(chars: List[Char]): Boolean = {
      filterParentheses(chars).isEmpty
    }

    def filterParentheses(chars: List[Char]): Stack[Char] = {
      val parentheses = Stack[Char]()

      if (chars.nonEmpty) {
        chars.head match {
          case '(' => {
            parentheses.push(chars.head)
            filterParentheses(chars.tail)
          }
          case ')' => {
            if (parentheses.nonEmpty){
              parentheses.pop()
              filterParentheses(chars.tail)
            }
            else
              parentheses.push('n')
          }
          case _ => filterParentheses(chars.tail)
        }
      }

      parentheses
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
