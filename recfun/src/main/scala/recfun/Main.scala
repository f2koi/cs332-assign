package recfun
import common._
import scala.annotation.tailrec

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
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balance_aux(chars: List[Char], opened: Int): Boolean = {
      if (chars.isEmpty) {
        opened == 0
      } else {
        val next_opened = if (chars.head == '(') {
          opened + 1
        } else if (chars.head == ')') {
          opened - 1
        } else {
          opened
        }
        if (next_opened < 0) {
          false
        } else {
          balance_aux(chars.tail, next_opened)
        }
      }
    }
    balance_aux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
