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
  def pascal(c: Int, r: Int): Int = {
    if (c==0) 
      1
    else
      if (r==0)
        0
        else
          pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def bal(count: Int , chars:List[Char]): Boolean = {
      if (chars.isEmpty)
        (count==0)
      else
        if (chars.head=='(') 
          bal(count+1,chars.tail)
        else
          if (chars.head==')')
            if (count==0) false else bal(count-1,chars.tail)
          else
            bal(count,chars.tail)
    }
    bal(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money<0 || coins.isEmpty)
      0
    else
      if (money==0) 1
      else {
        countChange(money-coins.head,coins)+countChange(money,coins.tail)
      }
  }
}
