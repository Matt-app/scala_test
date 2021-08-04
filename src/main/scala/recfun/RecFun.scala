package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do {
        print(s"${pascal(col, row)} ")
      }
      println()
//      print(balance("I told him (that it's not (yet) done). (But he wasn't listening".toList))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
      if (r == 0 || c == 0 || c == r) then 1 else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def myBalance(chars: List[Char], count: Int): Boolean = {
      if chars.isEmpty || count < 0 then count == 0 else {
        if chars.head == '(' then myBalance(chars.tail, count+1) else {
          if chars.head == ')' then myBalance(chars.tail, count-1) else myBalance(chars.tail, count)
        }
      }
    }
    myBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if money< 0 || coins.isEmpty then 0 else {
      if money % coins.head == 0 && coins.tail.isEmpty then 1 else {
        countChange(money, coins.tail) + countChange(money-coins.head, coins)
      }
    }
  }

