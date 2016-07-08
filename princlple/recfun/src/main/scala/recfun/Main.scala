package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(just an) example".toList))
    println(balance("())(".toList))

    println(countChange(10, List(1,2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c > r || c < 0) 0
    else {
      if (c == 0 || c == r) 1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def help(list: List[Char], meet: Int): Boolean = {
      if (list.isEmpty) meet == 0
      else {
        val c = list.head
        if (c == '(')
          help(list.tail, meet+1)
        else if (c == ')')
          if (meet <= 0) {
            false
          }
          else {
            help(list.tail, meet-1)
          }
        else
          help(list.tail, meet)
      }
    }
    help(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    var cache: Map[Int, Set[List[Int]]] = Map()

    def help(leftMoney: Int): Set[List[Int]] = {
      cache.get(leftMoney) match {
        case Some(x) => x
        case None =>
          val res = (for {
            coin <- coins
            nextMoney = leftMoney - coin
          } yield {
            if (nextMoney > 0) help(nextMoney).map(coin :: _)
            else if (nextMoney == 0)
              List(List(coin))
            else
              List()
          }).flatten.map(_.sorted).toSet
          cache += (leftMoney -> res)
          res
      }

    }

    help(money).filter(!_.isEmpty).size
  }
}
