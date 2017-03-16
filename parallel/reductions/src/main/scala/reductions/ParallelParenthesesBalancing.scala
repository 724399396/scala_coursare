package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def help(arr: Array[Char], count: Int): Boolean = {
      if (arr.isEmpty)
        count == 0
      else {
        val h = arr.head
        val next =
          if (h == '(') count + 1
          else if (h == ')') count - 1
          else count
        if (next < 0) false
        else help(arr.tail, next)
      }
    }

    help(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {


    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      ???
    }

    //reduce(0, chars.length) == ???

    def reduceOne(from: Int, until: Int): Array[Char] = {
      if (until - from <= threshold) {
        val stack: mutable.Stack[Char] = mutable.Stack()
        var idx = from
        while (idx < until) {
          val c = chars(idx)
          if (c == '(')
            stack.push(c)
          if (c == ')') {
            stack.headOption match {
              case Some('(') =>
                stack.pop()
              case _ =>
                stack.push(c)
            }

          }
          idx += 1
        }
        stack.reverse.toArray
      } else {
        val mid = (until + from) / 2
        val (l,r) = parallel(reduceOne(from, mid), reduceOne(mid, until))
        l ++ r
      }
    }

    balance(reduceOne(0, chars.length))
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
