package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert 2 return min") = forAll{ (m: Int, n: Int) =>
    val a = insert(m,empty)
    val b = insert(n,a)
    findMin(b) == (m min n)
  }

  property("insert empty then deleteMin") = forAll{ (h: H) =>
    deleteMin(insert(0,empty)) == empty
  }

  property("heap should sorted") = forAll{ (m: Int, n: Int, w: Int) =>
    def help(h: H, list: List[Int]): List[Int] = {
      if (isEmpty(h)) list
      else {
        help(deleteMin(h), findMin(h) :: list)
      }
    }
    val h = insert(m,insert(n,insert(w, empty)))
    val list = help(h, Nil)
    List(m,n,w).sorted == list.reverse
  }

  property("min of meld two should min of two") = forAll{ (h1: H, h2: H) =>
    val min = (findMin(h1) min findMin(h2))
    findMin(meld(h1,h2)) == min
  }

  property("insert 2 then delete 2 will return with sorted") = forAll { (a: Int, b: Int) =>
    val h = insert(a,insert(b, empty))
    findMin(h) == (a min b)
    findMin(deleteMin(h)) == (a max b)
  }
}
