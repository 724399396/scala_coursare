package kmeans

import java.io.{BufferedReader, File, FileReader}

object HList {
  import HList._

  sealed trait HList {
    type ViewAt[N <: Nat] <: IndexedView
  }

  final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    def ::[T](v: T) = HCons(v, this)

    override def toString = head + " :: " + tail

    type FullType = HCons[H,T]

    def viewAt[Idx <: Nat](implicit in: FullType => FullType#ViewAt[Idx])
    = in(this.asInstanceOf[FullType])

    type ViewAt[N <: Nat] = N#Expand[
      ({ type Z[P <: Nat] = HListViewN[H, T#ViewAt[P]] })#Z,
      HListView0[H,T],
      IndexedView]
  }

  final class HNil extends HList {
    def ::[T](v: T) = HCons(v, this)

    override def toString = "Nil"
  }

  object HList {
    type ::[H, T <: HList] = HCons[H, T]
    val :: = HCons
    val HNil = new HNil
  }



  sealed trait IndexedView {
    type Before <: HList
    type After <: HList
    type At

    def fold[R](f: (Before, At, After) => R): R

    def get = fold((_, value, _) => value)
  }

  class HListView0[H, T <: HList](val list: H :: T) extends IndexedView {
    type Before = HNil
    type After = T
    type At = H

    def fold[R](f: (Before, At, After) => R): R = f(HNil, list.head, list.tail)
  }

  final class HListViewN[H, NextIdxView <: IndexedView](
                                                         h: H, next: NextIdxView) extends IndexedView {
    type Before = H :: NextIdxView#Before
    type At = NextIdxView#At
    type After = NextIdxView#After

    def fold[R](f: (Before, At, After) => R): R =
      next.fold((before, at, after) =>
        f(HCons(h, before), at, after))
  }

  object IndexedView {
    implicit def index0[H, T <: HList](list: H :: T): HListView0[H,T] =
      new HListView0[H,T](list)
    implicit def indexN[H, T <: HList, Prev <: IndexedView](list : (H :: T))(
      implicit indexTail: T => Prev): HListViewN[H,Prev] =
        new HListViewN[H, Prev](list.head, indexTail(list.tail)
    )
  }


  sealed trait Nat {
    type Expand[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
  }

  object Nat {

    sealed trait _0 extends Nat {
      type Expand[NoneZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = IfZero
    }

    sealed trait Succ[Prev <: Nat] extends Nat {
      type Expand[NoneZero[N <: Nat] <: Ret, IfZero <: Ret, Ret] = NoneZero[Prev]
    }

    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type _5 = Succ[_4]
    type _6 = Succ[_5]
    type _7 = Succ[_6]
    type _8 = Succ[_7]
    type _9 = Succ[_8]
    type _10 = Succ[_9]
    type _11 = Succ[_10]
    type _12 = Succ[_11]
    type _13 = Succ[_12]
    type _14 = Succ[_13]
    type _15 = Succ[_14]
    type _16 = Succ[_15]
    type _17 = Succ[_16]
    type _18 = Succ[_17]
    type _19 = Succ[_18]
    type _20 = Succ[_19]
    type _21 = Succ[_20]
    type _22 = Succ[_21]
  }


  class FileLineTraversable(file: File) extends Traversable[String] {
    override def foreach[U](f: (String) => U): Unit = {
      println("Opening file")
      val input = new BufferedReader(new FileReader(file))
      try {
        var line = input.readLine()
        while (line != null) {
          f(line)
          line = input.readLine()
        }
        println("Done iterating file")
      } finally {
        println("Closing file")
        input.close()
      }
    }

    override def toString =
      "{Lines of" + file.getAbsoluteFile + "}"
  }
}
