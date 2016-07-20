package quickcheck

trait MyBinomialHeap extends Heap {
  type Rank = Int
  override type H = List[Node]
  case class Node(x: A, r: Rank, c: H)


  protected def root(n: Node) = n.x
  protected def rank(n: Node) = n.r
  protected def link(t: Node, r: Node): Node =
    if(rank(t) == rank(r)) {
      if (ord.lteq(root(t), root(r))) Node(t.x, t.r+1, r :: t.c)
      else Node(r.x, r.r + 1, t :: r.c)
    } else throw new IllegalStateException("link different rank node")

  protected def ins(n: Node, h: H): H = h match {
    case Nil => List(n)
    case t :: ts =>
      if (n.r < t.r)
        n :: t :: ts
      else if (n.r == t.r)
        ins(link(t,n), ts)
      else
        t :: ins(n, ts)
  }


  override def empty: H = Nil
  override def isEmpty(h: H): Boolean = h.isEmpty

  override def insert(x: A, h: H): H = ins(Node(x,0,Nil), h)
  override def meld(h1: H, h2: H): H = (h1,h2) match {
    case (t,Nil) => t
    case (Nil,t) => t
    case (t1::ts1,t2::ts2) =>
      if (t1.r < t2.r)
        t1 :: meld(ts1,t2::ts2)
      else if (t1.r > t2.r)
        t2 :: meld(t1::ts1,ts2)
      else
        ins(link(t1,t2), meld(ts1,ts2))
  }

  def findMin(h: H): A = h match {
    case Nil => throw new NoSuchElementException
    case x :: Nil =>
      root(x)
    case t :: ts =>
      val x = findMin(ts)
      if (ord.lteq(root(t),x)) root(t) else x
  }

  def deleteMin(h: H): H = h match {
    case Nil => throw new NoSuchElementException
    case t :: ts =>
      def getMin(t: Node, ts: H): (Node, H) = {
        ts match {
          case Nil => (t, Nil)
          case tp :: tsp =>
            val (tq,tsq) = getMin(tp,tsp)
            if (ord.lteq(root(t),root(tq))) (t,ts) else (tq,t::tsq)
        }
      }
      val (Node(_,_,c), left) = getMin(t,ts)
      meld(c.reverse, left)
  }
}
