package week7

/**
  * Created by weili on 16-7-12.
  */
class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialStates = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State): State = {
      state updated (glass, 0)
    }
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State): State = {
      state updated (glass, capacity(glass))
    }
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Path
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move): Path = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + "-->" + endState
  }

  val initialPath = new Path(Nil, initialStates)

  def from(pathSet: Set[Path]): Stream[Set[Path]] = {
    if (pathSet.isEmpty) Stream.empty
    else {
      val more =
        for {
          path <- pathSet
          move <- moves
        } yield path extend move
      pathSet #:: from(more)
    }
  }

  val pathSets = from(Set(initialPath))

  def solution(target: Int) = {
    for {
      pathSet <- pathSets
      path <- pathSet if ((path endState) contains target)
    } yield path
  }
}
