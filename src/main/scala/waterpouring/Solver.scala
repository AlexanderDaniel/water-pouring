package week7.waterpouringByAlexi

object Solver {

  type State = Vector[Int]
  type Generation = Set[(State, List[Move])]

  trait Move
  case class Fill(glass: Int) extends Move
  case class Empty(glass: Int) extends Move
  case class Pour(src: Int, dst: Int) extends Move

  class Solve(capacity: Vector[Int], desiredFill: Int) {
    def solve: Set[List[Move]] =
      doSolve(Set((initialState, List())), Set())

    def doSolve(gen: Generation, explored: Set[State]): Set[List[Move]] =
      if (isSolved(gen, desiredFill)) solutions(gen, desiredFill)
      else {
        val nextExplored = explored ++ (gen map { case (state, _) => state})
        val nextGen = for {
          (state, movesSoFar) <- gen
          nextMove <- moves(state)
          newState = doMove(state, nextMove)
          if !nextExplored.contains(newState)
        } yield (newState, nextMove :: movesSoFar)
        doSolve(nextGen, nextExplored)
      }

    def moves(state: Vector[Int]): Set[Move] = {
      val fills = (for (glass <- 0 until state.length) yield Fill(glass)).toSet
      val empties = (for (glass <- 0 until state.length) yield Empty(glass)).toSet
      val pours = (
        for (src <- 0 until state.length; dst <- 0 until state.length; if src!=dst) yield Pour(src, dst)
      ).toSet
      fills ++ empties ++ pours
    }

    def doMove(state: State, move: Move): State = move match {
      case Fill(index) => state updated(index, capacity(index))
      case Empty(index) => state updated(index, 0)
      case Pour(src, dst) => {
        val amount = state(src) min (capacity(dst)-state(dst))
        state updated(src, state(src)-amount) updated(dst, state(dst)+amount)
      }
    }

    def initialState: State =
      capacity map (c => 0)

    def isSolved(gen: Generation, desiredFill: Int): Boolean =
      !solutions(gen, desiredFill).isEmpty

    def solutions(gen: Generation, desiredFill: Int): Set[List[Move]] =
      gen filter { case (state, _) => isSolved(state, desiredFill)} map { case (_, moves) => moves.reverse}

    def isSolved(state: State, desiredFill: Int): Boolean =
      state exists (_==desiredFill)
  }
}


