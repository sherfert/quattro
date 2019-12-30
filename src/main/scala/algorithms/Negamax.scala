package algorithms

object Negamax {

  val RANGE = 1000000

  /**
   * Compute the best move using the Negamax algorithm with alpha-beta pruning.
   *
   * @param state    the current state
   * @param maxDepth the depth, or look-ahead of moves, on which to base the decision.
   *                 If 0, the algorithm will only take the immediate next moves into considerations.
   *                 If 1, it will also take the possible reponse moves to the first moves into consideration, and so on.
   * @param hVal     a heuristic function that, given a possible descendant state of the given state, returns a value that assesses the state.
   *                 The values must be between -[[RANGE]] and [[RANGE]] where higher values are better for the current player that invokes this
   *                 function and lower values are better for the opponent.
   * @tparam MOVE the type of move
   * @return the best next move.
   */
  def nextMove[MOVE](state: GameState[MOVE], maxDepth: Int, hVal: GameState[MOVE] => Int): MOVE = {
    val moveScores = {
      for {
        move <- state.availableMoves
      } yield move -> negamax(state.play(move), maxDepth, hVal)
    }
    val bestMove = moveScores.minBy(_._2)._1
    bestMove
  }

  private def negamax[MOVE](state: GameState[MOVE], maxDepth: Int, hVal: GameState[MOVE] => Int): Int = {
    negamax0(state, maxDepth, -RANGE, RANGE, -1, hVal)
  }

  private def negamax0[MOVE](state: GameState[MOVE], depth: Int, alpha: Int, beta: Int, myFactor: Int, hVal: GameState[MOVE] => Int): Int = {
    if(state.isTerminal || depth == 0)
      return myFactor * hVal(state)

    var bestResult = -RANGE

    val moves = state.availableMoves.toSeq
    var i = 0
    var break = false
    while(i < moves.size && !break) {
      val move = moves(i)
      val newState = state.play(move)
      val bestChildResult = -negamax0(newState, depth - 1, -beta, -alpha, -myFactor, hVal)
      bestResult = math.max(bestResult, bestChildResult)
      val nextAlpha = math.max(alpha, bestResult)
      if (nextAlpha >= beta) {
        break = true
      }
      i += 1
    }
    bestResult
  }
}
