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
    val NRes(value, moves) = negamax0(state, maxDepth + 1, -RANGE, RANGE, 1, hVal)
//    println(s"The computer has chosen a move with score $value and thinks this is going to happen: $moves")
    moves.head
  }

  private case class NRes[MOVE](value: Int, moves: List[MOVE]) {
    def max(other: NRes[MOVE]): NRes[MOVE] = {
      Seq(this, other).maxBy(x => (x.value, -x.moves.length))
    }

    def unary_-(): NRes[MOVE] = NRes(-value, moves)

    def withMove(move: MOVE): NRes[MOVE] = NRes(value, move :: moves)
  }

  private def negamax0[MOVE](state: GameState[MOVE], depth: Int, alpha: Int, beta: Int, myFactor: Int, hVal: GameState[MOVE] => Int): NRes[MOVE] = {
    if(state.isTerminal || depth == 0)
      return NRes(myFactor * hVal(state), Nil)

    var bestResult = NRes[MOVE](-RANGE, Nil)

    val moves = state.availableMoves
    var i = 0
    var break = false
    var nextAlpha = alpha
    while(i < moves.size && !break) {
      val move = moves(i)
      val newState = state.play(move)
      val bestChildResult = -negamax0(newState, depth - 1, -beta, -nextAlpha, -myFactor, hVal).withMove(move)
      bestResult = bestResult.max(bestChildResult)
      nextAlpha = math.max(nextAlpha, bestResult.value)
      if (nextAlpha >= beta) {
        break = true
      }
      i += 1
    }
    bestResult
  }
}
