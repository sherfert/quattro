package algorithms

object Negamax {

  def nextMove[MOVE](state: GameState[MOVE], maxDepth: Int, hVal: GameState[MOVE] => Int): MOVE = {
    val moveScores = {
      for {
        move <- state.availableMoves
      } yield move -> negamax(state.play(move), maxDepth, hVal)
    }
    val bestMove = moveScores.maxBy(_._2)._1
    bestMove
  }

  private def negamax[MOVE](state: GameState[MOVE], maxDepth: Int, hVal: GameState[MOVE] => Int): Int = {
    negamax0(state, maxDepth, -1000000, 1000000, 1, hVal)
  }

  private def negamax0[MOVE](state: GameState[MOVE], depth: Int, alpha: Int, beta: Int, myFactor: Int, hVal: GameState[MOVE] => Int): Int = {
    if(state.isTerminal || depth == 0)
      return myFactor * hVal(state)

    var bestResult = Integer.MIN_VALUE

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
