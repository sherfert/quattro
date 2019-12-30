package algorithms

import algorithms.TranspositionTable.{EXACT, LOWER_BOUND, TTEntry, UPPER_BOUND}

object Negamax {

  val RANGE = 1000000

  /**
   * Compute the best move using the Negamax algorithm with alpha-beta pruning.
   *
   * @param state                  the current state
   * @param maxDepth               the depth, or look-ahead of moves, on which to base the decision.
   *                               If 0, the algorithm will only take the immediate next moves into considerations.
   *                               If 1, it will also take the possible reponse moves to the first moves into consideration, and so on.
   * @param hVal                   a heuristic function that, given a possible descendant state of the given state, returns a value that assesses the state.
   *                               The values must be between -[[RANGE]] and [[RANGE]] where higher values are better for the current player that invokes this
   *                               function and lower values are better for the opponent.
   * @param transpositionTableSize If greater than zero, a transposition table will be used.
   *                               The number is the maximum allowed size for the table.
   * @tparam MOVE the type of move
   * @return the best next move.
   */
  def nextMove[MOVE](state: GameState[MOVE],
                     maxDepth: Int,
                     hVal: GameState[MOVE] => Int,
                     transpositionTableSize: Int = 15000): MOVE = {

    val transpositionTable = if (transpositionTableSize > 0) Some(new TranspositionTable[GameState[MOVE], NRes[MOVE]](transpositionTableSize)) else None
    val NRes(_, moves) = negamax(state, maxDepth + 1, -RANGE, RANGE, 1, hVal, transpositionTable)
    moves.head
  }

  private case class NRes[MOVE](value: Int, moves: List[MOVE]) {
    def max(other: NRes[MOVE]): NRes[MOVE] = {
      Seq(this, other).maxBy(x => (x.value, -x.moves.length))
    }

    def unary_-(): NRes[MOVE] = NRes(-value, moves)

    def withMove(move: MOVE): NRes[MOVE] = NRes(value, move :: moves)
  }

  private def negamax[MOVE](state: GameState[MOVE],
                            depth: Int,
                            alpha: Int,
                            beta: Int,
                            myFactor: Int,
                            hVal: GameState[MOVE] => Int,
                            transpositionTable: Option[TranspositionTable[GameState[MOVE], NRes[MOVE]]]): NRes[MOVE] = {
    var nextAlpha = alpha
    var nextBeta = beta

    // Transposition Table lookup
    transpositionTable.foreach(_.lookup(state) match {
      case Some(TTEntry(value, flag, ttDepth)) if ttDepth >= depth =>
        flag match {
          case TranspositionTable.EXACT => return value
          case TranspositionTable.LOWER_BOUND => nextAlpha = math.max(alpha, value.value)
          case TranspositionTable.UPPER_BOUND => nextBeta = math.min(beta, value.value)
        }
        if (nextAlpha >= nextBeta) {
          return value
        }
      case _ =>
    })

    if (state.isTerminal || depth == 0) {
      val result = NRes[MOVE](myFactor * hVal(state), Nil)

      // Transposition Table store
      transpositionTable.foreach(_.store(state, TTEntry(result, EXACT, depth)))

      return result
    }

    var bestResult = NRes[MOVE](-RANGE, Nil)

    val moves = state.availableMoves
    var i = 0
    var break = false
    while (i < moves.size && !break) {
      val move = moves(i)
      val newState = state.play(move)
      val bestChildResult = -negamax(newState, depth - 1, -nextBeta, -nextAlpha, -myFactor, hVal, transpositionTable).withMove(move)
      bestResult = bestResult.max(bestChildResult)
      nextAlpha = math.max(nextAlpha, bestResult.value)
      if (nextAlpha >= beta) {
        break = true
      }
      i += 1
    }

    // Transposition Table store
    transpositionTable.foreach { table =>
      val flag = if (bestResult.value <= alpha) { // Yes, the original alpha
        UPPER_BOUND
      } else if (bestResult.value >= nextBeta) {
        LOWER_BOUND
      } else {
        EXACT
      }
      table.store(state, TTEntry(bestResult, flag, depth))
    }

    bestResult
  }
}
