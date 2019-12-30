package algorithms

trait GameState[MOVE] {

  /**
   * @param move the move
   * @return the game state that results from playing the given move.
   */
  def play(move: MOVE): GameState[MOVE]

  /**
   * @return all possible moves from this game state
   */
  def availableMoves: Seq[MOVE]

  /**
   * @return whether this is a terminal game state.
   */
  def isTerminal: Boolean
}

