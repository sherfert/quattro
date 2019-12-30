package player

import quattro.{GameState, Move}

trait Player {
  def nextMove(gameState: GameState): Move
}
