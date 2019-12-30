package player

import quattro.{QuattroGameState, QuattroMove}

trait Player {
  def nextMove(gameState: QuattroGameState): QuattroMove
}
