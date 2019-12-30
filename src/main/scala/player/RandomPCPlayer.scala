package player

import quattro.{QuattroGameState, QuattroMove}

import scala.util.Random

class RandomPCPlayer extends Player {
  private val rand = new Random()
  override def nextMove(gameState: QuattroGameState): QuattroMove = {
    val moves = gameState.availableMoves
    moves(rand.nextInt(moves.size))
  }
}
