package player

import quattro.{GameState, Move}

import scala.util.Random

class RandomPCPlayer extends Player {
  private val rand = new Random()
  override def nextMove(gameState: GameState): Move = {
    val moves = gameState.availableMoves.toSeq
    moves(rand.nextInt(moves.size))
  }
}
