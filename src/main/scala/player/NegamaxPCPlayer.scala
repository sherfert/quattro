package player

import algorithms.{GameState, Negamax}
import quattro.{Color, QuattroGameState, QuattroMove}

class NegamaxPCPlayer(color: Color, maxDepth: Int = 3) extends Player {

  override def nextMove(state: QuattroGameState): QuattroMove = {
    Negamax.nextMove[QuattroMove](state, maxDepth, hVal)
  }

  private def hVal(state: GameState[QuattroMove]): Int = {
    state match {
      case qstate: QuattroGameState =>
        qstate.winner match {
          case Some(`color`) => 1000
          case Some(_) => -1000
          case None => 0 // TODO ???
        }
    }
  }
}
