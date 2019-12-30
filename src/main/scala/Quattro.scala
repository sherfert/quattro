import player.{ConsoleHumanPlayer, NegamaxPCPlayer, Player}
import quattro.{Black, Color, QuattroGameState, White}

import scala.annotation.tailrec

object Quattro {

  def play(white: Player, black: Player): Option[Color] = {
    def playerFor(color: Color): Player = color match {
      case White => white
      case Black => black
    }

    @tailrec
    def playNext(state: QuattroGameState): Option[Color] = {
      println(state)
      state.winner match {
        case s@Some(_) => s
        case _ =>
          playNext(state.play(playerFor(state.nextTurnColor).nextMove(state)))
      }
    }
    playNext(QuattroGameState())
  }

  def main(args: Array[String]): Unit = {
    println("Winner: " + play(new ConsoleHumanPlayer(), new NegamaxPCPlayer(Black, maxDepth = 3)))
  }
}
