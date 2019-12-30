import player.{ConsoleHumanPlayer, Player, RandomPCPlayer}
import quattro.{Black, Color, GameState, White}

import scala.annotation.tailrec

object Quattro {

  def play(white: Player, black: Player): Option[Color] = {
    def playerFor(color: Color): Player = color match {
      case White => white
      case Black => black
    }

    @tailrec
    def playNext(state: GameState): Option[Color] = {
      println(state)
      state.winner match {
        case s@Some(_) => s
        case _ =>
          playNext(state.put(playerFor(state.nextTurnColor).nextMove(state)))
      }
    }
    playNext(GameState())
  }

  def main(args: Array[String]): Unit = {
    println("Winner: " + play(new ConsoleHumanPlayer(), new RandomPCPlayer()))
  }
}
