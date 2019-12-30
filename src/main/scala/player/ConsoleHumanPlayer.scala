package player

import quattro.{GameState, Move}

class ConsoleHumanPlayer extends Player {

  override def nextMove(gameState: GameState): Move = {
    val figures = gameState.availableFiguresForCurrentPlayer.toSeq
    println("Choose figure:")
    figures.zipWithIndex.map {
      case (f,i) => s"$i. $f"
    }.foreach(println)
    Move(figures.head, 0, 0)
  }

  def readIntWithMax(max: Int): Int = {
    val read = scala.io.StdIn.readInt()
    if (read < 0 || read > max) {
      println(s"Enter a number between 0 and $max")
      readIntWithMax(max)
    } else read
  }
}
