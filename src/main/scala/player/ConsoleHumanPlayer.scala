package player

import quattro.{QuattroGameState, QuattroMove}

import scala.annotation.tailrec
import scala.util.{Success, Try}

class ConsoleHumanPlayer extends Player {

  override def nextMove(gameState: QuattroGameState): QuattroMove = {
    val figures = gameState.availableFiguresForCurrentPlayer.toSeq
    println("Choose figure:")
    figures.zipWithIndex.map {
      case (f, i) => s"${i + 1}. $f"
    }.foreach(println)

    val figure = figures(readIntWithMax(figures.size) - 1)
    println("Choose row:")
    val x = readIntWithMax(4) - 1
    println("Choose column:")
    val y = readIntWithMax(4) - 1

    QuattroMove(figure, x, y)
  }

  @tailrec
  private def readIntWithMax(max: Int): Int = {
    val read = scala.io.StdIn.readLine()
    if(read == null) {
      throw new IllegalStateException()
    }
    Try(read.toInt) match {
      case Success(value) if value > 0 && value <= max =>
        value
      case _ =>
        println(s"Enter a number between 1 and $max")
        readIntWithMax(max)
    }
  }
}
