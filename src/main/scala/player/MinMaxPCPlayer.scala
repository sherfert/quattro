package player

import quattro.{Color, GameState, Move}

class MinMaxPCPlayer(color: Color, maxDepth: Int = 3) extends Player {

  override def nextMove(state: GameState): Move = {
    val moveScores = {
      for {
        move <- state.availableMoves
      } yield move -> negamax(state.put(move))
    }
    moveScores.map {
      case (Move(figure, x, y), score) => s"   Move $figure to $x/$y has score $score"
    }.foreach(println(_))
    val bestMove = moveScores.maxBy(_._2)._1
    bestMove
  }

  private def hVal(state: GameState): Int = {
    val res = state.winner match {
      case Some(`color`) => 1000
      case Some(_) => -1000
      case None => 0 // ???
    }
    if (res != 0) {
      val indented = state.toString.split("\n").map("   " + _).mkString("\n")
      println(s"   hVal is $res for $indented")
    }
    res
  }

  private def negamax(state: GameState): Int = {
    negamax0(state, maxDepth, -1000000, 1000000, 1)
  }

  private def negamax0(state: GameState, depth: Int, alpha: Int, beta: Int, myFactor: Int): Int = {
    if(state.isTerminal || depth == 0)
      return myFactor * hVal(state)

    var bestResult = Integer.MIN_VALUE

    val moves = state.availableMoves.toSeq
    var i = 0
    var break = false
    while(i < moves.size && !break) {
      val move = moves(i)
      val newState = state.put(move)
      val bestChildResult = -negamax0(newState, depth - 1, -beta, -alpha, -myFactor)
      println(s"      Negamax $move $bestChildResult")
      bestResult = math.max(bestResult, bestChildResult)
      val nextAlpha = math.max(alpha, bestResult)
      if (nextAlpha >= beta) {
        break = true
      }
      i += 1
    }
    bestResult
  }
}
