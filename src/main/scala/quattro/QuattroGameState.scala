package quattro

import algorithms.GameState

case class QuattroMove(figure: Figure, x: Int, y: Int) {
  require(x >= 0)
  require(x < 4)
  require(y >= 0)
  require(y < 4)
}

object QuattroGameState {
  def apply(): QuattroGameState = QuattroGameState(White, Seq.fill(4)(Seq.fill(4)(None)))
}

case class QuattroGameState private(nextTurnColor: Color, board: Seq[Seq[Option[Figure]]]) extends GameState[QuattroMove] {

  override def play(move: QuattroMove): QuattroGameState = {
    require(availableMoves.contains(move))

    QuattroGameState(nextTurnColor.other,
      board.updated(move.x, board(move.x).updated(move.y, Some(move.figure))))
  }

  override lazy val availableMoves: Seq[QuattroMove] = {
    if (winner.isDefined) Seq.empty
    else for {
      f <- availableFiguresForCurrentPlayer
      x <- 0 until 4
      y <- 0 until 4 if board(x)(y).isEmpty
    } yield QuattroMove(f, x, y)
  }

  override lazy val isTerminal: Boolean = winner.isDefined || availableMoves.isEmpty

  lazy val availableFiguresForCurrentPlayer: Seq[Figure] = availableFiguresInColor(nextTurnColor)

  def availableFiguresForOpponent: Seq[Figure] = availableFiguresInColor(nextTurnColor.other)

  private def availableFiguresInColor(color: Color): Seq[Figure] = {
    val fs = figuresOnBoard
    Figure.all.filter { f => f.color == color && !fs.contains(f)}
  }

  private lazy val figuresOnBoard: Seq[Figure] = {
    board.flatten.flatten
  }

  lazy val winner: Option[Color] = {
    if(Figure.characteristics.exists { c=>
      lines.exists { l =>
        l.forall(_.isDefined) && {
          val values = l.map(mf => c(mf.get))
          values.tail.forall(_ == values.head)
        }
      }
    }) Some(nextTurnColor.other) else None
  }

  private lazy val lines: Seq[Seq[Option[Figure]]] = {
    val horizontal = board
    val vertical = (0 until 4).map(i => board.map(_(i)))
    val diagonal = Seq((0 until 4).map(i => board(i)(i)),
      (0 until 4).map(i => board(i)(3 - i)))
    horizontal ++ vertical ++ diagonal
  }

  override def toString: String = {
    val boardStr = board.map(_.map {
      case Some(figure) => figure.toString
      case None => "       "
    }.mkString(" <|> ")).mkString(System.lineSeparator)
    s"""
       |-------------------------------------------
       |$boardStr
       |-------------------------------------------
    """.stripMargin
  }
}
