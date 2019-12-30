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
    require(move.figure.color == nextTurnColor)
    require(!board.exists(_.exists(_.contains(move.figure))))
    require(board(move.x)(move.y).isEmpty)

    QuattroGameState(nextTurnColor.other,
      board.updated(move.x, board(move.x).updated(move.y, Some(move.figure))))
  }

  override lazy val availableMoves: Set[QuattroMove] = {
    if (winner.isDefined) Set.empty
    else for {
      f <- availableFiguresForCurrentPlayer
      x <- 0 until 4
      y <- 0 until 4 if board(x)(y).isEmpty
    } yield QuattroMove(f, x, y)
  }

  override lazy val isTerminal: Boolean = winner.isDefined || availableMoves.isEmpty

  lazy val availableFiguresForCurrentPlayer: Set[Figure] = {
    val fs = figuresOnBoard
    Figure.all.filter { f => f.color == nextTurnColor && !fs.contains(f)}
  }

  private lazy val figuresOnBoard: Set[Figure] = {
    board.flatten.flatten.toSet
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