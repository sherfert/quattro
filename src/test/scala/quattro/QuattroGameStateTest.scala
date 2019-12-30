package quattro

import org.scalatest.{FunSuite, Matchers}

class QuattroGameStateTest extends FunSuite with Matchers {

  // Play

  test("White starts") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0)) // Is OK
  }

  test("Black cannot start") {
    val before = QuattroGameState()
      an[IllegalArgumentException] should be thrownBy {
        before.play(QuattroMove(Figure(Black, Small, Round, WithHole), 0, 0))
      }
  }

  test("White cannot play twice in a row") {
    val before = QuattroGameState().play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      an[IllegalArgumentException] should be thrownBy {
        before.play(QuattroMove(Figure(White, Small, Round, NoHole), 0, 1))
      }
  }

  test("Cannot use the same figure twice") {
    val before = QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Small, Round, WithHole), 0, 1))
      an[IllegalArgumentException] should be thrownBy {
        before.play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 2))
      }
  }

  test("Cannot place a figure in a taken spot") {
    val before = QuattroGameState().play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
    an[IllegalArgumentException] should be thrownBy {
      before.play(QuattroMove(Figure(Black, Small, Round, NoHole), 0, 0))
    }
  }

  test("Cannot do a move after a winner exists") {
    val before = QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, WithHole), 0, 1))
      .play(QuattroMove(Figure(White, Small, Rectangle, WithHole), 0, 2))
      .play(QuattroMove(Figure(Black, Large, Rectangle, WithHole), 0, 3))
    an[IllegalArgumentException] should be thrownBy {
      before.play(QuattroMove(Figure(White, Large, Round, NoHole), 3, 4))
    }
  }

  // Winner

  test("no winner for 3 in a row") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, WithHole), 0, 1))
      .play(QuattroMove(Figure(White, Small, Rectangle, WithHole), 0, 2))
      .winner should be(empty)
  }

  test("Black wins vertical with all holes") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, WithHole), 0, 1))
      .play(QuattroMove(Figure(White, Small, Rectangle, WithHole), 0, 2))
      .play(QuattroMove(Figure(Black, Large, Rectangle, WithHole), 0, 3))
      .winner should be(Some(Black))
  }

  test("White wins vertical with all holes") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, WithHole), 0, 1))
      .play(QuattroMove(Figure(White, Small, Rectangle, WithHole), 0, 2))
      .play(QuattroMove(Figure(Black, Large, Rectangle, WithHole), 1, 3))
      .play(QuattroMove(Figure(White, Large, Rectangle, WithHole), 0, 3))
      .winner should be(Some(White))
  }

  test("Black wins vertical with two characteristics") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Large, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, WithHole), 0, 1))
      .play(QuattroMove(Figure(White, Large, Rectangle, WithHole), 0, 2))
      .play(QuattroMove(Figure(Black, Large, Rectangle, WithHole), 0, 3))
      .winner should be(Some(Black))
  }

  test("Black wins horizontal with all large") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Large, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, NoHole), 1, 0))
      .play(QuattroMove(Figure(White, Large, Rectangle, WithHole), 2, 0))
      .play(QuattroMove(Figure(Black, Large, Rectangle, NoHole), 3, 0))
      .winner should be(Some(Black))
  }

  test("Black wins diagonal 1 with all round") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Large, Round, WithHole), 0, 0))
      .play(QuattroMove(Figure(Black, Large, Round, NoHole), 1, 1))
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 2, 2))
      .play(QuattroMove(Figure(Black, Small, Round, NoHole), 3, 3))
      .winner should be(Some(Black))
  }

  test("White wins diagonal 2 with all white") {
    QuattroGameState()
      .play(QuattroMove(Figure(White, Large, Round, WithHole), 0, 3))
      .play(QuattroMove(Figure(Black, Small, Round, WithHole), 2, 2))
      .play(QuattroMove(Figure(White, Large, Rectangle, NoHole), 1, 2))
      .play(QuattroMove(Figure(Black, Small, Rectangle, WithHole), 3, 3))
      .play(QuattroMove(Figure(White, Small, Round, WithHole), 2, 1))
      .play(QuattroMove(Figure(Black, Large, Rectangle, WithHole), 0, 0))
      .play(QuattroMove(Figure(White, Small, Round, NoHole), 3, 0))
      .winner should be(Some(White))
  }
}
