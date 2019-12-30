package algorithms

import org.scalatest.{FunSuite, Matchers}

class NegamaxTest extends FunSuite with Matchers {

  sealed trait BinaryTreeMove

  case object Left extends BinaryTreeMove

  case object Right extends BinaryTreeMove

  sealed trait BinaryTreeState extends GameState[BinaryTreeMove] {
    def hVal: Int
  }

  case class LeafState(hVal: Int) extends BinaryTreeState {
    override def play(move: BinaryTreeMove): GameState[BinaryTreeMove] = throw new IllegalStateException()

    override def availableMoves: Set[BinaryTreeMove] = Set.empty

    override def isTerminal: Boolean = true
  }

  case class InnerState(hVal: Int, left: BinaryTreeState, right: BinaryTreeState) extends BinaryTreeState {
    override def play(move: BinaryTreeMove): GameState[BinaryTreeMove] = move match {
      case Left => left
      case Right => right
    }

    override def availableMoves: Set[BinaryTreeMove] = Set(Left, Right)

    override def isTerminal: Boolean = false
  }

  private def hVal(state: GameState[BinaryTreeMove]): Int = state match {
    case bState: BinaryTreeState => bState.hVal
  }

  private val treeDepth2 =
    InnerState(0,
      InnerState(5,
        LeafState(6),
        LeafState(10)
      ),
      InnerState(10,
        LeafState(4),
        LeafState(10)
      )
    )

  test("should pick the move with the best heuristic at depth 0") {
   Negamax.nextMove(treeDepth2, 0, hVal) should be(Right)
  }

  test("should pick left because 6 > 4") {
    Negamax.nextMove(treeDepth2, 1, hVal) should be(Left)
  }

  test("should pick right because 0 > -1000") {
    val state = InnerState(0,
      InnerState(5,
        LeafState(0),
        LeafState(-1000)
      ),
      InnerState(10,
        LeafState(0),
        LeafState(0)
      )
    )

    Negamax.nextMove(state, 1, hVal) should be(Right)
  }

  private val treeDepth3 =
    InnerState(0,
      InnerState(10,
        InnerState(-1000,
          LeafState(100),
          LeafState(-500)
        ),
        InnerState(1000,
          LeafState(100),
          LeafState(400)
        )
      ),
      InnerState(0,
        InnerState(0,
          LeafState(-10),
          LeafState(1000)
        ),
        InnerState(5000,
          LeafState(10),
          LeafState(12)
        )
      )
    )

  test("tree-depth-3 should pick left because 10 > 0 at depth 0") {
    Negamax.nextMove(treeDepth3, 0, hVal) should be(Left)
  }

  test("tree-depth-3 should pick right because 0 > -1000 at depth 1") {
    Negamax.nextMove(treeDepth3, 1, hVal) should be(Right)
  }

  test("tree-depth-3 should pick left because 100 > 12 at depth 2") {
    Negamax.nextMove(treeDepth3, 2, hVal) should be(Left)
  }
}
