package algorithms

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ArrayBuffer

//noinspection ZeroIndexToHead
class NegamaxTest extends FunSuite with Matchers {

  case class TreeMove(branch: Int)

  case class Node(hVal: Int, branches: Node*) extends GameState[TreeMove] {
    override def play(move: TreeMove): GameState[TreeMove] = branches(move.branch)

    override def availableMoves: Seq[TreeMove] = branches.indices.map(TreeMove)

    override def isTerminal: Boolean = availableMoves.isEmpty

    override def equals(obj: Any): Boolean = obj match {
      case a: AnyRef => this eq a
      case _ => false
    }

    override def hashCode(): Int = System.identityHashCode(this)
  }

  private def hVal(state: GameState[TreeMove]): Int = state match {
    case node: Node => node.hVal
  }

  private val treeDepth2 =
    Node(0,
      Node(5,
        Node(6),
        Node(10)
      ),
      Node(10,
        Node(4),
        Node(10)
      )
    )

  test("should pick the move with the best heuristic at depth 0") {
    Negamax.nextMove(treeDepth2, 0, hVal) should be(TreeMove(1))
  }

  test("should pick left because 6 > 4") {
    Negamax.nextMove(treeDepth2, 1, hVal) should be(TreeMove(0))
  }

  test("should pick right because 0 > -1000") {
    val state =
      Node(0,
        Node(5,
          Node(0),
          Node(-1000)
        ),
        Node(10,
          Node(0),
          Node(0)
        )
      )

    Negamax.nextMove(state, 1, hVal) should be(TreeMove(1))
  }

  private val treeDepth3 =
    Node(0,
      Node(10,
        Node(-1000,
          Node(100),
          Node(-500)
        ),
        Node(1000,
          Node(100),
          Node(400)
        )
      ),
      Node(0,
        Node(0,
          Node(-10),
          Node(1000)
        ),
        Node(5000,
          Node(10),
          Node(12)
        )
      )
    )

  test("tree-depth-3 should pick left because 10 > 0 at depth 0") {
    Negamax.nextMove(treeDepth3, 0, hVal) should be(TreeMove(0))
  }

  test("tree-depth-3 should pick right because 0 > -1000 at depth 1") {
    Negamax.nextMove(treeDepth3, 1, hVal) should be(TreeMove(1))
  }

  test("tree-depth-3 should pick left because 100 > 12 at depth 2") {
    Negamax.nextMove(treeDepth3, 2, hVal) should be(TreeMove(0))
  }

  test("should prefer to win sooner rather than later") {
    val state =
      Node(0,
        Node(5,
          Node(1000),
          Node(1000)
        ),
        Node(1000)
      )

    Negamax.nextMove(state, 1, hVal) should be(TreeMove(1))
  }

  private class RememberingHeuristic {
    private val _visitedNodes: ArrayBuffer[Node] = new ArrayBuffer()

    def hVal(state: GameState[TreeMove]): Int = state match {
      case node: Node =>
        _visitedNodes += node
        node.hVal
    }

    def visitedNodes: Seq[Node] = _visitedNodes.toSeq
  }

  test("alpha/beta pruning works in shallow trees") {
    val state =
      Node(6,
        Node(6,
          Node(7),
          Node(6)
        ),
        Node(4,
          Node(8),
          Node(4),
          Node(5) // should not be traversed
        ),
        Node(3,
          Node(3,
            Node(3)))
      )

    val hr = new RememberingHeuristic
    Negamax.nextMove(state, 10, hr.hVal) should be(TreeMove(0))
    hr.visitedNodes should not contain state.branches(1).branches(2)
  }

  test("alpha/beta pruning works in deep trees") {
    val state =
      Node(6,
        Node(3,
          Node(6,
            Node(6,
              Node(7),
              Node(6)
            ),
            Node(4,
              Node(8),
              Node(4),
              Node(5) // should not be traversed
            )
          ),
          Node(3,
            Node(3,
              Node(3)))
        ),
        Node(6,
          Node(6,
            Node(6)),
          Node(9,
            Node(0),
            Node(9),
            Node(12), // should not be traversed
            Node(-1))) // should not be traversed
      )

    val hr = new RememberingHeuristic
    Negamax.nextMove(state, 10, hr.hVal) should be(TreeMove(1))
    hr.visitedNodes should not contain state.branches(0).branches(0).branches(1).branches(2)
    hr.visitedNodes should not contain state.branches(1).branches(1).branches(2)
    hr.visitedNodes should not contain state.branches(1).branches(1).branches(3)
  }
}
