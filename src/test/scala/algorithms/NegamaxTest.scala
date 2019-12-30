package algorithms

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//noinspection ZeroIndexToHead
class NegamaxTest extends FunSuite with Matchers {

  case class TreeMove(branch: Int)
  case class Node(hVal: Int, branches: Node*) extends GameState[TreeMove] {
    override def play(move: TreeMove): GameState[TreeMove] = branches(move.branch)

    override def availableMoves: Seq[TreeMove] = branches.indices.map(TreeMove)

    override def isTerminal: Boolean = availableMoves.isEmpty

    // Equal nodes at different positions in the tree should not be equal
    override def equals(obj: Any): Boolean = obj match {
      case a: AnyRef => this eq a
      case _ => false
    }

    override def hashCode(): Int = System.identityHashCode(this)
  }

  private def nodeHVal(state: GameState[TreeMove]): Int = state match {
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
    Negamax.nextMove(treeDepth2, 0, nodeHVal) should be(TreeMove(1))
  }

  test("should pick left because 6 > 4") {
    Negamax.nextMove(treeDepth2, 1, nodeHVal) should be(TreeMove(0))
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

    Negamax.nextMove(state, 1, nodeHVal) should be(TreeMove(1))
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
    Negamax.nextMove(treeDepth3, 0, nodeHVal) should be(TreeMove(0))
  }

  test("tree-depth-3 should pick right because 0 > -1000 at depth 1") {
    Negamax.nextMove(treeDepth3, 1, nodeHVal) should be(TreeMove(1))
  }

  test("tree-depth-3 should pick left because 100 > 12 at depth 2") {
    Negamax.nextMove(treeDepth3, 2, nodeHVal) should be(TreeMove(0))
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

    Negamax.nextMove(state, 1, nodeHVal) should be(TreeMove(1))
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

  // Transposition Table tests

  sealed trait GridMove

  case object UP extends GridMove

  case object DOWN extends GridMove

  case object LEFT extends GridMove

  case object RIGHT extends GridMove

  // A game state where you can move in a grid of integers
  case class GridState(grid: Seq[Seq[Int]], x: Int = 0, y: Int = 0) extends GameState[GridMove] {
    override def play(move: GridMove): GameState[GridMove] = (GridState.apply _).tupled(move match {
      case UP => (grid, x - 1, y)
      case DOWN => (grid, x + 1, y)
      case LEFT => (grid, x, y - 1)
      case RIGHT => (grid, x, y + 1)
    })

    override def availableMoves: Seq[GridMove] = {
      var dirs: List[GridMove] = Nil
      if (x > 0) dirs = UP :: dirs
      if (x < grid.size - 1) dirs = DOWN :: dirs
      if (y > 0) dirs = LEFT :: dirs
      if (y < grid.size - 1) dirs = RIGHT :: dirs
      dirs
    }

    override def isTerminal: Boolean = false
  }

  private class CountingHeuristic {
    private val _counts = mutable.Map[GridState, Int]().withDefaultValue(0)

    def hVal(state: GameState[GridMove]): Int = state match {
      case state: GridState =>
        _counts(state) += 1
        state.grid(state.x)(state.y)
    }

    def counts: Map[GridState, Int] = _counts.toMap
  }

  test("should not recompute state at same depth") {
    val state = GridState(Seq(
      Seq(1, 3),
      Seq(4, 2)
    ))
    val hr = new CountingHeuristic
    Negamax.nextMove(state, 1, hr.hVal) // undefined which one is better
    hr.counts should be(Map(
      state -> 1,
      state.play(DOWN).play(RIGHT) -> 1
    ))
  }

  test("should not recompute state at higher depth") {
    val state = GridState(Seq(
      Seq(1, 3),
      Seq(4, 2)
    ))
    val hr = new CountingHeuristic
    Negamax.nextMove(state, 2, hr.hVal) // undefined which one is better
    hr.counts should be(Map(
      state.play(DOWN) -> 1,
      state.play(RIGHT) -> 1
    ))
  }

  test("should prune with transposition table") {
    val state = GridState(Seq(
      Seq(6, 6, 7),
      Seq(4, 4, 0),
      Seq(8, 0, 0) // 8 will not be traversed because of pruning
    ))
    val hr = new CountingHeuristic
    Negamax.nextMove(state, 1, hr.hVal) // undefined which one is better
    hr.counts should be(Map(
      state -> 1,
      state.play(RIGHT).play(DOWN) -> 1,
      state.play(RIGHT).play(RIGHT) -> 1
    ))
  }

  test("should prune with transposition table and more depth") {
    val state = GridState(Seq(
      Seq(6, 6, 7),
      Seq(4, 4, 10),
      Seq(8, 1, 2)
    ))
    val hr = new CountingHeuristic
    Negamax.nextMove(state, 3, hr.hVal) // undefined which one is better
    hr.counts should be(Map(
      state -> 1,
      // the heuristic gets computed once at maximum depth (4 steps played),
      // but then the entry in the transposition table gets overridden with a bound entry at the depth of 2 steps played.
      // This entry then cannot be read when next traversing this state at maximum depth, thus hVal is computed twice.
      state.play(RIGHT).play(DOWN) -> 2,
      state.play(RIGHT).play(RIGHT) -> 1,
      state.play(DOWN).play(DOWN) -> 1,
      state.play(RIGHT).play(RIGHT).play(DOWN).play(DOWN) -> 1,
    ))
  }
}
