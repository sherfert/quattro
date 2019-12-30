package algorithms

import algorithms.TranspositionTable.{EXACT, TTEntry}
import org.scalatest.{FunSuite, Matchers}

class TranspositionTableTest extends FunSuite with Matchers {

  test("empty table lookup") {
    new TranspositionTable[Int, Int](10).lookup(0) should be(empty)
  }

  test("lookup same key") {
    val t = new TranspositionTable[Int, Int](10)
    val entry = TTEntry(5, EXACT, 2)
    t.store(0, entry)
    t.lookup(0) should be(Some(entry))
  }

  test("LRU eviction") {
    val t = new TranspositionTable[Int, Int](3)
    val entries = (0 until 5).map(TTEntry(_, EXACT, 1))

    (0 to 2).foreach { i => t.store(i, entries(i)) }

    // All entries should be present
    (0 to 2).foreach { i => t.lookup(i) should be(Some(entries(i))) }

    t.store(3, entries(3))

    // 0 should have been evicted
    t.lookup(0) should be (empty)
    (3 to 1 by -1).foreach { i => t.lookup(i) should be(Some(entries(i))) }

    t.store(4, entries(4))

    // 3 should have been evicted
    t.lookup(3) should be (empty)
    Seq(1, 2, 4).foreach { i => t.lookup(i) should be(Some(entries(i))) }
  }
}
