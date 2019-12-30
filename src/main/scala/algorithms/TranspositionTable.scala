package algorithms

import java.util

import algorithms.TranspositionTable.{LruMap, TTEntry}

class TranspositionTable[KEY, VALUE](maxSize: Int) {
  private val map = new LruMap[KEY, TTEntry[VALUE]](maxSize)

  def lookup(key: KEY): Option[TTEntry[VALUE]] = {
    val value = map.get(key)
    if (value != null) {
    }
    Option(value)
  }

  def store(key: KEY, ttEntry: TTEntry[VALUE]): Unit = map.put(key, ttEntry)

  override def toString: String = s"Table $map"
}

object TranspositionTable {
  sealed trait Flag
  case object EXACT extends Flag
  case object LOWER_BOUND extends Flag
  case object UPPER_BOUND extends Flag

  case class TTEntry[VALUE](value: VALUE, flag: Flag, depth: Int)

  private class LruMap[K, V](maxSize: Int) extends util.LinkedHashMap[K, V](
    16,  // util.HashMap.DEFAULT_INITIAL_CAPACITY
    0.75f, // util.HashMap.DEFAULT_LOAD_FACTOR
    true // order by LRU, instead of insertion order
  ) {
    override protected def removeEldestEntry(eldest: util.Map.Entry[K, V]): Boolean = {
      this.size() > maxSize
    }
  }
}