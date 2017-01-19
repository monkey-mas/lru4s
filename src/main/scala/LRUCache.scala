package lru4s

import com.google.common.collect.HashBiMap

/**
  * LRU cache with Time Complexity
  *   get(key): O(1)
  *   put(key, value): O(1)
  *
  * This cache is based on
  *   a) Hash table to access to a value by get(key)
  *   b) Doubly-Linked List used to update LRU and MRU nodes quickly
  */
class LRUCache[K, V]() {
  
  sealed abstract class DoublyLinkedList()
  class Node(private[this] var value: V) extends DoublyLinkedList {
    private[this] var prev: DoublyLinkedList = DNil
    private[this] var next: DoublyLinkedList = DNil

    def getValue: V = value

    def setValue(newValue: V) =
      value = newValue

    def getPrev: DoublyLinkedList = prev

    def setPrev(newPrev: DoublyLinkedList) =
      prev = newPrev

    def getNext: DoublyLinkedList = next

    def setNext(newNext: DoublyLinkedList) =
      next = newNext
  }
  object DNil extends DoublyLinkedList

  private[this] val key2NodeBiMap = HashBiMap.create[K, Node](LRUCache.MAX_CACHE_SIZE)
  private[this] var lruNode: DoublyLinkedList = DNil
  private[this] var mruNode: DoublyLinkedList = DNil

  def isEmpty: Boolean = key2NodeBiMap.isEmpty

  def get(key: K): V = {
    if (key2NodeBiMap.containsKey(key)) {
      val node = key2NodeBiMap.get(key)
      updateCache(node)
      node.getValue
    }
    else {
      throw new RuntimeException(s"key=$key is not available in the cache: key might have been deleted already.")
    }
  }

  /**
    * If the @key already exists in the cache, we update the corresponding value with @value,
    * otherwise add the key-value pair to the cache
    */
  def put(key: K, value: V): Unit =
    if (key2NodeBiMap.containsKey(key)) {
      val node = key2NodeBiMap.get(key)
      updateCache(node)
      node.setValue(value)
    }
    else {
      if (key2NodeBiMap.size() == LRUCache.MAX_CACHE_SIZE) {
        deleteLruNode()
      }

      val node = new Node(value)
      appendToMruNode(node)
      key2NodeBiMap.put(key, node)
    }

  private def deleteLruNode(): Unit =
    if (isEmpty) {
      throw new RuntimeException("Can't delete LRU node: Cache is empty")
    } else {
      // As the Cache is not empty, we can safely access to the next node of LRU node.
      try {
        key2NodeBiMap.inverse().remove(lruNode.asInstanceOf[Node])

        val newLruNode = lruNode.asInstanceOf[Node].getNext
        newLruNode match {
          case node: Node =>
            node.setPrev(DNil)
            lruNode = node
          case DNil =>
            lruNode = DNil
            mruNode = DNil
        }
      } catch {
        case e: ClassCastException =>
          // This shouldn't happen when asInstanceOf[Node] is called.
      }
    }

  private def appendToMruNode(node: DoublyLinkedList): Unit =
    node match {
      case DNil => // Do nothing
      case node: Node =>
        if (isEmpty) {
          lruNode = node
          mruNode = node
        } else {
          node.setNext(DNil)
          node.setPrev(mruNode)
          try {
            // As the cache is not empty, MRU(last) node is not DNil
            mruNode.asInstanceOf[Node].setNext(node)
          } catch {
            case e: ClassCastException =>
              // This shouldn't happen
          }
          mruNode = node
        }
    }

  /**
   * Update the input node as the MRU node
   *
   * We have three cases when updating a node where the node is
   *
   * a) most recently used node
   *    If so, there's no need for us to do anything
   *
   * b) least recently used node
   *    So we update the lists as follows;
   *    Before: (node) <--> (second LRU node) <--> ... <--> (MRU node)
   *    After:  (second LRU node) <--> ... <--> (MRU node) <--> (node)
   *
   * c) a node somewhere between the list
   *    So we update the lists as follows;
   *    Before: ... <--> (prev) <--> (node) <--> (next) <--> ... <--> (MRU node)
   *    After:  ... <--> (prev) <--> (next) <--> ... <--> (MRU node) <--> (node)
   */
  private def updateCache(node: DoublyLinkedList): Unit =
    if (node eq mruNode) {
      // The node is already Most-Recently-Used
    }
    else if (node eq lruNode) {
      deleteLruNode()
      appendToMruNode(node)
    }
    else {
      try {
        // As the node is neither LRU(head) nor MRU(last),
        // we can safely say that (prev != DNil && next != DNil)
        val prev = node.asInstanceOf[Node].getPrev.asInstanceOf[Node]
        val next = node.asInstanceOf[Node].getNext.asInstanceOf[Node]
        prev.setNext(next)
        next.setPrev(prev)
      } catch {
        case e: ClassCastException =>
          // This shouldn't happen when asInstanceOf[Node] is called.
      }
    }
}

object LRUCache {
  final val MAX_CACHE_SIZE = 3
}
