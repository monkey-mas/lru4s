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
    if (!isEmpty) {
      key2NodeBiMap.inverse().remove(lruNode)

      val node = lruNode.asInstanceOf[Node].getNext
      if (node ne DNil) {
        node.asInstanceOf[Node].setPrev(DNil)
      }
      lruNode = node
    }

  private def appendToMruNode(node: DoublyLinkedList): Unit =
    node match {
      case DNil => // do nothing
      case node: Node =>
        node.setPrev(mruNode)
        node.setNext(DNil)

        if (lruNode eq DNil) {
          lruNode = node
        }

        if (mruNode ne DNil) {
          mruNode.asInstanceOf[Node].setNext(node)
        }
        mruNode = node
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

  private[lru4s] def getLruValue: Option[V] = if (lruNode eq DNil) None else Some(lruNode.asInstanceOf[Node].getValue)
  private[lru4s] def getMruValue: Option[V] = if (mruNode eq DNil) None else Some(mruNode.asInstanceOf[Node].getValue)
}

object LRUCache {
  final val MAX_CACHE_SIZE = 3
}
