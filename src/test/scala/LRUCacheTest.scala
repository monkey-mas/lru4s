package lru4s

import org.scalatest.FunSpec

class LRUCacheTest extends FunSpec {
  describe("get(k)") {
    it("get key if the key exists") {
      val cache = new LRUCache[Int, Int]()
      cache.put(0, 1)
      assert(cache.get(0) == 1)
    }

    it("throw exception if key doesn't exist") {
      val cache = new LRUCache[Int, Int]()
      assertThrows[RuntimeException] {
        cache.get(0)
      }
    }
  }

  describe("put(k, v)") {
    it("put key and value correctly") {
      val cache = new LRUCache[Int, Int]()
      cache.put(0, 1)
      assert(cache.get(0) == 1)
    }

    it("LRU node is deleted when a new key-value pair is put for a full cache") {
      val cache = new LRUCache[Int, Int]()
      for (i <- 0 to LRUCache.MAX_CACHE_SIZE) {
        cache.put(i, i)
      }
      assertThrows[RuntimeException] {
        cache.get(0) // key: 0 was deleted as it was the LRU node when we put the MAX_CACHE_SIZE-th node
      }
      for (i <- 1 to LRUCache.MAX_CACHE_SIZE) {
        assert(cache.get(i) == i)
      }
    }
  }
}
