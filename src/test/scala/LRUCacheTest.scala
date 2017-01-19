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

    it("key obtained via get(key) becomes the MRU node") {
      val cache = new LRUCache[Int, Int]()
      cache.put(2, 2)
      cache.put(1, 1)
      cache.put(0, 0)

      assert(cache.getLruValue.contains(2))
      assert(cache.getMruValue.contains(0))

      assert(cache.get(2) == 2)
      assert(cache.getLruValue.contains(1))
      assert(cache.getMruValue.contains(2))

      assert(cache.get(0) == 0)
      assert(cache.getLruValue.contains(1))
      assert(cache.getMruValue.contains(0))
    }
  }

  describe("put(k, v)") {
    it("put key and value correctly") {
      val cache = new LRUCache[Int, Int]()
      cache.put(0, 1)
      assert(cache.get(0) == 1)
    }

    it("put key changes LRU and MRU nodes properly") {
      val cache = new LRUCache[Int, Int]()
      assert(cache.getLruValue.isEmpty)
      assert(cache.getMruValue.isEmpty)

      cache.put(0, 0)
      assert(cache.getLruValue.contains(0))
      assert(cache.getMruValue.contains(0))

      cache.put(1, 1)
      assert(cache.getLruValue.contains(0))
      assert(cache.getMruValue.contains(1))

      cache.put(2, 2)
      assert(cache.getLruValue.contains(0))
      assert(cache.getMruValue.contains(2))

      cache.put(3, 3)
      assert(cache.getLruValue.contains(1))
      assert(cache.getMruValue.contains(3))
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
