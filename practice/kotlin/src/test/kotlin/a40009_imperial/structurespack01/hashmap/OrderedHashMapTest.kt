package a40009_imperial.structurespack01.hashmap

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

internal class OrderedHashMapTest {
    @Nested
    inner class SizeAndEmptyTests {
        @Test
        fun newMapStartsEmpty() {
            val map = OrderedHashMap<String, Int>()

            assertThat(map.size, equalTo(0))
            assertThat(map.isEmpty, equalTo(true))
        }

        @Test
        fun sizeIncreasesWhenNewKeysAreInserted() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("a", 1)
                map.put("b", 2)
                map.put("a", 10)
            }

            assertThat(map.size, equalTo(2))
            assertThat(map.isEmpty, equalTo(false))
        }

        @Test
        fun sizeDecreasesWhenKeysAreRemoved() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("a", 1)
                map.put("b", 2)
            }
            assumingImplemented("remove") {
                map.remove("a")
            }

            assertThat(map.size, equalTo(1))
        }
    }

    @Nested
    inner class PutTests {
        @Test
        fun insertingANewKeyReturnsNull() {
            val map = OrderedHashMap<String, Int>(bucketCount = 4)

            assertThat(map.put("alpha", 1), equalTo(null))
        }

        @Test
        fun updatingAnExistingKeyReturnsThePreviousValue() {
            val map = OrderedHashMap<String, Int>(bucketCount = 4)

            map.put("alpha", 1)

            assertThat(map.put("alpha", 99), equalTo(1))
        }

        @Test
        fun updatingAnExistingKeyDoesNotIncreaseSize() {
            val map = OrderedHashMap<String, Int>(bucketCount = 4)

            map.put("alpha", 1)
            map.put("beta", 2)
            map.put("alpha", 99)

            assumingImplemented("size") {
                assertThat(map.size, equalTo(2))
            }
        }
    }

    @Nested
    inner class GetTests {
        @Test
        fun returnsTheValueForAPresentKey() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("crew", 8)
            }

            assertThat(map["crew"], equalTo(8))
        }

        @Test
        fun returnsNullForAMissingKey() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("crew", 8)
            }

            assertThat(map["missing"], equalTo(null))
        }

        @Test
        fun handlesDifferentKeysInTheSameBucket() {
            val map = OrderedHashMap<CollisionKey, String>(bucketCount = 1)
            val first = CollisionKey("first")
            val second = CollisionKey("second")

            assumingImplemented("put") {
                map.put(first, "A")
                map.put(second, "B")
            }

            assertThat(map[first], equalTo("A"))
            assertThat(map[second], equalTo("B"))
            assertThat(map[CollisionKey("third")], equalTo(null))
        }
    }

    @Nested
    inner class ContainsKeyTests {
        @Test
        fun returnsFalseForAMissingKeyInAnEmptyMap() {
            val map = OrderedHashMap<String, Int>()

            assertThat(map.containsKey("missing"), equalTo(false))
        }

        @Test
        fun returnsTrueForKeysCurrentlyInTheMap() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("business", 20)
            }

            assertThat(map.containsKey("business"), equalTo(true))
        }

        @Test
        fun distinguishesDifferentKeysWithTheSameHashCode() {
            val map = OrderedHashMap<CollisionKey, Int>(bucketCount = 1)

            assumingImplemented("put") {
                map.put(CollisionKey("x"), 10)
            }

            assertThat(map.containsKey(CollisionKey("x")), equalTo(true))
            assertThat(map.containsKey(CollisionKey("y")), equalTo(false))
        }
    }

    @Nested
    inner class RemoveTests {
        @Test
        fun removingAMissingKeyReturnsNull() {
            val map = OrderedHashMap<String, Int>()

            assertThat(map.remove("missing"), equalTo(null))
        }

        @Test
        fun removingAPresentKeyReturnsItsValue() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("a", 1)
                map.put("b", 2)
            }

            assertThat(map.remove("a"), equalTo(1))
        }

        @Test
        fun removedKeyCannotBeRemovedTwice() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("a", 1)
            }

            assertThat(map.remove("a"), equalTo(1))
            assertThat(map.remove("a"), equalTo(null))
        }
    }

    @Nested
    inner class RemoveOldestTests {
        @Test
        fun returnsNullWhenTheMapIsEmpty() {
            val map = OrderedHashMap<String, Int>()

            assertThat(map.removeOldest(), equalTo(null))
        }

        @Test
        fun removesAndReturnsTheOldestEntry() {
            val map = OrderedHashMap<String, String>()

            assumingImplemented("put") {
                map.put("crew", "A")
                map.put("business", "B")
                map.put("economy", "C")
            }

            assertThat(map.removeOldest(), equalTo("crew" to "A"))
        }

        @Test
        fun repeatedCallsRemoveEntriesFromOldestToNewest() {
            val map = OrderedHashMap<String, String>()

            assumingImplemented("put") {
                map.put("crew", "A")
                map.put("business", "B")
            }

            assertThat(map.removeOldest(), equalTo("crew" to "A"))
            assertThat(map.removeOldest(), equalTo("business" to "B"))
            assertThat(map.removeOldest(), equalTo(null))
        }
    }

    @Nested
    inner class OrderedViewTests {
        @Test
        fun keysAreReturnedFromOldestToNewest() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("x", 1)
                map.put("y", 2)
                map.put("z", 3)
            }

            assertThat(map.keysInInsertionOrder(), equalTo(listOf("x", "y", "z")))
        }

        @Test
        fun valuesAreReturnedFromOldestToNewest() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("x", 1)
                map.put("y", 2)
                map.put("z", 3)
            }

            assertThat(map.valuesInInsertionOrder(), equalTo(listOf(1, 2, 3)))
        }

        @Test
        fun orderedViewsReflectUpdatesAndRemovals() {
            val map = OrderedHashMap<String, Int>()

            assumingImplemented("put") {
                map.put("x", 1)
                map.put("y", 2)
                map.put("z", 3)
                map.put("x", 10)
            }
            assumingImplemented("remove") {
                map.remove("y")
            }

            assertThat(map.keysInInsertionOrder(), equalTo(listOf("z", "x")))
            assertThat(map.valuesInInsertionOrder(), equalTo(listOf(3, 10)))
        }
    }

    @Nested
    inner class IteratorTests {
        @Test
        fun iteratorReturnsPairsFromOldestToNewest() {
            val map = OrderedHashMap<Int, String>()

            assumingImplemented("put") {
                map.put(10, "ten")
                map.put(20, "twenty")
            }

            assertThat(map.iterator().asSequence().toList(), equalTo(listOf(10 to "ten", 20 to "twenty")))
        }

        @Test
        fun forLoopUsesTheOrderedIterator() {
            val map = OrderedHashMap<String, Int>()
            val visited = mutableListOf<String>()

            assumingImplemented("put") {
                map.put("a", 1)
                map.put("b", 2)
            }

            for ((key, value) in map) {
                visited.add("$key:$value")
            }

            assertThat(visited, equalTo(listOf("a:1", "b:2")))
        }
    }

    private inline fun <T> assumingImplemented(methodName: String, block: () -> T): T {
        return try {
            block()
        } catch (exception: NotImplementedError) {
            assumeTrue(false, "Skipped because prerequisite method `$methodName` is not implemented yet.")
            throw exception
        }
    }

    private data class CollisionKey(private val label: String) {
        override fun hashCode(): Int = 1
    }
}
