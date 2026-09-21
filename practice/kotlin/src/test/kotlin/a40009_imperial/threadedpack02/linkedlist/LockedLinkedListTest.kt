package a40009_imperial.threadedpack02.linkedlist

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Test

internal class LockedLinkedListTest {
    @Test
    fun appendsAndRemovesFromTheFront() {
        val list = LockedLinkedList<String>()

        list.addLast("raw")
        list.addLast("recycled")
        list.addLast("goods")

        assertThat(list.size, equalTo(3))
        assertThat(list.snapshot(), equalTo(listOf("raw", "recycled", "goods")))
        assertThat(list.removeFirst(), equalTo("raw"))
        assertThat(list.removeFirst(), equalTo("recycled"))
        assertThat(list.removeFirst(), equalTo("goods"))
        assertThat(list.removeFirst(), equalTo(null))
        assertThat(list.isEmpty, equalTo(true))
    }

    @Test
    fun iteratorUsesAStableSnapshot() {
        val list = LockedLinkedList<Int>()
        list.addLast(1)
        list.addLast(2)

        val iterator = list.iterator()
        list.addLast(3)

        assertThat(iterator.asSequence().toList(), equalTo(listOf(1, 2)))
        assertThat(list.snapshot(), equalTo(listOf(1, 2, 3)))
    }

    @Test
    fun concurrentAddsAreNotLost() {
        val list = LockedLinkedList<Int>()
        val threads = List(4) { workerId ->
            Thread {
                repeat(250) { offset ->
                    list.addLast(workerId * 250 + offset)
                }
            }
        }

        threads.forEach(Thread::start)
        threads.forEach(Thread::join)

        assertThat(list.size, equalTo(1000))
        assertThat(list.snapshot().toSet().size, equalTo(1000))
    }
}
