package a40009_imperial.threadedpack02.iterators

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class ConcurrentBlockTextTest {
    @Test
    fun insertsRebalancesAndReadsAcrossBlocks() {
        val text = ConcurrentBlockText(blockSize = 4)

        text.append("AlphaBeta")
        text.insert(5, " ")
        text.rebalance()

        assertThat(text.length, equalTo(10))
        assertThat(text.toString(), equalTo("Alpha Beta"))
        assertThat(text.blocks(), equalTo(listOf("Alph", "a Be", "ta")))
        assertThat(text.charAt(0), equalTo('A'))
        assertThat(text.charAt(6), equalTo('B'))
    }

    @Test
    fun iteratorUsesAStableSnapshot() {
        val text = ConcurrentBlockText(blockSize = 4)
        text.append("Data")

        val iterator = text.iterator()
        text.append("!")

        assertThat(iterator.asSequence().joinToString(separator = ""), equalTo("Data"))
        assertThat(text.toString(), equalTo("Data!"))
    }

    @Test
    fun concurrentAppendsAreNotLost() {
        val text = ConcurrentBlockText(blockSize = 16)
        val threads = List(6) {
            Thread {
                repeat(100) {
                    text.append("x")
                }
            }
        }

        threads.forEach(Thread::start)
        threads.forEach(Thread::join)

        assertThat(text.length, equalTo(600))
        assertThat(text.toString().count { it == 'x' }, equalTo(600))
    }

    @Test
    fun invalidIndexesAndOffsetsThrow() {
        val text = ConcurrentBlockText(blockSize = 3)
        text.append("abc")

        assertThrows(IndexOutOfBoundsException::class.java) { text.charAt(-1) }
        assertThrows(IndexOutOfBoundsException::class.java) { text.charAt(3) }
        assertThrows(IndexOutOfBoundsException::class.java) { text.insert(4, "x") }
    }
}
