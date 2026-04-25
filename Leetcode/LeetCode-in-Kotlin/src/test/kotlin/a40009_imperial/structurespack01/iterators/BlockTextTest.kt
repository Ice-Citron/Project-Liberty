package a40009_imperial.structurespack01.iterators

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class BlockTextTest {
    @Test
    fun appendsAndReadsCharactersAcrossBlocks() {
        val text = BlockText(blockSize = 4)

        text.append("Alpha")
        text.append("Beta")

        assertThat(text.length, equalTo(9))
        assertThat(text.toString(), equalTo("AlphaBeta"))
        assertThat(text.charAt(0), equalTo('A'))
        assertThat(text.charAt(5), equalTo('B'))
        assertThat(text.iterator().asSequence().joinToString(separator = ""), equalTo("AlphaBeta"))
    }

    @Test
    fun insertsAtBeginningMiddleAndEnd() {
        val text = BlockText(blockSize = 4)
        text.append("AlphaBeta")

        text.insert(5, " ")
        text.insert(0, "[")
        text.insert(text.length, "]")

        assertThat(text.toString(), equalTo("[Alpha Beta]"))
        assertThat(text.length, equalTo(12))
    }

    @Test
    fun rebalanceSplitsIntoPredictableBlocks() {
        val text = BlockText(blockSize = 4)
        text.append("Alpha Beta")

        text.rebalance()

        assertThat(text.blocks(), equalTo(listOf("Alph", "a Be", "ta")))
        assertThat(text.toString(), equalTo("Alpha Beta"))
    }

    @Test
    fun invalidOffsetsThrow() {
        val text = BlockText(blockSize = 3)
        text.append("abc")

        assertThrows(IndexOutOfBoundsException::class.java) { text.charAt(-1) }
        assertThrows(IndexOutOfBoundsException::class.java) { text.charAt(3) }
        assertThrows(IndexOutOfBoundsException::class.java) { text.insert(4, "x") }
    }
}
