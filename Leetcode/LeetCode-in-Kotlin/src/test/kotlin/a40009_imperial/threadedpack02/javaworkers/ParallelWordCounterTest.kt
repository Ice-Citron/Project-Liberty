package a40009_imperial.threadedpack02.javaworkers

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class ParallelWordCounterTest {
    @Test
    fun workerCountsOnlyItsAssignedRange() {
        val lines = listOf("ignored ignored", "alpha beta", "beta gamma", "ignored")
        val worker = ParallelWordCounter.Worker(lines, 1, 3)

        worker.run()

        assertThat(worker.counts(), equalTo(mapOf("alpha" to 1, "beta" to 2, "gamma" to 1)))
    }

    @Test
    fun countsWordsUsingSeveralThreads() {
        val counter = ParallelWordCounter()
        val lines = listOf(
            "alpha beta",
            "beta gamma alpha",
            "",
            "delta beta",
        )

        val result = counter.countWords(lines, 3)

        assertThat(
            result,
            equalTo(mapOf("alpha" to 2, "beta" to 3, "gamma" to 1, "delta" to 1)),
        )
    }

    @Test
    fun rejectsNonPositiveThreadCount() {
        val counter = ParallelWordCounter()

        assertThrows(IllegalArgumentException::class.java) {
            counter.countWords(listOf("alpha"), 0)
        }
    }
}
