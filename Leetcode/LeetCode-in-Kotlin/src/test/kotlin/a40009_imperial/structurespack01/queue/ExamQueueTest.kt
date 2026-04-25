package a40009_imperial.structurespack01.queue

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Test

internal class ExamQueueTest {
    @Test
    fun emptyQueueReturnsNullForPeekAndDequeue() {
        val queue = ExamQueue<String>()

        assertThat(queue.size, equalTo(0))
        assertThat(queue.isEmpty, equalTo(true))
        assertThat(queue.peek(), equalTo(null))
        assertThat(queue.dequeue(), equalTo(null))
    }

    @Test
    fun dequeuesInFirstInFirstOutOrder() {
        val queue = ExamQueue<String>()
        queue.enqueue("raw-plastic")
        queue.enqueue("manufactured-good")
        queue.enqueue("disposed-good")

        assertThat(queue.peek(), equalTo("raw-plastic"))
        assertThat(queue.size, equalTo(3))
        assertThat(queue.dequeue(), equalTo("raw-plastic"))
        assertThat(queue.dequeue(), equalTo("manufactured-good"))
        assertThat(queue.dequeue(), equalTo("disposed-good"))
        assertThat(queue.dequeue(), equalTo(null))
        assertThat(queue.isEmpty, equalTo(true))
    }

    @Test
    fun iteratorAndToListDoNotMutateTheQueue() {
        val queue = ExamQueue<Int>()
        queue.enqueue(4)
        queue.enqueue(8)
        queue.enqueue(15)

        assertThat(queue.toList(), equalTo(listOf(4, 8, 15)))
        assertThat(queue.iterator().asSequence().toList(), equalTo(listOf(4, 8, 15)))
        assertThat(queue.size, equalTo(3))
        assertThat(queue.dequeue(), equalTo(4))
    }
}
