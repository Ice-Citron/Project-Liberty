package a40009_imperial.structurespack01.linkedlist

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class ExamLinkedListTest {
    @Test
    fun addsAtBothEndsAndReportsLogicalOrder() {
        val list = ExamLinkedList<String>()

        list.addLast("B")
        list.addLast("C")
        list.addFirst("A")

        assertThat(list.size, equalTo(3))
        assertThat(list.isEmpty, equalTo(false))
        assertThat(list.toList(), equalTo(listOf("A", "B", "C")))
        assertThat(list.toString(), equalTo("[A, B, C]"))
    }

    @Test
    fun supportsIndexedAccessAndRemoval() {
        val list = ExamLinkedList<Int>()
        list.addLast(10)
        list.addLast(20)
        list.addLast(30)
        list.addLast(40)

        assertThat(list.get(0), equalTo(10))
        assertThat(list.get(2), equalTo(30))
        assertThat(list.removeAt(1), equalTo(20))
        assertThat(list.removeAt(2), equalTo(40))
        assertThat(list.toList(), equalTo(listOf(10, 30)))
        assertThat(list.size, equalTo(2))
    }

    @Test
    fun removesOnlyTheFirstMatchingValue() {
        val list = ExamLinkedList<String>()
        list.addLast("red")
        list.addLast("blue")
        list.addLast("red")

        assertThat(list.remove("red"), equalTo(true))
        assertThat(list.toList(), equalTo(listOf("blue", "red")))
        assertThat(list.remove("green"), equalTo(false))
        assertThat(list.toList(), equalTo(listOf("blue", "red")))
    }

    @Test
    fun iteratorYieldsValuesInOrder() {
        val list = ExamLinkedList<Int>()
        list.addLast(1)
        list.addLast(2)
        list.addLast(3)

        assertThat(list.iterator().asSequence().toList(), equalTo(listOf(1, 2, 3)))
    }

    @Test
    fun invalidIndexesThrow() {
        val list = ExamLinkedList<Int>()
        list.addLast(1)

        assertThrows(IndexOutOfBoundsException::class.java) { list.get(-1) }
        assertThrows(IndexOutOfBoundsException::class.java) { list.get(1) }
        assertThrows(IndexOutOfBoundsException::class.java) { list.removeAt(1) }
    }
}
