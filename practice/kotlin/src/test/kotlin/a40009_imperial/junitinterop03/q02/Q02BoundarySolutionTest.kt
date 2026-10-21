package a40009_imperial.junitinterop03.q02

import a40009_imperial.junitinterop03.q01.ExamLinkedListJava
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class Q02BoundarySolutionTest {
    @Test
    fun `get returns first and last valid elements`() {
        val list = ExamLinkedListJava<String>()
        list.add(0, "a")
        list.add(1, "b")
        list.add(2, "c")

        assertEquals("a", list[0])
        assertEquals("c", list[2])
    }

    @Test
    fun `get rejects negative index`() {
        val list = ExamLinkedListJava<String>()
        list.add(0, "a")

        assertFailsWith<IndexOutOfBoundsException> {
            list[-1]
        }
    }

    @Test
    fun `get rejects index equal to size`() {
        val list = ExamLinkedListJava<String>()
        list.add(0, "a")

        assertFailsWith<IndexOutOfBoundsException> {
            list[1]
        }
    }
}
