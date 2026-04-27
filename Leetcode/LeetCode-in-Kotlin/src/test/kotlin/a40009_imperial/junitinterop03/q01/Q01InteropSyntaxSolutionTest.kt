package a40009_imperial.junitinterop03.q01

import kotlin.test.Test
import kotlin.test.assertEquals

class Q01InteropSyntaxSolutionTest {
    @Test
    fun `kotlin property and index syntax call java methods`() {
        val list = ExamLinkedListJava<String>()
        list.add(0, "cat")
        list.add(1, "dog")

        assertEquals(2, list.size)
        assertEquals("cat", list[0])
        assertEquals("dog", list[1])
        assertEquals("[cat, dog]", list.toString())
    }
}
