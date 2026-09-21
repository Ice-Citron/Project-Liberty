package a40009_imperial.junitinterop03.q02

import org.junit.jupiter.api.Disabled
import kotlin.test.Test

class Q02BoundaryPracticeTest {
    @Disabled("Practice file: remove @Disabled and write the assertions yourself.")
    @Test
    fun `get returns first and last valid elements`() {
        // TODO: create ExamLinkedListJava<String>, add three values, assert indexes 0 and 2.
    }

    @Disabled("Practice file: remove @Disabled and write the assertions yourself.")
    @Test
    fun `get rejects negative index`() {
        // TODO: assertFailsWith<IndexOutOfBoundsException> { list[-1] }.
    }

    @Disabled("Practice file: remove @Disabled and write the assertions yourself.")
    @Test
    fun `get rejects index equal to size`() {
        // TODO: assertFailsWith<IndexOutOfBoundsException> { list[list.size] }.
    }
}
