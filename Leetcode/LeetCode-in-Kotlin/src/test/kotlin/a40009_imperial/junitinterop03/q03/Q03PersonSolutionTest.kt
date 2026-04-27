package a40009_imperial.junitinterop03.q03

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class Q03PersonSolutionTest {
    @Test
    fun `constructor stores telephone number and address`() {
        val person = ExamPerson("Seb", 3, 4, 5)

        assertEquals("Seb", person.name)
        assertEquals(3, person.telephoneNumber)
        assertEquals(4, person.address.x)
        assertEquals(5, person.address.y)
    }

    @Test
    fun `constructor rejects blank name`() {
        assertFailsWith<IllegalArgumentException> {
            ExamPerson("", 3, 4, 5)
        }
    }
}
