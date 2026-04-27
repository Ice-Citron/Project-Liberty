package a40009_imperial.junitinterop03.q03

import org.junit.jupiter.api.Disabled
import kotlin.test.Test

class Q03PersonPracticeTest {
    @Disabled("Practice file: remove @Disabled and convert the old Java main-test into assertions.")
    @Test
    fun `constructor stores telephone number and address`() {
        // TODO: val p = ExamPerson("Seb", 3, 4, 5)
        // TODO: assert telephone number, address.x, and address.y.
    }

    @Disabled("Practice file: remove @Disabled and test the precondition.")
    @Test
    fun `constructor rejects blank name`() {
        // TODO: assertFailsWith<IllegalArgumentException> { ExamPerson("", 3, 4, 5) }
    }
}
