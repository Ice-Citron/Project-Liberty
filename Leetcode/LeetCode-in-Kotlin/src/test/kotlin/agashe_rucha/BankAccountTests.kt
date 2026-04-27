package agashe_rucha

import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class BankAccountTests {
    private lateinit var account: BankAccount1

    @BeforeTest
    fun setup() {
        account = BankAccount1(balance = 100)
    }

    @Test
    fun `withdrawing valid amount reduces balance correctly`() {
        account.withdraw(40)

        assertEquals(60, account.balance)
    }

    @Test
    fun `withdrawing more than balance throws exception`() {
        assertFailsWith<IllegalStateException> {
            account.withdraw(500)
        }
    }
}
