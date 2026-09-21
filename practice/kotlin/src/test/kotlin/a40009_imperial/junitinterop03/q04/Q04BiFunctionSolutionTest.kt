package a40009_imperial.junitinterop03.q04

import a40009_imperial.junitinterop03.q04q05.PracticeMatchmaker
import a40009_imperial.junitinterop03.q04q05.PracticeUser
import kotlin.test.Test
import kotlin.test.assertEquals

class Q04BiFunctionSolutionTest {
    @Test
    fun `kotlin function reference is accepted as java BiFunction`() {
        val matchmaker = PracticeMatchmaker(::sameYear)

        val ada = PracticeUser("ada", 1990)
        val grace = PracticeUser("grace", 1990)
        val alan = PracticeUser("alan", 1912)

        matchmaker.tryMatching(ada, grace)
        matchmaker.tryMatching(ada, alan)

        assertEquals(listOf(grace), ada.currentFriends)
        assertEquals(listOf(ada), grace.currentFriends)
        assertEquals(emptyList(), alan.currentFriends)
    }
}

private fun sameYear(first: PracticeUser, second: PracticeUser): Boolean {
    return first.yearOfBirth == second.yearOfBirth
}
