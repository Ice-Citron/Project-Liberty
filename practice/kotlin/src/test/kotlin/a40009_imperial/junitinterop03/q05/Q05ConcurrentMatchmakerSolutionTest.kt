package a40009_imperial.junitinterop03.q05

import a40009_imperial.junitinterop03.q04q05.PracticeMatchmaker
import a40009_imperial.junitinterop03.q04q05.PracticeUser
import java.util.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class Q05ConcurrentMatchmakerSolutionTest {
    @Test
    fun `concurrent matching never creates cross year friendships`() {
        repeat(30) {
            val users = (0 until 20).map { index ->
                PracticeUser(
                    "user$index",
                    if (index % 2 == 0) 1990 else 1991,
                )
            }

            val body = object : Runnable {
                override fun run() {
                    val random = Random()
                    val matchmaker = PracticeMatchmaker(::sameYear)
                    repeat(1_000) {
                        val first = users[random.nextInt(users.size)]
                        val second = users[random.nextInt(users.size)]
                        matchmaker.tryMatching(first, second)
                    }
                }
            }

            val threads = (0 until 4).map { Thread(body) }
            threads.forEach(Thread::start)
            threads.forEach(Thread::join)

            for (user in users) {
                for (friend in user.currentFriends) {
                    assertEquals(user.yearOfBirth, friend.yearOfBirth)
                }
            }
        }
    }
}

private fun sameYear(first: PracticeUser, second: PracticeUser): Boolean {
    return first.yearOfBirth == second.yearOfBirth
}
