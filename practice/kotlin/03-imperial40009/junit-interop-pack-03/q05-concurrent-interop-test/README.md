# Question 5: Concurrent Kotlin Test For A Java Class

Difficulty: Hard

Main files:

- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeUser.java`
- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeMatchmaker.java`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q05/Q05ConcurrentMatchmakerSolutionTest.kt`

## Background

This is based on the concurrent test shape used in the Kotlin social sample paper: create users, start several threads, join them, then check the final invariant.

## Task

Write a Kotlin/JUnit test that:

1. Creates 20 users.
2. Gives even-index users birth year `1990` and odd-index users birth year `1991`.
3. Starts several threads.
4. Each thread repeatedly calls Java `PracticeMatchmaker.tryMatching`.
5. After joining all threads, asserts that every friendship is between users born in the same year.

## Conceptual Hint

Concurrent tests usually do not assert exact final ordering. Instead, they assert invariants that must always hold.

Here the invariant is:

```text
for every user and every friend:
    user.yearOfBirth == friend.yearOfBirth
```

## Syntactic Hint

```kotlin
val body = object : Runnable {
    override fun run() {
        // repeated work here
    }
}

val threads = (0 until 4).map { Thread(body) }
threads.forEach(Thread::start)
threads.forEach(Thread::join)
```

## Answer

See `Q05ConcurrentMatchmakerSolutionTest.kt`.
