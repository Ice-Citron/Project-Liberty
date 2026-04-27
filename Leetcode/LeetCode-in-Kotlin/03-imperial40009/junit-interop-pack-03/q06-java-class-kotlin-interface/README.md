# Question 6: Test Java Class Implementing Kotlin Interface

Difficulty: Hard

Main files:

- `src/main/kotlin/a40009_imperial/junitinterop03/q06/Note.kt`
- `src/main/kotlin/a40009_imperial/junitinterop03/q06/Tune.kt`
- `src/main/kotlin/a40009_imperial/junitinterop03/q06/StandardTune.kt`
- `src/main/java/a40009_imperial/junitinterop03/q06/StretchedTune.java`

Practice test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q06/Q06StretchedTunePracticeTest.kt`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q06/Q06StretchedTuneSolutionTest.kt`

## Background

This mirrors the Kotlin tunes sample paper pattern where a Java class implements or wraps Kotlin tune abstractions.

`StretchedTune.java` implements the Kotlin interface:

```kotlin
interface Tune : Iterable<Note> {
    val notes: List<Note>
    val totalDuration: Double
    fun addNote(newNote: Note)
}
```

## Task

Write Kotlin tests checking:

1. `getNotes()` returns stretched copies.
2. `totalDuration` uses stretched durations.
3. `addNote()` stores the inverse-duration note into the target tune.

## Conceptual Hint

You are not just testing music logic. You are testing Java/Kotlin interop:

- Java implements a Kotlin interface.
- Kotlin sees Java `getNotes()` as `notes`.
- Kotlin sees Java `getTotalDuration()` as `totalDuration`.

## Syntactic Hint

Use a tolerance for floating-point assertions:

```kotlin
assertEquals(9.0, stretched.totalDuration, 0.0001)
```

Use the interface type to prove interop:

```kotlin
val stretched: Tune = StretchedTune(target, 2.0)
```

## Answer

See `Q06StretchedTuneSolutionTest.kt`.
