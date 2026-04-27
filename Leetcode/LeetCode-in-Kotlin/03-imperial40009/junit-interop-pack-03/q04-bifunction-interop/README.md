# Question 4: Kotlin Function Reference To Java `BiFunction`

Difficulty: Hard

Main files:

- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeUser.java`
- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeMatchmaker.java`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q04/Q04BiFunctionSolutionTest.kt`

## Background

This mirrors the `kotlin-social` sample paper pattern, where a Java `Matchmaker` takes a Java functional interface but is tested from Kotlin.

The Java constructor is:

```java
public PracticeMatchmaker(BiFunction<PracticeUser, PracticeUser, Boolean> canMatch)
```

## Task

Write a Kotlin test that:

1. Defines a Kotlin function `sameYear`.
2. Passes `::sameYear` into the Java constructor.
3. Creates two compatible users and one incompatible user.
4. Checks that only compatible users become friends.

## Conceptual Hint

Kotlin function references can be adapted to Java SAM interfaces, including `BiFunction<A, B, R>`.

## Syntactic Hint

```kotlin
val matchmaker = PracticeMatchmaker(::sameYear)

private fun sameYear(first: PracticeUser, second: PracticeUser): Boolean {
    return first.yearOfBirth == second.yearOfBirth
}
```

## Answer

See `Q04BiFunctionSolutionTest.kt`.
