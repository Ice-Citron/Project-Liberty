# Question 2: Write Boundary Tests For Java `get`

Difficulty: Medium

Main file:

- `src/main/java/a40009_imperial/junitinterop03/q01/ExamLinkedListJava.java`

Practice test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q02/Q02BoundaryPracticeTest.kt`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q02/Q02BoundarySolutionTest.kt`

## Task

Write three Kotlin/JUnit tests for:

```java
public T get(int index)
```

Required behaviour:

1. It returns the first and last valid elements.
2. It throws `IndexOutOfBoundsException` for a negative index.
3. It throws `IndexOutOfBoundsException` for an index equal to `size`.

## Implementation Requirement

Edit the practice file. Remove `@Disabled` from one test at a time, then write the assertions.

## Conceptual Hint

Boundary testing is about the smallest valid value, largest valid value, and the values immediately outside the valid range.

For a list of size `3`:

- valid indexes: `0`, `1`, `2`
- invalid low boundary: `-1`
- invalid high boundary: `3`

## Syntactic Hint

Use:

```kotlin
assertEquals(expected, actual)
assertFailsWith<IndexOutOfBoundsException> {
    list[-1]
}
```

## Answer

See `Q02BoundarySolutionTest.kt`.
