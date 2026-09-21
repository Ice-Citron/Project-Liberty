# Question 1: Read Kotlin Tests For A Java List

Difficulty: Easy

Main file:

- `src/main/java/a40009_imperial/junitinterop03/q01/ExamLinkedListJava.java`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q01/Q01InteropSyntaxSolutionTest.kt`

## Background

This is based on the pattern used in the Kotlin collections sample paper, where Kotlin tests target a Java implementation such as `SinglyLinkedListJava`.

You are given this Kotlin test:

```kotlin
val list = ExamLinkedListJava<String>()
list.add(0, "cat")
list.add(1, "dog")

assertEquals(2, list.size)
assertEquals("cat", list[0])
assertEquals("dog", list[1])
assertEquals("[cat, dog]", list.toString())
```

## Task

Explain which Java methods must exist for this Kotlin test to compile and pass.

## Conceptual Hint

Kotlin often makes Java code look more Kotlin-like at the call site. It does not mean the Java source literally contains a Kotlin property or `operator fun`.

## Syntactic Hint

- `list.size` calls Java `getSize()`.
- `list[0]` calls Java `get(0)`.
- `list.add(0, "cat")` calls Java `add(int, T)`.

## Answer

The Java class must expose:

```java
public int getSize()
public void add(int index, T element)
public T get(int index)
public String toString()
```

The solution test demonstrates this directly.
