# Question 3: Convert A Java Main Test To JUnit

Difficulty: Medium

Main files:

- `src/main/java/a40009_imperial/junitinterop03/q03/ExamPerson.java`
- `src/main/java/a40009_imperial/junitinterop03/q03/ExamLocation.java`

Practice test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q03/Q03PersonPracticeTest.kt`

Solution test:

- `src/test/kotlin/a40009_imperial/junitinterop03/q03/Q03PersonSolutionTest.kt`

## Background

Older Java papers sometimes contain simple `main`-method tests. For example, the Emergency Dialler archive includes this style:

```java
public static void main(String[] args) {
    Person p = new Person("Seb", 3, 4, 5);
    if (p.getTelephoneNumber() != 3) {
        System.out.println("Failed number");
        return;
    }
    System.out.println("Succeeded test!");
}
```

Modern Gradle/IntelliJ practice is better done with JUnit tests.

## Task

Write a Kotlin/JUnit test that checks:

1. `ExamPerson("Seb", 3, 4, 5)` stores the name.
2. It stores the telephone number.
3. It stores the address coordinates.
4. A blank name is rejected.

## Conceptual Hint

Manual `if` checks become assertions. A test should fail automatically when the assertion is false.

## Syntactic Hint

Kotlin can call Java getters as properties:

```kotlin
person.name
person.telephoneNumber
person.address.x
person.address.y
```

For exceptions:

```kotlin
assertFailsWith<IllegalArgumentException> {
    ExamPerson("", 3, 4, 5)
}
```

## Answer

See `Q03PersonSolutionTest.kt`.
