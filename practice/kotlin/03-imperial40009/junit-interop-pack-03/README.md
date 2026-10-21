# COMP40009 Practice Pack 03: JUnit Tests And Java Interop

This pack turns the six Java/JUnit interop drills into runnable project files.

Theme:

- reading Kotlin tests that target Java classes
- writing JUnit/Kotlin tests for Java implementations
- converting old Java `main`-style tests into proper JUnit tests
- passing Kotlin functions into Java functional interfaces
- testing Java concurrency from Kotlin
- testing a Java class implementing a Kotlin interface

## Sources Used As Style References

- `kotlin-collections-skeleton`: `SinglyLinkedListJava.java` and `SinglyLinkedListJavaTests.kt`
- `kotlin-social-skeleton`: `Matchmaker.java` and `Question7Tests.kt`
- `kotlin-textfiles-skeleton`: `LazyTextFile.java` and `Question6Tests.kt`
- `kotlin-tunes-skeleton`: `StretchedTune.java` and `Question6Tests.kt`
- older Java archive style: `Emergency Dialler/TestPerson.java`

These are not copied questions. They are new practice tasks shaped around the same exam patterns.

## Recommended Order

1. `q01-read-kotlin-java-list`: easy warm-up, read Kotlin syntax against Java methods.
2. `q02-write-boundary-tests`: write boundary tests for Java list indexing.
3. `q03-convert-main-to-junit`: convert old Java `main` testing into JUnit.
4. `q04-bifunction-interop`: Kotlin function reference passed to Java `BiFunction`.
5. `q05-concurrent-interop-test`: thread-based Kotlin test of a Java class.
6. `q06-java-class-kotlin-interface`: test Java implementation of a Kotlin interface.

## Code Layout

Main source:

- `src/main/java/a40009_imperial/junitinterop03/q01/ExamLinkedListJava.java`
- `src/main/java/a40009_imperial/junitinterop03/q03/ExamPerson.java`
- `src/main/java/a40009_imperial/junitinterop03/q03/ExamLocation.java`
- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeUser.java`
- `src/main/java/a40009_imperial/junitinterop03/q04q05/PracticeMatchmaker.java`
- `src/main/kotlin/a40009_imperial/junitinterop03/q06/Note.kt`
- `src/main/kotlin/a40009_imperial/junitinterop03/q06/Tune.kt`
- `src/main/kotlin/a40009_imperial/junitinterop03/q06/StandardTune.kt`
- `src/main/java/a40009_imperial/junitinterop03/q06/StretchedTune.java`

Test source:

- `src/test/kotlin/a40009_imperial/junitinterop03/q01/Q01InteropSyntaxSolutionTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q02/Q02BoundaryPracticeTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q02/Q02BoundarySolutionTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q03/Q03PersonPracticeTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q03/Q03PersonSolutionTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q04/Q04BiFunctionSolutionTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q05/Q05ConcurrentMatchmakerSolutionTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q06/Q06StretchedTunePracticeTest.kt`
- `src/test/kotlin/a40009_imperial/junitinterop03/q06/Q06StretchedTuneSolutionTest.kt`

## How To Use

For normal practice:

1. Open a question README.
2. Read the source class.
3. For test-writing questions, open the `PracticeTest.kt` file.
4. Remove `@Disabled`.
5. Write your tests.
6. Compare against the matching `SolutionTest.kt`.

For fast revision, run only one solution test class from IntelliJ using the green gutter icon.

## Notes On Disabled Practice Tests

The practice test files are intentionally marked with `@Disabled` so the project still compiles and runs before you fill them in. Remove `@Disabled` only when you want to attempt that file.
