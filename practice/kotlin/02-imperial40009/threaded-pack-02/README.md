# COMP40009 Retrofit Pack 02: Threads, Comparables, Java, and Tests

Suggested total value: 100 marks

This pack is a follow-up to `01-imperial40009`. It assumes you have already practised basic linked lists, iterators, hash maps, queues, and trees. The point here is to push the same ideas toward the current Kotlin/Java exam emphasis:

- Kotlin OOP, interfaces, inheritance, abstract classes, generics;
- custom collections, iterators, trees, and block-based structures;
- locks, thread-safety, `Thread`, `Runnable`, and `try/finally`;
- Java conversion/interoperability;
- writing JUnit tests yourself.

Source package:

```text
src/main/kotlin/a40009_imperial/threadedpack02
src/main/java/a40009_imperial/threadedpack02
```

Test package:

```text
src/test/kotlin/a40009_imperial/threadedpack02
```

## Suggested Order

1. `LockedLinkedListTest`
2. `ConcurrentBlockTextTest`
3. `ParallelWordCounterTest`
4. `ComparableFrequencyTreeTest`
5. `ReadyGradeBookTest`
6. `ReadyTicketOfficeTest`

The first four questions require implementation. The final two questions require test writing only: the production classes are already implemented, and you should edit only the test files.

## Question Overview

### Question 1: Locked Linked List

File:

```text
src/main/kotlin/a40009_imperial/threadedpack02/linkedlist/LockedLinkedList.kt
```

Implement a generic linked list protected by a `ReentrantLock`.

Key topics:

- linked-node representation;
- `ReentrantLock`;
- `try/finally`;
- thread-safe `size`, `addLast`, and `removeFirst`;
- snapshot-based iteration.

### Question 2: Concurrent Block Text

File:

```text
src/main/kotlin/a40009_imperial/threadedpack02/iterators/ConcurrentBlockText.kt
```

Implement a thread-safe block-backed text object.

Key topics:

- `StringBuilder` blocks;
- offset-based insertion;
- `charAt`;
- rebalancing;
- snapshot iterators;
- locking around compound state.

### Question 3: Java Parallel Word Counter

File:

```text
src/main/java/a40009_imperial/threadedpack02/javaworkers/ParallelWordCounter.java
```

Write Java code using `Runnable` and `Thread`.

Key topics:

- Java class syntax;
- `Runnable.run`;
- creating and joining threads;
- partitioning work;
- merging worker results safely after `join`.

### Question 4: Comparable Frequency Tree

File:

```text
src/main/kotlin/a40009_imperial/threadedpack02/tree/ComparableFrequencyTree.kt
```

Build a tree whose nodes implement `Comparable`.

Key topics:

- generic bounds: `T : Comparable<T>`;
- implementing `Comparable<...>`;
- priority-queue style ordering;
- deterministic tie-breaking;
- tree traversal and codes.

### Question 5: Test Writing - Grade Book

Files:

```text
src/main/kotlin/a40009_imperial/threadedpack02/testwriting/ReadyGradeBook.kt
src/test/kotlin/a40009_imperial/threadedpack02/testwriting/ReadyGradeBookTest.kt
```

The implementation is already complete. Write tests only.

Key topics:

- JUnit assertions;
- edge cases;
- ranking and tie-breaking;
- invalid input tests.

### Question 6: Test Writing - Ticket Office

Files:

```text
src/main/kotlin/a40009_imperial/threadedpack02/testwriting/ReadyTicketOffice.kt
src/test/kotlin/a40009_imperial/threadedpack02/testwriting/ReadyTicketOfficeTest.kt
```

The implementation is already complete. Write tests only.

Key topics:

- testing stateful classes;
- testing thread-safe behaviour;
- `Thread`;
- `join`;
- assertions after concurrent operations.

## Working Advice

Run one test class at a time from IntelliJ. The test-writing questions intentionally contain placeholder failing tests. Replace those placeholders with your own assertions.

Do not edit public method signatures. The intended exam workflow is:

1. read this question sheet;
2. inspect the skeleton;
3. inspect the public tests;
4. implement or write tests;
5. add your own hidden-edge-case tests mentally or directly.
