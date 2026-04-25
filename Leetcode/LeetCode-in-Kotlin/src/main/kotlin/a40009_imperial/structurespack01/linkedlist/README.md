# Question 1: Linked Lists

Suggested value: 20 marks

## Problem Description

In this question you will implement a small list library for a teaching system that records students in the order in which they join a course.

The department wants a list implementation that does not rely on the Kotlin or Java collection classes for its internal storage. Instead, the list should be represented using linked nodes. This is similar to the linked-list implementations studied in the module and to the collection questions that appear in the Java and Kotlin practice papers.

A linked list is a sequence of nodes. Each node stores one value and a reference to the next node. The first node is called the head. If the list is non-empty, the final node is called the tail and has no successor.

You are given a skeleton class:

- `ExamLinkedList<T>`

The class is generic, so it should be able to store values of any type `T`.

## Getting Started

You should edit only:

- `ExamLinkedList.kt`

You should use the public tests in:

- `ExamLinkedListTest.kt`

The tests are there to guide you. They are not exhaustive. Passing the public tests does not guarantee that your implementation is fully correct.

## What To Do

1. Basic list state. [3 marks]
   Implement enough private state to represent the list. A good solution will normally maintain references to the head and tail of the list, and an integer recording the number of elements.

2. Adding elements. [4 marks]
   Implement `addFirst(value: T)` and `addLast(value: T)`.

   `addFirst` should insert the value at the beginning of the list. `addLast` should insert the value at the end of the list. Both methods must behave correctly when the list is initially empty.

3. Index-based access. [3 marks]
   Implement `get(index: Int)`.

   Indexes are zero-based. Calling `get(0)` should return the first value in the list. Calling `get(size - 1)` should return the last value. Invalid indexes should throw `IndexOutOfBoundsException`.

4. Removing by index. [4 marks]
   Implement `removeAt(index: Int)`.

   This method should remove the value at the given index and return it. It must update all relevant list state, including the head, tail, and size. Invalid indexes should throw `IndexOutOfBoundsException`.

5. Removing by value. [3 marks]
   Implement `remove(value: T)`.

   This method should remove only the first occurrence of `value`. It should return `true` if an element was removed and `false` otherwise. Equality should use Kotlin's normal equality operator.

6. Views and iteration. [3 marks]
   Implement `toList`, `iterator`, and `toString`.

   `toList()` should return the current contents in logical order. `iterator()` should return an iterator over the list in logical order. `toString()` should use normal list formatting, for example `[A, B, C]`.

## Restrictions

- Do not store the list contents in `MutableList`, `ArrayList`, Java `LinkedList`, or any other library collection.
- You may use a private node class.
- You may create private helper methods.
- Your iterator should walk the linked structure; it should not rely on a stored Kotlin list.

## Examples

If the following operations are performed:

```kotlin
val list = ExamLinkedList<String>()
list.addLast("B")
list.addLast("C")
list.addFirst("A")
```

then:

```kotlin
list.size == 3
list.toList() == listOf("A", "B", "C")
list.toString() == "[A, B, C]"
```

After:

```kotlin
list.removeAt(1)
```

the list should contain:

```kotlin
listOf("A", "C")
```

## Edge Cases To Think About

- Removing from an empty list.
- Removing the only element.
- Removing the head.
- Removing the tail.
- Removing a value that appears more than once.
- Iterating over an empty list.
