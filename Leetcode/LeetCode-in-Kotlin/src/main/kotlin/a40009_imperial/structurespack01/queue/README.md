# Question 2: Queues

Suggested value: 15 marks

## Problem Description

In this question you will implement a generic first-in-first-out queue. Queue structures appear frequently in the Java practical papers: for example in graph algorithms, priority scheduling, critical path analysis, and producer/consumer style simulations.

A queue stores elements in arrival order. New elements are placed at the back. Elements are removed from the front. The first element inserted should be the first element removed.

You are given a skeleton class:

- `ExamQueue<T>`

The class is generic, so it should support any value type `T`.

## Getting Started

You should edit only:

- `ExamQueue.kt`

You should use the public tests in:

- `ExamQueueTest.kt`

The public tests cover the main behaviours, but they are not a substitute for reasoning about your representation invariants.

## What To Do

1. Queue representation. [3 marks]
   Implement private state for the queue. A linked representation with a front node, a back node, and a size counter is a natural choice.

2. Enqueue. [3 marks]
   Implement `enqueue(value: T)`.

   This method should add `value` to the back of the queue. It must handle both the empty and non-empty cases.

3. Peek and dequeue. [4 marks]
   Implement `peek()` and `dequeue()`.

   `peek()` should return the front element without removing it. `dequeue()` should remove and return the front element. If the queue is empty, both methods should return `null`.

4. Size and emptiness. [2 marks]
   Implement the `size` property so that it is correct after every queue operation. The existing `isEmpty` property should then work from `size`.

5. Queue views. [3 marks]
   Implement `toList()` and `iterator()`.

   Both should expose the remaining queue contents from front to back. Neither method should mutate the queue.

## Restrictions

- Do not delegate the queue implementation to `ArrayDeque`, `MutableList`, Java `Queue`, or Java `LinkedList`.
- You may use a private node class.
- `peek`, `toList`, and `iterator` must not remove values from the queue.
- When the final element is removed, both front and back references should be updated consistently.

## Examples

If the following operations are performed:

```kotlin
val queue = ExamQueue<String>()
queue.enqueue("raw")
queue.enqueue("recycled")
queue.enqueue("goods")
```

then repeated calls to `dequeue()` should return:

```text
raw
recycled
goods
null
```

Before any dequeue operation:

```kotlin
queue.peek() == "raw"
queue.toList() == listOf("raw", "recycled", "goods")
```

## Edge Cases To Think About

- Calling `dequeue` on an empty queue.
- Calling `peek` on an empty queue.
- Enqueuing after the queue has become empty again.
- Iterating over a queue with one element.
- Confirming that iteration does not change `size`.
