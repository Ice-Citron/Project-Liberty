# Question 2: Queues

Suggested value: 15 marks

This question is about implementing a generic first-in-first-out queue in Kotlin. Queue structures appear frequently in practical papers: for example in graph algorithms, scheduling, priority systems, and producer/consumer-style simulations.

You should edit:

```text
src/main/kotlin/a40009_imperial/structurespack01/queue/ExamQueue.kt
```

Use the tests in:

```text
src/test/kotlin/a40009_imperial/structurespack01/queue/ExamQueueTest.kt
```

## Background

A queue stores elements in arrival order.

```text
front -> first inserted -> next -> newest <- back
```

New values are enqueued at the back. Values are dequeued from the front.

The first value inserted should be the first value removed.

Do not solve this by wrapping Kotlin's `ArrayDeque`, `MutableList`, Java `Queue`, or Java `LinkedList`.

## 1. Representation and Size

Methods involved:

```kotlin
val size: Int
val isEmpty: Boolean
```

### Implementation Requirement

Choose private state that can represent an empty or non-empty queue. A typical linked solution uses:

- a private node class;
- a `front` reference;
- a `back` reference;
- an integer counter for the number of queued values.

The `size` property should return the current number of values in the queue. The `isEmpty` property is already written in terms of `size`.

### Restriction

Do not compute `size` by walking the queue every time. Keep a counter and update it when values are enqueued or dequeued.

Do not delegate the queue implementation to `ArrayDeque`, `MutableList`, Java `Queue`, or Java `LinkedList`.

### Example

For a new queue:

```kotlin
val queue = ExamQueue<String>()

queue.size == 0
queue.isEmpty == true
```

After adding two values:

```kotlin
queue.enqueue("raw")
queue.enqueue("goods")

queue.size == 2
queue.isEmpty == false
```

## 2. `enqueue`

Method involved:

```kotlin
fun enqueue(value: T)
```

### Implementation Requirement

Add `value` to the back of the queue.

This method must handle both cases:

- the queue is empty;
- the queue already contains one or more values.

### Restriction

When enqueuing into an empty queue, update both `front` and `back` consistently. A one-element queue should have both references pointing at the same node.

### Example

```kotlin
val queue = ExamQueue<String>()
queue.enqueue("raw-plastic")
queue.enqueue("manufactured-good")
queue.enqueue("disposed-good")

queue.size == 3
```

The next value to leave the queue should be `"raw-plastic"`.

## 3. `peek`

Method involved:

```kotlin
fun peek(): T?
```

### Implementation Requirement

Return the value at the front of the queue without removing it.

If the queue is empty, return `null`.

### Restriction

Calling `peek()` must not mutate the queue. It should not change `front`, `back`, `size`, or any node links.

### Example

```kotlin
val queue = ExamQueue<String>()
queue.enqueue("raw")
queue.enqueue("goods")

queue.peek() == "raw"
queue.size == 2
queue.peek() == "raw"
queue.size == 2
```

For an empty queue:

```kotlin
queue.peek() == null
```

## 4. `dequeue`

Method involved:

```kotlin
fun dequeue(): T?
```

### Implementation Requirement

Remove and return the value at the front of the queue.

If the queue is empty, return `null`.

The queue must remain valid after removing:

- the first value from a multi-element queue;
- the only value from a one-element queue.

### Restriction

When the final element is removed, both `front` and `back` should become empty references. Leaving `back` pointing to a removed node is a stale-state bug.

### Example

```kotlin
val queue = ExamQueue<String>()
queue.enqueue("raw")
queue.enqueue("recycled")
queue.enqueue("goods")

queue.dequeue() == "raw"
queue.dequeue() == "recycled"
queue.dequeue() == "goods"
queue.dequeue() == null
queue.isEmpty == true
```

## 5. `toList`

Method involved:

```kotlin
fun toList(): List<T>
```

### Implementation Requirement

Return the remaining queue contents from front to back.

This method should not remove values from the queue.

### Restriction

Return a new list. A caller should not be able to mutate your internal queue representation by changing the returned list.

### Example

```kotlin
val queue = ExamQueue<Int>()
queue.enqueue(4)
queue.enqueue(8)
queue.enqueue(15)

queue.toList() == listOf(4, 8, 15)
queue.size == 3
queue.dequeue() == 4
```

## 6. Iterator

Method involved:

```kotlin
override fun iterator(): Iterator<T>
```

### Implementation Requirement

Return an iterator over the remaining queue contents from front to back.

The class declaration already says:

```kotlin
class ExamQueue<T> : Iterable<T>
```

so this method is what allows:

```kotlin
for (value in queue) {
    println(value)
}
```

### Restriction

The iterator must not remove values from the queue. It should expose the current order without changing `size`.

### Example

```kotlin
val queue = ExamQueue<Int>()
queue.enqueue(4)
queue.enqueue(8)
queue.enqueue(15)

queue.iterator().asSequence().toList() == listOf(4, 8, 15)
queue.size == 3
```

## Public Tests

The public tests check the following behaviours:

- a new queue reports empty state correctly;
- `peek` and `dequeue` return `null` for an empty queue;
- values are dequeued in first-in-first-out order;
- `toList` and `iterator` do not mutate the queue.

Passing the public tests is not a proof of full correctness. You should also think through:

- enqueuing after the queue has become empty again;
- dequeuing the only element;
- peeking repeatedly;
- iterating over an empty queue;
- confirming that `front`, `back`, and `size` stay consistent after every operation.
