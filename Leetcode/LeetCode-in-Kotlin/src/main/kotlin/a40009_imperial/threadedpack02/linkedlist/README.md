# Question 1: Locked Linked List

Suggested value: 20 marks

Implement a generic linked list whose public operations are safe to call from several threads.

You should edit only:

```text
LockedLinkedList.kt
```

Use the public tests in:

```text
LockedLinkedListTest.kt
```

## Background

This question builds on the earlier linked-list question. The representation is still a linked list, but now all compound access to the structure must be protected by a `ReentrantLock`.

The intended shape is:

```text
ReentrantLock
head -> node -> node -> null
tail
size
```

## 1. Representation and Locking

Methods involved:

```kotlin
val size: Int
val isEmpty: Boolean
```

### Implementation Requirement

Use your own private linked nodes. Maintain a head, tail, and size counter. Protect reads and writes of shared state with the provided `ReentrantLock`.

### Restriction

Do not use `Collections.synchronizedList`, `ConcurrentLinkedQueue`, `MutableList`, or Java `LinkedList` as the real storage.

Use explicit locking. The expected pattern is:

```kotlin
lock.lock()
try {
    // read or mutate shared state
} finally {
    lock.unlock()
}
```

## 2. `addLast`

Method involved:

```kotlin
fun addLast(value: T)
```

### Implementation Requirement

Append `value` to the end of the list. Correctly handle both the empty and non-empty cases.

### Restriction

The entire pointer update must be inside the critical section. Do not update `tail`, `head`, and `size` outside the lock.

### Example

```kotlin
val list = LockedLinkedList<String>()
list.addLast("raw")
list.addLast("goods")

list.snapshot() == listOf("raw", "goods")
```

## 3. `removeFirst`

Method involved:

```kotlin
fun removeFirst(): T?
```

### Implementation Requirement

Remove and return the first value in the list. Return `null` if the list is empty.

### Restriction

If the final element is removed, both `head` and `tail` must become empty references.

### Example

```kotlin
list.removeFirst() == "raw"
list.removeFirst() == "goods"
list.removeFirst() == null
```

## 4. `snapshot`

Method involved:

```kotlin
fun snapshot(): List<T>
```

### Implementation Requirement

Return the current contents from head to tail.

### Restriction

The returned list must be a new list. It must not expose your internal nodes.

Take the snapshot while holding the lock, then return it after the structure has been copied.

## 5. Iterator

Method involved:

```kotlin
override fun iterator(): Iterator<T>
```

### Implementation Requirement

Return an iterator over a stable snapshot of the list.

### Restriction

Do not keep the lock held while the caller iterates. External iteration can be slow or never finish. Instead, copy the contents under the lock and return an iterator over the copy.

## Public Tests

The public tests check:

- append/remove behaviour;
- snapshot order;
- snapshot iterator stability;
- concurrent additions from several `Thread`s.

Think also about:

- removing from an empty list;
- adding after the list became empty;
- lock release when exceptions occur;
- whether every shared-state read is protected.
