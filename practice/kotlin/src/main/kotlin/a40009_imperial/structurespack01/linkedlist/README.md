# Question 1: Linked Lists

Suggested value: 20 marks

This question is about implementing a generic singly linked list from first principles. It is intentionally closer to an Imperial practical exam question than to a short LeetCode prompt: you are given a skeleton, public tests, and observable behaviour to satisfy.

You should edit:

```text
src/main/kotlin/a40009_imperial/structurespack01/linkedlist/ExamLinkedList.kt
```

Use the tests in:

```text
src/test/kotlin/a40009_imperial/structurespack01/linkedlist/ExamLinkedListTest.kt
```

## Background

A singly linked list is a sequence of nodes. Each node stores one value and a reference to the next node.

```text
head -> node("A") -> node("B") -> node("C") -> null
```

For efficient insertion at both ends, a good representation usually stores:

```text
head: first node
tail: final node
size: number of values
```

Do not solve this by delegating the storage to Kotlin's `MutableList` or Java's `LinkedList`.

## 1. Representation and Size

Methods involved:

```kotlin
val size: Int
val isEmpty: Boolean
```

### Implementation Requirement

Choose private state that can represent an empty or non-empty linked list. A typical solution uses:

- a private node class;
- a `head` reference;
- a `tail` reference;
- an integer counter for the number of elements.

The `size` property should return the current number of elements. The `isEmpty` property is already written in terms of `size`.

### Restriction

Do not compute `size` by walking the whole list every time. Keep a counter and update it when values are added or removed.

Do not store the list contents in `MutableList`, `ArrayList`, Java `LinkedList`, or any other library collection.

### Example

For a new list:

```kotlin
val list = ExamLinkedList<String>()

list.size == 0
list.isEmpty == true
```

After adding two values:

```kotlin
list.addLast("A")
list.addLast("B")

list.size == 2
list.isEmpty == false
```

## 2. `addFirst`

Method involved:

```kotlin
fun addFirst(value: T)
```

### Implementation Requirement

Insert `value` at the front of the list.

This method must handle both cases:

- the list is empty;
- the list already contains one or more nodes.

After insertion, the new value should be the first value returned by `get(0)`, `toList()`, and the iterator.

### Restriction

When adding to an empty list, update both `head` and `tail` consistently. A one-element list should have both references pointing at the same node.

### Example

```kotlin
val list = ExamLinkedList<String>()
list.addFirst("B")
list.addFirst("A")

list.toList() == listOf("A", "B")
list.size == 2
```

## 3. `addLast`

Method involved:

```kotlin
fun addLast(value: T)
```

### Implementation Requirement

Insert `value` at the back of the list.

This method must handle both cases:

- the list is empty;
- the list already contains one or more nodes.

### Restriction

Do not find the end by walking from `head` every time if you maintain a `tail` reference. The point of keeping `tail` is to make adding at the back direct.

### Example

```kotlin
val list = ExamLinkedList<String>()
list.addLast("A")
list.addLast("B")
list.addLast("C")

list.toList() == listOf("A", "B", "C")
```

## 4. `get`

Method involved:

```kotlin
fun get(index: Int): T
```

### Implementation Requirement

Return the value stored at the given zero-based index.

Valid indexes are from `0` to `size - 1`.

Invalid indexes should throw `IndexOutOfBoundsException`.

### Restriction

Do not remove or rearrange nodes while performing indexed access.

### Example

```kotlin
val list = ExamLinkedList<Int>()
list.addLast(10)
list.addLast(20)
list.addLast(30)

list.get(0) == 10
list.get(2) == 30
```

Invalid indexes:

```kotlin
list.get(-1) // should throw IndexOutOfBoundsException
list.get(list.size) // should throw IndexOutOfBoundsException
```

## 5. `removeAt`

Method involved:

```kotlin
fun removeAt(index: Int): T
```

### Implementation Requirement

Remove and return the value at the given zero-based index.

The method must update all relevant list state, including:

- `head`;
- `tail`;
- `size`;
- the `next` link of the previous node when removing from the middle.

Invalid indexes should throw `IndexOutOfBoundsException`.

### Restriction

Removing the node from the linked chain alone is not enough if `head`, `tail`, or `size` become stale.

### Example

```kotlin
val list = ExamLinkedList<Int>()
list.addLast(10)
list.addLast(20)
list.addLast(30)
list.addLast(40)

list.removeAt(1) == 20
list.toList() == listOf(10, 30, 40)

list.removeAt(2) == 40
list.toList() == listOf(10, 30)
```

Your code should also handle removing the head, removing the tail, and removing the only element.

## 6. `remove`

Method involved:

```kotlin
fun remove(value: T): Boolean
```

### Implementation Requirement

Remove the first occurrence of `value` from the list.

Return `true` if a node was removed. Return `false` if the value was not present.

Equality should use Kotlin's normal equality operator, `==`.

### Restriction

Do not remove every matching value. Only the first matching node should be removed.

The same representation invariants as `removeAt` must still be maintained.

### Example

```kotlin
val list = ExamLinkedList<String>()
list.addLast("red")
list.addLast("blue")
list.addLast("red")

list.remove("red") == true
list.toList() == listOf("blue", "red")

list.remove("green") == false
list.toList() == listOf("blue", "red")
```

## 7. Ordered Views

Methods involved:

```kotlin
fun toList(): List<T>
override fun toString(): String
```

### Implementation Requirement

`toList()` should return the current list contents in logical order from head to tail.

`toString()` should use normal list formatting, for example:

```text
[A, B, C]
```

### Restriction

`toList()` should return a new list. A caller should not be able to mutate your linked structure by changing the returned list.

### Example

```kotlin
val list = ExamLinkedList<String>()
list.addLast("A")
list.addLast("B")
list.addLast("C")

list.toList() == listOf("A", "B", "C")
list.toString() == "[A, B, C]"
```

## 8. Iterator

Method involved:

```kotlin
override fun iterator(): Iterator<T>
```

### Implementation Requirement

Return an iterator over the list values from head to tail.

The class declaration already says:

```kotlin
class ExamLinkedList<T> : Iterable<T>
```

so this method is what allows:

```kotlin
for (value in list) {
    println(value)
}
```

### Restriction

The iterator should walk the linked structure. It should not rely on a stored Kotlin list as the real representation.

Iterating should not change the list.

### Example

```kotlin
val list = ExamLinkedList<Int>()
list.addLast(1)
list.addLast(2)
list.addLast(3)

list.iterator().asSequence().toList() == listOf(1, 2, 3)
list.size == 3
```

## Public Tests

The public tests check the following behaviours:

- adding at both ends;
- size and emptiness;
- indexed access and indexed removal;
- removing only the first matching value;
- converting to a list and string;
- iteration in logical order;
- invalid index exceptions.

Passing the public tests is not a proof of full correctness. You should also think through:

- removing from an empty list;
- removing the only element;
- removing the head;
- removing the tail;
- removing a value that appears more than once;
- adding after the list has become empty again;
- iterating over an empty list.
