# Question 2: Concurrent Block Text

Suggested value: 20 marks

Implement a thread-safe block-backed text object. This builds on the earlier block text/iterator question, but now every operation that reads or mutates the block structure must be lock-protected.

You should edit only:

```text
ConcurrentBlockText.kt
```

Use the public tests in:

```text
ConcurrentBlockTextTest.kt
```

## Background

The text should be represented as several `StringBuilder` blocks:

```text
["Alph", "a Be", "ta"] -> "Alpha Beta"
```

The object is shared between threads, so `append`, `insert`, `rebalance`, `length`, `charAt`, `blocks`, `toString`, and `iterator` must not observe half-updated state.

## 1. Representation and Locking

Methods involved:

```kotlin
class ConcurrentBlockText(private val blockSize: Int = DEFAULT_BLOCK_SIZE) : Iterable<Char>
```

### Implementation Requirement

Store the text as a private mutable list of `StringBuilder` blocks. Use the provided `ReentrantLock` to protect all access to the block list.

### Restriction

Do not represent the object as one master string. `rebalance()` may temporarily flatten the text, but the object should remain block-backed.

Reject non-positive block sizes.

## 2. `append` and `insert`

Methods involved:

```kotlin
fun append(text: String)
fun insert(offset: Int, text: String)
```

### Implementation Requirement

`append` adds text to the end. `insert` inserts text at a valid offset from `0` to `length`, inclusive.

Invalid offsets should throw `IndexOutOfBoundsException`.

### Restriction

Lock the whole operation. Checking the offset and performing the mutation must be one critical section.

## 3. `length` and `charAt`

Methods involved:

```kotlin
val length: Int
fun charAt(index: Int): Char
```

### Implementation Requirement

`length` returns the total number of characters across all blocks. `charAt` indexes across the whole represented text.

Invalid indexes should throw `IndexOutOfBoundsException`.

### Restriction

Do not treat the index as a block index.

## 4. `rebalance` and `blocks`

Methods involved:

```kotlin
fun rebalance()
fun blocks(): List<String>
```

### Implementation Requirement

`rebalance()` should rebuild blocks so every block has exactly `blockSize` characters except possibly the final block.

`blocks()` should return the current block contents as strings.

### Restriction

`blocks()` must not expose the internal `StringBuilder` objects.

## 5. `toString` and Iterator

Methods involved:

```kotlin
override fun toString(): String
override fun iterator(): Iterator<Char>
```

### Implementation Requirement

`toString()` returns the represented text. `iterator()` returns characters in order.

### Restriction

The iterator should use a stable snapshot. Do not keep the lock held while the caller iterates.

## Public Tests

The public tests check:

- insertion, rebalancing, and `charAt`;
- snapshot iterator stability;
- concurrent appends from several `Thread`s;
- invalid offsets and indexes.
