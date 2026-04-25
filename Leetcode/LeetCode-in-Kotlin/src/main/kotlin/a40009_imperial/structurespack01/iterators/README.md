# Question 5: Block Text Files And Iterators

Suggested value: 15 marks

## Problem Description

In this question you will implement an in-memory text file backed by several blocks. This is based on the Kotlin Text Files sample paper, where one implementation stores text using multiple `StringBuilder` objects.

The representation is intended to support editing without requiring every operation to rebuild one very large string. A file is split into a sequence of blocks. Each block stores part of the text.

You are given a skeleton class:

- `BlockText`

## Getting Started

You should edit only:

- `BlockText.kt`

You should use the public tests in:

- `BlockTextTest.kt`

## Required Behaviour

1. Basic representation. [2 marks]
   Store the text as a list of `StringBuilder` blocks. The constructor parameter `blockSize` gives the preferred block size used by `rebalance`.

2. Appending text. [2 marks]
   Implement `append(text: String)`.

   The simplest acceptable behaviour is to add the appended text at the end of the file. It is not required that `append` immediately rebalance blocks.

3. Inserting text. [3 marks]
   Implement `insert(offset: Int, text: String)`.

   Valid offsets are from `0` to `length`, inclusive. Inserting at offset `0` inserts at the beginning. Inserting at offset `length` appends at the end. Invalid offsets should throw `IndexOutOfBoundsException`.

4. Character access and length. [3 marks]
   Implement `length` and `charAt(index)`.

   `length` should return the total number of characters across all blocks. `charAt(index)` should return the character at the given zero-based index, where indexes are counted across the whole file. Invalid indexes should throw `IndexOutOfBoundsException`.

5. Rebalancing. [2 marks]
   Implement `rebalance()`.

   After rebalancing, the text should be stored in blocks of exactly `blockSize`, except that the final block may be shorter. Rebalancing must not change the text represented by the file.

6. Blocks, string conversion, and iteration. [3 marks]
   Implement `blocks()`, `toString()`, and `iterator()`.

   `blocks()` should return the current block contents as strings. `toString()` should concatenate every block in order. `iterator()` should return every character in the file from beginning to end.

## Restrictions

- Do not store a single master string as the only representation.
- `iterator()` should walk across the block structure. It should not first create a list containing every character.
- `rebalance()` may temporarily flatten the file to rebuild the blocks.
- The constructor should reject non-positive block sizes.

## Example

Suppose `blockSize == 4` and the represented text is:

```text
Alpha Beta
```

After calling `rebalance()`, a valid block representation is:

```kotlin
listOf("Alph", "a Be", "ta")
```

The string representation should still be:

```text
Alpha Beta
```

## Edge Cases To Think About

- Inserting into an empty file.
- Inserting at the start.
- Inserting at the end.
- Calling `charAt` on the first and last valid positions.
- Rebalancing an empty file.
- Iterating across block boundaries.
