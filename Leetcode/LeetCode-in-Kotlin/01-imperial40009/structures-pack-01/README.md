# COMP40009 Retrofit Pack 01: Core Data Structures

Suggested total value: 100 marks

This pack is a synthetic practical paper in the style of the Java/Kotlin `40009` tests. It is not a normal LeetCode set. The questions are larger, use skeleton files, and are intended to be solved against public tests while you reason about hidden edge cases.

Source package:

- `src/main/kotlin/a40009_imperial/structurespack01`

Test package:

- `src/test/kotlin/a40009_imperial/structurespack01`

## Instructions

Open the normal `LeetCode-in-Kotlin` Gradle project in IntelliJ. Run one test class at a time using the green gutter button.

Suggested order:

1. `ExamLinkedListTest`
2. `ExamQueueTest`
3. `OrderedHashMapTest`
4. `AlphaTreeTest`
5. `BlockTextTest`

Each source folder has its own `README.md` written as the local question sheet for that task. Read that file before opening the skeleton.

The public tests are intentionally not exhaustive. Treat them like Imperial starter tests: passing them is evidence, not proof. You are expected to think about invalid inputs, empty structures, one-element structures, and representation invariants.

## Question Overview

### Question 1: Linked List

File: `ExamLinkedList.kt`

Implement a generic singly linked list from first principles.

Requirements:

- Store elements in your own linked nodes.
- Do not delegate storage to `MutableList`, `ArrayList`, or `LinkedList`.
- Support adding at the front and back.
- Support indexed access and indexed removal.
- Support removing the first occurrence of a value.
- Return elements in logical order via `toList` and `iterator`.
- Throw `IndexOutOfBoundsException` for invalid indexes.

### Question 2: Queue

File: `ExamQueue.kt`

Implement a generic first-in-first-out queue from first principles.

Requirements:

- `enqueue` adds at the back.
- `dequeue` removes from the front and returns `null` if empty.
- `peek` returns the front element without removing it.
- `size` and `isEmpty` must stay correct after every operation.
- The iterator must expose remaining queue contents from front to back without mutating the queue.

### Question 3: Ordered Hash Map

File: `OrderedHashMap.kt`

Implement a map that combines hash buckets with insertion order.

Requirements:

- Use buckets for key lookup.
- Maintain insertion order separately.
- `put` returns the old value if the key already existed, otherwise `null`.
- Updating an existing key must move that entry to the newest position.
- `removeOldest` removes and returns the oldest key-value pair.
- The iterator must return entries in insertion order.
- You may use arrays internally, but do not delegate the whole structure to `LinkedHashMap`.

### Question 4: Frequency Tree

File: `AlphaTree.kt`

Implement a binary frequency tree inspired by AlphaTree and Huffman-style papers.

Requirements:

- A singleton tree stores one character and its frequency.
- Combining two trees creates a parent whose frequency is the sum of child frequencies.
- `codeFor` returns the left/right path to a character, using `0` for left and `1` for right.
- `fromText` builds a tree from character frequencies in a string.
- Ties should be deterministic: for equal frequencies, prefer the character/tree that was seen later as the higher-priority item when building.

### Question 5: Iterator Over Blocks

File: `BlockText.kt`

Implement a text-file-like object stored as several blocks.

Requirements:

- Store content as multiple `StringBuilder` blocks.
- `append` appends text to the end.
- `insert` inserts text at a valid offset.
- `charAt` returns the character at an index.
- `rebalance` reorganises content into blocks of exactly `blockSize`, except possibly the final block.
- The iterator must walk characters lazily across blocks in order.

## Working Advice

For each question, first read the local question sheet, then inspect the tests, then implement the skeleton. Keep private helper methods small. Do not change public method names, parameter types, return types, or package declarations.
