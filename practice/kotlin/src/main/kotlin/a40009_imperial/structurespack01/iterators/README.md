# Question 5: Block Text Files And Iterators

Suggested value: 15 marks

This question is about implementing a small in-memory text file in Kotlin. It is based on the Kotlin Text Files sample paper, where one implementation stores text using several `StringBuilder` blocks rather than one large string.

You should edit:

```text
src/main/kotlin/a40009_imperial/structurespack01/iterators/BlockText.kt
```

Use the tests in:

```text
src/test/kotlin/a40009_imperial/structurespack01/iterators/BlockTextTest.kt
```

## Background

A plain `String` is immutable. If a text editor repeatedly inserts into the middle of one large string, it may need to rebuild a lot of text each time.

In this question, the text is represented as a sequence of smaller blocks:

```text
blocks: ["Alph", "a Be", "ta"]
text:   "Alpha Beta"
```

Each block should be stored as a `StringBuilder`, so that editing a block can be done without replacing the whole file.

The constructor parameter `blockSize` is the preferred block length used by `rebalance()`. It does not mean every operation must immediately keep all blocks perfectly balanced.

## 1. Representation and Constructor

Methods involved:

```kotlin
class BlockText(private val blockSize: Int = DEFAULT_BLOCK_SIZE) : Iterable<Char>
```

### Implementation Requirement

Store the file contents as a private list of `StringBuilder` blocks.

The constructor should accept only positive block sizes. A non-positive `blockSize` should be rejected immediately.

### Restriction

Do not store a single master string as the only real representation. The point of the question is to practise a block-based structure.

You may temporarily flatten the text inside helper methods, especially in `rebalance()`, but the object should still be represented by blocks afterwards.

### Example

```kotlin
val text = BlockText(blockSize = 4)
```

This creates an empty block text file whose preferred rebalance size is `4`.

Invalid construction should fail:

```kotlin
BlockText(blockSize = 0) // should throw IllegalArgumentException
```

## 2. `length`

Method involved:

```kotlin
val length: Int
```

### Implementation Requirement

Return the total number of characters stored across all blocks.

For example, if the internal blocks currently contain:

```kotlin
listOf("Alph", "a", "Beta")
```

then `length` should be `9`.

### Restriction

Do not return the number of blocks. The length is the number of characters in the whole represented text.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("Alpha")
text.append("Beta")

text.length == 9
```

## 3. `append`

Method involved:

```kotlin
fun append(text: String)
```

### Implementation Requirement

Append the supplied string to the end of the represented text.

The simplest acceptable implementation may add the appended text as a new block. It is not required that `append` immediately split or rebalance blocks.

Appending the empty string should leave the represented text unchanged.

### Restriction

Do not implement `append` by replacing the whole representation with one single master block every time.

### Example

```kotlin
val file = BlockText(blockSize = 4)

file.append("Alpha")
file.append("Beta")

file.toString() == "AlphaBeta"
file.length == 9
```

## 4. `insert`

Method involved:

```kotlin
fun insert(offset: Int, text: String)
```

### Implementation Requirement

Insert `text` at the given zero-based character offset.

Valid offsets are from `0` to `length`, inclusive.

- `offset == 0` means insert at the beginning.
- `offset == length` means insert at the end.
- an offset between them means insert into the middle of the represented text.

Invalid offsets should throw `IndexOutOfBoundsException`.

### Restriction

After insertion, the represented text must still be stored in the block structure.

It is acceptable for one block to become larger than `blockSize`; `rebalance()` is responsible for restoring predictable block sizes.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("AlphaBeta")

text.insert(5, " ")
text.toString() == "Alpha Beta"

text.insert(0, "[")
text.insert(text.length, "]")
text.toString() == "[Alpha Beta]"
```

Invalid offsets:

```kotlin
text.insert(-1, "x") // should throw IndexOutOfBoundsException
text.insert(text.length + 1, "x") // should throw IndexOutOfBoundsException
```

## 5. `charAt`

Method involved:

```kotlin
fun charAt(index: Int): Char
```

### Implementation Requirement

Return the character at the given zero-based index, where indexes are counted across the whole file, not within a single block.

If the block representation is:

```kotlin
listOf("Alph", "a Be", "ta")
```

then index `0` is `'A'`, index `4` is `'a'`, and index `6` is `'B'`.

Invalid indexes should throw `IndexOutOfBoundsException`.

### Restriction

Do not treat the index as a block index. You need to walk across block lengths until you find the block containing the requested character.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("AlphaBeta")

text.charAt(0) == 'A'
text.charAt(5) == 'B'
```

Invalid indexes:

```kotlin
text.charAt(-1) // should throw IndexOutOfBoundsException
text.charAt(text.length) // should throw IndexOutOfBoundsException
```

## 6. `rebalance`

Method involved:

```kotlin
fun rebalance()
```

### Implementation Requirement

Rebuild the block representation so that every block has exactly `blockSize` characters, except that the final block may be shorter.

Rebalancing must not change the represented text.

### Restriction

`rebalance()` may temporarily flatten the text to rebuild the blocks. However, after the method finishes, the object should again be represented as blocks.

Rebalancing an empty file should be valid and should leave the file empty.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("Alpha Beta")

text.rebalance()

text.blocks() == listOf("Alph", "a Be", "ta")
text.toString() == "Alpha Beta"
```

## 7. `blocks`

Method involved:

```kotlin
fun blocks(): List<String>
```

### Implementation Requirement

Return the current block contents as strings, in order.

This method is mainly for testing and inspection of your representation.

### Restriction

Return a new list of strings. A caller should not be able to mutate your internal `StringBuilder` objects through the returned value.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("Alpha Beta")
text.rebalance()

text.blocks() == listOf("Alph", "a Be", "ta")
```

## 8. `toString`

Method involved:

```kotlin
override fun toString(): String
```

### Implementation Requirement

Return the full represented text by concatenating every block in order.

### Restriction

`toString()` should report the observable file contents, not a debugging description of the block list.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("Alpha")
text.append("Beta")

text.toString() == "AlphaBeta"
```

## 9. Iterator

Method involved:

```kotlin
override fun iterator(): Iterator<Char>
```

### Implementation Requirement

Return an iterator over every character in the file from beginning to end.

The class declaration already says:

```kotlin
class BlockText(...) : Iterable<Char>
```

so this method is what allows:

```kotlin
for (char in text) {
    print(char)
}
```

### Restriction

The iterator should walk across the block structure. It should not first create a list containing every character in the file.

The iterator should correctly cross block boundaries.

### Example

```kotlin
val text = BlockText(blockSize = 4)
text.append("Alpha")
text.append("Beta")

text.iterator().asSequence().joinToString(separator = "") == "AlphaBeta"
```

## Public Tests

The public tests check the following behaviours:

- appending and reading characters across block boundaries;
- insertion at the beginning, middle, and end;
- rebalancing into predictable block sizes;
- invalid indexes and offsets;
- iteration from the first character to the final character.

Passing the public tests is not a proof of full correctness. You should also think through:

- inserting into an empty file;
- appending the empty string;
- inserting the empty string;
- calling `charAt` on the first and last valid indexes;
- rebalancing an empty file;
- iterating over empty, one-block, and multi-block files.
