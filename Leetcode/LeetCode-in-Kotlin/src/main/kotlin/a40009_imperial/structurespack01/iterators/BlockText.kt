package a40009_imperial.structurespack01.iterators

class BlockText(private val blockSize: Int = DEFAULT_BLOCK_SIZE) : Iterable<Char> {

    companion object {
        private const val DEFAULT_BLOCK_SIZE = 8
    }

    private val blockList: MutableList<StringBuilder> = mutableListOf()
    private var totalLength: Int = 0

    init {
        require(blockSize > 0) { "blockSize must be positive" }
    }

    val length: Int
        get() = totalLength

    fun append(text: String) {
        if (text.isEmpty()) return
        blockList.add(StringBuilder(text))
        totalLength += text.length
    }

    fun insert(offset: Int, text: String) {
        check(offset in 0..totalLength) {
            "Insert offset $offset out of bounds for length $totalLength"
        }

        if (text.isEmpty()) return

        if (blockList.isEmpty()) {
            blockList.add(StringBuilder(text))
            totalLength += text.length
            return
        }

        if (offset == totalLength) {
            blockList.add(StringBuilder(text))
            totalLength += text.length
            return
        }

        val position = locateChar(offset)
        blockList[position.blockIndex].insert(position.localIndex, text)
        totalLength += text.length
    }

    private fun locateChar(index: Int): BlockPosition {
        check(index in 0..<totalLength) {
            "Character index $index out of bounds for length $totalLength"
        }

        var remaining = index
        for ((blockIndex, block) in blockList.withIndex()) {
            if (remaining < block.length) {
                return BlockPosition(blockIndex, remaining)
            }
            remaining -= block.length
        }
        error("Valid index $index cannot be located; block representation is inconsistent")
    }

    fun charAt(index: Int): Char {
        val position: BlockPosition = locateChar(index)
        return blockList[position.blockIndex][position.localIndex]
    }

    fun rebalance() {
        val flattened = toString()

        check(flattened.length == totalLength) {
            "Internal length invariant broken: totalLength=$totalLength, actual=${flattened.length}"
        }

        blockList.clear()

        var start = 0
        while (start < flattened.length) {
            val end = minOf(start + blockSize, flattened.length)
            blockList.add(StringBuilder(flattened.substring(start, end)))
            start = end
        }
    }

    fun blocks(): List<String> = blockList.map { it.toString() }

    override fun iterator(): Iterator<Char> {
        return object : Iterator<Char> {
            private var blockIndex: Int = 0
            private var localIndex: Int = 0

            // var charsYielded: Int = 0
            // override fun hasNext(): Boolean = charsYielded < totalLength

            override fun hasNext(): Boolean {
                if (blockList.size == 0) return false
                if (localIndex < blockList[blockIndex].length) return true

                if (localIndex + 1 >= blockList[blockIndex].length) {
                    var tempBlockIndex = blockIndex + 1
                    while (tempBlockIndex < blockList.size) {
                        if(blockList[tempBlockIndex].isNotEmpty()) return true
                        tempBlockIndex++
                    }
                }
                return false
            }

            override fun next(): Char {
                check(hasNext()) {
                    "No characters left to iterate in BlockList."
                }

                val charToReturn = blockList[blockIndex][localIndex]
                localIndex++

                if (localIndex  >= blockList[blockIndex].length) {
                    blockIndex++
                    localIndex = 0
                    while (blockIndex < blockList.size && blockList[blockIndex].isEmpty()) {
                        blockIndex++
                    }
                }
                return charToReturn
            }
        }
    }

    override fun toString(): String = buildString {
        for (block in blockList) append(block)
    }

    private data class BlockPosition(
        val blockIndex: Int,
        val localIndex: Int,
    )
}

/*
1. THE MEMORY TRADE-OFF of `rebalance(0`
    The `rebalance()` function is essentially a garbage collection and
    defragmentation phase for your text blocks.






















* */
















/*
    This is exactly what the `for` loop does:

```
var remaining = index
for ((blockIndex, block) in blockList.withIndex()) {
    // If the remaining index is smaller than the block's size, the target is inside this block!
    if (remaining < block.length) {
        return BlockPosition(blockIndex, remaining)
    }

    // Otherwise, subtract the block's size and check the next block.
    remaining -= block.length
}
```
    This reduces the time complexity from O(n) (where n is total characters)
    down to O(b) (where b is the number of blocks).


---
3. The `insert` Edge Cases
    The `insert` function uses `locateChar` to find out where to drop the next
    text. But it also has two critical edge cases that `locateChar` cannot handle.

    - EDGE CASE 1: Empty File (`blockList.isEmpty()`)
        If the file is empty, there are no blocks to search for. If you called
        `locateChar(0)` it would crash. So we catch it early and just add a new
        block.
    - EDGE CASE 2: Appending to the End (`offset == totalLength`).
        If the total length is 10, valid global indices are 0 through 0. If a
        user `insert(10, "X")`, they want to put it at the very end. But
        `locateChar(10)` will crash because there is no character at index 10 to
        locate! So, we catch it early and just add a new block to the end.
                        <-- Ah huh, thinking about cases it seems...

    If it's not one of those edge cases, we know it's a valid middle insertion.
    We call `locateChar`, find the exact block and the exact local index, and
    tell that specific `StringBuilder` to insert the new text.



    For `offset == length`, creating a new block is not because it is the only
    correct way; it is because it is the SIMPLEST BLOCK-PRESERVING APPEND
    STRATEGY. We could instead of `blockList.last().append(text)`, and that would
    also be valid. The tradeoff is that appending to the last block can make one
    block grow very large unless you split it, while adding a new block keeps each
    append operation isolated and lets `rebalance()` later restore nice fixed-size
    blocks. So this design says: "editing ooperations may leave messy block sizes;
    `rebalance()` cleans them up later."

    A `StringBuilder` object is fairly cheap, but not free. On the JVM, each
    `StringBuilder` has object overhead plus an internal character buffer/array,
    plus one reference inside the `MutableList`. Roughly, think TENS OF BYTES
    of overhead per block, before the actual text contents. Thousands of blocks are
    usually totally fine. Millions of tiny blocks would become bad because pointer
    chasing, GC overhead, and list traversal would dominate. So yes: if you
    accidentally had one block per 10 words, that is not catastrophic for
    eaxm-sized or normal small text-editor eaxmples. But for a serious editor,
    you would want blocks to be reasonably chunky, maybe hundreds or thousands of
    characters, not tiny fragments.










* */


















/*
    `MutableList` is the general-purpose "ordered collection with indexes"
    abstraction: use it when you care about `list[i]`, iteratioin order, replacing
    items, mapping/filtering, or storing a normal sequence of things. In Kotlin,
    `mutableListOf()` is usually backed by an array list, so `add(...)` at the
    end and indexed access are efficient, but removing from the front with
    `removeAt(0)` is bad because everything after is has to shift left.

    `ArrayDeque` is more specialised: it is for STACK/QUEUE/DEQUE behaviour,
    where you mainly add/remove from the ends. Use `addlast`/`removeLast` for a
    stack, and `addLast`/`removeFirst` for a queue. That is why it feels nice for
    parsing, BFS, undo stacks, bracket matching, nested-string decoding, etc.
    For exam code, use `MutableList` when the object is conceptually a list;
    use `ArrayDeque` when the object is conceptually a stack/deque.
















WHY `append` ADDS A NEW BLOCK
    This is the simplest correct block-based implementation:
    ```
    blockList.add(StringBuilder(text))
    ```

    It avoids rebuilding the whole file. If the current file has 1,000,000 chars
    and you append... this implementation only copies "abc" into a new
    `StringBuilder`. It does NOT flatten and reconstruct the entire text.



-------

    This satisfies the question because the file is genuinely stored as MULTIPLE
    MUTABLE BLOCKS, not as one hidden master `String`. Each block is a
    `StringBuilder`, so later operations like `insert` can mutate a block
    directly. The name `blockList` is deliberately not just `blocks`, because
    later you need a public method called `blocks(): List<String>` for testing.

    The constructor uses:
    ```
    require(blockSize > 0)
    ```
    because `blockSize = 0` or negative makes `rebalance()` meaningless. This is
    an external caller error, so `require` is the correct Kotlin choice.


---
    You could write:
    ```
    var length: Int
        get() = blockList.sumOf { it.length }
    ```

    That is correct, but it is `O(number of blocks)` every time. The cleaner
    version stores a cached invariant:

    ...
* */










