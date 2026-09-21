package a40009_imperial.structurespack01.iterators;

public class CompleteTheBlanks {
}





class ChunkedReplacer {
    private val chunks: MutableList<StringBuilder> = mutableListOf(
        StringBuilder("Hell"),
        StringBuilder("x Wo"),
        StringBuilder("rld")
    )

    private val totalLength: Int
        get() = chunks.sumOf { it.length }

    private data class Position(
        val chunkIndex: Int,
        val localIndex: Int
    )

    fun setCharAt(index: Int, replacement: Char) {
        check(index in 0..<totalLength) {
            "index $index out of bounds for length $totalLength"
        }

        val position = locateChar(index)

        chunks[position.chunkIndex].setCharAt(position.localIndex, replacement)
    }

    private fun locateChar(index: Int): Position {
        check(index in 0 until totalLength)

        var remaining = index

        for ((chunkIndex, chunk) in chunks.withIndex()) {
            if (remaining < chunk.length) {
                return Position(chunkIndex, remaining)
            }

            remaining -= chunk.length
        }

        error("Valid index could not be located")
    }

    override fun toString(): String = buildString {
        for (chunk in chunks) append(chunk)
    }
}



















class ChunkedBuffer {
    private val chunks: MutableList<StringBuilder> = mutableListOf()
    private var totalLength: Int = 0

    private data class Position(
        val chunkIndex: Int,
        val localIndex: Int
    )

    val length: Int
        get() = totalLength

    fun append(text: String) {
        if (text.isEmpty()) return
        chunks.add(StringBuilder(text))
        totalLength += text.length
    }

    fun insert(offset: Int, text: String) {
        check(offset in 0..totalLength) {
            "offset $offset out of bounds for length $totalLength"
        }

        if (text.isEmpty()) return

        if (chunks.isEmpty() || offset == totalLength) {
            chunks.add(StringBuilder(text))
            totalLength += text.length
            return
        }

        val position = locateChar(offset)
        chunks[position.chunkIndex].insert(position.localIndex, text)
        totalLength += text.length
    }

    private fun locateChar(index: Int): Position {
        check(index in 0..<length) {
            "index $index out of bounds for length $totalLength"
        }

        var remaining = index

        for ((chunkIndex, chunk) in chunks.withIndex()) {
            if (remaining < chunk.length) {
                return Position(chunkIndex, remaining)
            }
            remaining -= chunk.length
        }

        error("Valid index could not be located")
    }

    override fun toString(): String = buildString {
        for (chunk in chunks) append(chunk)
    }
}

class ChunkedDeleter {
    private val chunks: MutableList<StringBuilder> = mutableListOf()
    private var totalLength: Int = 0

    private data class Position(
        val chunkIndex: Int,
        val localIndex: Int
    )

    fun append(text: String) {
        if (text.isEmpty()) return
        chunks.add(StringBuilder(text))
        totalLength += text.length
    }

    fun deleteAt(index: Int) {
        check(index in 0..<totalLength) {
            "index $index out of bounds for length $totalLength"
        }

        val position = locateChar(index)

        chunks[position.chunkIndex].deleteCharAt(position.localIndex)
        totalLength--

        if (chunks[position.chunkIndex].isEmpty()) {
            chunks.removeAt(position.chunkIndex)
        }
    }

    private fun locateChar(index: Int): Position {
        check(index in 0 until totalLength)

        var remaining = index

        for ((chunkIndex, chunk) in chunks.withIndex()) {
            if (remaining < chunk.length) {
                return Position(chunkIndex, remaining)
            }
            remaining -= chunk.length
        }

        error("Valid index could not be located")
    }

    override fun toString(): String = buildString {
        for (chunk in chunks) append(chunk)
    }
}


/*
    After `deleteCharAt(position.localIndex)`, it is possible that the block you
    deleted from has no characters left. For example, if `chunks = ["A", "Beta"]`
    and you call `deleteAt(0)`, the first block becomes `""`, so the internal
    representation becomes `["", "Beta"]`. That empty block is useless and can
    make later travesal slightly messier, so this code says: "if the block is
    now empty, remvoe the whole block from the list."
    `removeAt(position.chunkIndex)` removes the `StringBuilder` object at that
    block index, not anotjher cahracter.

    `isEmpty()` means that the string has length `0`, so `""` is empty.
    `isBlank()` means the string contains no visible/non-whitespace characters,
    so it is either empty or only spaces/tabs/newlines. Example: "" is both empty
    and blank, " " is NOT EMPTY but is blank; `"\n\t "` is blank, " a " is not
    blank because it contains `a`. For a ... `isEmpty()` when deciding whether
    a block has zero characters left; do not use `isBlank()`, because a block
    containing spaces is still real text.




* */






/*
                // character access/delete/replace
                index in 0..<length

                // insertion offset
                offset in 0..length
* */












class ChunkedString {
    private val chunks: MutableList<StringBuilder> = mutableListOf(
        StringBuilder("Alph"),
        StringBuilder("a Be"),
        StringBuilder("ta")
    )

    private val totalLength: Int
        get() = chunks.sumOf { it.length }

    private data class Position(
        val chunkIndex: Int,
        val localIndex: Int
    )

    fun charAt(index: Int): Char {
        val position: Position = locate(index)
        return chunks[position.chunkIndex][position.localIndex]
    }

    private fun locate(index: Int): Position {
        check(index in 0..<totalLength) {
            "index $index out of bounds for length $totalLength"
        }

        var remaining = index

        for ((chunkIndex, chunk) in chunks.withIndex()) {
            if (remaining < chunk.length) {
                return Position(chunkIndex, remaining)
            }

            remaining -= chunk.length
        }

        error("Valid index could not be located")
    }
}