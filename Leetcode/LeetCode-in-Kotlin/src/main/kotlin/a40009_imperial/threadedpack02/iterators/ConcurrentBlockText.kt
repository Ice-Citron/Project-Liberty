package a40009_imperial.threadedpack02.iterators

import java.util.concurrent.locks.ReentrantLock

class ConcurrentBlockText(private val blockSize: Int = DEFAULT_BLOCK_SIZE) : Iterable<Char> {
    init {
        require(blockSize > 0) { "blockSize must be positive" }
    }

    val length: Int
        get() = TODO("practice")

    fun append(text: String) {
        TODO("practice")
    }

    fun insert(offset: Int, text: String) {
        TODO("practice")
    }

    fun charAt(index: Int): Char {
        TODO("practice")
    }

    fun rebalance() {
        TODO("practice")
    }

    fun blocks(): List<String> {
        TODO("practice")
    }

    override fun iterator(): Iterator<Char> {
        TODO("practice")
    }

    override fun toString(): String {
        TODO("practice")
    }

    @Suppress("unused")
    private val lock = ReentrantLock()

    companion object {
        private const val DEFAULT_BLOCK_SIZE = 8
    }
}
