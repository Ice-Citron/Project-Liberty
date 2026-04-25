package a40009_imperial.structurespack01.queue

class ExamQueue<T> : Iterable<T> {
    val size: Int
        get() = TODO("practice")

    val isEmpty: Boolean
        get() = size == 0

    fun enqueue(value: T) {
        TODO("practice")
    }

    fun dequeue(): T? {
        TODO("practice")
    }

    fun peek(): T? {
        TODO("practice")
    }

    fun toList(): List<T> {
        TODO("practice")
    }

    override fun iterator(): Iterator<T> {
        TODO("practice")
    }
}
