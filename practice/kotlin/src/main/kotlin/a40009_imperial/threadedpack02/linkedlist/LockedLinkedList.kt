package a40009_imperial.threadedpack02.linkedlist

import java.util.concurrent.locks.ReentrantLock

class LockedLinkedList<T> : Iterable<T> {
    val size: Int
        get() = TODO("practice")

    val isEmpty: Boolean
        get() = size == 0

    fun addLast(value: T) {
        TODO("practice")
    }

    fun removeFirst(): T? {
        TODO("practice")
    }

    fun snapshot(): List<T> {
        TODO("practice")
    }

    override fun iterator(): Iterator<T> {
        TODO("practice")
    }

    private class Node<T>(
        val value: T,
        var next: Node<T>? = null,
    )

    @Suppress("unused")
    private val lock = ReentrantLock()
}
