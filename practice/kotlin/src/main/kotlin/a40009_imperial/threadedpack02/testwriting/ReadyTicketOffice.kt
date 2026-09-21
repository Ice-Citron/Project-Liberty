package a40009_imperial.threadedpack02.testwriting

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

data class TicketSnapshot(
    val remaining: Int,
    val sold: Int,
)

class ReadyTicketOffice(initialTickets: Int) {
    private val lock = ReentrantLock()
    private var remainingTickets: Int = initialTickets
    private var soldTickets: Int = 0

    init {
        require(initialTickets >= 0) { "initialTickets must not be negative" }
    }

    val remaining: Int
        get() = lock.withLock { remainingTickets }

    val sold: Int
        get() = lock.withLock { soldTickets }

    fun sell(requested: Int): Int {
        require(requested > 0) { "requested must be positive" }
        return lock.withLock {
            val soldNow = minOf(requested, remainingTickets)
            remainingTickets -= soldNow
            soldTickets += soldNow
            soldNow
        }
    }

    fun refund(count: Int) {
        require(count > 0) { "count must be positive" }
        lock.withLock {
            require(count <= soldTickets) { "cannot refund more tickets than have been sold" }
            soldTickets -= count
            remainingTickets += count
        }
    }

    fun snapshot(): TicketSnapshot {
        return lock.withLock {
            TicketSnapshot(
                remaining = remainingTickets,
                sold = soldTickets,
            )
        }
    }
}
