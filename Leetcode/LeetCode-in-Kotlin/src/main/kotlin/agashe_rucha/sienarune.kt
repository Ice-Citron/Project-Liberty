package agashe_rucha

import junit.framework.TestCase.assertEquals
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock
import kotlin.math.abs

import org.junit.Test
import kotlin.test.assertFailsWith


class SingleInterpreter {
    private val memory = mutableMapOf<String, Int>()

    fun assign(variable: String, value: Int) {
        memory[variable] = value
    }

    fun lookup(variable: String): Int {
        return memory[variable] ?:
        throw NoSuchElementException("Variable: $variable not found.")
    }

    fun divide(var1: String, var2: String): Int {
        val num1 = lookup(var1)
        val num2 = lookup(var2)
        if (num2 == 0) throw ArithmeticException("Division by zero")
        return num1 / num2
    }
}


class SingleInterpreterTests {
    private lateinit var interpreter: SingleInterpreter

    @BeforeEach
    fun setup() {
        interpreter = SingleInterpreter().apply {
            assign("x", 50)
            assign("x", 100)
            assign("y", 30)
        }
    }

    @Test
    fun `Testing if interpreter's lookup function returns correctly`() {
        assertEquals(100, interpreter.lookup("x"))
        assertEquals(30, interpreter.lookup("y"))
        assertFailsWith<NoSuchElementException>(interpreter.lookup("y"))
    }
}









/*
    This snippet is a classic Kotlin fail-fast pattern utilising the ELVIS
    OPERATOR (`?:`). It attempts to fetch `variable` from `memory` using the
    bracket operator (`get()`). If the lookup fins a value, it returns it. If
    the lookup returns `null`, the Elvis operator catches the `null` and executes
    the right side, throwing the exception. It is essentially a concise way to
    say, "Give me this value, and if it..."

    ... tailored for MAP and HASHMAP... In kotlin, `map[key]` natively returns
    a nullable type V?... so a missing key gracefully returns `null`, perfectly
    triggering the Elvis operator... For index-based ADTs... asking for an
    out-of-bounds index ... array[99]... doesn't return null; it immediately
    crashes with an `IndexOutOfBoundsException`... before the Elvis
    operator even gets a chance to look at it. ... To use the safe fallback pattern

    return array.getOrNull(index) ?: throw NoSuchElementException("Element at index $index not found.")
* */






class sienarune {
}


class BankAccount1(
    var balance: Int = 0,
    val initialBalance: Int = 0,
) {
    fun withdraw(amount: Int) {
        check(amount < balance) {
            "Error! Cannot withdraw more than available balance."
        }
        balance -= amount
    }
}


class BankAccountTests {
    // lateinit lets use bypass nullable `BankAccount?` types.
    private lateinit var account: BankAccount1

    @BeforeEach
    fun setup() {
        account = BankAccount1(initialBalance = 100)
    }

    @Test
    fun `withdrawing valid amount reduces balance correctly`() {
        account.withdraw(40)

        assertEquals(60, account.balance)   // Assert
    }

    @Test
    fun `withdrawing more than balance throws exception`() {
        // Act & Assert combined nicely into a Kotlin lambda
        assertFailsWith<IllegalArgumentException> {
            account.withdraw(500)
        }
    }
}









/*

    When writing JUnit tests for Kotlin and Java interop--... standardising your
    syntax and understanding exactly how the test runner interacts with your
    code is critical.

    Here is the formal breakdown of JUnit 5 syntax, using strict technical
    terminology.


---
1. The Anatomy of a JUnit Test
    A standard JUnit test follows the AAA pattern (Arrange, Act, Assert). You
    set up the initial state, execute the specific method being tested, and
    finally verify the output or state changes against expected values.

    In Kotlin, you use `@Test` annotation from ... or `kotlin.test` packages to
    flag a function for the test runner.
* */

class LinkedlistTests {

    @Test
    fun `add inserts element at specified index`() {
        // 1. Arrange: Initialise the class and required state
        val list = SinglyLinkedListJava<String>()

        // 2. Act: Execute the target behavior
        list.add(0, "cat")

        // 3. Assert: Verify the invariant or expected state
        assertEquals("cat", list[0])
        assertEquals(1, list.size)
    }

}
/*
    Note: Kotlin allows methods enclosed in backticks (` `) to contain spaces,
    making test execution logs highly readable.
* */



/*
--------
2. The Assertion Library
    Assertions are the core mechanism that determines if a test passes or fails.
    If the condition within the assertion evaluates to false, an `AssertionError`
    is thrown, halting that specific test immediately.

    assertEquals(expected, actual)
    assertTrue(condition)
    assertNull(actual)              // Verifies the object reference is null.
    assertFailsWith<t> { block }    // Verifies that the enclosed lambda throws the specified Exception type.

* */















/*
1. Threads and Runnables
    Threads are executing units within a process that share global memory but
    maintain separate local memory, program counters, and execution stacks.

Implementing a Runnable
    Kotlin (via Java) uses the `Runnable` interface, which requires implementing
    a `run()` method.

```
class MyTask : Runnable {
    override fun run() {
        // Code to execute concurrently
        println("Executing in a separate thread")
    }
}
```


Launching and Managing a Thread
    The `Thread` class is constructed using a `Runnable` instance.
```
val myThread = Thread(MyTask())

myThread.start()
myThread.join()
```




* */



/*
The Mechanics of `start()` and `join()`
    When you instantiate a `Thread` object in memory, it is essentially inert.
    Calling `start()` is the specific trigger that requests the OS's thread
    scheduler to allocate a new, independent call stack and execution context
    for this process. The OS transitions the thread's lifecycle state from `NEW`
    to `RUNNABLE`. Once CPU time is allocated, OS invokes `run()` method...
    If were to call `run()` directly instead of `start`


    ...



* */


/*
(\o/)___________________________________________________________(\o/)
(/|\)                                                           (/|\)
  |                                          .-~~~-.              |
  |                                        /        }             |
  |                                       /      .-~              |
  |                             \        |        }               |
  |             __   __       ___\.~~-.-~|     . -~_              |
  |            / \./  \/\_       { O |  ` .-~.    ;  ~-.__        |
  |        __{^\_ _}_   )  }/^\   ~--~/-|_\|   :   : .-~          |
  |       /  /\_/^\._}_/  //  /     /   |  \~ - - ~               |
  |      (  (__{(@)}\__}.//_/__A__/_A___|__A_\___A______A_____A   |
  |       \__/{/(_)\_}  )\\ \\---v-----V----v----v-----V-----v--- |
  |         (   (__)_)_/  )\ \>                                   |
  |          \__/     \__/\/\/                                    |
  |             \__,--'                                           |
  |                                                               |
(\o/)___________________________________________________________(\o/)
(/|\)                                                           (/|\)
* */


/*
1. `start()`: The Trigger
    - What it does: It officially spawns the background thread and tells it to
      execute its `run()` block concurrently. Your main code instantly moves to
      the next line without waiting for the thread to finish.
    - When to use it: Whenever you need to kick off a concurrent background
      task.
    - The Exam Trap: An exam question willl often show code calling
      `myThread.run()`. This is alwas a bug. Calling `.run()` directly does not
      create a background thread; it just executes the code synchronosuly on the
      main thread, completely destroying your concurrency.

2. `join()`: The Blockade
    - What it does: It forces the thread that calls it (usually your `main`
      thread) to freeze perfectly still. It will not execute the next line of
      code until the target thread has 100% finished its job.
    - When to use it: When your main code needs the result of the background
      thread to proceed.
    - The Exam Trap: If you forget to use `.join()`, the main thread will reach
      the end of the program and print out empty or half-finished variables
      before the backgroudn threads even get a chance to finish their math.



------
* */
fun main106() {
    var examScore = 0

    val grader = Thread {
        Thread.sleep(1000)
        examScore = 100
    }

    grader.start()

    // ... because there is no `join()`, the main thread does not wait.
    // It instantly prints this BEFORE the thread finishes sleeping.
    println("Final score: $examScore")      // Output is ALWAYS 0.
}

/*
    Fix: You must put `grader.join()` right before the `println` to force the
    main thread to wait for the `100` to be assigned.
* */



/*
Exam Example 2: The "Split Loop" (Performance Trap)
    If an exam asks you to dispatch an array of 10 threads, you MUST use two
    separate loops. If you put `start()` and `join()` in the same loop, you are
    writing synchronous code and will lose the marks.
*/


fun main136() {
    val threads = Array(10) { Thread { doHeavyMath() } }

    // CORRECT: Two Loops.
    // 1. Kick them all off simultaneously so they work at the same time.
    for (t in threads) { t.start() }

    // 2. Now wait for all of them to finish.
    for (t in threads) { t.join() }
}
/*
    If an exam shows them in the same loop (`for (t in threads) {t.start(); t.join()}`),
    you should immediately identify that the code is broken. It forces Thread 1
    to start and finish before Thread 2 is even allowed to start, making it just
    as slow as if you never used threads at all.
* */




fun main156() {
    val worker = Thread {
        println("Worker: Processing heavy data...")
        Thread.sleep(1000)          // Simulating work
        println("Worker: Finished.")
    }

    worker.start()

    println("Main: Waiting for worker to finish...")
    worker.join()           // Main thread HALTS here.

    // This line is guaranteed to execute only AFTER the worker threads terminates
    println("Main: Data processing confirmed. Exiting.")
}



/*
Example 2: Batch Execution (The Fork-Join Pattern)
    When dealing with multiple threads, you must dispatch all of them before
    invoking any blocking calls.
* */
fun main179() {
    val threads = List(3) { index ->
        Thread {
            println("Task $index executing")
            Thread.sleep(500)
        }
    }

    // Phase 1: Forking. Dispatch all threads to the OS scheduler immediately.
    for (t in threads) {
        t.start()
    }

    // Phase 2: Joining. Block the main thread until all workers hit TERMINATED.
    for (t in threads) {
        t.join()
    }

    println("All tasks completed. Proceeding with aggregation.")
}




/*

* */
fun main206() {
    val threadList: List<Thread> = List(10) { index ->
        Thread {
            print("hello world, this is thread $index")
        }
    }

    for (t in threadList) {
        t.start()
    }

    print("Starting now")

    for (t in threadList) {
        t.join()
    }
}


/*
* */

fun initializeSensors(): List<String> {
    val statuses = mutableListOf<String>()

    val gpsThread = Thread {
        Thread.sleep(200) // Simulating hardware delay
        statuses.add("GPS: 3D Fix")
    }
    val gyroThread = Thread {
        Thread.sleep(100)
        statuses.add("Gyro: Calibrated")
    }
    val baroThread = Thread {
        Thread.sleep(150)
        statuses.add("Baro: Nominal")
    }

    gpsThread.start()       // Dispatch all to the OS scheduler
    gyroThread.start()
    baroThread.start()

    gpsThread.join()        // Block the main thread until each one terminates
    gyroThread.join()
    baroThread.join()

    return statuses         // Safe to return
}



/*
* */

fun processBatches(batchCount: Int): IntArray {
    val results = IntArray(batchCount)
    val threads = Array(batchCount) { index ->
        Thread {
            results[index] = index * 100
        }
    }

    threads.map { t -> t.start() }
    threads.map { t -> t.join() }

    return results
}




/*  ----    ----    ----    ----    ----    -----   ----    ----    -   ----  */
/*
2. Data Races and Synchronisation
    A data race occurs when multiple threads access the same memory location
    simultaneously without synchronisation, and at least one access is a write.
    This laeads to non-deterministic behavior and state corruption, such as lost
    increments.


Manual Lock Management
    Locks provide mutual exclusion for critical sections. The standard
    implementation is `ReentrantLock`, which allows a thread to re-acquire a lock
    it already holds without deadlocking.
* */

class main292(
    private val lock: Lock = ReentrantLock()
) {
    fun updateState() {
        lock.lock()         // Acquires the lock
        try {
            // Critical Section
        } finally {
            lock.unlock()           // Must be explicitly released by the holding thread.
        }
    }

    fun safeUpdate() {
        lock.withLock {
            // Critical Section
            // Lock is automatically released at the end of this block
        }
    }
}





// Idiomatic Kotlin: `withLock`
/*
    Manual locking is prone to errors, such as failing to release the lock if an
    exception is thrown or an early return occurs. The `withLock` extension
    function guarantees the lock is released regardless of how the block exits.
* */



/*
Syntax && Strategy Hints
    Before starting the tasks, review these mechanical constraints for
    `ReentrantLock` and `withLock` in Kotlin:

    * THE EXTENSION FUNCTION: `lock.withLock { }` is not a special compiler
      keyword; it is an inline extension function. It compiles down to `lock.lock()`
      , followed immediately by a `try { ... } finally { ... }` block. It
      guarantees the lock is released even if an exception is thrown inside the
      block.
    * REENTRANCY: "Reentrant" means that if Thread A currently holds `Lock X`,
      Thread A can proceed past other `Lock X` barriers in your without blocking
      itself. The lock simply increments an internal hold count.
    * MANUAL CONTROL: You can call `lock.lock()` and `lock.unlock()` manually
      if the lock acquisition and release must occur in different methods (which
      `withLock` cannot do).
    * NON-BLOCKING ACQUISITION: `lock.tryLock()` attempts to acquire the lock.
      If another thread holds it, it immediately returns `false` instead of
      puttng the thread to sleep. It can also accept a timeout:
      `tryLock(500, TimeUnit.MILLISECONDS)`.

* */



class SensorData(
    var totalReadings: Int,
    val lock: Lock = ReentrantLock()
) {
    fun addReading(value: Int) {
        lock.withLock {
            totalReadings += value
        }
    }

    fun processHardware() {
        hardwareLock.lock()
        try {
            if (hardware.isOverheating()) {
                throw IllegalStateException("Hardware fault")
            }
            hardware.execute()
        } finally {
            hardwareLock.unlock()
        }
    }
}




class BankAccount(
    var balance: Double = 0.0,
    val lock: Lock = ReentrantLock(),
) {
    fun deposit(amount: Double) {
        lock.withLock {
            balance += amount
        }
    }

    fun applyBonus() {
        lock.withLock {
            deposit(50.0)
        }
    }
}

// Why it doesn't freeze: Because the lock is a `ReentrantLock`, the OS scheduler
// checks the thread ID; since the thread requesting the lock inside ... is the
// exact same thread that already holds the lock from `applyBonus`, it allows
// execution to proceed and simply increments the lock's internal hold count to
// 2.



class DatabasePinger(
    val pingLock: Lock = ReentrantLock()
) {
    fun attemptPing(): Boolean {
        // tryLock() returns true if acquired, false if held by someone else
        if (pingLock.tryLock()) {
            try {
                network.ping()
                return true
            } finally {
                pingLock.unlock()
            }
        }
        return false
    }
}



class Disk(
    val id: Int,
    val data: String,
    val lock: Lock = ReentrantLock(),
)

fun syncDisks(disk1: Disk, disk2: Disk) {
    val firstLock  = if (disk1.id < disk2.id) disk1.lock else disk2.lock
    val secondLock = if (disk1.id < disk2.id) disk2.lock else disk1.lock

    firstLock.withLock {
        secondLock.withLock {
            println("Safely copying data between ${disk1.id} and ${disk2.id}")
        }
    }
}



/*
Micro-Explanation
    A STRIPED LOCK is a concurrency optimisation technique designed to eliminate
    the severe performance bottleneck of a single, global lock. If you protect a
    massive data structure (like a server-wide cache or a custom Hash Map) with
    just one `ReentrantLock`, every single thread must wait in a single-file
    line, even if they are trying to read or write completely different,
    unrelated pieces of data.

    Striped locking solves this by splitting the single lock into an ARRAY OF
    MULTIPLE LOCKS (the "stripes"). When a thread wants to access a specific
    piece of data, it calculates the hash code of the data's key, performs a
    modulo operation against the number of locks, and only acquires the specific
    lock assigned to that partition. This means if you have 16 stripes, up to 16
    diffeent threads can theoretically read and write to your data structure at
    the same time without ever blocking each other, provided their keys happen
    to map to different locks. They only block if two threads try to hit the
    exact same stripe.
* */


// The Stripes
    // val locks = Array(16) { ReentrantLock() }

// The Routing
    // val index = Math.floorMod(key.hashCode(), 16)

// The Execution
    // locks[index].withLock { map.put(key, value) }
    // Acquire only the required stripe.


/*
    val locks = Array(16) { ReentrantLock() }
    val index = kotlin.math.abs(key.hashCode() % 16)
    locks[index].withLock { map.put(key, value) }
* */


class HashMap482(
    val buckets: Array<Int> = Array(1024) { 0 },
    val locks: Array<Lock> = Array(16) { ReentrantLock() },
) {
    fun transfer(key1: String, key2: String) {
        val lock1 = locks[abs(key1.hashCode() % locks.size)]
        val lock2 = locks[abs(key2.hashCode() % locks.size)]

        // Edge case: Both keys map to the exact same lock!
        if (lock1 == lock2) {
            lock1.withLock { doTransferLogic () }
            return
        }

        val firstLock = if (key1 > key2) lock1 else lock2
        val secondLock = if (key1 > key2) lock2 else lock1

        firstLock.withLock {
            secondLock.withLock {
                doTransferLogic()
            }
        }
    }
}







































