package a40009_imperial.structurespack01.hashmap

class OrderedHashMap<K, V>(
    private val bucketCount: Int = DEFAULT_BUCKET_COUNT,
) : Iterable<Pair<K, V>> {
    companion object {
        private const val DEFAULT_BUCKET_COUNT = 16
    }

    // 2. The State Trackers
    private val buckets: Array<Node<K, V>?> =
        Array<Node<K, V>?>(bucketCount) { null }
    private var head: Node<K, V>? = null
    private var tail: Node<K, V>? = null
    private var currentSize = 0

    val size: Int
        get() = currentSize
    val isEmpty: Boolean
        get() = size == 0

    init {
        require(bucketCount > 0) { "bucketCount must be positive" }
    }

    fun put(key: K, value: V): V? {
        val hcode: Int = key.hashCode() % bucketCount
        val newNode: Node<K, V> = Node(key, value)
        var present: Boolean = false
        if (head == null && tail == null) {
            head = newNode
            tail = newNode
        } else {
            tail!!.nextInOrder = newNode
            newNode.prevInOrder = tail
            tail = newNode
        }
        var curr: Node<K, V>? = buckets[hcode]
        while (curr != null) {
            if (curr!!.key == key) {
                present = true
                break
            }
            curr = curr.nextInBucket
        }
        return if (!present) {
            newNode.nextInBucket = buckets[hcode]
            buckets[hcode] = newNode
            currentSize++
            null
        } else {

            val oldValue: V = curr!!.value
            curr.value = value
            oldValue
        }
    }

    operator fun get(key: K): V? {
        TODO("practice")
    }

    fun containsKey(key: K): Boolean {
        TODO("practice")
    }

    fun remove(key: K): V? {
        TODO("practice")
    }

    fun removeOldest(): Pair<K, V>? {
        if (head == null)
            return null
        if (head == tail) {
            
            head = null
            tail = null
        }
        TODO("practice")
    }

    fun valuesInInsertionOrder(): List<V> {
        return listOf()
    }

    fun keysInInsertionOrder(): List<K> {
        return listOf()
    }

    override fun iterator(): Iterator<Pair<K, V>> {
        return object : Iterator<Pair<K, V>>{
            override fun hasNext(): Boolean {
                TODO("Not yet implemented")
            }

            override fun next(): Pair<K, V> {
                TODO("Not yet implemented")
            }
        }
    }

    private class Node<K, V>(
        val key: K,
        var value: V,
        var nextInBucket: Node<K, V>? = null,
        var prevInOrder: Node<K, V>? = null,
        var nextInOrder: Node<K, V>? = null
    )
}


/*
    ... `require()` belongs to a family of functions in Kotlin... PRECONDITIONS.
    These are essentially guardrails that "fail fast," crashing the program
    immediately if something is wrong so that a small bug doesn't turn into a
    massive data corruption issue later.

Micro-explanation
    There are four main functions in this family. The "Big Two" are `require`
    (for checking what the CALLER did wrong) and `check` (for checking if your
    OWN OBJECT is in a broken state). `assert` is the "Silent Partner"--it only
    works while you are developing and testing; it is completely ignored when
    the code is actually running in production (on a user's device).


Syntax Panel: The Precondition Family
    - `require(bool)`
    - Throws: `IllegalArgumentException`
    - Intent: Validating arguments passed into a function

    - `check(bool)`
    - IllegalStateException
    - Validating the internal state of an object.

* */


/*
1. Why embed a Linked List inside a Hash Map?
    A standard hash map provides non-deterministic iteration order. Because
    elements are distributed across the `buckets` array based on
    `hashCode() % bucketCount` modulo operation, their physical location in
    memory has no correlation to their insertion sequence.

    To build an `OrderedHashMap`, you must satisfy two competing time
    complexities:
    - O(1) expected lookup: Achieved by the primary `buckets` array.
    - Deterministic: O(n) ordered traversal: Achieved by an auxilary data
      structure.

    Instead of maintaining a completely separate list (which would double your
    memory overhead), we embed a Doubly LL directly into the hash map nodes.
    The `buckets` array handles the mathematical key-based lookups, while the
    embedded DLL pointers maintain a chronological graph that overlays the
    entire structure, allowing you to bypass the `buckets` array entirely during
    traversal.


---
2. The Mechanics of `prevInOrder`, `nextInOrder`, `head` and `tail`
    These four components define the auxiliary DLL.
       - `head`: A class-level pointer referencing the chronologically oldest
         node in the map.
       - `tail`: A class-level pointer referencing the chronologically newest
         node.
       - `prevInOrder` / `nextInOrder`: Node-level pointers that establish
         bidirectional link between elements, entirely independent of their
         position in the `buckets` array.

    When a new node is instantiated and dropped into a hash bucket, you must
    execute an O(1) pointer update for the DLL: you update the current `tail`
    node's `nextInOrder` to point to th enew node, set the new node's `prevInOrder`
    ... This guarantees that traversing from `head.nextInOrder` will strictly
    yield elements in insertion order.


---
3. Kotlin's Custom Property Getters (`get()`)
    In Kotlin, properties are not raw fields. They inherently encapsulate a
    backing field and accessor methods (getters/setters) to enforce
    encapsulation.

    When you declare `val size: Int`, the compiler automatically generates a
    public getter method under the hood. By explicitly defining
    `get() = currentSize` directly below the property, you are overriding that
    default accessor implementation. When an external caller attempts to read
    the `size` property, the runtime dynamically invokes your custom getter

    ... this provides the same memory protection as a traditional Java
    `public int getSize()` method, but with zero boilerplate syntax at the
    call site.





* */







/*
    It is completely normal for this... essentially building...

    ..

---
Why do we need buckets? (The Collision Problem)
    A standard hash map works by feeding your key (like the string "apple") into
    a mathematical function that spits out a number (like `4`). The map then
    goes to the index `4` in your internal array and drops the value there. This
    is what makes hash maps so incredibly fast.

    But what happens if you add "banana"... hashcode == 4 again... This is called
    a HASH COLLISION. Index `4` is already taken! To solve this... index `4`
    doesn't just hold one item; it acts as the start of a standard Singly Linked
    List (a "bucket"). If "banana" lands on "apple", we just attach banana
    to apple using the `nextInBucket` pointer.


---
Why are we treating a map like a linked list?
    A normal hash map is chaotic. Because the math scrambles our keys into
    random array indices, the map loses track of the order


* */









/*
    When I say "random array indices," I am referring to the exact slot in your
    main `buckets` array (e.g., index `0` through `15`), which is calculated by
    compressing the key's `hashCode()`. When you add "apple" first, its hash
    code might drop it into index `14`. If you add "banana" second, its hash
    code might drop it into index `2`. The map isn't filling up left-to-right




* */


/*
    For Part 1: Representation and Size, you don't need to import any special
    libraries. The entire goal is to build this from absolute scratch using
    Kotlin's fundamental building blocks: generic classes and arrays.

---
Piece 1: The "Dual-Purpose" Node
    Because you cannot use `LinkedHashMap`, you have to build your own custom
    Node. The prompt specifically mentions that one node takes part in two
    structures.

The syntax:
    In Kotlin, you can nest a `private class` inside your main class. To handle
    generic types `K` and `V`, and allow variables to change later, you'll need
    `val` for the key and `var` for the rest.


---
Piece 2: The State Trackers (Class Variables)
    Your map needs to remember its current state. You need an array for the
    buckets, pointers for the order, and a manual counter for the size.

The Syntax for the Buckets:
    Creating an array of a generic type that allows `null` values is a classic
    Kotlin gotcha. You use `Array(num) {}` and a lambda block to fill it with
    initial nulls.

    ...

The Syntax for the Pointers and Counter:
    These are standard nullable variables and a simple integer.



















* */



