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

    // HELPER METHODS (Single Responsibility Principle)
    // Standard Practice: Never mix Doubly Linked List pointer math inside your main map logic.
    private fun appendToTail(node: Node<K, V>) {
        if (head == null) {
            head = node
            tail = node
        } else {
            tail?.nextInOrder = node
            node.prevInOrder = tail
            tail = node
        }
    }

    private fun detachFromOrder(node: Node<K, V>) {
        val prev = node.prevInOrder
        val next = node.nextInOrder

        if (prev != null) prev.nextInOrder = next else head = next
        if (next != null) next.prevInOrder = prev else tail = prev

        // Clean up the node's own pointers
        node.prevInOrder = null
        node.nextInOrder = null
    }

    fun put(key: K, value: V): V? {
        // 1. Safe Hash Math: abs() prevents negative indices from crashing the program
        val index = kotlin.math.abs(key.hashCode() % bucketCount)
        var current = buckets[index]

        // 2. Searching for existing key (No `!!` needed, smart casting handles it)
        while (current != null) {
            if (current.key == key) {
                // Key exists: Update value and move to newest position
                val oldValue = current.value
                current.value = value

                detachFromOrder(current)
                appendToTail(current)

                return oldValue // Early return
            }
            current = current.nextInBucket
        }

        // 3. Key does not exist: Create new entry
        val newNode = Node(key, value)

        // Standard Practice: Prepend to bucket chain for O(1) insertion. Do not
        // walk to the end of the bucket chain; it wastes time!
        newNode.nextInBucket = buckets[index]
        buckets[index] = newNode

        // 4. Update the timeline and size
        appendToTail(newNode)
        currentSize++

        return null
    }

    operator fun get(key: K): V? {
        val index = kotlin.math.abs(key.hashCode() % bucketCount)   // Safe hashmap math `kotlin.math.abs()`
        var current: Node<K, V>? = buckets[index]

        while (current != null) {       // Traverse only the O(1) bucket chain, completely ignoring the timeline DLL
            if (current.key == key) {
                return current.value        // Fast return, no timeline modification
            }
            current = current.nextInBucket
        }

        return null // key not foudn
    }

    fun containsKey(key: K): Boolean {
        return get(key) != null
        // Standard practice -- Don't Repeat Yourself
        // Since our map does not accept null values (V is not V?)
        // a non-null return from get() strictly guarantees existence.
    }

    fun remove(key: K): V? {
        val index = kotlin.math.abs(key.hashCode() % bucketCount)
        var current: Node<K, V>? = buckets[index]
        var prevInBucket: Node<K, V>? = null

        // 1. Traverse the SLL (Bucket)
        while (current != null) {
            if (current.key == key) {
                if (prevInBucket == null) {
                    buckets[index] = current.nextInBucket
                } else {
                    // Case B: It's in the middle/end of the bucket
                    prevInBucket.nextInBucket = current.nextInBucket
                }
                detachFromOrder(current)
                currentSize--
                return current.value
            }
            // Move pointers forward
            prevInBucket = current
            current = current.nextInBucket
        }
        return null                 // Key not found
    }

    fun removeOldest(): Pair<K, V>? {
        val oldestNode = head ?: return null
        val removedValue = remove(oldestNode.key)
            ?: error("State corruption: Node found in timeline but missing from buckets")
        return oldestNode.key to removedValue       // Use `to` infix function to create a `Pair` cleanly
    }

    fun valuesInInsertionOrder(): List<V> {
        // Standard Practice: Program to an Interface, not an Implementation.
        // We use mutableListOf internally, but return it as a read-only List<K>
        val result: MutableList<V> = mutableListOf()
        var current: Node<K, V>? = head

        while (current != null) {
            result.add(current.value)
            current = current.nextInOrder
        }
        return result
    }

    fun keysInInsertionOrder(): List<K> {
        val result: MutableList<K> = mutableListOf()
        var current: Node<K, V>? = head

        while (current != null) {
            result.add(current.key)
            current = current.nextInOrder
        }
        return result
    }

    override fun iterator(): Iterator<Pair<K, V>> {
        return object : Iterator<Pair<K, V>>{
            var current = head

            override fun hasNext(): Boolean = current != null

            override fun next(): Pair<K, V> {
                val node = current ?: throw Exception("No more elements to iterate")
                current = node.nextInOrder
                return node.key to node.value
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
    SLL Traversal (`prevInBucket`): Because our buckets only point forward,
    the only way to delete a node is to connect the guy behind it to the guy
    in front of it. Tracking `prevInBucket` as you loop is the standard pattern
    for O(N) SLL deletion.





    ... especially when mapping concepts... Reading good ... Here are the standard,
    production-ready implementations for `get` and `containsKey`.


---
Kotlin Standards && Best Practices Used Here
    1. The DRY Principle in Maps: You will almost never see a manual
       bucket-traversal loop written inside `containsKey`. Because `get(key)`
       already executes the exact O(1) bucket lookup required, `containsKey`
       simply wraps it. This prevents duplicate logic that could go out of sync
       if the bucket structure ever changed.
    2. READ-ONLY SAFETIES: Notice that `get` never touches `head`, `tail`,
       `prevInOrder`, or `nextInOrder`. The prompt explicitly forbids making
       this an "access-order" map (like an LRU Cache usually does when you read
       a value). By strictly using `nextInBucket`, the traversal remains
       isolated to the underlying array.
    3. OPERATOR OVERLOADING: By using the `operator` keyword on `fun get`, Kotlin
       allows the syntax `map["crew"]` at the call site. Under the hood, the
       compiler translates those brackets directly into a call to this specific
       function.






* */







/*
Kotlin Standards && Best Practices Used Here
    1. The "No `!!` Rule": You should almost never use `!!` in production Kotlin
       . It explicitly tells the compiler "I don't care about your safety checks
       , crash the app with a NullPointerException if I'm wrong."
            * How we fixed it: By using a standard `while (current != null)`
              loop, Kotlin's compiler automatically "smart casts" `current` to a
              non-null type inside the loop. You can just type `current.key`
              safely!
    2. EARLY RETURNS (Guard Clauses): Instead of using a `var present = false`
       flag and nesting a massive `if/else` at the bottom, standard practice is
       to return immediately the moment you finish your work. If we find the
       key, we update it, relink it, and `return oldValue` right then and there.
       If the loop finishes without returning, we knwo the key is entirely new.
    3. O(1) Linked List Prepending: In your code, you write `while (curr.nextInBucket !=  null)`
       to attach the new node to the very end of the hash bucket. That makes
       insertion O(n) based on bucket size. Standard map implementaions just
       attach the new node to the front of the bucket (`newNode.next = buckets[index]`),
       making insertion a flawless O(1).
    4. SINGLE RESPONSIBILITY: Separating pointers into `detachFromOrder` and
       `appendToTail` isn't just to look pretty. When we write `remove()` later,
       you will have to detach nodes again. Writing it once as a helper prevents
       catastrophic copy-paste errros.




* */





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



