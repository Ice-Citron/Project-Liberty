package a40009_imperial.structurespack01.hashmap

class Romana( // : Comparable<T> (
    val cities: Int,
    val grains: Double
) {
    operator fun plus(other: Romana): Romana {
        return Romana(cities + other.cities, grains + other.grains)
    }

    operator fun contains(item: Int): Boolean = true
    // operator fun containsKey(item: Int): Boolean = true          <=== ILLEGAL FUNCTION NAME

    override operator fun equals(other: Any?): Boolean = false
}




/*
What are Kotlin scope functions for?
    They let you run a block of code in the context of an object.

    The five main ones are:
        let, run, with, apply, also

    with (object)
    object.let
    object.apply(initialise a new class's attributes?)
    object.run(running functions)
    object.also(printing things...)

    




* */


































/*

    linked list     --> override operator fun iterator() {}         // Iterable<T,k,k,,kmklkmjkkllllllkk> class // return object : Iterator<T>
    BST             --> operator fun contains(value: x): Boolean    // operator fun compareTo()
                            - `contains` for membership:
                                x in tree
                            - `compareTo` on the stored type if values need ordering:
                                a < b, a > b
                            Most BST code depends on comparison, even if not written with operator syntax.



    Your class needs to be iterable over pairs:
    `Iterable<Pair<K, V>>`

    Then `iterator()` returns pairs in insertion order, and each `Pair` supports
    destructuring into `k` and `v` (through `componentN()`).




    factory()           <-- this makes an object callable like a function.
    `factory.invoke()`
    `operator fun invoke(): R`

--- ---

    point.unaryMinus()
    operator fun unaryMinus(): Point

    operator fun unaryPlus(): R
    operator fun not(): Boolean

    operator fun inc(): T
    operator fun dec(): T

* */



/*

val (a, b) = pair
val a = pair.component1()
val b = pair.component2()

    Data classes automatically generate `component1()`, `component2()`, etc. for
    constructor properties,

    data class User(
        val name: String,
        val age: Int
    )

    allows: `val (name, age) = userObj`



---
for ((key, value) in map) { ... }
    Two conventions combine:
        1. `map.iterator()` gives entries.
        2. Each entry has `component1()` and `component2()`.
    So each entry can be destructured into key and value.




---

    operator fun component1()
    operator fun component2()
    operator fun component3()
    ...
    operator fun componentN()
        Kotlin calls as many component functions as variables requested.
* */




/*
    operator fun compareTo(other: B): Int         `a - b` // if positive... a > b.... if neutral... a = b.... if negative a < b....

    Usually implemented through `Comparable<T>`

    a.compareTo(b) = 0      ->  a = b
    a.compareTo(b) >= 0     ->  a >= b
    a.compareTo(b) > 0      ->  a > b
    a.compareTo(b) < 0      ->  a < b



    a?.equals(b) ?: (b === null)

    override operator fun equals(other: Any?): Boolean
            For hash-based collections, also implement `hashCode()` consistently.


    === and !== are identity checks and are not overloadable... Use == for structural equality through `equals`.
* */

/*
    operator fun equals(other: Any?): Boolean
        A typed overload like `equals(other: User)` will not be used by `==` in the standard way






* */













/*
    ... mental mind map for Kotlin's `operator` keyword. It is essentially
    Kotlin's version of "syntactic sugar"--it allows you to use standard
    mathematical or logical symbols on your own custom classes, making the code
    much easier to read.

    Under the hood, whenever the compiler sees a symbol like `+` or `[]`, it just
    translates it into a standard function call.

    ... complete categorised map of the exact function names you need to memorise
    , what symbols they map to, and how they behave.


1. Indexed Access (The Dictionary / Array Style)
    ... used to make your class behave like an Array, List or Map.
    - `operator fun get(index: Int)` maps to `a[i]`
       - Example: `operator fun get(key: K): V` translates `map["score"]` into
         `map.get("score")`
       - Note: You can have multiple parameters!
         `operator fun get(x: Int, y: Int)` allows `matrix[0, 1]`

2. Arithmetic (The Math Style)
    These are used to allow mathematical operations between objects...
    (like adding two `Vector` or `ComplexNumber` objects together).

    - `operator fun plus                == a + b
    - `operator fun minus`              == a - b
    - `operator fun times`              == a * b
    - `operator fun div`                == a / b
    - `operator fun rem`                == a % b

3. Augmented Assignments (The "Modify in place" style)
    If your object is mutable and you want to add something into it without
    creating a brand new object, you use these.

    - `plusAssign`              == a += b
    - `minusAssign`             == a -= b
    - `timesAssign`             == a *= b

    Exam trap: The compiler is smart. If you don't write `plusAssign`, but you
    do write `plus`... writing a += b still works... If you write both, the
    compiler will throw an error if you use `+=` because it doesn't know which
    one you want it to use!

4. Comparison && Equality (The Logic Style)
    This is crucial for sorting algorithm or checking states.
    - `equals`      maps to `a == b` or `a != b`
            * Note: You don't actually use the `operator` keyword for `equals`.
              You use `override` because it is inherited from the base `Any`
              class.
    - `compareTo`   maps to `a > b`, `a < b`, `a >= b`, `a <= b`
            * Requirement: This function must return an `Int`. (0 if equal,
              positive if `a` is bigger, negative if `b` is smaller).

5. The "Collection Checks" (The Search style)
    These make your custom data structures feel like native Kotlin collections.
    - `contains`    maps to `a in b` and `a !in b`
            * Requirement: Must return a `Boolean`.
            * Example: `operator fun contains(key: K): Boolean` translates
              `if ("apple" in myMap)` to `if (myMap.contains("apple"))`.

6. The "Weird but Powerful" Ones (Advanced)
    These are less common but show up in advanced architectures.
    - `operator fun invoke()` maps to `a()`
            * This lets you "call" an object as if the object itself were a
              function.
            * Example: If an object `Validator` has
                `operator fun invoke(text: String)`... you can just write
                `Validator("The password is password")`
    - `component1`, `component2`, ..., `componentN` maps to `val (x, y) = a`
        `operator fun component1() {}`
        ... This is called "Destructuring". It allows you to unpack an object
        directly into multiple variables at once. (Data classes do this
        automatically).




a.div(b)
operator fun div(b: B): R

a.rem(b)
operator fun rem(other: B): R

a.rangeTo(b)
operator fun rangeTo(other: B): R
a..b

a.rangeUntil(b)
operator fun rangeUntil(other: B): R

map.get("alice")
operator fun get(key: K): V?

map.set("alice", 42)
operator fun set(key: K, value: V)

matrix[2, 3]
matrix.get(2, 3)
operator fun get(row: Int, col: Int): T

matrix[2, 3] = 99
matrix.set(2, 3, 99)
operator fun set(row: Int, col: Int, value: V)

x in bag
bag.contains(x)
operator fun contains(item: T): Boolean

x !in bag
!bag.contains(x)
operator fun contains(item: T): Boolean
    Do not implement `notContains`, Kotlin simply just negates `operator fun contains`

---


key in orderedHashMap
if ("alice" in map) { ... }
    This can be nicer than `map.containsKey("alice")`, but only if the method
    name is `contains`

class OrderedHashMap<K, V>() {
    operator fun contains(key: K): Boolean {
        for (bucket in buckets) {
            for (i in bucket) {
                ... // check whether each i has key = key
                if (...) return true
            }
        }
        return false
    }
}










What does Kotlin need for `for (x in collection)`?
    Kotlin needs:
        `collection.iterator()`
    then repeatedly:
        `iterator.hasNext()` and `iterator.next()`
    These are operator conventions. Implementing `Iterable<T>` gives you the
    standard shape.


---
    ... the difference between `Iterable` and `Iterator` is the difference
    between a BOOK and a BOOKMARK.

Micro-Explanation
    An `Iterable<T>` is a declaration that an object can be looped over. It is
    a "factory" for iterators. If a class implements `Iterable`, it is
    essentially promising the compiler: "I have a collection of things, and if
    you ask me, I can give you a tool to go through them one by one."This is what
    allows you to use the object in a `for` loop.

    An `Iterator<T>` is the actual tool (the "pointer" or "bookmark") that does
    the walking. It keeps track of where you currently are in the collection and
    knows how to get the next item. It is short-lived; once it reaches the end
    of the collection, it's done.


---
    To make this concrete, let's look at how ... `OrderedHashMap` would actually
    implement these. Think of `Iterable` as the CONTRACT and the `Iterator` as
    the WORKER.


---
    A nested class marked as inner can access the members of its outer class.
    Inner classes carry a reference to an object of an outer class:

    class Outer {
        private val bar: Int = 1

        inner class Inner {
            fun foo() = bar
        }
    }

    val demo = Outer().Inner().foo          // == 1







--- --- ---
    ... can absolutely return an anonymous object using
    `return object : Iterator<T> { ... }`. ... this is highly idiomatic in Kotlin
    where you only need a quick, one-off implementation of an interface. It
    functions exactly like the `private inner class` because the compiler still
    secretly creates an inner class behind the scenes that holds onto the parent
    class's state, it just saves you the trouble of having to invent a formal
    name for a class you will only ever instantiate in that one specific line of
    code.

    As for the `override` keyword, you need it strictly because your class
    signed the `Iterable` contract (`: Iterable<T>`); in Kotlin, you are forced
    to explicitly type `override` anytime you fulfill an interface's promise.
    You didn't confuse `class` and `data class` here, but you did slightly
    mix up the origin of those default functions! Methods like `.toString()` and
    `.hashCode()` are baked into a special root class called `Any` (which every
    Kotlin class inherits from automatically), so they are always present
    everywhere. However, `iterator()` does not come from `Any`--it strictly
    belongs to the `Iterable` interface, meaning a standard class will never
    poseess an `iterator()` method natively unless you explicitly attach that
    interface to it.






-----------
    In Kotlin, a named `object` or `companion object` nested inside a class
    CANNOT directly access the instance attributes (variables) of that outer
    class. Because these objects act as Singletons (similar to `static` in Java)









----------

    .... though both act as singletones tied to the class! The key difference
    is HOW YOU ACCESS THEM. A class can have only one `companion object`, and
    its properties and methods are promoted to the outer class level, meaning
    you call them directly like `OuterClass.myStaticMethod()` -- this is your
    true Java `static` equivalent. On the other hand, you can have as many
    regular nested `object`s as you want, but they act as their own separate
    namespaces; you must explicitly use their name to access anything inside
    them, lik `OuterClass.MyNestedObject.myMethod()`. So, while both store
    shared state, only the companion object gives you that clean, seamless
    "static" access.







































* */