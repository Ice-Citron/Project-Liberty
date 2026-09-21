package a40009_imperial.structurespack01.hashmap

class Sienar {
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
When you should avoid `data class`
    Avoid it for mutable structural nodes or identity-heavy objects.

    Bad candidates:
        - linked-list nodes
        - graph nodes with changing edges
        - locked/thread-safe objects
        - objects where identity matters more than value equality.





    The core issues with using a `data class` for these candidates comes down to
    its defining feature: the automatic generation of `equals()`, `hashCode()`,
    and `toString()` based entirely on the data held inside. For STRUCTURAL
    NODES (like linked lists or graphs), this auto-generation is disastrous.
    Because the generated functions look at the properties--which include
    pointers to the next nodes--calling `toString()` or comparing two nodes forces
    Kotlin to recursively traverse the entire chain. If there is a circular
    reference (like a doubly-linked list or a cyclic graph), this instantly
    crashes your program with a `StackOverflowError`.

    For THREAD-SAFE or IDENTITY-HEAVY OBJECTS, a `data class` actively works against
    your design. Data classes enforce value equality, meaning if two objects hold
    the exact same data, they are treated as the exact same entity. If two
    separate systems...









When is `data class` a good choice?
    Use `data class` for small value objects.

    `data class Coordinate(val row: Int, val col: Int)`

    You get `equals, hashCode, componentN, toString, copy` and destructuring
    automatically.
* */










class Module(
    val code: String,
    val marks: Int,
) {
    init {
        require(code.isNotBlank()) { "code must not be blank" }
        require(marks > 0) { "marks must be positive" }
    }
}

class Bag38<T> {
    private val items = mutableListOf<T>()

    val size: Int
        get() = items.size

    val isEmpty: Boolean
        get() = items.isEmpty()
}


class LinkedBag<K, V> {
    private var head: Node<K, V>? = null

    private class Node<K, V>(
        val key: K,
        var value: V,
        var nextNode: Node<K, V>? = null
    )
}


/*
Why are linked-list/hash-map nodes often private nested classes?
    They are implementation detals, not part of the public API.

    Keep pointer structure hidden from callers.





When is custom getter better than storing another field?
    When the value is derived from existing state.
    Do not duplicate state unless you must.






... expose a readable property but prevent external mutation...
    Use a public `var` with a private setter.
    ...
    var value: Int = 0
        private set

    ---
    Useful for `size`, `isBorrowed`, counters and state flags.





Reading Checklist:
    `Identifiable` is a small interface contract.
    `Borrowable` extends another interface.
    `CatalogueItem` is abstract because it stores shared state and requires
    `loanDays`.                 <-- open functions() for overridable functions!                 // abstract for no body ++ overridable
    `Book` uses inheritance and interface implementation at the same time.
    `Magazine` is a catalogue item but not borrowable.
    `Loan<T : Borrowable>` uses a generic bound.
    `Repository<T : Identifiable>` can safely use `item.id`.
    `LendingDesk<T>` uses a `where` clause because `T` must be both a
    `CatalogueItem` and `Borrowable`.
    `repository[id]` works becuase `Repository` defines `operator fun get`



    Their functions adn properties can be:
        abstract, in which case they have no body in the abstract class, but
        subclasses must implement them.
        open, in which case they have a body in the abstract class, but subclasses
        may override them.




    At its core, an interface is a CONTRACT or a "behavioral blueprint." It tells
    a class what it must be able to do (e.g., "you must have a `fly()` function"),
    but it doesn't care how the class actually does it. Unlike a class, an
    interface cannot store "state"--it can't have a private variable like
    `var fuel = 100` that holds a value in memory. It can only define methods or
    properties that the implementing class is forced to provide.

    When one interface inherits from another ... it's simply COMBINING REQUIREMENTS
    . If `Robot` requires a `charge()`... any class that claims to be a `Drone`
    iis now legally bound by the contract to implement both. This allows you to
    build complex, specialised behaviors out of smaller, simpler pieces without
    the "Diamond Problem" mess of C++, because interfaces aren't carrying around
    their own private data.


    interface MyInterface {
        fun doWork()                    <-- abstract by default (must be overridden)

        fun log() { println("Hi") }     <-- interfaces can have default code
    }
    interface B : A { ... }
    class MyClass : myInterface { ... }





* */















