package g0101_0200.s0173_binary_search_tree_iterator

import com_github_leetcode.TreeNode

class BSTIterator(root: TreeNode?) {

    // Our "notepad" that only holds the current path back up the tree
    // Using ArrayDeque as it's the most efficient stack implementation in Kotlin
    private val stack = ArrayDeque<TreeNode>()

    init {
        pushAllLeft(root)
    }

    private fun pushAllLeft(node: TreeNode?) {

    }

    fun next(): Int {
        TODO("practice")
    }

    fun hasNext(): Boolean {
        TODO("practice")
    }
}


/*
Micro-Explanation
    THE BRUTE FORCE WAY: When the `BSTIterator` starts, you could just run a
    standard recursive in-order traversal and save every single node's value
    into a flat Kotlin `List`. Then, `next()` just reads the next item in the
    list. It is super easy to write, but it uses `O(n)` memory because you are
    storing the whole tree at once.

    THE PRO WAY (The Follow-up): To achieve `O(h)` memory (where `h` is the
    height of the tree), you have to manually simulate the computer's recursive
    call stack. Instead of processing the whole tree, you just take your current












* */









/*
    Leetcode is notoriously hard, so don't sweat it! The jump from standard
    algorithms to OO-design problems like iterators always throws people for a
    loop.

    The prompt gives you a huge clue with the "Follow up" section. There are two
    ways to solve this, and deciding which one to build changes your entire
    approach.


Micro-Explanation
   The



* */









/*
Micro-explanation: `object`
    In Kotlin, declaring an `object` is the language's built-in way to instantly
    implement the Singleton pattern. It defines a class and simultaneously
    creates the one and only instance of it in a single step. You never use a
    constructor or the `()` syntax to instantiate it; the object is created
    automatically (and thread-safely) the very first time you try to access it
    in your code.

    You typically use a standalone `object` when you need a single, glbally
    shared state or a collection of utility functions across your entire
    application--like a `DroneNetworkManager` or a `PhysicsConstants` file.
    Because there is only ever one instance, you interact with it directly by
    its name, calling its properties and methods like
    `DroneNetworkManager.connect()`.


Micro-explanation: `companion object`
    A `companion object`, on the other hand, is a specific type of object
    physically nested inside a regular `class`. Because Kotlin completely removed
    Java's `static` keyword, companion objects serve as the official replacement.
    They allow you to define properties and functions that belong to the class
    blueprint itself, rather than to individual instances created from that
    class.

    You will use companion objects primarily for class-level constant or "factory
    methods" (functions that create instances of the class with specific presets).
    The best part is the syntactic sugar: you don't need to reference the
    companion object's internal name. You just call the function using the outer
    class's name, perfectly mimicking Java's static method calls, like
    `User.createAdmin()`.


---

   A `companion object` is tied directly to the class blueprint itself, acting
   exactly like `staic` in Java. This means there is only one shared companion
   object in memory, no matter how many individual instances of the class you
   create. It does not belong to the instantiated objects, which is why you
   call its methods using the class name itself rather than a specific variable.

   Yes, for things inside a class, mapping `companion object` to Java's `static`
   is a perfect mental model--it acts exactly like a clean wrapper for your
   class-level variables and methods. However, if you just want general "static"
   utility functions, Kotlin's ultimate clean solution is to write top-level
   functions directly in a file, completely outside of any class braces!
* */


interface DroneTask {
    fun execute()
}

fun createScoutTask(): DroneTask {
    // Creating and returning an anonymous object on the fly!
    return object : DroneTask {
        override fun execute() {
            println("Scouting sector 7G...")
        }
    }
}

fun notMain103() {
    // We pass the object around like a normal variable
    val myTask: DroneTask = createScoutTask()

    // And trigger its task later
    myTask.execute()    //  .... Output: "Scoring sector 7G..."
}

/*
    ... When you use `object` inside a function like that, it is called an
    OBJECT EXPRESSION (or an anonymous object).

Micro-Explanation
    It lets you build a throwaway object on the fly without giving it a class
    name. You almost always use this when you need an object that implements a
    specific interface (or extends a class), but you only need it this one
    exact time. Instead of creaating a whole new file and a formal `class` for
    it, you just spin up an anonymous object, define the required behavior, and
    return it. It sounds abstract, but you can pass this object around your
    program to execute tasks whenever you need them.


'''
                Anonymous Object
                A quick, local data container (rare).
`
val data = object {
    val id = 1
}



                Implementing Interface
                Creating a one-off implementation (very common).
val worker = object : Runnable {
    ...

    override fun overfunc() : Int { ... }
}



                Returning Object
                Passing the behavior around your app.
'''



* */












