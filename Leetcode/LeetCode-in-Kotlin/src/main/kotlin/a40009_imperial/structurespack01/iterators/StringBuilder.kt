package a40009_imperial.structurespack01.iterators

import kotlin.text.StringBuilder


fun decode(s: String): String {
    val countStack: ArrayDeque<Int> = ArrayDeque()
    val stringStack: ArrayDeque<StringBuilder> = ArrayDeque()

    var newCount: Int = 0
    var currentCount: Int = 0
    var currentSB: StringBuilder = StringBuilder()

    for (c in s) {
        if (c in "0123456789") {
            newCount = newCount * 10 + c.digitToInt()
        } else if (c == '[') {
            countStack.addLast(currentCount)
            currentCount = newCount
            newCount = 0
            stringStack.add(currentSB)
            currentSB = StringBuilder()
        } else if (c == ']') {
            val newSB = stringStack.removeLast()
            repeat(currentCount) {
                newSB.append(currentSB.toString())
            }
            currentCount = countStack.removeLast()
            currentSB = newSB
        } else {
            currentSB.append(c)
        }
    }
    return currentSB.toString()
}


fun main() {
    println(decode("3[a]"))
    println(decode("2[ab3[c]]"))
    println(decode("3[a2[b]]"))
    println(decode("10[x]"))
}






fun removeVowels(s: String): String {
    val sb: StringBuilder = StringBuilder()
    for (c in s) {
        if (c !in "aeiouAEIOU") sb.append(c)
    }
    return sb.toString()
}

fun reverseWords(s: String): String =
    s.split(" ").map { StringBuilder(it).reverse().toString() }
        .joinToString(" ").toString()




fun compress(s: String): String {
    if (s.isEmpty()) return ""
    val sb = StringBuilder()
    var curr: Char = s[0]
    var count: Int = 1
    for (i in 1..<s.length) {
        if (s[i] == curr) {
            count++
        }
        else {
            sb.append(curr).append(count)
            curr = s[i]
            count = 1
        }
    }
    sb.append(curr).append(count)
    return sb.toString()
}


/*
    Before you dive in, keep these techncial details in your back pocket:
    - THE CHAIN: Almost all `StringBuilder` methods return the builder itself,
      allowing ou to chain calls smoothly: `sb.append("a").reverse().deleteCharAt(0)`
    - THE SHIFTING PENALTY: Remember that `.append()` is incredibly fast, but
      `.insert()` and `.delete()` force the underlying array to shift elements
      left or right. If you are doing hundreds of insertions/deletions in the
      middle of the string, you might want to rethink your loop.
    - STACK BEHAVIOR: A `StringBuilder` can act exactly like a Stack for
      characters. You can "push" with `.append(char)` and "pop" with
      `.deleteCharAt(sb.length - 1)`.
                <-- yeah but I prefer using `ArrayDeque<Char>` lol...





 */





/*
Micro-Explanation
    To understand `StringBuilder`, you first have to understand why standard
    `String`s can be dangerous. In Kotlin and Java, a standard `String` is
    IMMUTABLE--once it is created in memory, its size and contents can never be
    changed.

    If you write `var test = "A"` and then loop 10,000 times doing `text += "B"`
    , the computer does not just add "B" to the end. It is forced to create a
    brand new string, copy "A", and "B" and then throw the old string away...
    On the next loop... This constant copying causes massive memory waste and
    drops your algorithm to an extremely slow O(N^2) time complexity.

    `StringBuilder` is the solution. It is essentially a wrapper around a
    mutable, resizable character array (just like an `ArrayList` or the
    `ArrayDeque` you've used). When you create it, it allocates a "buffer"--a
    chunk of empty slots in memory. When you use `.append()`, it simply drops
    the new characters into the empty slots in O(1) time. If the buffer fills
    up, it creates a new array (usually double the size) behind the scenes, copies
    the data once, and keeps going.


Syntax Panel: Key Methods
    `append(value)`
        - Add characters to the absolute end of the buffer.
        - Amortized O(1)
    `insert(index, value)`
        - Pushes existing characters to the right and drops the new value in at
          the specififed index.
        - O(N) (Requires shifting)
    `delete(start, end)`
        - Removes a chunk of characters and shifts the remaining ones to the
          left to close the gap.
        ...
    `reverse`
        - Swaps the characters in the aray from front to back in-place.
    `toString()`
        - Locks the data in and return a standard, immutable `String`.



---
fun buildDroneLog(): String {
    // 1. Initialise the builder.
    //    We can pass a starting string, or leave it empty.
    val logBuilder = StringBuilder("Flight Log:\n")

    // 2. Append data in a loop (Lightning fast!)
    for (i in 1..3) {
        logBuilder.append("Waypoint ").append(i).append(" reached.\n")
    }

    // 3. Insert something we forgot at the beginning (after "Flight Log:\n")
    logBuilder.insert(12, "Status: SUCCESS\n")

    return logBuilder.toString // Convert it back to a normal String when we are totally finished.
}



























* */