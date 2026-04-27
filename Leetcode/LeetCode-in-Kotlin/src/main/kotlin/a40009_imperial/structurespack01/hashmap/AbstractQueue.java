package a40009_imperial.structurespack01.hashmap;




// What is the exam mental model for a handwritten hash map?
/*
    An array of buckets. Each bucket stores a linked list of entries that
    collided into the same index.
* */

import java.sql.Array;

class HashMap<K, V> {
    private Node<K, V>[] bucketList = Array;

    private class Node<K, V> {

    }
}


class BareboneHashMap<K, V>



















/*
    At minimum: `hasNext()` and `next()`. Throw `NoSuchElementException` if
    `next()` has no element.
* */
class object8 implements Iterable<T> {
    @Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            private Node<T> current = head;

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public T next() {
                if (current == null) {
                    throw new NoSuchElementException();
                }
                T value = current.value;
                current = current.next;
                return value;
            }
        };
    }

    private class Node<T> {
        Node<T> next;
        T value;
    }
}





/*
    The common Kotlin pattern is: a class that wants to be usable in a `for`
    loop implements `Iterable<T>`, then defines `override fun iterator(): Iterator<T>`
    . The returned `Iterator<T>` object must implement two methods: `hasNext()`,
    which answers is "is there another item?", and `next()`, which returns the
    current item and advances the cursor. Usually the iterator object stores
    some private cursor state, such as `index`, `currentNode`, `blockIndex // localIndex`
    , or `nextNode`.

    There are two common cursor styles. For arrays/list/blocks, you usually store
    a CURRENT POSITION like `index = 0`, read `items[index]`, then increment.
    For linked lists, you often store a NEXT POINTER instead: `nextNode` points
    to the node that will be returned next; `hasNext()` checks `nextNode != null`
    ; `next()` saves `nextNode.value`, then advances `nextNode = nextNode.next`.
    So the mental model is always the same: `hasNext()` checks whether the cursor
    is still valid, and `next()` consumes the current item and moves the cursor
    forward.

* */













// What methods must a Java `Iterator<T>` implement?
    // At minimum: `hasNext()` and `next()`. Throw `NoSuchElementException` if
    // `next()` has no element.

class RuchaIterator<T> implements Iterator<T> {
    public RuchaIterator() {

    }

    @Override
    public boolean hasNext() {
        return false;
    }

    @Override
    public T next() {
        return null;
    }
}












/*
    This is one of the most ... answer depends entirely on whether you TYPE THE
    CONSTRUCTOR YOURSELF or let Java auto-generate it for you.


Micro-explanation
    If you explicitly type a constructor with no modifier--like `MyClass() {}`--
    it gets PACKAGE-PRIATE visibily. It does not default to `public`, and it
    does not copy the class's modifier. It strictly means that only other classes
    living in the exact same folder (package) can use that constructor.

    However, the reason you probably suspect it matches the class modifier is
    because of Java's "invisible" defualt constructor. If you do not write a
    constructor at all, the Java compiler steps in and creates an invisible, empty
    one for you behind the scenes. THIS INVISIBLE CONSTRUCTOR PERFECLY COPIES THE
    VISIBILITY OF THE CLASS. So an empty `public class` gets a hidden `public`
    constructor, and a `private` class gets a hidden `private co...`

* */


// How do you implement a FIFO queue efficiently with a linked list?
class LinkedList<T> {
    Node<T> head = null;
    Node<T> tail = null;
    int size = 0;

    // LinkedList() {}
    public void enqueue(T item) {
        Node<T> newNode = new Node<>(item);
        if (tail == null) {
            head = newNode;
            tail = newNode;
        } else {
            tail.next = newNode;
            tail = newNode;
        }
        size++;
    }

    public T dequeue() {
        if (head == null) return null;
        T value = head.item;
        head = head.next;
        if (head == null) tail = null;
        size--;
        return value;
    }
        // The `tail = null` icase is a common exam bug.

    private static class Node<T> {
        Node<T> next;
        T item;

        public Node(T item) {
            this.item = item;
        }
    }
}









// What is the standard Java singly-linked-list node pattern?

class LinkedQueue<T> {
    private Node<T> head;
    private Node<T> tail;
    private int size;

    public static class Node<T> {
        final T value;
        Node<T> next;

        Node(T value) {
            this.value = value;
        }
    }
}

/*
* Use `static` for the nested node in Java unless it must access the outer
* object.
*
* Use a private static nested class, then keep head, sometimes tail, and a
* manual size counter.
* */




















/*
What is the key difference between a Java array and ArrayList?

    An array has fixed length. `ArrayList` is a resizing-array collection with
    changing size.






* */
public static void main(String[] args) {
    int[] marks = new int[10];
    marks[0] = 72;

    ArrayList<String> names = new ArrayList<>();
    names.add("Ada");
    names.add("Grace");

    int arrayLength = marks.length;
    int listSize = names.size();
}
    // ... often asks you to build the resizing behaviour yourself, store `size`
    // separately from array capacity.























/*
What is the difference between `Comparable` and `Comparator`

    `Comparable` defines the object's natural ordering. `Comparator` is an
    external ordering strategy.
* */

class Song implements Comparable<Song> {
    private final String title;
    private final int plays;

    Song(String title, int plays) {
        this.title = title;
        this.plays = plays;
    }

    @Override
    public int compareTo(Song other) {
        int byPlays = Integer.compare(this.plays, other.plays);
        if (byPlays != 0) return byPlays;
        return this.title.compareTo(other.title);
    }
}
    // Exam heuristic: implement `Comparable` when the prompt asks the class
    // itself to be ordered.

















public abstract class AbstractQueue<T> implements Queue<T> {
    private int size = 0;

    protected void incrementSize() { size++; }

    public int size() { return size; }
}

/*
    Use an `interface` for a capability contract. Use an `abstract class` when
    subclasses share real state or common implementation.
* */


// How do Java generics express that values must be comparable?
/*
    Use a bounded type parameter. The common exam-safe form is `T` extends
    `Comparable<T>`.
* */
class OrderedBox<T extends Comparable<T>> {
    private T best;

    public void offer(T value) {
        if (best == null || value.compareTo(best) > 0) { best = value; }
    }

    public T best() {
        return best;
    }
}
// <T extends Comparable<T>>            <-- This is relevant for trees, priority
// queues, Huffman-like structures, and sorted collections.





// What is dynamic dispatch in Java inheritance?
/*
    Dynamic Dispatch (often called runtime polymorphism) is the mechanism in
    Java where a class to an overriden method is resolved at runtime rather at
    compile time. When you have a parent class and a child class that share the
    exact same method signature, Java looks at the actual object residing in
    memory to decide which version of the method to execute, rather than relying
    on the reference type of the variable holding it.



* */