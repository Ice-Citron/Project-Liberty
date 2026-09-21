package a40009_imperial.structurespack01.hashmap;

// When should you use an interface, and when should you use an abstract class?
public interface Queue<T> {
    void enqueue(T item);
    T dequeue();
    boolean isEmpty();
}

