package a40009_imperial.junitinterop03.q01;

public final class ExamLinkedListJava<T> {
    private Node<T> head = null;
    private int size = 0;

    public int getSize() {
        return size;
    }

    public T get(int index) {
        checkIndex(index, false);
        return nodeAt(index).element;
    }

    public void add(int index, T element) {
        checkIndex(index, true);
        if (index == 0) {
            head = new Node<>(element, head);
        } else {
            Node<T> previous = nodeAt(index - 1);
            previous.next = new Node<>(element, previous.next);
        }
        size++;
    }

    public T set(int index, T element) {
        checkIndex(index, false);
        Node<T> node = nodeAt(index);
        T old = node.element;
        node.element = element;
        return old;
    }

    public void clear() {
        head = null;
        size = 0;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("[");
        Node<T> current = head;
        while (current != null) {
            if (current != head) {
                builder.append(", ");
            }
            builder.append(current.element);
            current = current.next;
        }
        builder.append("]");
        return builder.toString();
    }

    private Node<T> nodeAt(int index) {
        Node<T> current = head;
        for (int i = 0; i < index; i++) {
            current = current.next;
        }
        return current;
    }

    private void checkIndex(int index, boolean allowEnd) {
        int upperBound = allowEnd ? size : size - 1;
        if (index < 0 || index > upperBound) {
            throw new IndexOutOfBoundsException("index " + index + " out of bounds for size " + size);
        }
    }

    private static final class Node<T> {
        T element;
        Node<T> next;

        Node(T element, Node<T> next) {
            this.element = element;
            this.next = next;
        }
    }
}
