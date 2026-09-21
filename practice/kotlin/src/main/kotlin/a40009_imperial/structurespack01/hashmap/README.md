# Question 3: Ordered Hash Map

Suggested value: 30 marks

This question is about implementing a small generic data structure in Kotlin. It is intentionally closer to an Imperial practical exam question than to a short LeetCode prompt: you are given public method signatures, a set of behavioural requirements, and tests that check the observable behaviour.

You should edit:

```text
src/main/kotlin/a40009_imperial/structurespack01/hashmap/OrderedHashMap.kt
```

Use the tests in:

```text
src/test/kotlin/a40009_imperial/structurespack01/hashmap/OrderedHashMapTest.kt
```

## Background

A normal hash map lets you look up a value quickly from a key. However, a normal hash map does not necessarily remember the order in which keys were inserted.

In this question you will implement `OrderedHashMap<K, V>`, which should behave like a map while also remembering entries from oldest to newest.

The intended internal design is:

```text
hash buckets for key lookup
+ linked ordering structure for oldest-to-newest traversal
```

One node may therefore need to take part in two different structures at once:

```text
bucket chain:       node -> nextInBucket -> ...
insertion order:   older <-> node <-> newer
```

Do not solve this by wrapping Kotlin's `LinkedHashMap`.

## 1. Representation and Size

Methods involved:

```kotlin
val size: Int
val isEmpty: Boolean
```

### Implementation Requirement

Choose private fields that allow the map to support both efficient lookup and ordered traversal. A typical solution uses:

- an array of bucket heads;
- a private node class;
- a `head` reference for the oldest entry;
- a `tail` reference for the newest entry;
- an integer counter for the number of entries.

The `size` property should return the current number of key-value pairs in the map. The `isEmpty` property is already written in terms of `size`.

### Restriction

Do not compute `size` by walking the whole structure every time. Keep a counter and update it when entries are inserted or removed.

Do not use `LinkedHashMap` internally. The purpose of the question is to implement the linked hash map idea yourself.

### Example

After:

```kotlin
val map = OrderedHashMap<String, Int>(bucketCount = 4)
map.put("alpha", 1)
map.put("beta", 2)
```

then:

```kotlin
map.size == 2
map.isEmpty == false
```

For a new empty map:

```kotlin
map.size == 0
map.isEmpty == true
```

## 2. `put`

Method involved:

```kotlin
fun put(key: K, value: V): V?
```

### Implementation Requirement

If the key is not already present, insert a new entry and return `null`.

If the key is already present, replace the old value, return the old value, and move that existing entry to the newest position in the insertion ordering.

Your implementation must place new entries into a hash bucket selected using the key's hash code and `bucketCount`. It must still work when several different keys land in the same bucket.

### Restriction

Updating an existing key must not create a duplicate node for that key.

Do not rebuild the entire map when one key is updated. Relink the affected node instead.

### Example

```kotlin
val map = OrderedHashMap<String, Int>(bucketCount = 2)

map.put("a", 1) == null
map.put("b", 2) == null
map.keysInInsertionOrder() == listOf("a", "b")
```

After updating an existing key:

```kotlin
map.put("a", 10) == 1
map["a"] == 10
map.keysInInsertionOrder() == listOf("b", "a")
```

The key `"a"` is moved to the newest position because it was updated.

## 3. `get`

Method involved:

```kotlin
operator fun get(key: K): V?
```

### Implementation Requirement

Return the value associated with `key`, or `null` if the key is absent.

The lookup should search only the relevant bucket chain, not every entry in insertion order.

### Restriction

Calling `get` must not change the insertion ordering. This is not an access-order cache; it is an insertion/update-order map.

### Example

```kotlin
val map = OrderedHashMap<String, Int>()
map.put("crew", 8)

map["crew"] == 8
map["missing"] == null
map.keysInInsertionOrder() == listOf("crew")
```

## 4. `containsKey`

Method involved:

```kotlin
fun containsKey(key: K): Boolean
```

### Implementation Requirement

Return `true` exactly when the map currently contains the given key.

This method should be consistent with `get`: if `containsKey(key)` is false, then `get(key)` should return `null`.

### Restriction

Do not implement this by calling `keysInInsertionOrder()` and searching the returned list. That would make a lookup depend on a full ordered traversal.

### Example

```kotlin
val map = OrderedHashMap<String, Int>()
map.put("economy", 120)

map.containsKey("economy") == true
map.containsKey("first") == false
```

## 5. `remove`

Method involved:

```kotlin
fun remove(key: K): V?
```

### Implementation Requirement

If the key is present, remove its entry from both:

- the hash bucket chain;
- the insertion-order linked structure.

Return the removed value.

If the key is absent, leave the map unchanged and return `null`.

### Restriction

Removing from the bucket chain alone is not enough. If the removed entry is still linked into the ordered structure, later calls to `keysInInsertionOrder`, `valuesInInsertionOrder`, `removeOldest`, or `iterator` will be wrong.

### Example

```kotlin
val map = OrderedHashMap<String, Int>()
map.put("a", 1)
map.put("b", 2)
map.put("c", 3)

map.remove("b") == 2
map.keysInInsertionOrder() == listOf("a", "c")
map.remove("missing") == null
map.size == 2
```

Your code should also handle removing the oldest entry, the newest entry, and the only entry.

## 6. `removeOldest`

Method involved:

```kotlin
fun removeOldest(): Pair<K, V>?
```

### Implementation Requirement

Remove and return the oldest entry in the map.

If the map is empty, return `null`.

The returned value should be a Kotlin `Pair` containing the removed key and value.

### Restriction

This operation must also remove the entry from its hash bucket. If you only move the ordered `head` pointer, then `get` and `containsKey` may still find a supposedly removed key.

### Example

```kotlin
val map = OrderedHashMap<String, String>()
map.put("crew", "A")
map.put("business", "B")
map.put("economy", "C")

map.removeOldest() == ("crew" to "A")
map.keysInInsertionOrder() == listOf("business", "economy")
map.containsKey("crew") == false
```

For an empty map:

```kotlin
map.removeOldest() == null
```

## 7. Ordered Views

Methods involved:

```kotlin
fun keysInInsertionOrder(): List<K>
fun valuesInInsertionOrder(): List<V>
```

### Implementation Requirement

Return the current keys or values from oldest to newest.

The returned list should reflect the current map state after any sequence of insertions, updates, and removals.

### Restriction

The returned lists should be new lists. A caller should not be able to mutate your internal linked structure by changing the returned list.

### Example

```kotlin
val map = OrderedHashMap<String, Int>()
map.put("x", 1)
map.put("y", 2)
map.put("x", 10)

map.keysInInsertionOrder() == listOf("y", "x")
map.valuesInInsertionOrder() == listOf(2, 10)
```

## 8. Iterator

Method involved:

```kotlin
override fun iterator(): Iterator<Pair<K, V>>
```

### Implementation Requirement

Return an iterator over key-value pairs from oldest to newest.

The class declaration already says:

```kotlin
class OrderedHashMap<K, V>(...) : Iterable<Pair<K, V>>
```

so this method is what allows code such as:

```kotlin
for ((key, value) in map) {
    println("$key -> $value")
}
```

### Restriction

The iterator must use the insertion ordering, not the bucket ordering. Bucket ordering depends on hash codes and is not the required observable order.

### Example

```kotlin
val map = OrderedHashMap<Int, String>()
map.put(10, "ten")
map.put(20, "twenty")

map.iterator().asSequence().toList() == listOf(10 to "ten", 20 to "twenty")
```

## Public Tests

The public tests check the following behaviours:

- insertion, lookup, containment, and removal by key;
- update of an existing key and movement to newest position;
- removal of the oldest entry;
- iteration in insertion order.

Passing the public tests is not a proof of full correctness. You should also think through:

- all keys landing in the same bucket;
- removing from the middle of a bucket chain;
- updating the oldest entry;
- updating the newest entry;
- removing the only entry;
- alternating `put`, `remove`, and `removeOldest`.
