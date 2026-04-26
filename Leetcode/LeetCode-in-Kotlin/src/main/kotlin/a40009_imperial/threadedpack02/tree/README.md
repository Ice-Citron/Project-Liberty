# Question 4: Comparable Frequency Tree

Suggested value: 20 marks

Implement a frequency tree whose nodes are themselves comparable. This is a harder version of the earlier tree question: the tree must not only store frequencies and paths, but also define a deterministic natural ordering.

You should edit only:

```text
ComparableFrequencyTree.kt
```

Use the public tests in:

```text
ComparableFrequencyTreeTest.kt
```

## Background

`ComparableFrequencyTree<T>` is generic:

```kotlin
class ComparableFrequencyTree<T : Comparable<T>> : Comparable<ComparableFrequencyTree<T>>
```

The stored values must be comparable so that ties can be broken deterministically.

## 1. Representation

Properties involved:

```kotlin
val values: Set<T>
val frequency: Int
val minValue: T?
val left: ComparableFrequencyTree<T>?
val right: ComparableFrequencyTree<T>?
```

### Implementation Requirement

An empty tree has no values, frequency `0`, and `minValue == null`.

A singleton tree contains exactly one value and has no children.

An internal tree combines two subtrees. Its `frequency` is the sum of the child frequencies. Its `minValue` is the smallest value contained anywhere in the tree.

## 2. `compareTo`

Method involved:

```kotlin
override fun compareTo(other: ComparableFrequencyTree<T>): Int
```

### Implementation Requirement

Trees should be ordered by:

1. lower frequency first;
2. if frequencies tie, lower `minValue` first.

### Restriction

Do not compare by set string representation or by object identity.

## 3. `singleton` and `combine`

Methods involved:

```kotlin
fun singleton(value: T, frequency: Int): ComparableFrequencyTree<T>
fun combine(left: ComparableFrequencyTree<T>, right: ComparableFrequencyTree<T>): ComparableFrequencyTree<T>
```

### Implementation Requirement

`singleton` creates one leaf. `combine` creates a new parent whose left and right children are exactly the arguments.

Left edges contribute `0`; right edges contribute `1`.

## 4. `contains`, `codeFor`, and `valuesInOrder`

Methods involved:

```kotlin
fun contains(value: T): Boolean
fun codeFor(value: T): String?
fun valuesInOrder(): List<T>
```

### Implementation Requirement

`contains` checks membership. `codeFor` returns the path to a value, or `null` if absent. `valuesInOrder` returns leaf values from left to right.

## 5. `fromFrequencies`

Method involved:

```kotlin
fun fromFrequencies(frequencies: Map<T, Int>): ComparableFrequencyTree<T>
```

### Implementation Requirement

Create singleton trees from positive frequencies. Repeatedly remove the two lowest-priority trees according to `compareTo`, combine them, and reinsert the combined tree until one tree remains.

For deterministic behaviour, insert initial singleton trees in natural value order.

### Restriction

Do not store only a map from values to codes. Build an actual tree.

## Public Tests

The public tests check:

- singleton properties;
- `compareTo` frequency and tie-breaking;
- left/right codes after `combine`;
- deterministic tree construction from equal frequencies.
