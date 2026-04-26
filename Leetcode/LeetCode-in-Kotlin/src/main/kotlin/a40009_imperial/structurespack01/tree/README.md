# Question 4: Frequency Trees

Suggested value: 20 marks

This question is about implementing a binary frequency tree in Kotlin. It is inspired by AlphaTree and Huffman-style questions from older Java practical papers.

You should edit:

```text
src/main/kotlin/a40009_imperial/structurespack01/tree/AlphaTree.kt
```

Use the tests in:

```text
src/test/kotlin/a40009_imperial/structurespack01/tree/AlphaTreeTest.kt
```

## Background

A frequency tree stores characters and their occurrence counts.

Each leaf contains one character. Each internal node combines two subtrees and stores the total frequency of all characters below it.

The path from the root to a leaf is the character's code:

```text
left edge  -> 0
right edge -> 1
```

For example, if `'a'` is in the left subtree of the root, then its code starts with `"0"`.

The final representation should be made from `AlphaTree` objects, not just a map from characters to strings.

## 1. Representation and Basic Properties

Methods involved:

```kotlin
val chars: Set<Char>
val frequency: Int
val left: AlphaTree?
val right: AlphaTree?
val isEmpty: Boolean
val isSingleton: Boolean
```

### Implementation Requirement

An empty tree should contain no characters and have frequency `0`.

A singleton tree should contain exactly one character and have no children.

An internal tree should normally have a left and right child. Its `chars` set should contain exactly the characters stored in its leaves. Its `frequency` should be the total frequency of its leaves.

### Restriction

Do not treat a non-empty internal tree as a singleton just because it has one distinct character in its `chars` set. For this exercise, a singleton should be a leaf containing one character and no children.

### Example

```kotlin
val empty = AlphaTree()

empty.isEmpty == true
empty.frequency == 0
empty.chars == emptySet<Char>()
```

For a singleton:

```kotlin
val tree = AlphaTree.singleton('x', 4)

tree.isEmpty == false
tree.isSingleton == true
tree.frequency == 4
tree.chars == setOf('x')
```

## 2. `singleton`

Method involved:

```kotlin
fun singleton(char: Char, frequency: Int): AlphaTree
```

### Implementation Requirement

Return a tree containing exactly one leaf for `char` with the supplied frequency.

The resulting tree should have:

- `chars == setOf(char)`;
- `frequency == frequency`;
- no left child;
- no right child.

### Restriction

Do not create an internal parent node for a singleton tree.

The public tests use positive frequencies. You may still choose to reject non-positive frequencies if you want a stricter representation, but do not break the public expected behaviour.

### Example

```kotlin
val tree = AlphaTree.singleton('x', 4)

tree.contains('x') == true
tree.contains('y') == false
tree.codeFor('x') == ""
```

The empty string is the code for the only character in a singleton tree.

## 3. `combine`

Method involved:

```kotlin
fun combine(left: AlphaTree, right: AlphaTree): AlphaTree
```

### Implementation Requirement

Return a new tree whose left and right children are the two arguments.

The combined tree should contain all characters from both subtrees. Its frequency should be the sum of the child frequencies.

### Restriction

Do not mutate the input subtrees. Build a new parent tree.

The left argument must remain the left child, and the right argument must remain the right child. This matters for `codeFor`.

### Example

```kotlin
val left = AlphaTree.singleton('a', 2)
val right = AlphaTree.singleton('b', 5)
val tree = AlphaTree.combine(left, right)

tree.frequency == 7
tree.chars == setOf('a', 'b')
tree.codeFor('a') == "0"
tree.codeFor('b') == "1"
```

## 4. `contains`

Method involved:

```kotlin
fun contains(char: Char): Boolean
```

### Implementation Requirement

Return `true` exactly when the tree contains `char`.

The result should be consistent with `chars`.

### Restriction

Do not return `true` for an empty tree.

### Example

```kotlin
val tree = AlphaTree.combine(
    AlphaTree.singleton('a', 2),
    AlphaTree.singleton('b', 5),
)

tree.contains('a') == true
tree.contains('b') == true
tree.contains('z') == false
```

## 5. `codeFor`

Method involved:

```kotlin
fun codeFor(char: Char): String?
```

### Implementation Requirement

Return the left/right path to `char` as a string of `0` and `1` characters.

If `char` is not in the tree, return `null`.

For a singleton tree, the code for its only character should be the empty string.

### Restriction

Do not return a code for a character that is absent.

Do not use one shared mutable string builder in a way that leaks path state between recursive branches.

### Example

```kotlin
val tree = AlphaTree.combine(
    AlphaTree.singleton('a', 2),
    AlphaTree.singleton('b', 5),
)

tree.codeFor('a') == "0"
tree.codeFor('b') == "1"
tree.codeFor('z') == null
```

## 6. `leafCharsInOrder`

Method involved:

```kotlin
fun leafCharsInOrder(): List<Char>
```

### Implementation Requirement

Return the characters stored in leaves from left to right.

This should be based on the tree shape, not alphabetical ordering.

### Restriction

Do not simply return `chars.toList()` if that loses left-to-right tree order.

### Example

```kotlin
val tree = AlphaTree.combine(
    AlphaTree.singleton('a', 2),
    AlphaTree.singleton('b', 5),
)

tree.leafCharsInOrder() == listOf('a', 'b')
```

## 7. `fromText`

Method involved:

```kotlin
fun fromText(text: String): AlphaTree
```

### Implementation Requirement

Build a frequency tree from the characters in `text`.

The method should:

1. count the frequency of every character in `text`;
2. create singleton trees for characters with positive frequency;
3. repeatedly combine the two lowest-frequency trees;
4. return the final tree.

For empty input, return an empty tree.

### Tie-Breaking Rule

When two candidate trees have the same frequency during construction, treat the tree that was inserted later into the working collection as the one with higher priority for removal.

This mirrors the deterministic priority-queue rule used in AlphaTree-style practical papers.

### Restriction

Do not store only a map from characters to codes. Build an actual tree so that `left`, `right`, `chars`, `frequency`, `codeFor`, and `leafCharsInOrder` remain meaningful.

The public tests avoid depending on every exact code, but your implementation should still be deterministic.

### Example

For the text:

```text
abcbdecc
```

the frequencies are:

```text
a: 1
b: 2
c: 3
d: 1
e: 1
```

The final tree should have:

```kotlin
tree.frequency == 8
tree.chars == setOf('a', 'b', 'c', 'd', 'e')
tree.contains('c') == true
tree.contains('z') == false
```

Every character in the tree should have a distinct code:

```kotlin
tree.chars.mapNotNull { tree.codeFor(it) }.toSet().size == tree.chars.size
```

## Public Tests

The public tests check the following behaviours:

- singleton trees store one character;
- combined trees sum frequency and preserve left/right codes;
- `codeFor` returns `null` for absent characters;
- `leafCharsInOrder` follows tree order;
- `fromText` builds a tree with the correct total frequency and character set;
- empty input builds an empty tree.

Passing the public tests is not a proof of full correctness. You should also think through:

- strings containing one distinct character;
- several characters with equal frequency;
- calling `codeFor` on an empty tree;
- combining empty trees;
- preserving deterministic behaviour when priorities tie.
