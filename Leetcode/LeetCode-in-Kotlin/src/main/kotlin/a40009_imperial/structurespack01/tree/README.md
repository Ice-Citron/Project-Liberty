# Question 4: Frequency Trees

Suggested value: 20 marks

## Problem Description

In this question you will implement a binary frequency tree. This is inspired by the AlphaTree problem from the Java final papers and by older Huffman coding style questions.

A frequency tree stores characters together with occurrence counts. Each leaf contains one character. Each internal node combines two subtrees and stores the total frequency of all characters in those subtrees.

The path from the root to a leaf can be interpreted as a code for the character:

- moving to the left child contributes `0`;
- moving to the right child contributes `1`.

You are given a skeleton class:

- `AlphaTree`

## Getting Started

You should edit only:

- `AlphaTree.kt`

You should use the public tests in:

- `AlphaTreeTest.kt`

You may use Kotlin collections while constructing a tree, but the final representation should be made from `AlphaTree` objects.

## Required Behaviour

1. Empty and singleton trees. [4 marks]
   Implement `isEmpty`, `isSingleton`, and `singleton`.

   The empty tree should contain no characters and have frequency `0`.

   A singleton tree should contain exactly one character and should have the frequency provided to `singleton`.

2. Combining trees. [3 marks]
   Implement `combine(left, right)`.

   The combined tree should contain all characters from both subtrees. Its frequency should be the sum of the child frequencies. Its left and right children should be the arguments passed to `combine`.

3. Searching and codes. [5 marks]
   Implement `contains(char)` and `codeFor(char)`.

   `codeFor(char)` should return the path to `char` as a string of `0` and `1` characters. If the character is not in the tree, return `null`. For a singleton tree, the code for its only character should be the empty string.

4. Leaf order. [3 marks]
   Implement `leafCharsInOrder()`.

   This should return the characters stored in the leaves from left to right.

5. Building from text. [5 marks]
   Implement `fromText(text)`.

   The method should count the frequency of every character in `text`, create singleton trees for characters with positive frequency, and repeatedly combine the two lowest-frequency trees until one tree remains.

## Tie-Breaking Rule

The public tests avoid depending on the exact code assigned to every character, but your implementation should still be deterministic.

When two candidate trees have the same frequency during construction, treat the tree that was inserted later into the working collection as the one with higher priority for removal.

This mirrors the kind of deterministic priority-queue rule used in the Java AlphaTree-style paper.

## Restrictions

- Empty input should return an empty tree.
- Do not store only a map from characters to codes; build an actual tree.
- The `chars` property of a non-empty tree should contain exactly the characters in that tree.
- The `frequency` property of a non-empty tree should equal the total frequency of its leaves.

## Example

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
```

The exact code for each character depends on the order in which equal-frequency trees are combined, but each character in the tree should have a distinct code.

## Edge Cases To Think About

- Empty string.
- String containing one distinct character.
- Several characters with equal frequency.
- Calling `codeFor` for a character that is absent.
