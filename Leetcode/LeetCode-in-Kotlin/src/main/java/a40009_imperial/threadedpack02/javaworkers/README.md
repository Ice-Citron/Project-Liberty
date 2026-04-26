# Question 3: Java Parallel Word Counter

Suggested value: 15 marks

Write this question in Java. The goal is to practise Java interoperability and the `Runnable`/`Thread` pattern.

You should edit only:

```text
ParallelWordCounter.java
```

Use the public tests in:

```text
ParallelWordCounterTest.kt
```

## Background

You are given several lines of text. Words are separated by whitespace. Empty lines and empty word fragments should be ignored.

You must count how many times each word appears.

## 1. Worker

Class involved:

```java
public static final class Worker implements Runnable
```

### Implementation Requirement

Each worker receives:

- the full list of lines;
- a starting index, inclusive;
- an ending index, exclusive.

Its `run()` method should count words only in that range and store the result in its private `counts` map.

### Restriction

Do not start new threads inside `Worker.run()`. A worker is the body that a `Thread` executes.

## 2. `countWords`

Method involved:

```java
public Map<String, Integer> countWords(List<String> lines, int threadCount)
```

### Implementation Requirement

Reject non-positive `threadCount` values with `IllegalArgumentException`.

Split the input lines into roughly equal ranges. Create one `Worker` per range, wrap each worker in a `Thread`, start all threads, then join all threads.

After every worker has finished, merge their maps into one final result map.

### Restriction

Use `Runnable` and `Thread` directly.

Do not use:

- `ExecutorService`;
- `parallelStream`;
- Kotlin coroutines;
- shared mutation of one global map from every worker.

Workers may safely use private maps because those maps are merged only after `join()`.

### Example

For:

```text
alpha beta
beta gamma alpha
delta beta
```

the result should be:

```text
alpha -> 2
beta -> 3
gamma -> 1
delta -> 1
```
