# Kotlin LeetCode Practice Ladder

Use this repo in `practice mode`.

For the first batch, the implementation files have already been stripped back to `TODO(...)` so you can solve them yourself against the existing tests in IntelliJ.

## Batch 1

### 1. Two Sum

Files:

- `src/main/kotlin/g0001_0100/s0001_two_sum/Solution.kt`
- `src/test/kotlin/g0001_0100/s0001_two_sum/SolutionTest.kt`

Focus:

- `HashMap<Int, Int>`
- `for (i in numbers.indices)`
- returning `IntArray`

### 2. Contains Duplicate

Files:

- `src/main/kotlin/g0201_0300/s0217_contains_duplicate/Solution.kt`
- `src/test/kotlin/g0201_0300/s0217_contains_duplicate/SolutionTest.kt`

Focus:

- `HashSet<Int>`
- early return
- `for (n in nums)`

### 3. Valid Anagram

Files:

- `src/main/kotlin/g0201_0300/s0242_valid_anagram/Solution.kt`
- `src/test/kotlin/g0201_0300/s0242_valid_anagram/SolutionTest.kt`

Focus:

- strings
- arrays for frequency counts
- character arithmetic like `c - 'a'`

### 4. Binary Search

Files:

- `src/main/kotlin/g0701_0800/s0704_binary_search/Solution.kt`
- `src/test/kotlin/g0701_0800/s0704_binary_search/SolutionTest.kt`

Focus:

- `left`, `right`, `mid`
- while loops
- off-by-one discipline

### 5. Best Time to Buy and Sell Stock

Files:

- `src/main/kotlin/g0101_0200/s0121_best_time_to_buy_and_sell_stock/Solution.kt`
- `src/test/kotlin/g0101_0200/s0121_best_time_to_buy_and_sell_stock/SolutionTest.kt`

Focus:

- one-pass state tracking
- `minOf`, `maxOf` style thinking
- simple array scanning

## Batch 2

Do these after Batch 1:

1. `s0020_valid_parentheses`
2. `s0021_merge_two_sorted_lists`
3. `s0206_reverse_linked_list`
4. `s0739_daily_temperatures`
5. `s0155_min_stack`


## Imperial-Style Practice

After the LeetCode warmups, switch to the COMP40009 retrofit packs. These are larger than normal LeetCode problems and are closer to the Java/Kotlin practical exam style.

Start here:

- `01-imperial40009/structures-pack-01/README.md`

Batch 1 topics:

1. linked lists
2. queues
3. ordered hash maps
4. frequency trees
5. block iterators

## Rule For This Repo

When a problem already has an answer prewritten and you want to practice it, replace the method body with a `TODO(...)` first and solve it back from the tests. You do **not** need to hunt for some hidden “follow-up” unless you specifically want a harder extension.
