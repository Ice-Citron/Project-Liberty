package g0001_0100.s0015_3sum

// #Medium #Top_100_Liked_Questions #Top_Interview_Questions #Array #Sorting #Two_Pointers
// #Data_Structure_II_Day_1_Array #Algorithm_II_Day_3_Two_Pointers #Udemy_Two_Pointers
// #Top_Interview_150_Two_Pointers #Big_O_Time_O(n*log(n))_Space_O(n^2)
// #2023_07_03_Time_493_ms_(93.45%)_Space_53_MB_(93.97%)

class Solution {

    fun threeSum(nums: IntArray): List<List<Int>> {
        val result = mutableListOf<List<Int>>()
        nums.sort()
        for (i in 0..nums.size-2) {
            if (i > 0 && nums[i] == nums[i-1]) {
                continue            // Skips this iteration entirely
            }
            var left = i + 1
            var right = nums.size - 1
            while (left < right) {
                var sum = nums[left] + nums[right] + nums[i]
                if (sum < 0) {
                    left++
                } else if (sum > 0) {
                    right--
                } else {
                    result.add(listOf(nums[i], nums[left], nums[right]))
                    left++
                    right--         // Must immediately move pointers here, to prevent infintie loops
                    while (left < right && nums[left] == nums[left - 1]) {
                        left++
                    }
                }
            }
        }
        return result.toList().also { println(it.joinToString(",")) }
    }
}



/*
    At its core, 3Sum is just Two Sum disguised inside a loop. Think ... if you
    need three numbers to add up to 0... you can just lock in the first number
    (let's say it's -4). Now, your new goal is simply to find two other numbers
    in the rest of the array that adds up to +4 (because +4 -4 = 0).

    To avoid duplicate triplets and to make finding those two other numbers
    extremely fast, the absolute most important first step is to SORT THE
    ARRAY. Once sorted, you can use the exact same `left` and `right` pointer
    logic you learned.
* */



/*
    The biggest secret to cracking these practicals and algorithmic challenges
    is shifting your focus from memorising code to recoginising patterns. You've
    probably noticed already: problems like Two Sum and Group Anagrams are just
    the "Hash Map" pattern, while Valid Palindrome and 3Sum are the "Two
    Pointers" pattern. When you read  anew prompt, look for the literal keywords
    . If an array is already "sorted", that's a massive neon sign pointing
    to Binary Search or Two Pointers. If the problem asks about "frequencies",
    "uniqueness," or "complements," you should immediately be reaching for a Map
    or a Set.

    Second, always give yourself permission to think of the "dumb" brute-force
    solution first. When you're trying to build something fast under pressure,
    it's easy to get paralysed trying to write the perfect O(n) optimal solution
    on line one. Instead, figure out how you would solve it with terrible,
    nested `for` loops. Once you know the brute-force way, ask yourself: "What
    information am I calculating more than once?" or "What am I spending too
    much time searching for?" That exact bottleneck is where your optimised data
    structure slots in to save the day.

    Finally, never write your loop logic without "dry running" a tiny, nasty
    example first. As we saw with the infinite loop in 3Sum, the core logic is
    rarely the issue, it's the edge cases like `[0, 0, 0]` or getting stuck
    on duplicates that crash the system. Grab a piece of paper or open a comment
    block, draw your array, and physically move your mental `left` and `right`
    pointers through a weird edge case, translating that into Kotlin becomes
    trivial.
* */