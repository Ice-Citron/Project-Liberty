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















* */