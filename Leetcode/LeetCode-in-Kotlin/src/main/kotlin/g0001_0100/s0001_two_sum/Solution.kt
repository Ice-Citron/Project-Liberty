package g0001_0100.s0001_two_sum

import com.sun.jdi.InconsistentDebugInfoException

// #Easy #Top_100_Liked_Questions #Top_Interview_Questions #Array #Hash_Table
// #Data_Structure_I_Day_2_Array #Level_1_Day_13_Hashmap #Udemy_Arrays #Top_Interview_150_Hashmap

class Solution {
    fun twoSum(numbers: IntArray, target: Int): IntArray {
        val memory: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
        for (i in 0..<numbers.size) {
            memory[numbers[i]] = i
        }
        for (currentIndex in 0..<numbers.size) {
            val complement = target - numbers[currentIndex]
            if (memory.containsKey(complement)) {
                if (currentIndex != memory[complement]!!) {
                    return if (currentIndex < memory[complement]!!) {
                        intArrayOf(currentIndex, memory[complement]!!)
                    } else {
                        intArrayOf(memory[complement]!!, currentIndex)
                    }
                }
            }
        }
        return intArrayOf(-1, -1)
    }
}


class SolutionSlow {
    fun twoSum(numbers: IntArray, target: Int): IntArray {
        for (i in 0..<numbers.size) {
            for (j in 0..<numbers.size) {
                if (numbers[i] + numbers[j] == target && i != j)
                    return intArrayOf(i, j)
            }
        }
        return intArrayOf(-1, -1)
    }
}



class SolutionGemini {
    fun twoSum(nums: IntArray, target: Int): IntArray {
        // Map to store the number we've seen and its index
        val seen = mutableMapOf<Int, Int>()

        // Loop through the array once (O(n) time complexity)
        for ((index, num) in nums.withIndex()) {
            val complement= target - num
            if (seen.containsKey(complement)) {
                return intArrayOf(seen[complement]!!, index)
            }
            // Otherwise, add the current number and its index to the map for future checks
            seen[num] = index
        }

        return intArrayOf(-1, -1)
    }
}






