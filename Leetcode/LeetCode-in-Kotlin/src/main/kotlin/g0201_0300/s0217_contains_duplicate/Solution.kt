package g0201_0300.s0217_contains_duplicate

// #Easy #Top_Interview_Questions #Array #Hash_Table #Sorting #Data_Structure_I_Day_1_Array
// #Programming_Skills_I_Day_11_Containers_and_Libraries #Udemy_Arrays

class Solution {
    fun containsDuplicate(nums: IntArray): Boolean {
                // val seen: ArrayList<Int> = arrayListOf()
        // 1. Creating a mutable set (backed by Java's HashSet)
        val seen = mutableSetOf<Int>()

        for (value in nums) {
            // 2. Checking for existence (Instant lookup)
            if (value in seen) return true      // or `seen.contains(value)`
            seen.add(value)         // adding a value returns a boolean... returns true if successfully added, false if it was already in the set
        }
        return false
    }
}


