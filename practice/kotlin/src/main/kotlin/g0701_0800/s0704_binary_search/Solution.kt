package g0701_0800.s0704_binary_search

// #Easy #Array #Binary_Search #Algorithm_I_Day_1_Binary_Search #Binary_Search_I_Day_1
// #Level_1_Day_7_Binary_Search #Udemy_Binary_Search

class Solution {
    fun search(nums: IntArray, target: Int): Int {
        var left = 0
        var right = nums.size - 1
        while (left <= right) {
            val mid = left + (right - left) / 2
            if (nums[mid] == target) return mid
            else if (nums[mid] < target) left = mid + 1
            else right = mid - 1
        }
        return -1
    }
}
