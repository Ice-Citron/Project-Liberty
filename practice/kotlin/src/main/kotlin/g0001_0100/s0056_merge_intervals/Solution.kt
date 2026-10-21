package g0001_0100.s0056_merge_intervals

// #Medium #Top_100_Liked_Questions #Top_Interview_Questions #Array #Sorting
// #Data_Structure_II_Day_2_Array #Level_2_Day_17_Interval #Udemy_2D_Arrays/Matrix
// #Top_Interview_150_Intervals #Big_O_Time_O(n_log_n)_Space_O(n)
// #2023_07_10_Time_334_ms_(80.67%)_Space_43.7_MB_(78.81%)

class Solution {
    fun merge(intervals: Array<IntArray>): Array<IntArray> {
        // 1. Sort the intervals based on their start times
        intervals.sortBy { it[0] }

        // 2. Create a safe, new list to hold our merged results
        val result = mutableListOf<IntArray>()

        // 3. Iterate through the sorted intervals exactly once
        for (interval in intervals) {
            // If the results list is empty, OR there is no overlap
            // (current start time > last merged end time)
            if (result.isEmpty() || result.last()[1] < interval[0]) {
                result.add(interval)
            } else {
                result.last()[1] = maxOf(result.last()[1], interval[1])
            }
        }

        // 4. Convert our dynamic list back into the required Array of Arrays
        return result.toTypedArray()
    }
}


class Solution2 {
    fun merge(intervals: Array<IntArray>): Array<IntArray> {
        val results: MutableList<IntArray> = intervals.toMutableList()
        var i = 0
        while (i < results.size) {
            var min = results[i][0]
            var max = results[i][1]
            var j = i + 1
            while (j < results.size) {

                if (results[j][0] > min && results[j][1] < max) continue
                if (results[j][0] < min) min = results[j][0]
                if (results[j][1] > max) max = results[j][1]
                results.removeAt(i)
                results.removeAt(j-1)
                results.add(i, intArrayOf(min, max))
                j++
            }
            i++
        }
        println(results.joinToString(", "))
        return Array(results.size) { results[it] }
    }
}



class Solution3 {
    fun merge(intervals: Array<IntArray>): Array<IntArray> {
        intervals.sortBy { it[0] }
        val result = mutableListOf<IntArray>()

        for (interval in intervals) {
            if (result.isEmpty() || result.last()[1] < interval[1]) {
                result.add(interval)
            } else {
                result.last()[1] = maxOf(result.last()[1], interval[1])
            }
        }

        return result.toTypedArray()
    }
}





















