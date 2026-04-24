package g0001_0100.s0084_largest_rectangle_in_histogram

// #Hard #Array #Stack #Monotonic_Stack
// #Top_Interview_150_Stack #Big_O_Time_O(n)_Space_O(n)
// #2023_07_14_Time_252_ms_(99.93%)_Space_52.7_MB_(95.06%)

class Solution {
    fun largestRectangleArea(heights: IntArray): Int {
        if (heights.size == 0) return 0
        val stack: ArrayDeque<Int> = ArrayDeque()
        var maxArea: Int = 0

        for (i in 0..<heights.size) {
            while(stack.isNotEmpty() && heights[i] < heights[stack.last()]) {
                val poppedIndex = stack.removeLast()
                val h = heights[poppedIndex]
                val w = if (stack.isEmpty()) {

                }
                maxArea = maxOf(maxArea, h * w)
                stack.removeLast()
            }
            stack.addLast(i)
        }
        while (stack.isNotEmpty()) {
            val tailArea = heights[stack.last()] * (heights.size - stack.last())
            maxArea = maxOf(maxArea, tailArea)
            stack.removeLast()
        }
        return maxArea
    }
}


/*

* */










/*
    This is one of the most ... since you just mastered using a Stack (`ArrayDeque`)
    for the `BSTIterator`, you actually already have the exact data structure
    you need to solve this in O(n) time!

    ... MONOTONIC STACK pattern.


Micro-Explanation
    To find the largest rectangle, you need to know how far every single bar can
    expand to the left and to the right before it hits a shorter bar (a "wall").

    Instead of looking outward from every bar (which takes O(n^2) time), we
    sweep from left to right and keep a Stack of bars that are currently able to
    expand. We only add a bar to the stack if it is TALLER than the one before it.

    The magic happens the moment we encounter a bar that is SHORTER than the top
    of our stack. This short bar acts as a right-side "wall". Because we found a
    wall, we know the taller bars in our stack annot expand any further to the
    right. We poop them off one by one, calculate the massive rectangle they formed,
    and record the max area.
















* */













