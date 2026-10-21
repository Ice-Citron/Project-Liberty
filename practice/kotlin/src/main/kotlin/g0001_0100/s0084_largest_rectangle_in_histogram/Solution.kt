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
                    i
                } else {
                    i - stack.last() - 1        // The distance between the right wall and the left wall
                }
                maxArea = maxOf(maxArea, h * w)
            }
            stack.addLast(i)
        }
        while (stack.isNotEmpty()) {
            val poppedIndex = stack.removeLast()
            val h = heights[poppedIndex]

            val w = if (stack.isEmpty()) {
                heights.size
            } else {
                heights.size - stack.last() - 1
            }
            maxArea = maxOf(maxArea, h * w)
        }
        return maxArea
    }
}


/*
    ... exact line of code that makes the Monoonic Stack so confusing to read,
    but you have actually ... onto the exact secret of how it works!

    You are 100% correct: we ARE calling a different index. And that is
    exactly what we want to do.

    If we just used `poppedIndex`, we would only know where the bar is. But to#
    calculate a width, we need to know where the bar stops.














* */













/*
    A monotonic stack is a specialised data structure in LeetCode used to solve
    "next/previous greater/smaller element" problems in O(N) time. It maintains
    elements in strictly increasing or decreasing order by popping elements that
    violate this order, allowing efficient tracking of boundaries for array-based
    questions. Common patterns are monotonic increasing (for smaller elements)
    and decreasing (for greater eleements).


Key LeetCode Usage Example
    - Next Greater Element: Finding the next larger number for each element.
    - Daily Temperatures: Calculating how many days to wait for a warmer
      temperature.
    - Largest Rectangle in Histogram: Finding the largest rectangular area by
      identifying the left and right boundary (smaller element) for each bar.
    - Remove Duplicate Letters / Smallest Subsequence: ...
    - Trapping Rain Water: Determining the boundaries for water accumulation.
    - Sum of Subarray Minimums: Finding the boundaries where an element is the
      minimum.
      ...








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













