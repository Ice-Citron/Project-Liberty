package g0001_0100.s0042_trapping_rain_water

import com_github_leetcode.TreeNode

// #Hard #Top_100_Liked_Questions #Top_Interview_Questions #Array #Dynamic_Programming #Stack
// #Two_Pointers #Monotonic_Stack #Dynamic_Programming_I_Day_13
// #Top_Interview_150_Array/String #Big_O_Time_O(n)_Space_O(1)
// #2023_07_07_Time_174_ms_(99.66%)_Space_40.8_MB_(97.53%)

class Solution {
    fun trap(height: IntArray): Int {
        // Edge case: an empty array can't trap water
        if (height.isEmpty()) return 0

        var left: Int = 0
        var right: Int = height.size - 1

        var leftMax: Int = 0
        var rightMax: Int = 0
        var totalWater: Int = 0

        while (left < right) {
            // We always process the side with the SHORTER current elevation
            if (height[left] < height[right]) {
                // Processing the Left side
                leftMax = maxOf(leftMax, height[left])
                totalWater += leftMax - height[left]
                left++
            } else {
                // Processing the Right Side (height[right] <= height[left])
                rightMax = maxOf(height[right], rightMax)
                totalWater += rightMax - height[right]
                right--
            }
        }
        return totalWater
    }
}


/*
    Welcome to the major leagues! LeetCode Hards can be intimidating, but this
    specific problem is actually a masterpiece of algorithmic design. Since you
    just conquered 3Sum, you are already primed for the absolute best way to
    solve this: TWO POINTERS.

    Before we write a single line of code, we need to understand the physics
    of a puddle.


Micro-explanation
    Instead of looking at the whole array at once, zoom in on just ONE SINGLE
    VERTICAL COLUMN (let's say index `i`).

    Ask yourself: How much water can sit directly above this one specific
    block?
        To trap water, this block needs a wall somewhere to its left, and a wall
        somewhere to its right. The water will fill up to the height of the
        SHORTER of those two tallest walls.

        The math for a single column is always:

`Water = min(TallestWallOnLeft, TallestWallOnRight) - CurrentHeight`
        (If the math gives you a negative number, it just traps 0 water).



---
Syntax Panel
    To achieve O(n) time and O(1) space, we put a pointer at the fat left and a
    pointer at the far rifht. We also need to keep track of the tallest wall
    we've seen on each side.


```
// 1. Setup our pointers and max trackers
var left = 0
var right = height.size - 1

var leftMax = 0
var rightMax = 0
var totalWater = 0

// 2. We process until the pointers collide
while (left < right) {
    // 3. We always process the side with the SHORTER current wall

}
```

















* */








class preOrderBST(root: TreeNode?) {
    private val stack: ArrayDeque<TreeNode> = ArrayDeque()

    init {
        if (root != null) stack.addLast(root)
    }

    fun next(): Int? {
        val top = stack.removeLast()
        if (top.right != null) stack.addLast(top.right!!)
        if (top.left != null) stack.addLast(top.left!!)
        return top.`val`
    }

    fun hasNext(): Boolean = stack.isNotEmpty()
}


class PostOrder_BST(root: TreeNode?) {
    private val stack: ArrayDeque<TreeNode> = ArrayDeque()

    init {
        if (root != null) stack.addLast(root)
    }

    private fun slideDown(node: TreeNode?) {
        var curr = node
        while (curr != null) {
            stack.addLast(curr)
            curr = if (curr.left == null) curr.left else curr.right
        }
    }

    fun next(): Int {
        val topNode: TreeNode = stack.removeLast()      // We hit a leaf
        if (stack.isNotEmpty() && stack.last().left == topNode)
            slideDown(stack.last().right)
        return topNode.`val`
    }

    fun hasNext(): Boolean = stack.isNotEmpty()
}


/*
Syntax Panel: Traversal Stack Logic
- Pre-order
   - Stack Push Strategy: Push Root. In `next()`, push Right, then push left.
   - When to return `node.val` Immediately upon popping.

- In-ORDER




   ... An `ArrayDeque` acting as a stack is LIFO. The very last thing you put
   into the box is the first thing you take out.

   If you want to process the LEFT child immediately after the NODE,  we have to
   make sure the left child is sitting at the very top of the stack. To get it
   to the top, we must push the Right child into the box first, and then put
   the Left child in after it.

















* */