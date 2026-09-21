package g0101_0200.s0200_number_of_islands

// #Medium #Top_100_Liked_Questions #Top_Interview_Questions #Array #Depth_First_Search
// #Breadth_First_Search #Matrix #Union_Find
// #Algorithm_II_Day_6_Breadth_First_Search_Depth_First_Search
// #Graph_Theory_I_Day_1_Matrix_Related_Problems #Level_1_Day_9_Graph/BFS/DFS #Udemy_Graph
// #Top_Interview_150_Graph_General #Big_O_Time_O(M*N)_Space_O(M*N)
// #2022_09_09_Time_252_ms_(95.41%)_Space_52.4_MB_(86.52%)

class Solution {
    fun sink(grid: Array<CharArray>, row: Int, col: Int) {
        if (row < 0 || col < 0 || row >= grid.size || col >= grid[0].size ||
            grid[row][col] == '0') return

        grid[row][col] = '0'
        sink(grid, row + 1, col)
        sink(grid, row, col + 1)
        sink(grid, row - 1, col)
        sink(grid, row, col - 1)
    }

    fun numIslands(grid: Array<CharArray>): Int {
        var accumulator: Int = 0
        for (row in 0..<grid.size) {
            for (col in 0..<grid[0].size) {
                if (grid[row][col] == '1') {
                    accumulator++
                    sink(grid, row, col)
                }
            }
        }
        return accumulator
    }
}





/*
    It is definitely a jump in complexity! This is a classic "Graph Traversal"
    problem. It looks intimidating, but the core logic is incredibly satisfying
    once you see the pattern.

    Think of it like flying over the ocean in a helicopter. You are scanning the
    grid row by row. When you spot a `1` (land), you know you've found a new
    island. But to make sure you don't count that same island again later, you
    parachute down and physically walk to every connected piece of land,
    crossing them off your map (changing the `1`s to `0`s). Once you've "sunk"
    that entire island, you get back in the helicopter and resume your scan.

    This "walking around" process is typical done using a recursive function
    called DFS.


MICRO-EXPLANATION
    You need two parts to solve this:

    1. A MAIN LOOP that iterates through every cell in the 2D array. When it
       finds a `1`, it increments the island count and calls a helper function.
    2. A RECURSIVE HELPER FUNCTION (let's call it `sink`) that takes a row and
       column. Its job is to turn the current `1` into a `0`, and then call
       itself on the 4 adjacent cells (up, down, left, right) to sink them too.
       It stops if it hits water or goes off the edge of the map.
* */

/*  ----    ----    ----    ----    ----    ----    ----    ----    ----    */


class preSolution() {
    fun sink(grid: Array<CharArray>, r: Int, c: Int) {
        // Base Case: If we are off the map, or standing on water (`0`), stop walking!
        if (r < 0 || r >= grid.size ||
            c < 0 || c >= grid[0].size || grid[r][c] == '0') {
            return
        }
        // We are standing on a '1'. Sink it so we never count it again.
        grid[r][c] = '0'

        // Walk down and walk right (for a full solution, you need all 4 directions).
        sink(grid, r + 1, c)
        sink(grid, r, c + 1)
    }
}






























