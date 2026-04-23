package g0101_0200.s0121_best_time_to_buy_and_sell_stock

// #Easy #Top_100_Liked_Questions #Top_Interview_Questions #Array #Dynamic_Programming
// #Data_Structure_I_Day_3_Array #Dynamic_Programming_I_Day_7 #Level_1_Day_5_Greedy #Udemy_Arrays
// #Top_Interview_150_Array/String #Big_O_Time_O(N)_Space_O(1)

class Solution {
    fun maxProfit(prices: IntArray): Int {
        var minPrice: Int = Int.MAX_VALUE
        var maxProfit: Int = 0
        for (currentPrice in prices) {
            minPrice = minOf(minPrice, currentPrice)
            maxProfit = maxOf(maxProfit, currentPrice - minPrice)
        }
        return maxProfit
    }
}
