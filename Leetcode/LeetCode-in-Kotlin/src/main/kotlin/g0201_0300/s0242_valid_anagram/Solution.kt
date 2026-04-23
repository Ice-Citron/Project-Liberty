package g0201_0300.s0242_valid_anagram

// #Easy #String #Hash_Table #Sorting #Data_Structure_I_Day_6_String
// #Programming_Skills_I_Day_11_Containers_and_Libraries #Udemy_Strings #Top_Interview_150_Hashmap

class Solution {
    fun isAnagram(s: String, t: String): Boolean {
        val sArr = s.toCharArray() // s.toList().sorted() //
        val tArr = t.toCharArray()
        sArr.sort()
        tArr.sort()
        if (sArr.contentEquals(tArr)) return true
        return false
    }
}




/*
    In Kotlin, `.sort()` on a `CharArray`, `IntArray`, or `FloatArray` performs
    an IN-PLACE sort that rearranges the elements into ASCENDING ORDER (lowest
    to highest). For characters, this follows the Unicode/ASCII values, which
    effectively means alphabetical order for standard letters (`'a'` comes
    before `'b'`). For numbers and floats, it sorts from the smallest numerical
    value to the largest. Because this is an "in-place" operation, ...
    ... function returns `Unit`... rather than a new array.
* */




class Solution2 {
    fun isAnagram(s: String, t: String): Boolean {
        return false
    }
}