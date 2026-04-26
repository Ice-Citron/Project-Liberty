package a40009_imperial.threadedpack02.testwriting

class ReadyGradeBook {
    private val marksByCid: MutableMap<String, MutableList<Int>> = mutableMapOf()

    fun recordMark(cid: String, mark: Int) {
        require(cid.isNotBlank()) { "cid must not be blank" }
        require(mark in 0..100) { "mark must be between 0 and 100" }
        marksByCid.getOrPut(cid) { mutableListOf() }.add(mark)
    }

    fun marksFor(cid: String): List<Int> {
        return marksByCid[cid]?.toList() ?: emptyList()
    }

    fun averageFor(cid: String): Double? {
        val marks = marksByCid[cid] ?: return null
        if (marks.isEmpty()) {
            return null
        }
        return marks.average()
    }

    fun topStudent(): String? {
        return rankedStudents().firstOrNull()
    }

    fun rankedStudents(): List<String> {
        return marksByCid.keys.sortedWith(
            compareByDescending<String> { averageFor(it) ?: Double.NEGATIVE_INFINITY }
                .thenBy { it },
        )
    }

    fun passCount(passMark: Int = 40): Int {
        require(passMark in 0..100) { "passMark must be between 0 and 100" }
        return marksByCid.keys.count { cid ->
            val average = averageFor(cid)
            average != null && average >= passMark
        }
    }
}
