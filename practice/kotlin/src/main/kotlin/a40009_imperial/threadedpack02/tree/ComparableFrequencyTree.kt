package a40009_imperial.threadedpack02.tree

class ComparableFrequencyTree<T : Comparable<T>>(
    val values: Set<T> = emptySet(),
    val frequency: Int = 0,
    val minValue: T? = null,
    val left: ComparableFrequencyTree<T>? = null,
    val right: ComparableFrequencyTree<T>? = null,
) : Comparable<ComparableFrequencyTree<T>> {
    val isEmpty: Boolean
        get() = TODO("practice")

    val isSingleton: Boolean
        get() = TODO("practice")

    override fun compareTo(other: ComparableFrequencyTree<T>): Int {
        TODO("practice")
    }

    fun contains(value: T): Boolean {
        TODO("practice")
    }

    fun codeFor(value: T): String? {
        TODO("practice")
    }

    fun valuesInOrder(): List<T> {
        TODO("practice")
    }

    companion object {
        fun <T : Comparable<T>> singleton(value: T, frequency: Int): ComparableFrequencyTree<T> {
            TODO("practice")
        }

        fun <T : Comparable<T>> combine(
            left: ComparableFrequencyTree<T>,
            right: ComparableFrequencyTree<T>,
        ): ComparableFrequencyTree<T> {
            TODO("practice")
        }

        fun <T : Comparable<T>> fromFrequencies(frequencies: Map<T, Int>): ComparableFrequencyTree<T> {
            TODO("practice")
        }
    }
}
