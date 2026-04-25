package a40009_imperial.structurespack01.tree

class AlphaTree(
    val chars: Set<Char> = emptySet(),
    val frequency: Int = 0,
    val left: AlphaTree? = null,
    val right: AlphaTree? = null,
) {
    val isEmpty: Boolean
        get() = TODO("practice")

    val isSingleton: Boolean
        get() = TODO("practice")

    fun contains(char: Char): Boolean {
        TODO("practice")
    }

    fun codeFor(char: Char): String? {
        TODO("practice")
    }

    fun leafCharsInOrder(): List<Char> {
        TODO("practice")
    }

    companion object {
        fun singleton(char: Char, frequency: Int): AlphaTree {
            TODO("practice")
        }

        fun combine(left: AlphaTree, right: AlphaTree): AlphaTree {
            TODO("practice")
        }

        fun fromText(text: String): AlphaTree {
            TODO("practice")
        }
    }
}
