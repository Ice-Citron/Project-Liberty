package a40009_imperial.structurespack01.tree

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Test

internal class AlphaTreeTest {
    @Test
    fun singletonTreeStoresOneCharacter() {
        val tree = AlphaTree.singleton('x', 4)

        assertThat(tree.isEmpty, equalTo(false))
        assertThat(tree.isSingleton, equalTo(true))
        assertThat(tree.frequency, equalTo(4))
        assertThat(tree.chars, equalTo(setOf('x')))
        assertThat(tree.codeFor('x'), equalTo(""))
        assertThat(tree.codeFor('y'), equalTo(null))
    }

    @Test
    fun combinedTreeSumsFrequencyAndUsesLeftRightCodes() {
        val left = AlphaTree.singleton('a', 2)
        val right = AlphaTree.singleton('b', 5)
        val tree = AlphaTree.combine(left, right)

        assertThat(tree.isSingleton, equalTo(false))
        assertThat(tree.frequency, equalTo(7))
        assertThat(tree.chars, equalTo(setOf('a', 'b')))
        assertThat(tree.codeFor('a'), equalTo("0"))
        assertThat(tree.codeFor('b'), equalTo("1"))
        assertThat(tree.leafCharsInOrder(), equalTo(listOf('a', 'b')))
    }

    @Test
    fun buildsFrequencyTreeFromText() {
        val tree = AlphaTree.fromText("abcbdecc")

        assertThat(tree.isEmpty, equalTo(false))
        assertThat(tree.frequency, equalTo(8))
        assertThat(tree.chars, equalTo(setOf('a', 'b', 'c', 'd', 'e')))
        assertThat(tree.contains('c'), equalTo(true))
        assertThat(tree.contains('z'), equalTo(false))
        assertThat(tree.leafCharsInOrder().toSet(), equalTo(setOf('a', 'b', 'c', 'd', 'e')))
        assertThat(tree.chars.mapNotNull { tree.codeFor(it) }.toSet().size, equalTo(5))
    }

    @Test
    fun emptyInputBuildsAnEmptyTree() {
        val tree = AlphaTree.fromText("")

        assertThat(tree.isEmpty, equalTo(true))
        assertThat(tree.frequency, equalTo(0))
        assertThat(tree.chars, equalTo(emptySet()))
        assertThat(tree.codeFor('a'), equalTo(null))
    }
}
