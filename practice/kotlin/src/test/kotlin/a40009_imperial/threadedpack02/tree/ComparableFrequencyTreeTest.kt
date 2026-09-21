package a40009_imperial.threadedpack02.tree

import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Test

internal class ComparableFrequencyTreeTest {
    @Test
    fun singletonTreesExposeValueAndFrequency() {
        val tree = ComparableFrequencyTree.singleton('x', 4)

        assertThat(tree.isEmpty, equalTo(false))
        assertThat(tree.isSingleton, equalTo(true))
        assertThat(tree.frequency, equalTo(4))
        assertThat(tree.values, equalTo(setOf('x')))
        assertThat(tree.minValue, equalTo('x'))
        assertThat(tree.codeFor('x'), equalTo(""))
        assertThat(tree.codeFor('y'), equalTo(null))
    }

    @Test
    fun comparableOrdersByFrequencyThenMinimumValue() {
        val highA = ComparableFrequencyTree.singleton('a', 5)
        val lowB = ComparableFrequencyTree.singleton('b', 1)
        val lowA = ComparableFrequencyTree.singleton('a', 1)

        val sorted = listOf(highA, lowB, lowA).sorted()

        assertThat(sorted.map { it.frequency }, equalTo(listOf(1, 1, 5)))
        assertThat(sorted.map { it.minValue }, equalTo(listOf('a', 'b', 'a')))
    }

    @Test
    fun combinePreservesLeftRightCodes() {
        val left = ComparableFrequencyTree.singleton('a', 2)
        val right = ComparableFrequencyTree.singleton('b', 5)
        val tree = ComparableFrequencyTree.combine(left, right)

        assertThat(tree.frequency, equalTo(7))
        assertThat(tree.values, equalTo(setOf('a', 'b')))
        assertThat(tree.minValue, equalTo('a'))
        assertThat(tree.codeFor('a'), equalTo("0"))
        assertThat(tree.codeFor('b'), equalTo("1"))
        assertThat(tree.valuesInOrder(), equalTo(listOf('a', 'b')))
    }

    @Test
    fun fromFrequenciesUsesComparablePriorityDeterministically() {
        val tree = ComparableFrequencyTree.fromFrequencies(mapOf('b' to 1, 'a' to 1, 'c' to 2))

        assertThat(tree.frequency, equalTo(4))
        assertThat(tree.values, equalTo(setOf('a', 'b', 'c')))
        assertThat(tree.valuesInOrder(), equalTo(listOf('a', 'b', 'c')))
        assertThat(tree.codeFor('a'), equalTo("00"))
        assertThat(tree.codeFor('b'), equalTo("01"))
        assertThat(tree.codeFor('c'), equalTo("1"))
    }
}
