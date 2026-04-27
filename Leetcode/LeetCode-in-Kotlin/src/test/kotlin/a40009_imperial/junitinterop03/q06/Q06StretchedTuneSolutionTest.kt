package a40009_imperial.junitinterop03.q06

import kotlin.test.Test
import kotlin.test.assertEquals

class Q06StretchedTuneSolutionTest {
    @Test
    fun `getNotes returns stretched copies`() {
        val target = StandardTune()
        target.addNote(Note(60, 2.0))
        target.addNote(Note(62, 4.0))

        val stretched: Tune = StretchedTune(target, 2.0)
        val notes = stretched.notes

        assertEquals(2, notes.size)
        assertEquals(60, notes[0].pitch)
        assertEquals(4.0, notes[0].duration, 0.0001)
        assertEquals(62, notes[1].pitch)
        assertEquals(8.0, notes[1].duration, 0.0001)
    }

    @Test
    fun `total duration uses stretched durations`() {
        val target = StandardTune()
        target.addNote(Note(60, 2.0))
        target.addNote(Note(62, 4.0))

        val stretched: Tune = StretchedTune(target, 1.5)

        assertEquals(9.0, stretched.totalDuration, 0.0001)
    }

    @Test
    fun `addNote stores inverse duration in target tune`() {
        val target = StandardTune()
        val stretched: Tune = StretchedTune(target, 2.0)

        stretched.addNote(Note(60, 10.0))

        assertEquals(1, target.notes.size)
        assertEquals(60, target.notes[0].pitch)
        assertEquals(5.0, target.notes[0].duration, 0.0001)
    }
}
