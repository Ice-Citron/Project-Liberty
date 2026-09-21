package a40009_imperial.junitinterop03.q06

import org.junit.jupiter.api.Disabled
import kotlin.test.Test

class Q06StretchedTunePracticeTest {
    @Disabled("Practice file: remove @Disabled and write this test yourself.")
    @Test
    fun `getNotes returns stretched copies`() {
        // TODO: create StandardTune, wrap it with StretchedTune, assert pitch and stretched duration.
    }

    @Disabled("Practice file: remove @Disabled and write this test yourself.")
    @Test
    fun `total duration uses stretched durations`() {
        // TODO: assertEquals(expected, stretched.totalDuration, tolerance).
    }

    @Disabled("Practice file: remove @Disabled and write this test yourself.")
    @Test
    fun `addNote stores inverse duration in target tune`() {
        // TODO: add via stretched tune, then inspect target.notes.
    }
}
