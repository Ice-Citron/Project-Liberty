package a40009_imperial.junitinterop03.q06

class StandardTune : Tune {
    private val storedNotes = mutableListOf<Note>()

    override val notes: List<Note>
        get() = storedNotes.toList()

    override val totalDuration: Double
        get() = storedNotes.sumOf { it.duration }

    override fun addNote(newNote: Note) {
        require(newNote.duration > 0.0) { "duration must be positive" }
        storedNotes.add(newNote)
    }

    override fun iterator(): Iterator<Note> = notes.iterator()
}
