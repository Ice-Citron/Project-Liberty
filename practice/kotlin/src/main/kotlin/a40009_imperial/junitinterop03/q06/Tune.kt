package a40009_imperial.junitinterop03.q06

interface Tune : Iterable<Note> {
    val notes: List<Note>
    val totalDuration: Double

    fun addNote(newNote: Note)
}
