package a40009_imperial.junitinterop03.q06;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class StretchedTune implements Tune {
    private final Tune targetTune;
    private final double factor;

    public StretchedTune(Tune targetTune, double factor) {
        if (factor <= 0.0) {
            throw new IllegalArgumentException("factor must be positive");
        }
        this.targetTune = targetTune;
        this.factor = factor;
    }

    @Override
    public List<Note> getNotes() {
        List<Note> result = new ArrayList<>();
        for (Note note : targetTune.getNotes()) {
            double duration = Math.min(64.0, note.getDuration() * factor);
            result.add(new Note(note.getPitch(), duration));
        }
        return result;
    }

    @Override
    public double getTotalDuration() {
        double total = 0.0;
        for (Note note : getNotes()) {
            total += note.getDuration();
        }
        return total;
    }

    @Override
    public void addNote(Note newNote) {
        double durationInTarget = Math.min(64.0, newNote.getDuration() / factor);
        targetTune.addNote(new Note(newNote.getPitch(), durationInTarget));
    }

    @Override
    public Iterator<Note> iterator() {
        return getNotes().iterator();
    }
}
