package a40009_imperial.threadedpack02.javaworkers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class ParallelWordCounter {
    public Map<String, Integer> countWords(List<String> lines, int threadCount) throws InterruptedException {
        throw new UnsupportedOperationException("practice");
    }

    public static final class Worker implements Runnable {
        private final List<String> lines;
        private final int fromInclusive;
        private final int toExclusive;
        private final Map<String, Integer> counts = new HashMap<>();

        public Worker(List<String> lines, int fromInclusive, int toExclusive) {
            this.lines = lines;
            this.fromInclusive = fromInclusive;
            this.toExclusive = toExclusive;
        }

        @Override
        public void run() {
            throw new UnsupportedOperationException("practice");
        }

        public Map<String, Integer> counts() {
            return counts;
        }
    }
}
