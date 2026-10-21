package a40009_imperial.junitinterop03.q04q05;

import java.util.function.BiFunction;

public final class PracticeMatchmaker {
    private final BiFunction<PracticeUser, PracticeUser, Boolean> canMatch;

    public PracticeMatchmaker(BiFunction<PracticeUser, PracticeUser, Boolean> canMatch) {
        this.canMatch = canMatch;
    }

    public void tryMatching(PracticeUser first, PracticeUser second) {
        PracticeUser lockFirst;
        PracticeUser lockSecond;
        if (first.getUserName().compareTo(second.getUserName()) <= 0) {
            lockFirst = first;
            lockSecond = second;
        } else {
            lockFirst = second;
            lockSecond = first;
        }

        lockFirst.getLock().lock();
        lockSecond.getLock().lock();
        try {
            if (canMatch.apply(first, second)) {
                first.considerFriendRequest(second);
                second.considerFriendRequest(first);
            }
        } finally {
            lockSecond.getLock().unlock();
            lockFirst.getLock().unlock();
        }
    }
}
