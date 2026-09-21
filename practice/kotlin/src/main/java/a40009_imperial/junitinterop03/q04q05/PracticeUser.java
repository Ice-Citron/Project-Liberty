package a40009_imperial.junitinterop03.q04q05;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

public final class PracticeUser {
    private final String userName;
    private final int yearOfBirth;
    private final ReentrantLock lock = new ReentrantLock();
    private final List<PracticeUser> friends = new ArrayList<>();

    public PracticeUser(String userName, int yearOfBirth) {
        if (userName == null || userName.isBlank()) {
            throw new IllegalArgumentException("userName must not be blank");
        }
        this.userName = userName;
        this.yearOfBirth = yearOfBirth;
    }

    public String getUserName() {
        return userName;
    }

    public int getYearOfBirth() {
        return yearOfBirth;
    }

    public ReentrantLock getLock() {
        return lock;
    }

    public List<PracticeUser> getCurrentFriends() {
        lock.lock();
        try {
            return new ArrayList<>(friends);
        } finally {
            lock.unlock();
        }
    }

    void considerFriendRequest(PracticeUser other) {
        if (other == this || friends.contains(other)) {
            return;
        }
        friends.add(other);
    }

    @Override
    public String toString() {
        return userName + "(" + yearOfBirth + ")";
    }
}
