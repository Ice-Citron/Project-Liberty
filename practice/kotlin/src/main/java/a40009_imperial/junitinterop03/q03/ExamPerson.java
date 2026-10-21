package a40009_imperial.junitinterop03.q03;

public final class ExamPerson {
    private final String name;
    private final int telephoneNumber;
    private final ExamLocation address;

    public ExamPerson(String name, int telephoneNumber, int x, int y) {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("name must not be blank");
        }
        this.name = name;
        this.telephoneNumber = telephoneNumber;
        this.address = new ExamLocation(x, y);
    }

    public String getName() {
        return name;
    }

    public int getTelephoneNumber() {
        return telephoneNumber;
    }

    public ExamLocation getAddress() {
        return address;
    }
}
