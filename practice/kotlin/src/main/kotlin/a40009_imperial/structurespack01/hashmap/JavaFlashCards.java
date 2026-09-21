package a40009_imperial.structurespack01.hashmap;

public class JavaFlashCards {
}
























public class Student {
    private final String cid;
    private int mark;

    public Student(String cid, int mark) {
        if (cid == null || cid.isBlank()) {
            throw new IllegalArgumentException("cid must not be blank")
        }
        if (mark < 0 || mark > 100) {
            throw new IllegalArgumentException("mark must be in range")
        }
        this.cid = cid;
        this.mark = mark;
    }

    public String getCid() {
        return this.cid;
    }
}
/*
    Use private fields, initialise them in the constructor, and expose
    behaviour through methods.
        Exam methods: Keep representation private, validate early, use `final`
        where reassignment is not needed.
* */



















