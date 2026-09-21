# Questions 5 and 6: Test Writing

Suggested value: 25 marks total

These two questions reverse the normal workflow. The implementation classes are already complete. Your job is to write meaningful JUnit tests.

You should edit only:

```text
ReadyGradeBookTest.kt
ReadyTicketOfficeTest.kt
```

Do not edit:

```text
ReadyGradeBook.kt
ReadyTicketOffice.kt
```

## Question 5: Ready Grade Book

Class under test:

```kotlin
ReadyGradeBook
```

### Behaviours To Test

- recording one mark;
- recording several marks for the same CID;
- `marksFor` returns a defensive copy;
- `averageFor` returns `null` for a missing CID;
- `rankedStudents` sorts by average descending;
- ties in ranking are broken by CID ascending;
- `topStudent` returns `null` when there are no students;
- invalid CID and invalid marks throw `IllegalArgumentException`;
- `passCount` respects the supplied pass mark.

## Question 6: Ready Ticket Office

Class under test:

```kotlin
ReadyTicketOffice
```

### Behaviours To Test

- initial remaining and sold counts;
- selling fewer tickets than are available;
- selling more tickets than are available only sells the remainder;
- refunding moves tickets from sold back to remaining;
- invalid sell/refund inputs throw `IllegalArgumentException`;
- refunding more than sold throws `IllegalArgumentException`;
- concurrent sales from several `Thread`s cannot oversell;
- `snapshot` reports a consistent pair of values.

## Testing Advice

Use JUnit 5 plus Hamcrest, matching the existing repository style:

```kotlin
import org.hamcrest.CoreMatchers.equalTo
import org.hamcrest.MatcherAssert.assertThat
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
```

For thread tests, create a list of `Thread`s, call `start` on all of them, then `join` on all of them before asserting final state.
