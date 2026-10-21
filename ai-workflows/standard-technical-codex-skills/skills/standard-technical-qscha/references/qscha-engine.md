# The QSCHA engine

Read this file when you are about to teach: the user asks to learn a
concept, says they do not understand something, asks for exercises, or
accepts a build plan. You do not need it for ordinary questions,
reviews, or fixes.

## What QSCHA is

QSCHA = **Q**uestions with **S**yntactical + **C**onceptual **H**ints,
then **A**nswers.

It is an active-learning method for technical topics: codebases,
algorithms, libraries, mathematics, physics. The claim behind it: a
learner does not absorb a technical skill from a finished solution.
The learner absorbs it when they decide the structure, write the
steps, and check the result. So the material must withhold the
solution long enough for a real attempt. And the material must remove
every obstacle that is not the lesson.

Four stages, always in order:

| Stage | Name | Purpose |
|-------|------|---------|
| **A** | Concept and theory orientation | Give the why, the intuition, and a small pattern. |
| **B** | Question crafting | 3 to 4 real exercises, harder step by step, on the user's own work. |
| **C** | Double-hint buffering | One conceptual and one syntactical hint per exercise, collapsed. |
| **D** | Delayed resolution | Answers last, hidden, verified, released on request. |

Both modes below run this same engine. Do not invent a different
structure for each.

## Stage A — Concept and theory orientation

Before any question, give a short primer with three parts:

1. **Why the concept exists.** What problem does it solve? What breaks
   without it?
2. **The intuition.** The mathematical, logical, or physical picture:
   a coordinate frame shift, raw camera units against metric units,
   why a mask is a boolean array and not a list of indices.
3. **One small, localized pattern.** Five to ten lines that show the
   shape of the tools, not the solution. For maths: the key definition
   or theorem statement, not the worked proof.

Keep it short. This is a primer, not a lecture. A wall of prose here
stops the user, and the lesson never starts.

## Stage B — Question crafting

Give **3 to 4 exercises**, in order of difficulty. Useful splits:
2 easy, 1 medium, 1 hard; or 2 easy, 2 medium.

- Each exercise implements a practical, real function, helper class,
  or wrapper. For maths and physics: each exercise proves or computes
  a real result.
- Frame every exercise around the files or problems the user works on
  right now. A generic exercise does not connect to the work, so it
  does not stick.
- Easy wins come first. They build confidence before the hard problem.

Do not exceed 4 exercises in a concept lesson. A 13-exercise set is
overload, not generosity. A build plan may exceed 4 steps, because it
follows the shape of the code, not a difficulty curve.

## Stage C — Double-hint buffering

Every exercise gets **both** hint types, in one collapsed block. They
do different jobs. Do not merge them into one mixed nudge.

### Conceptual hint — the algorithm

Say what the code or derivation must do under the hood, in order, in
plain steps. Include the mathematics where it applies. **Never name an
API here.** A conceptual hint that names a function has leaked, and
the logical lesson is lost.

> Scale the millimetre values to metres. Then find every coordinate
> outside the physical limit and set it to zero.

### Syntactical hint — the parts list

Show what the pieces look like, so the user plays mix and match. Name
the precise functions, their arguments, the argument order, the return
type, and the array or tensor shapes. Write a signature when a
signature helps.

> Pieces you need:
> - `cv2.imread(path: str, flag) -> np.ndarray` — pass
>   `cv2.IMREAD_UNCHANGED` to keep the `uint16` detail
> - `array.astype(np.float32)` — cast before you divide, or integer
>   division truncates
> - `array[boolean_mask] = value` — NumPy boolean assignment, in place
> - a combined mask reads `(depth < min_val) | (depth >= max_val)` —
>   note `|`, not `or`

A hint that says "use the right OpenCV function" is not a syntactical
hint. Name the function. Name its arguments. If a shape matters, write
the shape.

For maths and physics the syntactical lane becomes the **notation
lane**: the exact definitions, identities, theorem names, and
constants the user needs, stated precisely. The conceptual lane keeps
the proof or solution strategy.

The puzzle frame is deliberate. A learner who assembles known pieces
still makes every structural decision: which piece, in what order,
with what data between them. That is the part that transfers. Recall
of a function name from memory is not.

## Hint levels — the scaffold that fades

The goal is that the user needs fewer hints over time, and finally
none. So the hints must fade on purpose. Track the level and step it
down as the user gets stronger.

| Level | Conceptual hint | Syntactical hint | Use when |
|-------|-----------------|------------------|----------|
| **1 – Puzzle** | Full ordered steps | Full parts list, in order | New library, new concept |
| **2 – Unordered parts** | Full ordered steps | Full parts list, order shuffled | The user knows the pieces exist |
| **3 – Names only** | Full ordered steps | Names, no arguments | The user can read the docs |
| **4 – Logic only** | Full ordered steps | None | The user knows the library |
| **5 – Cold** | One sentence of intent | None | The user is ready to work alone |

How to run it:

- Ask the user which level they want, or infer it from the last set.
- Mix levels inside one set. An easy exercise can sit at level 3 while
  the hard one sits at level 1.
- Step down after the user solves 4 to 5 exercises in a row with
  little help. Step back up without comment when the user stalls. A
  stall is information about the level, not about the user.
- Give more hints at once when the user asks. Never make the user
  argue for a hint.
- Say which level you used. The user then sees the progression, and
  the progression itself motivates.

## Stage D — Delayed resolution

Never show an answer beside its question or its hints. The user must
be able to read the whole set with no accidental sight of a solution.
The damage is permanent: the user cannot un-see it.

The mirror failure is as real: a set with **no answers at all** ("ask
me for the answer key") breaks the review loop. The user reviews
backwards against the reference answer, often offline, with no extra
round trip. So:

- Answers always ship with the set — in one collapsed block per
  exercise, after ALL exercises, or in a separate `answers.md`.
- In conversation, reveal an answer only when the user asks for it, by
  exercise number.
- Run the answer code before you ship it (coding-practices Rule 4).
  An unverified self-check corrupts the review loop silently.
- Answer code follows the code-shape contract: type hints, a one-line
  docstring, an idiomatic body, no defensive additions. Add one short
  "why this works" note. For maths: a worked solution, the shortest
  honest path.

## The exact output format

````markdown
#### Exercise 1: Metric scale and cap (Easy)
Write a function `scale_depth_map(depth_image, max_distance_m)` that
processes a depth matrix.

<details>
<summary>💡 Conceptual + Syntactical Hints</summary>

* **Conceptual Hint:** Scale millimetres to metres. Then find all
  coordinates that break the physical limit and set them to zero.
* **Syntactical Hint:** Cast to `float32` first, divide by `1000.0`,
  then filter with NumPy indexing:
  `depth_array[depth_array >= max_distance_m] = 0.0`.
</details>
````

The answer goes in its own `<details>` block with the summary
`🔑 Answer`, after all exercises — never directly under the exercise
it solves.

Deliver a short set inline. Write files for a large set, or when the
user wants to work offline: `questions.md` and `answers.md`. Ask where
to put them first — the default and the ask-every-time rule are in
SKILL.md. Never drop practice files into the user's repo unasked.

Write all prose in every QSCHA material — inline sets, `questions.md`,
`answers.md`, build plans — in ASD-STE100 (coding-practices Rule 1).
Code blocks are exempt.

## Mode 1 — Learn a concept

Trigger: the user says they do not understand something, or asks to
learn a concept or topic.

1. Run **Stage A**. Explain intuitively first, then technically.
2. Ask if the user wants a practice set. If yes, run Stages B, C
   and D.

**Review loop.** The user attempts, then compares with the answers.
When the user's work is wrong, or the user asks why:

1. Find the exact misunderstanding.
2. Explain the gap, intuitively first.
3. If the gap matters, add one follow-up exercise that targets it.

## Mode 2 — Build a feature

Trigger: the user wants to add a feature, fix a bug, or change a
codebase, and practice is on the table.

First, settle the design: propose a plan and agree it with the user
what to build. Then offer this choice every time:

- **Option A:** You give before/after suggestions, after a plan.
- **Option B:** The user writes the code. You write a QSCHA build
  plan.

Do not skip the offer. The user fights code atrophy on purpose.
Option B is frequently the better choice. Under Option B, also offer a
**pseudo-code first** step: the user drafts the logic in pseudo code,
with conceptual hints only, before real syntax. A stall in pseudo code
locates the gap early and cheaply.

A build plan is one markdown file, split into one section per feature
or function — twelve features across nine files means twelve sections.
Each section contains:

1. A short goal statement and the files involved.
2. Ordered steps. Write each step as a task plus a question. Example:
   "Step 2: make the route return JSON. Which Flask function converts
   a `dict` to a response?"
3. **Stage C hints for every step** — both lanes, collapsed.
4. An **Answers** block at the very bottom of the file (Stage D).

Two extras make a build plan work better than a concept lesson:

- **Plant a bug on purpose.** Point the user at a real trap in the
  codebase, but do not name it. Write "have the user find why this
  fails" in the step. A trap the user finds alone teaches more than a
  warning does.
- **State the design decisions already taken**, so the user does not
  re-open settled questions mid-implementation.

After you write the plan, go through the codebase together. The user
implements one step at a time. You review each step, give before/after
suggestions, and explain each part of the codebase as you meet it.

## Why this works

An exercise contains two independent difficulties:

| Difficulty | Example of the struggle | Hint that removes it |
|------------|-------------------------|----------------------|
| **Logical** | "I do not know that I must convert units before I compare." | Conceptual hint |
| **Syntactical** | "I do not know that OpenCV needs `IMREAD_UNCHANGED` to keep `uint16`." | Syntactical hint |

A learner who fights both at once learns neither, and gives up. The
split lets the tutor remove one difficulty and keep the other. That is
the whole mechanism. It also means each hint must be complete in its
own lane.

The syntactical hint starts as a parts list, so the exercise works
like a puzzle. But a puzzle that stays a puzzle never produces an
independent programmer. So the hints shrink: full parts list, then an
unordered list, then bare names, then logic alone, then nothing. The
end state is this: the user writes the function from a one-line
statement of intent.

## Failure modes

- An answer visible beside its question. The user cannot un-see it.
- A set with no answers included. The offline review loop dies.
- A vague syntactical hint. It leaves the library trivia in the way.
- A conceptual hint that names an API. The logical lesson is lost.
- One mixed hint instead of two lanes. Neither difficulty leaves
  cleanly.
- More than 4 exercises in a concept lesson. Overload, not
  generosity.
- A generic exercise. It does not connect to the work, so it does not
  stick.
- A wall of prose in Stage A. The lesson never starts.
- Hints that never fade. The user stays dependent, and that defeats
  the purpose.
- A practice file with no question first about where it goes.
