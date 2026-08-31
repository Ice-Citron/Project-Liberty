---
name: standard-technical-qscha
description: Use when the user wants to learn, understand, or practice any topic — programming, maths, physics — or asks for exercises, says they do not understand something, or wants to write a feature themselves for practice.
---

# Standard Technical QSCHA

QSCHA = **Q**uestions with **S**yntactical + **C**onceptual **H**ints,
then **A**nswers. It is this user's method to learn, and it has a
precise structure that matters.

**Read `references/qscha-engine.md` before you produce any teaching
output.** Do not improvise the structure from the name. Baseline agents
without the engine produced these failures:

- 13 exercises instead of 3 to 4.
- Hints in one mixed lane.
- No answers at all.
- Practice files in the user's repo, with no question first.

The details are the whole value.

## Two triggers

- **The user wants to understand something.** Explain intuitively,
  then technically. Then offer a practice set. (Mode 1 in the engine.)
- **The user wants to build something.** Offer a choice: you give
  before/after suggestions, or the user writes the code from a QSCHA
  build plan. Do not skip the offer. The user fights code atrophy on
  purpose. (Mode 2 in the engine.)

## The rules most often broken

1. **Answers exist AND stay hidden.** Every set carries its answers —
   collapsed, after all exercises — so the user can self-check with no
   extra round trip. Never put an answer beside its question: the user
   cannot un-see a solution. Never omit the answers: that breaks the
   review loop.
2. **A conceptual hint never names an API.** It states the algorithm
   and the mathematics. The moment it names a function, the logical
   lesson is gone.
3. **A syntactical hint is a parts list, not a gesture.** Give names,
   arguments, argument order, return types, and shapes, so the user
   assembles known pieces like a puzzle. "Use the right OpenCV
   function" is not a hint.

The hints fade over five levels, from a full ordered parts list to
nothing. The levels table is in the engine file.

## Where materials go

Ask before you write any file, every time. Propose this default:

```
<parent of current repo>/Personal-Miscellaneous/<YYYY-MM-DD>/<topic>/
```

If the user names another place (the project folder, `/tmp`), use it.
If a chosen parent directory does not exist, stop and confirm before
you create it. Never create practice files inside the user's repo
unless the user names it.

## Relationship to other skills

standard-technical-coding-practices is the authority on the shared
interaction rules: STE prose, answer length, before/after edits, plan
mode, verification, and code shape. Follow it inside all QSCHA output.
