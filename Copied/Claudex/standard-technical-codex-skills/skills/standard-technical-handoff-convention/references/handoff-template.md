# Handoff file template

Every heading below appears in every handoff file, in this order. A
section with nothing to report says "None." under its heading. Do not
drop headings.

````markdown
# Handoff: <topic> — <YYYY-MM-DD>

## How to resume

Working directory: <absolute path>. Read the skills listed under
Standing interaction rules before doing anything else. Then continue
from the first item under Remaining work.

## Standing interaction rules

The user's skills govern every reply. Read them first:

- `~/.codex/skills/standard-technical-coding-practices/SKILL.md` —
  STE prose, two-paragraph answers, NO direct edits (before/after
  suggestions only, permission per edit), plan before code, verify
  before assert, code shape (80 chars, minimal).
- `~/.codex/skills/standard-technical-qscha/SKILL.md` — the user's
  learning method, when teaching or practice comes up.
- Project constraints: <e.g. "never modify submodules", or None.>

## Task and current state

<What is being built, and exactly where it stands now.>

## Done in this context

<Each completed item, one line each.>

## Remaining work

<Ordered list. First item is where the next session starts.>

## Verified state

<Each load-bearing fact WITH its evidence — file:line, commit hash,
or the command run and its output. Example:
- sensor.log stores time under key "ts" (sensor.log:1)
- HEAD is 887f7a7 on main (`git log --oneline -1`)>

## Quirks and discoveries

<Non-obvious behavior found the hard way; each with where it bites.>

## Decisions taken — do not reopen

<Each with one line of why.>

## Decisions open

<Questions awaiting the user; the next session must not decide these
unilaterally.>

## Multi-agent coordination

<Required when other agents exist: who owns what, what was delegated
where, last known status, what you would report to the coordinator
next. Otherwise "None.">

## QSCHA state

<Required when a lesson or build plan is in flight: hint levels in
use, exercises completed and remaining, where the materials files
live. State the answers rule here AND in How to resume: answers stay
hidden until the user asks by exercise number. Otherwise "None.">
````

## Quick-prime block

When the user only needs to prime a new chat quickly (no full handoff),
give this block instead:

```markdown
I learn best with the active-learning "QSCHA" method (Questions with
Syntactical + Conceptual Hints, then Answers). Adapt our conversation:
1. Explain the concept briefly with one concrete example.
2. Write 3-4 questions that target the files we develop.
3. For each question give an independent "Conceptual Hint" (the
   algorithm) and "Syntactical Hint" (exact API names and slices).
4. Hide all answers in collapsed panels after ALL the questions, so I
   cannot see a solution while I draft. Release an answer only when I
   ask for it by number. Answers stay hidden until I ask — this rule
   is the one most often broken.
Also: write prose in Simplified Technical English, two paragraphs by
default; never edit my files — give before/after suggestions instead;
verify claims before you assert them.
```
