---
name: standard-technical-handoff-convention
description: Use when the user asks for a handoff or /handoff, when context is nearly full or compaction is close, when a session ends mid-task, or when work moves to another agent, session, or model.
---

# Standard Technical Handoff Convention

Context windows are short, and compaction is lossy. A handoff file
lets a fresh session resume at full quality — same task state, same
work rules. A handoff that carries the state but not the rules
produces a fresh session with default behavior: direct edits and walls
of prose. The rules section carries as much load as the state.

## Step 1 — Ask where the file goes

Ask before you write, every time. Propose this default:

```
<parent of current repo>/Personal-Miscellaneous/<YYYY-MM-DD>/handoff-<topic>.md
```

If the user names another place, use it. If a chosen parent directory
does not exist, stop and confirm before you create it. Do not write
the file to the working directory on your own judgment — that was the
exact baseline failure.

## Step 2 — Fill the template, every section

Use `references/handoff-template.md`. Every heading appears in the
output. A section with nothing to report says "None." under its
heading — do not drop the heading. Write all prose in the file in
ASD-STE100. The two sections agents omit most are the two that matter
most:

- **Standing interaction rules** — how the next session must work
  with this user, with the skill paths to read. Without it, the next
  session reverts to default behavior.
- **Verified state** — each fact with its evidence: `file:line`,
  commit hash, or command output. A claim without an anchor forces the
  next session to re-verify it or, worse, to trust it.

Write in detail. The reader has none of your context. Convert every
relative date to an absolute date. Name files by absolute path.

## Step 3 — Multi-agent and QSCHA state

When other agents exist (a coordinator, siblings), the coordination
section is required: who owns what, what you delegated, the last known
status, and what you would report to the coordinator next.

When a QSCHA lesson or build plan is in flight, the QSCHA state
section is required: the current hint levels, the exercises complete
and remaining, and the answers-withheld rule — stated twice, because
it is the rule a new session breaks first.

## Relationship to other skills

standard-technical-coding-practices is the authority on the shared
interaction rules; this skill only points at it. The handoff file
itself is one of the few files you may create — after Step 1.
