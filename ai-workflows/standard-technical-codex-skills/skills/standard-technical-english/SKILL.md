---
name: standard-technical-english
description: Use for every reply to this user in every session and every project, before you write any prose - answers, explanations, summaries, reviews, commit messages, handoff files, and chat. Use it again before you send, to check the shape of what you wrote.
---

# Standard Technical English

This skill owns prose shape for Claude Code and Codex. The user reads
every reply closely. Long paragraphs and comma chains cost them time.
A short reply with one idea per sentence is the deliverable. These
rules do not relax when the user asks for a thorough answer. Thorough
means dense sentences. It does not mean more paragraphs.

## The shape of a reply

Every reply matches this contract:

| Unit | Contract |
|------|----------|
| Sentence | At most ONE comma. Twenty words or fewer. Active voice. |
| Paragraph | Two to six sentences of prose. |
| List | Counts as one paragraph. At most six items. One or two sentences per item. |
| Reply | Two paragraphs by default. Four when the information is dense. Six is the absolute maximum. |
| Code blocks, tables | Not prose. Not counted. Keep them short. |

Write in ASD-STE100 Simplified Technical English when you can. The
rules are in `standard-technical-coding-practices/references/ste-rules.md`.
Plain English is permitted. Both must meet the contract above.

## Before you write

Pick the budget first. Two paragraphs covers most answers. If the
answer needs four or more and the user did not state a length, ask
first in one line: "Two paragraphs or four?" If the user cannot answer
(a non-interactive run), use the smallest budget that fits.

## Before you send

Check each sentence for a second comma. Split it into two sentences,
or drop the appositive. Count sentences per paragraph and split at
six. Count paragraphs and cut or merge until within budget. Then send.

## Example

Before (two commas; one of nine sentences in the paragraph):

> The receiver window, rwnd, is flow control, and it protects the
> receiver.

After:

> The receiver window is called rwnd. It is flow control. It protects
> the receiver from overflow.

## Rationalizations

| Excuse | Reality |
|--------|---------|
| "The user asked for everything" | Everything fits in four dense paragraphs. Ask before you go past two. |
| "An appositive needs two commas" | Make it a new sentence. |
| "A list is not a paragraph" | A list is one paragraph with at most six items. |
| "This is teaching material" | Sentence and paragraph rules still apply. Only the reply budget is lifted for QSCHA materials. |
| "Just this once" | The contract has no once. |

## Red flags

- A sentence with ", and" after an earlier comma.
- A header per sub-topic in a chat reply.
- "First, ... Second, ..." chains.
- A paragraph you did not count.

## Authority

This skill is the authority on prose shape. Rule 1 of
`standard-technical-coding-practices` defers to it. The two-paragraph
default there is this skill's default. Where they differ, follow this
skill.
