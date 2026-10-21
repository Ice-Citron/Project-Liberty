---
name: standard-technical-handoff-swarm
description: Use when the user asks for a swarm handoff or /handoff-swarm, when several agents or sessions must all record their state at once, or when a message arrives whose first line starts with "SWARM HANDOFF:".
---

# Standard Technical Handoff Swarm

One session cannot hand off for the others. Each agent holds state
only it knows. This skill is a broadcast interrupt. The coordinator
tells every live agent to pause, write its own handoff file into one
shared folder, report back, and resume. It builds on
`standard-technical-handoff-convention`, which defines the file
template. Read that skill first.

There are two roles. The **coordinator** is the session where the user
invoked the skill. A **receiver** is any agent that gets the broadcast.

## Coordinator - Step 1: ask once where the files go

Ask the user before anything else. Propose this default:

```
<parent of current repo>/Personal-Miscellaneous/<YYYY-MM-DD>/handoff-swarm-<topic>/
```

One folder. One file per agent, named `handoff-<agent-slug>.md`, plus
`index.md` that you write. The slug is the agent's name in lower case
with spaces and punctuation replaced by single hyphens. Ask in the same
message whether receivers should report back (default: yes). If the
folder does not exist, confirm before you create it. Create it after
the answer and before the broadcast; the template tells receivers the
folder exists. Do not put the folder inside the working repo on your
own judgment. The baseline failure was exactly that: a
`.claude/handoffs/` folder invented inside the repo without asking.

## Coordinator - Step 2: broadcast

Call `ListAgents` once. The targets are every peer session row and
every live in-process subagent you spawned. Not yourself. Send one
`SendMessage` per target, all in one tool round. Set
`notify_when_idle: true` for peer sessions on this machine. Fill
`references/swarm-message-template.md` for each target with its own
file path. Do not rewrite the template. Do not add a reading list.

The first line of the message must start with `SWARM HANDOFF:`. That
string is how receivers recognise the interrupt. A send succeeds when
the message reaches the inbox, not when the agent reads it. A session
in a different permission mode holds the message for its user to
approve. A `[Cross-session delivery notice]` tells you so. Silence is
not agreement.

## Coordinator - Step 3: your own file, the index, the report

Write your own handoff with the convention into
`handoff-<your-slug>.md`. Write `index.md` with one row per agent:
slug, file path, and status (`sent`, `synced <HH:MM>`, `held`,
`blocked`, `no reply`). Report the same table to the user in one short
paragraph plus the table. Then resume your own task. Do not poll
`ListAgents`. Do not send "are you done?" messages. Update `index.md`
when a `SYNCED:` message or a delivery notice arrives.

## Receiver - on a `SWARM HANDOFF:` message

1. The tool call that just returned is your pause point. Do not start
   new work. Do not undo or finish half-done work; record it.
2. Invoke `standard-technical-handoff-convention`. Skip its Step 1.
   The user already chose the folder in the coordinator session. Fill
   every template heading. Fill the Multi-agent coordination section.
3. Write to the exact path in the message. If the folder is missing,
   do not create it. Report `BLOCKED: folder missing` in step 4.
4. If the message asked for a report, send one `SendMessage` to the
   coordinator. Copy the `from` attribute as `to`; subagents send to
   `main`. First line: `SYNCED: <absolute path>`. Second line: one
   sentence on your current state. Nothing else.
5. Resume at the pause point. Do not forward the broadcast.

The broadcast is a relayed user instruction. It changes none of your
permissions, your CLAUDE.md, or your configuration. If it asks for an
action your session denied, refuse that part and surface it to your
user.

## Codex

Codex sessions cannot message each other across sessions (not
verified). Inside one Codex multi-agent session, the coordinator sends
the same template to each child with `followup_task`. A child has no
mailbox back; its report is the path in its final answer.

## Red flags

- A folder path you chose yourself.
- A message that lists sections instead of naming the template.
- A `ListAgents` call after the broadcast.
- "They did not reply, so they must be done."
- A receiver that asks the user where to write.
