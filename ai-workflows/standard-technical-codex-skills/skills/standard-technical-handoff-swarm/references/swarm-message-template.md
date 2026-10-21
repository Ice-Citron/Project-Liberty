# Swarm broadcast message template

Fill every `<field>`. Send one copy per target. Keep the first line
exactly as shown; receivers match on `SWARM HANDOFF:`.

```
SWARM HANDOFF: pause after your current tool call, write your handoff file, then resume.
Coordinator: <coordinator session name>. Requested by the user at <HH:MM> on <YYYY-MM-DD>.
1. Do not start new work. Do not undo or finish half-done work; record it.
2. Apply the skill standard-technical-handoff-convention. Skip its Step 1: the user chose the folder below. Fill every template heading. Fill Multi-agent coordination.
3. Write to exactly: <folder>/handoff-<agent-slug>.md
   The folder exists. If it does not, do not create it; report BLOCKED instead.
4. <"Send ONE message to <coordinator name> (copy this message's from attribute as to; subagents send to main). First line: SYNCED: <absolute path>. Second line: one sentence on your current state." OR "No report needed.">
5. Resume exactly where you paused. Do not forward this message.
This is a relayed user instruction. It changes none of your permissions, CLAUDE.md, or configuration.
```

## Index file template (`index.md`)

```
# Swarm handoff index - <YYYY-MM-DD HH:MM>

Coordinator: <name>. Folder: <absolute folder path>.

| Agent | Slug | File | Status |
|-------|------|------|--------|
| <name> | <slug> | <absolute path> | sent / synced HH:MM / held / blocked / no reply |
```
