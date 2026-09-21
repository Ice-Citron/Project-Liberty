# Install — Claude Code personal skills

These files go into `~/.claude/` on each PC.

1. Unzip this archive into `~/.claude/`:
   `unzip standard-technical-claude-skills.zip -d ~/.claude/`
2. Caution: the archive contains `CLAUDE.md`. If `~/.claude/CLAUDE.md`
   already exists on the PC, do not overwrite it. Copy the one pointer
   line into the file instead.
3. Start a new Claude Code session. Type `/qscha` or `/handoff` to test
   the aliases. Ask a small coding question to test the always-on
   rules.

Contents:

- `skills/standard-technical-coding-practices/` — STE prose,
  before/after edit suggestions, plan mode, verification, code shape.
- `skills/standard-technical-qscha/` — the QSCHA learning engine.
- `skills/standard-technical-handoff-convention/` — the handoff file
  convention.
- `commands/handoff.md`, `commands/qscha.md` — the `/handoff` and
  `/qscha` aliases.
- `CLAUDE.md` — one always-on pointer to the coding-practices skill.

For Codex on the same PC: copy the three `skills/` folders into
`~/.agents/skills/` as well. Codex reads that location.
