# Install — Claude Code personal skills

These files go into `~/.claude/` on each PC.

1. Unzip this archive into `~/.claude/`:
   `unzip standard-technical-claude-skills.zip -d ~/.claude/`
2. Caution: the archive contains `CLAUDE.md`. If `~/.claude/CLAUDE.md`
   already exists on the PC, do not overwrite it. Copy the one pointer
   line into the file instead.
3. Start a new Claude Code session. Type `/qscha`, `/handoff`,
   `/handoff-swarm`, or `/hegelian` to test the aliases. Ask a small coding question to test the always-on
   rules.

Contents:

- `skills/standard-technical-coding-practices/` — STE prose,
  before/after edit suggestions, plan mode, verification, code shape.
- `skills/standard-technical-qscha/` — the QSCHA learning engine.
- `skills/standard-technical-handoff-convention/` — the handoff file
  convention.
- `skills/standard-technical-english/` — prose shape: one comma per
  sentence, two to six sentences per paragraph, paragraph budget.
  Authority on prose; coding-practices Rule 1 defers to it.
- `skills/standard-technical-handoff-swarm/` — broadcast handoff to
  every live agent and session (coordinator + receiver roles).
- `skills/standard-technical-hegelian-explainer/` — research paper as
  a Hegelian dialectic tree of its references.
- `skills/standard-technical-karpathy/` — Andrej Karpathy's coding
  guidelines: think first, simplicity, surgical changes, verifiable goals.
- `skills/standard-technical-wandb/` — W&B link to a minimal PDF
  training report of Ultralytics runs (scripts in `scripts/`).
- `commands/handoff.md`, `commands/qscha.md`, `commands/handoff-swarm.md`,
  `commands/hegelian.md` — the `/handoff`, `/qscha`, `/handoff-swarm`,
  and `/hegelian` aliases.
- `CLAUDE.md` — always-on pointers to karpathy (top priority),
  coding-practices and english, and
  the swarm trigger line.

For Codex on the same PC: copy the eight `skills/` folders into
`~/.agents/skills/` as well. Codex reads that location.
