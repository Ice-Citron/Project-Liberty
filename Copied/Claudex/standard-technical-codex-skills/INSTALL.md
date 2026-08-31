# Install — Codex personal skills

These files go into `~/.codex/` on each PC.

1. Unzip this archive into a temporary folder.
2. Copy the three folders from `skills/` into `~/.codex/skills/`.
3. Copy `prompts/handoff.md` and `prompts/qscha.md` into
   `~/.codex/prompts/`. Create the `prompts` folder if it is absent.
4. Caution: if `~/.codex/AGENTS.md` already exists, do not overwrite
   it. Copy the one pointer line from this archive's `AGENTS.md` into
   the file. If it does not exist, copy the file whole.
5. Start a new Codex session. Type `/qscha` or `/handoff` to test the
   prompts. Ask a small coding question to test the always-on rules.

One command for steps 2 to 4 on a clean PC:

```
unzip standard-technical-codex-skills.zip -d /tmp/ctc && \
  cp -R /tmp/ctc/skills/* ~/.codex/skills/ && \
  mkdir -p ~/.codex/prompts && cp /tmp/ctc/prompts/* ~/.codex/prompts/ && \
  [ -f ~/.codex/AGENTS.md ] || cp /tmp/ctc/AGENTS.md ~/.codex/
```

Contents:

- `skills/standard-technical-coding-practices/` — STE prose,
  before/after edit suggestions, plan first, verification, code shape.
- `skills/standard-technical-qscha/` — the QSCHA active-learning
  engine.
- `skills/standard-technical-handoff-convention/` — the handoff file
  convention.
- `prompts/handoff.md`, `prompts/qscha.md` — the `/handoff` and
  `/qscha` prompts.
- `AGENTS.md` — one always-on pointer to the skill above.

Note: this port matches the Claude Code version, with three
adaptations: skill paths point at `~/.codex/skills/`, the no-edit rule
names no Claude tools, and "plan mode" became "propose a plan first".
The Claude version passed 10 of 10 behavior tests; this port ran no
behavior tests on Codex itself.
