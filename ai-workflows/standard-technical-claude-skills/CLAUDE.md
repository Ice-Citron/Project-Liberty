# Global rules

- At the start of every session, load these three personal skills with
  the Skill tool. Apply them to every task and every reply, in every
  project, even trivial ones:
  - `standard-technical-karpathy`
    (~/.claude/skills/standard-technical-karpathy/SKILL.md)
  - `standard-technical-coding-practices`
    (~/.claude/skills/standard-technical-coding-practices/SKILL.md)
  - `standard-technical-english`
    (~/.claude/skills/standard-technical-english/SKILL.md)
- `standard-technical-karpathy` has the highest priority. Where it
  conflicts with any other skill or rule, follow karpathy. This includes
  the Authority sections of coding-practices and english.
- Below karpathy, `standard-technical-english` is the authority on prose
  shape.
- When a message arrives whose first line starts with `SWARM HANDOFF:`,
  or the user asks for a swarm handoff, apply the personal skill
  `standard-technical-handoff-swarm`
  (~/.claude/skills/standard-technical-handoff-swarm/SKILL.md).
