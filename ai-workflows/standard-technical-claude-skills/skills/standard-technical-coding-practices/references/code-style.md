# Code shape contract

This is what every piece of code you suggest or write for this user
IS. It applies to before/after suggestions, QSCHA answer code,
build-plan answers, and any snippet in an explanation.

## Base standard

- Use the Google Python Style Guide for Python. Use the language's
  dominant style guide otherwise.
- Keep lines at 80 characters or fewer, code and comments alike.
- Keep the blank lines the style guide mandates: two between top-level
  definitions, one between methods.

## Vertical compactness

A statement occupies one line unless the 80-character limit forces a
split. Do not explode a call, a dict, or a comprehension
one-item-per-line when it fits in 80 characters. Do not insert blank
lines inside a function for visual air. Keep functions short, so they
need no internal blank lines.

## Names carry the documentation

Choose names so the code reads without comments. Three- or four-word
names are welcome: `depth_scale_m_per_unit` beats `s` plus a comment.
A short name stays only where the scope is a few lines (`i`, or `f` in
a `with open(...) as f`). A comment states only what the code cannot:
a physical unit, an external constraint, a non-obvious invariant.
Comments and docstrings are STE prose.

## Exactly enough structure

- A helper function, class, or dataclass exists only if the code
  cannot be clear without it. Do not add intermediate objects whose
  only job is to carry values between two lines.
- A function does one thing. Keep the nest shallow. When a third
  indentation level appears, simplify the logic. Do not add reflex
  helpers.
- No clever tricks. When code needs a comment to defend its behavior,
  rewrite the code. Do not defend it.

## Exactly enough safety

- Code handles the cases this codebase can produce. Match the data and
  formats you can see in the repo, not formats you imagine. If the log
  file uses the key `ts`, read `ts`. Do not add a fallback for a
  `timestamp` variant that exists nowhere.
- A bug fix changes the wrong thing and nothing else.
- Practice-exercise answer code (QSCHA): type hints, a one-line
  docstring, an idiomatic body. No try/except. No input validation.
- Real-repo code (build plans, before/after suggestions): add error
  handling only where this codebase genuinely produces the error case.
  The Why line names that case.
- Do not wrap code in `try/except` when it cannot raise in this
  codebase. Do not re-validate values a caller already validated.
