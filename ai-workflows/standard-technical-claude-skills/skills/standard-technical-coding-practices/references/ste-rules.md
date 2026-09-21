# ASD-STE100 rules for all prose

Apply these Simplified Technical English rules to every explanation,
suggestion, comment, docstring, README, and document you write for the
user.

## Sentence rules

- Write short sentences. Instructions: 20 words or fewer. Descriptions:
  25 words or fewer.
- Write one topic per sentence. Write one instruction per sentence.
- Keep paragraphs to 6 sentences or fewer. One topic per paragraph.
- Use the active voice. Say who does what.
- Use simple tenses only: present, past, future.
- Do not use "-ing" verb forms in prose. Write "the function returns",
  not "the function is returning".
- Use approved simple words. Say "use", not "utilize". Say "start",
  not "initiate". Say "make sure", not "ensure".
- Use articles ("a", "the") where the grammar allows them.
- Use a vertical list when a sentence would hold more than two items
  or steps.
- Put warnings and cautions before the instruction they apply to, in
  command form.

## Exemption: technical names

Code identifiers, keywords, file paths, API names, library names, and
error text are technical names. Write them exactly as they are, in code
format: `useEffect`, `git rebase`, `ValueError`. Code blocks are not
prose; STE does not restrict them.

## Example

**Not STE:**

> Decorators can be conceptualized as higher-order wrappers augmenting a
> function's behavior transparently, leveraging Python's first-class
> function semantics.

**STE:**

> A decorator is a function. It gets your function as its input. It gives
> a new function back. The new function does more steps. Python permits
> this because a function is a value.

## Documents and READMEs

Keep documents concise and understandable. If a paragraph can be omitted,
remove it. Do not pad with sections the reader did not need.
