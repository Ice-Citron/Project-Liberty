---
name: standard-technical-wandb
description: Use when the user asks for a W&B (Weights & Biases) training report or PDF, or gives a W&B project or report link of Ultralytics/YOLO runs to turn into a report.
---

# W&B training report

One W&B link in, one minimal PDF out. The PDF holds tables, original
W&B panels and original Ultralytics plots. It holds no sentences.
The scripts sit next to this file in `scripts/`.

## Steps

1. Ask one line: "W&B project link? Report LaTeX export folder
   (optional)?" W&B chart images exist only in a report export: report
   → ⋯ → Download → LaTeX. The user is logged in with `wandb login`.
   Never ask for, read or print the API key. `pull.py` takes the API
   host from the link.
2. Pull into your scratch dir (under one minute):
   `<python> -I scripts/pull.py <link> <scratch>/data`
3. EnterPlanMode. Call AskUserQuestion once. Every question is
   multiSelect, with one-word labels and descriptions of four words or
   fewer:

   | Header | Options (keys for `--keep`) |
   |---|---|
   | W&B charts | metrics, losses, lr, system |
   | Run plots | batches, curves, labels |
   | Tables | overview, epochs, inventory |
   | Run pages | run names with `"metrics": true` in `data/project.json` |

   Offer four runs at most. "Other" takes the rest. Put the chosen keys
   and run names in the plan. ExitPlanMode.
4. Ask one line: "Output folder for the PDF?"
5. Build (lists are comma-separated, no spaces):
   `<python> -I scripts/build.py <scratch>/data <export folder or -> <scratch>/out --keep <keys> --runs <names>`
   Look at `out/contact.png` before you deliver. Copy `out/report.pdf`
   into the output folder (`mkdir -p` first) and `open` it.

`<python>` needs wandb, pandas and PIL. On this Mac it is
`~/miniforge3/envs/numpy-j1/bin/python`. `build.py` also calls Google
Chrome and `pdftoppm`.

## Fixed by the user

- Never re-plot a chart. Matplotlib or SVG redraws look vibecoded.
- No prose in the PDF. Captions are the W&B key or `<run> · <file>`.
- `results.png`, model panels and per-class overlay panels repeat
  other content. Do not offer them.

## Common mistakes

| Symptom | Fix |
|---|---|
| W&B panels are 289 px wide and blurry | Re-export with three or fewer panels per row (526 px) |
| `service process is busy and did not respond` | `pull.py` already sets `timeout=120` and retries |
| No W&B figures in the PDF | Export file names must end in the report panel id |
