"""Build the PDF report from pull.py output, plus a contact sheet to check.

Usage: python -I build.py <data_dir> <export_dir or -> <out_dir>
           [--keep overview,metrics,...] [--runs <name or id>,...]
Writes <out_dir>/report.html, report.pdf and contact.png.
"""
import argparse
import glob
import html
import json
import os
import re
import shutil
import subprocess

import pandas as pd
from PIL import Image

GROUPS = ("overview", "metrics", "losses", "lr", "epochs", "batches", "curves",
          "labels", "system", "inventory")
METRICS = ["metrics/mAP50(B)", "metrics/mAP50-95(B)", "metrics/precision(B)",
           "metrics/recall(B)"]
LOSSES = ["train/box_loss", "train/cls_loss", "train/dfl_loss", "val/box_loss",
          "val/cls_loss", "val/dfl_loss"]
TWINS = ("Percent", ".memoryAllocated")  # the same data as a sibling panel
CHROME = os.environ.get(
    "CHROME", "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
CSS = """
@page { size: A4 landscape; margin: 11mm 13mm 12mm; }
* { box-sizing: border-box; }
body { margin: 0; color: #1b1b1b;
  font: 10.5pt/1.45 "Helvetica Neue", Helvetica, Arial, sans-serif; }
header { border-bottom: 1.5px solid #1b1b1b; padding-bottom: 2.5mm;
  margin-bottom: 5mm; }
h1 { font-size: 18pt; font-weight: 600; margin: 0 0 1.5mm; }
header p { margin: 0 0 1mm; color: #444; }
header p + p { font-size: 9pt; color: #666; }
h2 { font-size: 14pt; font-weight: 600; margin: 0 0 2mm; break-after: avoid; }
h2 span { color: #999; font-weight: 400; margin-right: 2.5mm; }
h2 small { font-size: 10pt; font-weight: 400; color: #666; margin-left: 3mm; }
section { margin-bottom: 6mm; }
.new-page { break-before: page; }
.grid { display: grid; gap: 4mm 5mm; margin-bottom: 4mm; }
.c2 { grid-template-columns: 1fr 1fr; }
.c3 { grid-template-columns: 1fr 1fr 1fr; }
figure { margin: 0; break-inside: avoid; }
figure img { display: block; width: 100%; border: 0.5px solid #ddd;
  border-radius: 2px; }
figure.wb img { max-height: 62mm; width: auto; max-width: 100%; margin: 0 auto; }
figure.sq img { max-height: 125mm; width: auto; max-width: 100%; margin: 0 auto; }
figcaption { font-size: 9pt; color: #555; margin-top: 1.4mm; }
figcaption b { font-weight: 600; color: #1b1b1b; margin-right: 1.5mm; }
table { border-collapse: collapse; width: 100%; font-size: 9.3pt; }
tr { break-inside: avoid; }
th, td { padding: 1mm 2mm; border-bottom: 0.5px solid #ddd; text-align: left; }
th { font-weight: 600; color: #444; border-bottom: 1px solid #999;
  font-size: 8.8pt; }
.n { text-align: right; font-variant-numeric: tabular-nums; }
.mono { font-family: Menlo, monospace; font-size: 8.8pt; color: #555; }
tr.rule td { border-top: 1px solid #999; }
tr.total td { font-weight: 600; border-top: 1px solid #999; }
"""

parser = argparse.ArgumentParser()
parser.add_argument("data")
parser.add_argument("charts")
parser.add_argument("out")
parser.add_argument("--keep", default=",".join(GROUPS))
parser.add_argument("--runs", default="")
args = parser.parse_args()
keep = set(args.keep.split(","))
with open(f"{args.data}/project.json") as fh:
    project = json.load(fh)
runs = [r for r in project["runs"] if r["metrics"]]
chosen = set(args.runs.split(","))
plot_runs = [r for r in runs if not args.runs or {r["id"], r["name"]} & chosen]
for r in runs:
    r["df"] = pd.read_csv(f"{args.data}/{r['id']}/history.csv")
os.makedirs(f"{args.out}/img", exist_ok=True)

# Export files are named Section-<s>-Panel-<i>-<panel id>.png.
charts = (f"{args.charts}/charts" if os.path.isdir(f"{args.charts}/charts")
          else args.charts)
exported = {} if args.charts == "-" else {
    os.path.basename(p)[:-4].rsplit("-", 1)[1]: p
    for p in glob.glob(f"{charts}/*.png")}
panels = {keys[0]: (exported[pid], keys)
          for pid, keys in project["panels"].items() if pid in exported}
figures = 0


def figure(src, caption, cls=""):
    global figures
    figures += 1
    dst = f"img/{figures}{os.path.splitext(src)[1]}"
    shutil.copyfile(src, f"{args.out}/{dst}")
    return (f'<figure class="{cls}"><img src="{dst}"><figcaption>'
            f'<b>Fig. {figures}</b>{html.escape(caption)}</figcaption></figure>')


def grid(cols, items, new_page=False):
    page = " new-page" if new_page else ""
    return f'<div class="grid c{cols}{page}">{"".join(items)}</div>'


def wandb_grid(keys, cols):
    titles = (", ".join(dict.fromkeys(re.sub(r"\.\d+\.", ".*.", k) for k in
                                      panels[key][1])) for key in keys)
    return grid(cols, [figure(panels[k][0], t, "wb") for k, t in zip(keys, titles)])


def plots(run, names, cls=""):
    folder = f"{args.data}/{run['id']}"
    return [figure(f"{folder}/{n}", f"{run['name']} · {n}", cls)
            for n in names if os.path.exists(f"{folder}/{n}")]


def value(df, key, pick=-1):
    series = df[key].dropna() if key in df else []
    return series.iloc[pick] if len(series) else None


def fmt(x, spec):
    return "·" if x is None else format(x, spec)


def table(head, rows):
    cells = "".join(f'<th class="n">{h}</th>' if i else f"<th>{h}</th>"
                    for i, h in enumerate(head))
    return f"<table><thead><tr>{cells}</tr></thead>{''.join(rows)}</table>"


def overview():
    rows = []
    for r in runs:
        df = r["df"]
        params = value(df, "model/parameters", 0)
        nums = [fmt(params and params / 1e6, ".1f") + " M",
                fmt(value(df, "model/GFLOPs", 0), ".1f"),
                fmt(value(df, "model/speed_PyTorch(ms)", 0), ".1f"),
                f"{len(df)} / {r['epochs'] or '·'}"]
        nums += [fmt(value(df, k), ".3f") for k in METRICS]
        nums.append(f"{value(df, '_runtime') / 60:.0f} min")
        rows.append(f"<tr><td>{r['name']}</td><td class='mono'>{r['id']}</td>"
                    f"<td>{r['state']}</td>"
                    + "".join(f"<td class='n'>{x}</td>" for x in nums) + "</tr>")
    return table(["Run", "W&amp;B id", "State", "Params", "GFLOPs", "Speed (ms)",
                  "Epochs logged / set", "mAP50", "mAP50-95", "Precision",
                  "Recall", "Runtime"], rows)


def epochs():
    rows = []
    for r in runs:
        df = r["df"].dropna(subset=[METRICS[0]])
        for i, (_, row) in enumerate(df.iterrows()):
            name = (f"<td rowspan='{len(df)}'>{r['name']}</td>" if i == 0 else "")
            nums = [f"{int(row['_step'])}"]
            nums += [fmt(row.get(k), ".4f") for k in METRICS + LOSSES]
            nums += [fmt(row.get("lr/pg0"), ".5f"), f"{row['_runtime'] / 60:.0f} min"]
            rows.append(f"<tr class='{'rule' if i == 0 else ''}'>{name}"
                        + "".join(f"<td class='n'>{x}</td>" for x in nums) + "</tr>")
    return table(["Run", "Epoch", "mAP50", "mAP50-95", "Precision", "Recall",
                  "train box", "train cls", "train dfl", "val box", "val cls",
                  "val dfl", "lr pg0", "Elapsed"], rows)


def inventory():
    states = ["finished", "crashed", "failed", "killed"]
    days = sorted({r["created"][:10] for r in project["runs"]})
    rows = []
    for label, group in [(d, [r for r in project["runs"] if r["created"][:10] == d])
                         for d in days] + [("All", project["runs"])]:
        counts = [sum(r["state"] == s for r in group) for s in states]
        counts += [len(group), sum(r["metrics"] for r in group)]
        rows.append(f"<tr class='{'total' if label == 'All' else ''}'><td>{label}</td>"
                    + "".join(f"<td class='n'>{c or '·'}</td>" for c in counts)
                    + "</tr>")
    return table(["Date"] + states + ["Runs", "With metrics"], rows)


sections = []


def section(title, content, new_page=False, label=None):
    label = label or str(sum(s[1].isdigit() for s in sections) + 1)
    sections.append((f'<section class="{"new-page" if new_page else ""}">'
                     f"<h2><span>{label}</span>{title}</h2>{content}</section>",
                     label))


if "overview" in keep:
    section("Overview", overview())
for group, title, keys in [("metrics", "Detection metrics", METRICS),
                           ("losses", "Training and validation losses", LOSSES),
                           ("lr", "Learning rate", ["lr/pg0"])]:
    keys = [k for k in keys if k in panels]
    if group in keep and keys:
        section(title, wandb_grid(keys, 2))
if "epochs" in keep:
    section("Per-epoch values", epochs())
for r in plot_runs:
    parts = []
    if "batches" in keep:
        parts += [grid(2, plots(r, ["val_batch0_labels.jpg",
                                    "val_batch0_pred.jpg"], "sq"))]
    if "curves" in keep:
        parts += [grid(2, plots(r, ["confusion_matrix_normalized.png",
                                    "PR_curve.png"]), new_page=bool(parts)),
                  grid(3, plots(r, ["F1_curve.png", "P_curve.png",
                                    "R_curve.png"]))]
    parts = [p for p in parts if "<figure" in p]
    if parts:
        df = r["df"]
        small = (f"<small>{len(df)} epochs · mAP50 "
                 f"{fmt(value(df, METRICS[0]), '.3f')}</small>")
        section(f"Run {r['name']}{small}", "".join(parts), new_page=True)
labels = [f for r in plot_runs for f in plots(r, ["labels.jpg"], "sq")]
if "labels" in keep and labels:
    section("Dataset statistics", grid(2, labels), new_page=True)
system = [k for k in panels if k.startswith("system/") and not k.endswith(TWINS)]
appendix = iter("ABC")
if "system" in keep and system:
    section("Appendix: system metrics", wandb_grid(system, 3), True, next(appendix))
if "inventory" in keep:
    section("Appendix: run inventory", inventory(), True, next(appendix))

page = (f'<!doctype html><html lang="en"><meta charset="utf-8">'
        f"<title>{html.escape(project['title'])}</title><style>{CSS}</style>"
        f"<body><header><h1>{html.escape(project['title'])}</h1>"
        f"<p>Weights &amp; Biases training report</p>"
        f"<p>Project {html.escape(project['path'])}</p></header>"
        + "".join(s[0] for s in sections) + "</body></html>")
with open(f"{args.out}/report.html", "w") as fh:
    fh.write(page)
subprocess.run([CHROME, "--headless=new", "--disable-gpu", "--no-pdf-header-footer",
                "--virtual-time-budget=8000", f"--print-to-pdf={args.out}/report.pdf",
                f"file://{os.path.abspath(args.out)}/report.html"],
               check=True, capture_output=True)
for old in glob.glob(f"{args.out}/sheet-*.png"):
    os.remove(old)
subprocess.run(["pdftoppm", "-r", "30", "-png", f"{args.out}/report.pdf",
                f"{args.out}/sheet"], check=True)
sheets = [Image.open(p) for p in sorted(glob.glob(f"{args.out}/sheet-*.png"))]
w, h = sheets[0].size
contact = Image.new("RGB", (4 * w + 40, -(-len(sheets) // 4) * (h + 8) + 8), "#777")
for i, s in enumerate(sheets):
    contact.paste(s, (8 + i % 4 * (w + 8), 8 + i // 4 * (h + 8)))
contact.save(f"{args.out}/contact.png")
print(f"{len(sheets)} pages, {figures} figures -> {args.out}/report.pdf")
