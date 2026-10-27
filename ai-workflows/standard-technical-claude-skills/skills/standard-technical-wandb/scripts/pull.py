"""Pull a W&B project for build.py: runs, per-epoch history, plots, panels.

Usage: python -I pull.py <W&B project or report link> <out_dir>
Needs a prior `wandb login`. Never reads the API key itself.
"""
import json
import os
import shutil
import sys
import time
from urllib.parse import urlparse

import pandas as pd
import wandb

PLOTS = ("val_batch0_labels", "val_batch0_pred", "confusion_matrix_normalized",
         "PR_curve", "F1_curve", "P_curve", "R_curve", "labels")


def retry(fn, tries=4):
    """Self-hosted W&B servers time out at random under load."""
    for i in range(tries):
        try:
            return fn()
        except Exception:
            if i == tries - 1:
                raise
            time.sleep(2 * (i + 1))


def panels_in(node):
    """Yields every panel of a report spec."""
    if isinstance(node, dict):
        if "__id__" in node and "viewType" in node:
            yield node
        for value in node.values():
            yield from panels_in(value)
    elif isinstance(node, list):
        for value in node:
            yield from panels_in(value)


link, out = sys.argv[1], sys.argv[2]
url = urlparse(link)
parts = [p for p in url.path.split("/") if p]
if url.netloc != "wandb.ai" and parts[0] == "wandb":
    parts = parts[1:]
path = "/".join(parts[:2])
base = os.environ.setdefault("WANDB_BASE_URL", f"https://api.{url.netloc}")
api = wandb.Api(overrides={"base_url": base}, timeout=120)

reports = retry(lambda: list(api.reports(path)))
project = {"path": path, "title": reports[0].display_name if reports else path,
           "panels": {p["__id__"]: p["config"]["metrics"]
                      for rep in reports for p in panels_in(rep.spec)
                      if (p.get("config") or {}).get("metrics")},
           "runs": []}

for run in retry(lambda: list(api.runs(path, order="+created_at"))):
    has_metrics = any(k.startswith("metrics/") for k in run.summary.keys())
    project["runs"].append({"id": run.id, "name": run.name, "state": run.state,
                            "created": run.created_at,
                            "epochs": run.config.get("epochs"),
                            "metrics": has_metrics})
    if not has_metrics:
        continue
    folder = os.path.join(out, run.id)
    os.makedirs(folder, exist_ok=True)
    history = pd.DataFrame(retry(lambda: list(run.scan_history())))
    history.sort_values("_step").to_csv(f"{folder}/history.csv", index=False)
    for key in PLOTS:
        item = run.summary.get(key) or {}
        if "path" not in item:
            continue
        retry(lambda: run.file(item["path"]).download(root=folder, replace=True))
        ext = os.path.splitext(item["path"])[1]
        os.replace(f"{folder}/{item['path']}", f"{folder}/{key}{ext}")
    shutil.rmtree(f"{folder}/media", ignore_errors=True)
    print(run.name, len(history), "epochs", flush=True)

with open(f"{out}/project.json", "w") as fh:
    json.dump(project, fh, indent=1)
print(len(project["runs"]), "runs,", len(project["panels"]), "panels")
