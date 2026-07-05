#!/usr/bin/env python3
"""Build the self-contained dashboard: inline results/web/web_data.json into
web/dashboard_template.html and write web/index.html.

Run from the repo root (after Rscript R/export_web_data.R):
    python3 web/build.py
"""
import json
import pathlib

root = pathlib.Path(__file__).resolve().parent.parent
template = (root / "web" / "dashboard_template.html").read_text()
data = (root / "results" / "web" / "web_data.json").read_text()

# minify and make safe to inline inside a <script> block
data = json.dumps(json.loads(data), separators=(",", ":")).replace("</", "<\\/")

out = template.replace("__DATA_JSON__", data)
assert "__DATA_JSON__" not in out
(root / "web" / "index.html").write_text(out)
print(f"Wrote web/index.html ({len(out) / 1024:.0f} KB)")
