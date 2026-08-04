#!/usr/bin/env python3
"""Detect RevealJS slides whose content overflows the configured slide size.

Drives a headless Chrome instance via the Chrome DevTools Protocol (no
Selenium/Playwright dependency beyond `websocket-client` + `requests`).
For each rendered <chapter>.html in a slides directory it:
  1. opens the file (needs internet access: MathJax loads from a CDN)
  2. waits for MathJax to finish typesetting all slides (MathJax v2's reveal.js
     plugin typesets the *entire* deck once on init, not lazily per slide, so
     a single fixed wait after page load is enough — no need to step through
     slides)
  3. reads Reveal.getConfig().width / .height
  4. actually NAVIGATES to every (h, v) slide index via Reveal.slide(h, v) and
     measures the scrollWidth/scrollHeight of the resulting `.present` slide.
     This is not optional: RevealJS sets `display: none` on every vertical
     sub-slide ("future"/"past" class) that isn't currently shown, which
     collapses its scrollHeight to 0. A naive bulk query of all <section>
     elements without navigating (an earlier version of this script did that)
     silently reports 0 overflow for every non-active vertical sub-slide, even
     if it badly overflows once actually presented — a real, previously-missed
     bug, not a hypothetical one. Top-level (horizontal) slides without
     vertical children stay `display: block` even when inactive, so bulk
     querying happened to work for those, which is what let this go unnoticed
     for a while.
  5. reports any slide whose scrollHeight exceeds the configured height
     (content "flows over" at the bottom) or whose scrollWidth exceeds the
     configured width (e.g. a long formula running off the right edge —
     easy to miss by eye since it just disappears past the slide boundary)

Usage:
    python check_overflow.py <slides_dir> [file1.html file2.html ...]

If no files are given, all *.html files in <slides_dir> are checked.
Requires: `google-chrome` on PATH, and the `websocket-client` + `requests`
Python packages (pip install websocket-client requests; a throwaway venv is
fine since this script has no other project dependency).
"""
import json
import os
import subprocess
import sys
import time

import requests
import websocket

PORT = 9333
CHROME_PROFILE = "/tmp/chrome-overflow-check"


def start_chrome():
    proc = subprocess.Popen(
        [
            "google-chrome",
            "--headless=new",
            "--disable-gpu",
            f"--remote-debugging-port={PORT}",
            "--no-sandbox",
            "--window-size=1400,900",
            "--remote-allow-origins=*",
            f"--user-data-dir={CHROME_PROFILE}",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    for _ in range(50):
        try:
            requests.get(f"http://127.0.0.1:{PORT}/json/version", timeout=1)
            break
        except Exception:
            time.sleep(0.2)
    return proc


def new_tab():
    return requests.put(f"http://127.0.0.1:{PORT}/json/new").json()


def close_tab(tab_id):
    requests.get(f"http://127.0.0.1:{PORT}/json/close/{tab_id}")


def eval_js(ws, expr, timeout=20):
    msg_id = int(time.time() * 1000) % 1000000
    ws.send(
        json.dumps(
            {
                "id": msg_id,
                "method": "Runtime.evaluate",
                "params": {
                    "expression": expr,
                    "returnByValue": True,
                    "awaitPromise": True,
                },
            }
        )
    )
    deadline = time.time() + timeout
    while time.time() < deadline:
        ws.settimeout(max(0.5, deadline - time.time()))
        try:
            raw = ws.recv()
        except Exception:
            break
        data = json.loads(raw)
        if data.get("id") == msg_id:
            return data
    return None


def check_file(slides_dir, fname):
    path = os.path.join(slides_dir, fname)
    url = "file://" + os.path.abspath(path)
    tab = new_tab()
    ws = websocket.create_connection(tab["webSocketDebuggerUrl"], timeout=20)
    try:
        ws.send(json.dumps({"id": 99, "method": "Page.enable"}))
        ws.recv()
        ws.send(json.dumps({"id": 100, "method": "Page.navigate", "params": {"url": url}}))
        deadline = time.time() + 15
        while time.time() < deadline:
            ws.settimeout(max(0.5, deadline - time.time()))
            try:
                raw = ws.recv()
            except Exception:
                break
            data = json.loads(raw)
            if data.get("method") == "Page.loadEventFired":
                break
        time.sleep(2.0)  # let reveal.js + mermaid.js finish rendering
        time.sleep(5.0)  # let MathJax (loaded from CDN) finish typesetting the whole deck
        script = r"""
(async function(){
  if (typeof Reveal === 'undefined') return JSON.stringify({error: 'no reveal'});
  var cfg = Reveal.getConfig();
  var h = cfg.height || 700;
  var w = cfg.width || 1050;
  var tops = document.querySelectorAll('.reveal .slides > section');
  var indices = [];
  tops.forEach(function(sec, i){
    var subs = sec.querySelectorAll(':scope > section');
    if (subs.length) {
      subs.forEach(function(_, j){ indices.push([i, j]); });
    } else {
      indices.push([i, null]);
    }
  });
  var results = [];
  for (var k = 0; k < indices.length; k++) {
    var idx = indices[k];
    if (idx[1] === null) { Reveal.slide(idx[0]); } else { Reveal.slide(idx[0], idx[1]); }
    await new Promise(function(r){ setTimeout(r, 60); });
    // NB: in a vertical stack, BOTH the outer <section class="stack present">
    // and the actual visible inner <section class="... present"> match
    // "section.present" -- document order puts the outer wrapper first, so a
    // plain querySelector() grabs the WRONG (outer, often fixed-height)
    // element and silently reports a bogus, often-repeated scrollHeight for
    // every sub-slide in that stack. Always take the LAST match (the
    // innermost/deepest one) instead.
    var presentMatches = document.querySelectorAll('.reveal .slides section.present');
    var present = presentMatches.length ? presentMatches[presentMatches.length - 1] : null;
    if (!present) continue;
    var sh = present.scrollHeight;
    var sw = present.scrollWidth;
    var h2 = present.querySelector('h1,h2,h3');
    var title = h2 ? h2.innerText : '(no title)';
    var subIdx = idx[1] === null ? 0 : idx[1];
    var entry = null;
    if (sh > h + 15) {
      entry = entry || {top: idx[0], sub: subIdx, title: title};
      entry.scrollHeight = Math.round(sh);
      entry.limitHeight = h;
    }
    if (sw > w + 15) {
      entry = entry || {top: idx[0], sub: subIdx, title: title};
      entry.scrollWidth = Math.round(sw);
      entry.limitWidth = w;
    }
    if (entry) results.push(entry);
  }
  return JSON.stringify({height: h, width: w, count: indices.length, overflow: results});
})()
"""
        res = eval_js(ws, script, timeout=40)
        if res is None:
            return {"file": fname, "error": "timeout"}
        val = res.get("result", {}).get("result", {}).get("value")
        if val is None:
            return {"file": fname, "error": str(res)}
        return {"file": fname, **json.loads(val)}
    finally:
        ws.close()
        close_tab(tab["id"])


def main():
    if len(sys.argv) < 2:
        print("Usage: check_overflow.py <slides_dir> [file1.html ...]", file=sys.stderr)
        sys.exit(1)
    slides_dir = sys.argv[1]
    files = sys.argv[2:]
    if not files:
        files = sorted(f for f in os.listdir(slides_dir) if f.endswith(".html"))

    start_chrome()
    time.sleep(1)
    any_overflow = False
    for h in files:
        if not os.path.exists(os.path.join(slides_dir, h)):
            print(f"MISSING {h}")
            continue
        r = check_file(slides_dir, h)
        n = len(r.get("overflow", []))
        if n:
            any_overflow = True
        print(f"{h}: slides={r.get('count')} overflow={n} err={r.get('error')}")
        for o in r.get("overflow", []):
            parts = []
            if "scrollHeight" in o:
                parts.append(f"height {o['scrollHeight']}>{o['limitHeight']}")
            if "scrollWidth" in o:
                parts.append(f"width {o['scrollWidth']}>{o['limitWidth']}")
            print(f"   -> slide {o['top']+1}.{o['sub']+1} '{o['title']}': " + ", ".join(parts))

    sys.exit(1 if any_overflow else 0)


if __name__ == "__main__":
    main()
