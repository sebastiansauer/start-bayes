// Berechnet die Zeitmarken des Films und schreibt sie nach assets/cues.js;
// setzt außerdem data-start/data-duration der Szenen-Hosts und der
// Sprachclips in index.html.
//
// Quelle der Wortzeiten, je Zeile N:
//   assets/voice/line-N.words.json  → [{text,start,end}] in Sekunden ab Clipbeginn (Azure)
//   sonst Schätzung aus der Zeichenzahl (scripts/anchors.json: charsPerSecond)
// Dauer der Sprachclips: assets/voice/line-N.wav (ffprobe), sonst Schätzung.
//
// Aufruf: node scripts/build-timing.mjs
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..");
const cfg = JSON.parse(readFileSync(join(here, "anchors.json"), "utf8"));

// --- SCRIPT.md einlesen -------------------------------------------------------
const script = readFileSync(join(root, "SCRIPT.md"), "utf8");
const lines = {};
let current = null;
for (const raw of script.split("\n")) {
  const m = raw.match(/^## Line (\d+)/);
  if (m) {
    current = +m[1];
    lines[current] = [];
    continue;
  }
  if (current && /^ {4}\S/.test(raw)) lines[current].push(raw.trim());
}
const lineText = (n) => lines[n].join(" ");

// --- Wörter: echt oder geschätzt ---------------------------------------------
const norm = (s) =>
  s
    .toLowerCase()
    .replace(/[„“"‚‘'.,:;!?()\[\]]/g, "")
    .replace(/-/g, " ")
    .trim();

function estimateWords(text, cps) {
  const tokens = text.split(/\s+/).filter(Boolean);
  const weights = tokens.map((t) => {
    let w = t.length + 1;
    if (/[.!?:]$/.test(t)) w += 5; // Satzpause
    else if (/,$/.test(t)) w += 2;
    return w;
  });
  const total = weights.reduce((a, b) => a + b, 0);
  const duration = text.length / cps;
  const words = [];
  let acc = 0;
  tokens.forEach((t, i) => {
    const start = (acc / total) * duration;
    acc += weights[i];
    words.push({ text: t, start, end: (acc / total) * duration });
  });
  return { words, duration };
}

function ffprobeDuration(file) {
  const out = execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", file]);
  return parseFloat(out.toString().trim());
}

function wordsForLine(n) {
  const text = lineText(n);
  const wordsFile = join(root, "assets", "voice", `line-${n}.words.json`);
  const wavFile = join(root, "assets", "voice", `line-${n}.wav`);
  if (existsSync(wordsFile) && existsSync(wavFile)) {
    const words = JSON.parse(readFileSync(wordsFile, "utf8"));
    return { words, duration: ffprobeDuration(wavFile), source: "azure" };
  }
  return { ...estimateWords(text, cfg.charsPerSecond), source: "estimate" };
}

function findAnchor(words, phrase) {
  const target = norm(phrase).split(/\s+/).filter(Boolean);
  const w = words.map((x) => norm(x.text).split(/\s+/).filter(Boolean));
  // flache Liste mit Rückverweis auf das Wort
  const flat = [];
  w.forEach((parts, i) => parts.forEach((p) => flat.push({ p, i })));
  for (let i = 0; i + target.length <= flat.length; i++) {
    let ok = true;
    for (let j = 0; j < target.length; j++) {
      if (flat[i + j].p !== target[j]) {
        ok = false;
        break;
      }
    }
    if (ok) return words[flat[i].i].start;
  }
  return null;
}

// --- Szenen aufbauen ----------------------------------------------------------
const lead = cfg.lead;
let t = 0;
const scenes = {};
const warnings = [];
for (const sc of cfg.scenes) {
  const { words, duration, source } = wordsForLine(sc.line);
  const cues = {};
  for (const [name, phrase] of Object.entries(sc.anchors)) {
    const s = findAnchor(words, phrase);
    if (s == null) {
      warnings.push(`Szene ${sc.id}: Anker „${phrase}" (${name}) nicht gefunden`);
      cues[name] = lead;
    } else cues[name] = +(lead + s).toFixed(3);
  }
  const sceneDuration = +(lead + duration + sc.tail).toFixed(3);
  scenes[sc.id] = {
    start: +t.toFixed(3),
    duration: sceneDuration,
    lead,
    speechEnd: +(lead + duration).toFixed(3),
    source,
    line: sc.line,
    cues,
  };
  t += sceneDuration;
}
const total = +t.toFixed(3);

// --- cues.js schreiben --------------------------------------------------------
const cuesOut = join(root, "assets", "cues.js");
writeFileSync(
  cuesOut,
  `// generiert von scripts/build-timing.mjs – nicht von Hand ändern\nwindow.__CUES = ${JSON.stringify({ total, scenes }, null, 1)};\n`,
);

// --- index.html patchen ---------------------------------------------------------
const indexPath = join(root, "index.html");
if (existsSync(indexPath)) {
  let html = readFileSync(indexPath, "utf8");
  const setAttr = (block, name, value) =>
    block.includes(`${name}="`) ? block.replace(new RegExp(`${name}="[^"]*"`), `${name}="${value}"`) : block;
  html = html.replace(/<div\s+id="root"[^>]*>/, (m) => setAttr(m, "data-duration", total));
  for (const [id, sc] of Object.entries(scenes)) {
    html = html.replace(new RegExp(`<div\\s+id="sc-${id}"[^>]*>`), (m) =>
      setAttr(setAttr(m, "data-start", sc.start), "data-duration", sc.duration),
    );
    html = html.replace(new RegExp(`<audio\\s+id="vo-${id}"[^>]*>`), (m) =>
      setAttr(setAttr(m, "data-start", +(sc.start + sc.lead).toFixed(3)), "data-duration", +(sc.speechEnd - sc.lead).toFixed(3)),
    );
  }
  writeFileSync(indexPath, html);
}

console.log(`Gesamtdauer: ${total}s (${(total / 60).toFixed(2)} min)`);
for (const [id, sc] of Object.entries(scenes))
  console.log(`  ${id.padEnd(11)} start ${String(sc.start).padStart(8)}  dauer ${String(sc.duration).padStart(7)}  (${sc.source})`);
for (const w of warnings) console.warn("WARNUNG:", w);
