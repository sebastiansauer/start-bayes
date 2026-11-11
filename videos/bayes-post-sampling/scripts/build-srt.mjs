// Schreibt captions.srt aus den Wortzeiten (Azure) bzw. der Schätzung.
// Gruppen: höchstens 7 Wörter oder ca. 46 Zeichen, Schnitt an Satzenden.
// Aufruf: node scripts/build-srt.mjs   (nach build-timing.mjs)
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..");
const cuesSrc = readFileSync(join(root, "assets", "cues.js"), "utf8");
const cues = JSON.parse(cuesSrc.slice(cuesSrc.indexOf("= ") + 2).replace(/;\s*$/, ""));
const cfg = JSON.parse(readFileSync(join(here, "anchors.json"), "utf8"));

const script = readFileSync(join(root, "SCRIPT.md"), "utf8");
const lines = {};
let current = null;
for (const raw of script.split("\n")) {
  const m = raw.match(/^## Line (\d+)/);
  if (m) { current = +m[1]; lines[current] = []; continue; }
  if (current && /^ {4}\S/.test(raw)) lines[current].push(raw.trim());
}

// Schätzung identisch zu build-timing.mjs
function estimateWords(text, cps) {
  const tokens = text.split(/\s+/).filter(Boolean);
  const weights = tokens.map((t) => t.length + 1 + (/[.!?:]$/.test(t) ? 5 : /,$/.test(t) ? 2 : 0));
  const total = weights.reduce((a, b) => a + b, 0);
  const duration = text.length / cps;
  let acc = 0;
  return tokens.map((t, i) => { const start = (acc / total) * duration; acc += weights[i]; return { text: t, start, end: (acc / total) * duration }; });
}

// Azure-Wörter (ohne Satzzeichen, teils anders getrennt: „M-C-M-C" → M C M C, „der Weg" → ein Wort)
// auf die Skript-Tokens abbilden; Zeiten kommen von Azure, Text vom Skript.
const strip = (t) => t.toLowerCase().replace(/[^a-z0-9äöüß]/g, "");
function alignWords(tokens, azure) {
  const out = [];
  let i = 0, j = 0;
  while (i < tokens.length && j < azure.length) {
    const tok = strip(tokens[i]);
    let acc = "", k = j;
    while (k < azure.length && acc.length < tok.length) { acc += strip(azure[k].text); k++; }
    if (acc === tok) { out.push({ text: tokens[i], start: azure[j].start, end: azure[k - 1].end }); i++; j = k; continue; }
    const az = strip(azure[j].text);
    let acc2 = "", m = i;
    while (m < tokens.length && acc2.length < az.length) { acc2 += strip(tokens[m]); m++; }
    if (acc2 === az) { out.push({ text: tokens.slice(i, m).join(" "), start: azure[j].start, end: azure[j].end }); i = m; j++; continue; }
    out.push({ text: azure[j].text, start: azure[j].start, end: azure[j].end }); i++; j++;
  }
  return out;
}

const fmt = (s) => {
  const ms = Math.round(s * 1000);
  const h = Math.floor(ms / 3600000), m = Math.floor((ms % 3600000) / 60000), sec = Math.floor((ms % 60000) / 1000), r = ms % 1000;
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${String(sec).padStart(2, "0")},${String(r).padStart(3, "0")}`;
};

const entries = [];
for (const sc of cfg.scenes) {
  const scene = cues.scenes[sc.id];
  const text = lines[sc.line].join(" ");
  const scriptTokens = text.split(/\s+/).filter(Boolean);
  const wordsFile = join(root, "assets", "voice", `line-${sc.line}.words.json`);
  let words;
  if (existsSync(wordsFile)) {
    // Azure-Wörter tragen keine Satzzeichen; die Skript-Tokens liefern sie per Position
    words = alignWords(scriptTokens, JSON.parse(readFileSync(wordsFile, "utf8")));
  } else words = estimateWords(text, cfg.charsPerSecond);
  const base = scene.start + scene.lead;
  let group = [];
  const flush = () => {
    if (!group.length) return;
    entries.push({ start: base + group[0].start, end: base + group[group.length - 1].end + 0.15, text: group.map((g) => g.text).join(" ") });
    group = [];
  };
  for (const w of words) {
    group.push(w);
    const chars = group.reduce((a, g) => a + g.text.length + 1, 0);
    if (/[.!?:]$/.test(w.text) || group.length >= 7 || chars >= 46) flush();
  }
  flush();
}
// keine Überlappungen
for (let i = 0; i < entries.length - 1; i++) entries[i].end = Math.min(entries[i].end, entries[i + 1].start - 0.05);

const srt = entries.map((e, i) => `${i + 1}\n${fmt(e.start)} --> ${fmt(e.end)}\n${e.text}\n`).join("\n");
writeFileSync(join(root, "captions.srt"), srt);
console.log(`captions.srt: ${entries.length} Einträge, letzter endet bei ${fmt(entries[entries.length - 1].end)}`);
