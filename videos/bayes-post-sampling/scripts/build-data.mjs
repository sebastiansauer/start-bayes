// Erzeugt assets/data.js: alle Zahlen, die das Video zeigt, aus einem festen Seed.
// Aufruf: node scripts/build-data.mjs
import { writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const out = join(here, "..", "assets", "data.js");

// --- Seeded PRNG (mulberry32) ------------------------------------------------
function mulberry32(a) {
  return function () {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = mulberry32(20261111);
function randn() {
  // Box-Muller
  let u = 0, v = 0;
  while (u === 0) u = rand();
  while (v === 0) v = rand();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

// --- Globusversuch: W = 6 Wasser bei N = 9 Würfen ---------------------------
const W = 6, N = 9;
const tosses = ["W", "L", "W", "W", "L", "W", "L", "W", "W"];
function choose(n, k) {
  let r = 1;
  for (let i = 1; i <= k; i++) r = (r * (n - k + i)) / i;
  return r;
}
const binom = (k, n, p) => choose(n, k) * p ** k * (1 - p) ** (n - k);

// Bayes-Box mit g = 11 Gitterwerten, flacher Prior
const g = 11;
const grid = Array.from({ length: g }, (_, i) => i / (g - 1));
const prior = grid.map(() => 1 / g);
const lik = grid.map((p) => binom(W, N, p));
const unstd = grid.map((p, i) => prior[i] * lik[i]);
const sumUnstd = unstd.reduce((a, b) => a + b, 0);
const post = unstd.map((u) => u / sumUnstd);
const bayesbox = grid.map((p, i) => ({
  p,
  prior: prior[i],
  likelihood: lik[i],
  post: post[i],
}));

// Lose pro Zeile (Post × 100, gerundet) – Frame 3
const lose = post.map((x) => Math.round(x * 100));

// 10 000 Ziehungen mit Zurücklegen, gewichtet nach Post
const nDraws = 10000;
const cum = [];
post.reduce((acc, x, i) => ((cum[i] = acc + x), cum[i]), 0);
const drawOne = () => {
  const u = rand();
  for (let i = 0; i < g; i++) if (u <= cum[i]) return i;
  return g - 1;
};
const counts = new Array(g).fill(0);
const firstDraws = [];
for (let i = 0; i < nDraws; i++) {
  const k = drawOne();
  counts[k]++;
  if (i < 80) firstDraws.push(k);
}
const nAbove50 = counts.reduce((a, c, i) => (grid[i] > 0.5 ? a + c : a), 0);
const nAt50 = counts[5];

// Glatte Post-Kurve: Beta(W+1, N-W+1) = Beta(7, 4), normiert auf Maximum 1
const betaDensity = (p) => (p <= 0 || p >= 1 ? 0 : p ** W * (1 - p) ** (N - W));
const curveN = 201;
const curveRaw = Array.from({ length: curveN }, (_, i) => betaDensity(i / (curveN - 1)));
const curveMax = Math.max(...curveRaw);
const curve = curveRaw.map((y) => y / curveMax);

// --- Wanderer im Nebel: Metropolis auf Beta(7,4) ----------------------------
const proposalSd = 0.08;
const nSteps = 10000;
let x = 0.3;
const walk = [x];
const detail = []; // die ersten Schritte ausführlich, für die Zeitlupe
for (let s = 1; s <= nSteps; s++) {
  const prop = x + proposalSd * randn();
  const hHere = betaDensity(x) / curveMax;
  const hThere = betaDensity(prop) / curveMax;
  const ratio = hHere > 0 ? hThere / hHere : hThere > 0 ? 1 : 0;
  const u = rand();
  const accept = u < ratio;
  if (detail.length < 40) {
    detail.push({
      from: x,
      to: prop,
      hHere,
      hThere,
      uphill: hThere >= hHere,
      accept,
      ratio: Math.min(1, ratio),
    });
  }
  if (accept) x = prop;
  walk.push(x);
}
const nBins = 40;
const hist = new Array(nBins).fill(0);
const histSteps = []; // Zwischenstände alle 250 Schritte, normiert auf das Endmaximum
const binOf = (v) => Math.min(nBins - 1, Math.max(0, Math.floor(v * nBins)));
walk.forEach((v, i) => {
  hist[binOf(v)]++;
  if (i % 250 === 0 || i === walk.length - 1) histSteps.push(hist.slice());
});
const histMax = Math.max(...hist);
const histNorm = hist.map((h) => h / histMax);
const histStepsNorm = histSteps.map((h) => h.map((c) => +(c / histMax).toFixed(4)));
// Sollwert der Kurve je Bin (Mitte), für den Vergleich im Video
const histTarget = Array.from({ length: nBins }, (_, b) => betaDensity((b + 0.5) / nBins) / curveMax);

// --- Regression: Lernstunden → Klausurpunkte (illustrativ) ------------------
const nPts = 22;
const regPoints = Array.from({ length: nPts }, () => {
  const h = 2 + rand() * 28;
  const y = 38 + 1.8 * h + randn() * 8;
  return { x: +h.toFixed(1), y: +Math.min(100, Math.max(0, y)).toFixed(1) };
});
const regLine = { intercept: 38, slope: 1.8, sigma: 8 };

// Stan-Tabelle (illustrative Posteriori-Stichproben)
const stanRows = Array.from({ length: 8 }, () => ({
  intercept: +(38 + randn() * 3.1).toFixed(1),
  slope: +(1.8 + randn() * 0.17).toFixed(2),
  sigma: +(8.2 + Math.abs(randn()) * 1.1).toFixed(1),
}));

const data = {
  tosses,
  W,
  N,
  grid,
  bayesbox,
  lose,
  nDraws,
  counts,
  firstDraws,
  nAbove50,
  nAt50,
  curve,
  walk,
  walkDetail: detail,
  hist: histNorm,
  histSteps: histStepsNorm,
  histTarget,
  histCounts: hist,
  regPoints,
  regLine,
  stanRows,
};

writeFileSync(out, `// generiert von scripts/build-data.mjs – nicht von Hand ändern\nwindow.__DATA = ${JSON.stringify(data)};\n`);
console.log(`geschrieben: ${out}`);
console.log("Post:", post.map((x) => x.toFixed(3)).join(" "));
console.log("Lose:", lose.join(" "));
console.log("counts:", counts.join(" "), "| >0.5:", nAbove50, "| =0.5:", nAt50);
console.log("Akzeptanzrate Wanderer:", (walk.filter((v, i) => i > 0 && v !== walk[i - 1]).length / nSteps).toFixed(2));
console.log("Hist vs Ziel (erste 10 Bins):", histNorm.slice(0, 10).map((v) => v.toFixed(2)).join(" "), "|", histTarget.slice(0, 10).map((v) => v.toFixed(2)).join(" "));
