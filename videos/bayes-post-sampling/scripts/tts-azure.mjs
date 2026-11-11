// Vertont SCRIPT.md mit Azure AI Speech (de-DE-ConradNeural) und schreibt je Zeile
//   assets/voice/line-N.wav         (24 kHz, 16 bit, mono)
//   assets/voice/line-N.words.json  [{text,start,end}] in Sekunden ab Clipbeginn
//
// Voraussetzung: .env im Projektordner mit
//   AZURE_SPEECH_KEY=...
//   AZURE_SPEECH_REGION=germanywestcentral   (oder westeurope usw.)
// und einmalig: npm install
//
// Aufruf: node scripts/tts-azure.mjs            (alle Zeilen)
//         node scripts/tts-azure.mjs 3          (nur Zeile 3)
//         node scripts/tts-azure.mjs --probe    (20-Sekunden-Hörprobe: assets/voice/probe.wav)
// Danach: node scripts/build-timing.mjs && node scripts/build-srt.mjs
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import sdk from "microsoft-cognitiveservices-speech-sdk";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "..");
const voiceDir = join(root, "assets", "voice");
mkdirSync(voiceDir, { recursive: true });

// --- .env lesen (ohne Abhängigkeit) ---------------------------------------------
const envPath = join(root, ".env");
if (existsSync(envPath)) {
  for (const raw of readFileSync(envPath, "utf8").split("\n")) {
    const m = raw.match(/^\s*([A-Z_]+)\s*=\s*"?([^"\n]*)"?\s*$/);
    if (m && !process.env[m[1]]) process.env[m[1]] = m[2];
  }
}
const KEY = process.env.AZURE_SPEECH_KEY, REGION = process.env.AZURE_SPEECH_REGION;
if (!KEY || !REGION) {
  console.error("AZURE_SPEECH_KEY und AZURE_SPEECH_REGION fehlen (in .env oder Umgebung).");
  process.exit(1);
}

const VOICE = "de-DE-ConradNeural";
const RATE = "-5%"; // Tempo 0.95 laut SCRIPT.md

// --- SCRIPT.md einlesen -----------------------------------------------------------
const script = readFileSync(join(root, "SCRIPT.md"), "utf8");
const lines = {};
let current = null;
for (const raw of script.split("\n")) {
  const m = raw.match(/^## Line (\d+)/);
  if (m) { current = +m[1]; lines[current] = []; continue; }
  if (current && /^ {4}\S/.test(raw)) lines[current].push(raw.trim());
}

const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
// Aussprache: „Bayes" wie „Bäjes", „Likelihood" englisch, „M-C-M-C" buchstabiert.
const PRON = [
  [/Bayes/g, '<phoneme alphabet="ipa" ph="ˈbɛːjəs">Bayes</phoneme>'],
  [/Likelihood/g, '<phoneme alphabet="ipa" ph="ˈlaɪklihʊd">Likelihood</phoneme>'],
  [/M-C-M-C/g, '<say-as interpret-as="characters">MCMC</say-as>'],
];
const pron = (s) => PRON.reduce((acc, [re, rep]) => acc.replace(re, rep), s);
// Jeder Skriptsatz wird ein eigener <s>-Block.
const ssmlFor = (sentences) => `<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" xmlns:mstts="https://www.w3.org/2001/mstts" xml:lang="de-DE">
  <voice name="${VOICE}"><prosody rate="${RATE}">
    ${sentences.map((s) => `<s>${pron(esc(s))}</s>`).join("\n    ")}
  </prosody></voice>
</speak>`;

function synthesize(ssml, outWav) {
  return new Promise((resolve, reject) => {
    const cfg = sdk.SpeechConfig.fromSubscription(KEY, REGION);
    cfg.speechSynthesisOutputFormat = sdk.SpeechSynthesisOutputFormat.Riff24Khz16BitMonoPcm;
    cfg.setProperty(sdk.PropertyId.SpeechServiceResponse_RequestWordBoundary, "true");
    const synth = new sdk.SpeechSynthesizer(cfg, null);
    const words = [];
    synth.wordBoundary = (_s, e) => {
      // audioOffset und duration in 100-ns-Ticks
      const type = e.boundaryType; // 0 Word, 1 Punctuation, 2 Sentence
      if (type === sdk.SpeechSynthesisBoundaryType.Word || type === 0 || type === "WordBoundary") {
        words.push({ text: e.text, start: +(e.audioOffset / 1e7).toFixed(3), end: +((e.audioOffset + e.duration) / 1e7).toFixed(3) });
      }
    };
    synth.speakSsmlAsync(
      ssml,
      (result) => {
        synth.close();
        if (result.reason === sdk.ResultReason.SynthesizingAudioCompleted) {
          writeFileSync(outWav, Buffer.from(result.audioData));
          resolve({ words, seconds: result.audioDuration / 1e7 });
        } else reject(new Error(`Synthese fehlgeschlagen: ${result.errorDetails || result.reason}`));
      },
      (err) => { synth.close(); reject(err); },
    );
  });
}

const arg = process.argv[2];
if (arg === "--probe") {
  const probe = ["Zur Erinnerung: die Bayes-Box.", "Für jede Zeile rechnen wir Prior mal Likelihood.", "Die Bayes-Box ist nicht falsch."];
  const r = await synthesize(ssmlFor(probe), join(voiceDir, "probe.wav"));
  console.log(`Hörprobe: assets/voice/probe.wav (${r.seconds.toFixed(1)} s)`);
  process.exit(0);
}
const wanted = arg ? [+arg] : Object.keys(lines).map(Number);
for (const n of wanted) {
  const r = await synthesize(ssmlFor(lines[n]), join(voiceDir, `line-${n}.wav`));
  writeFileSync(join(voiceDir, `line-${n}.words.json`), JSON.stringify(r.words));
  console.log(`Zeile ${n}: ${r.seconds.toFixed(1)} s, ${r.words.length} Wörter`);
}
console.log("Fertig. Jetzt: node scripts/build-timing.mjs && node scripts/build-srt.mjs");
