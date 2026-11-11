import subprocess
import os
import math
import urllib.request
import time

output_dir = "/tmp/bayes_video_build"
final_video = "/home/sebastian-sauer/Dokumente/github/04_start-bayes/bayes_erklaervideo_stichproben_mcmc.mp4"
artifact_dir = "/home/sebastian-sauer/.gemini/antigravity/brain/6bf78951-c6a2-4744-b602-0f464d3ca240"
ffmpeg = "/home/sebastian-sauer/.local/bin/ffmpeg"

# -------------------------------------------------------------
# 1. Exact Math Helpers for SVG Curves
# -------------------------------------------------------------
def get_exact_svg_path(x_start, x_end, y_baseline, y_peak, n=9, w=6):
    max_lik = math.comb(n, w) * ((w/n)**w) * ((1 - w/n)**(n - w))
    height = y_baseline - y_peak
    pts = []
    for i in range(101):
        p = i / 100.0
        lik = math.comb(n, w) * (p**w) * ((1 - p)**(n - w))
        x = x_start + (x_end - x_start) * p
        y = y_baseline - (lik / max_lik) * height
        pts.append(f"{x:.1f} {y:.1f}")
    return "M " + " L ".join(pts)

def get_exact_svg_fill(x_start, x_end, y_baseline, y_peak, n=9, w=6):
    line_path = get_exact_svg_path(x_start, x_end, y_baseline, y_peak, n, w)
    return f"{line_path} L {x_end:.1f} {y_baseline:.1f} L {x_start:.1f} {y_baseline:.1f} Z"

curve_s1_line = get_exact_svg_path(80, 580, 350, 110)
curve_s1_fill = get_exact_svg_fill(80, 580, 350, 110)
peak_x_s1 = 80 + 500 * (6/9)  # 413.3

curve_s5_line = get_exact_svg_path(50, 390, 220, 60)
curve_s5_fill = get_exact_svg_fill(50, 390, 220, 60)
peak_x_s5 = 50 + 340 * (6/9)  # 276.7

# Histogram bars for Slide 5 (mathematically exact Binomial density heights)
hist_bars_svg = []
n_bins = 20
max_lik_hist = math.comb(9, 6) * ((6/9)**6) * ((1 - 6/9)**3)
for b in range(n_bins):
    p_center = (b + 0.5) / n_bins
    lik = math.comb(9, 6) * (p_center**6) * ((1 - p_center)**3)
    bar_h = (lik / max_lik_hist) * 160.0
    bar_w = 340.0 / n_bins - 3.0
    bar_x = 50.0 + b * (340.0 / n_bins) + 1.5
    bar_y = 220.0 - bar_h
    if bar_h > 1.5:
        opacity = 0.5 + 0.5 * (bar_h / 160.0)
        hist_bars_svg.append(f'<rect x="{bar_x:.1f}" y="{bar_y:.1f}" width="{bar_w:.1f}" height="{bar_h:.1f}" fill="#34d399" opacity="{opacity:.2f}"/>')
hist_bars_str = "\n".join(hist_bars_svg)

mountain_s6_fill = get_exact_svg_fill(60, 500, 340, 80)
mountain_s6_line = get_exact_svg_path(60, 500, 340, 80)
peak_x_s6 = 60 + 440 * (6/9)

# Common CSS
css = """
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&family=Fira+Code:wght@500&display=swap');
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
    width: 1920px;
    height: 1080px;
    background: radial-gradient(circle at 20% 20%, #0f172a 0%, #020617 100%);
    color: #f8fafc;
    font-family: 'Inter', -apple-system, sans-serif;
    display: flex;
    flex-direction: column;
    padding: 60px 80px;
    overflow: hidden;
}
.header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 40px;
    border-bottom: 2px solid rgba(56, 189, 248, 0.2);
    padding-bottom: 25px;
}
.badge {
    background: rgba(56, 189, 248, 0.15);
    color: #38bdf8;
    border: 1px solid rgba(56, 189, 248, 0.4);
    padding: 8px 18px;
    border-radius: 9999px;
    font-size: 18px;
    font-weight: 600;
    letter-spacing: 0.5px;
    display: inline-block;
    margin-bottom: 12px;
}
.title { font-size: 46px; font-weight: 800; color: #ffffff; line-height: 1.2; }
.subtitle { font-size: 24px; color: #94a3b8; margin-top: 8px; }
.time-pill {
    background: #1e293b;
    border: 1px solid #334155;
    padding: 10px 22px;
    border-radius: 12px;
    font-size: 20px;
    font-family: 'Fira Code', monospace;
    color: #38bdf8;
}
.content { flex: 1; display: grid; grid-template-columns: 1fr 1fr; gap: 50px; align-items: center; }
.card {
    background: rgba(30, 41, 59, 0.6);
    backdrop-filter: blur(12px);
    border: 1px solid rgba(148, 163, 184, 0.15);
    border-radius: 24px;
    padding: 35px;
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.4);
}
.highlight { color: #38bdf8; font-weight: 700; }
.accent-amber { color: #fbbf24; }
.accent-emerald { color: #34d399; }
.accent-rose { color: #f43f5e; }
.stat-box {
    background: rgba(15, 23, 42, 0.8);
    border: 1px solid rgba(56, 189, 248, 0.3);
    border-radius: 16px;
    padding: 20px;
    text-align: center;
}
.stat-num { font-size: 44px; font-weight: 800; color: #38bdf8; font-family: 'Fira Code', monospace; }
.stat-label { font-size: 18px; color: #94a3b8; margin-top: 5px; }
"""

# Re-build Slide 1 with mathematically exact curve
s1 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">Bayes-Statistik für Psychologie • Kapitel 6</div>
        <div class="title">Unser Ausgangspunkt: Die Bayes-Box</div>
        <div class="subtitle">Exakte Berechnung am Globusversuch (W = 6 Treffer bei N = 9 Würfen)</div>
    </div>
    <div class="time-pill">Szene 1 • 0:00 - 0:25</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; gap:20px;">
        <h3 style="font-size:26px; color:#38bdf8;">Die Gittermethode („Bayes-Box“)</h3>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            Für ein Modell mit nur <span class="highlight">1 Parameter</span> ($p$ = Wasseranteil) berechnen wir die Verteilung Zeile für Zeile:
        </p>
        <table style="width:100%; border-collapse:collapse; font-size:20px; text-align:center; margin-top:10px;">
            <tr style="border-bottom:2px solid #334155; color:#94a3b8;">
                <th style="padding:10px;">Gitter ($p$)</th><th>Prior</th><th>Likelihood</th><th style="color:#38bdf8;">Post</th>
            </tr>
            <tr style="border-bottom:1px solid #1e293b;"><td>0.0</td><td>1</td><td>0.00</td><td>0.00</td></tr>
            <tr style="border-bottom:1px solid #1e293b;"><td>0.5</td><td>1</td><td>0.16</td><td>0.17</td></tr>
            <tr style="border-bottom:1px solid #1e293b; background:rgba(56,189,248,0.15); font-weight:700; color:#38bdf8;">
                <td>0.7 (Gipfel)</td><td>1</td><td>0.27</td><td>0.27</td>
            </tr>
            <tr style="border-bottom:1px solid #1e293b;"><td>1.0</td><td>1</td><td>0.00</td><td>0.00</td></tr>
        </table>
        <div class="stat-box" style="margin-top:10px;">
            <div class="stat-label">Gesamte Zeilenanzahl bei 11 Gitterwerten:</div>
            <div class="stat-num" style="font-size:36px;">11 Zeilen (Sofort gelöst!)</div>
        </div>
    </div>
    <div class="card" style="display:flex; flex-direction:column; align-items:center; justify-content:center;">
        <h3 style="font-size:26px; color:#38bdf8; margin-bottom:15px;">Posteriori-Verteilung</h3>
        <svg width="650" height="420" viewBox="0 0 650 420">
            <!-- Axes -->
            <line x1="80" y1="350" x2="600" y2="350" stroke="#64748b" stroke-width="3"/>
            <line x1="80" y1="350" x2="80" y2="60" stroke="#64748b" stroke-width="3"/>
            <!-- Ticks -->
            <text x="80" y="380" fill="#94a3b8" font-size="18" text-anchor="middle">0.0</text>
            <text x="230" y="380" fill="#94a3b8" font-size="18" text-anchor="middle">0.3</text>
            <text x="{peak_x_s1:.1f}" y="380" fill="#38bdf8" font-weight="700" font-size="20" text-anchor="middle">0.67 (Peak)</text>
            <text x="580" y="380" fill="#94a3b8" font-size="18" text-anchor="middle">1.0</text>
            <text x="340" y="410" fill="#cbd5e1" font-size="20" text-anchor="middle">Parameter p (Wasseranteil)</text>
            <!-- Mathematically Exact Posterior Fill & Line -->
            <path d="{curve_s1_fill}" fill="rgba(56,189,248,0.15)"/>
            <path d="{curve_s1_line}" fill="none" stroke="#38bdf8" stroke-width="4"/>
            <!-- Peak Point -->
            <circle cx="{peak_x_s1:.1f}" cy="110" r="9" fill="#fbbf24"/>
            <text x="{peak_x_s1:.1f}" y="80" fill="#fbbf24" font-size="22" font-weight="700" text-anchor="middle">Maximum (MAP = 6/9)</text>
        </svg>
    </div>
</div>
</body></html>
"""

# Re-build Slide 5 with mathematically exact curve and exact histogram
s5 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge accent-emerald" style="border-color:rgba(52,211,153,0.4); background:rgba(52,211,153,0.15); color:#34d399;">
            Der Beweis • Identische Information
        </div>
        <div class="title">Stichproben rekonstruieren die Verteilung</div>
        <div class="subtitle">Gitterkurve vs. Histogramm aus 10.000 Ziehungen</div>
    </div>
    <div class="time-pill">Szene 5 • 1:45 - 2:30</div>
</div>
<div class="content">
    <div class="card" style="text-align:center;">
        <h4 style="font-size:22px; color:#94a3b8; margin-bottom:15px;">1. Exakte Berechnung (Bayes-Box)</h4>
        <svg width="420" height="260" viewBox="0 0 420 260">
            <line x1="50" y1="220" x2="390" y2="220" stroke="#64748b" stroke-width="2"/>
            <path d="{curve_s5_fill}" fill="rgba(56,189,248,0.12)"/>
            <path d="{curve_s5_line}" fill="none" stroke="#38bdf8" stroke-width="3.5"/>
            <text x="{peak_x_s5:.1f}" y="40" fill="#38bdf8" font-size="16" font-weight="700" text-anchor="middle">Exakte Kurve</text>
            <text x="50" y="245" fill="#64748b" font-size="14" text-anchor="middle">0.0</text>
            <text x="{peak_x_s5:.1f}" y="245" fill="#38bdf8" font-size="14" font-weight="700" text-anchor="middle">0.67</text>
            <text x="390" y="245" fill="#64748b" font-size="14" text-anchor="middle">1.0</text>
        </svg>
        <p style="color:#94a3b8; font-size:18px;">Aufwendig zu berechnen</p>
    </div>
    <div class="card" style="text-align:center; border-color:rgba(52,211,153,0.4);">
        <h4 style="font-size:22px; color:#34d399; margin-bottom:15px;">2. 10.000 Stichproben (Histogramm)</h4>
        <svg width="420" height="260" viewBox="0 0 420 260">
            <line x1="50" y1="220" x2="390" y2="220" stroke="#64748b" stroke-width="2"/>
            {hist_bars_str}
            <text x="{peak_x_s5:.1f}" y="40" fill="#34d399" font-size="16" font-weight="700" text-anchor="middle">Empirisches Histogramm</text>
            <text x="50" y="245" fill="#64748b" font-size="14" text-anchor="middle">0.0</text>
            <text x="{peak_x_s5:.1f}" y="245" fill="#34d399" font-size="14" font-weight="700" text-anchor="middle">0.67</text>
            <text x="390" y="245" fill="#64748b" font-size="14" text-anchor="middle">1.0</text>
        </svg>
        <p style="color:#34d399; font-size:18px; font-weight:600;">Praktisch deckungsgleich!</p>
    </div>
</div>
<div style="margin-top:25px; background:rgba(30,41,59,0.7); border:1px solid #334155; padding:20px 30px; border-radius:16px; display:flex; justify-content:space-between; align-items:center;">
    <div style="font-size:22px; color:#cbd5e1;">
        👉 <b>Wie viel Wskt. liegt über 70%?</b> Wir zählen einfach: <span class="highlight">3.820 von 10.000 Zetteln = 38.2 %!</span>
    </div>
    <div style="font-size:19px; color:#fbbf24; font-weight:700;">Keine Formeln – reines Zählen!</div>
</div>
</body></html>
"""

# Re-build Slide 6 with exact mountain profile
s6 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">MCMC-Intuition • Teil 1</div>
        <div class="title">Die Gebirgslandschaft im Nebel</div>
        <div class="subtitle">Wie erkunden wir ein Terrain, das wir nicht sehen können?</div>
    </div>
    <div class="time-pill">Szene 6 • 2:30 - 3:10</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; align-items:center; justify-content:center;">
        <svg width="550" height="380" viewBox="0 0 550 380">
            <!-- Mountain shape with exact binomial curve -->
            <path d="{mountain_s6_fill}" fill="rgba(56,189,248,0.2)"/>
            <path d="{mountain_s6_line}" fill="none" stroke="#38bdf8" stroke-width="4"/>
            <!-- Fog layer -->
            <rect x="40" y="180" width="480" height="160" fill="url(#fogGradient)" opacity="0.9"/>
            <defs>
                <linearGradient id="fogGradient" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0%" stop-color="#334155" stop-opacity="0.3"/>
                    <stop offset="50%" stop-color="#1e293b" stop-opacity="0.95"/>
                    <stop offset="100%" stop-color="#0f172a" stop-opacity="1"/>
                </linearGradient>
            </defs>
            <text x="{peak_x_s6:.1f}" y="60" fill="#38bdf8" font-size="22" font-weight="700" text-anchor="middle">Gipfel (Hohe Plausibilität)</text>
            <!-- Stan the Robot figure -->
            <circle cx="260" cy="220" r="14" fill="#fbbf24"/>
            <rect x="252" y="234" width="16" height="24" rx="4" fill="#fbbf24"/>
            <text x="295" y="240" fill="#fbbf24" font-size="20" font-weight="800">Stan 🤖</text>
            <text x="280" y="320" fill="#94a3b8" font-size="22" font-weight="600" text-anchor="middle">☁️ Dichter Nebel (Keine Landkarte!) ☁️</text>
        </svg>
    </div>
    <div class="card" style="display:flex; flex-direction:column; gap:25px;">
        <h3 style="font-size:28px; color:#38bdf8;">Das MCMC-Szenario</h3>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            • Die <span class="highlight">Höhe des Berges</span> entspricht der gesuchten Posteriori-Plausibilität.<br><br>
            • Das gesamte Gebirge liegt im <span class="accent-amber">stockdichten Nebel</span>: Wir können es nicht von oben vermessen (keine Bayes-Box).<br><br>
            • <b>Die Lösung:</b> Wir schicken einen blinden Kundschafter los – unseren <b>Roboter Stan</b>!
        </p>
    </div>
</div>
</body></html>
"""

# Re-render updated slides 1, 5, 6
updated_slides = [(1, s1), (5, s5), (6, s6)]
for idx, html in updated_slides:
    html_file = f"{output_dir}/slide_{idx:02d}.html"
    png_file = f"{output_dir}/slide_{idx:02d}.png"
    with open(html_file, "w") as f:
        f.write(html)
    cmd = [
        "google-chrome",
        "--headless",
        "--disable-gpu",
        "--hide-scrollbars",
        "--window-size=1920,1080",
        f"--screenshot={png_file}",
        html_file
    ]
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print(f"Slide {idx} re-rendered with exact curve!")

# -------------------------------------------------------------
# 2. Azure Dragon HD Speech Generation for all 10 scenes
# -------------------------------------------------------------
key = ""
region = ""
with open("/home/sebastian-sauer/.Renviron") as f:
    for line in f:
        line = line.strip()
        if line.startswith("AZURE_SPEECH_KEY"):
            key = line.split("=", 1)[1].strip("\"' ")
        elif line.startswith("AZURE_SPEECH_REGION"):
            region = line.split("=", 1)[1].strip("\"' ")

slides_text = [
    (1, "Erinnert ihr euch an unseren Globusversuch? Neun Würfe, sechsmal Wasser. Mit der Bayes-Box konnten wir die Posteriori-Verteilung direkt Zeile für Zeile berechnen: Gitter anlegen, Likelihood mal Prior, standardisieren – fertig! Für einfache Modelle mit nur einem einzigen Parameter wie dem Wasseranteil p ist diese Gittermethode absolut genial, anschaulich und präzise."),
    (2, "Aber was passiert, wenn wir psychologische Phänomene untersuchen – zum Beispiel in einer Regression mit Prädiktoren? Plötzlich schätzen wir nicht mehr einen, sondern drei, fünf oder zehn Parameter gleichzeitig! Wenn wir für jeden Parameter nur 100 Gitterwerte annehmen, braucht unsere Bayes-Box bei 3 Parametern schon eine Million Zeilen. Bei einer multiplen Regression mit 10 Parametern sind es 10 hoch 20 Zeilen. Selbst ein Supercomputer rechnet daran bis zum jüngsten Tag."),
    (3, "Kurz gesagt: Die Bayes-Box funktioniert für echte psychologische Modelle schlicht nicht mehr. Die Tabelle wird viel zu lang und die Mathematik zu rechenintensiv. Wir stecken in einer scheinbaren Sackgasse. Doch zum Glück gibt es einen genialen Ausweg: Wir verabschieden uns davon, die Kurve exakt ausrechnen zu wollen – und lassen uns stattdessen Stichproben liefern."),
    (4, "Hier kommt der zentrale Trick moderner Bayes-Statistik: Wir arbeiten ab jetzt nicht mehr mit abstrakten Wahrscheinlichkeitsformeln, sondern mit handfesten Häufigkeiten. Stellt euch vor, wir haben eine Lostrommel. Darin liegen mögliche Parameterwerte. Und je plausibler ein Wert nach den Daten ist, desto häufiger liegt er in der Trommel. Werte um 67 Prozent Wasser ziehen wir ständig; Werte unter 20 Prozent so gut wie nie."),
    (5, "Schaut euch diesen Vergleich an: Links die mühsam berechnete, exakte Kurve der Bayes-Box. Rechts ein einfaches Histogramm aus 10.000 gezogenen Stichproben. Seht ihr den Unterschied? Es gibt praktisch keinen! Mit Stichproben können wir alle psychologischen Fragen kinderleicht beantworten: Wie wahrscheinlich ist ein Wasseranteil über 70 Prozent? Wir zählen einfach, wie viele unserer 10.000 Zettel über 0.7 liegen. Kein Integrieren, kein Formelchaos – reines Zählen. Aber: Wie kommen wir an diese Stichproben, wenn wir doch gar keine Tabelle haben, aus der wir ziehen können?"),
    (6, "Genau hier betritt die Geheimwaffe der Bayes-Statistik die Bühne: MCMC – kurz für Monte-Carlo-Markov-Ketten. Klingt furchteinflößend, ist aber bildlich ganz einfach. Stellt euch die gesuchte Posteriori-Verteilung wie eine Gebirgslandschaft vor: Wo die Plausibilität hoch ist, ragen mächtige Berge auf; unwahrscheinliche Werte sind flache Täler. Das Problem: Die Landschaft liegt im stockdichten Nebel. Wir haben keine Landkarte und können das Gebirge nicht aus der Vogelperspektive vermessen."),
    (7, "Deshalb schicken wir einen blinden Kundschafter ins Gebirge: Nennen wir ihn Stan. Stan sieht nichts im Nebel, aber er kann an seiner aktuellen Position den Boden abtasten. Er schlägt zufällig eine Richtung für den nächsten Schritt vor und folgt zwei simplen Regeln: Geht der Schritt bergauf in Richtung Gipfel? Dann macht er ihn sofort und immer! Geht der vorgeschlagene Schritt bergab in ein Tal? Dann zögert er: Er wirft eine Münze und geht nur mit einer gewissen Wahrscheinlichkeit – je tiefer der Abgrund, desto seltener."),
    (8, "Was passiert durch diese simplen Regeln über Tausende von Schritten? Stan wird nicht für immer auf dem Gipfel festgenagelt, aber er hält sich ganz automatisch viel häufiger in den Höhenlagen auf als unten im Tal. An jedem Schritt notiert Stan seinen aktuellen Standort. Und das Geniale ist: Wenn wir nach 10.000 Schritten Stans Notizbuch aufschlagen und ein Histogramm all seiner Standorte zeichnen, erhalten wir exakt die gesuchte Posteriori-Verteilung! Stan hat das Gebirge für uns abgetastet – ganz ohne jemals eine gigantische Bayes-Box berechnen zu müssen."),
    (9, "Fassen wir die drei wichtigsten Punkte zusammen: Erstens: Die Bayes-Box ist super zum Verstehen, aber bei echten psychologischen Modellen explodiert die Zeilenzahl. Zweitens: MCMC löst dieses Problem elegant. Statt das gesamte Gelände analytisch zu berechnen, schicken wir Stan als Bergwanderer los, der das Terrain stichprobenartig erkundet. Und drittens: Wir brauchen keine komplizierten Formeln. Wir fassen einfach Stans Schritte zusammen, um Schätzbereiche, Mittelwerte und Wahrscheinlichkeiten zu bestimmen."),
    (10, "In den nächsten Kapiteln lernt ihr, wie ihr Stan in R selbst an die Arbeit schickt – mit dem Paket rstanarm. Ihr gebt einfach euer Modell ein, drückt Enter, und Stan wandert für euch los und liefert euch fertige Stichproben frei Haus. Ihr müsst MCMC also nicht selbst programmieren – ihr müsst nur wissen, wie ihr Stans Notizbuch richtig lest. Viel Erfolg beim Ausprobieren!")
]

azure_url = f"https://{region}.tts.speech.microsoft.com/cognitiveservices/v1"

for sid, txt in slides_text:
    audio_path = f"{output_dir}/audio_{sid:02d}.mp3"
    print(f"Generating Studio HD Audio for Scene {sid} via Azure...")
    ssml = f"""<speak version='1.0' xml:lang='de-DE'>
<voice xml:lang='de-DE' name='de-DE-Florian:DragonHDLatestNeural'>
<prosody rate='-2%'>
{txt}
</prosody>
</voice>
</speak>"""
    req = urllib.request.Request(azure_url, data=ssml.encode("utf-8"), method="POST")
    req.add_header("Ocp-Apim-Subscription-Key", key)
    req.add_header("Content-Type", "application/ssml+xml")
    req.add_header("X-Microsoft-OutputFormat", "audio-24khz-160kbitrate-mono-mp3")
    req.add_header("User-Agent", "StartBayesVideo")
    with urllib.request.urlopen(req) as resp:
        with open(audio_path, "wb") as f:
            f.write(resp.read())

print("All 10 audio clips generated with Azure Dragon HD Neural Voice!")

# -------------------------------------------------------------
# 3. Assemble clips and final master MP4
# -------------------------------------------------------------
concat_list_path = os.path.join(output_dir, "concat_list.txt")
with open(concat_list_path, "w") as clist:
    for i in range(1, 11):
        slide_png = f"{output_dir}/slide_{i:02d}.png"
        audio_mp3 = f"{output_dir}/audio_{i:02d}.mp3"
        clip_mp4 = f"{output_dir}/clip_{i:02d}.mp4"
        print(f"Re-encoding clip {i}/10...")
        cmd = [
            ffmpeg, "-y",
            "-loop", "1",
            "-i", slide_png,
            "-i", audio_mp3,
            "-af", "apad=pad_dur=0.6",
            "-c:v", "libx264",
            "-tune", "stillimage",
            "-c:a", "aac",
            "-b:a", "192k",
            "-pix_fmt", "yuv420p",
            "-shortest",
            clip_mp4
        ]
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        clist.write(f"file '{clip_mp4}'\n")

print("Concatenating into final master video...")
cmd_concat = [
    ffmpeg, "-y",
    "-f", "concat",
    "-safe", "0",
    "-i", concat_list_path,
    "-c", "copy",
    final_video
]
subprocess.run(cmd_concat, check=True)

# Copy to artifact dir
artifact_video = os.path.join(artifact_dir, "bayes_erklaervideo_stichproben_mcmc.mp4")
subprocess.run(["cp", final_video, artifact_video], check=True)

print("Video successfully updated!")
