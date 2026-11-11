import subprocess
import os

slides_html = []

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
.title {
    font-size: 46px;
    font-weight: 800;
    color: #ffffff;
    line-height: 1.2;
}
.subtitle {
    font-size: 24px;
    color: #94a3b8;
    margin-top: 8px;
}
.time-pill {
    background: #1e293b;
    border: 1px solid #334155;
    padding: 10px 22px;
    border-radius: 12px;
    font-size: 20px;
    font-family: 'Fira Code', monospace;
    color: #38bdf8;
}
.content {
    flex: 1;
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 50px;
    align-items: center;
}
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
.stat-num {
    font-size: 44px;
    font-weight: 800;
    color: #38bdf8;
    font-family: 'Fira Code', monospace;
}
.stat-label {
    font-size: 18px;
    color: #94a3b8;
    margin-top: 5px;
}
"""

# Slide 1
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
            <text x="210" y="380" fill="#94a3b8" font-size="18" text-anchor="middle">0.3</text>
            <text x="430" y="380" fill="#38bdf8" font-weight="700" font-size="20" text-anchor="middle">0.67 (Peak)</text>
            <text x="590" y="380" fill="#94a3b8" font-size="18" text-anchor="middle">1.0</text>
            <text x="340" y="410" fill="#cbd5e1" font-size="20" text-anchor="middle">Parameter p (Wasseranteil)</text>
            <!-- Bell Curve -->
            <path d="M 80 348 Q 200 345 300 280 T 430 110 T 520 280 T 600 348" fill="none" stroke="#38bdf8" stroke-width="5"/>
            <!-- Peak Point -->
            <circle cx="430" cy="110" r="10" fill="#fbbf24"/>
            <text x="430" y="80" fill="#fbbf24" font-size="22" font-weight="700" text-anchor="middle">Maximum (MAP = 6/9)</text>
        </svg>
    </div>
</div>
</body></html>
"""

# Slide 2
s2 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge accent-rose" style="border-color:rgba(244,63,94,0.4); background:rgba(244,63,94,0.15); color:#f43f5e;">
            Das Problem • Der Fluch der Dimensionen
        </div>
        <div class="title">Warum die Bayes-Box kollabiert</div>
        <div class="subtitle">Vom 1-Parameter-Globus zur psychologischen Regression</div>
    </div>
    <div class="time-pill">Szene 2 • 0:25 - 0:50</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; gap:25px;">
        <h3 style="font-size:26px; color:#f43f5e;">Echte Modelle haben viele Parameter!</h3>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            In einer einfachen Regression (z. B. Psychotherapie-Effekt) schätzen wir mindestens <span class="highlight">3 Parameter gleichzeitig</span>:
        </p>
        <div style="background:#0f172a; border-left:6px solid #f43f5e; padding:20px 25px; border-radius:12px; font-family:'Fira Code'; font-size:24px; color:#f8fafc;">
            y<sub>i</sub> ~ Normal(&mu;<sub>i</sub>, &sigma;)<br>
            &mu;<sub>i</sub> = &beta;<sub>0</sub> + &beta;<sub>1</sub> &middot; x<sub>i</sub>
        </div>
        <p style="font-size:20px; color:#94a3b8;">
            Zu schätzen: Achsenabschnitt (&beta;<sub>0</sub>), Steigung (&beta;<sub>1</sub>) und Streuung (&sigma;).
        </p>
    </div>
    <div style="display:flex; flex-direction:column; gap:20px;">
        <div class="card" style="padding:22px; border-color:rgba(52,211,153,0.3);">
            <div style="display:flex; justify-content:space-between; align-items:center;">
                <div><b style="font-size:22px; color:#34d399;">1 Parameter (Globus)</b><br><span style="color:#94a3b8; font-size:18px;">100 Gitterwerte</span></div>
                <div class="stat-num" style="color:#34d399; font-size:32px;">100 Zeilen</div>
            </div>
        </div>
        <div class="card" style="padding:22px; border-color:rgba(251,191,36,0.4); background:rgba(251,191,36,0.08);">
            <div style="display:flex; justify-content:space-between; align-items:center;">
                <div><b style="font-size:22px; color:#fbbf24;">3 Parameter (1 Prädiktor)</b><br><span style="color:#94a3b8; font-size:18px;">100 &times; 100 &times; 100 Zeilen</span></div>
                <div class="stat-num" style="color:#fbbf24; font-size:32px;">1.000.000 Zeilen</div>
            </div>
        </div>
        <div class="card" style="padding:25px; border-color:rgba(244,63,94,0.6); background:rgba(244,63,94,0.12);">
            <div style="display:flex; justify-content:space-between; align-items:center;">
                <div><b style="font-size:24px; color:#f43f5e;">10 Parameter (Multipel)</b><br><span style="color:#cbd5e1; font-size:18px;">100<sup>10</sup> Kombinationen</span></div>
                <div class="stat-num" style="color:#f43f5e; font-size:36px;">10<sup>20</sup> Zeilen!</div>
            </div>
            <div style="margin-top:10px; color:#fca5a5; font-size:17px; font-weight:600;">
                ⚠️ Unendlich viel Speicher & Rechenzeit – Computer stürzt ab!
            </div>
        </div>
    </div>
</div>
</body></html>
"""

# Slide 3
s3 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge accent-amber" style="border-color:rgba(251,191,36,0.4); background:rgba(251,191,36,0.15); color:#fbbf24;">
            Didaktische Wende • Die Lösung
        </div>
        <div class="title">Die Sackgasse & Der Kniff</div>
        <div class="subtitle">Wir berechnen die Tabelle gar nicht mehr!</div>
    </div>
    <div class="time-pill">Szene 3 • 0:50 - 1:15</div>
</div>
<div class="content">
    <div class="card" style="text-align:center; display:flex; flex-direction:column; justify-content:center; align-items:center; gap:25px;">
        <div style="font-size:80px;">🤖💥</div>
        <div style="font-size:32px; font-weight:800; color:#f43f5e;">
            „Bitte tu mir das nicht an!“
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6; max-width:550px;">
            Die Bayes-Box wird bei realistischen Modellen schlicht zu lang. Wir können die Verteilung nicht mehr analytisch auflisten.
        </p>
    </div>
    <div class="card" style="background:linear-gradient(135deg, rgba(30,41,59,0.8), rgba(56,189,248,0.15)); border-color:rgba(56,189,248,0.4); display:flex; flex-direction:column; gap:25px;">
        <h3 style="font-size:32px; color:#38bdf8;">Der geniale Ausweg:</h3>
        <div style="font-size:40px; font-weight:800; color:#ffffff; line-height:1.3;">
            Stichproben statt Ausrechnen!
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            Wir verabschieden uns davon, das gesamte Gitter zu berechnen. 
            Stattdessen lassen wir uns <span class="highlight">Ziehungen (Stichproben)</span> aus der Verteilung liefern!
        </p>
        <div style="background:#0f172a; padding:20px; border-radius:16px; border:1px solid #38bdf8; display:flex; align-items:center; gap:15px;">
            <div style="font-size:36px;">💡</div>
            <div style="font-size:20px; color:#38bdf8; font-weight:600;">
                Häufigkeiten lassen sich kinderleicht zählen – ganz ohne Integralrechnung!
            </div>
        </div>
    </div>
</div>
</body></html>
"""

# Slide 4
s4 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">Das Prinzip • Stichproben-Logik</div>
        <div class="title">Häufigkeiten statt Wahrscheinlichkeiten</div>
        <div class="subtitle">Die Lostrommel der Parameterwerte</div>
    </div>
    <div class="time-pill">Szene 4 • 1:15 - 1:45</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; align-items:center; justify-content:center; gap:20px;">
        <h3 style="font-size:26px; color:#38bdf8; margin-bottom:10px;">Die Bayes-Lostrommel</h3>
        <svg width="500" height="340" viewBox="0 0 500 340">
            <!-- Drum Outline -->
            <ellipse cx="250" cy="180" rx="190" ry="120" fill="none" stroke="#38bdf8" stroke-width="4" stroke-dasharray="8 6"/>
            <path d="M 120 180 L 120 300 M 380 180 L 380 300" stroke="#64748b" stroke-width="6"/>
            <!-- Frequent high-probability balls (around 0.67) -->
            <circle cx="240" cy="160" r="32" fill="#38bdf8" opacity="0.9"/>
            <text x="240" y="167" fill="#0f172a" font-size="20" font-weight="800" text-anchor="middle">0.67</text>
            <circle cx="310" cy="180" r="30" fill="#38bdf8" opacity="0.9"/>
            <text x="310" y="187" fill="#0f172a" font-size="19" font-weight="800" text-anchor="middle">0.65</text>
            <circle cx="180" cy="190" r="28" fill="#38bdf8" opacity="0.9"/>
            <text x="180" y="196" fill="#0f172a" font-size="18" font-weight="800" text-anchor="middle">0.70</text>
            <!-- Rare low-probability balls -->
            <circle cx="160" cy="130" r="16" fill="#64748b" opacity="0.5"/>
            <text x="160" y="135" fill="#fff" font-size="12" text-anchor="middle">0.2</text>
            <circle cx="330" cy="130" r="14" fill="#64748b" opacity="0.5"/>
            <text x="330" y="135" fill="#fff" font-size="12" text-anchor="middle">0.9</text>
        </svg>
        <div style="font-size:20px; color:#cbd5e1; text-align:center;">
            Plausible Werte (z. B. 0.67) liegen tausendfach in der Trommel!
        </div>
    </div>
    <div class="card" style="display:flex; flex-direction:column; gap:25px;">
        <h3 style="font-size:28px; color:#38bdf8;">Die Ziehungsregel</h3>
        <div style="background:#0f172a; padding:25px; border-radius:18px; border:1px solid #334155; font-size:24px; color:#38bdf8; text-align:center; font-family:'Fira Code';">
            P(Zieh p) &prop; Posteriori-Plausibilität
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            • Je höher die Wahrscheinlichkeit eines Werts, desto <span class="highlight">öfter</span> ziehen wir ihn.<br><br>
            • Werte um 67 % ziehen wir ständig.<br><br>
            • Unplausible Werte (< 20 %) ziehen wir fast nie.
        </p>
    </div>
</div>
</body></html>
"""

# Slide 5
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
            <line x1="50" y1="220" x2="380" y2="220" stroke="#64748b" stroke-width="2"/>
            <path d="M 50 218 Q 150 215 220 170 T 290 60 T 340 170 T 380 218" fill="none" stroke="#38bdf8" stroke-width="4"/>
            <text x="290" y="40" fill="#38bdf8" font-size="16" font-weight="700" text-anchor="middle">Exakte Kurve</text>
        </svg>
        <p style="color:#94a3b8; font-size:18px;">Aufwendig zu berechnen</p>
    </div>
    <div class="card" style="text-align:center; border-color:rgba(52,211,153,0.4);">
        <h4 style="font-size:22px; color:#34d399; margin-bottom:15px;">2. 10.000 Stichproben (Histogramm)</h4>
        <svg width="420" height="260" viewBox="0 0 420 260">
            <line x1="50" y1="220" x2="380" y2="220" stroke="#64748b" stroke-width="2"/>
            <!-- Histogram bars -->
            <rect x="130" y="200" width="22" height="20" fill="#34d399" opacity="0.6"/>
            <rect x="155" y="180" width="22" height="40" fill="#34d399" opacity="0.7"/>
            <rect x="180" y="150" width="22" height="70" fill="#34d399" opacity="0.8"/>
            <rect x="205" y="120" width="22" height="100" fill="#34d399" opacity="0.85"/>
            <rect x="230" y="90" width="22" height="130" fill="#34d399" opacity="0.9"/>
            <rect x="255" y="70" width="22" height="150" fill="#34d399"/>
            <rect x="280" y="60" width="22" height="160" fill="#34d399"/>
            <rect x="305" y="85" width="22" height="135" fill="#34d399" opacity="0.9"/>
            <rect x="330" y="130" width="22" height="90" fill="#34d399" opacity="0.8"/>
            <rect x="355" y="180" width="22" height="40" fill="#34d399" opacity="0.6"/>
            <text x="290" y="40" fill="#34d399" font-size="16" font-weight="700" text-anchor="middle">Empirisches Histogramm</text>
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

# Slide 6
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
            <!-- Mountain shape -->
            <path d="M 50 340 Q 180 340 260 220 T 360 80 T 450 250 T 520 340 Z" fill="rgba(56,189,248,0.2)" stroke="#38bdf8" stroke-width="4"/>
            <!-- Fog layer -->
            <rect x="30" y="170" width="490" height="170" fill="url(#fogGradient)" opacity="0.85"/>
            <defs>
                <linearGradient id="fogGradient" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="0%" stop-color="#334155" stop-opacity="0.4"/>
                    <stop offset="60%" stop-color="#1e293b" stop-opacity="0.95"/>
                    <stop offset="100%" stop-color="#0f172a" stop-opacity="1"/>
                </linearGradient>
            </defs>
            <text x="360" y="60" fill="#38bdf8" font-size="22" font-weight="700" text-anchor="middle">Gipfel (Hohe Plausibilität)</text>
            <!-- Stan the Robot figure -->
            <circle cx="280" cy="210" r="14" fill="#fbbf24"/>
            <rect x="272" y="224" width="16" height="24" rx="4" fill="#fbbf24"/>
            <text x="315" y="230" fill="#fbbf24" font-size="20" font-weight="800">Stan 🤖</text>
            <text x="280" y="310" fill="#94a3b8" font-size="22" font-weight="600" text-anchor="middle">☁️ Dichter Nebel (Keine Landkarte!) ☁️</text>
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

# Slide 7
s7 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">MCMC-Intuition • Teil 2</div>
        <div class="title">Stans zwei einfache Schritt-Regeln</div>
        <div class="subtitle">Nur lokale Information – keine Vogelperspektive nötig</div>
    </div>
    <div class="time-pill">Szene 7 • 3:10 - 3:55</div>
</div>
<div class="content">
    <div class="card" style="border-color:rgba(52,211,153,0.5); background:rgba(52,211,153,0.08); display:flex; flex-direction:column; gap:20px;">
        <div style="display:flex; align-items:center; gap:15px;">
            <div style="background:#34d399; color:#0f172a; width:50px; height:50px; border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:28px; font-weight:800;">1</div>
            <h3 style="font-size:30px; color:#34d399;">Schritt geht BERGAUF?</h3>
        </div>
        <div style="font-size:42px; font-weight:800; color:#ffffff; margin:15px 0;">
            👉 IMMER GEHEN!
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            Der neue Punkt hat eine höhere Plausibilität als der aktuelle. Stan macht den Schritt <b>zu 100%</b>.
        </p>
    </div>
    <div class="card" style="border-color:rgba(251,191,36,0.5); background:rgba(251,191,36,0.08); display:flex; flex-direction:column; gap:20px;">
        <div style="display:flex; align-items:center; gap:15px;">
            <div style="background:#fbbf24; color:#0f172a; width:50px; height:50px; border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:28px; font-weight:800;">2</div>
            <h3 style="font-size:30px; color:#fbbf24;">Schritt geht BERGAB?</h3>
        </div>
        <div style="font-size:42px; font-weight:800; color:#ffffff; margin:15px 0;">
            🎲 MÜNZWURF!
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6;">
            Stan geht <b>nur manchmal</b>! Je tiefer das Tal, desto unwahrscheinlicher die Annahme (proportional zur Höhe).
        </p>
    </div>
</div>
<div style="margin-top:25px; text-align:center; font-size:22px; color:#94a3b8;">
    Stan weiß nur, was unter seinen Füßen liegt. Und doch entsteht Magie über die Zeit ...
</div>
</body></html>
"""

# Slide 8
s8 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge accent-emerald" style="border-color:rgba(52,211,153,0.4); background:rgba(52,211,153,0.15); color:#34d399;">
            Das Ergebnis • Rekonstruktion
        </div>
        <div class="title">Die Kette der Schritte: Das Histogramm</div>
        <div class="subtitle">Aus Stans Fußspuren wird die Posteriori-Verteilung</div>
    </div>
    <div class="time-pill">Szene 8 • 3:55 - 4:45</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; gap:20px;">
        <h4 style="font-size:22px; color:#38bdf8;">1. Stans Wanderpfad (Markov-Kette)</h4>
        <svg width="550" height="200" viewBox="0 0 550 200">
            <line x1="40" y1="180" x2="520" y2="180" stroke="#64748b" stroke-width="2"/>
            <line x1="40" y1="180" x2="40" y2="20" stroke="#64748b" stroke-width="2"/>
            <text x="30" y="30" fill="#94a3b8" font-size="14" text-anchor="end">p=1.0</text>
            <text x="30" y="80" fill="#38bdf8" font-size="14" font-weight="700" text-anchor="end">p=0.67</text>
            <text x="30" y="180" fill="#94a3b8" font-size="14" text-anchor="end">p=0.0</text>
            <!-- Trace line bouncing around 0.67 -->
            <path d="M 40 140 L 70 110 L 100 85 L 130 90 L 160 75 L 190 82 L 220 70 L 250 85 L 280 65 L 310 80 L 340 75 L 370 95 L 400 80 L 430 70 L 460 78 L 490 82 L 520 75" fill="none" stroke="#38bdf8" stroke-width="2.5"/>
            <circle cx="520" cy="75" r="6" fill="#fbbf24"/>
        </svg>
        <p style="font-size:18px; color:#94a3b8;">Stan pendelt dynamisch um die Region höchster Dichte.</p>
    </div>
    <div class="card" style="display:flex; flex-direction:column; gap:20px;">
        <h4 style="font-size:22px; color:#34d399;">2. Histogramm all seiner Standorte</h4>
        <div style="background:#0f172a; padding:25px; border-radius:18px; border:1px solid #34d399; text-align:center;">
            <div style="font-size:26px; font-weight:700; color:#34d399; margin-bottom:10px;">
                Stans Notizbuch = Unsere Stichproben!
            </div>
            <p style="font-size:20px; color:#cbd5e1; line-height:1.5;">
                Stan verbringt automatisch genau so viel Zeit an jedem Ort, wie es der Posteriori-Wahrscheinlichkeit entspricht.
            </p>
        </div>
        <div class="stat-box" style="border-color:rgba(52,211,153,0.4);">
            <div class="stat-label">Gesammelte Stichproben:</div>
            <div class="stat-num" style="color:#34d399;">10.000 Samples</div>
        </div>
    </div>
</div>
</body></html>
"""

# Slide 9
s9 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">Zusammenfassung • Merksätze</div>
        <div class="title">Drei goldene Takeaways</div>
        <div class="subtitle">Was ihr für euer Psychologiestudium mitnehmen sollt</div>
    </div>
    <div class="time-pill">Szene 9 • 4:45 - 5:25</div>
</div>
<div class="content" style="grid-template-columns: 1fr 1fr 1fr; gap:30px;">
    <div class="card" style="display:flex; flex-direction:column; gap:20px; border-color:rgba(244,63,94,0.4);">
        <div style="font-size:44px;">❌📦</div>
        <h3 style="font-size:26px; color:#f43f5e;">1. Bayes-Box hat Grenzen</h3>
        <p style="font-size:20px; color:#cbd5e1; line-height:1.6;">
            Bei mehr als einem Parameter explodiert die Zeilenzahl exponentiell. Die Gittermethode wird unbrauchbar.
        </p>
    </div>
    <div class="card" style="display:flex; flex-direction:column; gap:20px; border-color:rgba(251,191,36,0.4);">
        <div style="font-size:44px;">🧗♂️🌫️</div>
        <h3 style="font-size:26px; color:#fbbf24;">2. MCMC tastet ab</h3>
        <p style="font-size:20px; color:#cbd5e1; line-height:1.6;">
            Stan erkundet als Bergwanderer das Gelände im Nebel mit 2 simplen Regeln – ohne jemals eine Tabelle zu brauchen.
        </p>
    </div>
    <div class="card" style="display:flex; flex-direction:column; gap:20px; border-color:rgba(52,211,153,0.4);">
        <div style="font-size:44px;">📊🎯</div>
        <h3 style="font-size:26px; color:#34d399;">3. Stichproben genügen</h3>
        <p style="font-size:20px; color:#cbd5e1; line-height:1.6;">
            Aus Stans Schritten berechnen wir Mittelwerte, HDIs und Wahrscheinlichkeiten durch einfaches Auszählen.
        </p>
    </div>
</div>
</body></html>
"""

# Slide 10
s10 = f"""<!DOCTYPE html><html><head><style>{css}</style></head><body>
<div class="header">
    <div>
        <div class="badge">Ausblick auf die Praxis • Kapitel 8</div>
        <div class="title">Stan arbeitet für euch in R!</div>
        <div class="subtitle">Bayes-Modelle berechnen mit rstanarm</div>
    </div>
    <div class="time-pill">Szene 10 • 5:25 - 6:00</div>
</div>
<div class="content">
    <div class="card" style="display:flex; flex-direction:column; gap:20px;">
        <h3 style="font-size:26px; color:#38bdf8;">Der Code im R-Praktikum</h3>
        <div style="background:#020617; border:1px solid #334155; border-radius:16px; padding:25px; font-family:'Fira Code'; font-size:21px; line-height:1.8; color:#f8fafc;">
            <span style="color:#64748b;"># Paket laden</span><br>
            <span style="color:#38bdf8;">library</span>(rstanarm)<br><br>
            <span style="color:#64748b;"># Stan schätzen lassen</span><br>
            modell &lt;- <span style="color:#fbbf24;">stan_glm</span>(<br>
            &nbsp;&nbsp;therapie_erfolg ~ sitzungen,<br>
            &nbsp;&nbsp;data = daten<br>
            )
        </div>
        <p style="font-size:20px; color:#94a3b8;">
            Stan wandert automatisch für 4 Ketten mit je 2.000 Schritten!
        </p>
    </div>
    <div class="card" style="display:flex; flex-direction:column; align-items:center; justify-content:center; text-align:center; gap:25px;">
        <div style="font-size:80px;">🎓🤖🎉</div>
        <div style="font-size:32px; font-weight:800; color:#34d399;">
            Ihr müsst MCMC nicht von Hand rechnen!
        </div>
        <p style="font-size:22px; color:#cbd5e1; line-height:1.6; max-width:550px;">
            Ihr müsst nur wissen, wie ihr Stans Notizbuch (die Stichproben) richtig zusammenfasst.
        </p>
        <div style="margin-top:10px; font-size:20px; color:#38bdf8; font-weight:600;">
            👉 Weiterlesen: sebastiansauer.github.io/start-bayes
        </div>
    </div>
</div>
</body></html>
"""

slides = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

for idx, html in enumerate(slides, 1):
    html_file = f"/tmp/bayes_video_build/slide_{idx:02d}.html"
    png_file = f"/tmp/bayes_video_build/slide_{idx:02d}.png"
    with open(html_file, "w") as f:
        f.write(html)
    print(f"Rendering slide {idx} to PNG...")
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
    print(f"Slide {idx} rendered!")

print("All 10 slides successfully rendered to PNG!")
