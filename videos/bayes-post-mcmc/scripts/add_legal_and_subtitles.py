import subprocess
import os
import re

output_dir = "/tmp/bayes_video_build"
final_video = "/home/sebastian-sauer/Dokumente/github/04_start-bayes/bayes_erklaervideo_stichproben_mcmc.mp4"
artifact_dir = "/home/sebastian-sauer/.gemini/antigravity/brain/6bf78951-c6a2-4744-b602-0f464d3ca240"
ffmpeg = "/home/sebastian-sauer/.local/bin/ffmpeg"

# -------------------------------------------------------------
# 1. Update Slide 10 with CC-BY 4.0, Impressum & AI Disclaimer
# -------------------------------------------------------------
slide_10_file = f"{output_dir}/slide_10.html"
with open(slide_10_file) as f:
    c = f.read()

# Replace any existing footer or body ending
c = re.sub(r'<div style="margin-top:25px;.*?</div>\s*</body>', '</body>', c, flags=re.DOTALL)

legal_footer = """
<div style="margin-top:20px; padding-top:14px; border-top:1px solid rgba(148,163,184,0.2); display:flex; justify-content:space-between; align-items:center; font-size:15px; color:#64748b;">
    <div>
        📚 <b>start-bayes</b> • Prof. Dr. Sebastian Sauer • Lizenz: <span style="color:#38bdf8; font-weight:700;">CC-BY 4.0</span> (Open Access / Permissiv)
    </div>
    <div style="display:flex; align-items:center; gap:16px;">
        <span>🤖 <i>Stimme KI-synthetisiert (EU AI Act)</i></span>
        <span>•</span>
        <span>Impressum: <b style="color:#cbd5e1;">sebastiansauer.github.io/start-bayes/imprint.html</b></span>
    </div>
</div>
"""

c = c.replace('</body></html>', legal_footer + '</body></html>')
with open(slide_10_file, 'w') as f:
    f.write(c)

print("Slide 10 updated with complete legal footer!")
cmd_chrome = [
    "google-chrome",
    "--headless",
    "--disable-gpu",
    "--hide-scrollbars",
    "--window-size=1920,1080",
    f"--screenshot={output_dir}/slide_10.png",
    slide_10_file
]
subprocess.run(cmd_chrome, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
print("Slide 10 re-rendered to PNG!")

# Re-encode clip 10
cmd_clip = [
    ffmpeg, "-y",
    "-loop", "1",
    "-i", f"{output_dir}/slide_10.png",
    "-i", f"{output_dir}/audio_10.mp3",
    "-af", "apad=pad_dur=0.6",
    "-c:v", "libx264",
    "-tune", "stillimage",
    "-c:a", "aac",
    "-b:a", "192k",
    "-pix_fmt", "yuv420p",
    "-shortest",
    f"{output_dir}/clip_10.mp4"
]
subprocess.run(cmd_clip, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

# Re-concatenate master video
cmd_concat = [
    ffmpeg, "-y",
    "-f", "concat",
    "-safe", "0",
    "-i", f"{output_dir}/concat_list.txt",
    "-c", "copy",
    final_video
]
subprocess.run(cmd_concat, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run(["cp", final_video, f"{artifact_dir}/bayes_erklaervideo_stichproben_mcmc.mp4"], check=True)
print("Video successfully updated with legal notice!")

# -------------------------------------------------------------
# 2. Generate Professional Subtitles (SRT & VTT)
# -------------------------------------------------------------
# Durations of scenes (with 0.6s pad)
durations = [23.04, 27.94, 20.98, 25.85, 34.82, 30.98, 31.34, 32.33, 30.07, 21.43]

# Subtitle sentences per scene
scenes_text = [
    # Scene 1
    [
        "Erinnert ihr euch an unseren Globusversuch? Neun Würfe, sechsmal Wasser.",
        "Mit der Bayes-Box konnten wir die Posteriori-Verteilung direkt Zeile für Zeile berechnen:",
        "Gitter anlegen, Likelihood mal Prior, standardisieren – fertig!",
        "Für einfache Modelle mit nur einem einzigen Parameter wie dem Wasseranteil p...",
        "...ist diese Gittermethode absolut genial, anschaulich und präzise."
    ],
    # Scene 2
    [
        "Aber was passiert, wenn wir psychologische Phänomene untersuchen –",
        "...zum Beispiel in einer Regression mit Prädiktoren?",
        "Plötzlich schätzen wir nicht mehr einen, sondern drei, fünf oder zehn Parameter gleichzeitig!",
        "Wenn wir für jeden Parameter nur 100 Gitterwerte annehmen...",
        "...braucht unsere Bayes-Box bei 3 Parametern schon eine Million Zeilen.",
        "Bei einer multiplen Regression mit 10 Parametern sind es 10 hoch 20 Zeilen.",
        "Selbst ein Supercomputer rechnet daran bis zum jüngsten Tag."
    ],
    # Scene 3
    [
        "Kurz gesagt: Die Bayes-Box funktioniert für echte psychologische Modelle schlicht nicht mehr.",
        "Die Tabelle wird viel zu lang und die Mathematik zu rechenintensiv.",
        "Wir stecken in einer scheinbaren Sackgasse.",
        "Doch zum Glück gibt es einen genialen Ausweg:",
        "Wir verabschieden uns davon, die Kurve exakt ausrechnen zu wollen –",
        "...und lassen uns stattdessen Stichproben liefern."
    ],
    # Scene 4
    [
        "Hier kommt der zentrale Trick moderner Bayes-Statistik:",
        "Wir arbeiten ab jetzt nicht mehr mit abstrakten Wahrscheinlichkeitsformeln...",
        "...sondern mit handfesten Häufigkeiten.",
        "Stellt euch vor, wir haben eine Lostrommel. Darin liegen mögliche Parameterwerte.",
        "Und je plausibler ein Wert nach den Daten ist, desto häufiger liegt er in der Trommel.",
        "Werte um 67 Prozent Wasser ziehen wir ständig; Werte unter 20 Prozent so gut wie nie."
    ],
    # Scene 5
    [
        "Schaut euch diesen Vergleich an:",
        "Links die mühsam berechnete, exakte Kurve der Bayes-Box.",
        "Rechts ein einfaches Histogramm aus 10.000 gezogenen Stichproben.",
        "Seht ihr den Unterschied? Es gibt praktisch keinen!",
        "Mit Stichproben können wir alle psychologischen Fragen kinderleicht beantworten:",
        "Wie wahrscheinlich ist ein Wasseranteil über 70 Prozent?",
        "Wir zählen einfach, wie viele unserer 10.000 Zettel über 0.7 liegen.",
        "Kein Integrieren, kein Formelchaos – reines Zählen.",
        "Aber: Wie kommen wir an diese Stichproben, wenn wir gar keine Tabelle haben?"
    ],
    # Scene 6
    [
        "Genau hier betritt die Geheimwaffe der Bayes-Statistik die Bühne:",
        "MCMC – kurz für Monte-Carlo-Markov-Ketten. Klingt furchteinflößend, ist aber ganz einfach.",
        "Stellt euch die gesuchte Posteriori-Verteilung wie eine Gebirgslandschaft vor:",
        "Wo die Plausibilität hoch ist, ragen mächtige Berge auf; unwahrscheinliche Werte sind flache Täler.",
        "Das Problem: Die Landschaft liegt im stockdichten Nebel.",
        "Wir haben keine Landkarte und können das Gebirge nicht von oben vermessen."
    ],
    # Scene 7
    [
        "Deshalb schicken wir einen blinden Kundschafter ins Gebirge: Nennen wir ihn Stan.",
        "Stan sieht nichts im Nebel, aber er kann an seiner aktuellen Position den Boden abtasten.",
        "Er schlägt zufällig eine Richtung für den nächsten Schritt vor und folgt zwei simplen Regeln:",
        "Geht der Schritt bergauf in Richtung Gipfel? Dann macht er ihn sofort und immer!",
        "Geht der vorgeschlagene Schritt bergab in ein Tal? Dann zögert er:",
        "Er wirft eine Münze und geht nur mit einer gewissen Wahrscheinlichkeit – je tiefer, desto seltener."
    ],
    # Scene 8
    [
        "Was passiert durch diese simplen Regeln über Tausende von Schritten?",
        "Stan wird nicht für immer auf dem Gipfel festgenagelt...",
        "...aber er hält sich ganz automatisch viel häufiger in den Höhenlagen auf als im Tal.",
        "An jedem Schritt notiert Stan seinen aktuellen Standort.",
        "Und das Geniale ist: Wenn wir nach 10.000 Schritten Stans Notizbuch aufschlagen...",
        "...und ein Histogramm all seiner Standorte zeichnen, erhalten wir exakt die Posteriori-Verteilung!",
        "Stan hat das Gebirge abgetastet – ganz ohne gigantische Bayes-Box."
    ],
    # Scene 9
    [
        "Fassen wir die drei wichtigsten Punkte zusammen:",
        "Erstens: Die Bayes-Box ist super zum Verstehen, aber bei psychologischen Modellen explodiert die Zeilenzahl.",
        "Zweitens: MCMC löst dieses Problem elegant.",
        "Statt das gesamte Gelände analytisch zu berechnen, schicken wir Stan als Bergwanderer los.",
        "Und drittens: Wir brauchen keine komplizierten Formeln.",
        "Wir fassen einfach Stans Schritte zusammen, um Schätzbereiche, Mittelwerte und Intervalle zu bestimmen."
    ],
    # Scene 10
    [
        "In den nächsten Kapiteln lernt ihr, wie ihr Stan in R selbst an die Arbeit schickt –",
        "...mit dem Paket rstanarm.",
        "Ihr gebt einfach euer Modell ein, drückt Enter, und Stan wandert für euch los.",
        "Ihr müsst MCMC also nicht selbst programmieren –",
        "...ihr müsst nur wissen, wie ihr Stans Notizbuch richtig lest. Viel Erfolg beim Ausprobieren!"
    ]
]

def format_timestamp(seconds, vtt=False):
    hrs = int(seconds // 3600)
    mins = int((seconds % 3600) // 60)
    secs = int(seconds % 60)
    msecs = int(round((seconds - int(seconds)) * 1000))
    if msecs >= 1000:
        msecs = 999
    if vtt:
        return f"{hrs:02d}:{mins:02d}:{secs:02d}.{msecs:03d}"
    else:
        return f"{hrs:02d}:{mins:02d}:{secs:02d},{msecs:03d}"

srt_entries = []
vtt_entries = ["WEBVTT\n"]
entry_idx = 1
current_time = 0.0

for s_idx, lines in enumerate(scenes_text):
    scene_dur = durations[s_idx]
    total_chars = sum(len(line) for line in lines)
    scene_start = current_time
    
    sub_current = scene_start
    for l_idx, line in enumerate(lines):
        # Time proportional to character length with safety margins
        line_weight = len(line) / total_chars
        line_dur = scene_dur * line_weight
        line_start = sub_current
        line_end = sub_current + line_dur
        # Keep small margin before next line
        line_display_end = min(line_end - 0.05, scene_start + scene_dur)
        
        # SRT
        srt_entries.append(f"{entry_idx}\n{format_timestamp(line_start, False)} --> {format_timestamp(line_display_end, False)}\n{line}\n")
        # VTT
        vtt_entries.append(f"{format_timestamp(line_start, True)} --> {format_timestamp(line_display_end, True)}\n{line}\n")
        
        entry_idx += 1
        sub_current = line_end
    
    current_time += scene_dur

# Write SRT and VTT to project dir and artifact dir
srt_content = "\n".join(srt_entries)
vtt_content = "\n".join(vtt_entries)

files_to_write = [
    ("/home/sebastian-sauer/Dokumente/github/04_start-bayes/bayes_erklaervideo_stichproben_mcmc.srt", srt_content),
    ("/home/sebastian-sauer/Dokumente/github/04_start-bayes/bayes_erklaervideo_stichproben_mcmc.vtt", vtt_content),
    (f"{artifact_dir}/bayes_erklaervideo_stichproben_mcmc.srt", srt_content),
    (f"{artifact_dir}/bayes_erklaervideo_stichproben_mcmc.vtt", vtt_content),
]

for path, content in files_to_write:
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)
    print(f"Written {path}")

# Update video_player.html to include subtitle track
player_file = f"{artifact_dir}/video_player.html"
with open(player_file) as f:
    p_content = f.read()

track_tag = '<track kind="subtitles" src="bayes_erklaervideo_stichproben_mcmc.vtt" srclang="de" label="Deutsch (Untertitel)" default>'
if '<track' not in p_content:
    p_content = p_content.replace('</video>', f'  {track_tag}\n                    </video>')
    with open(player_file, 'w') as f:
        f.write(p_content)
    print("Updated video_player.html with subtitle track!")

print("All legal updates and subtitle files generated successfully!")
