-- Entfernt klickbare Hyperlink-Elemente für externe URLs im PDF/LaTeX-Export.
--
-- Hintergrund: KDP (Kindle Direct Publishing) lehnt interaktive Elemente im
-- Druck-Interior ab -- ein \href{...}{...} bleibt auch mit "hidelinks"
-- (unsichtbarer Kasten, keine Farbe) ein klickbares PDF-Element. Da jeder
-- externe Prosa-Link laut textlayout-ueberarbeitung-Skill (Schritt 4)
-- ohnehin eine Fußnote mit der Klartext-URL direkt danach bekommt, ist der
-- Hyperlink selbst für den Druck redundant.
--
-- Nur externe http(s)-Links werden zu reinem Text; interne Anker (Kreuz-
-- referenzen, Inhaltsverzeichnis, Zitationen) bleiben unangetastet, da sie
-- für die Buch-interne Navigation im PDF gebraucht werden und von KDP nicht
-- moniert werden.
--
-- Nur für LaTeX/PDF aktiv (siehe Filter-Eintrag unter "titlepage-pdf:" in
-- _quarto.yml) -- HTML behält klickbare externe Links wie bisher.

function Link(el)
  if el.target:match("^https?://") then
    return el.content
  end
  return el
end
