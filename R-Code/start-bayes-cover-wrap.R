## Erstellt das vollständige KDP-Wrap-Cover (Rückseite + Buchrücken + Vorderseite)
## als PDF für den Druck bei Amazon KDP.
##
## KDP-Vorgabe (Fehlermeldung beim Upload): erwartete Covergröße
## 15.275in x 10.250in (inkl. 0.125in Beschnitt an den Außenkanten),
## bei Trimgröße 7in x 10in -> Buchrücken-Breite 1.025in.
##
## Auflösung: 400dpi, damit alle Maße exakte Pixelwerte ergeben:
##   Gesamt   15.275in x 10.250in -> 6110 x 4100 px
##   Beschnitt 0.125in            ->  50 px
##   Rücken    1.025in            -> 410 px
##   Trimgröße 7in  x 10in        -> 2800 x 4000 px

library(magick)
library(yaml)

out_dir   <- "img/start-bayes-cover"
front_src <- file.path(out_dir, "start-bayes-cover.001.png")
out_png   <- file.path(out_dir, "start-bayes-cover-wrap.png")
out_pdf   <- file.path(out_dir, "start-bayes-cover-wrap.pdf")

dpi     <- 400
bg_col  <- "#D5D5D5"
font_fam <- "Lato"

bleed_px  <- round(0.125 * dpi)   # 50
spine_px  <- round(1.025 * dpi)   # 410
trim_w_px <- 7L * dpi             # 2800
trim_h_px <- 10L * dpi            # 4000

panel_w_px <- trim_w_px + bleed_px      # 2850 (eine Außenkante Beschnitt)
total_h_px <- trim_h_px + 2L * bleed_px # 4100
total_w_px <- 2L * panel_w_px + spine_px # 6110

stopifnot(total_w_px == round(15.275 * dpi))
stopifnot(total_h_px == round(10.250 * dpi))

cfg <- read_yaml("_quarto.yml")
blurb  <- gsub("\\s+", " ", trimws(cfg$book$description))
title_txt    <- cfg$book$title
subtitle_txt <- cfg$book$subtitle
author_txt   <- cfg$book$author[[1]]$name

## ---- Gesamtcanvas ---------------------------------------------------------
canvas <- image_blank(total_w_px, total_h_px, color = bg_col)

## ---- Vorderseite (bereits fertiges Cover, auf 400dpi hochskaliert) --------
front_trim <- image_read(front_src) %>%
  image_resize(paste0(trim_w_px, "x", trim_h_px, "!"))

front_x <- panel_w_px + spine_px  # linke Kante der Vorderseite im Gesamtcanvas
canvas <- image_composite(canvas, front_trim,
                           offset = paste0("+", front_x, "+", bleed_px))

## ---- Buchrücken ------------------------------------------------------------
spine_margin <- round(0.15 * dpi)
spine_text_h <- total_h_px - 2L * spine_margin
spine_img <- image_blank(spine_text_h, spine_px, color = bg_col) %>%
  image_annotate(paste0(title_txt, "   ·   ", author_txt),
                  size = 78, font = font_fam, weight = 700,
                  gravity = "center", color = "black") %>%
  image_rotate(270)
spine_x <- panel_w_px
canvas <- image_composite(canvas, spine_img,
                           offset = paste0("+", spine_x, "+", spine_margin))

## ---- Rückseite -------------------------------------------------------------
back_safe_x0 <- bleed_px
back_safe_x1 <- panel_w_px
back_safe_w  <- back_safe_x1 - back_safe_x0
text_margin  <- round(0.5 * dpi)

## Titel/Untertitel oben
back_title_img <- image_blank(back_safe_w - 2L * text_margin, round(1.4 * dpi),
                               color = bg_col) %>%
  image_annotate(title_txt, size = 150, font = font_fam, weight = 700,
                  gravity = "north", color = "black") %>%
  image_annotate(subtitle_txt, size = 70, font = font_fam, weight = 400,
                  gravity = "south", color = "black")
canvas <- image_composite(canvas, back_title_img,
                           offset = paste0("+", back_safe_x0 + text_margin, "+",
                                            bleed_px + round(0.6 * dpi)))

## Klappentext (Beschreibung aus _quarto.yml)
## image_annotate bricht lange Strings NICHT automatisch um -- Text vorher
## per strwrap() in Zeilen zerlegen, die in die verfuegbare Breite passen.
blurb_h <- round(4.6 * dpi)
blurb_font_size <- 46
blurb_wrapped <- paste(strwrap(blurb, width = 58), collapse = "\n")
blurb_img <- image_blank(back_safe_w - 2L * text_margin, blurb_h, color = bg_col) %>%
  image_annotate(blurb_wrapped, size = blurb_font_size, font = font_fam, weight = 400,
                  gravity = "north", color = "black")
canvas <- image_composite(canvas, blurb_img,
                           offset = paste0("+", back_safe_x0 + text_margin, "+",
                                            round(2.6 * dpi)))

## Barcode-Freifläche (ISBN), unten rechts auf der Rückseite (Konvention),
## d.h. nahe am Buchrücken -- nur Platzhalter-Markierung, kein echter Barcode.
bc_w <- round(2.0 * dpi); bc_h <- round(1.2 * dpi)
bc_margin <- round(0.25 * dpi)
bc_x <- back_safe_x1 - bc_margin - bc_w
bc_y <- total_h_px - bleed_px - bc_margin - bc_h
barcode_img <- image_blank(bc_w, bc_h, color = "white") %>%
  image_border("gray70", "2x2") %>%
  image_annotate("ISBN-Barcode\n(Platzhalter)", size = 34, font = font_fam,
                  gravity = "center", color = "gray50")
canvas <- image_composite(canvas, barcode_img, offset = paste0("+", bc_x, "+", bc_y))

## ---- Schnittmarken/Sicherheitsbereich (nur zur Kontrolle, dünne Linien) ---
## bewusst weggelassen -- Cover geht so direkt an KDP.

image_write(canvas, path = out_png, format = "png", density = paste0(dpi, "x", dpi))
image_write(canvas, path = out_pdf, format = "pdf", density = paste0(dpi, "x", dpi))

cat("Wrap-Cover geschrieben nach:\n -", out_png, "\n -", out_pdf,
    sprintf("\nGröße: %.3fin x %.3fin (bei %d dpi)\n",
            total_w_px / dpi, total_h_px / dpi, dpi))
