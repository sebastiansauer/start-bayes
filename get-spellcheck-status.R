# Scannt alle Kapitel-Dateien nach dem Marker-Kommentar
#   <!-- spellcheck: YYYY-MM-DD -->
# (leer, solange noch nicht geprüft) und erzeugt eine Uebersichtstabelle
# spellcheck-status.qmd. Nach jeder Rechtschreibpruefung eines Kapitels
# das Datum im Marker-Kommentar des betreffenden Kapitels eintragen und
# dieses Skript erneut laufen lassen.

library(stringr)

quarto_dir <- "."

pattern_marker <- "<!--\\s*spellcheck:\\s*(\\d{4}-\\d{2}-\\d{2})?\\s*-->"

read_files <- function(path = ".") {
  files <- list.files(path, recursive = FALSE, full.names = FALSE,
                       pattern = "^(\\d{3,4}.+\\.qmd|index\\.qmd)$")
  sort(files)
}

files <- read_files(quarto_dir)

status <- lapply(files, function(file) {
  content <- readLines(file, warn = FALSE)
  match <- str_match(content, pattern_marker)
  hit <- which(!is.na(match[, 1]))[1]
  date <- if (!is.na(hit)) match[hit, 2] else NA
  data.frame(
    Datei = file,
    Datum = ifelse(is.na(date), "-", date),
    Status = ifelse(is.na(date), "noch nicht geprüft", "geprüft"),
    stringsAsFactors = FALSE
  )
})
status <- do.call(rbind, status)

output_file <- file.path(quarto_dir, "spellcheck-status.qmd")
fileConn <- file(output_file, "w")
writeLines("# Stand Rechtschreibprüfung\n", fileConn)
writeLines(
  "Diese Übersicht wird von `get-spellcheck-status.R` aus dem Marker-Kommentar `<!-- spellcheck: YYYY-MM-DD -->` am Kapitelanfang erzeugt. Nicht von Hand editieren.\n",
  fileConn
)
writeLines("| Kapitel | Zuletzt geprüft | Status |", fileConn)
writeLines("|---|---|---|", fileConn)
for (i in seq_len(nrow(status))) {
  writeLines(
    paste0("| ", status$Datei[i], " | ", status$Datum[i], " | ", status$Status[i], " |"),
    fileConn
  )
}
close(fileConn)

cat("Status von", nrow(status), "Kapiteln nach", output_file, "geschrieben;",
    sum(status$Status == "geprüft"), "geprüft,",
    sum(status$Status != "geprüft"), "offen.\n")
