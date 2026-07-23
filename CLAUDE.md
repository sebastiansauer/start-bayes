# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

"Start:Bayes!" is a German-language Quarto book (an introductory statistics/Bayesian-inference textbook) rendered to HTML in `docs/`. Content lives in numbered `.qmd` chapter files at the repo root (e.g. `0200-zielarten.qmd`, `0250-inferenz.qmd`, ... `1200-abschluss.qmd`); the numeric prefix is usually but *not always* the reading order — `book.chapters` in `_quarto.yml` is the actual source of truth (e.g. `1050-Schaetzen-Testen.qmd` is taught before `1000-metrische-AV.qmd`). Always check `_quarto.yml` rather than assuming order from the filename.

## Commands

- Render the whole book: `quarto render` (equivalent to `render.R`, which calls `quarto::quarto_render(execute = TRUE, cache = TRUE)`)
- Render a single chapter: `quarto render 0250-inferenz.qmd`
- Regenerate the definitions list (`definitions.qmd`) from `:::{#def-...}` shortcodes across all `NNNN-*.qmd` files: `Rscript get-defs.R`
- Regenerate the theorems list (`theorems.qmd`) from `:::{#thm-...}` shortcodes: `Rscript get-thms.R`
- R dependencies are managed with `renv` (see `renv.lock`); `.Rprofile` sources `_common.R` on session start, which sets global knitr/ggplot options, custom color constants, and the plotting theme used across chapters.

There is no linter or test suite; "correctness" here means the book renders without errors and content/citations resolve.

## Architecture / structure

- `_quarto.yml` is the book config: defines part/chapter order (`book.chapters`), output format (HTML, `theme: lumen`/`darkly`), bibliography (`specifics/bib-local.bib`, `specifics/apa7.csl`), and freeze/cache execution settings. **Whenever a chapter `.qmd` file is added, renamed, or reordered, `_quarto.yml`'s `book.chapters` list must be updated to match**, or the book won't include/order it correctly.
- Chapter files follow a `NNNN-title.qmd` naming/numbering scheme; each generates a matching `NNNN-title_cache/` and `NNNN-title_files/` directory (Quarto/knitr build artifacts — don't hand-edit).
- `children/` holds `.qmd`/`.R` fragments included into chapters via `{{< include children/foo.qmd >}}` (shared plots, sub-sections, DAG figures, etc.), rather than duplicated inline.
- `funs/` holds standalone R helper scripts (DAG plotting, distribution plots, binomial likelihood, etc.) sourced by chapters as needed.
- `exr/` contains one subdirectory per quiz question, named `<slug>-schoice/`, each holding a single `.Rmd` in R/exams format (`Question` / `Answerlist` / `Solution` / `Meta-information` blocks with `extype: schoice`). These are referenced by path from a chapter's "Quiz-Aufgaben" section via `exams2forms::exams2forms()` inside a knitr chunk (see the `quiz_wskt2_files` list + `exams2forms(...)` call near the end of `0350-wskt2.qmd` for the pattern). Use the `schoice-aus-text` skill to generate new questions in this format.
  - Caching gotcha: the knitr chunk that calls `exams2forms(...)` is cached based on its own code text, not on the contents of the referenced `.Rmd` question files or of the file-list variable set in a prior chunk. After adding/removing/reordering entries in a chapter's `quiz_*_files` list, delete that chapter's stale cache (`rm <chapter>_cache/html/quiz-*`) before re-rendering, or the book will silently keep showing the old question set.
- `specifics/` holds cross-cutting assets referenced from `_quarto.yml`/chapters: CSS/JS (`styles.css`, `webex.css`, `webex.js` for interactive self-check widgets), bibliography/citation style, LaTeX partials for PDF output (currently commented out in `_quarto.yml`).
- `bib/` is a git submodule (see `.gitmodules`) — the bibliography source lives in a separate repo.
- `get-defs.R` / `get-thms.R` scan all chapter `.qmd` files for `:::{#def-...}` / `:::{#thm-...}` shortcode blocks and regenerate `definitions.qmd` / `theorems.qmd` as auto-generated cross-reference indexes; re-run them after adding/changing a definition or theorem, don't hand-edit those two output files.
- `docs/` is the rendered book output (HTML) — generated, not source.
