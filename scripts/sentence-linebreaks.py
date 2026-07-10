#!/usr/bin/env python3
"""Reformat prose in .qmd files: one sentence per line, plus a line break
after every semicolon and colon (git-friendly diffs).

Usage:
    python3 scripts/sentence-linebreaks.py 0300-Wskt.qmd 0400-Verteilungen.qmd
    python3 scripts/sentence-linebreaks.py 0*.qmd   # shell-expanded glob

Skips fenced code chunks (```{r}...``` etc.), raw LaTeX \\begin{}...\\end{}
blocks, table rows, standalone $$...$$ math, headings (#...), and div fences
(:::...). Protects inline math ($...$), inline code (`...`), markdown links,
footnote references, known German abbreviations (s., S., bzw., z.B., d.h.,
u.a., vgl., ca., Kap., usw., etc., ...) and ordinal numbers ("der 1. Zug")
from being mistaken for sentence ends.
"""
import re
import sys

ABBR = [
    "bzw.", "z.B.", "d.h.", "u.a.", "vgl.", "ca.", "Kap.", "usw.", "etc.",
    "s.", "S.", "u.s.w.", "ggf.", "Nr.", "Prof.", "Dr.", "u.U.", "o.ä.", "z.T.",
]
ABBR.sort(key=len, reverse=True)

MATH_RE = re.compile(r"\$[^$\n]+\$")
CODE_RE = re.compile(r"`[^`\n]+`")
LINK_RE = re.compile(r"\[[^\]\n]*\]\([^)\n]*\)")
FOOTNOTE_REF_RE = re.compile(r"\[\^[^\]\n]+\]")

LEADING_MARKER_RE = re.compile(
    r"^(\s*(?:"
    r"\d+[.)]\s+|"          # 1. / 1)
    r"[A-Za-z][.)]\s+|"     # A) / a.
    r"[-*+]\s+|"            # - bullet
    r">\s*|"                # blockquote
    r"\[\^[^\]]+\]:\s*"     # footnote definition
    r"))"
)

ORDINAL_RE = re.compile(
    r'\b(der|die|das|des|dem|den|im|beim|zum|zur)(\s+)(\d{1,2})\.'
)

SPLIT_RE = re.compile(
    r'(?<=[.!?])[ \t]+(?=[A-ZÄÖÜ0-9„"(\[\x02])'  # sentence end -> next sentence
    r'|(?<=[;:])[ \t]+'                           # semicolon/colon -> new line regardless of case
)


def protect_spans(text, placeholders):
    def repl(regex, tag):
        nonlocal text

        def sub(m):
            key = f"\x02{tag}{len(placeholders)}\x03"
            placeholders[key] = m.group(0)
            return key

        text = regex.sub(sub, text)

    repl(LINK_RE, "L")
    repl(FOOTNOTE_REF_RE, "F")
    repl(MATH_RE, "M")
    repl(CODE_RE, "C")
    return text


def protect_abbrev(text):
    for a in ABBR:
        text = re.sub(re.escape(a), a.replace(".", "\x01"), text)
    text = ORDINAL_RE.sub(lambda m: f"{m.group(1)}{m.group(2)}{m.group(3)}\x01", text)
    return text


def restore_abbrev(text):
    return text.replace("\x01", ".")


def restore_spans(text, placeholders):
    for key, val in placeholders.items():
        text = text.replace(key, val)
    return text


def process_line(line):
    nl = ""
    if line.endswith("\n"):
        nl = "\n"
        line = line[:-1]

    m = LEADING_MARKER_RE.match(line)
    prefix = m.group(1) if m else ""
    is_blockquote = bool(prefix.strip().startswith(">"))
    rest = line[len(prefix):]

    if not rest.strip():
        return line + nl

    placeholders = {}
    protected = protect_spans(rest, placeholders)
    protected = protect_abbrev(protected)
    protected = protected.rstrip()  # avoid a spurious empty trailing line

    parts = SPLIT_RE.split(protected)
    if len(parts) <= 1:
        return line + nl

    out_lines = []
    for i, part in enumerate(parts):
        part = restore_abbrev(part)
        part = restore_spans(part, placeholders)
        if i == 0:
            out_lines.append(prefix + part)
        elif is_blockquote:
            out_lines.append("> " + part)
        else:
            out_lines.append(part)

    return "\n".join(out_lines) + nl


def process_file(path):
    with open(path, encoding="utf-8") as f:
        lines = f.readlines()

    in_fence = False
    in_latex_env = False
    out = []
    changed = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("```"):
            in_fence = not in_fence
            out.append(line)
            continue
        if in_fence:
            out.append(line)
            continue
        if re.match(r"^\\begin\{", stripped):
            in_latex_env = True
            out.append(line)
            continue
        if re.match(r"^\\end\{", stripped):
            in_latex_env = False
            out.append(line)
            continue
        if in_latex_env:
            out.append(line)
            continue
        if (
            stripped.startswith("|")
            or stripped.startswith("$$")
            or stripped.startswith(":::")
            or re.match(r"^#{1,6}\s", stripped)
            or re.match(r"^<!--.*-->$", stripped)  # self-contained HTML comment
            or stripped == ""
        ):
            out.append(line)
            continue
        new_line = process_line(line)
        if new_line != line:
            changed = True
        out.append(new_line)

    if changed:
        with open(path, "w", encoding="utf-8") as f:
            f.writelines(out)
    return changed, len(lines), len("".join(out).splitlines())


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    for path in sys.argv[1:]:
        changed, before, after = process_file(path)
        status = f"{before} -> {after} Zeilen" if changed else "keine Änderung"
        print(f"{path}: {status}")


if __name__ == "__main__":
    main()
