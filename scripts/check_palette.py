#!/usr/bin/env python3
"""
check_palette.py
================================================================================
Guard for the ADA-PARC tier palette.

scripts/palettes.R is the single source of truth. Several consumers cannot call
R (static stylesheets and a browser script), so they mirror its values. This
script re-reads palettes.R and checks:

  1. the heritage palette's own accessibility invariants
  2. every selectable palette against the admission criteria, including that
     each recorded on_fill both clears AA and is the better of the two
     candidates
  3. the static mirrors, and the standard-contrast fallback :root in
     www/styles.css
  4. every declared foreground/background pair in the site chrome, at 4.5:1 in
     standard contrast and 7:1 in high contrast
  5. that no var() reference in the stylesheets is undefined and unfallbacked,
     which would render an element transparent rather than merely wrong
  6. that no retired tier hex survives anywhere in the source tree
  7. informationally, which generated artefacts still carry the old ramp

Run before deploy, and after any edit to scripts/palettes.R:

    python3 scripts/check_palette.py

Exit code 0 means every check passed. Non-zero means something drifted; the
output names the file and the expected value.

Dependencies: numpy, and docs/assets/palette_audit.py for the colour maths.
================================================================================
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "docs" / "assets"))

from palette_audit import (  # noqa: E402
    contrast_ratio, ciede2000, rgb_to_lab, hex_to_rgb, simulate_cvd,
)

# ------------------------------------------------------------------------------
# Thresholds. These mirror the ones in docs/assets/palette_audit.py and the
# floors declared in docs/color-palette-expansion.qmd.
# ------------------------------------------------------------------------------
AA_TEXT = 4.5          # WCAG 2.2 SC 1.4.3, normal-size text
AA_NONTEXT = 3.0       # WCAG 2.2 SC 1.4.11, graphical objects
FILL_DELTA_E = 15.0    # adjacent map/chart bin separation, all vision types
TABLE_TINT = "#f4efe9"  # summary table warm tint in state_scorecard.qmd
# Every surface an identity colour is drawn on, both contrast modes. The floor
# is measured against the DARKEST one each palette can actually reach, which is
# not the same for all five: pal_effective_fill_id() forces mono_high whenever
# high contrast is on, so mono_high is the only entry that meets the high
# contrast tint and the chromatic four never leave standard contrast.
#
# Two earlier versions of this check used doc_paper and then doc_cream. Both
# reported margins the design did not have. See the surface note in
# scripts/search_identity.py.
DOC_PAPER = "#ffffff"      # domain card, sub-index bar track
DOC_CREAM = "#faf7f3"      # domain score badge
DOC_TINT = "#f4efe9"       # summary table row and its 4px left rule
DOC_TINT_HIGH = "#e8e8e8"  # the same, in high contrast
VISION = ("normal", "protanopia", "deuteranopia", "tritanopia")

TIERS = ["poor", "below", "above", "excellent"]

# The three domains, in positional order light to dark. Mirrors
# ADAPARC_DOMAINS in scripts/palettes.R.
DOMAINS = ["cl", "cp", "we"]
DOMAIN_LABELS = {"cl": "Community Living", "cp": "Community Participation",
                 "we": "Work & Economic"}

# Identity is held HIGHER than the tier ramp, and deliberately. Adjacent tiers
# sit in a ranked sequence, so a reader who cannot separate two of them still
# has the ordering and the printed tier name. Three domains are peers with no
# ordering at all, so the colour is doing more work. See the ADMISSION CRITERIA
# note in scripts/palettes.R.
IDENTITY_DELTA_E = 20.0

# Hex values retired by the Heritage adoption. If any reappears in a source
# file, some surface has been edited back to the old ramp by hand.
#
# Note that #8a8078 (the na accent) is NOT retired. It carried over unchanged
# because it is a neutral, not a tier colour.
RETIRED = {
    "#f7e49c": "old map ramp, tier 1",
    "#e8c84a": "old scorecard ramp, tier 1",
    "#fa8e57": "old ramp, tier 2",
    "#c41306": "old ramp, tier 3",
    "#630801": "old ramp, tier 4",
    "#92530a": "old on-white ramp, poor",
    "#c75000": "old on-white ramp, below",
    "#9b1a0a": "old on-white ramp, above",
}

# Files searched for retired hexes. Rendered output, archives, backups and
# vendored libraries are excluded because they are regenerated or frozen.
SEARCH_GLOBS = ["*.Rmd", "*.qmd", "*.R", "*.css", "*.js", "*.yml"]
EXCLUDE_PARTS = {
    "archive", "old : testing site versions", "old", "renv", ".Rproj.user",
    "_preview", "backup", ".git", "rsconnect", "docs",
}
EXCLUDE_NAME_PREFIX = (".bak-",)

# These two files legitimately name the retired values: palettes.R documents
# what Heritage replaced and why, and this script lists them above. Exempting
# them keeps the history in the code rather than forcing it out.
SELF_EXEMPT = {"scripts/palettes.R", "scripts/check_palette.py"}


# ------------------------------------------------------------------------------
# Read the source of truth
# ------------------------------------------------------------------------------

def parse_palettes_r(path: Path) -> dict:
    """Pull the colour values out of scripts/palettes.R.

    Deliberately a regex read rather than an R evaluation, so this check can run
    anywhere Python runs, including a CI step with no R toolchain.
    """
    text = path.read_text(encoding="utf-8")

    def block(name: str) -> dict:
        # \b so that searching for "fill" cannot match inside "on_fill" or
        # "na_fill"; underscore counts as a word character in Python regex.
        m = re.search(r"\b" + name + r"\s*=\s*c\((.*?)\)", text, re.S)
        if not m:
            raise SystemExit(f"check_palette: cannot find '{name}' in {path}")
        pairs = re.findall(r'(\w+)\s*=\s*"(#[0-9a-fA-F]{6})"', m.group(1))
        got = dict(pairs)
        missing = [t for t in TIERS if t not in got]
        if missing:
            raise SystemExit(
                f"check_palette: '{name}' in {path} is missing tiers {missing}")
        return got

    def scalar(name: str) -> str:
        m = re.search(r"\b" + name + r'\s*=\s*"(#[0-9a-fA-F]{6})"', text)
        if not m:
            raise SystemExit(f"check_palette: cannot find '{name}' in {path}")
        return m.group(1)

    return {
        "fill": block("fill"),
        "on_fill": block("on_fill"),
        "on_white": block("on_white"),
        "na_fill": scalar("na_fill"),
        "na_text": scalar("na_text"),
        "na_accent": scalar("na_accent"),
        "stroke": scalar("stroke"),
    }


def parse_catalogue(path: Path) -> dict:
    """Pull the ADAPARC_PALETTES catalogue out of scripts/palettes.R.

    Returns {id: {"label": str, "fill": {tier: hex}, "on_fill": {tier: hex},
    "na": hex}} in the order declared in PALETTE_ORDER.

    Rewritten 2026-08-03 for phase 1 of docs/palette-v1-build-plan.qmd, which
    merged MAP_PALETTES and ADA_PARC_PALETTE into one list. Every entry now
    carries the full fill and on_fill roles as literal hex, so the special case
    that used to resolve heritage's stops through `ADA_PARC_PALETTE$fill` is
    gone: what is written is what is checked.
    """
    text = path.read_text(encoding="utf-8")
    na_default_m = re.search(r'MAP_NA_DEFAULT\s*<-\s*"(#[0-9a-fA-F]{6})"', text)
    if not na_default_m:
        raise SystemExit("check_palette: cannot find MAP_NA_DEFAULT")
    na_default = na_default_m.group(1)
    start = text.find("ADAPARC_PALETTES <- list(")
    if start < 0:
        raise SystemExit(f"check_palette: cannot find ADAPARC_PALETTES in {path}")
    body = text[start:]

    def named_block(blk: str, name: str, pid: str, keys=TIERS) -> dict:
        # \b so "fill" cannot match inside "on_fill" or "na_fill", and
        # "identity" cannot match inside "on_identity".
        m = re.search(r"(?<![\w_])" + name + r"\s*=\s*c\((.*?)\)", blk, re.S)
        if not m:
            raise SystemExit(
                f"check_palette: palette '{pid}' has no '{name}' block")
        got = dict(re.findall(r'(\w+)\s*=\s*"(#[0-9a-fA-F]{6})"', m.group(1)))
        missing = [t for t in keys if t not in got]
        if missing:
            raise SystemExit(
                f"check_palette: palette '{pid}' '{name}' is missing {missing}")
        return got

    entries: dict[str, dict] = {}
    pattern = re.compile(r"^\s{2}(\w+)\s*=\s*list\((.*?)^\s{2}\)", re.S | re.M)
    for m in pattern.finditer(body):
        pid, blk = m.group(1), m.group(2)
        label = re.search(r'label\s*=\s*"([^"]*)"', blk)
        na = re.search(r'\bna\s*=\s*"(#[0-9a-fA-F]{6})"', blk)
        entries[pid] = {
            "label": label.group(1) if label else pid,
            "fill": named_block(blk, "fill", pid),
            "on_fill": named_block(blk, "on_fill", pid),
            # Phase 2 of docs/palette-v2-build-plan.qmd. Required of every
            # entry, like fill, and missing one raises rather than defaulting:
            # a palette with no identity set would silently fall back to
            # heritage on the scorecards, which is the exact defect phase 2
            # exists to remove.
            "identity": named_block(blk, "identity", pid, keys=DOMAINS),
            "on_identity": named_block(blk, "on_identity", pid, keys=DOMAINS),
            # entries written as `na = MAP_NA_DEFAULT` carry no literal
            "na": na.group(1) if na else (
                na_default if "MAP_NA_DEFAULT" in blk else None),
        }

    order_m = re.search(r"PALETTE_ORDER\s*<-\s*c\((.*?)\)", text, re.S)
    if not order_m:
        raise SystemExit("check_palette: cannot find PALETTE_ORDER")
    order = re.findall(r'"(\w+)"', order_m.group(1))
    missing = [o for o in order if o not in entries]
    if missing:
        raise SystemExit(f"check_palette: PALETTE_ORDER names unknown "
                         f"palettes {missing}")
    unlisted = [k for k in entries if k not in order]
    if unlisted:
        raise SystemExit(f"check_palette: palettes defined but not in "
                         f"PALETTE_ORDER, so unreachable in the UI: {unlisted}")
    return {o: entries[o] for o in order}


# ------------------------------------------------------------------------------
# Checks
# ------------------------------------------------------------------------------

class Report:
    def __init__(self) -> None:
        self.failures: list[str] = []
        self.lines: list[str] = []

    def ok(self, msg: str) -> None:
        self.lines.append(f"  PASS  {msg}")

    def fail(self, msg: str) -> None:
        self.lines.append(f"  FAIL  {msg}")
        self.failures.append(msg)

    def check(self, cond: bool, msg: str) -> None:
        (self.ok if cond else self.fail)(msg)

    def section(self, title: str) -> None:
        self.lines.append("")
        self.lines.append(title)
        self.lines.append("-" * len(title))


def check_invariants(p: dict, r: Report) -> None:
    r.section("1. Palette accessibility invariants")

    for t in TIERS:
        ratio = contrast_ratio(p["fill"][t], p["on_fill"][t])
        r.check(ratio >= AA_TEXT,
                f"on_fill[{t}] {p['on_fill'][t]} on fill[{t}] {p['fill'][t]} "
                f"= {ratio:.2f}:1 (need {AA_TEXT})")

    for t in TIERS:
        cw = contrast_ratio(p["on_white"][t], "#ffffff")
        ct = contrast_ratio(p["on_white"][t], TABLE_TINT)
        r.check(cw >= AA_TEXT,
                f"on_white[{t}] {p['on_white'][t]} on white = {cw:.2f}:1")
        r.check(ct >= AA_TEXT,
                f"on_white[{t}] {p['on_white'][t]} on table tint {TABLE_TINT} "
                f"= {ct:.2f}:1")

    ratio = contrast_ratio(p["na_fill"], p["na_text"])
    r.check(ratio >= AA_TEXT,
            f"na_text {p['na_text']} on na_fill {p['na_fill']} = {ratio:.2f}:1")

    # Fill ramp must stay separable for every vision type. This is the check
    # that the Heritage adoption existed to satisfy.
    ramp = [p["fill"][t] for t in TIERS]
    for v in VISION:
        labs = [rgb_to_lab(hex_to_rgb(simulate_cvd(c, v))) for c in ramp]
        deltas = [ciede2000(labs[i], labs[i + 1]) for i in range(len(labs) - 1)]
        r.check(min(deltas) >= FILL_DELTA_E,
                f"fill ramp adjacent separation under {v}: "
                f"min {min(deltas):.1f} CIEDE2000 (need {FILL_DELTA_E})")

    # Fill ramp must be monotonic light to dark, which is what keeps the
    # neg-indicator reversal in render_national_map() correct.
    ls = [rgb_to_lab(hex_to_rgb(p["fill"][t]))[0] for t in TIERS]
    r.check(all(ls[i] > ls[i + 1] for i in range(len(ls) - 1)),
            f"fill ramp monotonic light to dark, L* = "
            f"{', '.join(f'{x:.1f}' for x in ls)}")


def check_map_palettes(p: dict, maps: dict, r: Report) -> None:
    """Every reader-selectable palette must meet the admission criteria."""
    r.section("2. Palette catalogue (every selectable entry)")

    for pid, entry in maps.items():
        cols = [entry["fill"][t] for t in TIERS]
        na = entry["na"] or p["na_fill"]
        name = f"{entry['label']} ({pid})"

        if len(cols) != 4:
            r.fail(f"{name}: expected 4 stops, found {len(cols)}")
            continue

        # on_fill is what the code actually draws, so check the RECORDED
        # pairing, not just that some legible ink exists. Extended from
        # heritage-only 2026-08-03: once a palette can colour a scorecard, a
        # wrong on_fill ships text at 2.31:1 rather than merely looking odd.
        for t in TIERS:
            ink = entry["on_fill"][t]
            ratio = contrast_ratio(entry["fill"][t], ink)
            r.check(ratio >= AA_TEXT,
                    f"{name}: on_fill[{t}] {ink} on fill[{t}] "
                    f"{entry['fill'][t]} = {ratio:.2f}:1 (need {AA_TEXT})")

        # on_fill must also be the BETTER of the two candidates. A pairing that
        # clears AA but is not the best available is a sign someone hand-picked
        # it, which is how the ramp drifts.
        for t in TIERS:
            best = max(("#111111", contrast_ratio(entry["fill"][t], "#111111")),
                       ("#ffffff", contrast_ratio(entry["fill"][t], "#ffffff")),
                       key=lambda x: x[1])
            r.check(entry["on_fill"][t].lower() == best[0],
                    f"{name}: on_fill[{t}] is the better of near-black and "
                    f"white ({best[0]} at {best[1]:.2f}:1)")

        # Monotonic light to dark, which keeps the neg-indicator reversal safe.
        ls = [rgb_to_lab(hex_to_rgb(c))[0] for c in cols]
        r.check(all(ls[i] > ls[i + 1] for i in range(3)),
                f"{name}: monotonic light to dark, L* "
                f"{', '.join(f'{x:.1f}' for x in ls)}")

        # Adjacent separation under every vision type.
        worst_v, worst_d = None, 1e9
        for v in VISION:
            labs = [rgb_to_lab(hex_to_rgb(simulate_cvd(c, v))) for c in cols]
            d = min(ciede2000(labs[i], labs[i + 1]) for i in range(3))
            if d < worst_d:
                worst_d, worst_v = d, v
        r.check(worst_d >= FILL_DELTA_E,
                f"{name}: adjacent separation min {worst_d:.1f} CIEDE2000 "
                f"(worst under {worst_v}, need {FILL_DELTA_E})")

        # Every stop must be able to carry a label.
        weak = [c for c in cols
                if max(contrast_ratio(c, "#ffffff"),
                       contrast_ratio(c, "#111111")) < AA_TEXT]
        r.check(not weak,
                f"{name}: every stop carries near-black or white text at "
                f"{AA_TEXT}:1" + (f" (failing: {weak})" if weak else ""))

        # The no-data colour must not be mistakable for any bin. This is the
        # check that forced High Contrast Mono onto a chromatic no-data value.
        worst_na_v, worst_na = None, 1e9
        for v in VISION:
            na_lab = rgb_to_lab(hex_to_rgb(simulate_cvd(na, v)))
            d = min(ciede2000(na_lab,
                              rgb_to_lab(hex_to_rgb(simulate_cvd(c, v))))
                    for c in cols)
            if d < worst_na:
                worst_na, worst_na_v = d, v
        r.check(worst_na >= FILL_DELTA_E,
                f"{name}: no-data {na} is {worst_na:.1f} CIEDE2000 from the "
                f"nearest bin (worst under {worst_na_v}, need {FILL_DELTA_E})")


def check_identity(maps: dict, r: Report) -> None:
    """The three domain colours, per palette. Phase 2.4 of the v2 build plan.

    Four criteria, and the third is the one that is easy to forget. An identity
    colour is not only a background for its own label: it is a bar whose length
    has to be readable, a card border and a rule. Those are graphical objects
    required to understand the content, so they answer to SC 1.4.11 against the
    page, not only to SC 1.4.3 against their own text.

    PAIRWISE, not adjacent. The tier ramp is a sequence and only neighbours can
    be confused; the domains are peers and any two of the three sitting side by
    side have to be tellable apart.
    """
    r.section("9. Domain identity triples (every selectable entry)")

    for pid, entry in maps.items():
        name = f"{entry['label']} ({pid})"
        ident = entry["identity"]
        ink = entry["on_identity"]
        cols = [ident[d] for d in DOMAINS]

        # 1. Pairwise separation under every vision type.
        worst_v, worst_d, worst_pair = None, 1e9, None
        for v in VISION:
            labs = {d: rgb_to_lab(hex_to_rgb(simulate_cvd(ident[d], v)))
                    for d in DOMAINS}
            for a, b in ((0, 1), (0, 2), (1, 2)):
                d = ciede2000(labs[DOMAINS[a]], labs[DOMAINS[b]])
                if d < worst_d:
                    worst_d, worst_v = d, v
                    worst_pair = f"{DOMAINS[a]}/{DOMAINS[b]}"
        r.check(worst_d >= IDENTITY_DELTA_E,
                f"{name}: pairwise separation min {worst_d:.1f} CIEDE2000 "
                f"({worst_pair} under {worst_v}, need {IDENTITY_DELTA_E})")

        # 2. The RECORDED ink clears AA, and is the better of the two.
        for d in DOMAINS:
            ratio = contrast_ratio(ident[d], ink[d])
            r.check(ratio >= AA_TEXT,
                    f"{name}: on_identity[{d}] {ink[d]} on {ident[d]} "
                    f"= {ratio:.2f}:1 (need {AA_TEXT})")
            best = max(("#111111", contrast_ratio(ident[d], "#111111")),
                       ("#ffffff", contrast_ratio(ident[d], "#ffffff")),
                       key=lambda x: x[1])
            r.check(ink[d].lower() == best[0],
                    f"{name}: on_identity[{d}] is the better of near-black and "
                    f"white ({best[0]} at {best[1]:.2f}:1)")

        # 3. Visible as a graphical object against the darkest surface this
        #    palette can be drawn on.
        surface = DOC_TINT_HIGH if pid == "mono_high" else DOC_TINT
        for d in DOMAINS:
            ratio = contrast_ratio(ident[d], surface)
            r.check(ratio >= AA_NONTEXT,
                    f"{name}: identity[{d}] {ident[d]} vs {surface} "
                    f"= {ratio:.2f}:1 (SC 1.4.11, need {AA_NONTEXT})")

        # 4. Monotonic light to dark, because the assignment is positional.
        ls = [rgb_to_lab(hex_to_rgb(c))[0] for c in cols]
        r.check(all(ls[i] > ls[i + 1] for i in range(len(ls) - 1)),
                f"{name}: identity monotonic cl light to we dark, L* "
                f"{', '.join(f'{x:.1f}' for x in ls)}")


def parse_ui_tokens(path: Path) -> tuple[dict, dict, list]:
    """Pull the contrast token set out of scripts/palettes.R.

    Returns (tokens, sizes, pairs) where tokens and sizes are
    {"standard": {name: value}, "high": {...}} with snake_case names, and pairs
    is a list of {"fg", "bg", "kind", "label"}.

    Added 2026-08-03 for phase 4. The pair list is read from the source rather
    than restated here, so a token added in palettes.R without a pairing is
    visibly unchecked instead of silently unchecked.
    """
    text = path.read_text(encoding="utf-8")

    def two_modes(var: str) -> dict:
        start = text.find(var + " <- list(")
        if start < 0:
            raise SystemExit(f"check_palette: cannot find {var} in {path}")
        # Stop at this variable's own closing paren, the first ")" in column 0.
        # Without the bound, ADAPARC_UI_TOKENS would run on into
        # ADAPARC_UI_SIZES, whose blocks carry the same two mode names and
        # would silently overwrite the colours with the widths.
        end = re.search(r"^\)", text[start:], re.M)
        body = text[start:start + end.start()] if end else text[start:]
        out: dict[str, dict] = {}
        for m in re.finditer(r"^\s{2}(standard|high)\s*=\s*c\((.*?)^\s{2}\)",
                             body, re.S | re.M):
            out[m.group(1)] = dict(
                re.findall(r'(\w+)\s*=\s*"([^"]+)"', m.group(2)))
        missing = [k for k in ("standard", "high") if k not in out]
        if missing:
            raise SystemExit(f"check_palette: {var} is missing modes {missing}")
        return out

    tokens = two_modes("ADAPARC_UI_TOKENS")
    sizes = two_modes("ADAPARC_UI_SIZES")

    if set(tokens["standard"]) != set(tokens["high"]):
        only_s = sorted(set(tokens["standard"]) - set(tokens["high"]))
        only_h = sorted(set(tokens["high"]) - set(tokens["standard"]))
        raise SystemExit(
            "check_palette: ADAPARC_UI_TOKENS modes do not carry the same "
            f"token names. standard only: {only_s}; high only: {only_h}")

    return tokens, sizes, parse_pairs(text, "ADAPARC_UI_PAIRS")


def parse_pairs(text: str, var: str) -> list:
    """Read a foreground/background pair list out of scripts/palettes.R."""
    start = text.find(var + " <- list(")
    if start < 0:
        raise SystemExit(f"check_palette: cannot find {var}")
    end = re.search(r"^\)", text[start:], re.M)
    body = text[start:start + end.start()] if end else text[start:]
    pairs = []
    for m in re.finditer(
            r'list\(\s*fg\s*=\s*"(\w+)"\s*,\s*bg\s*=\s*"(\w+)"\s*,'
            r'\s*kind\s*=\s*"(\w+)"\s*,\s*label\s*=\s*"([^"]*)"', body):
        pairs.append({"fg": m.group(1), "bg": m.group(2),
                      "kind": m.group(3), "label": m.group(4)})
    if not pairs:
        raise SystemExit(f"check_palette: {var} parsed as empty")
    return pairs


def parse_doc_tokens(path: Path) -> tuple[dict, list]:
    """Pull ADAPARC_DOC_TOKENS and ADAPARC_DOC_PAIRS out of scripts/palettes.R.

    The scorecards are documents, not app chrome, so they carry their own token
    group. Added 2026-08-03 for phase 5.
    """
    text = path.read_text(encoding="utf-8")
    start = text.find("ADAPARC_DOC_TOKENS <- list(")
    if start < 0:
        raise SystemExit("check_palette: cannot find ADAPARC_DOC_TOKENS")
    end = re.search(r"^\)", text[start:], re.M)
    body = text[start:start + end.start()] if end else text[start:]
    tokens: dict[str, dict] = {}
    for m in re.finditer(r"^\s{2}(standard|high)\s*=\s*c\((.*?)^\s{2}\)",
                         body, re.S | re.M):
        tokens[m.group(1)] = dict(
            re.findall(r'(\w+)\s*=\s*"([^"]+)"', m.group(2)))
    missing = [k for k in ("standard", "high") if k not in tokens]
    if missing:
        raise SystemExit(
            f"check_palette: ADAPARC_DOC_TOKENS is missing modes {missing}")
    if set(tokens["standard"]) != set(tokens["high"]):
        raise SystemExit(
            "check_palette: ADAPARC_DOC_TOKENS modes do not carry the same "
            "token names")
    return tokens, parse_pairs(text, "ADAPARC_DOC_PAIRS")


# Floors per pair kind and contrast mode.
#   text    WCAG 2.2 SC 1.4.3 AA in standard, SC 1.4.6 AAA in high contrast
#   nontext WCAG 2.2 SC 1.4.11, 3:1 in both
CONTRAST_FLOORS = {
    ("text", "standard"): 4.5,
    ("text", "high"): 7.0,
    ("nontext", "standard"): 3.0,
    ("nontext", "high"): 3.0,
}


def check_token_pairs(tokens: dict, pairs: list, r: Report,
                      title: str) -> None:
    r.section(title)

    for mode in ("standard", "high"):
        tok = tokens[mode]
        for p in pairs:
            for role in ("fg", "bg"):
                if p[role] not in tok:
                    r.fail(f"{mode}: pair '{p['label']}' names unknown token "
                           f"'{p[role]}'")
            if p["fg"] not in tok or p["bg"] not in tok:
                continue
            ratio = contrast_ratio(tok[p["fg"]], tok[p["bg"]])
            floor = CONTRAST_FLOORS.get((p["kind"], mode))
            if floor is None:
                # Declared decorative. Measured and printed so the judgment
                # stays visible, but not required to clear anything.
                r.lines.append(
                    f"  DECOR {mode}: {p['label']} = {ratio:.2f}:1 "
                    f"({tok[p['fg']]} on {tok[p['bg']]}, no floor applied)")
                continue
            r.check(ratio >= floor,
                    f"{mode}: {p['label']} = {ratio:.2f}:1 "
                    f"(need {floor}, {tok[p['fg']]} on {tok[p['bg']]})")

    # A token nobody pairs is a token nobody checks. Report it rather than
    # letting it sit there looking verified.
    used = {p[k] for p in pairs for k in ("fg", "bg")}
    unpaired = sorted(set(tokens["standard"]) - used)
    if unpaired:
        r.lines.append(f"  NOTE  tokens with no declared pairing, so not "
                       f"contrast-checked: {', '.join(unpaired)}")


def parse_css_root(path: Path) -> dict[str, str]:
    """Read the first :root block of a stylesheet into {--name: value}."""
    text = path.read_text(encoding="utf-8")
    m = re.search(r":root\s*\{(.*?)\}", text, re.S)
    if not m:
        return {}
    return {k: v.strip()
            for k, v in re.findall(r"(--[\w-]+)\s*:\s*([^;]+);", m.group(1))}


def check_css_fallbacks(path: Path, want: dict, r: Report, label: str,
                        require_all: bool = True) -> None:
    """A stylesheet's own :root is a FALLBACK copy of the STANDARD tokens.

    It is what renders before the generated block is injected, and what renders
    if the injection fails or the file is opened on its own, so it has to agree
    with palettes.R exactly.

    require_all=False checks only the tokens the stylesheet actually declares,
    which is right for the scorecard stylesheets: each uses a subset, and
    declaring tokens it never reads would be noise.
    """
    if not path.exists():
        r.fail(f"{label}: file not found at {path}")
        return
    got = parse_css_root(path)
    if not got:
        r.fail(f"{label}: no :root block found")
        return
    for name, value in sorted(want.items()):
        css_name = "--" + name.replace("_", "-")
        if css_name not in got:
            if require_all:
                r.fail(f"{label}: {css_name} not declared")
            continue
        if got[css_name].lower() != value.lower():
            r.fail(f"{label}: {css_name} is {got[css_name]}, "
                   f"expected {value}")
        else:
            r.ok(f"{label}: {css_name} = {value}")
    # A stylesheet must not invent a token name the source does not know.
    known = {"--" + k.replace("_", "-") for k in want}
    for css_name in sorted(got):
        if css_name not in known:
            r.fail(f"{label}: {css_name} is declared here but is not in "
                   f"scripts/palettes.R")


def emitted_token_names(tokens: dict, sizes: dict) -> set[str]:
    """The kebab-case custom property names pal_css_rules() writes.

    Kept in step with that function by hand, which is a real risk, so the check
    below is deliberately generous: it only reports var() references that match
    NOTHING and carry NO fallback, which is the case that renders an element
    invisible rather than merely off-brand.
    """
    names = {"palette-id", "contrast-mode", "tier-na-bg", "tier-na-text",
             "tier-na-ink", "tier-map-na-bg", "tier-stroke"}
    for t in TIERS:
        names |= {f"tier-{t}-bg", f"tier-{t}-text", f"tier-{t}-ink"}
    for k in list(tokens["standard"]) + list(sizes["standard"]):
        names.add(k.replace("_", "-"))
    return names


def check_var_references(tokens: dict, sizes: dict, r: Report,
                         doc_tokens: dict | None = None) -> None:
    """Every var(--x) in the site stylesheets must resolve to something.

    A var() naming a property nobody defines, with no fallback, computes to the
    initial value: transparent for a colour, which is an invisible border or
    invisible text rather than a wrong one. That failure mode is silent in a
    browser and invisible in a diff, so it is checked here.
    """
    r.section("6. var() references resolve")
    known = emitted_token_names(tokens, sizes)
    if doc_tokens:
        known |= {k.replace("_", "-") for k in doc_tokens["standard"]}
    targets = [
        (ROOT / "www" / "styles.css", "styles.css"),
        (ROOT / "www" / "cssloaders.html", "cssloaders.html"),
        (ROOT / "scorecard" / "scorecard_state.css", "scorecard_state.css"),
        (ROOT / "scorecard" / "scorecard_v3.css", "scorecard_v3.css"),
    ]
    unresolved = []
    checked = 0
    for path, label in targets:
        if not path.exists():
            r.fail(f"{label}: not found at {path}")
            continue
        text = path.read_text(encoding="utf-8")
        declared = set(re.findall(r"(--[\w-]+)\s*:", text))
        for m in re.finditer(r"var\(\s*(--[\w-]+)\s*(,)?", text):
            checked += 1
            name, has_fallback = m.group(1), bool(m.group(2))
            if has_fallback:
                continue
            if name[2:] in known or name in declared:
                continue
            unresolved.append(f"{label}: var({name}) has no definition "
                              f"and no fallback")
    for u in unresolved:
        r.fail(u)
    if not unresolved:
        r.ok(f"all {checked} var() references resolve to a token, a local "
             f"declaration, or a fallback")


def check_mirror(path: Path, expected: dict[str, str], r: Report,
                 label: str, within: str | None = None) -> None:
    """Verify each expected token -> hex appears in a file, case-insensitively.

    `within` is a regex naming a block to search inside. adaparc-map.js declares
    the same tier keys twice, once for fills and once for border ink, and this
    function matches the FIRST occurrence of a token; without a slice the border
    check would silently re-read the fills and pass on a drifted file.
    """
    if not path.exists():
        r.fail(f"{label}: file not found at {path}")
        return
    text = path.read_text(encoding="utf-8")
    if within is not None:
        block = re.search(within + r"\s*=\s*\{(.*?)\}", text, re.S)
        if block is None:
            r.fail(f"{label}: block '{within}' not found")
            return
        text = block.group(1)
    for token, want in expected.items():
        # 0,60 rather than 0,40: a var(--tier-excellent-bg, #560000) fallback
        # puts 40 characters between the selector and the hex on its own.
        m = re.search(re.escape(token) + r"[^#\n]{0,60}(#[0-9a-fA-F]{6})", text)
        if m is None:
            r.fail(f"{label}: '{token}' not found")
        elif m.group(1).lower() != want.lower():
            r.fail(f"{label}: '{token}' is {m.group(1)}, expected {want}")
        else:
            r.ok(f"{label}: {token} = {want}")


def check_panel_fallback(maps: dict, r: Report) -> None:
    """The Display panel's offline copy of the palette list still matches.

    Phase 1.3 of docs/palette-v2-build-plan.qmd. www/cssloaders.html carries
    FALLBACK_CHOICES, a hand-written {id, label} list used only when the server
    does not answer the panel's request for its state. It cannot be generated
    from palettes.R, because the case it exists for is precisely the case where
    the server did not answer. So it is duplicated, and duplicated state that
    nothing checks is duplicated state that drifts. This is the check.

    Ids AND order both matter: the panel presents them as the selector's
    options, so a reordering here would silently change what a reader on a
    broken load sees first.
    """
    path = ROOT / "www" / "cssloaders.html"
    if not path.exists():
        r.fail(f"panel fallback: file not found at {path}")
        return
    text = path.read_text(encoding="utf-8")
    m = re.search(r"FALLBACK_CHOICES\s*=\s*\[(.*?)\]", text, re.S)
    if not m:
        r.fail("panel fallback: FALLBACK_CHOICES not found in cssloaders.html")
        return
    got = re.findall(r"id:\s*'([^']*)'\s*,\s*label:\s*'([^']*)'", m.group(1))
    want = [(pid, e["label"]) for pid, e in maps.items()]
    if got == want:
        r.ok(f"panel fallback: {len(got)} palettes match PALETTE_ORDER, in order")
        return
    r.fail("panel fallback: FALLBACK_CHOICES in www/cssloaders.html is out of "
           "step with PALETTE_ORDER in scripts/palettes.R")
    r.fail(f"  expected: {want}")
    r.fail(f"  found:    {got}")


def check_mirrors(p: dict, maps: dict, r: Report) -> None:
    r.section("3. Static mirrors of the source of truth")

    check_panel_fallback(maps, r)

    # The .rp-qual rules became FILLED CHIPS on 2026-08-03, so the value
    # mirrored here is the heritage `fill`, not `on_white`. They were coloured
    # text and could not follow the reader's palette, because a per-palette
    # on_white ramp collapses; a chip puts the colour on a fill, which does
    # follow. The live values come from var(--tier-*-bg); these are the
    # fallbacks for opening the stylesheet on its own.
    check_mirror(
        ROOT / "scorecard" / "scorecard_state.css",
        {
            ".rank-pill .rp-qual.poor":      p["fill"]["poor"],
            ".rank-pill .rp-qual.below-avg": p["fill"]["below"],
            ".rank-pill .rp-qual.above-avg": p["fill"]["above"],
            ".rank-pill .rp-qual.excellent": p["fill"]["excellent"],
        },
        r, "scorecard_state.css")

    css_expected = {}
    for t in TIERS:
        css_expected[f"--tier-{t}-bg"] = p["fill"][t]
        css_expected[f"--tier-{t}-text"] = p["on_fill"][t]
    css_expected["--tier-na-bg"] = p["na_fill"]
    css_expected["--tier-na-text"] = p["na_text"]
    check_mirror(
        ROOT / "factsheets" / "_generate" / "assets" / "factsheet-base.css",
        css_expected, r, "factsheet-base.css")

    js_map = ROOT / "factsheets" / "_generate" / "assets" / "adaparc-map.js"

    js_expected = {f"{t}:": p["fill"][t] for t in TIERS}
    js_expected["na:"] = p["na_fill"]
    check_mirror(js_map, js_expected, r, "adaparc-map.js",
                 within=r"const ADAPARC_TIER_COLOR")

    # Per-state border ink, adopted 2026-08-04. The fact sheet map draws each
    # state's border in the ink its own fill calls for, so this block mirrors
    # on_fill exactly, and na_text for the no-data fill.
    js_border = {f"{t}:": p["on_fill"][t] for t in TIERS}
    js_border["na:"] = p["na_text"]
    check_mirror(js_map, js_border, r, "adaparc-map.js border ink",
                 within=r"const ADAPARC_TIER_BORDER")


def iter_source_files():
    for pattern in SEARCH_GLOBS:
        for path in ROOT.rglob(pattern):
            rel = path.relative_to(ROOT)
            if rel.as_posix() in SELF_EXEMPT:
                continue
            parts = set(rel.parts)
            if parts & EXCLUDE_PARTS:
                continue
            if any(pt.startswith(EXCLUDE_NAME_PREFIX) for pt in path.parts):
                continue
            yield path


def check_no_retired(r: Report) -> None:
    r.section("7. Retired tier hexes absent from source")
    hits: list[str] = []
    for path in iter_source_files():
        try:
            text = path.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            continue
        for hexval, why in RETIRED.items():
            for m in re.finditer(re.escape(hexval), text, re.I):
                line = text.count("\n", 0, m.start()) + 1
                rel = path.relative_to(ROOT)
                hits.append(f"{rel}:{line}  {hexval}  ({why})")
    if hits:
        for h in sorted(hits):
            r.fail(f"retired hex still present: {h}")
    else:
        r.ok(f"none of the {len(RETIRED)} retired hexes appear in source files")


def check_generated(r: Report) -> list[str]:
    """Report rendered HTML still carrying the old ramp.

    Not a failure. Generated files are expected to lag a palette change until
    they are re-rendered; this section just says which ones need it and how.
    """
    r.section("8. Generated output still on the old ramp (informational)")
    targets = [
        ("www/factsheets/*.html", "fact sheets served by the app"),
        ("factsheets/_generate/_preview/*.html", "fact sheet previews"),
        ("scorecard/*.html", "scorecard render artefacts"),
        ("www/state_output.html", "last state scorecard shown in the app"),
        ("www/cat_output.html", "last category scorecard shown in the app"),
    ]
    stale: list[str] = []
    for pattern, label in targets:
        for path in sorted(ROOT.glob(pattern)):
            text = path.read_text(encoding="utf-8", errors="ignore")
            n = sum(len(re.findall(re.escape(h), text, re.I)) for h in RETIRED)
            if n:
                rel = path.relative_to(ROOT).as_posix()
                stale.append(rel)
                r.lines.append(f"  STALE {rel}  ({n} old-ramp hexes, {label})")
    if not stale:
        r.ok("no generated output carries the old ramp")
    return stale


def main() -> int:
    pal_path = ROOT / "scripts" / "palettes.R"
    p = parse_palettes_r(pal_path)
    maps = parse_catalogue(pal_path)
    ui_tokens, ui_sizes, ui_pairs = parse_ui_tokens(pal_path)
    doc_tokens, doc_pairs = parse_doc_tokens(pal_path)

    r = Report()
    print("=" * 78)
    print("ADA-PARC PALETTE CONSISTENCY CHECK")
    print(f"Source of truth: {pal_path.relative_to(ROOT)}")
    print("=" * 78)
    print()
    print("Brand-only roles (heritage, palette-independent):")
    for t in TIERS:
        print(f"  {t:<10} on_white {p['on_white'][t]}")
    print(f"  {'no data':<10} fill {p['na_fill']}   text {p['na_text']}"
          f"   accent {p['na_accent']}   stroke {p['stroke']}")
    print()
    print("Palette catalogue, in display order:")
    for pid, e in maps.items():
        cols = [e["fill"][t] for t in TIERS]
        inks = [e["on_fill"][t] for t in TIERS]
        na = e["na"] or p["na_fill"]
        print(f"  {e['label']:<20} {' '.join(cols)}   no-data {na}")
        print(f"  {'':<20} ink {' '.join(inks)}")

    check_invariants(p, r)
    check_map_palettes(p, maps, r)
    check_mirrors(p, maps, r)

    want_site = dict(ui_tokens["standard"]); want_site.update(ui_sizes["standard"])
    check_css_fallbacks(ROOT / "www" / "styles.css", want_site, r,
                        "styles.css :root fallbacks")
    # The scorecard stylesheets also fall back on the DEFAULT palette's identity
    # roles, which are palette values rather than document tokens, so they are
    # folded in here. heritage because that is what PALETTE_DEFAULT resolves to
    # and therefore what renders before the generated block arrives.
    doc_and_identity = dict(doc_tokens["standard"])
    for d in DOMAINS:
        doc_and_identity[f"identity_{d}"] = maps["heritage"]["identity"][d]
        doc_and_identity[f"on_identity_{d}"] = \
            maps["heritage"]["on_identity"][d]
    for css in ("scorecard_state.css", "scorecard_v3.css"):
        check_css_fallbacks(ROOT / "scorecard" / css, doc_and_identity, r,
                            f"{css} :root fallbacks", require_all=False)

    check_token_pairs(ui_tokens, ui_pairs, r,
                      "4. Contrast token set (site chrome)")
    check_token_pairs(doc_tokens, doc_pairs, r,
                      "5. Contrast token set (scorecard documents)")
    check_var_references(ui_tokens, ui_sizes, r, doc_tokens)
    check_no_retired(r)
    check_identity(maps, r)
    stale = check_generated(r)

    print("\n".join(r.lines))
    print()
    if stale:
        print(f"NOTE: {len(stale)} generated file(s) still carry the old ramp.")
        print("  Fact sheets:  quarto render factsheets/_generate/generate-factsheets.qmd")
        print("  Scorecards:   re-render from the app, or")
        print("                quarto render scorecard/state_scorecard.qmd")
        print("                quarto render scorecard/category_scorecard.qmd")
        print("  These are build artefacts, so they do not fail the check.")
        print()
    if r.failures:
        print(f"RESULT: {len(r.failures)} check(s) FAILED.")
        print("Fix scripts/palettes.R or the named file, then re-run.")
        return 1
    print("RESULT: all source checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
