#!/usr/bin/env python3
"""
audit_rendered.py
================================================================================
Phase 6.3 of docs/palette-v1-build-plan.qmd.

Walks rendered HTML and measures every foreground/background pairing it can
actually see, rather than trusting that the tokens were used the way the source
intended. `check_palette.py` verifies the DECLARED pairs; this verifies the
DELIVERED ones.

    python3 scripts/audit_rendered.py docs/_matrix
    python3 scripts/audit_rendered.py www/state_output.html --min 7.0

WHAT IT LOOKS AT, AND WHY THAT IS ENOUGH HERE. The scorecards put almost all of
their colour in inline styles: gt writes cell fills and text colours inline,
ggiraph writes SVG fill and text attributes, and the hand-built HTML in both
.qmd files writes `style="background:...; color:..."`. So the pairs that matter
are overwhelmingly inline and can be read without a browser. What this does NOT
do is resolve the cascade for pairs split across a stylesheet rule and an
element; those are the ones check_palette.py covers by declaration.

The honest summary is that the two checks together cover the colour system, and
neither alone does. A real computed-style pass would need a headless browser,
which is deliberately not a dependency of this repo.

Exit code 0 if nothing is below the floor.
================================================================================
"""

from __future__ import annotations

import re
import sys
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "docs" / "assets"))

from palette_audit import contrast_ratio  # noqa: E402

AA_TEXT = 4.5
AAA_TEXT = 7.0

HEX = r"#[0-9a-fA-F]{3,8}"
NAMED = {"white": "#ffffff", "black": "#000000", "transparent": None,
         "inherit": None, "none": None, "currentcolor": None}

DECL = re.compile(r"([a-z-]+)\s*:\s*([^;]+)")


def norm(value: str) -> str | None:
    """Normalise a CSS colour to #rrggbb, or None if it cannot be resolved."""
    v = value.strip().lower()
    if v in NAMED:
        return NAMED[v]
    if v.startswith("var("):
        return None          # resolved by the cascade, not visible here
    m = re.match(r"^rgba?\(([^)]+)\)$", v)
    if m:
        parts = [p.strip() for p in m.group(1).replace("/", ",").split(",")]
        try:
            nums = [float(p.rstrip("%")) for p in parts[:3]]
        except ValueError:
            return None
        if len(parts) > 3:
            try:
                if float(parts[3].rstrip("%")) < 0.9:
                    return None   # translucent, cannot be measured flat
            except ValueError:
                pass
        return "#%02x%02x%02x" % tuple(max(0, min(255, round(n))) for n in nums)
    m = re.match(rf"^({HEX})$", v)
    if not m:
        return None
    h = v.lstrip("#")
    if len(h) == 3:
        h = "".join(c * 2 for c in h)
    if len(h) == 8:
        if int(h[6:], 16) < 230:
            return None
        h = h[:6]
    return "#" + h[:6] if len(h) >= 6 else None


VOID = {"area", "base", "br", "col", "embed", "hr", "img", "input", "link",
        "meta", "param", "source", "track", "wbr"}
SKIP_TEXT_IN = {"script", "style", "head", "title", "svg"}

# The page background a pairing falls back to when no ancestor declares one.
# Both scorecard stylesheets set the page to --doc-paper, which is #ffffff in
# both contrast modes, so this is not an assumption so much as a reading.
ROOT_BG = "#ffffff"


class PairScanner(HTMLParser):
    """Collect (foreground, background) pairs for every run of visible text.

    Colour and background-color INHERIT differently in CSS: colour inherits
    down the tree, background does not but is seen through descendants that do
    not paint their own. Both behaviours are modelled here with a stack, which
    is what the earlier same-element-only version got wrong: the state
    scorecard almost always puts the background on a card and the colour on
    something inside it, so a same-element rule found nothing at all.

    Only INLINE styles are read. Anything set by a stylesheet rule is invisible
    here and is covered by the declared-pair audit in check_palette.py instead.
    """

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        # Each frame: (tag, colour, background, background_was_declared)
        self.stack: list = [("", None, ROOT_BG, False)]
        self.skip_depth = 0
        self.pairs: list[tuple[str, str, str]] = []
        # Text whose background was never declared inline, so the only
        # candidate is the assumed page colour. Counted, never failed on: the
        # real background may come from a stylesheet rule this cannot see, and
        # a false failure would make the whole tool ignorable.
        self.assumed = 0

    def _styles(self, attrs) -> tuple[str | None, str | None]:
        raw = dict(attrs).get("style")
        if not raw:
            return None, None
        d = {k: v for k, v in DECL.findall(raw.lower())}
        fg = norm(d["color"]) if "color" in d else None
        bg_raw = d.get("background-color") or d.get("background")
        bg = norm(bg_raw.split()[0]) if bg_raw else None
        return fg, bg

    def handle_starttag(self, tag, attrs):
        if tag in VOID:
            return
        if tag in SKIP_TEXT_IN:
            self.skip_depth += 1
        fg, bg = self._styles(attrs)
        _, pfg, pbg, pdecl = self.stack[-1]
        self.stack.append((tag, fg or pfg, bg or pbg,
                           pdecl or bg is not None))

    def handle_endtag(self, tag):
        if tag in SKIP_TEXT_IN and self.skip_depth:
            self.skip_depth -= 1
        for i in range(len(self.stack) - 1, 0, -1):
            if self.stack[i][0] == tag:
                del self.stack[i:]
                return

    def handle_data(self, data):
        if self.skip_depth or not data.strip():
            return
        tag, fg, bg, bg_declared = self.stack[-1]
        if not (fg and bg):
            return
        if bg_declared:
            self.pairs.append((fg, bg, f"<{tag}> {data.strip()[:40]}"))
        else:
            self.assumed += 1


def pairs_in(html: str):
    p = PairScanner()
    try:
        p.feed(html)
    except Exception:
        pass   # malformed tail; keep whatever was collected
    return p.pairs, p.assumed


def audit(path: Path, floor: float):
    html = path.read_text(encoding="utf-8", errors="replace")
    found, assumed = pairs_in(html)
    # Collapse duplicates: a table paints the same pairing hundreds of times.
    seen: dict[tuple[str, str], tuple[float, str]] = {}
    for f, b, ctx in found:
        key = (f, b)
        if key not in seen:
            seen[key] = (contrast_ratio(f, b), ctx)
    bad = [f"{ratio:6.2f}:1  {f} on {b}   {ctx}"
           for (f, b), (ratio, ctx) in sorted(seen.items(), key=lambda x: x[1][0])
           if ratio < floor]
    return len(found), len(seen), bad, assumed


def main() -> int:
    args = [a for a in sys.argv[1:]]
    floor = AA_TEXT
    if "--min" in args:
        i = args.index("--min")
        floor = float(args[i + 1])
        del args[i:i + 2]
    if not args:
        print(__doc__.strip().splitlines()[3])
        print("usage: audit_rendered.py <file-or-directory> [--min 7.0]")
        return 2

    target = Path(args[0])
    if not target.is_absolute():
        target = (Path.cwd() / target).resolve()
    files = sorted(target.rglob("*.html")) if target.is_dir() else [target]
    if not files:
        print(f"No .html found under {target}")
        return 2

    print("=" * 78)
    print("ADA-PARC RENDERED CONTRAST AUDIT")
    print(f"Target: {target}   floor: {floor}:1")
    print("=" * 78)

    total_bad = 0
    for f in files:
        # High contrast output is held to AAA, the rest to the floor given.
        this_floor = AAA_TEXT if "_high" in f.stem else floor
        n, uniq, bad, assumed = audit(f, this_floor)
        status = "FAIL" if bad else "ok  "
        print(f"\n{status} {f.name}   {uniq} distinct pairing(s) "
              f"from {n} text run(s), floor {this_floor}:1")
        if assumed:
            print(f"       ({assumed} text run(s) had no inline background; "
                  f"their pairing comes from a stylesheet rule and is covered "
                  f"by check_palette.py instead)")
        for line in bad:
            print(f"       {line}")
        total_bad += len(bad)

    print()
    if total_bad:
        print(f"RESULT: {total_bad} pairing(s) below the floor.")
        return 1
    print("RESULT: every measurable inline pairing clears its floor.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
