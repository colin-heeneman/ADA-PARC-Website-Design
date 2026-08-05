#!/usr/bin/env python3
# =============================================================================
# search_identity.py
#
# Phase 2, steps 2.1 and 2.2 of docs/palette-v2-build-plan.qmd.
#
# Searches for an IDENTITY TRIPLE for each entry in the palette catalogue: the
# three colours that stand for Community Living, Community Participation and
# Work & Economic on the scorecards. These are a CATEGORICAL role, not an
# ordinal one. Nothing about CL is "more" than WE, so unlike the tier ramp in
# scripts/palettes.R these colours must not read as a sequence, and the only
# thing that matters between them is that they are TELLABLE APART.
#
#     python3 scripts/search_identity.py            # the frontier, all cells
#     python3 scripts/search_identity.py --fine     # add the fine refinement
#     python3 scripts/search_identity.py --self-test
#
# WHAT THIS DOES NOT DO. It does not choose. Step 2.1 of the plan is explicit
# that the constraint set is a design decision about how much brand warmth to
# spend, and that decision is not the search's to make. So this reports a
# frontier and stops there. Nothing here writes to palettes.R.
#
# -----------------------------------------------------------------------------
# THE OBJECTIVE IS THE INVERSE OF THE OBVIOUS ONE, AND THAT WAS A CORRECTION
# -----------------------------------------------------------------------------
# The first version of this script maximised separation, which is what section 4
# of the build plan measured. Run it and the answer for every palette is a
# corner of the sRGB cube: #a0ff00 and #ff60ff, neon green and magenta, scoring
# 59. Technically optimal, obviously unusable, and it answers a question nobody
# asked. Maximum separation is not scarce. The frontier reported by --ceiling is
# kept only to establish that, because knowing the ceiling is 59 is what makes
# it clear that the floor of 15 is cheap.
#
# What IS scarce is separation that still looks like the palette it belongs to.
# So the objective is turned around. Fix a separation target, then find the
# triple meeting it that sits CLOSEST TO THE PALETTE'S OWN RAMP, measured as the
# worst member's CIEDE2000 distance to the ramp's path through CIELAB. That
# distance, in the units the rest of this repo already argues in, is the price:
# "a target of 20 costs heritage 18 units of drift from its own ramp."
#
# That is the number step 2.1 needs. "How much brand warmth is separation worth"
# is unanswerable in the abstract and answerable once warmth has a price.
#
# -----------------------------------------------------------------------------
# THE MEASURE
# -----------------------------------------------------------------------------
# A triple's score is the WORST pairwise CIEDE2000 distance among its three
# pairs, taken as the worst across normal vision and simulated protanopia,
# deuteranopia and tritanopia. Worst-of-worst, deliberately: a triple is only as
# good as the pair a reader cannot separate, and a reader with deuteranopia is
# not an edge case. Roughly 8 percent of men have a red-green deficiency.
#
# The floor is 15 CIEDE2000, the same working floor the tier ramp is held to;
# see the ADMISSION CRITERIA note in scripts/palettes.R.
#
# -----------------------------------------------------------------------------
# WHY THE COLOUR MATHS IS DUPLICATED HERE
# -----------------------------------------------------------------------------
# docs/assets/palette_audit.py already has ciede2000(), rgb_to_lab() and the
# Machado CVD matrices, and they are correct. They are also SCALAR, one colour
# pair per call, which is right for auditing five palettes of four stops and
# hopeless for a search over millions of pairs.
#
# So the functions below are vectorised rewrites, and --self-test asserts they
# agree with the scalar originals to 1e-9 over a pseudo-random sample. The
# matrices and constants are IMPORTED from palette_audit rather than retyped, so
# the two implementations cannot drift on the numbers even in principle; only
# the arithmetic is restated. Run the self-test before trusting any output.
# =============================================================================

from __future__ import annotations

import argparse
import itertools
import sys
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "docs" / "assets"))
sys.path.insert(0, str(ROOT / "scripts"))

import palette_audit as pa          # noqa: E402  scalar reference implementation
from check_palette import parse_catalogue   # noqa: E402


# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

WHITE = "#ffffff"
NEAR_BLACK = "#111111"

MIN_TEXT_CONTRAST = 4.5    # WCAG 2.2 SC 1.4.3 AA, normal-size text
MIN_SEPARATION = 15.0      # the working floor; see THE MEASURE above

# WCAG 2.2 SC 1.4.11, non-text contrast, against the paper the identity colour
# is drawn on.
#
# THIS CONSTRAINT WAS ADDED AFTER THE FIRST RUN AND IT CHANGED THE ANSWER. The
# search without it returned #ffffff as one of High Contrast Mono's three
# identity colours: a perfect result by every measure it was being scored on,
# and a white bar on white paper. The "light chip with dark text" allowance is
# real, but it is bounded, because an identity colour is not only a background
# for its own label. It is also a BAR, whose length has to be readable, and a
# card border and a rule. Those are graphical objects required to understand the
# content, and they have to be visible against the page.
MIN_NONTEXT_CONTRAST = 3.0

VISION = ("normal", "protanopia", "deuteranopia", "tritanopia")

# EVERY surface an identity colour is drawn on, from ADAPARC_DOC_TOKENS in
# scripts/palettes.R, in both contrast modes.
#
#   doc_paper  #ffffff / #ffffff   the domain card, and the sub-index bar track
#   doc_cream  #faf7f3 / #ffffff   the domain score badge
#   doc_tint   #f4efe9 / #e8e8e8   the summary table row, and its 4px left rule
#
# CORRECTED TWICE ON 2026-08-03, WHICH IS THE POINT OF WRITING THEM ALL DOWN.
# The constraint first used doc_paper, pure white, on the reasoning that a
# domain bar is drawn on the card. Then cream, on noticing the score badge is
# not on the card. Both were guesses at which surface binds, and both were
# wrong, because the binding surface is simply the DARKEST one: a light colour
# has least contrast against the darkest background, and so does a dark one
# against the lightest, but every identity member here is chosen for its
# lightness range rather than its darkness, so the dark surface governs.
#
# The surface is PER PALETTE, and that is accuracy rather than an exception.
# pal_effective_fill_id() forces High Contrast Mono whenever high contrast is
# on, so the four chromatic palettes are only ever drawn in STANDARD contrast
# and never meet the #e8e8e8 tint at all. Mono can appear in either mode, so it
# answers to the darker one. Holding everything to #e8e8e8 costs heritage 7.3
# units of drift, and buys a margin against a background it cannot encounter.
#
# The rule that generalises: enumerate every surface a colour lands on BEFORE
# searching, then take the darkest it can actually reach. Reasoning from the
# most common surface is how this got two wrong answers in one afternoon.
DOC_PAPER = "#ffffff"
DOC_CREAM = "#faf7f3"
DOC_TINT = "#f4efe9"
DOC_TINT_HIGH = "#e8e8e8"


def identity_surface(pid: str) -> str:
    return DOC_TINT_HIGH if pid == "mono_high" else DOC_TINT

# Today's triple, for reference in every report. This is what is shipping.
CURRENT = ("#c2410c", "#a16207", "#7b1717")

DOMAINS = ("cl", "cp", "we")


# ------------------------------------------------------------------------------
# Vectorised colour maths
# ------------------------------------------------------------------------------
# Every function takes and returns arrays with colour on the LAST axis, so the
# leading axes are free and the same code serves a single colour, a candidate
# list and a pairwise grid.

def srgb_to_linear(c: np.ndarray) -> np.ndarray:
    return np.where(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055) ** 2.4)


def linear_to_srgb(c: np.ndarray) -> np.ndarray:
    c = np.clip(c, 0.0, 1.0)
    return np.where(c <= 0.0031308, c * 12.92, 1.055 * c ** (1 / 2.4) - 0.055)


def rgb_to_lab(rgb: np.ndarray) -> np.ndarray:
    """(..., 3) sRGB in 0..1 -> (..., 3) CIELAB."""
    lin = srgb_to_linear(rgb)
    xyz = lin @ pa.M_RGB2XYZ.T
    t = xyz / pa.WHITE_D65
    d = (6 / 29) ** 3
    f = np.where(t > d, np.cbrt(np.maximum(t, 0.0)),
                 t / (3 * (6 / 29) ** 2) + 4 / 29)
    return np.stack([116 * f[..., 1] - 16,
                     500 * (f[..., 0] - f[..., 1]),
                     200 * (f[..., 1] - f[..., 2])], axis=-1)


def simulate_cvd(rgb: np.ndarray, kind: str) -> np.ndarray:
    """(..., 3) sRGB -> (..., 3) sRGB as seen with `kind`. 'normal' is a no-op.

    QUANTISED TO 8 BITS ON THE WAY OUT, and that is not a rounding nicety. The
    scalar reference in palette_audit.py returns a HEX STRING, so every
    measurement the rest of this repo makes, including the gate in
    check_palette.py, is taken on a CVD result that has been through 8-bit.
    Measuring in continuous space here instead makes this search disagree with
    the check that admits its answers, by up to a fifth of a CIEDE2000 unit.

    That is small and it is exactly the size that matters at a threshold: the
    first heritage triple this script proposed scored 20.0 here and 19.8 in the
    checker, and was rejected. A search must be scored by the same instrument
    that will judge it.
    """
    if kind == "normal":
        return rgb
    out = linear_to_srgb(srgb_to_linear(rgb) @ pa.CVD_MATRICES[kind].T)
    return np.round(np.clip(out, 0.0, 1.0) * 255.0) / 255.0


def relative_luminance(rgb: np.ndarray) -> np.ndarray:
    lin = srgb_to_linear(rgb)
    return lin @ np.array([0.2126, 0.7152, 0.0722])


def contrast_ratio(rgb_a: np.ndarray, rgb_b: np.ndarray) -> np.ndarray:
    la = relative_luminance(rgb_a)
    lb = relative_luminance(rgb_b)
    hi = np.maximum(la, lb)
    lo = np.minimum(la, lb)
    return (hi + 0.05) / (lo + 0.05)


def ciede2000(lab1: np.ndarray, lab2: np.ndarray) -> np.ndarray:
    """Broadcasting CIEDE2000. Arrays are (..., 3); the result is (...).

    A direct transcription of the scalar version in docs/assets/palette_audit.py
    with the three branch-on-scalar sections rewritten as np.where. The branches
    are the only place a vectorised port can quietly go wrong, which is what the
    self-test is for.
    """
    L1, a1, b1 = lab1[..., 0], lab1[..., 1], lab1[..., 2]
    L2, a2, b2 = lab2[..., 0], lab2[..., 1], lab2[..., 2]

    C1 = np.hypot(a1, b1)
    C2 = np.hypot(a2, b2)
    Cbar = (C1 + C2) / 2
    C7 = Cbar ** 7
    G = 0.5 * (1 - np.sqrt(C7 / (C7 + 25.0 ** 7)))

    a1p, a2p = (1 + G) * a1, (1 + G) * a2
    C1p, C2p = np.hypot(a1p, b1), np.hypot(a2p, b2)
    h1p = np.degrees(np.arctan2(b1, a1p)) % 360
    h2p = np.degrees(np.arctan2(b2, a2p)) % 360

    dLp = L2 - L1
    dCp = C2p - C1p

    prod_zero = (C1p * C2p) == 0
    dh = h2p - h1p
    dhp = np.where(prod_zero, 0.0,
                   np.where(np.abs(dh) <= 180, dh,
                            np.where(dh > 180, dh - 360, dh + 360)))
    dHp = 2 * np.sqrt(C1p * C2p) * np.sin(np.radians(dhp) / 2)

    Lbarp = (L1 + L2) / 2
    Cbarp = (C1p + C2p) / 2

    hsum = h1p + h2p
    hbarp = np.where(prod_zero, hsum,
                     np.where(np.abs(h1p - h2p) <= 180, hsum / 2,
                              np.where(hsum < 360, (hsum + 360) / 2,
                                       (hsum - 360) / 2)))

    T = (1
         - 0.17 * np.cos(np.radians(hbarp - 30))
         + 0.24 * np.cos(np.radians(2 * hbarp))
         + 0.32 * np.cos(np.radians(3 * hbarp + 6))
         - 0.20 * np.cos(np.radians(4 * hbarp - 63)))

    dTheta = 30 * np.exp(-(((hbarp - 275) / 25) ** 2))
    Cbarp7 = Cbarp ** 7
    RC = 2 * np.sqrt(Cbarp7 / (Cbarp7 + 25.0 ** 7))
    SL = 1 + (0.015 * (Lbarp - 50) ** 2) / np.sqrt(20 + (Lbarp - 50) ** 2)
    SC = 1 + 0.045 * Cbarp
    SH = 1 + 0.015 * Cbarp * T
    RT = -np.sin(np.radians(2 * dTheta)) * RC

    return np.sqrt(np.maximum(
        (dLp / SL) ** 2 + (dCp / SC) ** 2 + (dHp / SH) ** 2
        + RT * (dCp / SC) * (dHp / SH), 0.0))


def lab_to_lch(lab: np.ndarray) -> np.ndarray:
    """(..., 3) CIELAB -> (..., 3) CIELCh, hue in degrees 0..360."""
    C = np.hypot(lab[..., 1], lab[..., 2])
    h = np.degrees(np.arctan2(lab[..., 2], lab[..., 1])) % 360
    return np.stack([lab[..., 0], C, h], axis=-1)


def hexes_to_rgb(hexes) -> np.ndarray:
    return np.array([pa.hex_to_rgb(h) for h in hexes])


def rgb_to_hexes(rgb: np.ndarray) -> list[str]:
    arr = np.atleast_2d(rgb)
    return [pa.rgb_to_hex(row) for row in arr]


# ------------------------------------------------------------------------------
# Separation
# ------------------------------------------------------------------------------

def lab_by_vision(rgb: np.ndarray) -> dict[str, np.ndarray]:
    """CIELAB for each vision type, so the CVD simulation is done once."""
    return {v: rgb_to_lab(simulate_cvd(rgb, v)) for v in VISION}


def pairwise_separation(labs: dict[str, np.ndarray],
                        block: int = 512) -> np.ndarray:
    """(N, N) worst-across-vision-types CIEDE2000, computed in row blocks.

    Blocked because the intermediate arrays inside ciede2000() are the memory
    cost, not the result: a 2000-candidate run holds about twenty (N, N)
    temporaries at once, which is 640 MB unblocked and 160 MB at this block
    size. The result is float32; the search compares distances against a floor
    of 15 and does not need the last few digits.
    """
    n = labs[VISION[0]].shape[0]
    out = np.empty((n, n), dtype=np.float32)
    for start in range(0, n, block):
        stop = min(start + block, n)
        worst = None
        for v in VISION:
            lab = labs[v]
            d = ciede2000(lab[start:stop, None, :], lab[None, :, :])
            worst = d if worst is None else np.minimum(worst, d)
        out[start:stop] = worst.astype(np.float32)
    return out


def triple_separation(hexes) -> dict[str, float]:
    """Per-vision-type worst pair, plus the overall worst, for one triple."""
    rgb = hexes_to_rgb(hexes)
    res = {}
    for v in VISION:
        lab = rgb_to_lab(simulate_cvd(rgb, v))
        res[v] = float(min(ciede2000(lab[i], lab[j])
                           for i, j in itertools.combinations(range(3), 2)))
    res["worst"] = float(min(res[v] for v in VISION))
    return res


# ------------------------------------------------------------------------------
# Candidate generation and constraints
# ------------------------------------------------------------------------------
# Two axes, which together are the whole of the step 2.1 decision.
#
# TEXT MODE. Whether every member must carry WHITE text at AA, or whether each
# member may carry the better of white and near-black. The second is looser by
# exactly one thing: it lets a member be a LIGHT chip with dark text, which
# hands back most of the lightness range. Section 4 of the build plan is blunt
# that lightness is where separation actually comes from once hue is spent, so
# this is the single highest-leverage constraint in the search.
#
# HUE MODE. How tightly identity colour must track the palette it belongs to.
#   family  hue within the palette ramp's own arc, widened 15 degrees each way.
#           A Viridis scorecard looks like Viridis. For an achromatic ramp this
#           degenerates to "stay achromatic", which is the honest reading of
#           what Mono's family is.
#   loose   the same arc widened 45 degrees each way. Recognisably related.
#   free    no hue constraint at all. Maximum separation, no family resemblance.
#
# There is no "warm only" mode, though section 4 measured one. Warm-only is what
# `family` MEANS for heritage, and hardcoding it for the other four would ask
# Cividis to be warm, which is not a question anyone wants answered.

def gamut_grid(step: int) -> np.ndarray:
    levels = np.unique(np.clip(np.arange(0, 256 + step, step), 0, 255))
    g = np.array(np.meshgrid(levels, levels, levels, indexing="ij"))
    return g.reshape(3, -1).T / 255.0


def ramp_hue_arc(fills: list[str], chroma_floor: float = 8.0):
    """(lo, hi) hue arc of a ramp's chromatic stops, or None if achromatic.

    Stops below the chroma floor are dropped before the arc is taken, because a
    near-neutral has a hue angle that is numerically defined and perceptually
    meaningless; including one would swing the arc by a hundred degrees on the
    strength of a colour nobody would call coloured.
    """
    lch = lab_to_lch(rgb_to_lab(hexes_to_rgb(fills)))
    hues = lch[lch[:, 1] >= chroma_floor][:, 2]
    if hues.size == 0:
        return None
    # Unwrap around the circle so an arc spanning 0 degrees is not read as
    # spanning the whole wheel. Anchored on the first chromatic stop.
    ref = hues[0]
    rel = (hues - ref + 180) % 360 - 180
    return (ref + rel.min(), ref + rel.max())


def hue_mask(rgb: np.ndarray, arc, widen: float,
             achromatic_ceiling: float = 10.0) -> np.ndarray:
    """True where a candidate is inside the widened arc, or near-neutral."""
    lch = lab_to_lch(rgb_to_lab(rgb))
    C, h = lch[..., 1], lch[..., 2]
    # A near-neutral is admitted by every hue mode. Black, white and grey have
    # no family and belong to all of them, and excluding them would be the
    # search refusing to consider the very colours section 4 says win.
    neutral = C < achromatic_ceiling
    if arc is None:
        return neutral
    lo, hi = arc[0] - widen, arc[1] + widen
    if hi - lo >= 360:
        return np.ones(rgb.shape[0], dtype=bool)
    rel = (h - lo) % 360
    return neutral | (rel <= (hi - lo))


def text_mask(rgb: np.ndarray, mode: str,
              surface: str = DOC_TINT) -> tuple[np.ndarray, np.ndarray]:
    """Admissible candidates and the ink each one takes.

    Returns (mask, ink) where ink is 0 for white text and 1 for near-black.
    The non-text floor against paper applies in BOTH modes; see the note on
    MIN_NONTEXT_CONTRAST for why it is not part of the mode question.
    """
    w = contrast_ratio(rgb, pa.hex_to_rgb(WHITE))
    k = contrast_ratio(rgb, pa.hex_to_rgb(NEAR_BLACK))
    visible = (contrast_ratio(rgb, pa.hex_to_rgb(surface))
               >= MIN_NONTEXT_CONTRAST)
    if mode == "white_only":
        return (w >= MIN_TEXT_CONTRAST) & visible, np.zeros(rgb.shape[0], dtype=int)
    if mode == "better_of_two":
        return ((np.maximum(w, k) >= MIN_TEXT_CONTRAST) & visible,
                (k > w).astype(int))
    raise ValueError(f"unknown text mode {mode!r}")


HUE_MODES = {"family": 15.0, "loose": 45.0, "free": 360.0}
TEXT_MODES = ("white_only", "better_of_two")

# Separation targets to price. 15 is the repo's working floor; the rest buy
# margin. Past about 30 the answer stops resembling any palette at all.
TARGETS = (15.0, 20.0, 25.0, 30.0)

# How many nearest-to-ramp candidates the coherence search considers. A cap,
# not a constraint on the answer: it is the search saying that a colour 60 units
# off the ramp is not a candidate for "closest to the ramp" and need not be
# carried through an N-squared distance matrix. Raise it if a target reports no
# solution and you doubt that.
COHERENCE_POOL = 1200


# ------------------------------------------------------------------------------
# Coherence: distance from the palette's own ramp
# ------------------------------------------------------------------------------

def ramp_path(fills: list[str], n: int = 96) -> np.ndarray:
    """The ramp as a dense path through CIELAB, not four points.

    Interpolated linearly between consecutive stops, because what makes a colour
    look like it belongs to a palette is sitting ON the palette's path, not
    sitting near one of the four values that happened to be sampled from it. A
    mid gold between stops one and two is recognisably heritage; measuring only
    against the four stops would score it as a miss.
    """
    lab = rgb_to_lab(hexes_to_rgb(fills))
    t = np.linspace(0, len(lab) - 1, n)
    i = np.clip(np.floor(t).astype(int), 0, len(lab) - 2)
    f = (t - i)[:, None]
    return lab[i] * (1 - f) + lab[i + 1] * f


def ramp_distance(rgb: np.ndarray, path: np.ndarray,
                  block: int = 1024) -> np.ndarray:
    """(N,) CIEDE2000 from each candidate to the nearest point on the path.

    Normal vision only, deliberately. Coherence is a question about whether the
    colour looks like it belongs to the palette, which is a question about the
    design as drawn. Separation is the property that has to survive colour
    vision deficiency, and it is measured separately and does.
    """
    lab = rgb_to_lab(rgb)
    out = np.empty(lab.shape[0], dtype=np.float32)
    for s in range(0, lab.shape[0], block):
        e = min(s + block, lab.shape[0])
        d = ciede2000(lab[s:e, None, :], path[None, :, :])
        out[s:e] = d.min(axis=1).astype(np.float32)
    return out


def cheapest_coherent_triple(sep: np.ndarray, dist: np.ndarray,
                             target: float):
    """Triple meeting `target` separation whose WORST member is closest to the ramp.

    Binary search on the coherence radius. Worst member rather than mean, for
    the same reason the separation score is worst-pair: a triple with two
    perfect members and one that has wandered off reads as a mistake, not as a
    compromise.

    Each probe is an EXISTENCE test only, not best_triple(). Maximising
    separation inside the probe was the first version and it made the search
    quadratically slower for an answer that gets thrown away at every radius
    except the winning one, where it is computed once at the end.
    """
    radii = np.unique(dist)
    lo, hi = 0, radii.size - 1
    found_radius = None
    while lo <= hi:
        mid = (lo + hi) // 2
        keep = np.flatnonzero(dist <= radii[mid])
        ok = (keep.size >= 3
              and find_triangle(sep[np.ix_(keep, keep)], target) is not None)
        if ok:
            found_radius = float(radii[mid])
            hi = mid - 1
        else:
            lo = mid + 1
    if found_radius is None:
        return None
    keep = np.flatnonzero(dist <= found_radius)
    idx, score = best_triple(sep[np.ix_(keep, keep)], floor=target)
    if idx is None:
        return None
    return ([int(keep[i]) for i in idx], score, found_radius)


# ------------------------------------------------------------------------------
# The search
# ------------------------------------------------------------------------------

def find_triangle(sep: np.ndarray, t: float):
    """Any index triple whose three pairwise separations all reach `t`, or None.

    The k-core reduction before the matrix multiply is what makes this quick. A
    node with fewer than two neighbours cannot be in any triangle, and removing
    such nodes creates more of them, so the loop repeats until it stops paying.
    At a useful threshold that takes a thousand-node graph down to a few dozen.

    The multiply itself counts two-step paths; a two-step path from i to j that
    is also a direct edge is a triangle. Done in float32 because BLAS is what
    makes this fast and boolean matrices do not reach BLAS.
    """
    n = sep.shape[0]
    if n < 3:
        return None
    keep = np.arange(n)
    adj = sep >= t
    np.fill_diagonal(adj, False)
    while True:
        alive = adj.sum(axis=1) >= 2
        if alive.all():
            break
        if alive.sum() < 3:
            return None
        keep = keep[alive]
        adj = adj[np.ix_(alive, alive)]
    if keep.size < 3:
        return None
    f = adj.astype(np.float32)
    hits = np.argwhere((f @ f) * adj > 0)
    if hits.size == 0:
        return None
    i, j = hits[0]
    common = np.flatnonzero(adj[i] & adj[j])
    return (int(keep[i]), int(keep[j]), int(keep[common[0]]))


def best_triple(sep: np.ndarray, floor: float = 0.0):
    """Index triple maximising the worst pairwise separation. Exact over the set.

    Binary search on the threshold over the observed distance values. Exact for
    the candidate set it is given, which is the honest claim; a grid search is
    exact over its grid and no finer than its step.
    """
    n = sep.shape[0]
    if n < 3:
        return None, 0.0
    vals = np.unique(sep[np.triu_indices(n, k=1)])
    vals = vals[vals >= floor]
    if vals.size == 0:
        return None, 0.0

    best = find_triangle(sep, vals[0])
    if best is None:
        return None, 0.0
    lo, hi, best_val = 0, vals.size - 1, vals[0]
    while lo < hi:
        mid = (lo + hi + 1) // 2
        got = find_triangle(sep, vals[mid])
        if got is None:
            hi = mid - 1
        else:
            lo, best, best_val = mid, got, vals[mid]
    return best, float(best_val)


def search_coherent(fills: list[str], text_mode: str, step: int,
                    surface: str = DOC_TINT) -> dict:
    """Price each separation target in units of drift from the palette's ramp.

    One candidate pool, one distance vector and one separation matrix, reused
    across every target. The targets differ only in where the triangle test is
    thresholded, so recomputing the expensive parts per target would be pure
    waste.
    """
    rgb = gamut_grid(step)
    tmask, ink = text_mask(rgb, text_mode, surface)
    rgb, ink = rgb[tmask], ink[tmask]

    path = ramp_path(fills)
    dist = ramp_distance(rgb, path)
    if rgb.shape[0] > COHERENCE_POOL:
        keep = np.argpartition(dist, COHERENCE_POOL)[:COHERENCE_POOL]
        rgb, ink, dist = rgb[keep], ink[keep], dist[keep]

    sep = pairwise_separation(lab_by_vision(rgb))

    rows = []
    for target in TARGETS:
        got = cheapest_coherent_triple(sep, dist, target)
        if got is None:
            rows.append({"target": target, "ok": False})
            continue
        idx, score, radius = got
        # Reported light to dark so the three are listed in a stable order and
        # two runs can be compared by eye. This is NOT the domain assignment;
        # see the open question on positional versus hue mapping in the plan.
        idx = sorted(idx, key=lambda i: -float(rgb_to_lab(rgb[i])[0]))
        rows.append({
            "target": target, "ok": True,
            "hex": rgb_to_hexes(rgb[idx]),
            "ink": [WHITE if ink[i] == 0 else NEAR_BLACK for i in idx],
            "score": score,
            "radius": radius,
            "each": [float(dist[i]) for i in idx],
        })
    return {"rows": rows, "pool": int(rgb.shape[0])}


def search_cell(fills: list[str], hue_mode: str, text_mode: str,
                step: int) -> dict | None:
    """One cell of the frontier: best triple under one constraint combination."""
    rgb = gamut_grid(step)
    arc = ramp_hue_arc(fills)
    keep = hue_mask(rgb, arc, HUE_MODES[hue_mode])
    tmask, ink = text_mask(rgb, text_mode)  # ceiling mode: paper-agnostic
    keep &= tmask
    if keep.sum() < 3:
        return None
    cand = rgb[keep]
    cand_ink = ink[keep]

    sep = pairwise_separation(lab_by_vision(cand))
    idx, score = best_triple(sep)
    if idx is None:
        return None
    return {
        "rgb": cand[list(idx)],
        "hex": rgb_to_hexes(cand[list(idx)]),
        "ink": [WHITE if cand_ink[i] == 0 else NEAR_BLACK for i in idx],
        "score": score,
        "n_candidates": int(keep.sum()),
    }


def refine(result: dict, fills: list[str], hue_mode: str, text_mode: str,
           step: int, radius: int) -> dict:
    """Coordinate ascent: move one member at a time over a local grid.

    Not a joint fine search, which would be step^9 and pointless. Moving one
    member while the other two are fixed is a strict improvement or no change at
    every accepted step, so this terminates, and it recovers most of what a
    coarse grid gives up. It is a LOCAL optimum by construction and the report
    says so rather than implying the coarse figure was the global one.
    """
    arc = ramp_hue_arc(fills)
    cur = np.array(result["rgb"], dtype=float)
    cur_ink = list(result["ink"])
    best_score = result["score"]

    offsets = np.arange(-radius, radius + 1, step) / 255.0
    grid = np.array(np.meshgrid(offsets, offsets, offsets,
                                indexing="ij")).reshape(3, -1).T

    improved = True
    while improved:
        improved = False
        for m in range(3):
            cand = np.clip(cur[m] + grid, 0.0, 1.0)
            cand = np.unique(np.round(cand * 255).astype(int), axis=0) / 255.0
            keep = hue_mask(cand, arc, HUE_MODES[hue_mode])
            tmask, ink = text_mask(cand, text_mode)
            keep &= tmask
            if not keep.any():
                continue
            cand, ink = cand[keep], ink[keep]

            others = np.array([cur[i] for i in range(3) if i != m])
            worst = None
            for v in VISION:
                lab_c = rgb_to_lab(simulate_cvd(cand, v))
                lab_o = rgb_to_lab(simulate_cvd(others, v))
                d = np.minimum(ciede2000(lab_c, lab_o[0]),
                               ciede2000(lab_c, lab_o[1]))
                # The pair between the two FIXED members caps the triple, so it
                # belongs in the score being maximised, not outside it.
                d_fixed = ciede2000(lab_o[0], lab_o[1])
                d = np.minimum(d, d_fixed)
                worst = d if worst is None else np.minimum(worst, d)
            k = int(np.argmax(worst))
            if worst[k] > best_score + 1e-9:
                cur[m] = cand[k]
                cur_ink[m] = WHITE if ink[k] == 0 else NEAR_BLACK
                best_score = float(worst[k])
                improved = True

    return {
        "rgb": cur,
        "hex": rgb_to_hexes(cur),
        "ink": cur_ink,
        "score": best_score,
        "n_candidates": result["n_candidates"],
        "refined": True,
    }


# ------------------------------------------------------------------------------
# The COL_TXT question
# ------------------------------------------------------------------------------

def text_form_frontier(triple_hex: list[str], bg: str = DOC_TINT) -> dict:
    """Can the triple survive being turned into TEXT on the summary table tint?

    Not part of steps 2.1 to 2.5 as written, and it should be. Step 3.1 asks
    state_scorecard.qmd's COL_TXT to read pal_identity(), but COL_TXT is not the
    identity colour: it is a DARKENED form of it, because the fill is a bar and
    the text form is a label on a warm tint. The catalogue as specified in 2.3
    carries `identity` and `on_identity` and has nothing to offer COL_TXT.

    This is the same shape as the on_white finding recorded in the ROLES note in
    scripts/palettes.R, so it is measured the same way: take each member, hold
    its hue and chroma, and walk L* down until it clears AA on the tint. Then
    report what separation survives. If it collapses, the answer is that the
    text form stops being per-domain, exactly as on_white did.
    """
    bg_rgb = pa.hex_to_rgb(bg)
    out = []
    for h in triple_hex:
        lab = rgb_to_lab(pa.hex_to_rgb(h))
        lch = lab_to_lch(lab)
        C, hue = float(lch[1]), float(lch[2])
        chosen = None
        for L in np.arange(float(lch[0]), -0.5, -0.5):
            a = C * np.cos(np.radians(hue))
            b = C * np.sin(np.radians(hue))
            rgb = lab_to_rgb(np.array([L, a, b]))
            if rgb is None:
                continue
            if contrast_ratio(rgb, bg_rgb) >= MIN_TEXT_CONTRAST:
                chosen = rgb
                break
        out.append(chosen if chosen is not None else pa.hex_to_rgb(NEAR_BLACK))
    arr = np.array(out)
    labs = lab_by_vision(arr)
    worst = min(
        float(ciede2000(labs[v][i], labs[v][j]))
        for v in VISION for i, j in itertools.combinations(range(3), 2))
    return {"hex": rgb_to_hexes(arr), "worst": worst}


def lab_to_rgb(lab: np.ndarray):
    """CIELAB -> sRGB, or None if the colour is outside the sRGB gamut.

    Out-of-gamut is returned as None rather than clipped. Clipping would move
    the colour to a different hue and chroma and then report a separation for a
    colour the search never actually proposed.
    """
    fy = (lab[0] + 16) / 116
    fx = fy + lab[1] / 500
    fz = fy - lab[2] / 200
    d = 6 / 29

    def finv(t):
        return t ** 3 if t > d else 3 * d * d * (t - 4 / 29)

    xyz = np.array([finv(fx), finv(fy), finv(fz)]) * pa.WHITE_D65
    lin = np.linalg.inv(pa.M_RGB2XYZ) @ xyz
    if np.any(lin < -1e-6) or np.any(lin > 1 + 1e-6):
        return None
    return linear_to_srgb(lin)


# ------------------------------------------------------------------------------
# Self-test
# ------------------------------------------------------------------------------

def self_test() -> int:
    rng = np.random.default_rng(20260803)
    sample = rng.integers(0, 256, size=(400, 3)) / 255.0

    lab_vec = rgb_to_lab(sample)
    lab_ref = np.array([pa.rgb_to_lab(row) for row in sample])
    e_lab = float(np.abs(lab_vec - lab_ref).max())

    a, b = sample[:200], sample[200:]
    de_vec = ciede2000(rgb_to_lab(a), rgb_to_lab(b))
    de_ref = np.array([pa.ciede2000(pa.rgb_to_lab(x), pa.rgb_to_lab(y))
                       for x, y in zip(a, b)])
    e_de = float(np.abs(de_vec - de_ref).max())

    hexes = [pa.rgb_to_hex(r) for r in sample]
    e_cvd = 0.0
    for kind in ("protanopia", "deuteranopia", "tritanopia"):
        vec = simulate_cvd(sample, kind)
        ref = hexes_to_rgb([pa.simulate_cvd(h, kind) for h in hexes])
        # Compared after the round trip through 8-bit hex, which is what the
        # reference returns, so this checks agreement at the precision the
        # reference actually has.
        e_cvd = max(e_cvd, float(np.abs(hexes_to_rgb(rgb_to_hexes(vec))
                                        - ref).max()))

    cr_vec = contrast_ratio(sample, pa.hex_to_rgb(WHITE))
    cr_ref = np.array([pa.contrast_ratio(h, WHITE) for h in hexes])
    e_cr = float(np.abs(cr_vec - cr_ref).max())

    print("Self-test against docs/assets/palette_audit.py, 400 random colours")
    print(f"  CIELAB           max abs diff  {e_lab:.3e}")
    print(f"  CIEDE2000        max abs diff  {e_de:.3e}")
    print(f"  CVD simulation   max abs diff  {e_cvd:.3e}   (8-bit round trip)")
    print(f"  contrast ratio   max abs diff  {e_cr:.3e}")

    bad = [n for n, e in (("CIELAB", e_lab), ("CIEDE2000", e_de),
                          ("contrast", e_cr)) if e > 1e-9]
    if e_cvd > 1.5 / 255:
        bad.append("CVD")
    if bad:
        print(f"\nFAIL: {', '.join(bad)} disagree with the scalar reference.")
        return 1
    print("\nPASS: vectorised maths matches the scalar reference.")
    return 0


# ------------------------------------------------------------------------------
# Report
# ------------------------------------------------------------------------------

def describe(hexes: list[str]) -> str:
    lch = lab_to_lch(rgb_to_lab(hexes_to_rgb(hexes)))
    return "  ".join(f"L{l:3.0f} C{c:3.0f} h{h:3.0f}" for l, c, h in lch)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--self-test", action="store_true")
    ap.add_argument("--step", type=int, default=32,
                    help="coarse sRGB grid step, default 32")
    ap.add_argument("--fine", action="store_true",
                    help="run the coordinate-ascent refinement")
    ap.add_argument("--ceiling", action="store_true",
                    help="also report the max-separation frontier, which is "
                         "the unusable upper bound; see the header note")
    ap.add_argument("--palette", default=None, help="limit to one palette id")
    args = ap.parse_args()

    if args.self_test:
        return self_test()

    maps = parse_catalogue(ROOT / "scripts" / "palettes.R")
    if args.palette:
        maps = {k: v for k, v in maps.items() if k == args.palette}

    print("=" * 78)
    print("ADA-PARC IDENTITY TRIPLE SEARCH")
    print("Phase 2.2 of docs/palette-v2-build-plan.qmd")
    print("=" * 78)
    print()
    print(f"Score = worst pairwise CIEDE2000 across {', '.join(VISION)}.")
    print(f"Floor = {MIN_SEPARATION:.0f}. Grid step {args.step}"
          f"{' plus refinement' if args.fine else ''}.")
    print()

    cur = triple_separation(CURRENT)
    print("SHIPPING TODAY, for reference")
    print(f"  {'  '.join(CURRENT)}")
    print(f"  {describe(list(CURRENT))}")
    for v in VISION:
        print(f"  {v:<14} worst pair {cur[v]:5.1f}")
    print(f"  {'OVERALL':<14} worst pair {cur['worst']:5.1f}"
          f"   {'PASS' if cur['worst'] >= MIN_SEPARATION else 'FAIL'}")
    print()

    for pid, entry in maps.items():
        fills = [entry["fill"][t]
                 for t in ("poor", "below", "above", "excellent")]
        print("-" * 78)
        print(f"{entry['label']}  ({pid})   ramp {' '.join(fills)}")
        print("-" * 78)

        if args.ceiling:
            arc = ramp_hue_arc(fills)
            for text_mode in TEXT_MODES:
                for hue_mode in HUE_MODES:
                    res = search_cell(fills, hue_mode, text_mode, args.step)
                    if res is None:
                        print(f"  ceiling  {text_mode:<14} {hue_mode:<7} "
                              f"no admissible triple")
                        continue
                    print(f"  ceiling  {text_mode:<14} {hue_mode:<7} "
                          f"{res['score']:5.1f}  {' '.join(res['hex'])}")
            print()

        for text_mode in TEXT_MODES:
            out = search_coherent(fills, text_mode, args.step,
                                  identity_surface(pid))
            print(f"  text mode: {text_mode}   (pool {out['pool']})")
            for row in out["rows"]:
                if not row["ok"]:
                    print(f"    target {row['target']:4.0f}   "
                          f"NO TRIPLE within the candidate pool")
                    continue
                inks = "".join("w" if i == WHITE else "k" for i in row["ink"])
                print(f"    target {row['target']:4.0f}   "
                      f"drift {row['radius']:5.1f}   "
                      f"actual sep {row['score']:5.1f}   ink {inks}   "
                      f"{' '.join(row['hex'])}")
                print(f"    {'':>11}   per member drift "
                      f"{', '.join(f'{d:.0f}' for d in row['each'])}")
                txt = text_form_frontier(row["hex"])
                print(f"    {'':>11}   as text on the summary tint: "
                      f"{' '.join(txt['hex'])}  worst pair {txt['worst']:5.1f}")
            print()

    print("=" * 78)
    print("READING THIS")
    print()
    print("  drift       CIEDE2000 from the worst member to the palette's own")
    print("              ramp. This is the price of the target, in the units")
    print("              the rest of the repo argues in. Under about 12 reads")
    print("              as the same family; over about 25 does not.")
    print("  actual sep  what the chosen triple achieves, which can exceed the")
    print("              target when the cheapest coherent triple is generous.")
    print("  ink         w or k per member, the better of white and near-black")
    print("              at AA. A k is a LIGHT chip, which is the constraint")
    print("              section 4 of the plan says buys back lightness range.")
    print()
    print("This is a frontier, not a recommendation. Step 2.1 of the build plan")
    print("is the decision about which row to stand on, and it is not the")
    print("search's to make.")
    print("=" * 78)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
