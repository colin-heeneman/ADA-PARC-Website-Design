# =============================================================================
# palettes.R
#
# SINGLE SOURCE OF TRUTH for ADA-PARC tier colour.
#
# Every surface that colours a performance tier (national map, city tract map,
# state scorecard, category scorecard, fact sheets) reads its colours from
# here. Nothing downstream should carry a tier hex literal. Where a file is
# static CSS and cannot call R, it mirrors these values and is verified by
#
#     python3 scripts/check_palette.py
#
# which fails loudly if any consumer has drifted. Run that before deploy.
#
# -----------------------------------------------------------------------------
# ADA-PARC HERITAGE, adopted 2026-07-28
# -----------------------------------------------------------------------------
# Replaces the previous ramp c("#f7e49c", "#fa8e57", "#c41306", "#630801") on
# the map and c("#e8c84a", "#fa8e57", "#c41306", "#630801") on the scorecards.
#
# WHY IT CHANGED. The previous scorecard ramp separated its two lowest tiers by
# only 7.7 CIEDE2000 units under simulated deuteranopia, against a working
# floor of about 15 for confident discrimination of adjacent filled regions.
# Roughly 8 percent of men have a red-green colour vision deficiency, of which
# deuteranomaly is the largest share, so "Poor" and "Below Average" were close
# to the same colour for a substantial part of the audience. Heritage clears 15
# under normal vision and under all three simulated deficiencies.
#
# HOW IT WAS CONSTRUCTED. In CIELCh, holding the gold-to-maroon hue arc that
# carries ADA-PARC identity while making the lightness steps even:
#
#     L* = 86, 62, 38, 15    C* = 45, 60, 62, 45    h = 82, 62, 40, 34
#
# Even L* steps are what make the ramp safe. Because lightness carries the
# ordering, the palette survives any colour vision deficiency, greyscale
# printing and glare, and the "neg" indicator reversal in render_national_map()
# stays correct without special casing.
#
# See docs/color-palette-expansion.qmd for the full analysis and
# docs/assets/palette_audit.py for the measurements.
#
# -----------------------------------------------------------------------------
# ORDERING CONVENTION
# -----------------------------------------------------------------------------
# All four-element vectors run WORST to BEST, which is also LIGHT to DARK:
#
#     [1] Poor          lightest
#     [2] Below Average
#     [3] Above Average
#     [4] Excellent     darkest
#
# This matches the site-wide rule that darker always means more opportunity for
# people with disabilities. Do not reorder these vectors. If an indicator runs
# the other way, reverse at the point of use, as render_national_map() does.
#
# No package dependencies, by design. This file is sourced from the Shiny app
# and from Quarto subprocesses that render with a minimal library set.
# =============================================================================


# =============================================================================
# THE CATALOGUE
# =============================================================================
# ONE list, five entries. Restructured 2026-08-03, phase 1 of
# docs/palette-v1-build-plan.qmd, from the two lists that stood here before:
# ADA_PARC_PALETTE, the brand palette that applied everywhere and was not
# selectable, and MAP_PALETTES, which was selectable but applied to maps only.
# Nothing downstream could carry a reader's choice past a map while those were
# separate objects with different role names.
#
# -----------------------------------------------------------------------------
# ROLES
# -----------------------------------------------------------------------------
# Every entry carries these four, so any entry can colour any fill surface:
#
#   label       Display name for the palette selector.
#   note        One-line plain-language description, shown beside the selector.
#   fill        The four tier fills, light to dark. Map bins, gauge arcs, tier
#               chips, tier table headers, ranking table score cells.
#   on_fill     Text drawn ON the matching fill. Each is the better of
#               near-black and white and clears WCAG 2.2 SC 1.4.3 AA (4.5:1).
#               Never pick one of these by eye; read it from here. Verified by
#               check_palette.py for every entry, not just heritage.
#   identity    The three DOMAIN colours: cl, cp, we. See IDENTITY below.
#   on_identity Text drawn ON the matching identity colour.
#   na          No-data fill for MAP BINS.
#
# -----------------------------------------------------------------------------
# IDENTITY, AND WHY IT IS A SEPARATE ROLE FROM fill
# -----------------------------------------------------------------------------
# Added 2026-08-03, phase 2 of docs/palette-v2-build-plan.qmd.
#
# `fill` is ORDINAL. Its four values mean worse to better and the ordering is
# carried by lightness, which is why the ramp survives greyscale and why the
# "neg" reversal in render_national_map() is a simple flip.
#
# `identity` is CATEGORICAL. Nothing about Community Living is more or less than
# Work & Economic. The only property that matters between the three is that they
# are TELLABLE APART, and nothing about them should suggest a sequence. Putting
# a categorical role and an ordinal one on the same three values, which is what
# reusing the ramp would do, is the confusion the build plan records the category
# scorecard already making in a different form: banding by rank and banding by
# score quartile side by side on one page.
#
# WHY IT FOLLOWS THE READER'S SELECTION AND on_white DOES NOT. These look like
# the same question and they are not. on_white is TEXT: it is forced dark by
# contrast, and once three or four warm colours are all dark the hue differences
# that separated them are gone, which is measured above. identity is FILL. It
# keeps its lightness range, so it keeps its separation, so it can be authored
# per palette. A reader who chooses Viridis gets a Viridis scorecard.
#
# ASSIGNMENT IS POSITIONAL, LIGHT TO DARK: cl, cp, we. Not by hue. Holding hue
# meaning across palettes is only possible where the palettes share hues, and
# Viridis has no maroon, so the rule would break exactly where it was meant to
# help. Positional is also the rule `fill` already uses, so one convention
# governs both.
#
# WHAT identity DOES NOT COVER. The domain label as TEXT on the summary table
# tint, which used to be dom_cl_txt and friends. That role collapses for the
# same measured reason on_white does: darkening the three to clear AA on the
# tint takes heritage's separation to 2.5 and High Contrast Mono's to 0.2. So
# the label text is plain doc_ink and the identity colour appears beside it as a
# swatch or rule. Colour stays reinforcement; the domain name is always printed.
#
# heritage additionally carries five BRAND-ONLY roles, which are deliberately
# NOT per-palette:
#
#   on_white  The tier expressed as TEXT on white or on the warm table tint.
#   na_fill   No-data background for tier chips and fact sheet table swatches.
#   na_text   Text on na_fill.
#   na_accent Stands in for on_white where a rank is NA.
#   stroke    Polygon and legend-key outline on maps.
#
# WHY on_white DOES NOT FOLLOW THE SELECTION. This was measured, not assumed.
# Deriving an on_white ramp for each palette, by darkening each stop at its own
# hue until it clears 4.5:1 on white and on the summary table tint, collapses
# every one of them: minimum adjacent separation comes out at 2.2 CIEDE2000 for
# heritage, 0.8 for Blue-Gold, 1.1 for Cividis, 7.3 for Viridis, and High
# Contrast Mono has no solution at all because a greyscale ramp has no hue to
# preserve and its lightest stop is white. The contrast requirement forces all
# four stops into roughly thirty points of lightness, and once they are all
# dark the hue differences that separated them as fills are gone.
#
# So on_white stays fixed at the heritage ramp whichever palette is selected.
# That is defensible because the tier NAME is always printed beside the colour,
# so the text colour is reinforcement and never the sole carrier of meaning
# (WCAG 2.2 SC 1.4.1). It also keeps the rank pill visually stable while fills
# change, which reads as more deliberate than every element shifting at once.
# The full measurements are in docs/palette-v1-build-plan.qmd section 1.2.
#
# -----------------------------------------------------------------------------
# ADMISSION CRITERIA
# -----------------------------------------------------------------------------
# Every entry must clear these, and is verified by
# `python3 scripts/check_palette.py`:
#   - adjacent-bin separation >= 15 CIEDE2000 under normal vision AND under
#     simulated protanopia, deuteranopia and tritanopia
#   - monotonic CIELAB L*, light to dark, so the "neg" indicator reversal in
#     render_national_map() stays correct without special casing
#   - every stop able to carry either near-black or white text at >= 4.5:1,
#     which is what `on_fill` records
#   - the no-data colour >= 15 CIEDE2000 from every stop in its own ramp
#
# And for `identity`, added 2026-08-03:
#   - PAIRWISE separation >= 20 CIEDE2000 under normal vision AND under all
#     three simulated deficiencies. Higher than the ramp's 15 because the three
#     domains sit side by side as peers rather than in a ranked sequence, so a
#     reader has no ordering to fall back on when two look alike.
#   - each member carries the better of near-black and white at >= 4.5:1, which
#     is what `on_identity` records
#   - each member >= 3:1 against doc_paper, WCAG 2.2 SC 1.4.11. An identity
#     colour is a BAR whose length has to be readable and a card border, not
#     only a background for its own label. This is the constraint that keeps
#     mono_high's cl off the ramp's own #ffffff.
#   - monotonic L*, cl light to we dark, because the assignment is positional
#
# The five triples were chosen by scripts/search_identity.py, which prices each
# separation target in CIEDE2000 drift from the palette's own ramp, and reports
# a frontier rather than an answer. The target of 20 was chosen over 25 because
# 25 costs heritage 14 units of drift and forces mono_high to become chromatic.
# The run is in docs/identity-frontier.txt and the visual check, under all four
# vision types, is docs/identity-candidates.html.
#
# The four non-heritage entries were adopted 2026-07-30, replacing six non-brand
# options that shipped before (Purple Gradient, Yellow-Orange, Yellow-Orange-Red,
# Grayscale Gradient, Teal-Blue Gradient, Green-Yellow Gradient). Every one of
# those either failed or sat marginal against the separation floor; the three
# ColorBrewer four-class ramps in particular compress into the light half of the
# lightness range and collapse under deuteranopia. Measurements and the
# retirement argument are in docs/color-palette-expansion.qmd section 2.
#
# Burnt Orange, a single-hue ramp at CIELCh h = 52, was offered on 2026-07-30
# and removed the same day at the client's request. It measured well (min 18.5,
# the best of the brand-adjacent options) and its definition is kept in
# docs/assets/palette_audit.py should it ever be wanted back. Its removal leaves
# the catalogue with no single-hue option, so nothing here relies on lightness
# alone except High Contrast Mono. If a reader reports difficulty separating
# bins, Mono is the fallback to point them at.
#
# -----------------------------------------------------------------------------
# NO-DATA
# -----------------------------------------------------------------------------
# `na` is per-palette, and both parts of that were forced by measurement.
#
#   1. The chromatic palettes share MAP_NA_DEFAULT, a slightly cool mid grey.
#      heritage's own na_fill (#e0e0e0) does NOT work on maps: it sits 13.8 to
#      14.8 CIEDE2000 units from the nearest bin of four of the five chromatic
#      palettes, under the floor. Nor does a pure grey. Every one of these ramps
#      starts warm and light, and CIEDE2000 under tritanopia compresses
#      warm-against-neutral differences, so a pure grey peaks at about 15.1 on a
#      knife edge. A trace of the opposite cast (C* 2.7, which still reads as
#      plain pencil grey, not as blue) buys the margin and lands at 17.7 across
#      all five.
#
#   2. High Contrast Mono cannot use any grey at all. Its ramp spans pure white
#      to pure black, so no achromatic value is more than about 13 units from
#      all four stops, and #e0e0e0 in particular sits 6.5 units from the white
#      stop; a reader could not tell "no data" from "best". It therefore takes
#      a muted steel blue, 23.2 units from every stop under every simulated
#      deficiency. Blue is the safe chromatic choice because red-green
#      deficiency does not touch it.
#
# na_fill (#e0e0e0) is unchanged and still correct for tier chips and fact sheet
# tables, where the swatch sits on a white card beside its label rather than
# competing with map bins across a distance.
# =============================================================================

# Shared no-data fill for the chromatic entries. See the NO-DATA note above for
# why this is not #e0e0e0 and not a pure grey.
MAP_NA_DEFAULT <- "#b6bcbf"

ADAPARC_PALETTES <- list(

  # ---------------------------------------------------------------------------
  # heritage. The brand palette and the default. This is the ONLY entry that
  # carries the brand-only roles on_white, na_fill, na_text, na_accent and
  # stroke; see the ROLES note above for why those do not vary by palette.
  # ---------------------------------------------------------------------------
  heritage = list(

    id    = "heritage",
    label = "ADA-PARC",
    note  = "Brand gold to maroon. The ADA-PARC house palette.",

    # -------------------------------------------------------------------------
    # fill: large areas. Map bins, gauge arcs, tier chips, tier table headers.
    # -------------------------------------------------------------------------
    fill = c(
      poor      = "#fdd182",
      below     = "#d88036",
      above     = "#a52f1a",
      excellent = "#560000"
    ),

    # -------------------------------------------------------------------------
    # on_fill: text drawn ON the matching fill. Each pairing is the better of
    # near-black and white and clears WCAG 2.2 SC 1.4.3 AA (4.5:1). Measured:
    #   poor 13.15:1 | below 6.35:1 | above 6.95:1 | excellent 15.05:1
    # Never pick a text colour for a tier by eye. Read it from here.
    # -------------------------------------------------------------------------
    on_fill = c(
      poor      = "#111111",
      below     = "#111111",
      above     = "#ffffff",
      excellent = "#ffffff"
    ),

    # -------------------------------------------------------------------------
    # on_white: the tier expressed as TEXT on a white or warm-tinted background,
    # plus small graphic marks such as the 9px summary-table swatch. The fill
    # ramp cannot be used here because its two lightest tiers are far too light
    # to carry text.
    #
    # Derived at the Heritage hue angles, darkened until each clears 4.5:1
    # against both white and the summary table tint #f4efe9. Measured against
    # white: 5.16 | 7.19 | 10.09 | 14.02. Against the tint: 4.52 | 6.29 | 8.83 |
    # 12.27.
    #
    # HONEST LIMIT. Four warm text colours on white cannot reach the separation
    # the fill ramp achieves. Every one must be dark enough to clear contrast,
    # which confines them to roughly 30 points of lightness, and red-green colour
    # vision deficiency compresses the warm hue arc further. The best achievable
    # adjacent separation here is 7.4 CIEDE2000 units, up from 4.0 for the set
    # this replaces, but still short of 15. That is acceptable ONLY because the
    # tier NAME is always printed beside the colour, so colour is reinforcement
    # and never the sole carrier of meaning (WCAG 2.2 SC 1.4.1). If a future
    # design drops the tier label, this ramp is not sufficient on its own.
    # -------------------------------------------------------------------------
    on_white = c(
      poor      = "#9b6000",
      below     = "#993800",
      above     = "#8a0000",
      excellent = "#600000"
    ),

    # -------------------------------------------------------------------------
    # Neutrals for missing data.
    # -------------------------------------------------------------------------
    na_fill   = "#e0e0e0",   # map and chip background where no data exists
    na_text   = "#444444",   # text on na_fill, 7.38:1
    na_accent = "#8a8078",   # warm grey standing in for on_white when rank is NA

    # -------------------------------------------------------------------------
    # Structural colours that travel with the tier ramp.
    # -------------------------------------------------------------------------
    stroke = "#1c2b3a",      # polygon and legend-key outline on maps

    # -------------------------------------------------------------------------
    # identity: the three DOMAINS. Community Living, Community Participation,
    # Work & Economic. See the IDENTITY note in the header for what this role is
    # and why it is per-palette.
    #
    # A REPLACEMENT, NOT A TRANSCRIPTION. The triple that shipped before this
    # was #c2410c #a16207 #7b1717, and it measured 0.9 CIEDE2000 under simulated
    # deuteranopia. Not 9. Zero point nine. For the most common colour vision
    # deficiency the three ADA-PARC domains were one colour, and the scorecards
    # were readable only because every domain prints its name. Anyone reaching
    # for the old hexes for continuity should read that number first.
    #
    # Measured, worst pair: normal 22.4 | protan 20.1 | deutan 20.0 | tritan
    # 20.3. Drift from the heritage ramp: 3.6, 3.6, 5.1.
    # -------------------------------------------------------------------------
    identity = c(
      cl = "#d07048",   # L 58 C 44 h 48, warm terracotta
      cp = "#a81808",   # L 36 C 70 h 40, deep red
      we = "#480008"    # L 12 C 38 h 30, near-black maroon
    ),

    # on_identity: text and marks drawn ON the matching identity colour. The
    # better of near-black and white, as on_fill is. Measured 5.46 | 7.51 |
    # 16.42. cl takes near-black; it is the light chip the step 2.1 decision
    # allows, and it is what buys this palette its separation.
    on_identity = c(
      cl = "#111111",
      cp = "#ffffff",
      we = "#ffffff"
    ),

    # -------------------------------------------------------------------------
    # na: no-data fill for MAP BINS. Not the same value as na_fill above, which
    # is for tier chips and fact sheet table swatches. See the NO-DATA note in
    # the header for why a map needs a different neutral from a chip.
    # -------------------------------------------------------------------------
    na = MAP_NA_DEFAULT
  ),

  # ---------------------------------------------------------------------------
  # blue_gold. Runs ACROSS the red-green confusion axis rather than along it, so
  # protan and deutan readers keep full separation. The red-free option.
  # ---------------------------------------------------------------------------
  blue_gold = list(
    id    = "blue_gold",
    label = "Blue-Gold",
    note  = "Contains no red. Crosses the red-green confusion axis, so protan and deutan readers keep full separation.",
    fill = c(
      poor      = "#f7e3a1",
      below     = "#c9a227",
      above     = "#3f6f9c",
      excellent = "#132f4c"
    ),
    # 14.80 | 7.81 | 5.30 | 13.65 against these fills
    on_fill = c(
      poor      = "#111111",
      below     = "#111111",
      above     = "#ffffff",
      excellent = "#ffffff"
    ),
    # Worst pair: normal 21.9 | protan 22.6 | deutan 21.0 | tritan 22.2.
    # Drift from this ramp: 0.7, 0.8, 0.8. The cheapest of the five, because a
    # ramp that already crosses the confusion axis has separation to spare.
    identity = c(
      cl = "#988868",   # L 57 C 19 h  87, muted gold
      cp = "#4070a0",   # L 46 C 31 h 268, mid blue
      we = "#103050"    # L 19 C 23 h 273, deep navy
    ),
    # 5.45 | 5.19 | 13.46
    on_identity = c(
      cl = "#111111",
      cp = "#ffffff",
      we = "#ffffff"
    ),
    na = MAP_NA_DEFAULT
  ),

  # ---------------------------------------------------------------------------
  # cividis_r. Nunez, Anderton & Renslow (2018), reversed so dark = best. Built
  # so that readers with and without colour vision deficiency perceive the same
  # ordering and the same relative spacing.
  # ---------------------------------------------------------------------------
  cividis_r = list(
    id    = "cividis_r",
    label = "Cividis",
    note  = "Designed specifically for colour vision deficiency. Readers with and without it see the same ordering and spacing.",
    fill = c(
      poor      = "#fee838",
      below     = "#a59c74",
      above     = "#575d6d",
      excellent = "#00224e"
    ),
    # 15.12 | 6.84 | 6.58 | 15.69 against these fills
    on_fill = c(
      poor      = "#111111",
      below     = "#111111",
      above     = "#ffffff",
      excellent = "#ffffff"
    ),
    # Worst pair: normal 22.9 | protan 22.1 | deutan 23.3 | tritan 20.4.
    # Drift from this ramp: 1.2, 1.3, 1.1. Note deutan beats normal here, which
    # is what a palette engineered for colour vision deficiency should do.
    identity = c(
      cl = "#908870",   # L 57 C 14 h  95, khaki
      cp = "#586070",   # L 41 C 10 h 274, slate
      we = "#002048"    # L 13 C 29 h 284, deep navy
    ),
    # 5.34 | 6.32 | 16.15
    on_identity = c(
      cl = "#111111",
      cp = "#ffffff",
      we = "#ffffff"
    ),
    na = MAP_NA_DEFAULT
  ),

  # ---------------------------------------------------------------------------
  # viridis_r. Perceptually uniform, reversed so dark = best. Widely recognised
  # in research contexts, which helps academic readers.
  # ---------------------------------------------------------------------------
  viridis_r = list(
    id    = "viridis_r",
    label = "Viridis",
    note  = "Perceptually uniform and widely recognised in research publications.",
    fill = c(
      poor      = "#fde725",
      below     = "#35b779",
      above     = "#31688e",
      excellent = "#440154"
    ),
    # 14.96 | 7.37 | 6.00 | 15.24 against these fills
    on_fill = c(
      poor      = "#111111",
      below     = "#111111",
      above     = "#ffffff",
      excellent = "#ffffff"
    ),
    # Worst pair: normal 27.7 | protan 23.6 | deutan 20.3 | tritan 20.4.
    # Drift from this ramp: 4.0, 4.0, 4.0. The widest normal-vision spread of
    # the five and the narrowest margin over it, which is the shape of every
    # green-to-purple ramp under red-green deficiency.
    identity = c(
      cl = "#389888",   # L 57 C 26 h 178, teal
      cp = "#3858a0",   # L 39 C 40 h 281, indigo
      we = "#300048"    # L 10 C 46 h 315, deep purple
    ),
    # 5.31 | 6.84 | 17.22
    on_identity = c(
      cl = "#111111",
      cp = "#ffffff",
      we = "#ffffff"
    ),
    na = MAP_NA_DEFAULT
  ),

  # ---------------------------------------------------------------------------
  # mono_high. Full 100-point L* span. For low vision, glare, and monochrome
  # printing. Its lightest stop is pure white, so any surface that draws this
  # ramp on a white background needs a border; the map already strokes every
  # polygon, and the contrast token set in pal_css_vars() carries a rule width
  # for the rest.
  # ---------------------------------------------------------------------------
  mono_high = list(
    id    = "mono_high",
    label = "High Contrast Mono",
    note  = "Maximum separation, no colour. For low vision, glare, and monochrome printing. No-data areas are shown in blue so they cannot be mistaken for the lightest band.",
    fill = c(
      poor      = "#ffffff",
      below     = "#b0b0b0",
      above     = "#585858",
      excellent = "#000000"
    ),
    # 18.88 | 8.71 | 7.11 | 21.00 against these fills
    on_fill = c(
      poor      = "#111111",
      below     = "#111111",
      above     = "#ffffff",
      excellent = "#ffffff"
    ),
    # Identical under every vision type because there is no hue to confuse.
    #
    # WHY NOT THE RAMP'S OWN #ffffff FOR cl. It fails the 3 to 1 non-text floor,
    # being lighter than every surface it would sit on. It works as a MAP BIN
    # because every polygon is stroked; a domain bar has no such guarantee. This
    # is the one place identity and fill genuinely could not share a value, and
    # the reason is structural rather than a matter of degree.
    #
    # THIS ENTRY ANSWERS TO A DARKER SURFACE THAN THE OTHER FOUR, at #e8e8e8
    # rather than #f4efe9, and that is not an exception to the criteria but a
    # consequence of them. pal_effective_fill_id() forces this palette whenever
    # high contrast is on, so it is the only entry that meets the high contrast
    # tint. The chromatic four are only ever drawn in standard contrast.
    #
    # This is also why the separation target is 20 and not 25. At 25 the only
    # solutions for this palette are chromatic, and a High Contrast Mono that
    # prints three colours is not the thing anyone selected.
    # Worst pair 20.2, identical under every vision type. Drift 0.5, 0.1, 0.0.
    identity = c(
      cl = "#808080",   # L 54, mid grey
      cp = "#484848",   # L 31, dark grey
      we = "#000000"    # L  0, black
    ),
    # 4.78 | 9.15 | 21.00
    on_identity = c(
      cl = "#111111",
      cp = "#ffffff",
      we = "#ffffff"
    ),
    na = "#4c82b8"
  )
)

# Display order in the palette selector. ADA-PARC first because it is the
# default; the rest run from most conventional to most specialised.
#
# ONE PLACE THIS IS MIRRORED, AND IT IS NOT AUTOMATIC. www/cssloaders.html
# holds FALLBACK_CHOICES, a copy of these ids with their labels, used only when
# the server does not answer the Display panel's request for its state within
# three seconds. That copy exists so the control is never dead on a broken
# load, which by definition is a load where it cannot be fetched from here. If
# a palette is added, renamed or reordered, update FALLBACK_CHOICES too.
PALETTE_ORDER <- c("heritage", "blue_gold",
                   "cividis_r", "viridis_r", "mono_high")

PALETTE_DEFAULT <- "heritage"

# Contrast modes. A second binding of the same roles, not a second catalogue.
# The full token set lands in phase 4; pal_css_vars() below emits what exists.
CONTRAST_MODES   <- c("standard", "high")
CONTRAST_DEFAULT <- "standard"


# Tier order, worst to best. Use these names rather than positional indices
# wherever the calling code can, so a future five-tier scheme is a smaller edit.
ADA_PARC_TIERS <- c("poor", "below", "above", "excellent")

# Reader-facing tier names. Held here rather than repeated at each call site so
# the map legend, the map tooltip, the ranking table and the state scorecard
# cannot drift into three different spellings of the same four tiers.
#
# The short set exists only for the ranking table's Tier column, which is 95px
# wide and wraps at the full labels. Prefer the full set everywhere else.
ADA_PARC_TIER_LABELS <- c(
  poor      = "Poor",
  below     = "Below Average",
  above     = "Above Average",
  excellent = "Excellent"
)

ADA_PARC_TIER_LABELS_SHORT <- c(
  poor      = "Poor",
  below     = "Below Avg",
  above     = "Above Avg",
  excellent = "Excellent"
)

# CSS class suffixes used by the `.rp-qual.*` rules in scorecard_state.css.
# These are markup contracts, not display text; they are not interchangeable
# with the labels above.
ADA_PARC_TIER_CLASSES <- c(
  poor      = "poor",
  below     = "below-avg",
  above     = "above-avg",
  excellent = "excellent"
)


# -----------------------------------------------------------------------------
# ADA_PARC_PALETTE
# -----------------------------------------------------------------------------
# The brand entry, bound to its old name. Derived, never edited.
#
# This is the one alias kept from the pre-2026-08-03 split. It survives because
# heritage is where the brand-only roles live, so `ADA_PARC_PALETTE$on_white`,
# `$na_fill` and `$stroke` still read as what they are: values that do not vary
# by palette. New code that wants a SELECTED palette must go through pal_get()
# or the pal_* accessors, which take an id; reaching for this name is a sign the
# call site has quietly assumed heritage.
#
# MAP_PALETTES, MAP_PALETTE_ORDER and MAP_PALETTE_DEFAULT were removed rather
# than aliased. They named the map-only half of a split that no longer exists,
# and every consumer was repointed in the same change.
ADA_PARC_PALETTE <- ADAPARC_PALETTES$heritage

# -----------------------------------------------------------------------------
# Accessors
# -----------------------------------------------------------------------------

#' Resolve a palette id to its catalogue entry, falling back to the default.
#'
#' Guards against an empty, NA or stale id, which Shiny can hand over on the
#' first flush before the selector has initialised, and which a Quarto param can
#' hand over if a scorecard is rendered by hand with a typo.
#'
#' @param id Palette id, e.g. "heritage". NULL and unknown ids resolve to the
#'   default rather than erroring, because a wrong colour is recoverable and a
#'   blank map is not.
pal_id <- function(id = PALETTE_DEFAULT) {
  if (is.null(id) || length(id) != 1 || is.na(id) ||
      !nzchar(id) || !id %in% names(ADAPARC_PALETTES)) {
    return(PALETTE_DEFAULT)
  }
  id
}

pal_get <- function(id = PALETTE_DEFAULT) ADAPARC_PALETTES[[pal_id(id)]]

#' Tier fill colours, worst to best.
#'
#' @param n Number of bins required. Returns the four canonical stops unchanged
#'   when n is 4, which is the case for every current consumer. For any other n
#'   the stops are interpolated, which keeps the ramp usable if the map ever
#'   moves to a different number of classes.
#' @param id Palette id. Defaults to heritage, so every call site written before
#'   the catalogue was unified behaves exactly as it did.
#' @return Unnamed character vector of hex colours, light to dark.
pal_fill <- function(n = 4, id = PALETTE_DEFAULT) {
  stops <- unname(pal_get(id)$fill)
  if (n == length(stops)) stops else grDevices::colorRampPalette(stops)(n)
}

#' Text colour that clears WCAG AA on a given tier fill.
#'
#' Must be read with the SAME id as the fill it will sit on. Pairing heritage
#' ink with a Cividis fill is how a 2.31:1 gets shipped.
#'
#' @param tier Tier name ("poor", "below", "above", "excellent") or index 1-4.
#' @param id Palette id. Defaults to heritage.
pal_on_fill <- function(tier, id = PALETTE_DEFAULT) {
  if (is.numeric(tier)) tier <- ADA_PARC_TIERS[tier]
  unname(pal_get(id)$on_fill[tier])
}

#' No-data fill for map bins, per palette.
#'
#' @param id Palette id. Defaults to heritage.
pal_na <- function(id = PALETTE_DEFAULT) pal_get(id)$na

# Domain keys, in positional order light to dark. The assignment is positional
# and not by hue; see the IDENTITY note in the header.
ADAPARC_DOMAINS <- c("cl", "cp", "we")

# Display names, so a call site never has to spell them out and they cannot
# drift between the state scorecard, the category scorecard and the About tab.
ADAPARC_DOMAIN_LABELS <- c(
  cl = "Community Living",
  cp = "Community Participation",
  we = "Work & Economic"
)

#' Domain identity colour, per palette.
#'
#' UNLIKE pal_on_white(), THIS TAKES AN id AND MUST BE GIVEN ONE. Identity
#' follows the reader's selection. A call site that omits `id` silently gets
#' heritage, which on a Cividis scorecard is exactly the bug phase 2 of
#' docs/palette-v2-build-plan.qmd exists to fix. The default is kept only so
#' that a static render with no palette param still produces a document.
#'
#' @param role One of "cl", "cp", "we", or an index 1-3. Vectorised over role,
#'   so pal_identity(ADAPARC_DOMAINS, id) returns all three in order.
#' @param id Palette id.
pal_identity <- function(role = ADAPARC_DOMAINS, id = PALETTE_DEFAULT) {
  if (is.numeric(role)) role <- ADAPARC_DOMAINS[role]
  unname(pal_get(id)$identity[role])
}

#' Text colour that clears WCAG AA on a given identity colour.
#'
#' Must be read with the SAME id as the identity colour it will sit on, for the
#' same reason pal_on_fill() must. Note that `cl` takes near-black in every
#' palette and the other two take white; that is not a coincidence to rely on,
#' it is the light-chip allowance recorded in the ADMISSION CRITERIA, and a
#' future palette could differ.
#'
#' @param role One of "cl", "cp", "we", or an index 1-3.
#' @param id Palette id.
pal_on_identity <- function(role = ADAPARC_DOMAINS, id = PALETTE_DEFAULT) {
  if (is.numeric(role)) role <- ADAPARC_DOMAINS[role]
  unname(pal_get(id)$on_identity[role])
}

#' Tier colour for text and small marks on a white or warm-tinted background.
#'
#' DELIBERATELY HAS NO `id` PARAMETER. on_white does not follow the reader's
#' palette selection; it is fixed at the heritage ramp for every palette. The
#' measurements that forced that are in the ROLES note at the top of this file
#' and in docs/palette-v1-build-plan.qmd section 1.2. In short: every derived
#' per-palette on_white ramp collapses to between 0.8 and 7.3 CIEDE2000 of
#' adjacent separation, and High Contrast Mono has no solution at all. Do not
#' add an id parameter here without redoing that measurement.
#'
#' @param tier Tier name or index 1-4. NA returns the neutral warm grey, which
#'   is what an unranked row should use.
pal_on_white <- function(tier) {
  if (length(tier) == 1 && is.na(tier)) return(ADA_PARC_PALETTE$na_accent)
  if (is.numeric(tier)) tier <- ADA_PARC_TIERS[tier]
  unname(ADA_PARC_PALETTE$on_white[tier])
}

#' Rank cut points for the four tier bands.
#'
#' The site has always banded ranks at 13, 26 and 39, which is what
#' ceiling(n * c(.25, .5, .75)) gives for the 51 jurisdictions that carry an
#' overall index score. Deriving them from n rather than writing them down means
#' a domain where fewer places have data, or a future year with a different
#' count, bands correctly instead of pushing every unranked slot into "Poor".
#'
#' @param n Number of ranked units. Defaults to 51, which reproduces the
#'   historical 13/26/39 exactly.
#' @return Integer vector of length 3, the highest rank number in each of the
#'   top three bands.
pal_rank_cuts <- function(n = 51L) {
  n <- suppressWarnings(as.numeric(n))
  if (length(n) != 1 || is.na(n) || n < 1) n <- 51
  as.integer(ceiling(n * c(0.25, 0.5, 0.75)))
}

#' Map a 1-based rank into a tier name using the site's quartile cut points.
#'
#' Vectorised over `r`. Centralising the banding here means a future change is
#' one edit rather than the five call sites it was spread across in
#' state_scorecard.qmd and category_scorecard.qmd.
#'
#' CAUTION. Banding by RANK and banding by SCORE QUARTILE are not the same
#' classification and can disagree at a boundary when scores tie or when the
#' number of ranked units is not a multiple of four. Where a surface also shows
#' a colour taken from a score quartile, classify the label from the same score
#' quartile, not from here. See category_scorecard.qmd, which does exactly that.
#'
#' @param r Integer rank, 1 = best. NA returns NA_character_.
#' @param n Number of ranked units, passed to pal_rank_cuts().
pal_tier_of_rank <- function(r, n = 51L) {
  cuts <- pal_rank_cuts(n)
  idx  <- 4L - findInterval(suppressWarnings(as.numeric(r)), cuts + 1L)
  out  <- ADA_PARC_TIERS[idx]
  out[is.na(r)] <- NA_character_
  out
}

#' Reader-facing label for a tier.
#'
#' @param tier Tier name, or index 1-4, or a vector of either.
#' @param short Use the abbreviated set, which exists only for the ranking
#'   table's narrow Tier column.
#' @param na_label What to return for NA. Defaults to NA_character_; the map
#'   tooltip passes "No data".
pal_tier_label <- function(tier, short = FALSE, na_label = NA_character_) {
  if (is.numeric(tier)) tier <- ADA_PARC_TIERS[tier]
  lab <- if (isTRUE(short)) ADA_PARC_TIER_LABELS_SHORT else ADA_PARC_TIER_LABELS
  out <- unname(lab[tier])
  out[is.na(tier)] <- na_label
  out
}

#' CSS class suffix for a tier, for the `.rp-qual.*` rules in
#' scorecard_state.css. Returns "" for NA, which is what the rank pill expects.
#'
#' @param tier Tier name, or index 1-4, or a vector of either.
pal_tier_class <- function(tier) {
  if (is.numeric(tier)) tier <- ADA_PARC_TIERS[tier]
  out <- unname(ADA_PARC_TIER_CLASSES[tier])
  out[is.na(tier)] <- ""
  out
}

#' Resolve a contrast mode, falling back to the default.
pal_contrast_mode <- function(contrast = CONTRAST_DEFAULT) {
  if (is.null(contrast) || length(contrast) != 1 || is.na(contrast) ||
      !contrast %in% CONTRAST_MODES) {
    return(CONTRAST_DEFAULT)
  }
  contrast
}

#' Which palette actually supplies FILL, given a selection and a contrast mode.
#'
#' High contrast forces High Contrast Mono, because Mono is the only entry with
#' a full 100-point L* span. The reader's own choice is not discarded: it is
#' still what the selector shows and what returning to standard contrast
#' restores.
#'
#' Every surface that draws a fill must route through here rather than deciding
#' locally, or the maps and the CSS will disagree about what high contrast means.
pal_effective_fill_id <- function(id = PALETTE_DEFAULT,
                                  contrast = CONTRAST_DEFAULT) {
  if (pal_contrast_mode(contrast) == "high") "mono_high" else pal_id(id)
}

# =============================================================================
# SITE CHROME TOKENS
# =============================================================================
# Phase 4 of docs/palette-v1-build-plan.qmd, added 2026-08-03.
#
# Contrast is a SECOND BINDING OF THE SAME ROLES, not a second catalogue and not
# a sixth palette. Every token below exists in both modes; nothing appears in
# one and not the other, so no consumer has to branch.
#
# The `standard` column is what www/styles.css already shipped, moved here
# unchanged. Its own :root block keeps those values as fallbacks, so the app
# renders correctly before the generated block is injected and if the injection
# ever fails. This block is the source; that one is the safety net.
#
# HIGH CONTRAST is pure black on pure white for every text pair, structural
# rules doubled, focus rings widened, and the tier ramp forced to High Contrast
# Mono by pal_effective_fill_id(). Two tokens are deliberately NOT pure black:
#
#   navy-dark      #333333, so the active navbar item stays distinguishable
#                  from the navbar itself once navy is #000000. White on it is
#                  12.63:1, comfortably past AAA.
#   bg-stripe      #e8e8e8, so table zebra striping survives. Black on it is
#                  17.14:1. Flattening it to white would clear the contrast
#                  requirement while destroying the row banding, which is the
#                  kind of "passes the audit, harms the reader" trade this file
#                  exists to prevent.
#
# THE FLOOR IS 7:1 IN HIGH CONTRAST, WCAG 2.2 SC 1.4.6 AAA, against 4.5:1 AA
# for standard. Both are verified for every declared pair by
# `python3 scripts/check_palette.py`, which reads ADAPARC_UI_PAIRS below rather
# than a list kept in its own head.
# =============================================================================

ADAPARC_UI_TOKENS <- list(

  standard = c(
    navy            = "#1c2b3a",
    navy_dark       = "#152233",
    bg_page         = "#f5f7fa",
    bg_card         = "#ffffff",
    bg_stripe       = "#fafbfd",
    border          = "#e4e8ef",
    input_border    = "#6b625a",
    text_body       = "#1c2b3a",
    text_muted      = "#1c2b3a",
    on_dark         = "#ffffff",
    on_dark_hover   = "#f5f5f5",
    # Gold rule under the selected navbar tab. Before this token the active tab
    # was signalled only by navy_dark on navy, which is 1.11:1, invisible in
    # practice and colour-only besides. #e8b84b is 7.8:1 on navy, and the rule
    # is a shape as well as a colour, so the state survives both low vision and
    # greyscale. Canon gold, not a new hue.
    nav_active      = "#e8b84b",
    focus_ring      = "#0d3b8e",
    focus_ring_dark = "#ffffff",
    ui_accent       = "#7b1717",
    ui_accent_dark  = "#5e1111",
    attr_text       = "#591313",
    attr_bg         = "#f9f7f4",
    attr_border     = "#e2dcd4",
    attr_rule       = "#7b1a1a"
  ),

  high = c(
    navy            = "#000000",
    navy_dark       = "#333333",
    bg_page         = "#ffffff",
    bg_card         = "#ffffff",
    bg_stripe       = "#e8e8e8",
    border          = "#000000",
    input_border    = "#000000",
    text_body       = "#000000",
    text_muted      = "#000000",
    on_dark         = "#ffffff",
    # Hover is NOT dimmed in high contrast. A dimmed white would cost contrast
    # to signal something the underline added by the high contrast block in
    # www/styles.css already signals, without relying on colour at all.
    on_dark_hover   = "#ffffff",
    # White, not gold. The navbar is #000000 here and the whole point of the
    # mode is to stop asking the reader to resolve a hue. White on black is
    # 21:1 and the 4px rule still carries the state without colour. This is
    # also what the high contrast block in www/styles.css used to hardcode as
    # var(--on-dark); binding it to the same token as standard mode means the
    # rule lives in ONE place and the two modes cannot drift.
    nav_active      = "#ffffff",
    focus_ring      = "#000000",
    focus_ring_dark = "#ffffff",
    ui_accent       = "#000000",
    ui_accent_dark  = "#333333",
    attr_text       = "#000000",
    attr_bg         = "#ffffff",
    attr_border     = "#000000",
    attr_rule       = "#000000"
  )
)

# Non-colour tokens. Held beside the colours because they are part of the same
# binding: widening a rule is how high contrast compensates where a colour
# change alone cannot.
ADAPARC_UI_SIZES <- list(
  standard = c(
    rule_width       = "1px",
    map_stroke_width = "0.35px",
    focus_ring_width = "3px",
    focus_halo       = "rgba(13, 59, 142, 0.2)"
  ),
  high = c(
    rule_width       = "2px",
    map_stroke_width = "1.2px",
    focus_ring_width = "4px",
    focus_halo       = "rgba(0, 0, 0, 0.35)"
  )
)

# Every foreground/background pairing the stylesheets actually create, named so
# a failure report says which surface broke rather than which hex.
#
# `kind` selects the floor:
#   text     4.5:1 standard, 7:1 high. SC 1.4.3 and SC 1.4.6.
#   nontext  3:1 in both. SC 1.4.11, for boundaries that IDENTIFY a component
#            or a state.
#   decor    no floor. Recorded and measured, but not required to pass, because
#            the boundary is redundant: something else already carries the same
#            information. Every "decor" entry has to say what that something is.
#            This exists so a hairline can be called decorative ON PURPOSE and
#            in the open, rather than being left off the list where nobody can
#            see the judgment or argue with it.
#
# ADD A PAIR HERE WHENEVER A STYLESHEET CREATES ONE. An unlisted pairing is not
# checked, and check_palette.py will name any token that has no pairing at all.
ADAPARC_UI_PAIRS <- list(
  list(fg = "text_body",       bg = "bg_page",   kind = "text",    label = "body text on page background"),
  list(fg = "text_body",       bg = "bg_card",   kind = "text",    label = "body text on card"),
  list(fg = "text_body",       bg = "bg_stripe", kind = "text",    label = "body text on table zebra stripe"),
  list(fg = "text_muted",      bg = "bg_card",   kind = "text",    label = "muted text on card"),
  list(fg = "on_dark",         bg = "navy",      kind = "text",    label = "navbar link and table header text"),
  list(fg = "on_dark_hover",   bg = "navy",      kind = "text",    label = "navbar link text on hover"),
  list(fg = "on_dark",         bg = "navy_dark", kind = "text",    label = "active navbar item text"),
  list(fg = "nav_active",      bg = "navy",      kind = "nontext", label = "gold rule under the selected navbar tab"),
  list(fg = "on_dark",         bg = "navy",      kind = "nontext", label = "white rule under the hovered navbar tab"),
  list(fg = "navy",            bg = "bg_page",   kind = "text",    label = "heading on page background"),
  list(fg = "navy",            bg = "bg_card",   kind = "text",    label = "heading on card"),
  list(fg = "attr_text",       bg = "attr_bg",   kind = "text",    label = "attribution footer text"),
  list(fg = "on_dark",         bg = "ui_accent", kind = "text",    label = "Display button label"),
  list(fg = "on_dark",         bg = "ui_accent_dark", kind = "text", label = "Display button label on hover"),
  list(fg = "focus_ring",      bg = "bg_page",   kind = "nontext", label = "focus ring on page background"),
  list(fg = "focus_ring",      bg = "bg_card",   kind = "nontext", label = "focus ring on card"),
  list(fg = "focus_ring_dark", bg = "navy",      kind = "nontext", label = "focus ring on navbar"),
  list(fg = "ui_accent",       bg = "bg_card",   kind = "nontext", label = "Display panel border on card"),
  list(fg = "input_border",    bg = "bg_card",   kind = "nontext", label = "select and text input boundary"),
  list(fg = "attr_rule",       bg = "attr_bg",   kind = "nontext", label = "attribution footer top rule"),
  # Decorative, with the reason stated. In standard contrast both of these are
  # hairlines sitting beside something that already draws the same boundary, so
  # holding them to 3:1 would mean darkening a line whose job is to be quiet.
  # In high contrast both become #000000 and do real work, which is why they
  # are tokens at all rather than literals.
  list(fg = "border",          bg = "bg_card",   kind = "decor",   label = "table cell rule; zebra striping carries the row banding in standard contrast"),
  list(fg = "attr_border",     bg = "attr_bg",   kind = "decor",   label = "attribution footer hairline; the 3px top rule above carries the boundary")
)

#' Chrome tokens for a contrast mode, as a named character vector.
pal_ui_tokens <- function(contrast = CONTRAST_DEFAULT) {
  k <- pal_contrast_mode(contrast)
  c(ADAPARC_UI_TOKENS[[k]], ADAPARC_UI_SIZES[[k]])
}


# =============================================================================
# DOCUMENT TOKENS
# =============================================================================
# Phase 5 of docs/palette-v1-build-plan.qmd, added 2026-08-03.
#
# The scorecards are DOCUMENTS, not app chrome. They are warm, cream and
# maroon; the app is cool and navy. Pointing scorecard_state.css and
# scorecard_v3.css at the chrome tokens would have made the scorecards navy in
# standard mode, which is a redesign, not a contrast mode. So they get their own
# group, whose `standard` column is exactly the literals those two stylesheets
# already carried.
#
# DOMAIN COLOURS, AND WHY THEY GO BLACK IN HIGH CONTRAST. Community Living,
# Community Participation and Work & Economic have carried burnt orange, muted
# gold and deep crimson since before this work. Two measurements decided what
# happens to them:
#
#   1. Darkened just enough for white text to clear 7:1, they become #9f350a,
#      #7f4d06 and #7b1717, whose minimum pairwise separation is 2.7 CIEDE2000
#      under protanopia and 2.1 under deuteranopia. Against a working floor of
#      15, that is three identical browns.
#
#   2. That is not a loss, because they were ALREADY 4.9 and 1.1 under those
#      two deficiencies at their current values. The three domains have never
#      been told apart by colour. Every domain card, rank pill and summary row
#      prints its domain NAME beside the colour.
#
# So in high contrast all three go to #000000. Colour was reinforcement, the
# reader has asked for maximum contrast, and nothing that was carrying meaning
# is lost. This is the same argument as on_white in the ROLES note above, and
# it was reached the same way, by measuring rather than by taste.
# =============================================================================

ADAPARC_DOC_TOKENS <- list(

  standard = c(
    doc_ink        = "#111111",
    doc_ink_soft   = "#3d3630",
    doc_muted      = "#6b7280",
    doc_paper      = "#ffffff",
    doc_cream      = "#faf7f3",
    doc_tint       = "#f4efe9",   # summary table warm tint
    doc_rule       = "#e5ddd3",
    doc_rule_soft  = "#dde2ea",
    doc_navy       = "#1c2b3a",
    doc_accent     = "#7b1717",
    doc_accent_alt = "#8b1a1a"
  ),

  high = c(
    doc_ink        = "#000000",
    doc_ink_soft   = "#000000",
    doc_muted      = "#000000",
    doc_paper      = "#ffffff",
    doc_cream      = "#ffffff",
    doc_tint       = "#e8e8e8",
    doc_rule       = "#000000",
    doc_rule_soft  = "#000000",
    doc_navy       = "#000000",
    doc_accent     = "#000000",
    doc_accent_alt = "#000000"
  )
)

ADAPARC_DOC_PAIRS <- list(
  list(fg = "doc_ink",        bg = "doc_paper", kind = "text",    label = "scorecard body text on paper"),
  list(fg = "doc_ink",        bg = "doc_cream", kind = "text",    label = "scorecard body text on cream"),
  list(fg = "doc_ink",        bg = "doc_tint",  kind = "text",    label = "scorecard body text on summary table tint"),
  list(fg = "doc_ink_soft",   bg = "doc_paper", kind = "text",    label = "scorecard secondary text on paper"),
  list(fg = "doc_ink_soft",   bg = "doc_cream", kind = "text",    label = "scorecard secondary text on cream"),
  list(fg = "doc_muted",      bg = "doc_paper", kind = "text",    label = "scorecard muted caption on paper"),
  list(fg = "doc_navy",       bg = "doc_paper", kind = "text",    label = "category scorecard heading on paper"),
  list(fg = "doc_accent",     bg = "doc_paper", kind = "text",    label = "scorecard accent text on paper"),
  list(fg = "doc_accent_alt", bg = "doc_paper", kind = "text",    label = "category scorecard accent text on paper"),
  list(fg = "doc_paper",      bg = "doc_navy",  kind = "text",    label = "column label text on the summary table chrome band"),
  list(fg = "doc_navy",       bg = "doc_tint",  kind = "text",    label = "Overall row label on summary table tint"),
  # Hairlines, redundant with the card backgrounds they separate. See the
  # `decor` note on ADAPARC_UI_PAIRS.
  list(fg = "doc_rule",       bg = "doc_paper", kind = "decor",   label = "scorecard card hairline; the cream fill carries the boundary"),
  list(fg = "doc_rule_soft",  bg = "doc_paper", kind = "decor",   label = "category scorecard hairline; the tinted fill carries the boundary")
)

#' Document tokens for a contrast mode, as a named character vector.
pal_doc_tokens <- function(contrast = CONTRAST_DEFAULT) {
  ADAPARC_DOC_TOKENS[[pal_contrast_mode(contrast)]]
}

#' Emit a palette and contrast combination as CSS custom properties.
#'
#' Used to inject a :root block into the Shiny app head and into rendered Quarto
#' documents, so their stylesheets consume var(--tier-*) and var(--navy) instead
#' of carrying literals. Every combination of the five palettes and two contrast
#' modes produces a COMPLETE block; nothing downstream needs to know which one
#' it got, and no token is ever absent.
#'
#' WHAT VARIES AND WHAT DOES NOT.
#'   --tier-*-bg, --tier-*-text, --tier-map-na-bg  follow the selected palette
#'   --identity-*, --on-identity-*                 follow the selected palette
#'   --tier-*-ink, --tier-na-*                     fixed at heritage, always;
#'                                                 see the note on pal_on_white()
#'   the chrome tokens                             follow contrast only, never
#'                                                 the palette. The reader chose
#'                                                 a colour scheme for the DATA,
#'                                                 not for the navbar.
#'
#' @param id Palette id. Unknown ids resolve to the default.
#' @param contrast "standard" or "high".
#' @param selector CSS selector to bind the properties to.
#' @param chrome Emit the site chrome tokens. The app wants these; a scorecard
#'   has no navbar and passes FALSE.
#' @param doc Emit the document tokens. A scorecard wants these; the app has no
#'   cream cards and passes FALSE. Both default to their app-side value, so a
#'   bare pal_css_rules() call is the app's block.
#' @return The CSS rule text, without a <style> wrapper.
pal_css_rules <- function(id = PALETTE_DEFAULT,
                          contrast = CONTRAST_DEFAULT,
                          selector = ":root",
                          chrome = TRUE,
                          doc = FALSE) {
  contrast <- pal_contrast_mode(contrast)
  # High contrast overrides the reader's palette for FILL only, through the same
  # helper the maps use, so CSS and map can never disagree about what high
  # contrast means.
  p <- pal_get(pal_effective_fill_id(id, contrast))
  b <- ADA_PARC_PALETTE   # brand-only roles, palette-independent

  line <- function(k, v) paste0("  --", k, ": ", v, ";")
  body <- c(
    line("palette-id",    pal_id(id)),
    line("contrast-mode", contrast),
    vapply(ADA_PARC_TIERS,
           function(t) line(paste0("tier-", t, "-bg"), p$fill[[t]]),
           character(1)),
    vapply(ADA_PARC_TIERS,
           function(t) line(paste0("tier-", t, "-text"), p$on_fill[[t]]),
           character(1)),
    vapply(ADA_PARC_TIERS,
           function(t) line(paste0("tier-", t, "-ink"), b$on_white[[t]]),
           character(1)),
    line("tier-na-bg",     b$na_fill),
    line("tier-na-text",   b$na_text),
    line("tier-na-ink",    b$na_accent),
    line("tier-map-na-bg", p$na),
    line("tier-stroke",    if (contrast == "high") "#000000" else b$stroke),
    # Domain identity. Read off `p`, the EFFECTIVE fill palette, so high
    # contrast takes the domains to High Contrast Mono's greyscale triple by
    # the same route the tiers take. That is a change in behaviour and an
    # improvement: the high contrast doc tokens set dom_cl, dom_cp and dom_we
    # all to #000000, which is three domains at zero separation, in the mode
    # chosen by the readers least able to absorb that.
    vapply(ADAPARC_DOMAINS,
           function(d) line(paste0("identity-", d), p$identity[[d]]),
           character(1)),
    vapply(ADAPARC_DOMAINS,
           function(d) line(paste0("on-identity-", d), p$on_identity[[d]]),
           character(1)),
    # Sizes are emitted even when chrome is off. A scorecard has no navbar but
    # it does have rules and map strokes, and those are the tokens that carry
    # high contrast where a colour change alone cannot.
    # Token names are snake_case in R and kebab-case in CSS.
    vapply(names(ADAPARC_UI_SIZES[[contrast]]),
           function(k) line(gsub("_", "-", k), ADAPARC_UI_SIZES[[contrast]][[k]]),
           character(1))
  )

  add_group <- function(tok) {
    vapply(names(tok), function(k) line(gsub("_", "-", k), tok[[k]]),
           character(1))
  }
  if (isTRUE(chrome)) body <- c(body, add_group(ADAPARC_UI_TOKENS[[contrast]]))
  if (isTRUE(doc))    body <- c(body, add_group(ADAPARC_DOC_TOKENS[[contrast]]))

  paste0(selector, " {\n", paste(body, collapse = "\n"), "\n}\n")
}

#' The same block wrapped in a <style> element, for an `asis` Quarto chunk.
pal_css_vars <- function(id = PALETTE_DEFAULT,
                         contrast = CONTRAST_DEFAULT,
                         selector = ":root",
                         chrome = TRUE,
                         doc = FALSE) {
  paste0("<style>\n",
         pal_css_rules(id, contrast, selector, chrome, doc),
         "</style>\n")
}

#' Everything a rendered scorecard needs injected: the token block, plus the
#' data-adaparc-contrast hook its stylesheet keys its structural rules on.
#'
#' Emitted from an `asis` chunk. The attribute is set by script rather than
#' written into the <html> tag because Quarto owns that tag; a one-line script
#' in the body is the smallest honest way in, and it runs before paint because
#' it is inline and synchronous.
pal_doc_header <- function(id = PALETTE_DEFAULT, contrast = CONTRAST_DEFAULT) {
  contrast <- pal_contrast_mode(contrast)
  paste0(
    pal_css_vars(id, contrast, chrome = FALSE, doc = TRUE),
    "<script>document.documentElement.setAttribute(",
    "'data-adaparc-contrast', '", contrast, "');</script>\n"
  )
}


# -----------------------------------------------------------------------------
# Map palette accessors
# -----------------------------------------------------------------------------
# Kept as the names the two maps already call. They are now thin wrappers over
# the pal_* accessors rather than a second system; the split they used to
# represent (brand palette versus map choice) ended with the restructure.

#' Named vector for selectInput: display label -> palette id.
#'
#' Returned in PALETTE_ORDER so the dropdown order is defined in one place.
map_palette_choices <- function() {
  ids <- PALETTE_ORDER
  stats::setNames(ids, vapply(ids, function(i) ADAPARC_PALETTES[[i]]$label,
                              character(1)))
}

#' Resolve a palette id, falling back to the default.
map_palette_id <- function(id) pal_id(id)

#' Bin colours for a map palette, light to dark.
#'
#' @param id Palette id, e.g. "heritage".
#' @param n  Number of bins. Four returns the canonical stops unchanged.
map_palette_colors <- function(id, n = 4) pal_fill(n, id)

#' No-data fill for a map palette.
#'
#' Per-palette rather than global, because High Contrast Mono needs a chromatic
#' no-data colour; see the NO-DATA note in the catalogue header.
map_palette_na <- function(id) pal_na(id)

#' One-line plain-language description, for the palette selector.
map_palette_note <- function(id) pal_get(id)$note
