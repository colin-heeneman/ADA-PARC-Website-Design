# ══════════════════════════════════════════════════════════════════════════
# State Scorecard — shared scoring / tier / presentation logic
#
# Single source of truth for everything that is NOT specific to a particular
# rendered document: which state, what its scores and ranks are, which tier
# each rank falls in, and how a gauge / sub-index bar row / summary table is
# drawn from those numbers.
#
# state_scorecard.qmd (the interactive, on-screen version shown in the Shiny
# app's iframe) and state_scorecard_print.qmd (the branded, paginated
# download built for "Download" — see project_state_scorecard_print_download
# memory) both call build_state_scorecard() and get back the SAME numbers,
# the SAME tier colours, and the SAME gauge/bar/table HTML. Before this file
# existed, the download simply copied the on-screen render's output file, so
# there was one copy of this logic and it was correct by construction. Adding
# a second, purpose-built print document made a second copy an ever-present
# risk: the two documents could report different scores or tiers for the
# same state if a scoring change were made in only one of them. Extracting
# the computation here keeps that impossible rather than merely unlikely —
# exactly the principle national_topic_bundle() already established for the
# National Data download (the app is the single source of truth; the
# document only handles presentation and branding).
#
# WHAT STAYS OUT OF THIS FILE, ON PURPOSE:
#   - knitr chunk options, showtext/systemfonts font registration: these are
#     per-document rendering concerns, not scoring logic, and the print
#     document does not even use ggplot, so it does not need them at all.
#   - The banner/footer masthead and page markup: those are literal, hand-
#     written HTML in each .qmd, matching the existing convention (see the
#     note on the national_topic.qmd footer chunk: report types intentionally
#     do not share a banner/footer component, so their layouts can diverge
#     independently). What must never diverge between the two documents is
#     the SCORES, and that is what lives here.
#
# To update the underlying data: re-run the data pipeline and copy the new
# index_scores_wide.csv into scorecard/scorecard_data/final/, same as today.
# ══════════════════════════════════════════════════════════════════════════

library(readr)
library(dplyr)
library(tibble)
library(htmltools)
library(gt)

# ── Tier palette: single source of truth ──────────────────────────────────
# Shared with the Shiny app, the category scorecard and the fact sheets.
# Path-probed rather than assumed, so this file works whether it is sourced
# with the working directory at scorecard/ (quarto_render(execute_dir=...))
# or at the project root (a hand render from an open RStudio session).
local({
  cand <- c(file.path("..", "scripts", "palettes.R"),
            file.path("scripts", "palettes.R"))
  hit  <- cand[file.exists(cand)]
  if (length(hit) == 0) {
    stop("state_scorecard_core.R cannot locate scripts/palettes.R. ",
         "Render with execute_dir set to the scorecard/ directory, ",
         "or from the project root.")
  }
  source(hit[1], local = FALSE)
})

#' Build every score, rank, tier, and ready-to-render presentation object
#' the State Scorecard needs, for one state under one display setting.
#'
#' @param state_abbr Two-letter state/territory abbreviation (or "USA").
#' @param year       Data year label shown on the page (does not filter the
#'                    data; index_scores_wide.csv currently holds one year).
#' @param palette    Reader's selected palette id, e.g. "heritage".
#' @param contrast   "standard" or "high".
#' @param data_dir   Where scorecard_data/ lives relative to the working
#'                    directory at render time. Defaults to the path both
#'                    state_scorecard.qmd and state_scorecard_print.qmd use
#'                    when rendered with execute_dir = scorecard/.
#'
#' @return A list. Data: state_name, year_label, s(), n(), f(), n_ranked,
#'   RNK. Chrome tokens: CHROME, CHROME_ON, CHROME_RULE, SPINE, COL_TXT,
#'   doc_header_html. Presentation helpers: qual_info(), rank_pill(),
#'   bar_qual_label(), rank_band_note(), tier_bg(), tier_ink(), rank_line(),
#'   make_gauge(), make_subindex_bars(). Ready-built output: summary_gt, a
#'   complete gt table object.
build_state_scorecard <- function(state_abbr,
                                  year     = 2024,
                                  palette  = "heritage",
                                  contrast = "standard",
                                  data_dir = "scorecard_data") {

  # ── Active palette and contrast ─────────────────────────────────────────
  # PAL is the EFFECTIVE fill palette, which is not always the one selected:
  # high contrast forces High Contrast Mono. Routing through
  # pal_effective_fill_id() is what keeps every consumer of this list
  # agreeing with the app's Display settings.
  CONTRAST <- pal_contrast_mode(contrast)
  PAL      <- pal_effective_fill_id(palette, CONTRAST)

  # ── Data ─────────────────────────────────────────────────────────────────
  raw       <- read_csv(file.path(data_dir, "final", "index_scores_wide.csv"),
                        show_col_types = FALSE)
  state_row <- raw |> filter(ABBR == state_abbr)
  nat_row   <- raw |> filter(ABBR == "USA")

  # If no USA row in pipeline output, fall back to column means across all states
  if (nrow(nat_row) == 0) {
    nat_row <- raw |>
      filter(!is.na(ABBR), ABBR != state_abbr) |>
      summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) |>
      mutate(ABBR = "USA", NAME = "US Average")
  }

  state_name <- as.character(state_row$NAME)
  year_label <- year

  # ── Shorthand helpers ────────────────────────────────────────────────────
  s <- function(col) {
    val <- state_row[[col]]
    if (is.null(val)) return(NA_real_)
    as.numeric(val)
  }
  n <- function(col) {
    val <- nat_row[[col]]
    if (is.null(val)) return(NA_real_)
    as.numeric(val)
  }
  f <- function(x) ifelse(is.na(x), "—", sprintf("%.1f", x))

  # ── Page chrome ───────────────────────────────────────────────────────────
  # doc_navy follows the CONTRAST mode and goes to pure black in high
  # contrast. It never follows the palette, which is the point: it is the
  # same chrome colour as category_scorecard.qmd and the site navbar.
  .DOC      <- pal_doc_tokens(CONTRAST)
  CHROME    <- unname(.DOC[["doc_navy"]])
  CHROME_ON <- unname(.DOC[["doc_paper"]])

  # The 4px left rule marking where one domain's block of rows ends and the
  # next begins. One grey for all three domains; the row labels say which
  # domain, this rule only says where the block boundary is.
  SPINE <- unname(.DOC[["doc_muted"]])

  # The 1px divider between column labels, drawn on the chrome band, as the
  # band's own ink at low alpha, so it stays visible in either contrast mode
  # without a second hex to re-pick whenever CHROME changes.
  CHROME_RULE <- local({
    ink <- CHROME_ON
    r <- strtoi(substr(ink, 2, 3), 16L)
    g <- strtoi(substr(ink, 4, 5), 16L)
    b <- strtoi(substr(ink, 6, 7), 16L)
    sprintf("rgba(%d,%d,%d,0.35)", r, g, b)
  })

  # Domain LABEL colour in the summary table. `overall` is not a domain and
  # takes doc_navy rather than doc_accent; see the long note in
  # state_scorecard.qmd history (phase 3.4) for why a fourth identity-like
  # colour does not belong there.
  COL_TXT <- list(
    overall = unname(.DOC[["doc_navy"]]),
    cl      = unname(.DOC[["doc_ink"]]),
    cp      = unname(.DOC[["doc_ink"]]),
    we      = unname(.DOC[["doc_ink"]])
  )

  # The :root custom-property block scorecard_state.css and scorecard_v3.css
  # consume, plus the data-adaparc-contrast hook their structural rules key
  # on. Callers emit this with cat() early in the document body, after the
  # linked stylesheet, so at equal specificity it wins.
  doc_header_html <- pal_doc_header(palette, CONTRAST)

  # ── Rankings (higher score = better = lower rank number) ─────────────────
  states_only <- raw |> filter(ABBR != "USA")

  rank_of <- function(col) {
    if (!col %in% names(states_only)) return(NA_integer_)
    vals      <- as.numeric(states_only[[col]])
    state_idx <- which(states_only$ABBR == state_abbr)
    if (length(state_idx) == 0 || is.na(vals[state_idx])) return(NA_integer_)
    as.integer(rank(-vals, ties.method = "min", na.last = "keep")[state_idx])
  }

  n_ranked <- sum(!is.na(as.numeric(states_only[["index_adaparc"]])))

  RNK <- list(
    overall             = rank_of("index_adaparc"),
    cl                  = rank_of("index_rel_community_living"),
    cp                  = rank_of("index_rel_community_participation"),
    we                  = rank_of("index_rel_work_economic"),
    # CL sub-indices
    living_arrangements = rank_of("index_rel_living_arrangements"),
    community_res       = rank_of("index_rel_community_resource"),
    housing_access      = rank_of("index_rel_housing_access"),
    # CP sub-indices
    tech                = rank_of("index_rel_tech"),
    education           = rank_of("index_rel_education"),
    insurance            = rank_of("index_rel_insurance"),
    commute             = rank_of("index_rel_commute"),
    safety              = rank_of("index_rel_safety"),
    # WE sub-indices
    positive_work       = rank_of("index_rel_positive_work"),
    negative_work       = rank_of("index_rel_negative_work"),
    housing_afford      = rank_of("index_rel_housing_affordability")
  )

  # ── Rank pill helper (for banner) ─────────────────────────────────────────
  # The 13/26/39 cut points come from pal_rank_cuts(n_ranked) in
  # scripts/palettes.R rather than being written out here, so the banding
  # stays correct if the number of ranked jurisdictions ever changes.
  qual_info <- function(r) {
    if (is.na(r)) return(list(label = "N/A", cls = ""))
    tier <- pal_tier_of_rank(r, n_ranked)
    list(label = pal_tier_label(tier), cls = pal_tier_class(tier))
  }

  rank_pill <- function(domain_label, rnk, n_total) {
    if (is.na(rnk)) return("")
    qi <- qual_info(rnk)
    paste0(
      '<div class="rank-pill">',
      '<div class="rp-domain">', domain_label, '</div>',
      '<div class="rp-number">#', rnk, '</div>',
      '<div class="rp-of">of ', n_total, '</div>',
      '<div class="rp-qual ', qi$cls, '">', qi$label, '</div>',
      '</div>'
    )
  }

  # ── Qualitative label for a rank ──────────────────────────────────────────
  bar_qual_label <- function(r) {
    pal_tier_label(pal_tier_of_rank(r, n_ranked), na_label = "")
  }

  # ── Rank band legend, printed in the footer ───────────────────────────────
  # Built from the same pal_rank_cuts() the classifiers use, so the bands a
  # reader is told about are the bands actually applied.
  rank_band_note <- function() {
    cuts  <- pal_rank_cuts(n_ranked)
    tiers <- c("excellent", "above", "below", "poor")
    lo    <- c(1L, cuts[1] + 1L, cuts[2] + 1L, cuts[3] + 1L)
    hi    <- c(cuts, as.integer(n_ranked))
    parts <- mapply(function(t, a, b) {
      paste0("<strong>", pal_tier_label(t), "</strong> (", a,
             if (b > a) paste0("–", b) else "", ")")
    }, tiers, lo, hi, USE.NAMES = FALSE)
    paste(parts, collapse = " &nbsp;·&nbsp; ")
  }

  # ── Tier colour ────────────────────────────────────────────────────────────
  # Delegates to scripts/palettes.R. Always use tier_bg()/tier_ink() TOGETHER;
  # tier_bg() without tier_ink() is how white lands on a light fill at 1.3:1.
  tier_bg <- function(r) {
    tier <- pal_tier_of_rank(r, n_ranked)
    if (length(tier) != 1 || is.na(tier)) return(ADA_PARC_PALETTE$na_fill)
    unname(pal_get(PAL)$fill[[tier]])
  }

  tier_ink <- function(r) {
    tier <- pal_tier_of_rank(r, n_ranked)
    if (length(tier) != 1 || is.na(tier)) return(ADA_PARC_PALETTE$na_text)
    pal_on_fill(tier, PAL)
  }

  # Badge rank line, e.g. "Rank #44 of 51 - Poor".
  rank_line <- function(r) {
    if (is.na(r)) return("—")
    paste0(
      "Rank #", r, " of ", n_ranked, " · ",
      '<span class="rank-tier" style="background:', tier_bg(r),
      ';color:', tier_ink(r), '">',
      bar_qual_label(r), '</span>'
    )
  }

  # ── SVG speedometer-style gauge ───────────────────────────────────────────
  make_gauge <- function(score, nat_score, gauge_label = "Score") {

    if (is.na(score)) {
      return(HTML(paste0(
        '<svg width="300" height="210" viewBox="0 0 300 210" xmlns="http://www.w3.org/2000/svg"',
        ' role="img" aria-label="', gauge_label, ': score pending">',
        '<title>', gauge_label, ': score pending</title>',
        '<text x="150" y="130" text-anchor="middle" font-family="EB Garamond,Garamond,Georgia,serif"',
        ' font-size="22" fill="#111111">Score pending</text></svg>'
      )))
    }

    score_c <- min(score,     99.9)
    nat_c   <- if (!is.na(nat_score)) min(nat_score, 99.9) else NA

    cx <- 150; cy <- 148; r <- 108; arc_w <- 26

    xy <- function(val, radius = r) {
      a <- pi * (1 - val / 100)
      list(x = round(cx + radius * cos(a), 2),
           y = round(cy - radius * sin(a), 2))
    }

    arc_seg <- function(v1, v2, color, width = arc_w) {
      p1 <- xy(v1); p2 <- xy(v2)
      large <- if ((v2 - v1) > 50) 1 else 0
      paste0('<path d="M ', p1$x, ' ', p1$y,
             ' A ', r, ' ', r, ' 0 ', large, ' 1 ', p2$x, ' ', p2$y, '"',
             ' fill="none" stroke="', color, '"',
             ' stroke-width="', width, '" stroke-linecap="butt"/>')
    }

    .zone_fill <- pal_fill(4, PAL)   # poor, below, above, excellent
    zone_arcs <- paste0(
      arc_seg(0,   25,  .zone_fill[1]),   # Poor
      arc_seg(25,  50,  .zone_fill[2]),   # Subpar
      arc_seg(50,  75,  .zone_fill[3]),   # Good
      arc_seg(75,  100, .zone_fill[4])    # Excellent
    )

    divider_line <- function(val) {
      inner <- xy(val, r - arc_w / 2 - 1)
      outer <- xy(val, r + arc_w / 2 + 1)
      paste0('<line x1="', inner$x, '" y1="', inner$y,
             '" x2="', outer$x, '" y2="', outer$y,
             '" stroke="white" stroke-width="3"/>')
    }
    dividers <- paste0(divider_line(25), divider_line(50), divider_line(75))

    tick_line <- function(val, major = FALSE) {
      inner_r <- if (major) r - arc_w / 2 - 13 else r - arc_w / 2 - 5
      outer_r <- r - arc_w / 2 - 1
      p1 <- xy(val, inner_r); p2 <- xy(val, outer_r)
      col <- "#111111"
      w   <- if (major) 3 else 1.5
      paste0('<line x1="', p1$x, '" y1="', p1$y,
             '" x2="', p2$x, '" y2="', p2$y,
             '" stroke="', col, '" stroke-width="', w, '"/>')
    }

    majors <- c(0, 25, 50, 75, 100)
    ticks  <- paste0(
      sapply(seq(0, 100, by = 10),
             function(v) tick_line(v, v %in% majors)),
      collapse = ""
    )

    lbl <- function(val, dx = 0, dy = 0) {
      a  <- pi * (1 - val / 100)
      lx <- round(cx + (r - arc_w / 2 - 20) * cos(a) + dx, 1)
      ly <- round(cy - (r - arc_w / 2 - 20) * sin(a) + dy, 1)
      paste0('<text x="', lx, '" y="', ly,
             '" text-anchor="middle"',
             ' font-family="EB Garamond,Garamond,Georgia,serif" font-size="15" fill="#111111">',
             val, '</text>')
    }

    tick_labels <- paste0(
      lbl(0,   dx =  6, dy = 4),
      lbl(25,  dy = -2),
      lbl(50,  dy =  6),
      lbl(75,  dy = -2),
      lbl(100, dx = -6, dy = 4)
    )

    hub_r      <- 11
    angle_s    <- pi * (1 - score_c / 100)
    tip        <- xy(score_c, r - arc_w / 2 - 2)
    base_half  <- 6
    base1_x    <- round(cx + base_half * sin(angle_s), 2)
    base1_y    <- round(cy + base_half * cos(angle_s), 2)
    base2_x    <- round(cx - base_half * sin(angle_s), 2)
    base2_y    <- round(cy - base_half * cos(angle_s), 2)

    needle_html <- paste0(
      '<polygon points="', tip$x, ',', tip$y, ' ',
      base1_x, ',', base1_y, ' ',
      base2_x, ',', base2_y, '"',
      ' fill="#111111" opacity="0.92"/>'
    )

    hub_html <- paste0(
      '<circle cx="', cx, '" cy="', cy, '" r="', hub_r, '"',
      ' fill="white" stroke="#111111" stroke-width="2.5"/>',
      '<circle cx="', cx, '" cy="', cy, '" r="4" fill="#111111"/>'
    )

    nat_marker <- ""

    score_text <- paste0(
      '<text x="', cx, '" y="', cy + 68, '"',
      ' text-anchor="middle"',
      ' font-family="EB Garamond,Garamond,Georgia,serif"',
      ' font-size="52" font-weight="700" fill="#111111">',
      f(score), '</text>',
      '<text x="', cx, '" y="', cy + 88, '"',
      ' text-anchor="middle"',
      ' font-family="EB Garamond,Garamond,Georgia,serif"',
      ' font-size="16" fill="#111111">out of 100</text>'
    )

    nat_label <- if (!is.na(nat_score)) {
      paste0('<text x="', cx, '" y="', cy + 108, '"',
             ' text-anchor="middle"',
             ' font-family="EB Garamond,Garamond,Georgia,serif"',
             ' font-size="16" fill="#111111">US avg: ', f(nat_score), '</text>')
    } else ""

    gauge_desc <- paste0(gauge_label, ": ", f(score), " out of 100")
    if (!is.na(nat_score)) gauge_desc <- paste0(gauge_desc, ". US average: ", f(nat_score))

    HTML(paste0(
      '<svg width="300" height="272" viewBox="0 0 300 272"',
      ' xmlns="http://www.w3.org/2000/svg"',
      ' role="img" aria-label="', gauge_desc, '">',
      '<title>', gauge_desc, '</title>',
      zone_arcs,
      dividers,
      ticks, tick_labels,
      nat_marker,
      needle_html,
      score_text,
      nat_label,
      '</svg>'
    ))
  }

  # ── Horizontal bar chart ───────────────────────────────────────────────────
  # NA-aware: silently drops any label/score/rank where score is NA. Every
  # bar is coloured by ITS OWN tier (fill and chip both call tier_bg()/
  # tier_ink() on the same rank, so they can never disagree). See the long
  # design note this carries forward from state_scorecard.qmd's 2026-08-06
  # history if that reasoning ever needs revisiting.
  make_subindex_bars <- function(labels, state_scores, ranks,
                                 order_by = c("index", "score")) {

    order_by     <- match.arg(order_by)
    keep         <- !is.na(state_scores)
    labels       <- labels[keep]
    state_scores <- state_scores[keep]
    ranks        <- ranks[keep]

    if (length(labels) == 0) {
      return(HTML(
        '<p class="sib-empty">No sub-index scores available for this domain.</p>'
      ))
    }

    if (order_by == "score") {
      o            <- order(state_scores, decreasing = TRUE)
      labels       <- labels[o]
      state_scores <- state_scores[o]
      ranks        <- ranks[o]
    }

    rows <- vapply(seq_along(labels), function(i) {
      sc    <- state_scores[i]
      rk    <- ranks[i]
      lab   <- labels[i]
      pct_w <- sprintf("%.1f", max(min(sc, 100), 0))

      tier <- bar_qual_label(rk)
      tbg  <- tier_bg(rk)
      tink <- tier_ink(rk)

      inside <- sc >= 16

      desc <- paste0(
        lab, ": ", f(sc), " out of 100",
        if (is.na(rk)) "." else
          paste0(". Rank ", rk, " of ", n_ranked, ", ", tier, ".")
      )

      paste0(
        '<div class="sib-row">',
          '<div class="sib-label">', lab, '</div>',
          '<div class="sib-track" role="img" aria-label="', desc, '">',
            '<div class="sib-fill" style="width:', pct_w, '%;background:', tbg, '">',
              if (inside)
                paste0('<span class="sib-value sib-value-in" style="color:',
                       tink, '">', f(sc), '</span>')
              else '',
            '</div>',
            if (!inside)
              paste0('<span class="sib-value sib-value-out" style="left:calc(',
                     pct_w, '% + 8px)">', f(sc), '</span>')
            else '',
          '</div>',
          '<div class="sib-meta" aria-hidden="true">',
            if (is.na(rk)) '<span class="sib-rank">—</span>' else
              paste0('<span class="sib-rank">Rank #', rk, '</span>',
                     '<span class="sib-tier" style="background:', tbg,
                            ';color:', tink, '">', tier, '</span>'),
          '</div>',
        '</div>'
      )
    }, character(1))

    HTML(paste0(
      '<div class="subindex-bars">',
        paste0(rows, collapse = ""),
        '<div class="sib-row sib-axis" aria-hidden="true">',
          '<div></div>',
          '<div class="sib-axis-scale">',
            '<span>0</span><span>25</span><span>50</span><span>75</span><span>100</span>',
          '</div>',
          '<div></div>',
        '</div>',
      '</div>'
    ))
  }

  # ── Score Summary table (all 15 indices) ──────────────────────────────────
  score_cols_tbl <- c(
    "index_adaparc",
    "index_rel_community_living",
      "index_rel_living_arrangements",
      "index_rel_community_resource",
      "index_rel_housing_access",
    "index_rel_community_participation",
      "index_rel_tech",
      "index_rel_education",
      "index_rel_insurance",
      "index_rel_commute",
      "index_rel_safety",
    "index_rel_work_economic",
      "index_rel_positive_work",
      "index_rel_negative_work",
      "index_rel_housing_affordability"
  )

  metric_tbl <- c(
    "Overall ADA-PARC",
    "Community Living",
      "Living Arrangements",
      "Community Resources",
      "Housing Access",
    "Community Participation",
      "Technology Access",
      "Education Access",
      "Insurance Access",
      "Transportation Access",
      "Public Safety",
    "Work & Economic",
      "Positive Work Score",
      "Negative Work Score",
      "Housing Affordability"
  )

  lvl_tbl <- c(
    "overall",
    "domain","sub","sub","sub",
    "domain","sub","sub","sub","sub","sub",
    "domain","sub","sub","sub"
  )

  dom_tbl <- c(
    "overall",
    "cl","cl","cl","cl",
    "cp","cp","cp","cp","cp","cp",
    "we","we","we","we"
  )

  rnk_list <- c(
    RNK$overall,
    RNK$cl,  RNK$living_arrangements, RNK$community_res, RNK$housing_access,
    RNK$cp,  RNK$tech,   RNK$education, RNK$insurance,
             RNK$commute, RNK$safety,
    RNK$we,  RNK$positive_work, RNK$negative_work, RNK$housing_afford
  )

  tbl_df <- tibble(
    metric      = metric_tbl,
    level       = lvl_tbl,
    domain      = dom_tbl,
    state_score = sapply(score_cols_tbl, s),
    rank_n      = rnk_list
  ) |>
    mutate(
      tier = sapply(rank_n, bar_qual_label)
    )

  overall_row <- which(tbl_df$level == "overall")
  dom_rows    <- which(tbl_df$level %in% c("overall", "domain"))
  sub_rows    <- which(tbl_df$level == "sub")
  cl_rows     <- which(tbl_df$domain == "cl")
  cp_rows     <- which(tbl_df$domain == "cp")
  we_rows     <- which(tbl_df$domain == "we")
  block_head_rows <- setdiff(which(tbl_df$level == "domain"), overall_row + 1)
  last_row    <- nrow(tbl_df)

  stripe_rows <- unlist(lapply(
    list(cl_rows, cp_rows, we_rows),
    function(idx) {
      subs <- idx[tbl_df$level[idx] == "sub"]
      subs[seq_along(subs) %% 2 == 0]
    }
  ))

  tier_html <- mapply(
    function(lbl, r) {
      if (is.na(r) || !nzchar(lbl)) return("—")
      paste0(
        '<span class="rank-tier" style="background:', tier_bg(r),
        ';color:', tier_ink(r), '">', lbl, '</span>'
      )
    },
    tbl_df$tier, tbl_df$rank_n, USE.NAMES = FALSE
  )

  summary_gt <- tbl_df |>
    select(metric, state_score, rank_n, tier) |>
    gt() |>
    cols_label(
      metric      = "Index / Sub-Index",
      state_score = state_name,
      rank_n      = "Rank",
      tier        = "Tier"
    ) |>
    fmt_number(columns = state_score, decimals = 1) |>
    fmt(columns = rank_n,
        fns     = function(x) ifelse(is.na(x), "—", paste0("#", as.integer(x)))) |>
    text_transform(
      locations = cells_body(columns = tier),
      fn        = function(x) tier_html
    ) |>
    cols_align(align = "left",  columns = c(metric, tier)) |>
    cols_align(align = "right", columns = c(state_score, rank_n)) |>
    cols_width(
      metric      ~ pct(40),
      state_score ~ pct(16),
      rank_n      ~ pct(14),
      tier        ~ pct(30)
    ) |>
    tab_style(
      style     = cell_text(color = "#111111"),
      locations = cells_body()
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = state_score)
    ) |>
    tab_style(
      style     = cell_fill(color = unname(.DOC[["doc_cream"]])),
      locations = cells_body(rows = stripe_rows)
    ) |>
    tab_style(
      style     = list(cell_text(weight = "bold"),
                       cell_fill(color = unname(.DOC[["doc_tint"]]))),
      locations = cells_body(rows = dom_rows)
    ) |>
    tab_style(
      style     = cell_text(color = COL_TXT$cl),
      locations = cells_body(columns = metric,
                             rows = intersect(cl_rows, dom_rows))
    ) |>
    tab_style(
      style     = cell_text(color = COL_TXT$cp),
      locations = cells_body(columns = metric,
                             rows = intersect(cp_rows, dom_rows))
    ) |>
    tab_style(
      style     = cell_text(color = COL_TXT$we),
      locations = cells_body(columns = metric,
                             rows = intersect(we_rows, dom_rows))
    ) |>
    tab_style(
      # 16px, down from 17px 2026-08-18 — still a clear step above the 15px
      # base so the Overall row keeps its emphasis, but the full table now
      # needs to fit on one printed page (see the tab_options() note below).
      style     = list(cell_text(size = px(16)),
                       cell_fill(color = unname(.DOC[["doc_tint"]]))),
      locations = cells_body(rows = overall_row)
    ) |>
    tab_style(
      style     = cell_text(color = COL_TXT$overall),
      locations = cells_body(columns = metric, rows = overall_row)
    ) |>
    tab_style(
      style     = cell_borders(sides = "bottom", color = CHROME, weight = px(2)),
      locations = cells_body(rows = overall_row)
    ) |>
    tab_style(
      style     = cell_text(indent = px(22)),
      locations = cells_body(columns = metric, rows = sub_rows)
    ) |>
    tab_style(
      style     = cell_borders(sides = "left", color = "#e5ddd3", weight = px(1)),
      locations = cells_body(columns = c(state_score, rank_n, tier))
    ) |>
    tab_style(
      style     = cell_borders(sides = "left", color = CHROME_RULE, weight = px(1)),
      locations = cells_column_labels(
        columns = c(state_score, rank_n, tier))
    ) |>
    tab_style(
      style     = cell_borders(sides = "top", color = "#d8cec2", weight = px(1)),
      locations = cells_body(rows = block_head_rows)
    ) |>
    tab_style(
      style     = cell_borders(sides = "left", color = SPINE, weight = px(4)),
      locations = cells_body(columns = metric,
                             rows = which(tbl_df$domain %in% ADAPARC_DOMAINS))
    ) |>
    tab_style(
      style     = cell_borders(sides = "left", color = CHROME, weight = px(4)),
      locations = cells_body(columns = metric, rows = overall_row)
    ) |>
    tab_style(
      style     = cell_borders(sides = "bottom", color = CHROME, weight = px(2)),
      locations = cells_body(rows = last_row)
    ) |>
    tab_style(
      style     = cell_text(color = CHROME_ON, weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_source_note(
      source_note = md(
        paste0(
          "Scores range 0–100; higher is better. Rankings among all U.S. ",
          "states and territories with available data. Tier follows rank, not ",
          "score, so a state can score well above the national norm and still ",
          "land in a lower tier when the distribution is skewed. Puerto Rico ",
          "may show blank cells for PUMS-sourced indices, which are not ",
          "available at the territory level."
        )
      )
    ) |>
    # Sized down 2026-08-18: at 17px/13px padding, 15 rows plus the roomier
    # heading ran taller than one Letter page in state_scorecard_print.qmd,
    # which forces this card to open on its own page (break-before) and
    # never straddle two (break-inside: avoid) — a card taller than the full
    # page can't satisfy that second rule. These values plus the heading/
    # padding trim in scorecard_state.css bring the whole card back under
    # ~9.8in (Letter minus @page margins) with room to spare; see
    # project_state_scorecard_score_summary_pagination_fix memory for the
    # arithmetic. Still a step up from the pre-2026-08-18 originals (16px /
    # 8px), not a reversion to them.
    tab_options(
      table.width                        = pct(100),
      table.font.names                   = c("EB Garamond", "Georgia", "serif"),
      table.font.size                    = px(15),
      data_row.padding                   = px(8),
      column_labels.font.weight          = "bold",
      column_labels.font.size            = px(13.5),
      column_labels.background.color     = CHROME,
      column_labels.padding              = px(10),
      column_labels.padding.horizontal   = px(10),
      table.border.top.color             = CHROME,
      table.border.top.width             = px(2),
      table.border.bottom.style          = "none",
      table_body.border.top.style        = "none",
      table_body.border.bottom.style     = "none",
      table_body.hlines.color            = "#f2ece4",
      source_notes.font.size             = px(12.5),
      source_notes.background.color      = "#ffffff"
    )

  list(
    state_abbr      = state_abbr,
    state_name      = state_name,
    year_label      = year_label,
    palette         = palette,
    contrast        = CONTRAST,
    s               = s,
    n               = n,
    f               = f,
    n_ranked        = n_ranked,
    RNK             = RNK,
    CHROME          = CHROME,
    CHROME_ON       = CHROME_ON,
    CHROME_RULE     = CHROME_RULE,
    SPINE           = SPINE,
    COL_TXT         = COL_TXT,
    doc_header_html = doc_header_html,
    qual_info       = qual_info,
    rank_pill       = rank_pill,
    bar_qual_label  = bar_qual_label,
    rank_band_note  = rank_band_note,
    tier_bg         = tier_bg,
    tier_ink        = tier_ink,
    rank_line       = rank_line,
    make_gauge      = make_gauge,
    make_subindex_bars = make_subindex_bars,
    summary_gt      = summary_gt
  )
}
