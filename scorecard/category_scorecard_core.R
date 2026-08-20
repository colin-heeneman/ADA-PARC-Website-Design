# ══════════════════════════════════════════════════════════════════════════
# Category (National) Scorecard — shared scoring / tier / presentation logic
#
# Single source of truth for everything that is NOT specific to a particular
# rendered document: which category, the state scores/ranks/quartile tiers
# for it, the choropleth map DATA (and the plain ggplot object underneath
# it), the rankings table, the Top 5 / Bottom 5 cards, and the regional
# breakdown rows.
#
# category_scorecard.qmd (the interactive, on-screen version, map wrapped in
# ggiraph for hover tooltips) and category_scorecard_print.qmd (the branded,
# paginated download, map exported to static SVG) both call
# build_category_scorecard() and get back the SAME scores, the SAME tier
# colours, and the SAME table/card HTML. Before this file existed, the
# download simply copied the on-screen render's output file, so there was
# one copy of this logic; splitting the download into its own document made
# a second copy of the scoring logic an ever-present risk. See the identical
# reasoning in state_scorecard_core.R and, further back, in
# national_topic_bundle() in ADA_PARC.Rmd.
#
# WHAT STAYS OUT OF THIS FILE, ON PURPOSE:
#   - knitr chunk options, showtext/systemfonts font registration: per-
#     document rendering concerns, not scoring logic.
#   - Whether the map is interactive (ggiraph) or static (svglite SVG): that
#     is a presentation choice each document makes about the SAME p_map
#     ggplot object this file returns.
#   - The banner masthead / funding footer / print button / live search box:
#     hand-written per document, matching the convention already established
#     for the National Data and City Data downloads and for
#     state_scorecard_print.qmd (a report type's banner/footer is not shared,
#     so different downloadable products can diverge independently).
#
# To update the underlying data: re-run the data pipeline and copy the new
# index_scores_wide.csv into scorecard/scorecard_data/final/, same as today.
# ══════════════════════════════════════════════════════════════════════════

library(readr)
library(dplyr)
library(ggplot2)
library(gt)
library(maps)
library(mapproj)
library(scales)
library(htmltools)

# ── Tier palette: single source of truth ──────────────────────────────────
local({
  cand <- c(file.path("..", "scripts", "palettes.R"),
            file.path("scripts", "palettes.R"))
  hit  <- cand[file.exists(cand)]
  if (length(hit) == 0) {
    stop("category_scorecard_core.R cannot locate scripts/palettes.R. ",
         "Render with execute_dir set to the scorecard/ directory, ",
         "or from the project root.")
  }
  source(hit[1], local = FALSE)
})

#' Build every score, rank, tier, map object, table, and card the Category
#' Scorecard needs, for one category (CL / CP / WE) under one display
#' setting.
#'
#' @param category  "CL", "CP", or "WE".
#' @param year      Data year label shown on the page.
#' @param palette   Reader's selected palette id, e.g. "heritage".
#' @param contrast  "standard" or "high".
#' @param data_dir  Where scorecard_data/ lives relative to the working
#'                   directory at render time.
#'
#' @return A list. `data_available` gates everything else: when FALSE (a
#'   category with no populated sub-index data yet), only `cfg`, `f`, and
#'   the chrome/heading-style strings are meaningful and callers should show
#'   the "pending" message and stop, exactly as category_scorecard.qmd
#'   already does. When TRUE, also includes: scores_df, nat_overall,
#'   nat_subs, top_row, bot_row, n_above; the map objects us_map, us_outline,
#'   map_labels, legend_labels, and p_map (a plain ggplot, not wrapped in
#'   ggiraph); region_df; top5, bot5, spread_val; the presentation helpers
#'   quartile_col/idx/ink, mix_toward_white, make_perf_row, make_perf_card,
#'   make_region_row; and gt_table, a complete, ready-to-print gt object.
build_category_scorecard <- function(category,
                                     year     = 2024,
                                     palette  = "heritage",
                                     contrast = "standard",
                                     data_dir = "scorecard_data") {

  CONTRAST <- pal_contrast_mode(contrast)
  PAL      <- pal_effective_fill_id(palette, CONTRAST)

  # ═══════════════════════════════════════════════════════════════════════
  # CATEGORY CONFIG — see category_scorecard.qmd's original header comment
  # for the full history of why no category carries a colour literal.
  # ═══════════════════════════════════════════════════════════════════════
  CATS <- list(

    CL = list(
      name        = "Community Living",
      desc        = "The degree to which people with disabilities (aged 18–64) live in community settings rather than nursing homes, correctional facilities, or other institutions, and whether states invest in community-based housing and support resources.",
      col_overall = "index_rel_community_living",
      col_sub     = c("index_rel_living_arrangements",
                      "index_rel_community_resource",
                      "index_rel_housing_access"),
      score_col   = "CL Score",
      sub_cols    = c("Living Arrangements", "Community Resource", "Housing Access"),
      sub_labels  = c("Living Arrangements",
                      "Community Resource",
                      "Housing Access"),
      sub_cols_pending = character(0),
      sub_desc    = c(
        "Composite of four living situation indicators for people with disabilities (aged 18–64): share living at home (private residence), in non-institutional group quarters, in nursing homes, and in correctional facilities. Higher scores reflect greater community integration.",
        "Measures state investment in community-based supports: (1) ratio of Medicaid HCBS to institutional spending, and (2) HCBS spending per disabled Medicaid enrollee.",
        "Disability housing access indicators from HUD Picture of Subsidized Households (POSH): percentage of Housing Choice Voucher and Public Housing households where the head of household has a disability. Higher scores indicate greater access to subsidized housing for people with disabilities."
      ),
      pal         = pal_fill(4, PAL),
      domain      = "cl"
    ),

    CP = list(
      name        = "Community Participation",
      desc        = "Access to the tools and systems that enable full participation in community life — including technology, health insurance, educational attainment, transportation, and public safety.",
      col_overall = "index_rel_community_participation",
      col_sub     = c("index_rel_tech",
                      "index_rel_education",
                      "index_rel_insurance",
                      "index_rel_commute",
                      "index_rel_safety"),
      score_col   = "CP Score",
      sub_cols    = c("Technology", "Education", "Insurance",
                      "Transportation", "Safety"),
      sub_labels  = c("Technology Access",
                      "Education Access",
                      "Insurance Access",
                      "Transportation Access",
                      "Public Safety"),
      sub_cols_pending = character(0),
      sub_desc    = c(
        "Computer ownership and broadband internet access among households with disabilities — increasingly essential for employment, healthcare navigation, and civic engagement.",
        "Educational attainment among adults with disabilities, including high school completion and higher education rates — key drivers of economic and civic participation.",
        "Health insurance coverage among people with disabilities aged 19–64: private insurance, public insurance (Medicare/Medicaid/CHIP), and uninsured rate.",
        "Commute and transportation accessibility for people with disabilities, reflecting ease of access to employment, services, and community resources.",
        "State-level property and violent crime rates per 100,000 population. Note: FL, PA, and NY have lower agency participation rates."
      ),
      pal         = pal_fill(4, PAL),
      domain      = "cp"
    ),

    WE = list(
      name        = "Work & Economic",
      desc        = "Economic inclusion and security for people with disabilities — measuring work outcomes (employment, income, poverty, and labor force participation) and housing affordability.",
      col_overall = "index_rel_work_economic",
      col_sub     = c("index_rel_positive_work",
                      "index_rel_negative_work",
                      "index_rel_housing_affordability"),
      score_col   = "WE Score",
      sub_cols    = c("Positive Work", "Negative Work", "Housing Affordability"),
      sub_labels  = c("Positive Work Score",
                      "Negative Work Score",
                      "Housing Affordability"),
      sub_cols_pending = character(0),
      sub_desc    = c(
        "Employment rate and cost-of-living-adjusted income (individual and household) for people with disabilities. Income adjusted using BEA Regional Price Parities (2022).",
        "Negative work indicators for people with disabilities: not-in-labor-force rate, poverty rate (all ages, derived from age-split ACS counts), and unemployment rate. Higher scores indicate lower rates of these negative outcomes.",
        "SSI as a percent of fair market rent for a one-bedroom apartment, plus disability-specific renter and owner cost burden rates. Note: Puerto Rico missing PUMS data."
      ),
      pal         = pal_fill(4, PAL),
      domain      = "we"
    )
  )

  cfg <- CATS[[category]]

  # ── Page chrome, NEUTRAL ─────────────────────────────────────────────────
  # See category_scorecard.qmd's original header note for the full
  # reasoning: CHROME is doc_navy, shared with the state scorecard and the
  # site navbar, and never follows the palette.
  .DOC      <- pal_doc_tokens(CONTRAST)
  CHROME    <- unname(.DOC[["doc_navy"]])
  CHROME_ON <- unname(.DOC[["doc_paper"]])
  CAT_ON    <- CHROME_ON

  cfg$accent        <- CHROME
  cfg$col_hdr_bg    <- CHROME
  cfg$heading_color <- CHROME
  cfg$banner_dark   <- CHROME
  cfg$banner_light  <- CHROME

  doc_header_html <- pal_doc_header(palette, CONTRAST)

  f <- function(x) ifelse(is.na(x), "—", sprintf("%.1f", as.numeric(x)))

  # ── Inline style strings, shared verbatim by every document ─────────────
  banner_style <- paste0(
    "background: ", cfg$banner_light, "; ",
    "color: ", CAT_ON, "; padding: 2.4rem 3rem 2.2rem; ",
    "border-radius: 12px; margin-bottom: 2rem;"
  )
  heading_style <- paste0(
    "font-family: 'EB Garamond', Garamond, Georgia, serif; ",
    "font-size: 1.6rem; font-weight: 700; color: ", cfg$heading_color, "; ",
    "margin: 0 0 0.3rem; letter-spacing: -0.01em;"
  )
  heading_style_black <- paste0(
    "font-family: 'EB Garamond', Garamond, Georgia, serif; ",
    "font-size: 1.6rem; font-weight: 700; color: #1c2b3a; ",
    "margin: 0 0 0.3rem; letter-spacing: -0.01em;"
  )

  # ── About-this-category list, shared markup fragment ─────────────────────
  # Used both by the early-exit "pending" card and by the full "About This
  # Category" section further down the page, in both documents.
  about_list_html <- function() {
    paste0(
      '<ul class="about-list">',
      paste0(
        '<li><span class="al-name">', cfg$sub_labels, '</span>',
        '<span class="al-desc">', cfg$sub_desc, '</span></li>',
        collapse = "\n"
      ),
      '</ul>'
    )
  }

  # ═══════════════════════════════════════════════════════════════════════
  # DATA
  # ═══════════════════════════════════════════════════════════════════════
  raw     <- read_csv(file.path(data_dir, "final", "index_scores_wide.csv"),
                      show_col_types = FALSE)
  nat_row <- raw |> filter(ABBR == "USA")
  states  <- raw |> filter(ABBR != "USA")

  scores_df <- states |>
    select(GEOID, NAME, ABBR,
           cat_overall = all_of(cfg$col_overall),
           all_of(cfg$col_sub)) |>
    mutate(cat_overall = as.numeric(cat_overall),
           across(all_of(cfg$col_sub), as.numeric)) |>
    filter(!is.na(cat_overall)) |>
    rename_with(~ paste0("sub_", seq_along(cfg$col_sub)),
                all_of(cfg$col_sub)) |>
    arrange(desc(cat_overall)) |>
    mutate(rank = row_number())

  data_available <- nrow(scores_df) > 0

  out <- list(
    category            = category,
    year                = year,
    palette             = palette,
    contrast            = CONTRAST,
    cfg                 = cfg,
    CHROME              = CHROME,
    CHROME_ON           = CHROME_ON,
    CAT_ON              = CAT_ON,
    doc_header_html     = doc_header_html,
    f                   = f,
    banner_style        = banner_style,
    heading_style       = heading_style,
    heading_style_black = heading_style_black,
    about_list_html     = about_list_html,
    data_available      = data_available,
    scores_df           = scores_df
  )

  if (!data_available) return(out)

  # ── Only computed when data are available ────────────────────────────────
  nat_overall <- as.numeric(nat_row[[cfg$col_overall]])
  nat_subs    <- sapply(cfg$col_sub, function(x) as.numeric(nat_row[[x]]))

  top_row <- scores_df |> slice(1)
  bot_row <- scores_df |> slice(n())
  n_above <- sum(scores_df$cat_overall > nat_overall, na.rm = TRUE)

  q_breaks  <- quantile(scores_df$cat_overall, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  min_score <- min(scores_df$cat_overall, na.rm = TRUE)
  max_score <- max(scores_df$cat_overall, na.rm = TRUE)

  quartile_col <- function(score) {
    dplyr::case_when(
      is.na(score)         ~ pal_na(PAL),
      score <= q_breaks[1] ~ cfg$pal[1],
      score <= q_breaks[2] ~ cfg$pal[2],
      score <= q_breaks[3] ~ cfg$pal[3],
      TRUE                 ~ cfg$pal[4]
    )
  }

  quartile_idx <- function(score) {
    dplyr::case_when(
      is.na(score)         ~ NA_integer_,
      score <= q_breaks[1] ~ 1L,
      score <= q_breaks[2] ~ 2L,
      score <= q_breaks[3] ~ 3L,
      TRUE                 ~ 4L
    )
  }

  quartile_ink <- function(score) {
    idx <- quartile_idx(score)
    ifelse(is.na(idx), pal_on_fill("poor", PAL), pal_on_fill(idx, PAL))
  }

  # ── Map data ───────────────────────────────────────────────────────────
  us_map <- map_data("state") |>
    left_join(
      scores_df |> mutate(region = tolower(NAME)),
      by = "region"
    ) |>
    mutate(
      fill_col   = quartile_col(cat_overall),
      border_col = quartile_ink(cat_overall)
    )

  us_outline <- map_data("usa")

  state_centroids <- us_map |>
    group_by(region) |>
    summarise(long = mean(range(long)), lat = mean(range(lat)), .groups = "drop")

  map_labels <- scores_df |>
    mutate(region = tolower(NAME)) |>
    inner_join(state_centroids, by = "region") |>
    mutate(lbl_col = quartile_ink(cat_overall))

  legend_labels <- c(
    paste0("Poor: ",      sprintf("%.1f", min_score),   "–", sprintf("%.1f", q_breaks[1])),
    paste0("Subpar: ",    sprintf("%.1f", q_breaks[1]), "–", sprintf("%.1f", q_breaks[2])),
    paste0("Good: ",      sprintf("%.1f", q_breaks[2]), "–", sprintf("%.1f", q_breaks[3])),
    paste0("Excellent: ", sprintf("%.1f", q_breaks[3]), "–", sprintf("%.1f", max_score))
  )

  us_map_ig <- us_map |>
    mutate(
      tier_text = pal_tier_label(quartile_idx(cat_overall), na_label = "No data"),
      tooltip   = dplyr::case_when(
        is.na(cat_overall) ~ paste0(stringr::str_to_title(region), "\nNo data available"),
        TRUE ~ paste0(
          stringr::str_to_title(region), "\n",
          "Score: ", sprintf("%.1f", cat_overall), "\n",
          "Rank: #", rank, " of ", nrow(scores_df), "\n",
          "Tier: ", tier_text
        )
      ),
      data_id = region
    )

  # p_map is a PLAIN ggplot object, deliberately not wrapped in ggiraph here.
  # category_scorecard.qmd wraps it in girafe() for on-screen hover
  # tooltips; category_scorecard_print.qmd exports it to static SVG via
  # svglite instead, the same technique the National Data map download
  # already uses (see render_national_map() / national_topic_bundle() in
  # ADA_PARC.Rmd). geom_polygon_interactive() below still works fine as a
  # plain (non-interactive) geom_polygon when never passed to girafe(): the
  # tooltip/data_id aesthetics are simply unused mappings in that path.
  p_map <- ggplot(us_map_ig, aes(x = long, y = lat, group = group, fill = fill_col)) +
    geom_polygon(
      data        = us_outline,
      aes(x = long, y = lat, group = group),
      inherit.aes = FALSE,
      fill        = NA,
      color       = "#1c2b3a",
      linewidth   = 1.1
    ) +
    ggiraph::geom_polygon_interactive(
      aes(tooltip = tooltip, data_id = data_id, color = border_col),
      linewidth = 0.35,
      hover_css = "fill-opacity:0.75;stroke-width:2px;"
    ) +
    scale_fill_identity(
      name   = "Performance Tier",
      guide  = guide_legend(
        title.position = "top",
        title.hjust    = 0.5,
        nrow           = 2,
        byrow          = TRUE,
        keywidth       = unit(2.4, "cm"),
        keyheight      = unit(0.7, "cm"),
        label.theme    = element_text(size = 16, color = "#1c2b3a", family = "EB Garamond")
      ),
      breaks = cfg$pal,
      labels = legend_labels
    ) +
    geom_text(
      data = map_labels,
      aes(x = long, y = lat, label = ABBR, color = lbl_col),
      inherit.aes = FALSE,
      size = 2.5, fontface = "bold", show.legend = FALSE
    ) +
    scale_color_identity() +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(x = NULL, y = NULL) +
    theme_void(base_size = 14, base_family = "EB Garamond") +
    theme(
      text             = element_text(family = "EB Garamond"),
      legend.position  = "bottom",
      legend.title     = element_text(size = 17, face = "bold",
                                      color = "#1c2b3a", family = "EB Garamond"),
      legend.text      = element_text(size = 16, color = "#1c2b3a",
                                      family = "EB Garamond"),
      legend.spacing.x = unit(0.4, "cm"),
      legend.key       = element_rect(fill = NA, color = "#1c2b3a", linewidth = 0.6),
      plot.background  = element_blank(),
      panel.background = element_blank()
    )

  # ── Census region lookup ───────────────────────────────────────────────
  census_regions <- c(
    "Connecticut"="Northeast","Maine"="Northeast","Massachusetts"="Northeast",
    "New Hampshire"="Northeast","Rhode Island"="Northeast","Vermont"="Northeast",
    "New Jersey"="Northeast","New York"="Northeast","Pennsylvania"="Northeast",
    "Illinois"="Midwest","Indiana"="Midwest","Iowa"="Midwest",
    "Kansas"="Midwest","Michigan"="Midwest","Minnesota"="Midwest",
    "Missouri"="Midwest","Nebraska"="Midwest","North Dakota"="Midwest",
    "Ohio"="Midwest","South Dakota"="Midwest","Wisconsin"="Midwest",
    "Alabama"="South","Arkansas"="South","Delaware"="South",
    "District of Columbia"="South","Florida"="South","Georgia"="South",
    "Kentucky"="South","Louisiana"="South","Maryland"="South",
    "Mississippi"="South","North Carolina"="South","Oklahoma"="South",
    "South Carolina"="South","Tennessee"="South","Texas"="South",
    "Virginia"="South","West Virginia"="South",
    "Alaska"="West","Arizona"="West","California"="West",
    "Colorado"="West","Hawaii"="West","Idaho"="West",
    "Montana"="West","Nevada"="West","New Mexico"="West",
    "Oregon"="West","Utah"="West","Washington"="West","Wyoming"="West"
  )

  region_df <- scores_df |>
    mutate(region = census_regions[NAME]) |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarise(avg_score = mean(cat_overall, na.rm = TRUE),
              n_states  = n(), .groups = "drop") |>
    arrange(desc(avg_score)) |>
    mutate(region = factor(region, levels = region))

  top5 <- scores_df |> slice(1:min(5, n()))
  bot5 <- scores_df |> slice(max(1, n() - 4):n())
  spread_val <- top5$cat_overall[1] - bot5$cat_overall[nrow(bot5)]

  # ── Colour mixing + performer card helpers ───────────────────────────────
  mix_toward_white <- function(hex, amount) {
    rgb   <- grDevices::col2rgb(hex)
    mixed <- rgb * (1 - amount) + 255 * amount
    grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
  }

  make_perf_row <- function(r, name, score, spot_rank, hdr_color, hdr_ink,
                             badge_tint, badge_ink) {
    is_spot <- r == spot_rank

    rank_style <- if (is_spot) {
      paste0('background:', hdr_color, ';color:', hdr_ink, ';')
    } else {
      paste0('background:', badge_tint, ';color:', badge_ink, ';')
    }

    row_class   <- if (is_spot) 'perf-row perf-row-spotlight' else 'perf-row'
    row_style   <- if (is_spot) paste0(' style="background:', badge_tint, ';"') else ''
    score_style <- if (is_spot) paste0(' style="color:', hdr_color, ';"') else ''

    paste0(
      '<li class="', row_class, '"', row_style, ' aria-label="Rank ', r, ', ', name,
      ', score ', sprintf("%.1f", score), '">',
      '<span class="perf-rank" style="', rank_style, '" aria-hidden="true">#', r, '</span>',
      '<span class="perf-name">', name, '</span>',
      '<span class="perf-score"', score_style, '>', sprintf("%.1f", score), '</span>',
      '</li>'
    )
  }

  make_perf_card <- function(label, df, hdr_color, hdr_ink = "#ffffff",
                              spotlight_index, glyph) {
    card_tint  <- mix_toward_white(hdr_color, 0.965)
    badge_tint <- mix_toward_white(hdr_color, 0.88)
    badge_ink  <- unname(.DOC[["doc_ink"]])
    spot_rank  <- df$rank[spotlight_index]

    rows <- paste0(
      mapply(make_perf_row, df$rank, df$NAME, df$cat_overall,
             MoreArgs = list(spot_rank = spot_rank, hdr_color = hdr_color,
                              hdr_ink = hdr_ink, badge_tint = badge_tint,
                              badge_ink = badge_ink)),
      collapse = ""
    )
    paste0('<div class="perf-card" role="region" aria-label="', label, '">',
           '<div class="perf-header" style="background:', hdr_color,
           ';color:', hdr_ink, ';">',
           '<span class="perf-header-glyph" aria-hidden="true">', glyph, '</span>',
           label, '</div>',
           '<ol class="perf-body" style="list-style:none; margin:0; padding:0; background:',
           card_tint, ';">', rows, '</ol>',
           '</div>')
  }

  performer_cards_html <- function() {
    paste0(
      '<div class="top-bottom-container">',
      make_perf_card("Top 5 States",    top5,
                     pal_identity("we", PAL), pal_on_identity("we", PAL),
                     spotlight_index = 1, glyph = "▲"),
      make_perf_card("Bottom 5 States", bot5,
                     pal_identity("cp", PAL), pal_on_identity("cp", PAL),
                     spotlight_index = nrow(bot5), glyph = "▼"),
      '<div class="spread-stat">',
      '<div class="num">', f(spread_val), ' points</div>',
      '<div class="label">separates the highest state from the lowest</div>',
      '</div>',
      '</div>'
    )
  }

  # ── Regional breakdown rows (HTML, no ggplot — 2026-08-18 redesign) ──────
  make_region_row <- function(region, avg_score, n_states, nat_overall, is_top) {
    delta     <- avg_score - nat_overall
    dir_word  <- if (delta >= 0) "above" else "below"
    row_class <- if (is_top) "region-row is-top" else "region-row"

    aria <- paste0(
      region, ": average score ", sprintf("%.1f", avg_score),
      " across ", n_states, " states, ",
      sprintf("%.1f", abs(delta)), " points ", dir_word,
      " the U.S. average of ", sprintf("%.1f", nat_overall), "."
    )

    paste0(
      '<div class="', row_class, '" role="img" aria-label="', aria, '">',
        '<div class="region-figure" aria-hidden="true">',
          '<div class="region-score">', sprintf("%.1f", avg_score), '</div>',
          '<span class="region-name">', region, '</span>',
        '</div>',
        '<div class="region-bar-track" aria-hidden="true">',
          '<div class="region-bar-fill" style="width:', sprintf("%.2f", avg_score), '%;"></div>',
          '<div class="avg-tick" style="left:', sprintf("%.2f", nat_overall), '%;"></div>',
        '</div>',
      '</div>'
    )
  }

  regional_card_html <- function() {
    rows_html <- paste0(
      vapply(seq_len(nrow(region_df)), function(i) {
        make_region_row(as.character(region_df$region[i]), region_df$avg_score[i],
                         region_df$n_states[i], nat_overall, is_top = (i == 1))
      }, character(1)),
      collapse = ""
    )
    paste0(
      '<div class="region-rows" role="group" aria-label="Average ', cfg$name,
      ' score by U.S. Census region, sorted highest to lowest.">',
      rows_html,
      '</div>',
      '<div class="avg-legend">',
        '<span class="swatch" aria-hidden="true"></span>',
        '<span>US national average&nbsp;<strong>', f(nat_overall), '</strong></span>',
      '</div>'
    )
  }

  # ── Stat pills for the banner ─────────────────────────────────────────────
  stat_pills_html <- function() {
    paste0(
      '::: {.stat-pills}\n\n',
      '::: {.stat-pill}\n',
      '<div class="pill-val">', f(nat_overall), '</div>\n',
      '<div class="pill-lbl">US Average</div>\n',
      ':::\n\n',
      '::: {.stat-pill}\n',
      '<div class="pill-val">', f(top_row$cat_overall), '</div>\n',
      '<div class="pill-lbl">Highest Score</div>\n',
      '<div class="pill-sub">', top_row$NAME, '</div>\n',
      ':::\n\n',
      '::: {.stat-pill}\n',
      '<div class="pill-val">', f(bot_row$cat_overall), '</div>\n',
      '<div class="pill-lbl">Lowest Score</div>\n',
      '<div class="pill-sub">', bot_row$NAME, '</div>\n',
      ':::\n\n',
      '::: {.stat-pill}\n',
      '<div class="pill-val">', n_above, '</div>\n',
      '<div class="pill-lbl">States Above US Avg</div>\n',
      '<div class="pill-sub">of ', nrow(scores_df), ' states &amp; territories</div>\n',
      ':::\n\n',
      ':::\n'
    )
  }

  # ── Rankings table (gt), ready-built ──────────────────────────────────────
  safe_score_col <- gsub("[^A-Za-z0-9]", "_", cfg$score_col)
  safe_sub_cols  <- gsub("[^A-Za-z0-9]", "_", cfg$sub_cols)

  sub_rename_safe <- setNames(paste0("sub_", seq_along(cfg$sub_cols)),
                              safe_sub_cols)

  tbl_data <- scores_df |>
    mutate(Tier = pal_tier_label(quartile_idx(cat_overall), short = TRUE,
                                 na_label = "No data")) |>
    rename(!!safe_score_col := cat_overall) |>
    rename(!!!sub_rename_safe) |>
    select(rank, Tier, State = NAME, Abbr = ABBR,
           all_of(safe_score_col),
           all_of(safe_sub_cols))

  score_tier_idx <- quartile_idx(tbl_data[[safe_score_col]])
  tier_rows <- lapply(1:4, function(k) which(score_tier_idx == k))

  score_cols <- c(safe_score_col, safe_sub_cols)

  label_list <- as.list(c(
    setNames("Rank",               "rank"),
    setNames("Tier",               "Tier"),
    setNames("State / Territory",  "State"),
    setNames("Abbrev.",            "Abbr"),
    setNames(cfg$score_col,        safe_score_col),
    setNames(cfg$sub_cols,         safe_sub_cols)
  ))

  nat_note_parts <- paste0(f(nat_subs), " ", cfg$sub_cols)
  nat_note <- paste0(
    "**US National Average:** ",
    f(nat_overall), " overall",
    paste0(" &nbsp;·&nbsp; ", nat_note_parts, collapse = "")
  )

  # build_rankings_gt() is a FUNCTION, not a single gt object, so
  # category_scorecard_print.qmd can call it once per printed page of the
  # rankings table -- each call produces its own real, separate gt table
  # (with its own <thead>), rather than one 51-row gt table that relies on
  # the browser fragmenting a single <table> and CSS repeating its header on
  # each resulting page. That CSS-repeat approach is what
  # category_scorecard_print.qmd used until 2026-08-19; a rendered PDF
  # showed WebKit dropping the header entirely from row 21 on, matching the
  # same WebKit thead-repeat gap national_topic.qmd's own comments already
  # document and route around (there, by hand-splitting into one <table>
  # per page). category_scorecard.qmd (the on-screen version) still calls
  # this once, for the full 51-row table, so its own output is unchanged --
  # see `gt_table` below.
  # drop_abbrev = TRUE removes the two-letter "Abbrev." column. Used ONLY by
  # category_scorecard_print.qmd: on paper the state name is already spelled
  # out in the adjacent column, so the code carries no information a print
  # reader needs, while costing ~7% of the printable table width that the
  # sub-index columns badly need -- their labels (e.g. "Living
  # Arrangements") were being clipped mid-word in Colin's exported PDF.
  # The on-screen table keeps the column: it costs nothing there (that
  # table may horizontal-scroll) and helps anyone cross-referencing codes.
  #
  # NOTE for whoever edits the print CSS next: the print-only
  # `.gt_table col:nth-child(n)` width rules in
  # category_scorecard_print.qmd are POSITIONAL, so dropping this column
  # shifts every column after it one place left. The two must change
  # together or the widths land on the wrong columns.
  build_rankings_gt <- function(df_chunk, tier_rows_chunk,
                                 show_title_bar = TRUE,
                                 show_source_note = TRUE,
                                 drop_abbrev = FALSE) {
    if (drop_abbrev) df_chunk <- dplyr::select(df_chunk, -Abbr)

    # Both derived from df_chunk's ACTUAL columns rather than hard-coded, so
    # neither cols_label() nor cols_align() can be handed a column that
    # drop_abbrev just removed (gt errors on an unknown column rather than
    # ignoring it).
    labels_used <- label_list[names(label_list) %in% names(df_chunk)]
    center_cols <- intersect(
      c("rank", "Tier", "Abbr", safe_score_col, safe_sub_cols),
      names(df_chunk)
    )

    width_list <- list(rank ~ px(55), Tier ~ px(95), State ~ px(155))
    if (!drop_abbrev) width_list <- c(width_list, list(Abbr ~ px(48)))
    width_list <- c(width_list, list(everything() ~ px(105)))

    gt_obj <- df_chunk |>
      gt() |>
      fmt_number(columns = all_of(score_cols), decimals = 1) |>
      sub_missing(
        columns      = all_of(score_cols),
        missing_text = html('<span aria-label="Data not available">—</span>')
      ) |>
      cols_label(.list = labels_used) |>
      cols_width(.list = width_list) |>
      cols_align(align = "center", columns = all_of(center_cols)) |>
      cols_align(align = "left", columns = State) |>
      tab_style(
        style     = list(cell_fill(color = pal_fill(4, PAL)[1]),
                         cell_text(color = pal_on_fill(1L, PAL), weight = "bold")),
        locations = cells_body(columns = all_of(safe_score_col),
                               rows    = tier_rows_chunk[[1]])
      ) |>
      tab_style(
        style     = list(cell_fill(color = pal_fill(4, PAL)[2]),
                         cell_text(color = pal_on_fill(2L, PAL), weight = "bold")),
        locations = cells_body(columns = all_of(safe_score_col),
                               rows    = tier_rows_chunk[[2]])
      ) |>
      tab_style(
        style     = list(cell_fill(color = pal_fill(4, PAL)[3]),
                         cell_text(color = pal_on_fill(3L, PAL), weight = "bold")),
        locations = cells_body(columns = all_of(safe_score_col),
                               rows    = tier_rows_chunk[[3]])
      ) |>
      tab_style(
        style     = list(cell_fill(color = pal_fill(4, PAL)[4]),
                         cell_text(color = pal_on_fill(4L, PAL), weight = "bold")),
        locations = cells_body(columns = all_of(safe_score_col),
                               rows    = tier_rows_chunk[[4]])
      ) |>
      tab_style(
        style     = cell_text(weight = "bold", size = px(15)),
        locations = cells_body(columns = rank)
      ) |>
      tab_style(
        style     = cell_borders(sides = "left", color = cfg$accent, weight = px(4)),
        locations = cells_body(rows = rank <= 3)
      ) |>
      tab_caption(
        caption = md(paste0(
          cfg$name, " — All States Ranked 1–", nrow(tbl_data),
          " by overall score (",  year, " ACS Data)"
        ))
      ) |>
      tab_options(
        table.width                     = pct(100),
        table.font.names                = c("EB Garamond", "Georgia", "serif"),
        table.font.size                 = px(16),
        data_row.padding                = px(5),
        heading.background.color        = cfg$accent,
        heading.title.font.size         = px(18),
        heading.subtitle.font.size      = px(13),
        column_labels.background.color  = cfg$col_hdr_bg,
        column_labels.font.size         = px(13),
        row.striping.include_table_body = TRUE,
        row.striping.background_color   = "#f8f8fc",
        source_notes.font.size          = px(13),
        source_notes.background.color   = "#f8f8fc",
        table.border.top.color          = cfg$accent,
        table.border.top.width          = px(3),
        quarto.disable_processing       = TRUE
      ) |>
      tab_style(
        style     = cell_text(color = CAT_ON, weight = "bold"),
        locations = cells_column_labels(everything())
      )

    # Decorative title bar (tab_header) and the "US National Average"
    # source note are each shown once for the whole ranked list, not on
    # every print-page chunk -- callers set these FALSE on continuation
    # chunks. The on-screen call below leaves both TRUE, same as before.
    if (show_title_bar) {
      gt_obj <- gt_obj |>
        tab_header(
          title    = md(paste0("**", cfg$name, "** — All States Ranked")),
          subtitle = md(paste0(
            "Ranked 1–", nrow(tbl_data),
            " by overall ", cfg$name, " score · ",
            year, " ACS Data"
          ))
        ) |>
        tab_style(
          style     = cell_text(color = CAT_ON),
          locations = cells_title(groups = c("title", "subtitle"))
        )
    }

    if (show_source_note) {
      gt_obj <- gt_obj |>
        tab_source_note(source_note = md(nat_note)) |>
        tab_style(
          style     = cell_text(color = "#111111"),
          locations = cells_source_notes()
        )
    }

    gt_obj
  }

  # tier_rows_for(idx): the same tier-fill row lookup as `tier_rows` below,
  # but relative to an arbitrary slice of tbl_data's rows (idx), expressed
  # in that slice's own 1..n row positions -- what cells_body(rows = ...)
  # needs when df_chunk is a subset rather than the full table. Used by
  # category_scorecard_print.qmd to build each print-page chunk.
  tier_rows_for <- function(idx) {
    st <- score_tier_idx[idx]
    lapply(1:4, function(k) which(st == k))
  }

  gt_table <- build_rankings_gt(tbl_data, tier_rows,
                                show_title_bar   = TRUE,
                                show_source_note = TRUE)

  c(out, list(
    tbl_data              = tbl_data,
    build_rankings_gt     = build_rankings_gt,
    tier_rows_for         = tier_rows_for,
    nat_overall           = nat_overall,
    nat_subs              = nat_subs,
    top_row               = top_row,
    bot_row               = bot_row,
    n_above               = n_above,
    q_breaks              = q_breaks,
    min_score             = min_score,
    max_score             = max_score,
    quartile_col          = quartile_col,
    quartile_idx          = quartile_idx,
    quartile_ink          = quartile_ink,
    us_map                = us_map,
    us_outline            = us_outline,
    map_labels            = map_labels,
    legend_labels         = legend_labels,
    p_map                 = p_map,
    region_df             = region_df,
    top5                  = top5,
    bot5                  = bot5,
    spread_val            = spread_val,
    mix_toward_white      = mix_toward_white,
    make_perf_row         = make_perf_row,
    make_perf_card        = make_perf_card,
    performer_cards_html  = performer_cards_html,
    make_region_row       = make_region_row,
    regional_card_html    = regional_card_html,
    stat_pills_html       = stat_pills_html,
    gt_table              = gt_table
  ))
}
