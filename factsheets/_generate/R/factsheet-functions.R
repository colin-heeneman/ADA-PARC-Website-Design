# =============================================================================
# factsheet-functions.R
# Shared builder library for ADA-PARC national factsheets.
#
# Purpose: reverse-engineered, reproducible generator for the hand-built
# factsheet HTML files in /factsheets. Each factsheet is assembled from shared
# parts (CSS base, D3 map renderer, header, footer) plus per-factsheet content.
#
# Design notes
#   - Output is bespoke, self-contained HTML (matching the original lean files),
#     NOT a Quarto-themed document. The generator QMD writes these files as a
#     side effect and renders a validation report.
#   - Tier assignment is equal-count-ish quartiles via quantile(type = 7) and
#     findInterval, matching the original ADA-PARC method. Direction (whether a
#     high or low value is "Excellent") is per indicator.
#   - Jurisdiction scope is the 50 states + DC (USA and Puerto Rico excluded),
#     matching the published factsheets.
#
# This file is sourced by generate-factsheets.qmd.
# =============================================================================

# ---- paths -------------------------------------------------------------------

# Directory holding shared assets (factsheet-base.css, adaparc-map.js).
# Resolved relative to this file so the library is location-independent.
fs_assets_dir <- function() {
  here::here("factsheets", "_generate", "assets")
}

fs_read_asset <- function(filename) {
  readChar(
    file.path(fs_assets_dir(), filename),
    nchars = file.info(file.path(fs_assets_dir(), filename))$size,
    useBytes = TRUE
  )
}

# Directory holding the ADA-PARC brand marks (provisional identity, pending
# approval from the ADA centres). www/brand is the single source of truth,
# shared with the Shiny home page; the fact sheets read from it rather than
# keeping a second copy under assets/, so an approved revision drops in once.
fs_brand_dir <- function() {
  here::here("www", "brand")
}

# Brand marks as base64 data URIs.
#
# Two reasons this is not a relative <img src>. First, fs_read_asset() uses
# readChar(), which cannot carry PNG bytes. Second, and the deciding one, each
# fact sheet is a self-contained single file that is both served from
# www/factsheets and downloaded by users; a relative path to ../brand/ breaks
# the moment the file leaves the site. Base64 keeps the mark with the document.
#
# Cached because fs_header() is called once per fact sheet and each call would
# otherwise re-read and re-encode the same two PNGs.
.fs_brand_uri_cache <- new.env(parent = emptyenv())

fs_brand_data_uri <- function(filename) {
  if (!is.null(.fs_brand_uri_cache[[filename]])) {
    return(.fs_brand_uri_cache[[filename]])
  }
  path <- file.path(fs_brand_dir(), filename)
  if (!file.exists(path)) {
    stop("Brand mark not found: ", path,
         "\nExpected www/brand to hold PARC_{blue,white}_{icon,text}.png.",
         call. = FALSE)
  }
  uri <- base64enc::dataURI(file = path, mime = "image/png")
  .fs_brand_uri_cache[[filename]] <- uri
  uri
}

# ---- value formatting --------------------------------------------------------

# Percentages are stored in the data on a 0-100 scale already.
fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "N/A", paste0(formatC(x, format = "f", digits = digits), "%"))
}

fmt_count <- function(x) {
  # format = "f", digits = 0 rather than "d" to avoid the same 32-bit integer
  # overflow that affected fmt_dollars_full (counts are small today, but this
  # keeps the formatter safe for any future large-count indicator).
  ifelse(is.na(x), "N/A",
         formatC(round(x), format = "f", digits = 0, big.mark = ","))
}

fmt_dollars_full <- function(x) {
  # format = "f", digits = 0 (not "d"): "d" coerces to 32-bit integer and
  # overflows to NA for values above 2,147,483,647. HCBS state totals run into
  # the billions, so the integer format silently produced "$NA".
  ifelse(is.na(x), "N/A",
         paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ",")))
}

# Abbreviated dollars for the map tooltip / data object, e.g. $24.4B, $369M.
fmt_dollars_abbr <- function(x) {
  if (is.na(x)) return("N/A")
  ax <- abs(x)
  if (ax >= 1e9) {
    paste0("$", formatC(x / 1e9, format = "f", digits = 1), "B")
  } else if (ax >= 1e6) {
    paste0("$", formatC(round(x / 1e6), format = "d"), "M")
  } else if (ax >= 1e3) {
    paste0("$", formatC(round(x / 1e3), format = "d"), "K")
  } else {
    paste0("$", formatC(round(x), format = "d"))
  }
}

# ---- tier assignment ---------------------------------------------------------

FS_TIERS <- c("excellent", "above", "below", "poor")

# Tier display labels. The performance set is the default and matches the
# published factsheets. The magnitude set is for indicators where a high or low
# value is not better or worse, only larger or smaller; it keeps the same four
# color bins (so the map still reads as a sequential ramp) while removing the
# value judgment from every label. See fs_indicator_section(direction =
# "magnitude"). The keys must stay as FS_TIERS because the CSS and the map
# renderer both key their colors on those names.
FS_TIER_LABELS_PERF <- c(excellent = "Excellent", above = "Above Average",
                         below = "Below Average", poor = "Poor")

FS_TIER_LABELS_MAGNITUDE <- c(excellent = "Highest quarter",
                              above     = "Second highest quarter",
                              below     = "Second lowest quarter",
                              poor      = "Lowest quarter")

# Quartile breakpoints (type-7, matching base R quantile defaults).
# Pass a pooled vector to share one scale across two series (e.g. poverty
# with-disability and without-disability).
fs_tier_breaks <- function(x) {
  stats::quantile(x[!is.na(x)], probs = seq(0, 1, by = 0.25),
                  type = 7, names = FALSE)
}

# Assign tiers to a numeric vector.
#   direction = "high_good": highest values are Excellent (e.g. HCBS spending).
#   direction = "low_good" : lowest values are Excellent (e.g. poverty, nursing).
#   breaks: optional precomputed breakpoints (for a shared/pooled scale).
# Returns a character vector of tier names, NA preserved for missing values.
#   direction = "magnitude": binned like high_good (highest values get the
#     darkest bin) but paired with FS_TIER_LABELS_MAGNITUDE so no label claims
#     one end is better. Use for indicators with no defensible direction.
fs_assign_tiers <- function(x, direction = c("high_good", "low_good", "magnitude"),
                            breaks = NULL) {
  direction <- match.arg(direction)
  if (direction == "magnitude") direction <- "high_good"
  if (is.null(breaks)) breaks <- fs_tier_breaks(x)

  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x)

  # findInterval with all.inside collapses values into bins 1..4
  # (bin 1 = lowest quartile ... bin 4 = highest quartile).
  bin <- findInterval(x[ok], breaks, rightmost.closed = TRUE, all.inside = TRUE)

  labels <- if (direction == "high_good") {
    c("poor", "below", "above", "excellent")   # high value -> excellent
  } else {
    c("excellent", "above", "below", "poor")    # low value  -> excellent
  }
  out[ok] <- labels[bin]
  out
}

# Sort order WITHIN a tier table: best value first.
#   high_good -> descending; low_good -> ascending.
fs_within_tier_order <- function(values, direction) {
  if (direction %in% c("high_good", "magnitude")) order(-values) else order(values)
}

# ---- legend range labels -----------------------------------------------------
# Turn quartile breakpoints into the per-tier range strings shown in a legend.
#   breaks: length-5 vector (min, q25, q50, q75, max) from fs_tier_breaks().
#
# Closed style (used by poverty): every tier shows a "lo-hi" band, lowest band
# is Excellent (direction = low_good).
fs_ranges_closed <- function(breaks, digits = 0, unit = "%") {
  b <- formatC(round(breaks, digits), format = "f", digits = digits)
  c(excellent = paste0(b[1], unit, "-", b[2], unit),
    above     = paste0(b[2], unit, "-", b[3], unit),
    below     = paste0(b[3], unit, "-", b[4], unit),
    poor      = paste0(b[4], unit, "-", b[5], unit))
}

# Open-extreme style (used by housing): best/worst tiers are open-ended,
# highest band is Excellent (direction = high_good).
fs_ranges_open_high <- function(breaks, digits = 0, unit = "%") {
  b <- formatC(round(breaks, digits), format = "f", digits = digits)
  c(excellent = paste0("&gt;", b[4], unit),
    above     = paste0(b[3], unit, "-", b[4], unit),
    below     = paste0(b[2], unit, "-", b[3], unit),
    poor      = paste0("&lt;", b[2], unit))
}

# ---- disability / no-disability comparison table ----------------------------
# Three-column alphabetical table (State / With Disability / No Disability).
# Every factsheet indicator carries a disabled and a non-disabled series, so
# this shape is shared across all of them. Originally written for the poverty
# sheet as fs_poverty_table(); generalized here, with the old name kept as an
# alias so the poverty build chunk keeps working unchanged.
#
#   fmt         formatter applied to both series (default fmt_pct)
#   flag_fn     optional function(value) -> logical. Rows where it returns TRUE
#               get flag_class on the with-disability cell. The poverty sheet
#               uses this to mark rates above 25 percent.
#   flag_class  CSS class applied when flag_fn is TRUE
fs_compare_table <- function(caption, aria_label, names, dis, nod,
                             col_dis = "With Disability",
                             col_nod = "No Disability",
                             fmt = function(x) fmt_pct(x, 1),
                             flag_fn = NULL,
                             flag_class = "high-poverty") {
  rows <- vapply(seq_along(names), function(i) {
    flagged <- !is.null(flag_fn) && !is.na(dis[i]) && isTRUE(flag_fn(dis[i]))
    cls <- if (flagged) paste0(' class="', flag_class, '"') else ""
    paste0("            <tr><td>", fs_esc(names[i]), "</td><td", cls, ">",
           fmt(dis[i]), "</td><td>", fmt(nod[i]), "</td></tr>")
  }, character(1))
  paste0(
'      <div class="state-table-wrapper">
        <table aria-label="', fs_esc(aria_label), '">
          <caption>', caption, '</caption>
          <thead>
            <tr>
              <th scope="col">State</th>
              <th scope="col" class="disability-col">', col_dis, '</th>
              <th scope="col">', col_nod, '</th>
            </tr>
          </thead>
          <tbody>
', paste(rows, collapse = "\n"), '
          </tbody>
        </table>
      </div>')
}

# Deprecated alias. Retained so the existing poverty build chunk renders
# byte-identical output while the sheets migrate to fs_compare_table().
fs_poverty_table <- function(caption, aria_label, names, dis, nod,
                             high_thresh = 25) {
  fs_compare_table(caption = caption, aria_label = aria_label, names = names,
                   dis = dis, nod = nod,
                   flag_fn = function(v) v > high_thresh,
                   flag_class = "high-poverty")
}

# ---- Community Living derived measures ---------------------------------------
# Every Community Living measure shares one denominator, the total population
# with a disability (pop_grp_total_dis), so the residence categories form an
# exact partition:
#
#   living at home + non-institutional group quarters + institutional = 100%
#
# and the institutional share decomposes again into nursing and medical
# facilities, correctional facilities, and a small "other institutional"
# residual (psychiatric hospitals, juvenile facilities). That means the
# institutional total, which has no variable of its own in dict_vars.csv, is
# derivable exactly from variables that do exist. No pipeline change needed.
#
# Note the contrast with the age-specific nursing variables
# (pct_grp_dis_18to64_nursing, pct_grp_dis_65plus_nursing). Those use
# age-specific denominators (pop_grp_18to64_dis, pop_grp_65plus_dis), so they
# are NOT on this common base and must not be added to or compared against the
# measures returned here. They are carried through separately, clearly labeled,
# for the supplementary age table.
#
# Returns the input with the derived columns appended, for both the disabled and
# non-disabled series. Stops loudly if the partition does not hold, so a future
# vintage with changed variable definitions fails at build time rather than
# publishing a decomposition that does not add up.
fs_cl_measures <- function(states_df, tol = 0.01) {

  required <- c("pop_grp_total_dis", "pop_grp_home_dis",
                "pop_grp_noninstgroupquarters_total_dis",
                "pop_grp_nursing_18to64_dis", "pop_grp_nursing_65plus_dis",
                "pop_grp_corrections_total_dis",
                "pop_grp_total_nodis", "pop_grp_home_nodis",
                "pop_grp_noninstgroupquarters_total_nodis",
                "pop_grp_nursing_18to64_nodis", "pop_grp_nursing_65plus_nodis",
                "pop_grp_corrections_total_nodis")
  missing <- setdiff(required, names(states_df))
  if (length(missing) > 0) {
    stop("fs_cl_measures: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  pct <- function(num, den) num / den * 100

  out <- states_df
  bd  <- out$pop_grp_total_dis
  bn  <- out$pop_grp_total_nodis

  # Institutional counts as the residual of the partition.
  out$cl_inst_n_dis <- bd - out$pop_grp_home_dis -
    out$pop_grp_noninstgroupquarters_total_dis
  out$cl_inst_n_nodis <- bn - out$pop_grp_home_nodis -
    out$pop_grp_noninstgroupquarters_total_nodis

  out$cl_nursing_n_dis   <- out$pop_grp_nursing_18to64_dis +
    out$pop_grp_nursing_65plus_dis
  out$cl_nursing_n_nodis <- out$pop_grp_nursing_18to64_nodis +
    out$pop_grp_nursing_65plus_nodis

  out$cl_other_inst_n_dis   <- out$cl_inst_n_dis -
    out$cl_nursing_n_dis - out$pop_grp_corrections_total_dis
  out$cl_other_inst_n_nodis <- out$cl_inst_n_nodis -
    out$cl_nursing_n_nodis - out$pop_grp_corrections_total_nodis

  # Shares on the common base.
  out$cl_home_dis        <- pct(out$pop_grp_home_dis, bd)
  out$cl_nonintgq_dis    <- pct(out$pop_grp_noninstgroupquarters_total_dis, bd)
  out$cl_community_dis   <- out$cl_home_dis + out$cl_nonintgq_dis
  out$cl_inst_dis        <- pct(out$cl_inst_n_dis, bd)
  out$cl_nursing_dis     <- pct(out$cl_nursing_n_dis, bd)
  out$cl_corrections_dis <- pct(out$pop_grp_corrections_total_dis, bd)
  out$cl_other_inst_dis  <- pct(out$cl_other_inst_n_dis, bd)

  out$cl_home_nodis        <- pct(out$pop_grp_home_nodis, bn)
  out$cl_nonintgq_nodis    <- pct(out$pop_grp_noninstgroupquarters_total_nodis, bn)
  out$cl_community_nodis   <- out$cl_home_nodis + out$cl_nonintgq_nodis
  out$cl_inst_nodis        <- pct(out$cl_inst_n_nodis, bn)
  out$cl_nursing_nodis     <- pct(out$cl_nursing_n_nodis, bn)
  out$cl_corrections_nodis <- pct(out$pop_grp_corrections_total_nodis, bn)
  out$cl_other_inst_nodis  <- pct(out$cl_other_inst_n_nodis, bn)

  # ---- guards ----
  # 1. The three residence categories must sum to 100 on both series.
  for (s in c("dis", "nodis")) {
    total <- out[[paste0("cl_home_", s)]] +
             out[[paste0("cl_nonintgq_", s)]] +
             out[[paste0("cl_inst_", s)]]
    if (any(abs(total - 100) > tol, na.rm = TRUE)) {
      stop("fs_cl_measures: residence shares do not sum to 100 for the '", s,
           "' series (max deviation ",
           round(max(abs(total - 100), na.rm = TRUE), 4), " points). ",
           "The denominator assumption behind this factsheet no longer holds.")
    }
  }

  # 2. No negative residuals. A negative institutional or other-institutional
  #    count means the source variables have drifted apart.
  for (v in c("cl_inst_n_dis", "cl_inst_n_nodis",
              "cl_other_inst_n_dis", "cl_other_inst_n_nodis")) {
    if (any(out[[v]] < 0, na.rm = TRUE)) {
      stop("fs_cl_measures: negative derived count in ", v,
           " for ", sum(out[[v]] < 0, na.rm = TRUE), " state(s).")
    }
  }

  # 3. Components must not exceed the institutional total they belong to.
  if (any(out$cl_nursing_n_dis + out$pop_grp_corrections_total_dis >
          out$cl_inst_n_dis + 1e-6, na.rm = TRUE)) {
    stop("fs_cl_measures: nursing plus correctional exceeds the institutional ",
         "total for at least one state.")
  }

  out
}

# ---- Employment derived measures ---------------------------------------------
# ACS Table C18120 splits the civilian noninstitutionalized population aged 18
# to 64 into three mutually exclusive states: employed, unemployed, and not in
# the labor force. Each share is taken over the sum of the three cells for the
# same disability group, so within each group they sum to exactly 100.
#
# These values were independently reconciled against a fresh Census API pull in
# July 2026 (scripts/audit_employment_acs.R, zero discrepancies), so the guards
# below are regression protection rather than a first check.
#
# Two additional rates are derived here because they are the figures usually
# quoted and they use DIFFERENT denominators from the three mapped shares:
#
#   labor force participation = (employed + unemployed) / all in age group
#   unemployment rate         = unemployed / (employed + unemployed)
#
# The unemployment rate in particular is not the same as the "unemployed" share
# on the maps: nationally 10.9% against 5.3%, because so many working-age people
# with disabilities are outside the labor force altogether. Conflating the two
# is the most common way this data is misread, so both are surfaced in a
# supplementary table with their denominators stated.
fs_emp_measures <- function(states_df, tol = 0.01) {

  required <- c("pop_cni_18to64_employed_dis", "pop_cni_18to64_unemployed_dis",
                "pop_cni_18to64_notlabor_dis",
                "pop_cni_18to64_employed_nodis", "pop_cni_18to64_unemployed_nodis",
                "pop_cni_18to64_notlabor_nodis",
                "pct_cni_18to64_employed_dis", "pct_cni_18to64_unemployed_dis",
                "pct_cni_18to64_notlabor_dis",
                "pct_cni_18to64_employed_nodis", "pct_cni_18to64_nodis_unemployed",
                "pct_cni_18to64_notlabor_nodis")
  missing <- setdiff(required, names(states_df))
  if (length(missing) > 0) {
    stop("fs_emp_measures: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  out <- states_df

  # C18120 universe totals, per disability group.
  out$emp_base_dis <- out$pop_cni_18to64_employed_dis +
    out$pop_cni_18to64_unemployed_dis + out$pop_cni_18to64_notlabor_dis
  out$emp_base_nodis <- out$pop_cni_18to64_employed_nodis +
    out$pop_cni_18to64_unemployed_nodis + out$pop_cni_18to64_notlabor_nodis

  # Alias the pipeline percentages under consistent names. Note the pipeline's
  # non-disabled unemployment column breaks the naming pattern used by the other
  # five (pct_cni_18to64_nodis_unemployed, not ..._unemployed_nodis); aliasing
  # here keeps that irregularity out of the factsheet build code.
  out$emp_employed_dis     <- out$pct_cni_18to64_employed_dis
  out$emp_unemployed_dis   <- out$pct_cni_18to64_unemployed_dis
  out$emp_notlabor_dis     <- out$pct_cni_18to64_notlabor_dis
  out$emp_employed_nodis   <- out$pct_cni_18to64_employed_nodis
  out$emp_unemployed_nodis <- out$pct_cni_18to64_nodis_unemployed
  out$emp_notlabor_nodis   <- out$pct_cni_18to64_notlabor_nodis

  # Labor force participation and the conventional unemployment rate.
  lf_dis   <- out$pop_cni_18to64_employed_dis + out$pop_cni_18to64_unemployed_dis
  lf_nodis <- out$pop_cni_18to64_employed_nodis + out$pop_cni_18to64_unemployed_nodis

  out$emp_lfp_dis    <- lf_dis   / out$emp_base_dis   * 100
  out$emp_lfp_nodis  <- lf_nodis / out$emp_base_nodis * 100
  out$emp_urate_dis   <- out$pop_cni_18to64_unemployed_dis   / lf_dis   * 100
  out$emp_urate_nodis <- out$pop_cni_18to64_unemployed_nodis / lf_nodis * 100

  # ---- guards ----
  # 1. The three categories must sum to 100 within each disability group.
  for (s in c("dis", "nodis")) {
    total <- out[[paste0("emp_employed_", s)]] +
             out[[paste0("emp_unemployed_", s)]] +
             out[[paste0("emp_notlabor_", s)]]
    if (any(abs(total - 100) > tol, na.rm = TRUE)) {
      stop("fs_emp_measures: employment shares do not sum to 100 for the '", s,
           "' series (max deviation ",
           round(max(abs(total - 100), na.rm = TRUE), 4), " points).")
    }
  }

  # 2. The pipeline percentages must reproduce from the counts. This catches a
  #    numerator and denominator drifting apart in a future vintage.
  checks <- list(
    c("pop_cni_18to64_employed_dis",     "emp_base_dis",   "emp_employed_dis"),
    c("pop_cni_18to64_unemployed_dis",   "emp_base_dis",   "emp_unemployed_dis"),
    c("pop_cni_18to64_notlabor_dis",     "emp_base_dis",   "emp_notlabor_dis"),
    c("pop_cni_18to64_employed_nodis",   "emp_base_nodis", "emp_employed_nodis"),
    c("pop_cni_18to64_unemployed_nodis", "emp_base_nodis", "emp_unemployed_nodis"),
    c("pop_cni_18to64_notlabor_nodis",   "emp_base_nodis", "emp_notlabor_nodis"))
  for (ck in checks) {
    err <- max(abs(out[[ck[1]]] / out[[ck[2]]] * 100 - out[[ck[3]]]), na.rm = TRUE)
    if (err > tol) {
      stop("fs_emp_measures: ", ck[3], " does not reproduce from its counts ",
           "(max deviation ", round(err, 4), " points).")
    }
  }

  # 3. Sanity bands. Observed 2024 ranges are roughly 34-56% employed and
  #    6-19% unemployment rate for people with disabilities; these bounds are
  #    deliberately wide, catching a structural break rather than a real shift.
  if (any(out$emp_employed_dis < 10 | out$emp_employed_dis > 90, na.rm = TRUE) ||
      any(out$emp_urate_dis    < 1  | out$emp_urate_dis    > 50, na.rm = TRUE)) {
    stop("fs_emp_measures: employment or unemployment rate outside a plausible ",
         "band. Check the source table before publishing.")
  }

  out
}

# ---- Health insurance derived measures ---------------------------------------
# ACS Table B18135 reports coverage status and coverage type by disability
# status, in two age bands. Four measures per band: no coverage, covered, public
# and private.
#
# Two properties drive how this sheet is built, and both are enforced below.
#
#   1. Covered is the exact complement of no coverage. Mapping both would be
#      duplication, so only "no coverage" is mapped and coverage appears in a
#      table.
#   2. Public and private are NOT mutually exclusive. A person with Medicare and
#      a private supplement is in both. Their sum exceeds the covered total by
#      7 to 14 points at ages 19 to 64, and by 40 to 62 points at 65 and over.
#      They must never be presented as parts of a whole, and neither carries a
#      defensible better-or-worse direction, so both are mapped by magnitude.
#
# The 65+ band is deliberately not mapped. Medicare makes coverage close to
# universal, so the observed spread is roughly 98.5 to 99.9 percent and ranking
# states on it would manufacture a difference that is not there. Those measures
# are returned here for the supplementary table only.
fs_hins_measures <- function(states_df, tol = 0.01) {

  slugs <- c(none = "nohealthins", covered = "healthins",
             public = "publichealth", private = "privatehealth")
  ages  <- c(a19to64 = "19to64", a65plus = "65plus")

  required <- unlist(lapply(ages, function(a)
    unlist(lapply(slugs, function(s)
      c(paste0("pct_cni_", a, "_dis_", s),  paste0("pop_cni_", a, "_dis_", s),
        paste0("pct_cni_", a, "_nodis_", s), paste0("pop_cni_", a, "_nodis_", s))))))
  missing <- setdiff(required, names(states_df))
  if (length(missing) > 0) {
    stop("fs_hins_measures: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  out <- states_df

  # Alias to short, consistent names: hins_<measure>_<band>_<group>.
  for (an in names(ages)) {
    for (sn in names(slugs)) {
      for (g in c("dis", "nodis")) {
        out[[paste0("hins_", sn, "_", an, "_", g)]] <-
          out[[paste0("pct_cni_", ages[[an]], "_", g, "_", slugs[[sn]])]]
        out[[paste0("hins_n_", sn, "_", an, "_", g)]] <-
          out[[paste0("pop_cni_", ages[[an]], "_", g, "_", slugs[[sn]])]]
      }
    }
  }

  # ---- guards ----
  # 1. Covered and no-coverage must be exact complements. If this ever fails,
  #    the sheet's decision to map only one of them is no longer safe.
  for (an in names(ages)) {
    for (g in c("dis", "nodis")) {
      tot <- out[[paste0("hins_none_", an, "_", g)]] +
             out[[paste0("hins_covered_", an, "_", g)]]
      if (any(abs(tot - 100) > tol, na.rm = TRUE)) {
        stop("fs_hins_measures: covered and no-coverage are not complements for ",
             an, " / ", g, " (max deviation ",
             round(max(abs(tot - 100), na.rm = TRUE), 4), " points). ",
             "The Insured table on this factsheet assumes they are.")
      }
    }
  }

  # 2. Public plus private must EXCEED the covered total, confirming the
  #    overlap the prose describes. If they ever summed to the covered total,
  #    the categories would be exclusive and the framing would need rewriting.
  for (an in names(ages)) {
    excess <- out[[paste0("hins_public_", an, "_dis")]] +
              out[[paste0("hins_private_", an, "_dis")]] -
              out[[paste0("hins_covered_", an, "_dis")]]
    if (any(excess < -tol, na.rm = TRUE)) {
      stop("fs_hins_measures: public plus private is less than the covered ",
           "total for ", an, ", which contradicts the overlap this factsheet ",
           "describes. Check Table B18135 before publishing.")
    }
  }

  # 3. The 65+ band should stay compressed. If a future vintage opens it up,
  #    the decision to show it as a table rather than a map deserves revisiting,
  #    so warn rather than fail.
  spread <- diff(range(out$hins_none_a65plus_dis, na.rm = TRUE))
  if (spread > 10) {
    warning("fs_hins_measures: the 65+ uninsured share now spans ",
            round(spread, 1), " points. It was under 2 when this factsheet was ",
            "written and shown as a table for that reason. Consider mapping it.")
  }

  out
}

# ---- Commuting derived measures ----------------------------------------------
# ACS Table S1811 reports means of transportation to work by disability status.
# The universe is WORKERS AGED 16 AND OVER, not the whole population, so every
# percentage on this factsheet describes disabled workers rather than disabled
# people. That is the single most important thing to get right on this sheet.
#
# Two consequences the build depends on, both enforced below.
#
#   1. The mode shares do NOT partition the commute. Public transit and driving
#      alone together account for 55 to 79 percent of commutes; the remainder is
#      carpooling, walking, cycling, taxis and working from home. They must not
#      be presented as parts of a whole.
#   2. Neither mode has a defensible direction. High transit use can mean
#      accessible transit worth using, or no alternative for someone who cannot
#      drive. High drive-alone can mean independence, or the absence of any
#      usable transit. Both are mapped by magnitude.
#
# This function also derives the figure the maps cannot show: what share of each
# population is in the worker universe at all. Nationally that is 23 percent of
# people with disabilities against 51 percent of people without, which is a far
# larger disparity than anything in the mode split, and it belongs on the sheet
# so the denominator is visible rather than implied.
#
# Note on an earlier concern, recorded because it was checked and found not to
# hold: it was assumed that a state might show a high transit share simply
# because few disabled residents work, making the mode split an artifact of
# employment. Measured against the 2024 vintage, the transit share correlates
# with the employment rate at only -0.05 and drive-alone at -0.12. The mode
# split is driven by urban density, not by employment. The worker share of the
# population is of course strongly related to employment (+0.93), which is
# exactly why it is reported separately rather than folded into the maps.
fs_commute_measures <- function(states_df, tol = 0.01) {

  required <- c("pop_cni_16plus_commute_dis", "pop_cni_16plus_commute_nodis",
                "pop_cni_16plus_transit_dis", "pop_cni_16plus_transit_nodis",
                "pct_cni_16plus_transit_dis", "pct_cni_16plus_transit_nodis",
                "pop_cni_16plus_drivealone_dis", "pop_cni_16plus_drivealone_nodis",
                "pct_cni_16plus_drivealone_dis", "pct_cni_16plus_drivealone_nodis",
                "pop_grp_total_dis", "pop_grp_total_nodis")
  missing <- setdiff(required, names(states_df))
  if (length(missing) > 0) {
    stop("fs_commute_measures: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  out <- states_df

  out$cm_transit_dis      <- out$pct_cni_16plus_transit_dis
  out$cm_transit_nodis    <- out$pct_cni_16plus_transit_nodis
  out$cm_drivealone_dis   <- out$pct_cni_16plus_drivealone_dis
  out$cm_drivealone_nodis <- out$pct_cni_16plus_drivealone_nodis

  # The denominator, made explicit and reportable.
  out$cm_workers_dis   <- out$pop_cni_16plus_commute_dis
  out$cm_workers_nodis <- out$pop_cni_16plus_commute_nodis

  # Share of each population that is in the worker universe. This is the
  # context measure; it is tabulated, not mapped, because it is essentially
  # employment and the Employment factsheet measures that directly.
  out$cm_worker_share_dis   <- out$cm_workers_dis   / out$pop_grp_total_dis   * 100
  out$cm_worker_share_nodis <- out$cm_workers_nodis / out$pop_grp_total_nodis * 100

  # ---- guards ----
  # 1. Percentages must reproduce from counts over the worker base, confirming
  #    the denominator this whole sheet is premised on.
  checks <- list(
    c("pop_cni_16plus_transit_dis",       "cm_workers_dis",   "cm_transit_dis"),
    c("pop_cni_16plus_transit_nodis",     "cm_workers_nodis", "cm_transit_nodis"),
    c("pop_cni_16plus_drivealone_dis",    "cm_workers_dis",   "cm_drivealone_dis"),
    c("pop_cni_16plus_drivealone_nodis",  "cm_workers_nodis", "cm_drivealone_nodis"))
  for (ck in checks) {
    err <- max(abs(out[[ck[1]]] / out[[ck[2]]] * 100 - out[[ck[3]]]), na.rm = TRUE)
    if (err > tol) {
      stop("fs_commute_measures: ", ck[3], " does not reproduce over the worker ",
           "base (max deviation ", round(err, 4), " points). The denominator ",
           "assumption behind this factsheet no longer holds.")
    }
  }

  # 2. The two modes must NOT sum to 100. If they ever did, the categories
  #    would be exhaustive and the prose describing a remainder would be wrong.
  tot <- out$cm_transit_dis + out$cm_drivealone_dis
  if (any(tot > 100 + tol, na.rm = TRUE)) {
    stop("fs_commute_measures: transit plus drive-alone exceeds 100 percent, ",
         "which is impossible over a shared worker base.")
  }
  if (all(abs(tot - 100) < 1, na.rm = TRUE)) {
    stop("fs_commute_measures: transit plus drive-alone now accounts for ",
         "essentially all commutes. This factsheet's prose describes a ",
         "remainder of carpooling, walking and remote work; rewrite it before ",
         "publishing.")
  }

  # 3. The worker share must stay well below the whole population, since the
  #    universe is workers. A value near 100 would mean the denominator changed.
  if (any(out$cm_worker_share_dis > 60, na.rm = TRUE)) {
    stop("fs_commute_measures: the commuting universe now covers more than 60 ",
         "percent of people with disabilities in at least one state. Check ",
         "whether S1811's universe has changed.")
  }

  out
}

# ---- Educational attainment derived measures ---------------------------------
# ACS Table S1811 reports educational attainment for the population aged 25 and
# over. ADA-PARC carries three of its categories: less than high school, high
# school or equivalent, and bachelor's degree or higher.
#
# Those three are NOT exhaustive. Adults whose highest attainment is some
# college without a degree, or an associate's degree, fall into none of them.
# Nationally that is 29 percent of adults with a disability and 28 percent of
# adults without, so the three published shares sum to between 63 and 79 percent
# depending on the state. This function derives the remainder exactly, as 100
# less the three, so the factsheet can report the full picture rather than leave
# a third of the population silently unaccounted for.
#
# Direction differs by measure and is set in the build chunk, not here:
# below high school is low-is-better, bachelor's or higher is high-is-better,
# and high school or equivalent is shown by magnitude because it reads two ways
# (more people completing high school, or more people stopping there).
#
# One oddity that looks like a bug and is not. In the 2024 vintage California
# reports 23.0 percent both for "less than high school" and for "bachelor's or
# higher" among adults with disabilities, and because both counts are derived
# from the same base they come out bit-identical. It was checked against the
# aggregate identity: reconstructing the all-population share from the disabled
# and non-disabled shares reproduces the published figure to within 0.06 points
# for both measures. It is a coincidence at one-decimal rounding, not a copied
# value. Four of the 8,407 places in the city file show the same coincidence.
fs_edu_measures <- function(states_df, tol = 0.01) {

  required <- c("pop_cni_25plus_edu_dis", "pop_cni_25plus_edu_nodis",
                "pct_cni_25plus_noGED_dis", "pct_cni_25plus_GED_dis",
                "pct_cni_25plus_bachelors_dis",
                "pct_cni_25plus_noGED_nodis", "pct_cni_25plus_GED_nodis",
                "pct_cni_25plus_bachelors_nodis",
                "pop_cni_25plus_noGED_dis", "pop_cni_25plus_GED_dis",
                "pop_cni_25plus_bachelors_dis")
  missing <- setdiff(required, names(states_df))
  if (length(missing) > 0) {
    stop("fs_edu_measures: missing required columns: ",
         paste(missing, collapse = ", "))
  }

  out <- states_df

  for (g in c("dis", "nodis")) {
    out[[paste0("edu_nohs_", g)]]      <- out[[paste0("pct_cni_25plus_noGED_", g)]]
    out[[paste0("edu_hs_", g)]]        <- out[[paste0("pct_cni_25plus_GED_", g)]]
    out[[paste0("edu_bachelors_", g)]] <- out[[paste0("pct_cni_25plus_bachelors_", g)]]
    # The uncounted middle: some college without a degree, or an associate's.
    out[[paste0("edu_other_", g)]] <- 100 -
      out[[paste0("edu_nohs_", g)]] -
      out[[paste0("edu_hs_", g)]] -
      out[[paste0("edu_bachelors_", g)]]
  }

  out$edu_base_dis   <- out$pop_cni_25plus_edu_dis
  out$edu_base_nodis <- out$pop_cni_25plus_edu_nodis
  out$edu_other_n_dis <- out$edu_other_dis / 100 * out$edu_base_dis

  # ---- guards ----
  # 1. The derived remainder must be positive everywhere. A negative value would
  #    mean the three published shares overlap, which they must not.
  for (g in c("dis", "nodis")) {
    v <- out[[paste0("edu_other_", g)]]
    if (any(v < -tol, na.rm = TRUE)) {
      stop("fs_edu_measures: the three attainment categories sum to more than ",
           "100 percent for the '", g, "' series in ",
           sum(v < -tol, na.rm = TRUE), " state(s), so they are not mutually ",
           "exclusive. The derived remainder on this factsheet assumes they are.")
    }
  }

  # 2. The three must NOT be exhaustive. If a future vintage added the missing
  #    categories, the remainder would collapse to zero and the sheet's final
  #    table and its prose would both need rewriting.
  if (all(abs(out$edu_other_dis) < 1, na.rm = TRUE)) {
    stop("fs_edu_measures: the three attainment categories now account for ",
         "essentially the whole population. This factsheet reports an uncounted ",
         "remainder of some-college and associate's degrees; rewrite it before ",
         "publishing.")
  }

  # 3. Published percentages must reproduce from their counts over the S1811
  #    base, confirming the denominator.
  for (m in c("noGED", "GED", "bachelors")) {
    err <- max(abs(out[[paste0("pop_cni_25plus_", m, "_dis")]] /
                   out$edu_base_dis * 100 -
                   out[[paste0("pct_cni_25plus_", m, "_dis")]]), na.rm = TRUE)
    if (err > tol) {
      stop("fs_edu_measures: pct_cni_25plus_", m, "_dis does not reproduce from ",
           "its count over pop_cni_25plus_edu_dis (max deviation ",
           round(err, 4), " points).")
    }
  }

  out
}

# ---- editable content (intros, footnotes, summary stats) ---------------------
# The prose for each factsheet lives in content/factsheet-content.yml so it can
# be edited without touching code. These helpers load it and render the blocks.

# Load the content config. Returns a named list keyed by factsheet
# (nursing, poverty, housing, hcbs).
fs_load_content <- function(
    path = here::here("factsheets", "_generate", "content",
                      "factsheet-content.yml")) {
  yaml::read_yaml(path)
}

# Substitute year placeholders ({acs_start}, {acs_end}, {acs_year},
# {source_year}) so citations track the live data vintage. Unsupplied tokens are
# left untouched, so fixed years typed into the YAML pass through unchanged.
fs_fill_years <- function(x, acs_year = NULL, source_year = NULL,
                          pums_year = NULL) {
  if (!is.null(acs_year)) {
    x <- gsub("{acs_start}", acs_year - 4, x, fixed = TRUE)
    x <- gsub("{acs_end}",   acs_year,     x, fixed = TRUE)
    x <- gsub("{acs_year}",  acs_year,     x, fixed = TRUE)
  }
  if (!is.null(source_year)) {
    x <- gsub("{source_year}", source_year, x, fixed = TRUE)
  }
  # Technology Access is built from the ACS Public Use Microdata Sample, a
  # different vintage and methodology from the five-year table estimates behind
  # every other sheet. It needs its own token so {acs_year} never silently
  # prints the wrong year in a PUMS citation.
  if (!is.null(pums_year)) {
    x <- gsub("{pums_year}", pums_year, x, fixed = TRUE)
  }
  x
}

# Render the intro <section> from a character vector of paragraph HTML strings.
fs_intro <- function(paragraphs, acs_year = NULL, source_year = NULL) {
  ps <- fs_fill_years(unlist(paragraphs), acs_year, source_year)
  body <- paste0("      <p>", ps, "</p>", collapse = "\n\n")
  paste0(
'    <section class="intro-section" aria-label="Introduction">
', body, '
    </section>')
}

# Render the numbered data-source footnotes from a character vector.
fs_footnotes <- function(items, acs_year = NULL, source_year = NULL,
                         aria_label = "Data sources") {
  lis <- fs_fill_years(unlist(items), acs_year, source_year)
  body <- paste0("        <li>", lis, "</li>", collapse = "\n")
  paste0(
'    <div class="footnotes" role="note" aria-label="', fs_esc(aria_label), '">
      <ol>
', body, '
      </ol>
    </div>')
}

# Render the poverty summary-stat callout grid from a list of
# list(number=, label=) entries.
fs_summary_stats <- function(stats) {
  cards <- vapply(stats, function(s) {
    paste0(
'      <div class="summary-stat">
        <div class="stat-number">', s$number, '</div>
        <div class="stat-label">', s$label, '</div>
      </div>')
  }, character(1))
  paste0(
'    <div class="summary-stats" aria-label="Key statistics">
', paste(cards, collapse = "\n"), '
    </div>')
}

# Render an "About These Data" note box from a list(title=, body=). Used to make
# data attribution explicit on a factsheet. Semantics mirror the direction-note
# boxes (role="note"); a labeled region keeps it accessible to screen readers.
fs_about <- function(about) {
  if (is.null(about)) return("")
  title <- if (!is.null(about$title)) about$title else "About These Data"
  paste0(
'    <section class="about-data" role="note" aria-label="', fs_esc(title), '">
      <h2 class="about-data-title">', title, '</h2>
      <p>', about$body, '</p>
    </section>')
}

# ---- small HTML helpers ------------------------------------------------------

# Minimal HTML escape for text injected into cells/labels.
fs_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

# ---- document skeleton -------------------------------------------------------

# <head> through opening of <body>. base CSS is read from the shared asset;
# extra_css is appended (per-factsheet overrides win via the cascade).
fs_html_head <- function(title, description, extra_css = "") {
  base_css <- fs_read_asset("factsheet-base.css")
  paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', title, '</title>
  <meta name="description" content="', fs_esc(description), '">
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link href="https://fonts.googleapis.com/css2?family=EB+Garamond:wght@400;500;600;700&display=swap" rel="stylesheet">
  <style>
', base_css,
    if (nzchar(extra_css)) paste0("\n    /* ---- per-factsheet overrides ---- */\n", extra_css, "\n") else "",
'  </style>
</head>
<body>

<a class="skip-link" href="#main-content">Skip to main content</a>

<div class="page-wrapper">
')
}

# Dark brand banner. title_html may contain inline markup.
#
# Layout is a three-column grid: white brand icon left, title block centre,
# white ADA-PARC wordmark right. Replaced the gold banner and its two typeset
# word-logos on 2026-08-04.
#
# TITLE AND SUBTITLE. The title is the topic and the subtitle beneath it is the
# ADA-PARC outcome category: "Access to Housing" over "Community Living", not
# "Community Living for People with Disabilities: Access to Housing" on two
# wrapped lines. The pair is the same title/category registry that drives the
# fact sheet cards in ADA_PARC.Rmd, so the banner and the card a reader clicked
# to get here now say the same words in the same order. Keep the two in step: if
# a sheet is renamed, rename it in both places.
#
# Because the titles are now short, none of the sheets needs the
# `.header-title h1 { font-size: 1.35rem }` shrink they used to carry in
# extra_css. Those overrides were removed from the build chunks; do not
# reintroduce one without checking that the title actually overflows.
#
# The "Last updated" line is NOT here any more. It moved to fs_footer(): it is
# provenance rather than title matter, and in the banner it took the reader's
# first glance away from the topic. `published` is deliberately not accepted as
# an argument, so an old call site fails loudly rather than silently dropping
# the date.
#
# The icon is PARC_white_icon_knockout.png, not PARC_white_icon.png. The
# latter is a white TILE with a black glyph, which on a dark banner reads as a
# sticker beside the wordmark's clean knockout; it is kept for the light-ground
# uses it was drawn for. The knockout is a white glyph on transparent, derived
# from the blue mark, and matches the wordmark's treatment.
#
# alt text: the icon is alt="" on purpose. It is decorative here because the
# wordmark image beside it already announces "ADA-PARC", and giving both a
# label makes every screen reader read the organisation name twice before it
# reaches the fact sheet's actual title. The wordmark carries the name.
#
# The subtitle is a <p> rather than an <h2>. It is a classifier attached to the
# h1, not a section of the document, and promoting it to a heading would put a
# spurious entry at the top of every screen reader's heading list, above the
# sheet's real sections.
#
# The ADA National Network badge that used to sit in the right slot is gone,
# displaced by the wordmark. The footer still links to adata.org, so the
# affiliation is stated but no longer appears in the banner.
fs_header <- function(title_html, subtitle = NULL) {
  subtitle_html <- if (!is.null(subtitle) && nzchar(subtitle)) {
    paste0('\n      <p class="header-subtitle">', subtitle, '</p>')
  } else ""
  paste0(
'
  <header class="factsheet-header" role="banner">
    <img class="brand-icon" src="', fs_brand_data_uri("PARC_white_icon_knockout.png"), '" alt="">
    <div class="header-title">
      <h1>', title_html, '</h1>', subtitle_html, '
    </div>
    <img class="brand-wordmark" src="', fs_brand_data_uri("PARC_white_text.png"), '" alt="ADA-PARC">
  </header>
')
}

# Standard footer. The second paragraph wording varies slightly between the
# original files; pass `rights_sentence` to match a specific one if needed.
#
# `updated` is the "Last updated" line relocated from the banner. It is the last
# thing in the footer, set off by a rule, so the sheet ends on its provenance.
# Pass NULL to omit it. One string, one place: every sheet takes the default, so
# a new publication date is a single edit here rather than nine.
fs_footer <- function(
    rights_sentence = "For more information about your rights, go to the ADA National Network at <a href=\"https://adata.org\" target=\"_blank\" rel=\"noopener noreferrer\">ADATA.ORG</a>.",
    updated = "Last updated: August 2026") {
  updated_html <- if (!is.null(updated) && nzchar(updated)) {
    paste0('\n    <p class="updated-note">', updated, '</p>')
  } else ""
  paste0(
'
  <footer class="factsheet-footer" role="contentinfo">
    <p class="funding-note">The ADA-PARC is funded by the National Institute for Disability, Independent Living, and Rehabilitation Research (NIDILRR) under grants 90DP0026 and 90DPAD0001.</p>
    <p>The Americans with Disabilities Act Participation Action Research Consortium (ADA-PARC) is a collaborative national project to document participation disparities experienced by people with disabilities at the national, state, and city levels. For more information on your state, visit <a href="https://adaparc.org" target="_blank" rel="noopener noreferrer">ADAPARC.ORG</a>. ', rights_sentence, ' Please contact <a href="mailto:hammel@uic.edu">hammel@uic.edu</a> with any questions.</p>', updated_html, '
  </footer>

</div>
')
}

# Population note: the universe the sheet's numbers describe, stated as the
# first thing in the body, immediately under the banner.
#
# Every sheet has one and they are not all the same. Education is adults 25 and
# over, employment is the civilian noninstitutionalized population 18 to 64,
# commuting is workers 16 and over, and the Community Living sheets are all
# people with a disability at any age. Those denominators were previously only
# recoverable from a footnote or from the prose, which is how a reader ends up
# reading an attainment figure as if it covered children.
#
# `note` is the universe itself, a noun phrase rather than a sentence, e.g.
# "Adults age 25 and over". `detail` is an optional second clause for the cases
# where the universe alone still misleads, e.g. that commuting percentages
# describe workers and not all people with disabilities.
#
# role="note" and the aria-label make it a labelled landmark, so a screen reader
# user can reach it directly instead of hearing it only if they start from the
# top. Not a <section> with a heading: a heading here would sit above the
# sheet's real first section in the heading list.
fs_population_note <- function(note, detail = NULL, label = "Population covered") {
  if (is.null(note) || !nzchar(note)) return("")
  detail_html <- if (!is.null(detail) && nzchar(detail)) paste0(" ", detail) else ""
  paste0(
'    <div class="population-note" role="note" aria-label="', fs_esc(label), '">
      <span class="population-note-label">', label, ':</span> ', note, detail_html, '
    </div>
')
}

# Shared <script> tags: D3 + topojson CDNs and the shared renderer asset,
# followed by any per-factsheet JS data/render blocks (passed in `tail_js`).
fs_scripts <- function(tail_js) {
  renderer <- fs_read_asset("adaparc-map.js")
  paste0(
'
<!-- D3 MAP SCRIPTS (ADA-PARC shared) -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/7.8.5/d3.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/topojson/3.0.2/topojson.min.js"></script>
<script>
', renderer, '

', tail_js, '
</script>
</body>
</html>
')
}

# ---- content builders --------------------------------------------------------

# One tier table block (header chip + table). `cells` is a list of character
# vectors, one per data column (already formatted). `col_headers` names them.
fs_tier_table <- function(tier, col_headers, state_names, cells, aria_label,
                          disability_col = NA_integer_,
                          tier_labels = FS_TIER_LABELS_PERF) {
  thead <- paste0(
    "            <tr>\n",
    paste0(
      "              <th scope=\"col\"",
      ifelse(seq_along(col_headers) == disability_col, " class=\"disability-col\"", ""),
      ">", col_headers, "</th>",
      collapse = "\n"
    ),
    "\n            </tr>")

  body_rows <- vapply(seq_along(state_names), function(i) {
    tds <- paste0("<td>", vapply(cells, function(col) col[i], character(1)), "</td>",
                  collapse = "")
    paste0("              <tr><td>", state_names[i], "</td>", tds, "</tr>")
  }, character(1))

  tier_label <- tier_labels[[tier]]

  paste0(
'        <div class="tier-table-group">
          <span class="tier-header ', tier, '" aria-hidden="true">', tier_label, '</span>
          <table aria-label="', fs_esc(aria_label), '">
            <thead>
', thead, '
            </thead>
            <tbody>
', paste(body_rows, collapse = "\n"), '
            </tbody>
          </table>
        </div>')
}

# The JS data object + render call for one map.
#   data: data.frame with columns fips (chr, 2-digit), tier (chr), display (chr)
#   tier_labels: "performance" (default) or "magnitude". The magnitude set is
#     passed through to the renderer so tooltips and per-state aria-labels read
#     "Highest quarter" rather than "Excellent" on indicators with no direction.
fs_map_js <- function(obj_name, container_id, desc_id, data, value_label,
                      svg_title, svg_desc,
                      tier_labels = c("performance", "magnitude")) {
  tier_labels <- match.arg(tier_labels)
  entries <- paste0(
    '  "', data$fips, '":["', data$tier, '","', data$display, '"]',
    collapse = ",\n")
  labels_opt <- if (tier_labels == "magnitude") {
    ',\n  tierLabels:  ADAPARC_TIER_LABEL_MAGNITUDE'
  } else ""
  paste0(
'const ', obj_name, ' = {
', entries, '
};

adaparcRenderMap({
  containerId: "', container_id, '",
  descId:      "', desc_id, '",
  stateData:   ', obj_name, ',
  valueLabel:  "', value_label, '",
  svgTitle:    "', svg_title, '",
  svgDesc:     "', svg_desc, '"', labels_opt, '
});')
}

# ---- indicator section builder ----------------------------------------------
# Assembles one complete indicator section (lead prose, direction note, map
# figure, legend, tier tables) and returns its HTML plus the JS that renders its
# map. A multi-indicator factsheet is then an lapply over a list of specs rather
# than one hand-written block per indicator.
#
# Every accessibility requirement is satisfied here rather than per section, so
# a fix lands once: the map carries a descriptive aria-labelledby pair, the
# legend is a labeled region, and each tier table gets its own aria-label.
#
# `spec` is a list with these fields:
#   id            slug. Drives element ids and the JS object name. Must be
#                 unique within a factsheet and safe in a CSS id.
#   heading       section <h2> text. NULL to omit (single-section sheets that
#                 already carry their heading in the page title).
#   map_title     <h3> above the map.
#   table_heading <h2> above the tier tables.
#   value_col     column in `data` holding the numeric value to map and tier.
#   tier_col      column in `data` holding the tier assignment.
#   direction     "high_good", "low_good" or "magnitude".
#   fmt           function(x) -> character, applied to values. Default 1dp pct.
#   value_label   short label for tooltips, e.g. "Institutional rate".
#   svg_title     accessible <title> for the map.
#   svg_desc      accessible <desc> for the map.
#   legend_best   text appended to the darkest chip, e.g. "lowest rate".
#   legend_worst  text appended to the palest chip.
#   direction_note  sentence explaining how to read the colors. NULL to omit.
#   caption       figcaption text.
#   col_headers   tier table column headers, first is always "State".
#   cell_cols     character vector of columns in `data` for the remaining
#                 tier table columns, formatted with `cell_fmts`.
#   cell_fmts     list of formatter functions, one per cell_cols entry.
#   lead          character vector of paragraph HTML, from the prose file.
#   table_note    optional <p> under the table heading.
#
# `data` must contain fips, name, the value column and the tier column.
fs_indicator_section <- function(spec, data) {
  stopifnot(is.list(spec), !is.null(spec$id), !is.null(spec$value_col))
  direction <- spec$direction %||% "high_good"
  fmt       <- spec$fmt %||% function(x) fmt_pct(x, 1)
  labels    <- if (identical(direction, "magnitude")) FS_TIER_LABELS_MAGNITUDE
               else FS_TIER_LABELS_PERF

  vals  <- data[[spec$value_col]]
  tiers <- data[[spec$tier_col]]

  # ---- map ----
  keep   <- !is.na(tiers)
  map_df <- data.frame(
    fips    = data$fips[keep],
    tier    = tiers[keep],
    display = vapply(vals[keep], function(v) fmt(v), character(1)),
    stringsAsFactors = FALSE)

  map_js <- fs_map_js(
    obj_name     = paste0(toupper(gsub("[^A-Za-z0-9]", "_", spec$id)), "_DATA"),
    container_id = paste0("map-", spec$id),
    desc_id      = paste0(spec$id, "-map-desc"),
    data         = map_df,
    value_label  = spec$value_label,
    svg_title    = spec$svg_title,
    svg_desc     = spec$svg_desc,
    tier_labels  = if (identical(direction, "magnitude")) "magnitude" else "performance")

  # ---- tier tables ----
  cell_cols <- spec$cell_cols %||% spec$value_col
  cell_fmts <- spec$cell_fmts %||% list(fmt)

  tier_table_for <- function(t) {
    idx <- which(!is.na(tiers) & tiers == t)
    if (length(idx) == 0) return("")
    ord <- fs_within_tier_order(vals[idx], direction)
    idx <- idx[ord]
    cells <- lapply(seq_along(cell_cols), function(k) {
      vapply(data[[cell_cols[k]]][idx], function(v) cell_fmts[[k]](v), character(1))
    })
    fs_tier_table(
      tier        = t,
      col_headers = spec$col_headers %||% c("State", "Percent"),
      state_names = data$name[idx],
      cells       = cells,
      tier_labels = labels,
      aria_label  = paste0(spec$value_label, ", ", labels[[t]], " group"))
  }

  left  <- paste0(tier_table_for("excellent"), "\n\n", tier_table_for("above"))
  right <- paste0(tier_table_for("below"),     "\n\n", tier_table_for("poor"))

  # ---- assemble ----
  heading_html <- if (!is.null(spec$heading)) {
    paste0('    <h2 class="section-heading">', spec$heading, "</h2>\n")
  } else ""

  lead_html <- if (!is.null(spec$lead) && length(unlist(spec$lead)) > 0) {
    paste0("    ", paste0("<p>", unlist(spec$lead), "</p>", collapse = "\n    "), "\n")
  } else ""

  note_html <- if (!is.null(spec$direction_note)) {
    paste0(
'    <div class="direction-note" role="note">
      <strong>How to read the colors.</strong> ', spec$direction_note, '
    </div>
')
  } else ""

  table_note_html <- if (!is.null(spec$table_note)) {
    paste0("    <p>", spec$table_note, "</p>\n")
  } else ""

  html <- paste0(
heading_html,
lead_html,
note_html,
'    <figure class="map-figure">
      <h3 class="map-title" id="', spec$id, '-map-heading">', spec$map_title, '</h3>
      <div id="map-', spec$id, '" class="map-container" aria-labelledby="', spec$id, '-map-heading ', spec$id, '-map-desc">
        <p id="', spec$id, '-map-desc" class="map-loading" aria-live="polite">Loading map&hellip;</p>
        <noscript><p class="map-noscript">Map requires JavaScript. All data is in the table below.</p></noscript>
      </div>
      <div class="map-legend" aria-label="', fs_esc(paste0("Map color legend for ", spec$value_label)), '">
        <span class="tier-chip excellent">', labels[["excellent"]],
        if (!is.null(spec$legend_best))  paste0(" (", spec$legend_best, ")")  else "", '</span>
        <span class="tier-chip above">',  labels[["above"]], '</span>
        <span class="tier-chip below">',  labels[["below"]], '</span>
        <span class="tier-chip poor">',   labels[["poor"]],
        if (!is.null(spec$legend_worst)) paste0(" (", spec$legend_worst, ")") else "", '</span>
      </div>
      <figcaption class="map-caption">', spec$caption, '</figcaption>
    </figure>

    <h2 class="section-heading">', spec$table_heading %||% spec$map_title, '</h2>
', table_note_html,
'    <div class="tables-grid">
      <div>
', left, '
      </div>

      <div>
', right, '
      </div>
    </div>
')

  list(html = html, js = map_js)
}

# Null-coalescing helper used by the section builder.
`%||%` <- function(x, y) if (is.null(x)) y else x

# FIPS lookup keyed by 2-letter ABBR (50 states + DC).
FS_ABBR_TO_FIPS <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10",
  DC="11", FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19",
  KS="20", KY="21", LA="22", ME="23", MD="24", MA="25", MI="26", MN="27",
  MS="28", MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34", NM="35",
  NY="36", NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44",
  SC="45", SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53",
  WV="54", WI="55", WY="56")

# Write a complete factsheet HTML file from its assembled parts.
fs_write_file <- function(path, head_html, header_html, main_html,
                          footer_html, scripts_html) {
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con))
  writeChar(paste0(head_html, header_html,
                   '\n  <main id="main-content" class="factsheet-body">\n',
                   main_html,
                   '\n  </main>\n',
                   footer_html, scripts_html),
            con, eos = NULL, useBytes = TRUE)
  invisible(path)
}
