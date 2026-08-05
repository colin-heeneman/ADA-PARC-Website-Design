# ─────────────────────────────────────────────────────────────────────────────
# preflight_deploy.R — verify the deployment bundle is complete before pushing
#
# WHY THIS EXISTS
# ADA_PARC.Rmd is deployed to shinyapps.io as a `runtime: shiny` document.
# rsconnect uploads the .Rmd plus whatever the `resource_files:` YAML block
# declares. Nothing else. A file that sits happily in the repo but is absent
# from that list simply will not exist in the container, and the app dies on
# first render with the near-useless message "cannot open the connection".
# It works locally, so the gap is invisible until production.
#
# This happened on 2026-07-30: scripts/palettes.R was added and sourced by the
# setup chunk but never declared, taking the whole site down on deploy.
#
# WHAT IT CHECKS
#   1. Every path listed in `resource_files:` actually exists on disk.
#      (Catches renames and typos that would silently ship a broken bundle.)
#   2. Every file the app sources or reads at load time is covered by the
#      manifest, either declared directly or contained in a declared directory.
#      (Catches the palettes.R class of failure.)
#   3. Files the on-demand render templates depend on are covered.
#
# USAGE
#   Rscript scripts/preflight_deploy.R
#
# Exit status is 0 when clean and 1 when anything is missing, so this can be
# wired into a pre-deploy hook or CI step later without modification.
# ─────────────────────────────────────────────────────────────────────────────

suppressWarnings(suppressMessages({
  ok_here <- requireNamespace("here", quietly = TRUE)
}))

root <- if (ok_here) here::here() else normalizePath(getwd())
rmd  <- file.path(root, "ADA_PARC.Rmd")

if (!file.exists(rmd)) {
  stop("Cannot find ADA_PARC.Rmd at ", rmd,
       "\nRun this from the project root.", call. = FALSE)
}

lines <- readLines(rmd, warn = FALSE)

# ── Parse the resource_files: block out of the YAML header ───────────────────
# The header is delimited by the first two lines that are exactly "---".
# Within it, `resource_files:` is followed by "- path" entries until a line
# appears that is not an entry (i.e. the next top-level YAML key).
yaml_bounds <- which(trimws(lines) == "---")
if (length(yaml_bounds) < 2) {
  stop("Could not locate the YAML header delimiters in ADA_PARC.Rmd.",
       call. = FALSE)
}
yaml <- lines[(yaml_bounds[1] + 1):(yaml_bounds[2] - 1)]

rf_start <- grep("^resource_files:\\s*$", yaml)
if (length(rf_start) != 1) {
  stop("Expected exactly one `resource_files:` key in the YAML header; found ",
       length(rf_start), ".", call. = FALSE)
}

declared <- character(0)
i <- rf_start + 1
while (i <= length(yaml) && grepl("^\\s*-\\s+", yaml[i])) {
  declared <- c(declared, trimws(sub("^\\s*-\\s+", "", yaml[i])))
  i <- i + 1
}
declared <- declared[nzchar(declared)]

# ── Check 1: everything declared exists ──────────────────────────────────────
missing_declared <- declared[!file.exists(file.path(root, declared))]

# ── Check 2: everything needed is declared ───────────────────────────────────
# A path counts as covered if it is declared verbatim, or if any declared
# entry is a directory that contains it. Directory declarations are how the
# scorecard data and factsheets ship, so both forms must be honoured.
declared_dirs <- declared[dir.exists(file.path(root, declared))]

is_covered <- function(rel) {
  if (rel %in% declared) return(TRUE)
  any(vapply(
    declared_dirs,
    function(d) startsWith(rel, paste0(d, "/")),
    logical(1)
  ))
}

# Paths the app needs. Load-time dependencies are listed first because a gap
# there is fatal on startup; render-time ones fail later, on user action.
required <- c(
  # Sourced or read by the [setup] chunk. A gap here takes the site down.
  "scripts/functions.R",
  "scripts/palettes.R",
  "data/dict_vars.csv",
  "data/final/city_place_full.Rds",
  "data/final/dict_location_crosswalk.Rds",
  "data/final/tracts_data.Rds",
  "data/final/tracts_sf.Rds",
  "data/final/national_data.Rds",
  "data/final/us_states.Rds",
  # Referenced from the YAML `includes:`/`css:` keys.
  "www/cssloaders.html",
  "www/styles.css",
  # Rendered on demand by download handlers. A gap breaks the download only.
  "scorecard/state_scorecard.qmd",
  "scorecard/category_scorecard.qmd",
  "scorecard/scorecard_state.css",
  "scorecard/scorecard_v3.css",
  "scorecard/font-loader.html",
  "scorecard/dictionary/scorecard_indicator_dictionary.csv",
  "scorecard/dictionary/scorecard_index_dictionary.csv",
  "scorecard/scorecard_data/final/index_scores_wide.csv",
  "national/national_topic.qmd",
  # Static assets served to the browser.
  "www/adaparclogo.png",
  # Provisional brand identity (pending ADA centre approval). The home hero
  # reads PARC_blue_icon.png at render time; the other three are declared so
  # the folder ships whole and the white variants are ready for dark surfaces.
  "www/brand/PARC_blue_icon.png",
  "www/brand/PARC_blue_text.png",
  "www/brand/PARC_white_icon.png",
  "www/brand/PARC_white_icon_knockout.png",
  "www/brand/PARC_white_text.png",
  "www/participant_logos.png",
  "www/Status_for_Disabled_image.png",
  "www/ScorecardReport.pdf",
  "www/factsheets"
)

absent_on_disk  <- required[!file.exists(file.path(root, required))]
undeclared      <- required[!vapply(required, is_covered, logical(1))]

# ── Check 3: palette sourcing from the render templates ──────────────────────
# state_scorecard.qmd and category_scorecard.qmd both reach for
# ../scripts/palettes.R relative to scorecard/. Confirm that resolves.
palette_from_scorecard <- file.path(root, "scorecard", "..", "scripts",
                                    "palettes.R")
palette_reachable <- file.exists(palette_from_scorecard)

# ── Report ───────────────────────────────────────────────────────────────────
bar <- strrep("─", 74)
cat(bar, "\n")
cat("ADA-PARC deploy preflight\n")
cat("Project root: ", root, "\n", sep = "")
cat(bar, "\n\n")

problems <- 0L

report <- function(label, items, hint) {
  if (length(items) == 0) {
    cat("PASS  ", label, "\n", sep = "")
    return(0L)
  }
  cat("FAIL  ", label, "\n", sep = "")
  for (x in items) cat("        ", x, "\n", sep = "")
  cat("        -> ", hint, "\n", sep = "")
  length(items)
}

problems <- problems + report(
  sprintf("All %d resource_files entries exist on disk", length(declared)),
  missing_declared,
  "Declared in YAML but not present. Fix the path or remove the entry."
)

problems <- problems + report(
  sprintf("All %d required app files exist on disk", length(required)),
  absent_on_disk,
  "The app reads this path but the file is not in the repo."
)

problems <- problems + report(
  "All required app files are declared in resource_files",
  undeclared,
  paste("Present locally but NOT in the deploy bundle. This is the failure",
        "mode that works locally and dies live. Add to resource_files:.")
)

problems <- problems + report(
  "Scorecard templates can reach ../scripts/palettes.R",
  if (palette_reachable) character(0) else "scorecard/../scripts/palettes.R",
  "state_scorecard.qmd and category_scorecard.qmd source this path."
)

cat("\n", bar, "\n", sep = "")
if (problems == 0L) {
  cat("Clean. Safe to deploy.\n")
  cat(bar, "\n")
  quit(status = 0, save = "no")
} else {
  cat(sprintf("%d problem(s) found. Resolve before deploying.\n", problems))
  cat(bar, "\n")
  quit(status = 1, save = "no")
}
