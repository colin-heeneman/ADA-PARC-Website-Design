# =============================================================================
# render_matrix.R
#
# Phase 6.2 of docs/palette-v1-build-plan.qmd.
#
# Renders every combination of palette and contrast mode for both scorecards,
# so a change to the colour system can be re-checked in one command instead of
# by clicking through the app. Five palettes by two contrast modes by two
# scorecards is twenty files.
#
#     Rscript scripts/render_matrix.R
#     Rscript scripts/render_matrix.R --palettes heritage,mono_high
#     Rscript scripts/render_matrix.R --state NY --category CL --out /tmp/mx
#
# Output goes to docs/_matrix/ by default, which is gitignored build output.
# Filenames are self-describing: state_NY_cividis_r_high.html.
#
# WHY NOT RENDER IN PARALLEL. quarto_render() writes its output beside the .qmd
# under a fixed name and the copy step moves it. Two concurrent renders of the
# same document race on that file. This is the same limitation the deployed app
# has with www/state_output.html, recorded as an open question in the plan.
#
# Follow with the contrast audit over what this produced:
#
#     python3 scripts/audit_rendered.py docs/_matrix
# =============================================================================

suppressPackageStartupMessages({
  ok <- requireNamespace("quarto", quietly = TRUE)
})
if (!ok) stop("render_matrix.R needs the 'quarto' package installed.")

# Locate the project root by walking up until scorecard/ and scripts/ are both
# present, so this runs from the root, from scripts/, or from an IDE.
find_root <- function(start = getwd()) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:6) {
    if (dir.exists(file.path(d, "scorecard")) &&
        dir.exists(file.path(d, "scripts"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("render_matrix.R cannot find the project root. ",
       "Run it from the ADA-PARC-Website-Design directory.")
}
root <- find_root()
source(file.path(root, "scripts", "palettes.R"))

# ── Arguments ───────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1]
}

palettes  <- strsplit(arg("palettes", paste(PALETTE_ORDER, collapse = ",")),
                      ",")[[1]]
contrasts <- strsplit(arg("contrasts", paste(CONTRAST_MODES, collapse = ",")),
                      ",")[[1]]
state     <- arg("state", "IL")
category  <- arg("category", "CL")
outdir    <- arg("out", file.path(root, "docs", "_matrix"))

unknown <- setdiff(palettes, names(ADAPARC_PALETTES))
if (length(unknown)) {
  stop("Unknown palette id(s): ", paste(unknown, collapse = ", "),
       ". Known: ", paste(PALETTE_ORDER, collapse = ", "))
}
unknown <- setdiff(contrasts, CONTRAST_MODES)
if (length(unknown)) {
  stop("Unknown contrast mode(s): ", paste(unknown, collapse = ", "))
}

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
scorecard_dir <- file.path(root, "scorecard")

jobs <- expand.grid(
  palette  = palettes,
  contrast = contrasts,
  doc      = c("state", "category"),
  stringsAsFactors = FALSE
)

message(sprintf("Rendering %d file(s) into %s", nrow(jobs), outdir))
message(sprintf("  state = %s, category = %s", state, category))

started <- Sys.time()
failures <- character(0)

for (i in seq_len(nrow(jobs))) {
  j <- jobs[i, ]
  qmd <- if (j$doc == "state") "state_scorecard.qmd" else "category_scorecard.qmd"
  src <- file.path(scorecard_dir, sub("\\.qmd$", ".html", qmd))
  key <- if (j$doc == "state") state else category
  dest <- file.path(outdir, sprintf("%s_%s_%s_%s.html",
                                    j$doc, key, j$palette, j$contrast))

  params <- list(year = 2024, palette = j$palette, contrast = j$contrast)
  params[[if (j$doc == "state") "state_abbr" else "category"]] <- key

  message(sprintf("[%2d/%2d] %s", i, nrow(jobs), basename(dest)))
  res <- tryCatch({
    quarto::quarto_render(
      input          = file.path(scorecard_dir, qmd),
      execute_params = params,
      execute_dir    = scorecard_dir,
      quiet          = TRUE
    )
    if (!file.exists(src)) stop("render produced no ", basename(src))
    file.copy(src, dest, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    message("        FAILED: ", conditionMessage(e))
    FALSE
  })
  if (!isTRUE(res)) failures <- c(failures, basename(dest))
}

elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)
message(sprintf("\nDone in %s min. %d succeeded, %d failed.",
                elapsed, nrow(jobs) - length(failures), length(failures)))
if (length(failures)) {
  message("Failed: ", paste(failures, collapse = ", "))
  quit(status = 1)
}
message("\nNext: python3 scripts/audit_rendered.py ", outdir)
