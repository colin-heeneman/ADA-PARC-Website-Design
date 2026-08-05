# =============================================================================
# promote_factsheets.R
#
# Copies reviewed fact sheets from factsheets/_generate/_preview/ to
# www/factsheets/, which is the directory the Shiny app serves.
#
# The generator deliberately never writes to www/factsheets/, so a render can
# never publish an unreviewed sheet. With four sheets, moving them by hand was
# tolerable. With ten it is a reliable source of drift, hence this script.
#
# Safety behavior:
#   - reports what would change before changing it
#   - refuses to promote a preview file that is OLDER than its published
#     counterpart, which almost always means the preview is stale
#   - takes a timestamped backup of anything it overwrites
#
# Run:  Rscript scripts/promote_factsheets.R              (dry run, reports only)
#       Rscript scripts/promote_factsheets.R --write      (actually copies)
#       Rscript scripts/promote_factsheets.R --write --force   (ignore staleness)
# =============================================================================

args  <- commandArgs(trailingOnly = TRUE)
write <- "--write" %in% args
force <- "--force" %in% args

preview_dir <- file.path("factsheets", "_generate", "_preview")
public_dir  <- file.path("www", "factsheets")
backup_dir  <- file.path("factsheets", "old",
                         format(Sys.time(), "promoted-%Y-%m-%d-%H%M%S"))

if (!dir.exists(preview_dir)) {
  stop("No preview directory at ", preview_dir,
       ". Render factsheets/_generate/generate-factsheets.qmd first.")
}

files <- list.files(preview_dir, pattern = "^factsheet-.*\\.html$")
if (length(files) == 0) stop("No factsheet HTML found in ", preview_dir, ".")

status <- data.frame(file = files, action = NA_character_, note = "",
                     stringsAsFactors = FALSE)

for (i in seq_along(files)) {
  f    <- files[i]
  src  <- file.path(preview_dir, f)
  dest <- file.path(public_dir, f)

  if (!file.exists(dest)) {
    status$action[i] <- "new"
    status$note[i]   <- "not currently published"
    next
  }

  same <- identical(
    readBin(src,  "raw", file.info(src)$size),
    readBin(dest, "raw", file.info(dest)$size))

  if (same) {
    status$action[i] <- "unchanged"
    next
  }

  if (file.info(src)$mtime < file.info(dest)$mtime && !force) {
    status$action[i] <- "SKIPPED"
    status$note[i]   <- "preview is older than the published file; re-render, or pass --force"
    next
  }

  status$action[i] <- "update"
  status$note[i]   <- paste0(
    round(file.info(dest)$size / 1024), "KB -> ",
    round(file.info(src)$size  / 1024), "KB")
}

cat("\nFact sheet promotion", if (write) "" else "(dry run)", "\n")
cat("  from ", preview_dir, "\n  to   ", public_dir, "\n\n", sep = "")
for (i in seq_len(nrow(status))) {
  cat(sprintf("  %-10s %-42s %s\n",
              status$action[i], status$file[i], status$note[i]))
}

to_copy <- status$file[status$action %in% c("new", "update")]
skipped <- status$file[status$action == "SKIPPED"]

if (length(skipped) > 0) {
  cat("\n", length(skipped), " file(s) skipped as stale. Nothing was copied for them.\n", sep = "")
}

if (!write) {
  cat("\nDry run.", length(to_copy), "file(s) would be copied. Re-run with --write.\n")
} else if (length(to_copy) == 0) {
  cat("\nNothing to copy.\n")
} else {
  dir.create(public_dir, showWarnings = FALSE, recursive = TRUE)
  backed_up <- 0
  for (f in to_copy) {
    dest <- file.path(public_dir, f)
    if (file.exists(dest)) {
      dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
      file.copy(dest, file.path(backup_dir, f), overwrite = TRUE)
      backed_up <- backed_up + 1
    }
    file.copy(file.path(preview_dir, f), dest, overwrite = TRUE)
  }
  cat("\nCopied", length(to_copy), "file(s).\n")
  if (backed_up > 0) cat("Backed up", backed_up, "replaced file(s) to", backup_dir, "\n")
  cat("\nReminder: the Fact Sheets tab reads its list from the fact_sheets\n")
  cat("object in ADA_PARC.Rmd. A new file also needs an entry there.\n")
}
