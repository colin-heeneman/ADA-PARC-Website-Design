# =============================================================================
# prose_docx_to_yml.R
#
# Converts the shared editorial document docs/factsheet-prose.docx into
# factsheets/_generate/content/factsheet-content.yml, the file the fact sheet
# generator reads.
#
# Direction of authority: the Word document is the editorial source of truth.
# The YAML is generated from it and committed so changes are reviewable as
# diffs, but it is not hand-edited once this script is in use. Conversion is
# one-way. Two-way sync between a document and a structured file eventually
# loses an edit.
#
# Structure the document must follow (see its own "How to use this document"
# page, which states the same rules for editors):
#
#   Heading 1   <Sheet title>  [key: <yaml_key>]
#     "Category: ..."  paragraph, skipped
#     Heading 2   Introduction        -> intro (list of paragraphs)
#     Heading 2   Takeaway            -> takeaway (one lead sentence; simplified
#                                        single-indicator template only)
#     Heading 2   Section leads
#       Heading 3 <Section name>  [key: <section_key>]
#                                     -> <section_key>_lead
#     Heading 2   Summary statistics  -> summary_stats, "number | label" per line
#     Heading 2   About These Data    -> about$title / about$body
#     Heading 2   Footnotes           -> footnotes (ordered list)
#
# Word formatting is converted back to the small HTML subset the generator
# expects: bold -> <strong>, italic -> <em>, superscript -> <sup>. Characters
# that must survive as entities are escaped. Year placeholders such as
# {acs_start} are passed through untouched and filled at render time.
#
# Run:  Rscript scripts/prose_docx_to_yml.R
#       Rscript scripts/prose_docx_to_yml.R --dry-run    (report only, no write)
#
# Deps: officer, yaml, dplyr
# =============================================================================

suppressPackageStartupMessages({
  library(officer)
  library(yaml)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

args     <- commandArgs(trailingOnly = TRUE)
dry_run  <- "--dry-run" %in% args

docx_path <- file.path("docs", "factsheet-prose.docx")
yml_path  <- file.path("factsheets", "_generate", "content", "factsheet-content.yml")

if (!file.exists(docx_path)) {
  stop("Cannot find ", docx_path,
       ". This script expects to run from the project root.")
}

# Headings the parser recognizes. Anything else at Heading 2 is an error rather
# than a silent skip, because a renamed heading means prose is being dropped.
KNOWN_H2 <- c("Introduction", "Takeaway", "Section leads", "Summary statistics",
              "About These Data", "Footnotes")

# ---- inline formatting -------------------------------------------------------

# Escape the characters that must be entities in the generator's HTML, without
# touching the {placeholder} braces or the tags we are about to add ourselves.
esc_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("–", "&ndash;", x, fixed = TRUE)   # en dash
  x <- gsub("—", "&ndash;", x, fixed = TRUE)   # em dash, normalized
  x <- gsub("×", "&times;", x, fixed = TRUE)
  x <- gsub("§", "&sect;",  x, fixed = TRUE)
  x <- gsub("“", "&ldquo;", x, fixed = TRUE)
  x <- gsub("”", "&rdquo;", x, fixed = TRUE)
  x <- gsub("’", "&rsquo;", x, fixed = TRUE)
  x
}

# Hyperlink targets live in word/_rels/document.xml.rels, keyed by the r:id on
# each w:hyperlink node. Read them once so citation links survive the round trip
# instead of being silently flattened to plain text.
read_rels <- function(path) {
  tmp <- tempfile("proserels"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = "word/_rels/document.xml.rels", exdir = tmp)
  f <- file.path(tmp, "word", "_rels", "document.xml.rels")
  if (!file.exists(f)) return(character(0))
  x  <- xml2::read_xml(f)
  nd <- xml2::xml_find_all(x, "//*[local-name()='Relationship']")
  setNames(xml2::xml_attr(nd, "Target"), xml2::xml_attr(nd, "Id"))
}

RELS <- read_rels(docx_path)

# Rebuild one paragraph's HTML from its runs. officer's docx_summary() collapses
# a paragraph to plain text, so runs are read from the document XML directly to
# preserve bold, italic, superscript and hyperlinks.
runs_to_html <- function(run_nodes) {
  pieces <- vapply(run_nodes, function(r) {
    # A w:hyperlink wraps its own runs. Resolve the target and recurse.
    if (xml2::xml_name(r) == "hyperlink") {
      rid  <- xml2::xml_attr(r, "id")
      href <- if (!is.na(rid) && rid %in% names(RELS)) RELS[[rid]] else NA_character_
      inner <- runs_to_html(xml2::xml_find_all(r, "./w:r"))
      if (is.na(href) || !nzchar(inner)) return(inner)
      return(paste0('<a href="', href,
                    '" target="_blank" rel="noopener noreferrer">', inner, '</a>'))
    }
    txt <- paste(xml2::xml_text(xml2::xml_find_all(r, "./w:t")), collapse = "")
    if (!nzchar(txt)) return("")
    rpr  <- xml2::xml_find_first(r, "./w:rPr")
    has  <- function(tag) {
      if (inherits(rpr, "xml_missing")) return(FALSE)
      n <- xml2::xml_find_first(rpr, paste0("./w:", tag))
      if (inherits(n, "xml_missing")) return(FALSE)
      val <- xml2::xml_attr(n, "val")
      is.na(val) || !val %in% c("0", "false", "none")
    }
    vert <- if (inherits(rpr, "xml_missing")) NA_character_ else
      xml2::xml_attr(xml2::xml_find_first(rpr, "./w:vertAlign"), "val")

    out <- esc_text(txt)
    if (isTRUE(has("b")))  out <- paste0("<strong>", out, "</strong>")
    if (isTRUE(has("i")))  out <- paste0("<em>", out, "</em>")
    if (!is.na(vert) && vert == "superscript") out <- paste0("<sup>", out, "</sup>")
    out
  }, character(1))

  # Merge adjacent identical tags so "<strong>a</strong><strong>b</strong>",
  # which Word produces routinely, becomes "<strong>ab</strong>".
  html <- paste(pieces, collapse = "")
  for (tag in c("strong", "em", "sup")) {
    html <- gsub(paste0("</", tag, ">(\\s*)<", tag, ">"), "\\1", html)
  }
  trimws(html)
}

# ---- read the document -------------------------------------------------------

doc  <- officer::read_docx(docx_path)
# docx_body_xml() is the documented accessor; older officer versions only expose
# the internal object, so fall back rather than failing on a version bump.
body <- tryCatch(officer::docx_body_xml(doc),
                 error = function(e) doc$doc_obj$get())
paras <- xml2::xml_find_all(body, "//w:body/w:p")

para_style <- function(p) {
  s <- xml2::xml_attr(xml2::xml_find_first(p, "./w:pPr/w:pStyle"), "val")
  if (is.na(s)) "" else s
}

is_heading <- function(style, level) {
  grepl(paste0("^(Heading", level, "|heading ", level, ")$"), style,
        ignore.case = TRUE)
}

# ---- walk --------------------------------------------------------------------

content   <- list()
sheet_key <- NULL
h2        <- NULL
sec_key   <- NULL
about_ttl <- NULL
problems  <- character(0)

add <- function(key, field, value) {
  if (is.null(content[[key]])) content[[key]] <<- list()
  content[[key]][[field]] <<- c(content[[key]][[field]], value)
}

for (p in paras) {
  style <- para_style(p)
  # Top-level runs AND hyperlink wrappers, in document order, so a link in the
  # middle of a paragraph does not get hoisted to the end.
  rn    <- xml2::xml_find_all(p, "./w:r | ./w:hyperlink")
  txt   <- trimws(paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t")), collapse = ""))
  if (!nzchar(txt)) next

  # ---- Heading 1: a sheet ----
  if (is_heading(style, 1)) {
    m <- regmatches(txt, regexpr("\\[key:\\s*([A-Za-z0-9_]+)\\]", txt))
    if (length(m) == 0) {
      # The "How to use this document" page is a Heading 1 with no key.
      sheet_key <- NULL; h2 <- NULL; sec_key <- NULL
      next
    }
    sheet_key <- sub(".*\\[key:\\s*([A-Za-z0-9_]+)\\].*", "\\1", m)
    h2 <- NULL; sec_key <- NULL
    if (is.null(content[[sheet_key]])) content[[sheet_key]] <- list()
    next
  }

  if (is.null(sheet_key)) next          # front matter

  # ---- Heading 2: a section of the sheet ----
  if (is_heading(style, 2)) {
    h2 <- txt; sec_key <- NULL
    if (!h2 %in% KNOWN_H2) {
      problems <- c(problems, paste0(
        "Unrecognized Heading 2 '", h2, "' on sheet '", sheet_key,
        "'. Expected one of: ", paste(KNOWN_H2, collapse = ", "), "."))
    }
    next
  }

  # ---- Heading 3: one indicator section lead ----
  if (is_heading(style, 3)) {
    m <- regmatches(txt, regexpr("\\[key:\\s*([A-Za-z0-9_]+)\\]", txt))
    if (length(m) == 0) {
      problems <- c(problems, paste0(
        "Heading 3 '", txt, "' on sheet '", sheet_key, "' has no [key: ...]."))
      sec_key <- NULL
    } else {
      sec_key <- sub(".*\\[key:\\s*([A-Za-z0-9_]+)\\].*", "\\1", m)
    }
    next
  }

  # ---- body paragraphs ----
  if (grepl("^Category:", txt)) next                     # category label
  if (grepl("^\\[.*\\]$", txt)) next                     # unfilled placeholder
  if (is.null(h2)) next

  html <- runs_to_html(rn)
  if (!nzchar(html)) next

  if (h2 == "Introduction") {
    add(sheet_key, "intro", html)

  } else if (h2 == "Takeaway") {
    add(sheet_key, "takeaway", html)

  } else if (h2 == "Section leads") {
    if (is.null(sec_key)) next
    add(sheet_key, paste0(sec_key, "_lead"), html)

  } else if (h2 == "Summary statistics") {
    if (grepl("^One per line", txt)) next                # the editor hint
    parts <- strsplit(html, "\\s*\\|\\s*")[[1]]
    if (length(parts) != 2) {
      problems <- c(problems, paste0(
        "Summary statistic on sheet '", sheet_key,
        "' is not in 'number | label' form: ", txt))
    } else {
      if (is.null(content[[sheet_key]]$summary_stats))
        content[[sheet_key]]$summary_stats <- list()
      content[[sheet_key]]$summary_stats <- c(
        content[[sheet_key]]$summary_stats,
        list(list(number = parts[1], label = parts[2])))
    }

  } else if (h2 == "About These Data") {
    if (grepl("^Box title:", txt)) {
      about_ttl <- trimws(sub("^Box title:\\s*", "", txt))
      next
    }
    content[[sheet_key]]$about <- list(
      title = about_ttl %||% "About These Data", body = html)
    about_ttl <- NULL

  } else if (h2 == "Footnotes") {
    add(sheet_key, "footnotes", html)
  }
}

# ---- validation --------------------------------------------------------------
# Fail loudly. A silent parse failure publishes a fact sheet with missing prose.

# A sheet with no prose at all is a scaffold for a phase not yet started. That
# is expected, so it is reported and dropped rather than treated as a failure.
# A sheet with SOME prose but a missing Introduction or footnotes is a real
# error: something was half-written, or a heading was renamed and the text under
# it was silently lost.
is_empty_scaffold <- function(s) {
  length(s$intro) == 0 && length(s$footnotes) == 0 &&
    length(grep("_lead$", names(s))) == 0 &&
    is.null(s$about) && is.null(s$summary_stats)
}

scaffolds <- names(content)[vapply(content, is_empty_scaffold, logical(1))]
content    <- content[setdiff(names(content), scaffolds)]

for (k in names(content)) {
  s <- content[[k]]
  if (is.null(s$intro) || length(s$intro) == 0) {
    problems <- c(problems, paste0("Sheet '", k, "' has no Introduction text."))
  }
  if (is.null(s$footnotes) || length(s$footnotes) == 0) {
    problems <- c(problems, paste0("Sheet '", k, "' has no footnotes."))
  }
  # Every <sup>n</sup> marker in body text must have a footnote to point at.
  bodytext <- unlist(s[setdiff(names(s), c("footnotes", "summary_stats"))])
  marks <- unlist(regmatches(bodytext, gregexpr("<sup>([0-9]+)</sup>", bodytext)))
  if (length(marks) > 0) {
    nums <- as.integer(gsub("\\D", "", marks))
    if (any(nums > length(s$footnotes))) {
      problems <- c(problems, paste0(
        "Sheet '", k, "' cites footnote ", max(nums), " but only ",
        length(s$footnotes), " footnote(s) are listed."))
    }
  }
}

if (length(problems) > 0) {
  cat("\nProblems found:\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  cat("\n")
  stop("Refusing to write ", yml_path, " until the document is corrected.")
}

# ---- report ------------------------------------------------------------------

cat("\nParsed", length(content), "drafted sheet(s) from", docx_path, "\n\n")
for (k in names(content)) {
  s <- content[[k]]
  leads <- grep("_lead$", names(s), value = TRUE)
  cat(sprintf("  %-20s intro %d para, %d section lead(s), %d footnote(s)%s\n",
              k, length(s$intro), length(leads), length(s$footnotes),
              if (!is.null(s$about)) ", about" else ""))
}

if (length(scaffolds) > 0) {
  cat("\nNot yet drafted, skipped:\n  ",
      paste(scaffolds, collapse = ", "), "\n", sep = "")
}

if (dry_run) {
  cat("\nDry run. Nothing written.\n")
} else {
  header <- c(
    "# =============================================================================",
    "# factsheet-content.yml",
    "#",
    "# GENERATED FILE. Do not edit by hand.",
    "#",
    "# Source: docs/factsheet-prose.docx",
    "# Regenerate: Rscript scripts/prose_docx_to_yml.R",
    paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "#",
    "# Edits made here will be lost the next time the Word document is converted.",
    "# Edit the Word document instead; it is the editorial source of truth.",
    "# =============================================================================",
    "")
  tmp <- yaml::as.yaml(content, indent = 2)
  writeLines(c(header, tmp), yml_path, useBytes = TRUE)
  cat("\nWrote", yml_path, "\n")
}
