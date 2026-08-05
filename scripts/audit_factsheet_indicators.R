# =============================================================================
# audit_factsheet_indicators.R
#
# Verification script for docs/factsheets_audit.qmd.
#
# For every indicator the ten target fact sheets require, checks:
#   1. whether the variable is defined in data/dict_vars.csv
#   2. whether the column actually exists in data/final/national_data.Rds
#   3. whether it is populated for all 50 states plus DC
#
# Writes docs/factsheets_audit_indicators.csv and prints a summary. Any row
# with status other than "in data" contradicts a claim in the audit document
# and means that document is stale.
#
# Run:  Rscript scripts/audit_factsheet_indicators.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

# ── The indicator specification the audit document asserts ───────────────────
# Kept here rather than in the .qmd so the claim and its test live in one place.
spec <- tribble(
  ~sheet,                     ~section,                              ~dis_var,                           ~nodis_var,                          ~acs_source,     ~direction,

  "Living in Community",      "Living at home",                      "pct_grp_dis_home",                 "pct_grp_nodis_home",                "S2601A/B26108", "high_good",
  "Living in Community",      "Non-institutional group quarters",    "pct_grp_dis_noninstgroupquarters", "pct_grp_nodis_noninstgroupquarters","S2601A/B26108", "high_good",

  "Living in Institution",    "Nursing home, ages 18-64",            "pct_grp_dis_18to64_nursing",       "pct_grp_nodis_18to64_nursing",      "S2602",         "low_good",
  "Living in Institution",    "Nursing home, ages 65+",              "pct_grp_dis_65plus_nursing",       "pct_grp_nodis_65plus_nursing",      "S2602",         "low_good",
  "Living in Institution",    "Correctional facility",               "pct_grp_dis_corrections",          "pct_grp_nodis_corrections",         "S2601A/B26108", "low_good",
  "Living in Institution",    "Any institution, combined",           "pct_grp_dis_instgroupquarters",    NA_character_,                       "S2601A",        "low_good",

  "Employment",               "Employed",                            "pct_cni_18to64_employed_dis",      "pct_cni_18to64_employed_nodis",     "C18120",        "high_good",
  "Employment",               "Unemployed",                          "pct_cni_18to64_unemployed_dis",    "pct_cni_18to64_nodis_unemployed",   "C18120",        "low_good",
  "Employment",               "Not in labor force",                  "pct_cni_18to64_notlabor_dis",      "pct_cni_18to64_notlabor_nodis",     "C18120",        "context",

  "Educational Attainment",   "Less than high school, 25+",          "pct_cni_25plus_noGED_dis",         "pct_cni_25plus_noGED_nodis",        "S1811",         "low_good",
  "Educational Attainment",   "High school or equivalent, 25+",      "pct_cni_25plus_GED_dis",           "pct_cni_25plus_GED_nodis",          "S1811",         "context",
  "Educational Attainment",   "Bachelor's degree or higher, 25+",    "pct_cni_25plus_bachelors_dis",     "pct_cni_25plus_bachelors_nodis",    "S1811",         "high_good",

  "Health Insurance Access",  "No coverage, 19-64",                  "pct_cni_19to64_dis_nohealthins",   "pct_cni_19to64_nodis_nohealthins",  "B18135",        "low_good",
  "Health Insurance Access",  "No coverage, 65+",                    "pct_cni_65plus_dis_nohealthins",   "pct_cni_65plus_nodis_nohealthins",  "B18135",        "low_good",
  "Health Insurance Access",  "Covered, 19-64",                      "pct_cni_19to64_dis_healthins",     "pct_cni_19to64_nodis_healthins",    "B18135",        "high_good",
  "Health Insurance Access",  "Covered, 65+",                        "pct_cni_65plus_dis_healthins",     "pct_cni_65plus_nodis_healthins",    "B18135",        "high_good",
  "Health Insurance Access",  "Public coverage, 19-64",              "pct_cni_19to64_dis_publichealth",  "pct_cni_19to64_nodis_publichealth", "B18135",        "context",
  "Health Insurance Access",  "Public coverage, 65+",                "pct_cni_65plus_dis_publichealth",  "pct_cni_65plus_nodis_publichealth", "B18135",        "context",
  "Health Insurance Access",  "Private coverage, 19-64",             "pct_cni_19to64_dis_privatehealth", "pct_cni_19to64_nodis_privatehealth","B18135",        "context",
  "Health Insurance Access",  "Private coverage, 65+",               "pct_cni_65plus_dis_privatehealth", "pct_cni_65plus_nodis_privatehealth","B18135",        "context",

  "Commuting to Work",        "Public transit, 16+",                 "pct_cni_16plus_transit_dis",       "pct_cni_16plus_transit_nodis",      "S1811",         "context",
  "Commuting to Work",        "Drove alone, 16+",                    "pct_cni_16plus_drivealone_dis",    "pct_cni_16plus_drivealone_nodis",   "S1811",         "context",

  "Technology Access",        "Computer",                            "pwd_computer_pct",                 "pwod_computer_pct",                 "2022 ACS PUMS", "high_good",
  "Technology Access",        "Internet",                            "pwd_int_pct",                      "pwod_int_pct",                      "2022 ACS PUMS", "high_good",
  "Technology Access",        "Smartphone",                          "pwd_smartphone_pct",               "pwod_smartphone_pct",               "2022 ACS PUMS", "high_good"
)

# ── Sources ───────────────────────────────────────────────────────────────────
dict <- readr::read_csv(here::here("data", "dict_vars.csv"), show_col_types = FALSE)

national <- readRDS(here::here("data", "final", "national_data.Rds"))
states <- national |> dplyr::filter(!ABBR %in% c("USA", "PR"))
n_states <- nrow(states)

stopifnot(n_states == 51)

# ── Checks ────────────────────────────────────────────────────────────────────
check_var <- function(v) {
  if (is.na(v)) {
    return(tibble(in_dict = NA, in_data = NA, n_nonmissing = NA_integer_,
                  status = "not specified"))
  }
  in_dict <- v %in% dict$var_readable
  in_data <- v %in% names(states)
  n_ok <- if (in_data) sum(!is.na(states[[v]])) else 0L

  status <- dplyr::case_when(
    !in_data                 ~ "MISSING from national_data.Rds",
    n_ok < n_states          ~ paste0("incomplete (", n_ok, "/", n_states, ")"),
    !in_dict                 ~ "in data, undocumented in dict_vars",
    TRUE                     ~ "in data"
  )
  tibble(in_dict = in_dict, in_data = in_data,
         n_nonmissing = as.integer(n_ok), status = status)
}

result <- spec |>
  dplyr::rowwise() |>
  dplyr::mutate(
    dis_check   = list(check_var(dis_var)),
    nodis_check = list(check_var(nodis_var))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    dis_status   = vapply(dis_check,   function(x) x$status, character(1)),
    nodis_status = vapply(nodis_check, function(x) x$status, character(1)),
    dis_n        = vapply(dis_check,   function(x) as.integer(x$n_nonmissing), integer(1)),
    nodis_n      = vapply(nodis_check, function(x) as.integer(x$n_nonmissing), integer(1))
  ) |>
  dplyr::select(sheet, section, acs_source, direction,
                dis_var, dis_status, dis_n,
                nodis_var, nodis_status, nodis_n)

# ── Blank var_readable rows, the dictionary hygiene finding ───────────────────
blank_rows <- dict |>
  dplyr::mutate(.row = dplyr::row_number() + 1L) |>   # +1 for the header line
  dplyr::filter(is.na(var_readable) | !nzchar(trimws(var_readable))) |>
  dplyr::select(.row, var_topic, var_pretty, var_database)

# ── Duplicated var_readable within the dictionary ─────────────────────────────
dupes <- dict |>
  dplyr::filter(!is.na(var_readable), nzchar(trimws(var_readable))) |>
  dplyr::count(var_readable, name = "n") |>
  dplyr::filter(n > 1)

# ── Output ────────────────────────────────────────────────────────────────────
out_path <- here::here("docs", "factsheets_audit_indicators.csv")
readr::write_csv(result, out_path)

cat("\nFact sheet indicator audit\n")
cat("Data vintage: national_data.Rds,", n_states, "states plus DC\n\n")

cat("Indicator sections by status:\n")
print(result |> dplyr::count(dis_status, name = "sections"), n = Inf)

problems <- result |> dplyr::filter(dis_status != "in data" | nodis_status != "in data")
if (nrow(problems) > 0) {
  cat("\nSections needing attention:\n")
  print(problems |> dplyr::select(sheet, section, dis_var, dis_status,
                                  nodis_var, nodis_status), n = Inf)
} else {
  cat("\nAll indicator sections resolve cleanly.\n")
}

cat("\ndict_vars.csv rows with a blank var_readable:", nrow(blank_rows), "\n")
if (nrow(blank_rows) > 0) print(blank_rows, n = Inf)

cat("\ndict_vars.csv duplicated var_readable values:", nrow(dupes), "\n")
if (nrow(dupes) > 0) print(dupes, n = Inf)

cat("\nWritten to", out_path, "\n")
