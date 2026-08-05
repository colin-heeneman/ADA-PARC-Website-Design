# ---------------------------------------------------------------------------
# audit_city_indicator_coverage.R
#
# Regenerates the coverage figures reported in docs/city_data_audit.html.
#
# For every indicator that appears in the National Data dropdown, this script
# reports whether the underlying column exists in the city place file and, if
# so, how many of the 8,407 places carry a non-missing value.
#
# Run from the project root:
#   Rscript scripts/audit_city_indicator_coverage.R
#
# Writes: docs/city_data_audit_coverage.csv
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

dict_vars       <- read_csv(here::here("data", "dict_vars.csv"), show_col_types = FALSE)
city_place_full <- readRDS(here::here("data", "final", "city_place_full.Rds"))
tracts_data     <- readRDS(here::here("data", "final", "tracts_data.Rds"))
national_data   <- readRDS(here::here("data", "final", "national_data.Rds"))

# Indicators offered in the national dropdown ------------------------------
national_indicators <- dict_vars %>%
  filter(!is.na(national_dropdown_label), national_dropdown_label != "NA") %>%
  select(var_readable, var_topic, national_dropdown_label,
         data_source_label,
         is_demographics, is_community_living,
         is_community_participation, is_work_economic)

n_places <- nrow(city_place_full)
n_tracts <- nrow(tracts_data)

coverage <- national_indicators %>%
  rowwise() %>%
  mutate(
    in_national     = var_readable %in% names(national_data),
    in_city_file    = var_readable %in% names(city_place_full),
    city_non_na     = if (in_city_file) sum(!is.na(city_place_full[[var_readable]])) else NA_integer_,
    city_pct_non_na = if (in_city_file) round(100 * city_non_na / n_places, 1) else NA_real_,
    in_tract_file   = var_readable %in% names(tracts_data),
    tract_non_na    = if (in_tract_file) sum(!is.na(tracts_data[[var_readable]])) else NA_integer_
  ) %>%
  ungroup() %>%
  arrange(desc(is_demographics), desc(is_community_living),
          desc(is_community_participation), national_dropdown_label)

write_csv(coverage, here::here("docs", "city_data_audit_coverage.csv"))

cat("Places in city file:", n_places, "\n")
cat("Tracts in tract file:", n_tracts, "\n")
cat("National dropdown indicators:", nrow(coverage), "\n")
cat("Absent from city file:", sum(!coverage$in_city_file), "\n")
cat("Present but wholly empty at city level:",
    sum(coverage$in_city_file & coverage$city_non_na == 0, na.rm = TRUE), "\n")
