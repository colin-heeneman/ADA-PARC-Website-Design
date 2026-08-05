# audit_employment_acs.R -------------------------------------------------------
#
# PURPOSE
#   Independently verify the ADA-PARC employment status indicators (Employed,
#   Unemployed, Not in Labor Force) in data/final/national_data.Rds and
#   data/final/city_place_full.Rds against the source ACS table pulled fresh
#   from the Census Bureau API.
#
#   Written in response to a client query (July 2026) questioning whether the
#   Employed share was too high and the Not in Labor Force share too low.
#
# SOURCE OF TRUTH
#   ACS 5-Year Detailed Table C18120, "Employment Status by Disability Status"
#   Universe: Civilian noninstitutionalized population 18 to 64 years
#     C18120_004  Employed, With a disability
#     C18120_005  Employed, No disability
#     C18120_007  Unemployed, With a disability
#     C18120_008  Unemployed, No disability
#     C18120_010  Not in labor force, With a disability
#     C18120_011  Not in labor force, No disability
#
# DENOMINATOR CONVENTION (see R/addACSCalculatedVariables.R in the pipeline)
#   Each share is expressed over the sum of the three C18120 cells for the same
#   disability group, so the three shares sum to exactly 100 percent.
#
# OUTPUTS (written to docs/)
#   employment_audit_state_comparison.csv
#   employment_audit_city_comparison.csv
#
# USAGE
#   Sys.setenv(CENSUS_API_KEY = "...")   # or tidycensus::census_api_key()
#   source("scripts/audit_employment_acs.R")
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(tidycensus)

acs_year   <- 2024   # must match config.yml acs$years in ADAPARCDataPipeline
acs_survey <- "acs5"

vars <- c(
  emp_dis      = "C18120_004",
  emp_nodis    = "C18120_005",
  unemp_dis    = "C18120_007",
  unemp_nodis  = "C18120_008",
  notlabor_dis = "C18120_010",
  notlabor_nodis = "C18120_011"
)

# 1. Pull source ACS ----------------------------------------------------------

pull_acs <- function(geography, ...) {
  get_acs(
    geography = geography,
    variables = vars,
    year      = acs_year,
    survey    = acs_survey,
    output    = "wide",
    cache_table = TRUE,
    ...
  ) |>
    select(GEOID, NAME, ends_with("E"), -NAME) |>
    rename_with(~ sub("E$", "", .x), ends_with("E"))
}

acs_us    <- pull_acs("us")
acs_state <- pull_acs("state")
acs_place <- pull_acs("place")

acs_natl <- bind_rows(acs_us, acs_state) |>
  mutate(NAME = sub(" city.*$", "", NAME))

# 2. Load local ADA-PARC data -------------------------------------------------

national <- readRDS("data/final/national_data.Rds")
city     <- readRDS("data/final/city_place_full.Rds")

local_cols <- c(
  "pop_cni_18to64_employed_dis", "pop_cni_18to64_unemployed_dis",
  "pop_cni_18to64_notlabor_dis", "pop_cni_18to64_employed_nodis",
  "pop_cni_18to64_unemployed_nodis", "pop_cni_18to64_notlabor_nodis",
  "pct_cni_18to64_employed_dis", "pct_cni_18to64_unemployed_dis",
  "pct_cni_18to64_notlabor_dis"
)

# 3. Compare ------------------------------------------------------------------

compare <- function(local, acs, join_by) {

  local |>
    select(GEOID, NAME, all_of(local_cols)) |>
    inner_join(acs, by = join_by) |>
    mutate(
      base_dis = emp_dis + unemp_dis + notlabor_dis,

      # count-level reconciliation
      diff_employed_n   = pop_cni_18to64_employed_dis   - emp_dis,
      diff_unemployed_n = pop_cni_18to64_unemployed_dis - unemp_dis,
      diff_notlabor_n   = pop_cni_18to64_notlabor_dis   - notlabor_dis,

      # percentage recomputed independently from the fresh ACS pull
      acs_employed_pct   = emp_dis      / base_dis * 100,
      acs_unemployed_pct = unemp_dis    / base_dis * 100,
      acs_notlabor_pct   = notlabor_dis / base_dis * 100,

      diff_employed_pct   = pct_cni_18to64_employed_dis   - acs_employed_pct,
      diff_unemployed_pct = pct_cni_18to64_unemployed_dis - acs_unemployed_pct,
      diff_notlabor_pct   = pct_cni_18to64_notlabor_dis   - acs_notlabor_pct,

      shares_sum = pct_cni_18to64_employed_dis +
                   pct_cni_18to64_unemployed_dis +
                   pct_cni_18to64_notlabor_dis
    )
}

state_cmp <- compare(national, acs_natl, join_by = "NAME")
city_cmp  <- compare(city,     acs_place, join_by = "GEOID")

# 4. Report -------------------------------------------------------------------

summarise_audit <- function(x, label) {
  d <- x |>
    summarise(
      n = n(),
      max_abs_diff_count = max(abs(c(diff_employed_n, diff_unemployed_n,
                                     diff_notlabor_n)), na.rm = TRUE),
      max_abs_diff_pct   = max(abs(c(diff_employed_pct, diff_unemployed_pct,
                                     diff_notlabor_pct)), na.rm = TRUE),
      shares_sum_min = min(shares_sum, na.rm = TRUE),
      shares_sum_max = max(shares_sum, na.rm = TRUE)
    )
  message(label, ": n = ", d$n,
          " | max |count diff| = ", d$max_abs_diff_count,
          " | max |pct diff| = ", signif(d$max_abs_diff_pct, 3),
          " | shares sum range = [", round(d$shares_sum_min, 6), ", ",
          round(d$shares_sum_max, 6), "]")
  d
}

summarise_audit(state_cmp, "National + states")
summarise_audit(city_cmp,  "Places")

dir.create("docs", showWarnings = FALSE)
write_csv(state_cmp, "docs/employment_audit_state_comparison.csv")
write_csv(city_cmp,  "docs/employment_audit_city_comparison.csv")

# 5. Context check: alternative universes that produce different-looking rates -
#   S1811 reports the employed share over the CNI population age 16 and OVER,
#   which folds in retirement-age adults and yields a much lower employed share.
#   This is the most common source of an apparent discrepancy.

s1811 <- get_acs(
  geography = "us",
  variables = c(pct_emp_dis_16plus = "S1811_C02_002"),
  year = acs_year, survey = acs_survey
)
print(s1811)
