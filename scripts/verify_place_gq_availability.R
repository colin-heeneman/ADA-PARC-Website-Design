# ---------------------------------------------------------------------------
# verify_place_gq_availability.R
#
# Settles whether the ACS group quarters tables the Community Living domain
# depends on are published below state geography.
#
# Background. All 62 pop_grp_* and pct_grp_* columns in city_place_full.Rds are
# empty for all 8,407 places and all 85,382 tracts, while B25070 and B25091 in
# the same extract are complete. Two explanations were possible: either the
# request is malformed for these particular tables, which would be a contained
# pipeline fix, or the Census does not publish them at these geographies, which
# would make Community Living permanently unavailable at city level. Those two
# outcomes have very different costs, so the question is settled here directly
# against the API rather than inferred from the empty extract.
#
# Method. One variable from each table is requested at four geographies for a
# single large, unambiguous case: Illinois, Cook County, Chicago and one Chicago
# tract. Chicago is chosen because it is the third largest city in the country,
# so a null there cannot be attributed to a small-population suppression rule.
# A null returned alongside a 200 response and a correctly named row means the
# geography is accepted and the estimate is simply not published.
#
# B26001, group quarters population, is included as a control and as the
# candidate substitute. It carries no disability crossing, so it can only ever
# appear as citywide context in the manner of housing cost burden.
#
# Requires a Census API key. As of 2026 the API rejects unkeyed data requests
# outright, so this cannot be run anonymously. Set CENSUS_API_KEY in ~/.Renviron
# rather than passing it inline:
#   CENSUS_API_KEY=your_key_here
#
# Writes docs/city_gq_availability_check.csv.
# ---------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

census_key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key)) {
  stop(
    "CENSUS_API_KEY is not set. Add it to ~/.Renviron and restart R.\n",
    "Request a key at https://api.census.gov/data/key_signup.html"
  )
}

VINTAGE <- 2023

# Each probe names the table, the single variable used to test it, and which
# API endpoint it lives on. Subject tables (S....) and detailed tables (B....)
# are served from different paths.
probes <- tribble(
  ~table,     ~variable,          ~endpoint,
  "S2601A",   "S2601A_C01_001E",  "subject",
  "S2602",    "S2602_C01_001E",   "subject",
  "B26108",   "B26108_001E",      "detail",
  "B26001",   "B26001_001E",      "detail"
)

# Illinois, Cook County, Chicago, and one Chicago tract.
geographies <- tribble(
  ~geography, ~for_clause,        ~in_clause,
  "state",    "state:17",         NA_character_,
  "county",   "county:031",       "state:17",
  "place",    "place:14000",      "state:17",
  "tract",    "tract:842200",     "state:17 county:031"
)

fetch_one <- function(variable, endpoint, for_clause, in_clause) {
  base <- if (endpoint == "subject") {
    sprintf("https://api.census.gov/data/%d/acs/acs5/subject", VINTAGE)
  } else {
    sprintf("https://api.census.gov/data/%d/acs/acs5", VINTAGE)
  }

  query <- list(get = paste0("NAME,", variable), `for` = for_clause)
  if (!is.na(in_clause)) query$`in` <- in_clause
  query$key <- census_key

  resp <- httr::GET(base, query = query)

  if (httr::status_code(resp) != 200) {
    return(list(
      status = httr::status_code(resp),
      name   = NA_character_,
      value  = NA_character_,
      note   = "request rejected"
    ))
  }

  body <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  # Row one is the header, row two is the single record requested.
  list(
    status = 200L,
    name   = body[2, 1],
    value  = body[2, 2],
    note   = if (is.na(body[2, 2])) {
      "geography accepted, estimate not published"
    } else {
      "published"
    }
  )
}

results <- tidyr::crossing(probes, geographies) %>%
  rowwise() %>%
  mutate(res = list(fetch_one(variable, endpoint, for_clause, in_clause))) %>%
  mutate(
    http_status = res$status,
    area_name   = res$name,
    estimate    = res$value,
    finding     = res$note
  ) %>%
  select(table, variable, geography, area_name, http_status, estimate, finding) %>%
  ungroup() %>%
  arrange(table, match(geography, geographies$geography))

print(as.data.frame(results))

out_path <- file.path("docs", "city_gq_availability_check.csv")
readr::write_csv(results, out_path)
message("Wrote ", out_path)

# ---------------------------------------------------------------------------
# Interpreting the output.
#
# A row with http_status 200, a correct area_name and an NA estimate is the
# decisive case: the API understood the geography and has nothing to return.
# That is a publication rule, not a request error, and no change to
# city_import_acs.R can produce the value.
#
# If instead S2601A, S2602 and B26108 come back populated at place, the empty
# columns in city_place_full.Rds are a pipeline defect and the extract request
# should be re-examined.
# ---------------------------------------------------------------------------
