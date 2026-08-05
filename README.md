# ADA-PARC Website

This repository holds the public ADA-PARC website, an interactive dashboard of disability outcomes across the United States. It is an R and Quarto project that renders a Shiny flexdashboard and deploys to [shinyapps.io](https://www.shinyapps.io).

This repository is the presentation layer. It reads finished datasets and displays them as maps, tables, scorecards, and factsheets. It does not download or transform data. That work happens upstream in the companion `ADAPARCDataPipeline` repository.

For a full walkthrough of how the pieces fit together, open the architecture map at `theory/website_repo_architecture.html`.

## What renders the site

The entire application is a single flexdashboard document, `ADA_PARC.Rmd`. It runs a Shiny runtime, so maps and tables respond to user input. It defines every page of the site (Home, National Data, City Data, Scorecards, Fact Sheets, and Help). Shared logic that would otherwise bloat that document lives in `scripts/functions.R`.

## Where the data comes from

The five prepared datasets in `data/final/` are produced by the `ADAPARCDataPipeline` repository and copied in.

- `national_data.Rds`
- `tracts_data.Rds`
- `tracts_sf.Rds`
- `city_place_full.Rds`
- `dict_location_crosswalk.Rds`

The app also reads `data/dict_vars.csv`, the front-end dictionary that supplies variable labels and the descriptive text shown on maps, charts, and tables.

## Repository structure

| Path | Role |
| --- | --- |
| `ADA_PARC.Rmd` | The whole application. Defines all pages, maps, tables, inputs, and downloads. |
| `scripts/functions.R` | Shared functions for building maps, charts, and tables. |
| `data/final/` | The five prepared datasets from the pipeline. The app reads these. |
| `data/dict_vars.csv` | Front-end dictionary of variable labels and descriptive text. |
| `scorecard/` | Scorecard render layer. `state_scorecard.qmd` and `category_scorecard.qmd`, their CSS, index and indicator dictionaries, and scorecard data. Rendered at runtime. |
| `factsheets/` | Factsheet system. `_generate/generate-factsheets.qmd` builds the four topic factsheets. |
| `national/` | `national_topic.qmd`, the branded single-topic download (map plus data table). |
| `www/` | Web assets. `styles.css`, `cssloaders.html`, the logo, images, and deployed factsheet and report files. |
| `rsconnect/` | shinyapps.io deployment records. |
| `theory/` | Project documentation, including the repository architecture map. |
| `renv.lock`, `renv/` | Locked package versions for a reproducible environment. |

Note that `archive/`, `backup/`, and the older site-version folder hold superseded material and are slated for cleanup.

## Styling and accessibility

Two files carry the brand and the accessibility behavior for the whole site.

- `www/styles.css` is the canonical source for the EB Garamond font, design tokens, typography, tables, focus rings, and navigation styling.
- `www/cssloaders.html` adds the font, a skip-to-content link, a document language, the WAI-ARIA tabs pattern with keyboard navigation, reduced-motion handling, and loading spinners.

Accessibility is a core commitment of this project. These behaviors are layered on top of flexdashboard, so confirm they still work when you change the navigation or add a page.

## Running locally

1. Restore the locked environment with `renv::restore()` so your packages match the deployed site.
2. Open the project in RStudio and run `ADA_PARC.Rmd` (Run Document).

## Updating and deploying

A data update originates upstream. Run the pipeline in `ADAPARCDataPipeline`, then copy the five final `.Rds` files into `data/final/` here. A presentation update happens entirely within this repository, in `ADA_PARC.Rmd`, `scripts/functions.R`, or the `www/` styles.

Deploy to shinyapps.io with the `rsconnect` package. Scorecards render at runtime, so a redeploy alone does not always refresh a committed scorecard file. Verify the fresh render after deploying.
