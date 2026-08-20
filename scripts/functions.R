
# Dashboard functions -----------------------------------------------------

# Font registration --------------------------------------------------------
# EB Garamond is the canonical site font. ggplot/ggiraph need the font
# registered with R's graphics device or text falls back to a generic
# serif/Helvetica and the SVG `font-family` attribute may not be honoured
# the way we want.
#
# Strategy (defensive, runs at source-load time):
#   1. systemfonts::register_variant — alias "EB Garamond" to any
#      installed serif so R has metrics to compute layout against.
#      ggiraph's SVG output still emits font-family="EB Garamond", and
#      the browser pairs that with the EB Garamond webfont loaded via
#      the Google Fonts <link> in www/cssloaders.html.
#   2. showtext (optional) — if available, also register the actual
#      Google Font so static PNG output renders in EB Garamond too.
# Wrapped in try() so a missing package never breaks the dashboard.
local({
  # 1. systemfonts variant alias (ships as a ggiraph dependency)
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    have_eb <- "EB Garamond" %in% systemfonts::system_fonts()$family
    if (!have_eb) {
      candidates <- c("Garamond", "EB Garamond", "Georgia", "Times New Roman", "serif")
      installed  <- systemfonts::system_fonts()$family
      fallback   <- candidates[candidates %in% installed][1]
      if (!is.na(fallback)) {
        try(systemfonts::register_variant(
          name   = "EB Garamond",
          family = fallback
        ), silent = TRUE)
      }
    }
  }

  # 2. showtext (optional; pulls the real webfont for raster output)
  if (requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)) {
    if (!"EB Garamond" %in% sysfonts::font_families()) {
      try(sysfonts::font_add_google("EB Garamond", "EB Garamond"),
          silent = TRUE)
    }
    try({
      showtext::showtext_auto()
      showtext::showtext_opts(dpi = 96)
    }, silent = TRUE)
  }
})

altTitle <- function(variable) {
  # Title, vars_pretty field for variable
  title <- dict_vars %>%
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>%
    head(1) %>%
    pull(var_pretty)
  
  title <- str_trim(str_replace_all(title, " (with|without) Disabilities", ""))
  title <- str_trim(str_replace_all(title, " (with|without) Disability", ""))
  
  return(title)
}

# Create non-overlapping quartile buckets
create_non_overlapping_buckets <- function(data) {
  # Coerce to numeric and drop NAs for break calculation
  data_num   <- suppressWarnings(as.numeric(data))
  data_clean <- data_num[!is.na(data_num)]
  
  # If no usable data, return a harmless default
  if (length(data_clean) == 0L) {
    return(seq(0, 1, length.out = 5))
  }
  
  # Basic range
  rng <- range(data_clean, na.rm = TRUE)
  
  # If no variation, create a tiny spread around the single value
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    v   <- rng[1]
    eps <- max(abs(v), 1) * 0.001
    breaks <- seq(v - 2 * eps, v + 2 * eps, length.out = 5)
  } else {
    # Try quantile-based breaks first
    probs <- seq(0, 1, length.out = 5)
    q     <- stats::quantile(data_clean, probs = probs, na.rm = TRUE)
    breaks <- as.numeric(q)
    
    # If any NAs sneak in, interpolate them
    if (anyNA(breaks)) {
      good   <- which(!is.na(breaks))
      breaks <- approx(x = good, y = breaks[good],
                       xout = seq_along(breaks))$y
    }
    
    # Sort to be safe
    breaks <- sort(breaks)
    
    # If quantiles still collapse (duplicate breaks), fall back to equal-width
    if (any(diff(breaks) <= 0)) {
      breaks <- seq(rng[1], rng[2], length.out = 5)
    }
  }
  
  # Final safety: enforce strict monotonicity so cut() never complains
  eps <- .Machine$double.eps
  for (i in 2:length(breaks)) {
    if (breaks[i] <= breaks[i - 1]) {
      breaks[i] <- breaks[i - 1] + eps
    }
  }
  
  breaks
}

format_ranges <- function(breaks, col_name) {
  if (grepl("pct", col_name, ignore.case = TRUE)) {
    # Breaks are already in percent units (e.g., 1.71, 5.90, etc.)
    formatted_breaks <- formatC(breaks, format = "f", digits = 1)
    formatted_ranges <- paste0(
      head(formatted_breaks, -1), "%-",
      tail(formatted_breaks,  -1), "%"
    )
  } else {
    # Non-percent variables: comma separated, no decimals
    formatted_breaks  <- formatC(breaks, format = "f", big.mark = ",", digits = 0)
    formatted_ranges  <- paste(
      head(formatted_breaks, -1),
      tail(formatted_breaks,  -1),
      sep = "-"
    )
  }
  formatted_ranges
}

# State border ink -------------------------------------------------------------
# One fixed border color cannot contrast with a 4-class sequential ramp that
# runs from near-white to near-black. Dark navy on the darkest heritage class is
# about 1.3:1, so clusters of high-value states merge into one blob; plain white
# on the palest class is about 1.4:1 and disappears just as badly at the other
# end.
#
# This replaces the earlier casing approach (every boundary drawn twice, a wide
# dark stroke under a narrow white inline). Casing worked but thickened every
# edge to carry two colours where only one was ever legible. Instead each state
# now takes ONE stroke whose colour is chosen from its own fill: near-black on
# the light half of the ramp, white on the dark half.
#
# WHY A LUMINANCE TEST AND NOT A BIN INDEX. render_national_map() reverses the
# palette for "neg" indicators, so bin 1 is the lightest colour on a "pos"
# indicator and the darkest on a "neg" one. Deriving the ink from the resolved
# fill colour survives that reversal with no special casing, and it keeps
# working if ADAPARC_PALETTES ever gains an entry.
#
# THE THRESHOLD is the WCAG crossover luminance, sqrt(1.05 * 0.05) - 0.05, the
# point at which black and white contrast equally against a background. Every
# palette in ADAPARC_PALETTES is admitted on a monotonic-L* criterion, so this test
# always splits a ramp cleanly into a light half and a dark half rather than
# interleaving them. Measured worst case across all six palettes plus both
# no-data fills is 5.30:1 (Blue-Gold bin 3 under white), well clear of the 3:1
# WCAG 2.2 SC 1.4.11 floor for non-text contrast. Verified in
# docs/color-palette-expansion.qmd.
map_border_ink_dark  <- "#111111"   # matches ADA_PARC_PALETTE$on_fill near-black
map_border_ink_light <- "#ffffff"
map_border_ink_width <- 0.55

# WCAG relative luminance. Vectorised over a character vector of colours.
rel_luminance <- function(hex) {
  chan <- grDevices::col2rgb(hex) / 255
  lin  <- ifelse(chan <= 0.03928, chan / 12.92, ((chan + 0.055) / 1.055)^2.4)
  as.numeric(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ])
}

# sqrt(1.05 * 0.05) - 0.05
map_border_ink_threshold <- 0.179129

#' Border colour for a given fill colour.
#'
#' @param hex Character vector of fill colours (hex or any R colour name).
#' @return Character vector of border colours, same length.
border_ink_for_fill <- function(hex) {
  ifelse(rel_luminance(hex) >= map_border_ink_threshold,
         map_border_ink_dark, map_border_ink_light)
}

#' Resolve each row's fill colour from its binned factor.
#'
#' scale_fill_manual() matches an unnamed `values` vector to factor levels
#' positionally, so the level index is the palette index. Rows the cut() dropped
#' to NA take the palette's no-data fill, which is what the scale's na.value
#' will draw.
fill_hex_from_bin <- function(bin, palette, na_color) {
  idx <- as.integer(bin)
  ifelse(is.na(idx), na_color, palette[idx])
}

# National perimeter -----------------------------------------------------------
# The conditional ink above solves interior boundaries but breaks the coastline.
# A dark-filled coastal state takes a white stroke, and against the white page
# behind the map that outer edge vanishes, so the national silhouette dissolves
# wherever the best-performing states happen to sit on the border.
#
# Fix: dissolve the state polygons once and draw that outline FIRST, wider than
# the per-state stroke. The filled layer then paints over its inner half and
# leaves a dark flank on the outside only. That is the old casing behaviour,
# kept exactly where it was doing necessary work and dropped everywhere it was
# not.
#
# st_union runs once per session and is cached; the geometry is static and the
# app re-renders this map on every indicator and palette change.
map_border_perimeter_color <- "#1c2b3a"   # = ADA_PARC_PALETTE$stroke
map_border_perimeter_width <- 1.3

.map_perimeter_cache <- new.env(parent = emptyenv())

map_border_perimeter <- function(data) {
  if (is.null(.map_perimeter_cache$outline)) {
    .map_perimeter_cache$outline <- sf::st_sf(
      geometry = sf::st_union(sf::st_geometry(data))
    )
  }
  ggplot2::geom_sf(
    data        = .map_perimeter_cache$outline,
    fill        = NA,
    color       = map_border_perimeter_color,
    linewidth   = map_border_perimeter_width,
    show.legend = FALSE
  )
}

#' Render the national choropleth.
#'
#' @param palette_selected Either a colorRampPalette function or a ColorBrewer
#'   name. The app always passes a function now (see access_map_palette_selected
#'   in ADA_PARC.Rmd); the ColorBrewer branch is kept only so this function
#'   still works when called directly from a console for debugging.
#' @param na_color Fill for states with no value. Belongs to the selected
#'   palette rather than being fixed, because High Contrast Mono needs a
#'   chromatic no-data colour; see the NO-DATA note in scripts/palettes.R. The
#'   default matches MAP_NA_DEFAULT, the shared neutral used by every chromatic
#'   palette, and is written literally so this function stays callable when
#'   palettes.R has not been sourced.
render_national_map <- function(selected, palette_selected = "YlOrRd",
                                na_color = "#b6bcbf") {
  is_comp <- dict_vars %>%
    filter(var_readable == selected) %>%
    pull(display_type) %>%
    head(1) %>%
    {. == "comp"}

  palette <- if (is.function(palette_selected)) {
    palette_selected(4)
  } else {
    brewer.pal(4, palette_selected)
  }

  # Directional coloring: the darkest color always denotes the BETTER outcome
  # for people with disabilities, regardless of indicator direction.
  #   - "pos" (and demographic / uncoded) indicators: higher value = better,
  #      so the default light-to-dark palette (dark = high) is correct.
  #   - "neg" indicators: higher value = worse, so we reverse the palette
  #      (dark = low value = better outcome).
  # This keeps the single map, the comparative maps, and the shared legend
  # (all built from `palette`) consistent from one source of truth.
  coding <- dict_vars %>%
    filter(var_readable == selected) %>%
    pull(var_coding) %>%
    head(1)
  if (length(coding) == 1 && !is.na(coding) && coding == "neg") {
    palette <- rev(palette)
  }

  if (!is_comp) {
    legend_title <- paste0(
      dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1]
    )
    
    us_states_with_data <- us_states %>%
      filter(ABBR != "USA") %>%
      select(1:8, estimate = sym(selected)) %>%
      mutate(
        estimate = as.numeric(
          gsub(pattern = "[,]", replacement = "", x = estimate)
        )
      )
    
    buckets <- create_non_overlapping_buckets(us_states_with_data$estimate)
    labels  <- format_ranges(buckets, selected)
    
    us_states_with_data <- us_states_with_data %>%
      mutate(
        estimate_cat = cut(
          estimate,
          breaks        = buckets,
          include.lowest = TRUE,
          labels        = labels
        ),
        .fmt_val = dplyr::case_when(
          is.na(estimate) ~ "No data",
          grepl("pct_|_pct", selected, ignore.case = TRUE) ~
            scales::percent(estimate / 100, accuracy = 0.1),
          TRUE ~
            scales::comma(estimate, accuracy = 1)
        ),
        .tooltip = paste0(NAME, " (", ABBR, ")\n", .fmt_val),
        .data_id = as.character(GEOID),
        # Border ink resolved per state from its own fill; see the note on
        # border_ink_for_fill() above for why this is derived from the colour
        # rather than from the bin index.
        .border_ink = border_ink_for_fill(
          fill_hex_from_bin(estimate_cat, palette, na_color)
        )
      )

    ggplot(data = us_states_with_data) +
      # Drawn first, so the filled layer covers its inner half and only the
      # outside flank survives to hold the coastline.
      map_border_perimeter(us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_cat, color = .border_ink,
            tooltip = .tooltip, data_id = .data_id),
        linewidth = map_border_ink_width
      ) +
      # Draws .border_ink verbatim. Its default guide is "none", so no second
      # legend appears alongside the fill legend.
      scale_color_identity() +
      # na.value was previously unset, so states with no value fell through to
      # ggplot's default "grey50", which sits inside several palettes' ranges.
      # Binding it to the selected palette's no-data colour keeps "no data"
      # distinguishable from every bin, and is what makes High Contrast Mono
      # usable at all.
      scale_fill_manual(values = palette, name = legend_title,
                        na.value = na_color) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  # 2x2 legend
      theme_void(base_family = "EB Garamond") +
      theme(
        text             = element_text(family = "EB Garamond"),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_text(size = 14, family = "EB Garamond"),
        legend.box       = "horizontal",
        legend.text      = element_text(size = 14, family = "EB Garamond"),
        # theme_void() strips legend key borders; without one, pale palette
        # colors (e.g. the ADA-PARC palette's lightest yellow) are nearly
        # invisible against the white legend background. A thin outline
        # keeps every swatch perceptible regardless of palette choice.
        # 0.6 (was 0.4) so the edge survives SVG downscaling.
        legend.key       = element_rect(fill = NA, color = "#1c2b3a", linewidth = 0.6)
      )
    
  } else {
    base_var <- dict_vars %>%
      filter(var_readable == selected) %>%
      pull(var_base)
    
    comp_var <- dict_vars %>%
      filter(var_base == base_var, var_readable != selected) %>%
      pull(var_readable)
    
    us_states_with_data <- us_states %>%
      filter(ABBR != "USA") %>%
      select(1:8, estimate = sym(selected), estimate_2 = sym(comp_var)) %>%
      mutate(
        estimate   = as.numeric(gsub(pattern = "[,]", replacement = "", x = estimate)),
        estimate_2 = as.numeric(gsub(pattern = "[,]", replacement = "", x = estimate_2))
      )
    
    combined_var <- c(us_states_with_data$estimate,
                      us_states_with_data$estimate_2)
    breaks <- create_non_overlapping_buckets(combined_var)
    labels <- format_ranges(breaks, selected)
    
    us_states_with_data <- us_states_with_data %>%
      mutate(
        estimate_cat   = cut(
          estimate,
          breaks         = breaks,
          include.lowest = TRUE,
          labels         = labels
        ),
        estimate_2_cat = cut(
          estimate_2,
          breaks         = breaks,
          include.lowest = TRUE,
          labels         = labels
        ),
        .fmt_val1 = dplyr::case_when(
          is.na(estimate) ~ "No data",
          grepl("pct_|_pct", selected, ignore.case = TRUE) ~
            scales::percent(estimate / 100, accuracy = 0.1),
          TRUE ~ scales::comma(estimate, accuracy = 1)
        ),
        .fmt_val2 = dplyr::case_when(
          is.na(estimate_2) ~ "No data",
          grepl("pct_|_pct", comp_var, ignore.case = TRUE) ~
            scales::percent(estimate_2 / 100, accuracy = 0.1),
          TRUE ~ scales::comma(estimate_2, accuracy = 1)
        ),
        .tooltip1 = paste0(NAME, " (", ABBR, ")\n", .fmt_val1),
        .tooltip2 = paste0(NAME, " (", ABBR, ")\n", .fmt_val2),
        .data_id  = as.character(GEOID),
        # One ink column per map. The two panels bin different columns against
        # the same breaks, so a state can sit in a light bin on one and a dark
        # bin on the other and must take a different border in each.
        .border_ink1 = border_ink_for_fill(
          fill_hex_from_bin(estimate_cat, palette, na_color)
        ),
        .border_ink2 = border_ink_for_fill(
          fill_hex_from_bin(estimate_2_cat, palette, na_color)
        )
      )

    # Use a meaningful legend title (still hidden visually by element_blank())
    legend_title <- paste0(
      dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1]
    )

    shared_scale <- scale_fill_manual(
      values   = palette,
      drop     = FALSE,
      name     = legend_title,
      na.value = na_color   # see the note on the single-map scale above
    )

    # Both maps suppress their own legend; we draw ONE shared legend below
    # as an explicit plot of colored tiles + text.
    #
    # Why not a normal ggplot legend? The two maps map different columns
    # (estimate_cat vs estimate_2_cat), so patchwork's guides = "collect"
    # treats them as distinct guides and renders TWO legends. And letting
    # ggiraph draw geom_sf legend keys is unreliable under ggplot2 4.x /
    # ggiraph 0.9.x: individual pale keys (e.g. the YlOrRd lightest yellow,
    # ~1.06:1 on white) intermittently wash out to the blank key background.
    # Plotting the swatches directly from `palette` + `labels` as geom_tile
    # rects is fully deterministic: exactly one legend, all four colors,
    # each drawn from the same source of truth as the maps.
    map1 <- ggplot(data = us_states_with_data) +
      map_border_perimeter(us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_cat, color = .border_ink1,
            tooltip = .tooltip1, data_id = .data_id),
        linewidth = map_border_ink_width
      ) +
      scale_color_identity() +
      shared_scale +
      theme_void(base_family = "EB Garamond") +
      theme(
        text            = element_text(family = "EB Garamond"),
        legend.position = "none",
        plot.title      = element_text(
          family = "EB Garamond", size = 14, hjust = 0.5
        )
      ) +
      ggtitle("People with Disabilities")

    map2 <- ggplot(data = us_states_with_data) +
      map_border_perimeter(us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_2_cat, color = .border_ink2,
            tooltip = .tooltip2, data_id = .data_id),
        linewidth = map_border_ink_width
      ) +
      scale_color_identity() +
      shared_scale +
      theme_void(base_family = "EB Garamond") +
      theme(
        text            = element_text(family = "EB Garamond"),
        legend.position = "none",
        plot.title      = element_text(
          family = "EB Garamond", size = 14, hjust = 0.5
        )
      ) +
      ggtitle("People without Disabilities")

    # One shared legend, built directly from palette + labels. Evenly
    # spaced slots, each = a colored swatch with its range label to the
    # right. scale_fill_identity() draws the hex colors verbatim so no
    # bin can wash out. Centered under both maps.
    n_lab      <- length(labels)
    slot_gap   <- 1.9
    legend_df  <- data.frame(
      slot = 1 + (seq_len(n_lab) - 1) * slot_gap,
      fill = palette[seq_len(n_lab)],
      lab  = as.character(labels),
      stringsAsFactors = FALSE
    )
    x_span     <- c(min(legend_df$slot) - 0.9, max(legend_df$slot) + 1.4)

    legend_plot <- ggplot(legend_df) +
      geom_tile(
        aes(x = slot, y = 0, fill = fill),
        width = 0.55, height = 0.7,
        color = "#1c2b3a", linewidth = 0.6
      ) +
      geom_text(
        aes(x = slot + 0.4, y = 0, label = lab),
        hjust = 0, vjust = 0.5,
        family = "EB Garamond", size = 5, color = "#1c2b3a"
      ) +
      scale_fill_identity() +
      coord_cartesian(
        xlim = x_span, ylim = c(-0.8, 0.8), clip = "off"
      ) +
      theme_void(base_family = "EB Garamond")

    combined <- (map1 + map2) / legend_plot +
      plot_layout(heights = c(10, 1.3))

    combined
  }
}


# City tract choropleth, static (print/download) ---------------------------
# Companion to render_national_map(), for the "Download Selected City"
# report. output$snapshotmap (the on-screen map) is Leaflet, which has no
# server-side static export, so this draws the same tract polygons as a
# plain ggplot choropleth for svglite export.
#
# Deliberately reads a pre-computed `fill_col` column rather than re-deriving
# bins/colors here. `fill_col` is built once, in the city_map_data() reactive
# in ADA_PARC.Rmd, from the same city_map_breaks / city_map_labels / palette
# the Leaflet map uses, and both the interactive map and this static one
# consume it. That is what guarantees the printed map and the on-screen map
# are always in the same bins with the same colors, never a second
# computation that could drift from the first.
#
# No basemap tiles. The interactive map uses CartoDB Positron tiles for
# geographic orientation, but pulling raster tiles into a server-side render
# means a network call at render time and a dependency on a third-party tile
# service being reachable from wherever the Shiny process runs. Tract
# boundaries alone are enough context for a printed reference map, and this
# keeps the render reproducible offline.
render_city_tract_map <- function(map_data, border_color = "#1c2b3a",
                                  border_width = 0.3) {
  ggplot2::ggplot(data = map_data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = fill_col),
      color = border_color, linewidth = border_width
    ) +
    # fill_col already holds resolved hex values (see the note above), so the
    # scale draws them verbatim rather than mapping a factor to a palette a
    # second time.
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void(base_family = "EB Garamond") +
    ggplot2::theme(
      text            = ggplot2::element_text(family = "EB Garamond"),
      legend.position = "none",
      plot.margin     = ggplot2::margin(4, 4, 4, 4)
    )
}

#' Plain-language summary of a city's tract map, for alt text.
#'
#' Mirrors the role of altText() for the national map, but city tract counts
#' run from 1 to several hundred, so naming individual tracts the way
#' altText() names individual states would not be meaningful. This reports
#' the range and, when relevant, how many tracts actually have data.
cityMapAltText <- function(map_data, city_name,
                           value_col = "pct_cni_total_dis_place") {
  vals    <- suppressWarnings(as.numeric(map_data[[value_col]]))
  n_total <- length(vals)
  n_have  <- sum(!is.na(vals))

  if (n_have == 0) {
    return(paste0(
      "Tract-level disability rate data is not available for ", city_name, "."
    ))
  }

  fmt <- scales::label_percent(accuracy = 0.1, scale = 1)

  coverage_note <- if (n_have < n_total) {
    paste0(" Data was available for ", n_have, " of ", n_total, " census tracts.")
  } else {
    ""
  }

  paste0(
    "This map shows the percent of residents with disabilities in each ",
    "census tract in ", city_name, ". Tract rates ranged from ",
    fmt(min(vals, na.rm = TRUE)), " to ", fmt(max(vals, na.rm = TRUE)), ".",
    coverage_note
  )
}


# Accessibility Functions -------------------------------------------------


englishLangList <- function(x) {
  if(length(x) > 2){
    next_to_last <- length(x) - 1
    paste0(paste(x[1:next_to_last], collapse = ", "), ", and ", x[length(x)], collapse = "")
  } else {
    paste(x, collapse = " and ")
  }
}

between <- function(df, variable, probs) {
  df %>% 
    filter(!!sym(variable) >= probs[1] & !!sym(variable) <= probs[2]) %>%
    pull(NAME)
}


altText <- function(data, variable) {
  
  # helper: decide whether this variable should be treated as a percent
  is_pct_var <- function(var) grepl("(^pct_|_pct$)", var)
  
  # helper: format a single numeric value with commas + 1 decimal (trim .0),
  # or as percent (0–100 scale, trim .0)
  format_value <- function(x, var) {
    if (is_pct_var(var)) {
      scales::label_percent(accuracy = 0.1, scale = 1, trim = TRUE)(x)
    } else {
      scales::label_number(accuracy = 0.1, big.mark = ",", trim = TRUE)(x)
    }
  }
  
  df <- data %>%
    dplyr::select(NAME, ABBR, !!rlang::sym(variable)) %>%
    dplyr::filter(ABBR != "USA") %>%
    dplyr::mutate(State = paste0(NAME, " (", ABBR, ")")) %>%
    dplyr::filter(!is.na(!!rlang::sym(variable)))
  
  # Min row
  min_val <- min(df[[variable]], na.rm = TRUE)
  min_state <- df %>%
    dplyr::filter(.data[[variable]] == min_val) %>%
    dplyr::slice(1) %>%
    dplyr::pull(State)
  
  text_min <- paste0(
    " The lowest state or territory was ",
    min_state,
    " at ",
    format_value(min_val, variable),
    "."
  )
  
  # Max row
  max_val <- max(df[[variable]], na.rm = TRUE)
  max_state <- df %>%
    dplyr::filter(.data[[variable]] == max_val) %>%
    dplyr::slice(1) %>%
    dplyr::pull(State)
  
  text_max <- paste0(
    " The highest state or territory was ",
    max_state,
    " at ",
    format_value(max_val, variable),
    "."
  )
  
  paste0(text_min, text_max)
  
  # Max static check
  # max_text_static <- demographics %>%
  #   mutate("State" = paste0(NAME, " (", ABBR, ")")) %>%
  #   select(State, sym("pop_total")) %>%
  #   filter(!!sym("pop_total") == max(!!sym("pop_total"))) %>%
  #   mutate(across(-State & -ends_with("_pct"),
  #                 ~scales::comma(.x))) %>%
  #   mutate(across(ends_with("_pct"),
  #                 ~scales::percent(.x,
  #                                  accuracy = 0.1,
  #                                  scale = 1))) %>%
  #   mutate("summary_text" = paste0(" The highest state was ",
  #                                  State, " at ",
  #                                  !!sym("pop_total"), ".")) %>%
  #   pull(summary_text)
  
  # Title, vars_pretty field for variable
  title <- dict_vars %>%
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>%
    head(1) %>%
    pull(national_dropdown_label)

  # Summary text for variable
  summary_text <- dict_vars %>%
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>%
    head(1) %>%
    pull(national_summary_text)

  # Data source label for variable
  source_label <- dict_vars %>%
    filter(var_readable == variable) %>%
    head(1) %>%
    pull(data_source_label)

  source_suffix <- if (length(source_label) > 0 && !is.na(source_label) && nzchar(trimws(source_label))) {
    paste0(" (Source: ", trimws(source_label), ")")
  } else {
    ""
  }

  # Text for summary
  paste0(
    # "<b>", title, "</b><br>",
    if (is.na(summary_text)) {
      ""
    } else {
    paste0(summary_text, " ")
      }
    ,
    # Min/Max
    text_min, text_max,
    # Source attribution
    source_suffix
  )

}
