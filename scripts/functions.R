
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
    formatted_breaks <- formatC(breaks, format = "f", digits = 2)
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

render_national_map <- function(selected, palette_selected = "YlOrRd") {
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
        .data_id = as.character(GEOID)
      )

    ggplot(data = us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_cat, tooltip = .tooltip, data_id = .data_id)
      ) +
      scale_fill_manual(values = palette, name = legend_title) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  # 2x2 legend
      theme_void(base_family = "EB Garamond") +
      theme(
        text             = element_text(family = "EB Garamond"),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = element_text(size = 14, family = "EB Garamond"),
        legend.box       = "horizontal",
        legend.text      = element_text(size = 14, family = "EB Garamond")
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
        .data_id  = as.character(GEOID)
      )

    # Use a meaningful legend title (still hidden visually by element_blank())
    legend_title <- paste0(
      dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1]
    )

    shared_scale <- scale_fill_manual(
      values = palette,
      drop   = FALSE,
      name   = legend_title
    )

    map1 <- ggplot(data = us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_cat, tooltip = .tooltip1, data_id = .data_id)
      ) +
      shared_scale +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  # 2x2 legend
      theme_void(base_family = "EB Garamond") +
      theme(
        text            = element_text(family = "EB Garamond"),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.text     = element_text(size = 13, family = "EB Garamond"),
        legend.box      = "horizontal",
        plot.title      = element_text(family = "EB Garamond")
      ) +
      ggtitle("People with Disabilities")

    map2 <- ggplot(data = us_states_with_data) +
      ggiraph::geom_sf_interactive(
        aes(fill = estimate_2_cat, tooltip = .tooltip2, data_id = .data_id)
      ) +
      shared_scale +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  # 2x2 legend
      theme_void(base_family = "EB Garamond") +
      theme(
        text            = element_text(family = "EB Garamond"),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.text     = element_text(size = 13, family = "EB Garamond"),
        legend.box      = "horizontal",
        plot.title      = element_text(family = "EB Garamond")
      ) +
      ggtitle("People without Disabilities")

    legend <- cowplot::get_legend(
      map1 +
        theme(
          text             = element_text(family = "EB Garamond"),
          legend.position  = "bottom",
          legend.direction = "horizontal",
          legend.title     = element_blank(),
          legend.text      = element_text(size = 20, family = "EB Garamond"),
          legend.box       = "horizontal"
        )
    )
    
    combined <- (map1 + map2) / legend + plot_layout(heights = c(10, 3))
    combined
  }
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
    text_min, text_max
  )
  
}
