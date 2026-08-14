plot_longterm_arthropod_trends <- function(
    data,
    dateRange,
    minSurveyRecords,
    minNumWeeks,
    minNumYears       = 3,
    arthropodGroup    = "caterpillar",
    plotVar           = "fracSurveys",
    sitesToPlot       = NULL,
    sitesToExclude    = NULL,
    sortByLatitude    = TRUE,
    commonY           = TRUE,
    maxNameStrLen     = 23,
    showTrendLine     = TRUE,   
    showTrendEnvelope = TRUE    
) {
  
  ## ── Package checks ──────────────────────────────────────────────────────────
  for (pkg in c("dplyr", "ggplot2", "patchwork", "scales")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", pkg))
  }
  suppressPackageStartupMessages({
    library(dplyr); library(ggplot2); library(patchwork)
  })
  
  ## ── Input validation ────────────────────────────────────────────────────────
  if (length(dateRange) != 2 || !is.numeric(dateRange))
    stop("`dateRange` must be a numeric vector of length 2: c(minJulianDay, maxJulianDay).")
  if (dateRange[1] > dateRange[2])
    stop("`dateRange[1]` must be <= `dateRange[2]`.")
  
  valid_plotVars <- c("fracSurveys", "meanDensity", "meanBiomass")
  if (!plotVar %in% valid_plotVars)
    stop(sprintf("`plotVar` must be one of: %s.",
                 paste(valid_plotVars, collapse = ", ")))
  
  if (!is.numeric(maxNameStrLen) || maxNameStrLen < 1)
    stop("`maxNameStrLen` must be a positive integer.")
  maxNameStrLen <- as.integer(maxNameStrLen)
  
  if (!is.logical(showTrendLine)     || length(showTrendLine)     != 1)
    stop("`showTrendLine` must be a single logical value.")
  if (!is.logical(showTrendEnvelope) || length(showTrendEnvelope) != 1)
    stop("`showTrendEnvelope` must be a single logical value.")
  
  ## ── Derived display strings ─────────────────────────────────────────────────
  group_label <- paste0(toupper(substring(arthropodGroup, 1, 1)),
                        substring(arthropodGroup, 2))
  
  yaxis_label <- switch(plotVar,
                        fracSurveys = paste0(group_label, "\noccurrence"),
                        meanDensity = paste0("Mean ", arthropodGroup, "\nper survey"),
                        meanBiomass = "Mean biomass (mg)\nper survey"
  )
  
  y_formatter <- switch(plotVar,
                        fracSurveys = scales::percent_format(accuracy = 1),
                        meanDensity = scales::number_format(accuracy = 0.01),
                        meanBiomass = scales::number_format(accuracy = 0.01)
  )
  
  response_word <- switch(plotVar,
                          fracSurveys = "occurrence",
                          meanDensity = "density",
                          meanBiomass = "biomass"
  )
  
  ## ── Helper: panel title with name truncation ─────────────────────────────────
  make_title <- function(name, region) {
    display <- if (nchar(name) > maxNameStrLen)
      paste0(substr(name, 1L, maxNameStrLen), "\u2026")
    else
      name
    paste0(display, " (", region, ")")
  }
  
  ## ── Helper: optimal panel layout ────────────────────────────────────────────
  find_layout <- function(n) {
    if (n == 1L) return(c(1L, 1L))
    best_rows  <- 1L
    best_cols  <- n
    best_score <- Inf
    for (rows in seq_len(ceiling(sqrt(n)) + 2L)) {
      for (extra in 0:2) {
        cols <- rows + extra
        if (rows * cols >= n) {
          score <- (rows * cols - n) * 100L + extra
          if (score < best_score) {
            best_score <- score
            best_rows  <- rows
            best_cols  <- cols
          }
          break
        }
      }
    }
    c(best_rows, best_cols)
  }
  
  ## ── Step 1: Filter observations to the annual date window ───────────────────
  dat <- data %>%
    filter(julianday >= dateRange[1], julianday <= dateRange[2])
  
  if (nrow(dat) == 0)
    stop("No observations fall within the specified `dateRange`.")
  
  ## ── Step 2: Collapse to individual plant-survey level ───────────────────────
  survey_dat <- dat %>%
    group_by(Name, Region, Latitude, Longitude,
             Year, julianweek, LocalDate, PlantFK, Circle, Orientation) %>%
    summarise(
      groupQuantity = sum(Quantity[Group == arthropodGroup & !is.na(Group)],
                          na.rm = TRUE),
      groupBiomass  = sum(Biomass_mg[Group == arthropodGroup & !is.na(Group)],
                          na.rm = TRUE),
      hasGroup      = groupQuantity > 0,
      .groups       = "drop"
    )
  
  ## ── Step 3: Aggregate to site × year ────────────────────────────────────────
  site_yr <- survey_dat %>%
    group_by(Name, Region, Latitude, Longitude, Year) %>%
    summarise(
      nSurveys    = n(),
      nWeeks      = n_distinct(julianweek),
      fracSurveys = sum(hasGroup)      / n(),
      meanDensity = sum(groupQuantity) / n(),
      meanBiomass = sum(groupBiomass)  / n(),
      .groups     = "drop"
    ) %>%
    mutate(plotValue = .data[[plotVar]])
  
  ## ── Step 4: Apply per-site-year effort filters ───────────────────────────────
  site_yr <- site_yr %>%
    filter(nSurveys >= minSurveyRecords, nWeeks >= minNumWeeks)
  
  ## ── Step 5: Keep only sites with enough qualifying years ────────────────────
  good_sites <- site_yr %>%
    count(Name, name = "nYears") %>%
    filter(nYears >= minNumYears) %>%
    pull(Name)
  
  site_yr <- filter(site_yr, Name %in% good_sites)
  
  ## ── Step 6: Apply sitesToExclude ─────────────────────────────────────────────
  if (!is.null(sitesToExclude))
    site_yr <- filter(site_yr, !Name %in% sitesToExclude)
  
  ## ── Step 7: Optional user-supplied site inclusion filter ────────────────────
  if (!is.null(sitesToPlot)) {
    site_yr  <- filter(site_yr, Name %in% sitesToPlot)
    excluded <- setdiff(sitesToPlot, unique(site_yr$Name))
    if (length(excluded))
      message("Requested site(s) not in qualifying set (skipped): ",
              paste(excluded, collapse = ", "))
  }
  
  if (nrow(site_yr) == 0) {
    message("No sites meet the specified criteria. Returning NULL.")
    return(invisible(NULL))
  }
  
  ## ── Step 8: Build ordered site list ─────────────────────────────────────────
  site_meta <- site_yr %>%
    distinct(Name, Region, Latitude) %>%
    { if (sortByLatitude) arrange(., desc(Latitude)) else arrange(., Name) }
  
  n_sites <- nrow(site_meta)
  
  ## ── Step 9: Determine panel layout ──────────────────────────────────────────
  lyt    <- find_layout(n_sites)
  n_rows <- lyt[1]; n_cols <- lyt[2]
  message(sprintf(
    "Arranging %d site(s) in a %d-row \u00d7 %d-col grid (%d empty panel(s)).",
    n_sites, n_rows, n_cols, n_rows * n_cols - n_sites
  ))
  
  ## ── Step 10: Y-axis ceiling ──────────────────────────────────────────────────
  y_floor <- switch(plotVar,
                    fracSurveys = 0.02,
                    meanDensity = 0.01,
                    meanBiomass = 0.01
  )
  
  global_y_upper <- max(max(site_yr$plotValue, na.rm = TRUE) * 1.08, y_floor)
  
  ## ── Step 11: X-axis label strategy ──────────────────────────────────────────
  stagger <- n_cols >= 5
  
  year_labeler <- if (stagger) {
    function(x) {
      ifelse(
        is.na(x), "",
        ifelse(seq_along(x) %% 2 == 0,
               paste0("\n", as.integer(x)),
               as.character(as.integer(x)))
      )
    }
  } else {
    function(x) ifelse(is.na(x), "", as.character(as.integer(x)))
  }
  
  ## ── Step 12: Pre-build smooth layer list ─────────────────────────────────────
  # geom_smooth draws both the line and ribbon in a single call, so to control
  # them independently we decompose it:
  #   – The ribbon is drawn with stat_smooth(geom = "ribbon", color = NA).
  #   – The line  is drawn with stat_smooth(geom = "line",   se    = FALSE).
  # Both use the same method/formula so they share an identical fit. Building
  # the list once here (outside the per-site loop) is efficient because ggplot2
  # layers carry no site-specific state — only the data passed to ggplot() varies.
  #
  # Four combinations:
  #   line + envelope  → ribbon layer then line layer (ribbon drawn first so the
  #                       line sits on top of it, matching geom_smooth's default)
  #   line only        → line layer alone
  #   envelope only    → ribbon layer alone
  #   neither          → empty list (no smooth added at all)
  
  smooth_layers <- list()
  
  if (showTrendEnvelope) {
    smooth_layers <- c(smooth_layers, list(
      stat_smooth(
        geom    = "ribbon",
        method  = "lm",
        formula = y ~ x,
        fill    = "firebrick3",
        alpha   = 0.12,
        color   = NA       # suppress the ribbon's own border line
      )
    ))
  }
  
  if (showTrendLine) {
    smooth_layers <- c(smooth_layers, list(
      stat_smooth(
        geom      = "line",
        method    = "lm",
        formula   = y ~ x,
        color     = "firebrick3",
        linewidth = 0.75,
        se        = FALSE
      )
    ))
  }
  
  ## ── Step 13: Build one ggplot per site ───────────────────────────────────────
  plot_list <- vector("list", n_sites)
  
  for (i in seq_len(n_sites)) {
    
    sname       <- site_meta$Name[i]
    sreg        <- site_meta$Region[i]
    panel_title <- make_title(sname, sreg)
    
    pdata <- filter(site_yr, Name == sname) %>% arrange(Year)
    
    panel_row <- ceiling(i / n_cols)
    panel_col <- ((i - 1L) %% n_cols) + 1L
    
    panel_y_upper <- if (commonY)
      global_y_upper
    else
      max(max(pdata$plotValue, na.rm = TRUE) * 1.08, y_floor)
    
    p <- ggplot(pdata, aes(x = Year, y = plotValue)) +
      geom_line(color = "grey65", linewidth = 0.45) +
      geom_point(aes(size = nSurveys), color = "steelblue3", alpha = 0.9) +
      smooth_layers +                          # ← empty list adds nothing
      scale_y_continuous(
        labels = y_formatter,
        expand = expansion(mult = c(0, 0))
      ) +
      coord_cartesian(ylim = c(0, panel_y_upper)) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 4),
        labels = year_labeler
      ) +
      scale_size_continuous(range = c(1.5, 4), guide = "none") +
      labs(
        title = panel_title,
        x     = if (panel_row == n_rows) "Year"      else NULL,
        y     = if (panel_col == 1L)     yaxis_label else NULL
      ) +
      theme_bw(base_size = 9) +
      theme(
        plot.title       = element_text(size = 7.5, face = "bold",
                                        hjust = 0.5, margin = margin(b = 3)),
        axis.title       = element_text(size = 8),
        axis.text        = element_text(size = 7),
        panel.grid.minor = element_blank(),
        plot.margin      = margin(5, 7, 3, 7)
      )
    
    plot_list[[i]] <- p
  }
  
  ## ── Step 14: Assemble multipanel figure ─────────────────────────────────────
  # Build the caption's trend description dynamically so it always accurately
  # reflects what is actually shown.
  trend_note <- dplyr::case_when(
    showTrendLine & showTrendEnvelope  ~ "OLS trend line + 95% CI ribbon.",
    showTrendLine & !showTrendEnvelope ~ "OLS trend line only (no CI ribbon).",
    !showTrendLine & showTrendEnvelope ~ "95% CI ribbon only (no trend line).",
    TRUE                               ~ "No trend overlay."
  )
  
  wrap_plots(plot_list, nrow = n_rows, ncol = n_cols) +
    plot_annotation(
      title   = paste0("Long-term trends in ", group_label, " ", response_word),
      caption = paste0(
        "Julian days ", dateRange[1], "\u2013", dateRange[2],
        "  \u2022  Variable: ", plotVar,
        "  \u2022  Min. surveys / site-year: ", minSurveyRecords,
        "  \u2022  Min. survey weeks: ", minNumWeeks,
        "  \u2022  Min. qualifying years: ", minNumYears,
        if (!commonY) "  \u2022  Free y-axes." else "",
        "\nPoint area \u221d number of surveys.  ", trend_note
      ),
      theme = theme(
        plot.title   = element_text(hjust = 0.5, size = 13, face = "bold",
                                    margin = margin(b = 6)),
        plot.caption = element_text(hjust = 0.5, size = 7.5, color = "grey45")
      )
    )
}