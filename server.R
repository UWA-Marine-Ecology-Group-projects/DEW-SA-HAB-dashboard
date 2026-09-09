# --------------------------- shared helpers ----------------------------------
base_map <- function(max_zoom = 20, current_zoom = 6) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, max_zoom)) |>
    setView(lng = 137.618521, lat = -34.25, current_zoom) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    # Use regular polygons for static layers:
    addPolygons(
      data = state_mp, 
      color = "black", weight = 1,
      fillColor = ~state.pal(zone), fillOpacity = 0.8,
      group = "State Marine Parks",
      popup = ~name,
      options = pathOptions(pane = "polys")
    ) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      options = pathOptions(pane = "polys"), group = "Australian Marine Parks"
    ) %>%
    
    # Legends
    addLegend(
      pal = state.pal,
      values = state_mp$zone,
      opacity = 1,
      title = "State Zones",
      position = "bottomleft",
      group = "State Marine Parks"
    ) |>
    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Australian Marine Park Zones",
      position = "bottomleft",
      group = "Australian Marine Parks"
    ) 
}

# viridis colours for depth using full domain for consistent legend
depth_cols_and_pal <- function(values_numeric) {
  list(
    cols = colourvalues::colour_values_rgb(-values_numeric, palette = "viridis", include_alpha = FALSE) / 255,
    pal  = colorNumeric(palette = rev(viridisLite::viridis(256)), domain = values_numeric)
  )
}

# shared updater for "Sampling locations" group with numeric legend
update_points_with_numeric_legend <- function(map_id, data, fill_cols, legend_pal, legend_values,
                                              legend_title = "Depth (m)") {
  leafletProxy(map_id, data = data) |>
    clearGroup("Sampling locations") |>
    leafgl::addGlPoints(
      data = data,
      fillColor = fill_cols,
      weight = 1,
      popup = data$popup,
      group = "Sampling locations",
      pane = "points"
    ) |>
    clearControls() |>
    addLegend(
      "topright",
      pal = legend_pal,
      values = legend_values,
      title = legend_title,
      opacity = 1,
      group = "Sampling locations"
    )
}

add_bubble_legend <- function(map, max_val, title, layerId = "bubbleLegendSpecies", methodcol = "#f89f00") {
  leaflet::removeControl(map, layerId) %>%
    add_legend(
      colors = c("white", methodcol, methodcol),
      labels = c(0, round(max_val / 2), max_val),
      sizes  = c(5, 20, 40),
      title  = title,
      group  = "Sampling locations",
      layerId = layerId
    )
}

filter_by_park <- function(df, park, park_col = "location") {
  if (is.null(park)) return(df)                    # statewide
  if (!park_col %in% names(df)) return(df)         # fallback if missing
  dplyr::filter(df, .data[[park_col]] %in% park)
}

twoValueBoxServer <- function(id,
                              left_reactive,
                              right_reactive,
                              format_fn = scales::label_comma()) {
  
  moduleServer(id, function(input, output, session) {
    
    left_val  <- reactive(left_reactive())
    right_val <- reactive(right_reactive())
    
    output$left_val <- renderUI({
      x <- left_val()
      
      html <- if (length(x) == 0 || is.null(x) || all(is.na(x))) {
        "<span style='color: rgba(194,194,194,0.6); 
                      font-size: 0.85rem; 
                      font-style: italic;'>
           Surveys incomplete
         </span>"
      } else {
        format_fn(x)
      }
      
      HTML(html)
    })
    
    output$right_val <- renderUI({
      x <- right_val()
      
      html <- if (length(x) == 0 || is.null(x) || all(is.na(x))) {
        "<span style='color: rgba(194,194,194,0.6); 
                      font-size: 0.85rem; 
                      font-style: italic;'>
           Surveys incomplete
         </span>"
      } else {
        format_fn(x)
      }
      
      HTML(html)
    })
  })
}

no_data_plot <- function(title = NULL) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Data not available",
             size = 5, fontface = "italic", colour = "black") +
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

# ---- RLS (Dive) % change table + impact gauge helpers ----------------------
# Mirrors BRUV's fmt_change()/get_metric_plot()/make_impact_gauges() (defined
# inline in the BRUV output$*_change_table renderers, and above/below for the
# gauges) but reused across all of RLS's region/location tables and gauges
# instead of being copied per-output. Colour thresholds match BRUV's exactly
# (<=-50% red, <=-20% amber, else blue) - no RLS-specific recalibration asked
# for.
rls_fmt_pct_change_html <- function(vals) {
  vals_chr <- as.character(vals)

  # "Undefined - pre-bloom zero" (RLS-only case, see script 10) is treated
  # the same as "Surveys incomplete" - a percentage change isn't meaningful
  # off a zero baseline.
  is_incomplete <- grepl("Surveys incomplete", vals_chr, ignore.case = TRUE) |
    grepl("Undefined", vals_chr, ignore.case = TRUE) |
    is.na(vals_chr)

  num <- suppressWarnings(as.numeric(vals_chr))
  has_num <- !is.na(num) & !is_incomplete

  arrows  <- ifelse(num < 0, "&#8595;", "&#8593;")
  colours <- ifelse(num <= -50, "#EB5757",
                    ifelse(num <= -20, "#D4A017", "#3B7EA1"))

  out <- rep("", length(vals_chr))
  out[has_num] <- sprintf(
    "<span style='color:%s; font-weight:700;'>%s %s%%</span>",
    colours[has_num],
    arrows[has_num],
    scales::number(abs(num[has_num]), accuracy = 1)
  )
  out[is_incomplete] <- "<em>Surveys incomplete</em>"
  out
}

# Looks up one metric's `impact` category (Low/Medium/High/Surveys
# incomplete/Undefined - pre-bloom zero) from one of rls_data's "long"
# pct-change tables, for the overall (not by-status) Pre-bloom -> Bloom
# comparison - the same comparison BRUV's own impact gauges use.
get_rls_metric_impact <- function(pct_change_long_tbl, spatial_col, spatial_value, this_metric_id) {
  # Guard against a missing/empty CSV (e.g. script 10 hasn't been run, or
  # rls_data predates this feature) - an empty tibble from read_pct_change()
  # has none of these columns, so filtering on them would error rather than
  # just showing "no data" gauges.
  required_cols <- c(spatial_col, "metric_id", "comparison_type", "status", "impact")
  if (nrow(pct_change_long_tbl) == 0 || !all(required_cols %in% names(pct_change_long_tbl))) {
    return(character(0))
  }

  pct_change_long_tbl %>%
    dplyr::filter(
      .data[[spatial_col]] == spatial_value,
      metric_id == this_metric_id,
      comparison_type == "period",
      status == "Overall"
    ) %>%
    dplyr::pull(impact)
}

# One gauge (half-donut-with-dial) for one RLS metric, or a "no data"
# placeholder - mirrors get_metric_plot()/get_metric_plot_location() below,
# reusing the same half_donut_with_dial() helper BRUV's gauges use (it only
# needs a Low/Medium/High status, which script 10 already computes for RLS
# with the same thresholds BRUV uses).
get_metric_plot_rls <- function(pct_change_long_tbl, spatial_col, spatial_value,
                                this_metric_id, title_lab, wrap_width = 16) {
  txt <- get_rls_metric_impact(pct_change_long_tbl, spatial_col, spatial_value, this_metric_id)

  if (length(txt) == 0 || is.na(txt) || txt %in% c("Surveys incomplete", "Undefined - pre-bloom zero")) {
    return(no_data_plot(stringr::str_wrap(title_lab, wrap_width)))
  }

  half_donut_with_dial(values = c(1, 1, 1), mode = "absolute", status = txt) +
    labs(title = stringr::str_wrap(title_lab, width = wrap_width)) +
    theme(
      plot.title  = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Full gauge grid for Dive: one row per biological metric (metric_group,
# same 5 groups/order as the "Explore indicators" tabset), one gauge per
# method/phylum (facet_label) within that row - up to 3 columns. Each gauge
# is titled "<metric_group_label>: <facet_label>" so it's unambiguous
# regardless of position, since (unlike the GLMM plots) patchwork gauges
# can't share one facet strip label per row.
make_impact_gauges_rls <- function(pct_change_long_tbl, spatial_col, spatial_value) {
  rows <- lapply(seq_len(nrow(rls_data$metric_groups)), function(i) {
    this_group       <- rls_data$metric_groups$metric_group[i]
    this_group_label <- rls_data$metric_groups$metric_group_label[i]

    group_rows <- rls_data$metric_lookup %>%
      dplyr::filter(metric_group == this_group)

    gauges <- lapply(seq_len(nrow(group_rows)), function(j) {
      get_metric_plot_rls(
        pct_change_long_tbl, spatial_col, spatial_value,
        this_metric_id = group_rows$metric_id[j],
        title_lab = paste0(this_group_label, ": ", group_rows$facet_label[j])
      )
    })

    Reduce(`|`, gauges)
  })

  Reduce(`/`, rows)
}

get_metric_plot <- function(metric_id, title_lab, wrap_width = 22, chosen_region) {
  
  txt <- hab_data$impact_data |>
    dplyr::filter(region == chosen_region, impact_metric == metric_id) |>
    dplyr::pull(impact)
  
  # ---- If no data, return the “no data” plot ----
  if (txt == "Surveys incomplete") {
    return(no_data_plot(stringr::str_wrap(title_lab, wrap_width)))
  }
  
  # ---- Otherwise return the gauge ----
  half_donut_with_dial(
    values = c(1, 1, 1),
    mode   = "absolute",
    status = txt
  ) +
    labs(title = stringr::str_wrap(title_lab, width = wrap_width)) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.margin = margin(2, 2, 2, 2)
    )
}


safe_pull <- function(expr) {
  reactive({
    val <- expr()
    if (length(val) == 0 || is.null(val) || all(is.na(val))) {
      NA_real_   # <- return numeric NA, not a string
    } else {
      val
    }
  })
}

make_overall_impact_gauge <- function(region_name) {
  
  message(region_name)
  
  p1 <- get_metric_plot("shannon_diversity",        "Shannon diversity index",          chosen_region = region_name)
  p1
}

make_impact_gauges <- function(region_name) {
  
  # ---- Overall impact ----
  overall_status <- hab_data$overall_impact |>
    dplyr::filter(region == region_name) |>
    dplyr::pull(overall_impact)
  
  p0 <- if (identical(overall_status, "Surveys incomplete") ||
            length(overall_status) == 0 ||
            is.na(overall_status)) {
    
    no_data_plot("Overall impact")
    
  } else {
    half_donut_with_dial(
      values = c(1, 1, 1),
      mode   = "absolute",
      status = overall_status
    ) +
      ggtitle("Overall impact") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold.italic", size = 16),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  # ---- Individual indicator plots ----
  p1 <- get_metric_plot("species_richness",         "Species richness",                 chosen_region = region_name)
  p2 <- get_metric_plot("total_abundance",          "Total abundance",                  chosen_region = region_name)
  p3 <- get_metric_plot("shark_ray_richness",       "Shark and ray richness",           chosen_region = region_name)
  p4 <- get_metric_plot("reef_associated_richness", "Reef associated species richness", chosen_region = region_name)
  p5 <- get_metric_plot("fish_200_abundance",       "Fish > 200 mm abundance",          chosen_region = region_name)
  p6 <- get_metric_plot("thamnaconus_degeni",       "Bluefin leatherjacket displacement*",          chosen_region = region_name)
  # p7 <- get_metric_plot("shannon_diversity",        "Shannon diversity index",          chosen_region = region_name)
  
  # Final layout
  (p1 | p2 | p3) /
    (p4 | p5 | p6 #p7
    )
}

# ---- output id helpers -------------------------------------------------------
metric_plot_id <- function(prefix, metric_id, which) {
  paste0(prefix, "_plot_", metric_id, "_", which)
}

metric_plotOutput <- function(prefix, metric_id, which, height = 600, spinner_type = 6) {
  withSpinner(
    plotOutput(metric_plot_id(prefix, metric_id, which), height = height, width = "100%"),
    color = getOption("spinner.color", default = "#0D576E"),
    type = spinner_type
  )
}

metric_plot_type_input_id <- function(prefix, metric_id) {
  paste0(prefix, "_", metric_id, "_plot_type")
}

metric_tab_body_ui <- function(metric_id, prefix = "em") {
  
  data_id <- metric_data_key(metric_id)
  plot_type_id <- metric_plot_type_input_id(prefix, data_id)
  
  tagList(
    bslib::layout_columns(
      col_widths = c(12),
      bslib::input_switch(
        id = plot_type_id,
        label = "Show boxplots (instead of bars)",
        value = FALSE  # FALSE = default bars
      )
    ),
    
    # Your existing layout(s)
    switch(
      metric_id,
      
      combined = {
        tagList(
          h4("Combined plot"),
          layout_columns(
            col_widths = c(12),
            # metric_plotOutput(prefix, "trophic", "main"),
            combined_plot_with_downloads(prefix, data_id, "main"),
            combined_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      richness = {
        tagList(
          h4("Species richness"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
            
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      total_abundance = {
        tagList(
          h4("Total abundance"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      shark_ray_richness = {
        tagList(
          h4("Shark and ray richness"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      reef_associated_richness = {
        tagList(
          h4("Reef associated species richness"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      shannon_diversity = {
        tagList(
          h4("Shannon diversity index"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      fish_200_abundance = {
        tagList(
          h4("Fish greater than 200mm abundance"),
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      trophic = {
        tagList(
          h4("Abundance by trophic level"),
          layout_columns(
            col_widths = c(6, 6),
            # metric_plotOutput(prefix, "trophic", "main"),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          )
        )
      },
      
      
      
      # default: 2 plots
      {
        div(
          layout_columns(
            col_widths = c(6, 6),
            metric_plot_with_downloads(prefix, data_id, "main"),
            metric_plot_with_downloads(prefix, data_id, "status")
          ),
          layout_columns(
            col_widths = c(12),
            metric_plot_with_downloads(prefix, data_id, "year")
          ))
      }
    )
  )
}


# RLS-specific tab body for the Dive "Explore indicators" tabset. Unlike
# metric_tab_body_ui() (whose switch() dispatches on BRUV's own metric
# ids), every RLS tab now shows the same three plots - period, period x
# status, temporal - stacked full width, one tab per biological metric
# (metric_group), each plot internally faceted by method/phylum
# (facet_label) - see rls_metric_lookup / rls_metric_groups in script 15.
# Full width (not the side-by-side main+status BRUV uses) because each
# plot can now have up to 3 facet panels and would be cramped at half
# width.
rls_metric_group_tab_body_ui <- function(metric_group, prefix = "rls_loc") {
  tagList(
    bslib::layout_columns(
      col_widths = c(12),
      bslib::input_switch(
        id = metric_plot_type_input_id(prefix, metric_group),
        label = "Show boxplots (instead of bars)",
        value = FALSE  # FALSE = default bars
      )
    ),
    layout_columns(
      col_widths = c(12),
      metric_plot_with_downloads(prefix, metric_group, "main")
    ),
    layout_columns(
      col_widths = c(12),
      metric_plot_with_downloads(prefix, metric_group, "status")
    ),
    layout_columns(
      col_widths = c(12),
      metric_plot_with_downloads(prefix, metric_group, "year")
    )
  )
}

metric_plot_with_downloads <- function(prefix, data_id, plot_id) {

  tagList(
    metric_plotOutput(prefix, data_id, plot_id),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      downloadButton(
        outputId = paste(prefix, "download", data_id, plot_id, "results", sep = "_"),
        label = "Download plot results"
      ),
      
      downloadButton(
        outputId = paste(prefix, "download", data_id, plot_id, "raw", sep = "_"),
        label = "Download raw data"
      ),
      
      downloadButton(
        outputId = paste(prefix, "download", data_id, plot_id, "plot", sep = "_"),
        label = "Download plot"
      )
    )
  )
}

combined_plot_with_downloads <- function(prefix, data_id, plot_id) {
  
  tagList(
    metric_plotOutput(prefix, data_id, plot_id, height = 800),
    
    layout_columns(
      col_widths = c(12),
      
      downloadButton(
        outputId = paste(prefix, "download", data_id, plot_id, "plot", sep = "_"),
        label = "Download plot"
      )
    )
  )
}


plot_cell <- function(id, width = "120px", height = "120px") {
  div(
    style = sprintf("width:%s; height:%s;", width, height),
    plotOutput(id, width = width, height = height)
  )
}

# add_metric_downloads <- function(output, prefix, data_id, plot_id,
#                                  results_reactive, raw_reactive, plot_reactive,
#                                  input) {
#   
#   results_id <- paste(prefix, "download", data_id, plot_id, "results", sep = "_")
#   raw_id     <- paste(prefix, "download", data_id, plot_id, "raw", sep = "_")
#   plot_id_full <- paste(prefix, "download", data_id, plot_id, "plot", sep = "_")
#   
#   output[[results_id]] <- downloadHandler(
#     filename = function() {
#       paste0(data_id, "_", plot_id, "_results_", input$region, "_", Sys.Date(), ".csv")
#     },
#     content = function(file) {
#       readr::write_csv(results_reactive(), file)
#     }
#   )
#   
#   output[[raw_id]] <- downloadHandler(
#     filename = function() {
#       paste0(data_id, "_", plot_id, "_raw_", input$region, "_", Sys.Date(), ".csv")
#     },
#     content = function(file) {
#       readr::write_csv(raw_reactive(), file)
#     }
#   )
#   
#   output[[plot_id_full]] <- downloadHandler(
#     filename = function() {
#       paste0(data_id, "_", plot_id, "_plot_", input$region, "_", Sys.Date(), ".png")
#     },
#     content = function(file) {
#       ggplot2::ggsave(
#         filename = file,
#         plot = plot_reactive(),
#         width = 8,
#         height = 5,
#         dpi = 300
#       )
#     }
#   )
# }
add_metric_downloads <- function(output, prefix, data_id, plot_id,
                                 results_reactive, raw_reactive, plot_reactive,
                                 download_label_reactive, width = 8,
                                 height = 5) {
  
  results_id <- paste(prefix, "download", data_id, plot_id, "results", sep = "_")
  raw_id     <- paste(prefix, "download", data_id, plot_id, "raw", sep = "_")
  plot_id_full <- paste(prefix, "download", data_id, plot_id, "plot", sep = "_")
  
  clean_label <- function(x) {
    x |>
      stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
      stringr::str_replace_all("^_|_$", "")
  }
  
  output[[results_id]] <- downloadHandler(
    filename = function() {
      paste0(
        data_id, "_", plot_id, "_results_",
        clean_label(download_label_reactive()),
        "_", Sys.Date(), ".csv"
      )
    },
    content = function(file) {
      readr::write_csv(results_reactive(), file)
    }
  )
  
  output[[raw_id]] <- downloadHandler(
    filename = function() {
      paste0(
        data_id, "_", plot_id, "_raw_",
        clean_label(download_label_reactive()),
        "_", Sys.Date(), ".csv"
      )
    },
    content = function(file) {
      readr::write_csv(raw_reactive(), file)
    }
  )
  
  output[[plot_id_full]] <- downloadHandler(
    filename = function() {
      paste0(
        data_id, "_", plot_id, "_plot_",
        clean_label(download_label_reactive()),
        "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = plot_reactive(),
        width = width,
        height = height,
        dpi = 300
      )
    }
  )
}

plot_stacked_species <- function(
    plot_df,
    other_labels,
    selected_name,
    colour_pool = species_colours,
    period_order = c("Pre-bloom", "Bloom")
) {
  
  df <- plot_df %>%
    dplyr::filter(group_name == selected_name)
  
  period_order <- c(
    "Pre-bloom",
    sort(setdiff(unique(df$period_name), "Pre-bloom"))
  )
  
  df <- df %>%
    dplyr::mutate(
      period_name = factor(period_name, levels = period_order)
    )
  
  species_order <- df %>%
    dplyr::filter(species_plot != "Other") %>%
    dplyr::group_by(species_plot) %>%
    dplyr::summarise(
      total_percent = sum(percent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_percent)) %>%
    dplyr::pull(species_plot)
  
  n_species <- length(species_order)
  
  if (n_species > length(colour_pool)) {
    stop(
      "This plot contains ", n_species,
      " species, but only ", length(colour_pool),
      " colours are available."
    )
  }
  
  # Use colours 1:n for the species in this particular plot
  plot_palette <- setNames(
    colour_pool[seq_len(n_species)],
    species_order
  )
  
  # Other is always grey
  plot_palette <- c(plot_palette, Other = "#d9d9d9")
  
  species_order <- c(species_order, "Other")
  
  df <- df %>%
    dplyr::mutate(
      species_plot = factor(species_plot, levels = species_order)
    )
  
  labels_df <- other_labels %>%
    dplyr::filter(group_name == selected_name) %>%
    dplyr::left_join(
      df %>%
        dplyr::filter(species_plot == "Other") %>%
        dplyr::select(group_name, period_name, percent),
      by = c("group_name", "period_name")
    ) %>%
    dplyr::mutate(ypos = percent / 2)
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = period_name,
      y = percent,
      fill = species_plot
    )
  ) +
    ggplot2::geom_col(width = 0.75, colour = "black") +
    # ggplot2::geom_text(
    #   data = labels_df,
    #   ggplot2::aes(
    #     x = period_name,
    #     y = ypos,
    #     label = label
    #   ),
    #   inherit.aes = FALSE,
    #   fontface = "bold",
    #   size = 4
    # ) +
    ggplot2::scale_fill_manual(
      values = plot_palette,
      drop = FALSE
    ) +
    # ggplot2::scale_y_continuous(
    #   labels = scales::label_percent(scale = 1),
    #   limits = c(0, 100),
    #   expand = ggplot2::expansion(mult = c(0, 0.02))
    # ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      breaks = c(0, 25, 50, 75, 100),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 100),
      expand = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Percentage of observations",
      fill = "Species"
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 13),
      legend.text = ggtext::element_markdown(size = 10),
      legend.position = "right",
      legend.key.height = grid::unit(0.8, "cm"),
      legend.spacing.y = grid::unit(0.2, "cm")
    ) +
    plot_theme
}

# ---------------------------------------------------------------------------
# RLS (Dive) equivalents of the species-plot helpers above.
#
# RLS stacked/top-abundance data covers THREE separate RLS "methods" (M1
# fish, M2 fish, M2 invertebrates) for every region/location, whereas each
# BRUV plot only ever covers one set of species. Rather than cram all three
# into a single ggplot (which would need up to 3x14 fill colours for the
# stacked plot, and mismatched top-N species per method for the
# common-species plot), each RLS plot below builds one panel per method and
# combines them:
#   - plot_stacked_taxa_rls() calls the EXISTING plot_stacked_species() once
#     per method and combines the panels with patchwork::wrap_plots() - each
#     panel keeps its own independent colour legend, since the top species
#     are picked separately per method.
#   - plot_top_taxa_rls() uses facet_wrap(scales = "free") instead, since
#     species there are only used for the y-axis (not for colour) - one plot
#     with free x AND y scales works and needs no extra package. Free x is
#     important here: M2 invertebrate abundances are much smaller than fish
#     abundances, so without it their bars are invisible next to the fish
#     panels on a shared 0-100 scale.
#
# Both stack their method-panels one above the other (M1 fish / M2 fish /
# M2 invertebrates, top to bottom) rather than side by side, per Brooke's
# request - pre-bloom and bloom stay as two side-by-side plots (as BRUVS
# already shows them), each of which stacks its own methods vertically.
# BRUVS only ever has one method, so its plots are unaffected (one row).
#
# Both take a `spatial_level_value` of "region" or "location" so the SAME
# code drives the Region Summary and Location Summary tabs - once region-
# level rows exist in rls_data$stacked_period / stacked_period_split /
# top_occurrence_abundance_selection (scripts 08 & 09 re-run with region
# enabled), these will start finding and plotting them automatically.
# ---------------------------------------------------------------------------

plot_stacked_taxa_rls <- function(
    stacked_df,
    spatial_level_value,
    group_value,
    colour_pool = species_colours
) {

  df <- stacked_df %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_name == group_value
    ) %>%
    dplyr::rename(species_plot = taxon_plot)

  methods_present <- df %>%
    dplyr::distinct(method) %>%
    dplyr::arrange(method) %>%
    dplyr::pull(method)

  # plot_stacked_species() computes (but never draws - the geom_text call
  # inside it is commented out) a data frame of "Other" labels; an empty
  # one with the right columns is all it needs. IMPORTANT: don't include a
  # "percent" column here - plot_stacked_species() left_joins this onto a
  # table that also has "percent", and if both sides already have that
  # column dplyr suffixes them (percent.x/percent.y) instead of leaving a
  # plain "percent" column, which then makes its own `percent / 2`
  # calculation silently resolve to scales::percent (a function) instead
  # of data and error with "non-numeric argument to binary operator".
  empty_labels <- tibble::tibble(
    group_name  = character(),
    period_name = character(),
    label       = character()
  )

  if (length(methods_present) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No stacked abundance data available") +
        ggplot2::theme_void()
    )
  }

  panels <- lapply(methods_present, function(m) {
    plot_stacked_species(
      plot_df       = df %>% dplyr::filter(method == m),
      other_labels  = empty_labels,
      selected_name = group_value,
      colour_pool   = colour_pool
    ) +
      ggplot2::labs(title = m) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13)
      )
  })

  # Stacked one above the other (M1 fish / M2 fish / M2 invertebrates), not
  # side by side - see server.R's comment above plot_top_taxa_rls() for why.
  patchwork::wrap_plots(panels, ncol = 1)
}

plot_top_taxa_rls <- function(
    selection_df,
    spatial_level_value,
    group_value,
    focal_period,
    title_lab,
    number_species
) {

  period_cols <- c(
    "Pre-bloom"  = "#193b73",
    "Bloom"      = "#92bd83",
    "Post-bloom" = "#92bd83"
  )

  df_raw <- selection_df %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_name == group_value
    )

  # Top N species PER RLS method (dataset_label), within the focal period -
  # keeps M1 fish / M2 fish / M2 invertebrates picks independent of each
  # other, rather than one shared top-N across all of them.
  top_by_dataset <- df_raw %>%
    dplyr::filter(focus_group == focal_period) %>%
    dplyr::group_by(dataset_label) %>%
    dplyr::slice_max(
      order_by  = average_abundance,
      n         = number_species,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dataset_label, display_name)

  plot_df <- df_raw %>%
    dplyr::inner_join(top_by_dataset, by = c("dataset_label", "display_name"))

  # Defensive: with no rows there's no dataset_label to facet on at all,
  # which makes facet_wrap() error out ("Faceting variables must have at
  # least one value") rather than just drawing an empty panel. This can
  # happen genuinely (no data for this group) or transiently (the method
  # switch fired before the location dropdown updated to match it) - show
  # a message instead of crashing either way.
  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = paste("No data available for:", group_value)) +
        ggplot2::theme_void() +
        ggplot2::labs(title = title_lab)
    )
  }

  period_levels <- c("Pre-bloom", setdiff(unique(plot_df$focus_group), "Pre-bloom"))
  plot_df$focus_group <- factor(plot_df$focus_group, levels = period_levels)

  # A facet-unique key so free_y scales can order species independently
  # within each method's panel (base ggplot2 has no built-in "reorder
  # within facet" - the trick is to make the factor levels unique per facet,
  # then strip the method prefix back off again for the axis labels).
  plot_df <- plot_df %>%
    dplyr::mutate(label_key = paste(dataset_label, display_name, sep = "___"))

  order_levels <- plot_df %>%
    dplyr::filter(focus_group == focal_period) %>%
    dplyr::arrange(dataset_label, average_abundance) %>%
    dplyr::pull(label_key) %>%
    unique()

  # Belt-and-braces: any label_key not covered above (shouldn't happen -
  # top_by_dataset is derived from the focal period already).
  order_levels <- c(order_levels, setdiff(unique(plot_df$label_key), order_levels))

  plot_df$label_key <- factor(plot_df$label_key, levels = order_levels)

  dodge <- ggplot2::position_dodge(width = 0.8)

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = average_abundance, y = label_key, fill = focus_group)
  ) +
    ggplot2::geom_col(position = dodge) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = average_abundance - abundance_se,
        xmax = average_abundance + abundance_se
      ),
      position = dodge,
      height   = 0.3
    ) +
    ggplot2::facet_wrap(~ dataset_label, scales = "free", ncol = 1) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    ggplot2::scale_fill_manual(values = period_cols) +
    ggplot2::labs(
      x     = "Average abundance per transect",
      y     = NULL,
      title = title_lab,
      fill  = NULL
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggtext::element_markdown(size = 11),
      strip.text      = ggplot2::element_text(face = "bold")
    ) +
    plot_theme
}

# plot_stacked_species <- function(
#     plot_df,
#     other_labels,
#     selected_name,
    # palette,
# #    colour_pool = species_colours,
#     period_order = c("Pre-bloom", "Bloom")
# ) {
#   
#   dat <- plot_df%>%
#     dplyr::filter(group_name == selected_name)
#   
#   period_order <- c(
#     "Pre-bloom",
#     sort(setdiff(unique(dat$period_name), "Pre-bloom"))
#   )
#   
#   df <- plot_df %>%
#     dplyr::filter(group_name == selected_name) %>%
#     dplyr::mutate(
#       period_name = factor(period_name, levels = period_order)#,
#       # species_plot = forcats::fct_relevel(species_plot, "Other", after = Inf)
#     )
#   
#   species_order <- df %>%
#     dplyr::filter(species_plot != "Other") %>%
#     dplyr::group_by(species_plot) %>%
#     dplyr::summarise(total_percent = sum(percent, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::arrange(-total_percent) %>%
#     dplyr::pull(species_plot)
#   
#   species_order <- c(species_order, "Other")
#   
#   df <- df %>%
#     dplyr::mutate(
#       species_plot = factor(species_plot, levels = species_order)
#     )
#   
#   labels_df <- other_labels %>%
#     dplyr::filter(group_name == selected_name) %>%
#     dplyr::left_join(
#       df %>%
#         dplyr::filter(species_plot == "Other") %>%
#         dplyr::select(group_name, period_name, percent),
#       by = c("group_name", "period_name")
#     ) %>%
#     dplyr::mutate(ypos = percent / 2)
#   
#   ggplot2::ggplot(df, ggplot2::aes(x = period_name, y = percent, fill = species_plot)) +
#     ggplot2::geom_col(width = 0.75, colour = "black") +
#     ggplot2::geom_text(
#       data = labels_df,
#       ggplot2::aes(x = period_name, y = ypos, label = label),
#       inherit.aes = FALSE,
#       fontface = "bold",
#       size = 4
#     ) +
#     ggplot2::scale_fill_manual(values = palette) +
#     ggplot2::scale_y_continuous(
#       labels = scales::label_percent(scale = 1),
#       limits = c(0, 100),
#       expand = ggplot2::expansion(mult = c(0, 0.02))
#     ) +
#     ggplot2::labs(
#       x = NULL,
#       y = "Percentage of observations",
#       fill = "Species"
#     ) +
#     ggplot2::theme_minimal(base_size = 15) +
#     ggplot2::theme(
#       panel.grid = ggplot2::element_blank(),
#       axis.text.x = ggplot2::element_text(size = 13),
#       # legend.text = ggplot2::element_text(face = "italic"),
#       legend.text = ggtext::element_markdown(size = 10),
#       legend.position = "right",
#       legend.key.height = unit(0.8, "cm"),
#       legend.spacing.y = unit(0.2, "cm")
#     ) + plot_theme + 
#     scale_y_continuous(expand = expansion(mult = c(0, 0)))
# }

# ------------------------------ server ---------------------------------------

server <- function(input, output, session) {

  regions_joined <- hab_data$regions_shp |>
    left_join(hab_data$regions_summaries, by = "region") %>%
    left_join(hab_data$overall_impact)

  # ---- RLS (Dive) narrative summary text: live-read from csv ---------------
  # These two lookup csvs are created/updated by
  # "01_Download and format data for app/RLS/15_combine_rls_data_for_app.R"
  # and are meant to be hand-edited by collaborators afterwards. Reading
  # them with reactiveFileReader (rather than baking them into
  # rls_data.Rdata) means a saved edit shows up in the running app within
  # a few seconds, with no restart needed.
  rls_region_summary_text <- if (!is.null(rls_data) && file.exists(rls_data$region_summary_lookup_path)) {
    reactiveFileReader(
      intervalMillis = 5000,
      session        = session,
      filePath       = rls_data$region_summary_lookup_path,
      readFunc       = readr::read_csv,
      show_col_types = FALSE
    )
  } else {
    function() tibble::tibble(region = character(), summary = character())
  }

  rls_location_summary_text <- if (!is.null(rls_data) && file.exists(rls_data$location_summary_lookup_path)) {
    reactiveFileReader(
      intervalMillis = 5000,
      session        = session,
      filePath       = rls_data$location_summary_lookup_path,
      readFunc       = readr::read_csv,
      show_col_types = FALSE
    )
  } else {
    function() tibble::tibble(reporting_location = character(), summary = character())
  }

  # Default selected region (first available)
  # selected_region <- reactiveVal({
  #   (regions_joined$region[!is.na(regions_joined$region)])[8]
  # })
  
  selected_region <- reactiveVal((regions_joined$region[!is.na(regions_joined$region)])[8])
  
  # whenever the dropdown changes, update selected_region
  observeEvent(input$region, {
    req(input$region)
    selected_region(input$region)
  }, ignoreInit = TRUE)
  
  # Value boxes ----
  # Number of BRUV Deployments ----
  number_bruv_deployments_pre <- reactive({
    x <- hab_data$hab_number_bruv_deployments %>%
      dplyr::filter(period == "Pre-bloom",
                    region %in% selected_region()) %>%
      dplyr::pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  number_bruv_deployments_post <- reactive({
    x <- hab_data$hab_number_bruv_deployments %>%
      dplyr::filter(period == "Bloom",
                    region %in% selected_region()) %>%
      dplyr::pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "number_bruv_deployments",
    left_reactive  = number_bruv_deployments_pre,
    right_reactive = number_bruv_deployments_post,
    format_fn = scales::label_comma()
  )
  
  # Number of UVC surveys ----
  number_rls_deployments_pre <- safe_pull(function() {
    x <- hab_data$hab_number_rls_deployments %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  number_rls_deployments_post <- safe_pull(function() {
    x <- hab_data$hab_number_rls_deployments %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "number_rls_deployments",
    left_reactive  = number_rls_deployments_pre,
    right_reactive = number_rls_deployments_post,
    format_fn = scales::label_comma()
  )
  
  # Number of fish counted ----
  fish_counted_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  fish_counted_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "fish_counted",
    left_reactive  = fish_counted_pre,
    right_reactive = fish_counted_post,
    format_fn = scales::label_comma()
  )
  
  # Number of fish species ----
  fish_species_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish_species %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  fish_species_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_fish_species %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  # TODO probs doesn't make sense to split this for demo
  twoValueBoxServer(
    id = "fish_species",
    left_reactive  = fish_species_pre,
    right_reactive = fish_species_post,
    format_fn = scales::label_comma()
  )
  
  # Number of other species ----
  non_fish_species_pre <- safe_pull(function() {
    x <- hab_data$hab_number_of_nonfish_species %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  non_fish_species_post <- safe_pull(function() {
    x <- hab_data$hab_number_of_nonfish_species %>%
      dplyr::filter(period %in% "Bloom") %>%
      dplyr::filter(region %in% selected_region()) %>%
      pull(number)
    if (length(x) == 0) NA_real_ else x
  })
  
  twoValueBoxServer(
    id = "non_fish_species",
    left_reactive  = non_fish_species_pre,
    right_reactive = non_fish_species_post,
    format_fn = scales::label_comma()
  )
  
  # Years surveyed----
  min_year_pre <- reactive({
    hab_data$hab_min_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  max_year_pre <- reactive({
    hab_data$hab_max_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  years_pre <- reactive({
    paste0(min_year_pre(), " - ", max_year_pre()) 
  })
  
  min_year_post <- reactive({
    hab_data$hab_min_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  max_year_post <- reactive({
    hab_data$hab_max_year %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  years_post <- reactive({
    min_year <- min_year_post()
    max_year <- max_year_post()
    
    # If no data, bail out early with NA (character)
    if (length(min_year) == 0 || length(max_year) == 0 ||
        all(is.na(min_year)) || all(is.na(max_year))) {
      return(NA_character_)
    }
    
    # Coerce once
    min_year_num <- as.numeric(min_year)
    max_year_num <- as.numeric(max_year)
    
    # Safety: if still NA after coercion, treat as no data
    if (is.na(min_year_num) || is.na(max_year_num)) {
      return(NA_character_)
    }
    
    if (min_year_num == max_year_num) {
      as.character(min_year_num)
    } else {
      paste0(min_year_num, " - ", max_year_num)
    }
  })
  
  twoValueBoxServer(
    id = "years",
    left_reactive  = years_pre,
    right_reactive = years_post,
    format_fn = as.character
  )
  
  # Depth ranges ----
  min_depth_pre <- reactive({
    hab_data$hab_min_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  max_depth_pre <- reactive({
    hab_data$hab_max_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Pre-bloom") %>%
      pull(number)
  })
  
  depth_pre <- reactive({
    paste0(min_depth_pre(), " - ", max_depth_pre(), " m") 
  })
  
  min_depth_post <- reactive({
    hab_data$hab_min_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  max_depth_post <- reactive({
    hab_data$hab_max_depth %>%
      dplyr::filter(region %in% selected_region()) %>%
      dplyr::filter(period %in% "Bloom") %>%
      pull(number)
  })
  
  depth_post <- reactive({
    min_depth <- min_depth_post()
    max_depth <- max_depth_post()
    
    # If no data, bail out early with NA (character)
    if (length(min_depth) == 0 || length(max_depth) == 0 ||
        all(is.na(min_depth)) || all(is.na(max_depth))) {
      return(NA_character_)
    }
    
    # Coerce once
    min_depth_num <- as.numeric(min_depth)
    max_depth_num <- as.numeric(max_depth)
    
    # Safety: if still NA after coercion, treat as no data
    if (is.na(min_depth_num) || is.na(max_depth_num)) {
      return(NA_character_)
    }
    
    if (min_depth_num == max_depth_num) {
      paste0(as.character(min_depth_num), " m")
    } else {
      paste0(min_depth_num, " - ", max_depth_num, " m")
    }
  })
  
  twoValueBoxServer(
    id = "depths",
    left_reactive  = depth_pre,
    right_reactive = depth_post,
    format_fn = as.character
  )
  # 
  # depths <- reactive({
  #   paste0(scales::label_comma()(min_depth()), " - ", scales::label_comma()(max_depth()), " m") 
  # })
  # Average Depth  ----
  mean_depth_pre <- reactive({
    x <- hab_data$hab_mean_depth %>%
      dplyr::filter(period == "Pre-bloom",
                    region %in% selected_region()) %>%
      pull(number)
    
    if (length(x) == 0) NA_real_ else paste0(scales::label_comma()(x), " m") 
  })
  
  mean_depth_post <- reactive({
    x <- hab_data$hab_mean_depth %>%
      dplyr::filter(period == "Bloom",
                    region %in% selected_region()) %>%
      pull(number)
    
    if (length(x) == 0) NA_real_ else paste0(scales::label_comma()(x), " m") 
  })
  
  twoValueBoxServer(
    id = "mean_depth",
    left_reactive  = mean_depth_pre,
    right_reactive = mean_depth_post,
    format_fn = as.character
  )
  
  output$map <- renderLeaflet({
    
    method_cols <- c("BRUVs" = "#004DA7", "UVC" = "#C600FF")
    pts <- ensure_sf_ll(hab_data$hab_combined_metadata)
    
    m <- base_map(current_zoom = 7) |>
      # define panes with explicit stacking
      addMapPane("points",    zIndex = 411) |>
      addMapPane("regions",   zIndex = 412) |>
      addMapPane("highlight", zIndex = 415) %>%
      
      leafgl::addGlPoints(
        data = pts,
        fillColor = method_cols[pts$method],
        weight = 1,
        popup = pts$popup,
        group = "Sampling locations",
        pane  = "points"
      ) %>%
      
      # polygons ABOVE points
      addPolygons(
        data = regions_joined,
        layerId = ~region,
        label   = ~region,
        color = ~hab_data$pal_factor(regions_joined$overall_impact),#"#444444",
        weight = 5,
        opacity = 1,
        fillOpacity = 0, #0.7
        fillColor = ~hab_data$pal_factor(regions_joined$overall_impact),
        group = "Impact regions",
        options = pathOptions(pane = "highlight"),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 6,
          bringToFront = TRUE
        )
      ) |>
      
      addLegend(
        "bottomright",
        title  = "Overall Impact",
        colors = c(unname(hab_data$pal_vals[hab_data$ordered_levels]), "grey"),
        labels = c("High", "Medium","Low", "Surveys incomplete"),
        opacity = 0.8,
        group   = "Impact regions"
      ) |>
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Impact regions", "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      
      hideGroup("Australian Marine Parks") |>
      
      hideGroup("Impact regions") |>
      
      addLegend(
        "topright",
        colors = unname(method_cols),
        labels = names(method_cols),
        title = "Survey method",
        opacity = 1,
        group = "Sampling locations",
        layerId = "methodLegend"
      ) 
    
    
    m
  })
  
  
  # # Click handler
  # observeEvent(input$map_shape_click, {
  #   click <- input$map_shape_click
  #   if (!is.null(click$id)) {
  #     selected_region(click$id)
  #   }
  # })
  
  # observe({
  #   req(selected_region())
  #   
  #   region_selected <- regions_joined |>
  #     dplyr::filter(region == selected_region())
  #   
  #   leafletProxy("map") |>
  #     clearGroup("highlight") |>
  #     addPolygons(
  #       data = region_selected,
  #       color = "white",
  #       weight = 6,
  #       fillColor = "white",
  #       fillOpacity = 0.2,
  #       opacity = 0.75,
  #       group = "highlight",
  #       options = pathOptions(pane = "highlight")
  #     )
  # })
  
  # # Selected region badge
  # output$selected_region_badge <- renderUI({
  #   req(selected_region())
  #   reg <- selected_region()
  #   ov <- hab_data$regions_summaries |> 
  #     filter(region == reg) |> 
  #     pull(overall) |> 
  #     as.character()
  #   
  #   badge_col <- hab_data$pal_vals[[ov %||% "low"]]
  #   
  #   tags$div(
  #     style = sprintf("padding:8px 12px;border-radius:8px;background:%s;color:white;display:inline-block;", badge_col),
  #     tags$b(reg),
  #     if (!is.na(ov)) tags$span(sprintf(" — %s", tools::toTitleCase(ov)))
  #   )
  # })
  
  # # Selected region title ----
  # output$region_title <- renderUI({
  #   req(selected_region())
  #   reg <- selected_region()
  #   
  #   tags$div(
  #     tags$h3(paste("Algal bloom impacts on nearshore marine biodiversity monitoring progress:", reg))
  #   )
  # })
  
  # ---- Summary text ----
  output$region_summary_text <- renderUI({
    req(input$region)

    reg <- input$region

    txt <- if (identical(input$method, "Dive")) {
      rls_region_summary_text() |>
        dplyr::filter(region == reg) |>
        dplyr::pull(summary)
    } else {
      hab_data$regions_summaries |>
        dplyr::filter(region == reg) |>
        dplyr::pull(summary)
    }

    if (length(txt) == 0) txt <- "Add summary text for this region."

    HTML(markdown::markdownToHTML(text = txt, fragment.only = TRUE))
  })
  
  indicator_table <- tibble::tibble(
    Threshold = c(
      "Low = ≥80% of the pre-bloom value",
      "Medium = 50–80% of the pre-bloom value",
      "High = 0–50% of the pre-bloom value"
    ),
    Example = list(
      plot_cell("example_low"),
      plot_cell("example_medium"),
      plot_cell("example_high")
    )
  )
  
  output$pointer_table <- renderUI({
    tags$table(
      class = "table table-sm hab-table",
      tags$thead(
        tags$tr(
          tags$th("Threshold"),
          tags$th("Example Plot")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Low = ≥80% of the pre-bloom value"),
          tags$td(plotOutput("example_low",  height = 80, width = 120))
        ),
        tags$tr(
          tags$td("Medium = 50–80% of the pre-bloom value"),
          tags$td(plotOutput("example_medium", height = 80, width = 120))
        ),
        tags$tr(
          tags$td("High = 0–50% of the pre-bloom value"),
          tags$td(plotOutput("example_high", height = 80, width = 120))
        )
      )
    )
  })
  
  output$example_low <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "Low") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  output$example_medium <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "Medium") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  output$example_high <- renderPlot(bg = "transparent", {
    half_donut_with_dial(values = c(1,1,1), mode = "absolute", status = "High") +
      theme(
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)
      )
  })
  
  # Indiactor table
  output$indicator_table <- renderUI({
    
    # # text for the single big cell
    # threshold_html <- HTML(paste(
    #   "Low = ≥80% of the pre-bloom value",
    #   "Medium = 50–80% of the pre-bloom value",
    #   "High = 0–50% of the pre-bloom value",
    #   sep = "<br>"
    # ))
    
    tags$table(
      class = "table table-sm hab-table",  # uses bootstrap styling
      # header
      tags$thead(
        tags$tr(
          tags$th("Indicator"),
          tags$th("Description")#,
          # tags$th("Impact thresholds")
        )
      ),
      # body
      tags$tbody(
        # first row: also contains the big thresholds cell
        tags$tr(
          tags$td(indicator_tbl$Indicator[1]),
          tags$td(indicator_tbl$Description[1])#,
          # tags$td(
          #   rowspan = nrow(indicator_tbl),    # merge down all rows
          #   style   = "vertical-align:top; white-space:normal;",
          #   threshold_html
          # )
        ),
        # remaining rows: just Indicator + Description
        lapply(2:nrow(indicator_tbl), function(i) {
          tags$tr(
            tags$td(indicator_tbl$Indicator[i]),
            tags$td(indicator_tbl$Description[i])
          )
        })
      )
    )
  })
  
  observeEvent(input$open_info_table, {
    showModal(
      modalDialog(
        title = "Metric definitions",
        tableOutput("indicator_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$open_info_pointers, {
    showModal(
      modalDialog(
        title = "Impact assessment",
        tableOutput("pointer_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$open_info_table_location, {
    showModal(
      modalDialog(
        title = "Metric definitions",
        tableOutput("indicator_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$open_info_pointers_location, {
    showModal(
      modalDialog(
        title = "Impact assessment",
        tableOutput("pointer_table"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  # Pointer plots----
  # Pointer plots: overall + 5 indicators in one figure ------------------------
  output$impact_gauges <- renderPlot({
    req(input$region)
    make_impact_gauges(input$region)
  })
  
  output$overall_impact_gauge <- renderPlot({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      # Mirrors BRUV's own convention (make_overall_impact_gauge() above)
      # of using the Shannon diversity index as the single "overall
      # impact" headline gauge. RLS has 3 Shannon diversity metrics (one
      # per method) rather than BRUV's one - M1 fish is shown here as the
      # representative one. Let us know if a different metric, or a
      # combination of the three, would be more meaningful.
      return(
        get_metric_plot_rls(
          rls_data$pct_change_region, "spatial_group", input$region,
          this_metric_id = "m1_fish_shannon",
          title_lab = "M1 fish Shannon diversity index"
        )
      )
    }

    make_overall_impact_gauge(input$region)
  })

  output$region_impact_gauges <- renderPlot({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      return(make_impact_gauges_rls(rls_data$pct_change_region, "spatial_group", input$region))
    }

    make_impact_gauges(input$region)
  }, height = function() {
    # Dive's gauge grid has 5 rows (one per biological metric) vs BRUV's
    # 2 rows (6 metrics in a 3x2 grid) - see "Region Impact overview"
    # card's fill = FALSE in ui.R for why the card can grow to match.
    if (identical(input$method, "Dive")) 780 else 300
  })
  
  # Filters to whichever method is selected in this tab's BRUVS / Dive
  # switch (input$method). RLS uses rls_data$sites (its own location/region
  # vocabulary, already carrying plain lat/lon columns).
  deployments <- reactive({
    if (identical(input$method, "Dive")) {
      req(rls_data)

      rls_data$sites %>%
        dplyr::filter(region %in% input$region) %>%
        dplyr::rename(longitude_dd = longitude, latitude_dd = latitude)
    } else {
      deployments <- hab_data$hab_combined_metadata %>%
        dplyr::filter(region %in% input$region, method %in% "BRUVs")

      # Extract coordinates
      coords <- st_coordinates(deployments)

      # Convert coordinates to a data frame or tibble
      coords_df <- as.data.frame(coords)

      # Rename columns for clarity (optional)
      colnames(coords_df) <- c("longitude_dd", "latitude_dd")

      # Bind the new coordinate columns to the original sf object
      bind_cols(deployments, coords_df)
    }
  })
  
  min_lat <- reactive({min(deployments()$latitude_dd, na.rm = TRUE)})
  min_lon <- reactive({min(deployments()$longitude_dd, na.rm = TRUE)})
  max_lat <- reactive({max(deployments()$latitude_dd, na.rm = TRUE)})
  max_lon <- reactive({max(deployments()$longitude_dd, na.rm = TRUE)})
  
  output$region_survey_effort <- renderLeaflet({
    req(input$region)

    is_dive <- identical(input$method, "Dive")

    method_cols <- if (is_dive) c("Dive" = "#C600FF") else c("BRUVs" = "#004DA7")

    pts <- if (is_dive) {
      req(rls_data)
      ensure_sf_ll(rls_data$sites, lon = "longitude", lat = "latitude") %>%
        dplyr::filter(region %in% input$region)
    } else {
      ensure_sf_ll(hab_data$hab_combined_metadata) %>%
        dplyr::filter(region %in% input$region, method %in% "BRUVs")
    }

    shp <- regions_joined %>%
      dplyr::filter(region %in% input$region)

    m <- base_map(current_zoom = 7) %>%
      # polygons for reporting region
      addPolygons(
        data = shp,
        layerId = ~region,
        label   = ~region,
        weight = 5,
        opacity = 1,
        fillOpacity = 0
      )

    # Guard against a region with no geocoded points (Inf/-Inf bounds
    # would otherwise crash fitBounds).
    if (nrow(pts) > 0 &&
        is.finite(min_lon()) && is.finite(min_lat()) &&
        is.finite(max_lon()) && is.finite(max_lat())) {
      m <- m %>% fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    }

    point_fill <- if (is_dive) "#C600FF" else "#004DA7"

    if (nrow(pts) > 0) {
      m <- addCircleMarkers(
        m, data = pts, radius = 6, fillColor = point_fill, fillOpacity = 1,
        weight = 1, color = "black", popup = pts$popup,
        group = "Sampling locations", options = pathOptions(pane = "points")
      )
    } else {
      warning(
        "No geocoded ", if (is_dive) "Dive" else "BRUVS",
        " points found for region '", input$region, "'."
      )
    }

    addLegend(m,
              "topright",
              colors = unname(method_cols),
              labels = names(method_cols),
              title = "Survey method",
              opacity = 1,
              group = "Sampling locations",
              layerId = "methodLegend"
    ) %>%
      hideGroup("Australian Marine Parks")

    return(m)
  })
  
  # ===== EXPLORE INDICATORS & METRICS =====
  
  # Populate region choices (reuse your regions_joined)
  observe({
    req(regions_joined)
    updateSelectizeInput(
      session, "region",
      choices = sort(unique(regions_joined$region)),
      selected = selected_region() %||% sort(unique(regions_joined$region))[1],
      server = TRUE
    )
  })
  
  # Build a tabbed card with one tab per metric
  output$region_tabset <- renderUI({
    req(input$region)

    if (identical(input$method, "Dive")) {
      # RLS's GLMMs are fitted per LOCATION only (see script 11 / the
      # diagnostics in script 15) - there is no region-level modelled-means
      # data to show here. Point people at the Location Summary tab instead
      # of rendering an empty/broken tabset.
      return(
        card(
          card_header("Explore indicators"),
          p(
            "Modelled means (from the RLS GLMMs) are only available at the ",
            strong("location"), " level, since each model is fitted per ",
            "location rather than per region. Switch to the ",
            strong("Location Summary"), " tab to explore Dive indicators, ",
            "or choose BRUVS here to see region-level indicators."
          )
        )
      )
    }

    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(id, prefix = "em")
        )
      })
    )
  })
  
  # helper if you still like dummy_metric_data()
  get_metric_data <- function(metric_id, region, n = 120) {
    dummy_metric_data(metric_id, region, n = n)
  }
  
  metric_plot_type <- function(input, prefix, data_id) {
    isTRUE(input[[metric_plot_type_input_id(prefix, data_id)]])
  }
  
  # RICHNESS --------------------
  
  richness_main_raw <- reactive({
    req(input$region)
    
    hab_data$species_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  richness_main_results <- reactive({
    req(input$region)
    
    hab_data$species_richness_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  richness_status_raw <- reactive({
    req(input$region)
    
    hab_data$species_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  richness_status_results <- reactive({
    richness_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      )
  })
  
  
  # RICHNESS: main plot --------------------
  richness_main_plot <- reactive({
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "richness")
    
    if (show_box) {
      df <- richness_main_raw()
      mean_se <- richness_main_results()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 0.15, height = 0, alpha = 0.35, size = 1.2) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean, ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", 
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      df <- richness_main_results()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(width = 0.6, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 0.6) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          #subtitle = paste0(input$region, ": Average species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", 
              
              panel.grid.minor = element_blank(),           
              panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  
  
  output$em_plot_richness_main <- renderPlot({
    richness_main_plot()
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "richness")]])
  
  # RICHNESS:  status plot --------------------
  richness_status_plot <- reactive({
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "richness")
    
    if (show_box) {
      
      df <- hab_data$species_richness_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          #subtitle = paste0(input$region, ": Species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$species_richness_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(n_species_sample, na.rm = TRUE),
          se   = sd(n_species_sample, na.rm = TRUE) /
            sqrt(sum(!is.na(n_species_sample))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          #subtitle = paste0(input$region, ": Average species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
  })
  
  output$em_plot_richness_status <- renderPlot({
    
    richness_status_plot()
    
  }) |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "richness")]])
  
  # Downloads ----
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "richness",
    plot_id = "main",
    results_reactive = richness_main_results,
    raw_reactive = richness_main_raw,
    plot_reactive = richness_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "richness",
    plot_id = "status",
    results_reactive = richness_status_results,
    raw_reactive = richness_status_raw,
    plot_reactive = richness_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  # TOTAL ABUNDANCE ------------
  total_abundance_main_raw <- reactive({
    req(input$region)
    
    hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  total_abundance_main_results <- reactive({
    req(input$region)
    
    hab_data$total_abundance_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  total_abundance_status_raw <- reactive({
    req(input$region)
    
    hab_data$total_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  total_abundance_status_results <- reactive({
    total_abundance_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        n = sum(!is.na(total_abundance_sample)),
        .groups = "drop"
      )
  })
  
  # TOTAL ABUNDANCE: main plot ------------
  total_abundance_main_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "total_abundance")
    
    if (show_box) {
      
      # Filter for this region
      df <- total_abundance_main_raw()
      
      mean_se <- total_abundance_main_results()
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean,
              ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- total_abundance_main_results()
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          #subtitle = paste0(input$region, ": Average total abundance per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$em_plot_total_abundance_main <- renderPlot({
    total_abundance_main_plot()
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "total_abundance")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "total_abundance")]])
  
  # TOTAL ABUNDANCE: status plot ------------
  total_abundance_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "total_abundance")
    
    if (show_box) {
      
      df <- hab_data$total_abundance_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          #subtitle = paste0(input$region, ": Total abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    } else {
      
      df <- hab_data$total_abundance_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(total_abundance_sample, na.rm = TRUE),
          se   = sd(total_abundance_sample, na.rm = TRUE) /
            sqrt(sum(!is.na(total_abundance_sample))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          #subtitle = paste0(input$region,
          # ": Average total abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  
  output$em_plot_total_abundance_status <- renderPlot({
    
    total_abundance_status_plot()
    
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "total_abundance")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "total_abundance")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "total_abundance",
    plot_id = "main",
    results_reactive = total_abundance_main_results,
    raw_reactive = total_abundance_main_raw,
    plot_reactive = total_abundance_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "total_abundance",
    plot_id = "status",
    results_reactive = total_abundance_status_results,
    raw_reactive = total_abundance_status_raw,
    plot_reactive = total_abundance_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  
  # SHARK & RAYS -------
  
  shark_ray_richness_main_raw <- reactive({
    req(input$region)
    
    hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shark_ray_richness_main_results <- reactive({
    req(input$region)
    
    hab_data$shark_ray_richness_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shark_ray_richness_status_raw <- reactive({
    req(input$region)
    
    hab_data$shark_ray_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shark_ray_richness_status_results <- reactive({
    shark_ray_richness_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      )
  })
  
  # SHARK & RAYS: main plot -----
  shark_ray_richness_main_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "shark_ray_richness")
    
    if (show_box) {
      
      df <- hab_data$shark_ray_richness_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      mean_se <- hab_data$shark_ray_richness_summary %>%
        dplyr::filter(region == input$region)
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$shark_ray_richness_summary %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          #subtitle = paste0(input$region, ": Average shark and ray species richness per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  
  output$em_plot_shark_ray_richness_main <- renderPlot({
    
    shark_ray_richness_main_plot()
    
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "shark_ray_richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "shark_ray_richness")]])
  
  # SHARK & RAYS: status plot -----
  
  shark_ray_richness_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "shark_ray_richness")
    
    if (show_box) {
      df <- hab_data$shark_ray_richness_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          #subtitle = paste(input$region, "— Shark & ray species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$shark_ray_richness_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(n_species_sample, na.rm = TRUE),
          se   = sd(n_species_sample, na.rm = TRUE) /
            sqrt(sum(!is.na(n_species_sample))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          #subtitle = paste0(input$region, ": Average shark & ray species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$em_plot_shark_ray_richness_status <- renderPlot({
    
    shark_ray_richness_status_plot()
    
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "shark_ray_richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "shark_ray_richness")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "shark_ray_richness",
    plot_id = "main",
    results_reactive = shark_ray_richness_main_results,
    raw_reactive = shark_ray_richness_main_raw,
    plot_reactive = shark_ray_richness_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "shark_ray_richness",
    plot_id = "status",
    results_reactive = shark_ray_richness_status_results,
    raw_reactive = shark_ray_richness_status_raw,
    plot_reactive = shark_ray_richness_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  # REEF_ASSOCIATED ----
  reef_associated_richness_main_raw <- reactive({
    req(input$region)
    
    hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  reef_associated_richness_main_results <- reactive({
    req(input$region)
    
    hab_data$reef_associated_richness_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  reef_associated_richness_status_raw <- reactive({
    req(input$region)
    
    hab_data$reef_associated_richness_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  reef_associated_richness_status_results <- reactive({
    reef_associated_richness_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      )
  })
  
  # REEF-ASSOCIATED: main plot -----
  reef_associated_richness_main_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "reef_associated_richness")
    
    if (show_box) {
      
      df <- hab_data$reef_associated_richness_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      mean_se <- hab_data$reef_associated_richness_summary %>%
        dplyr::filter(region == input$region)
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$reef_associated_richness_summary %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          #subtitle = paste0(input$region, ": Average reef associated species richness per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  
  output$em_plot_reef_associated_richness_main <- renderPlot({
    
    reef_associated_richness_main_plot()
    
  })   |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "reef_associated_richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "reef_associated_richness")]])
  
  # REEF-ASSOCIATED: status plot ---------------
  reef_associated_richness_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "reef_associated_richness")
    
    if (show_box) {
      
      df <- hab_data$reef_associated_richness_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          #subtitle = paste0(input$region, ": Reef-associated species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$reef_associated_richness_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(n_species_sample, na.rm = TRUE),
          se   = sd(n_species_sample, na.rm = TRUE) /
            sqrt(sum(!is.na(n_species_sample))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          #subtitle = paste0(input$region, ": Average reef-associated species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$em_plot_reef_associated_richness_status <- renderPlot({
    reef_associated_richness_status_plot()
  })    |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "reef_associated_richness")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "reef_associated_richness")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "reef_associated_richness",
    plot_id = "main",
    results_reactive = reef_associated_richness_main_results,
    raw_reactive = reef_associated_richness_main_raw,
    plot_reactive = reef_associated_richness_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "reef_associated_richness",
    plot_id = "status",
    results_reactive = reef_associated_richness_status_results,
    raw_reactive = reef_associated_richness_status_raw,
    plot_reactive = reef_associated_richness_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  # LARGE FISH -----
  fish_200_abundance_main_raw <- reactive({
    req(input$region)
    
    hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  fish_200_abundance_main_results <- reactive({
    req(input$region)
    
    hab_data$fish_200_abundance_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  fish_200_abundance_status_raw <- reactive({
    req(input$region)
    
    hab_data$fish_200_abundance_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  fish_200_abundance_status_results <- reactive({
    fish_200_abundance_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        n = sum(!is.na(total_abundance_sample)),
        .groups = "drop"
      )
  })
  
  # LARGE FISH: main plot ------------
  fish_200_abundance_main_plot <- reactive({
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "fish_200_abundance")
    
    if (show_box) {
      
      # Filter for this region
      df <- hab_data$fish_200_abundance_samples %>%
        dplyr::filter(region == input$region)
      
      mean_se <- hab_data$fish_200_abundance_summary %>%
        dplyr::filter(region == input$region)
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean,
              ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$fish_200_abundance_summary %>%
        dplyr::filter(region == input$region)
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = paste0(input$region, ": Average total abundance per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$em_plot_fish_200_abundance_main <- renderPlot({
    
    fish_200_abundance_main_plot()
    
  })     |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "fish_200_abundance")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "fish_200_abundance")]])
  
  # ---------- LARGE FISH: status plot --------------------
  fish_200_abundance_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "fish_200_abundance")
    
    if (show_box) {
      df <- hab_data$fish_200_abundance_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = paste0(input$region, ": Large fish (>200 mm) abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$fish_200_abundance_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(total_abundance_sample, na.rm = TRUE),
          se   = sd(total_abundance_sample, na.rm = TRUE) /
            sqrt(sum(!is.na(total_abundance_sample))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = paste0(input$region, ": Average large fish (>200 mm) abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  output$em_plot_fish_200_abundance_status <- renderPlot({
    
    fish_200_abundance_status_plot()
    
  })     |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "fish_200_abundance")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "fish_200_abundance")]])
  
  # Downloads -----
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "fish_200_abundance",
    plot_id = "main",
    results_reactive = fish_200_abundance_main_results,
    raw_reactive = fish_200_abundance_main_raw,
    plot_reactive = fish_200_abundance_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "fish_200_abundance",
    plot_id = "status",
    results_reactive = fish_200_abundance_status_results,
    raw_reactive = fish_200_abundance_status_raw,
    plot_reactive = fish_200_abundance_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  # SHANNON DIVERSITY -------
  shannon_diversity_main_raw <- reactive({
    req(input$region)
    
    hab_data$shannon_diversity_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shannon_diversity_main_results <- reactive({
    req(input$region)
    
    hab_data$shannon_diversity_summary %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shannon_diversity_status_raw <- reactive({
    req(input$region)
    
    hab_data$shannon_diversity_samples %>%
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  shannon_diversity_status_results <- reactive({
    shannon_diversity_status_raw() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(shannon , na.rm = TRUE),
        se = sd(shannon , na.rm = TRUE) /
          sqrt(sum(!is.na(shannon ))),
        n = sum(!is.na(shannon )),
        .groups = "drop"
      )
  })
  
  # SHANNON DIVERSITY: main plot -----
  shannon_diversity_main_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "shannon_diversity")
    
    if (show_box) {
      
      df <- hab_data$shannon_diversity_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      mean_se <- hab_data$shannon_diversity_summary %>%
        dplyr::filter(region == input$region)
      
      ggplot(df, aes(x = period, y = shannon, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$shannon_diversity_summary %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          #subtitle = paste0(input$region, ": Average shannon diversity per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  
  output$em_plot_shannon_diversity_main <- renderPlot({
    
    shannon_diversity_main_plot()
    
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "shannon_diversity")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "shannon_diversity")]])
  
  # SHANNON DIVERSITY: status plot -----
  
  shannon_diversity_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "shannon_diversity")
    
    if (show_box) {
      df <- hab_data$shannon_diversity_samples %>%
        dplyr::filter(region == input$region)
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = shannon, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          #subtitle = paste0(input$region, ": shannon diversity per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- hab_data$shannon_diversity_samples %>%
        dplyr::filter(region == input$region) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(
          mean = mean(shannon, na.rm = TRUE),
          se   = sd(shannon, na.rm = TRUE) /
            sqrt(sum(!is.na(shannon))),
          .groups = "drop"
        )
      
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          #subtitle = paste0(input$region, ": Average shannon diversity per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$em_plot_shannon_diversity_status <- renderPlot({
    
    shannon_diversity_status_plot()
    
  })  |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "shannon_diversity")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "shannon_diversity")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "shannon_diversity",
    plot_id = "main",
    results_reactive = shannon_diversity_main_results,
    raw_reactive = shannon_diversity_main_raw,
    plot_reactive = shannon_diversity_main_plot,
    download_label_reactive = reactive(input$region)
  )
  
  add_metric_downloads(
    output,
    prefix = "em",
    data_id = "shannon_diversity",
    plot_id = "status",
    results_reactive = shannon_diversity_status_results,
    raw_reactive = shannon_diversity_status_raw,
    plot_reactive = shannon_diversity_status_plot,
    download_label_reactive = reactive(input$region)
  )
  
  # ---------- Trophic Groups  ------------
  
  trophic_main_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "trophic")
    
    if (show_box) {
      
      
      # Filter for this region
      df <- hab_data$trophic_groups_samples %>%
        dplyr::filter(region == input$region)
      
      mean_se <- hab_data$trophic_groups_summary %>%
        dplyr::filter(region == input$region)
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
      
      # (Optional) order diet groups if you want a specific order
      diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet unknown")
      df$diet <- factor(df$diet, levels = diet_levels)
      mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
      
      dodge <- position_dodge(width = 0.75)
      
      ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
        geom_boxplot(
          position = dodge,
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          position = position_jitterdodge(
            jitter.width  = 0.15,
            jitter.height = 0,
            dodge.width   = 0.75
          ),
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(
            x    = diet,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se,
            group = period,
            colour = period
          ),
          position = dodge,
          inherit.aes = FALSE,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,  # or "Diet group"
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "top",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      diet_levels <- names(diet_cols)
      
      # Start from the SUMMARY table (means per sample)
      mean_se <- hab_data$trophic_groups_richness_summary %>%
        dplyr::filter(region == input$region) %>%
        dplyr::mutate(
          period = factor(period, levels = c("Pre-bloom", "Bloom")),
          diet   = factor(diet,   levels = diet_levels)
        )
      
      # -------- COUNT VIEW (mean-based) --------
      ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = diet_cols, drop = FALSE) +
        labs(
          x        = NULL,
          y        = "Average no. species",
          fill     = "Diet group"#,
          #subtitle = input$region
        ) +
        theme_minimal(base_size = 16) +
        theme(panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })   |>
    bindCache(input$region, input[[metric_plot_type_input_id("em", "trophic")]]) |>
    bindEvent(input$region, input[[metric_plot_type_input_id("em", "trophic")]])
  
  
  output$em_plot_trophic_main <- renderPlot({
    
    trophic_main_plot()
    
  }) 
  
  trophic_status_plot <- reactive({
    
    req(input$region)
    
    show_box <- metric_plot_type(input, "em", "trophic")
    
    if (show_box) {
      
      # Filter for this region
      df <- hab_data$trophic_groups_samples %>%
        dplyr::filter(region == input$region)
      
      mean_se <- hab_data$trophic_groups_summary %>%
        dplyr::filter(region == input$region)
      
      # Order periods
      df$period     <- factor(df$period,     levels = c("Pre-bloom", "Bloom"))
      mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
      
      # Diet ordering
      diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet unknown")
      df$diet     <- factor(df$diet,     levels = diet_levels)
      mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
      
      dodge <- position_dodge(width = 0.75)
      
      ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
        geom_boxplot(
          position = dodge,
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          position = position_jitterdodge(
            jitter.width  = 0.15,
            jitter.height = 0,
            dodge.width   = 0.75
          ),
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(
            x    = diet,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se,
            group = period,
            colour = period
          ),
          inherit.aes = FALSE,
          position = dodge,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_colour_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          #subtitle = input$region
        ) +
        facet_wrap(~ status) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      diet_levels <- names(diet_cols)
      
      # Start from the SUMMARY table (means per sample)
      mean_se <- hab_data$trophic_groups_richness_summary_status %>%
        dplyr::filter(region == input$region) %>%
        dplyr::mutate(
          period = factor(period, levels = c("Pre-bloom", "Bloom")),
          diet   = factor(diet,   levels = diet_levels)
        )
      
      # -------- COUNT VIEW (mean-based) --------
      ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = diet_cols, drop = FALSE) +
        labs(
          x        = NULL,
          y        = "Average no. species",
          fill     = "Diet group"#,
          #subtitle = input$region
        ) +
        facet_wrap(~ status) +
        theme_minimal(base_size = 16) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$em_plot_trophic_status <- renderPlot({
    
    trophic_status_plot()
    
  }) 
  
  
  # ---- HAB % change summary table (per region) ------------------------------
  output$region_change_table <- renderUI({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)

      # Guard against a missing/empty CSV (script 10 not yet run for this
      # rls_data build) - an empty tibble has none of these columns.
      required_cols <- c("spatial_group", "comparison_type", "metric", "change_overall")
      if (nrow(rls_data$pct_change_region_wide) == 0 ||
          !all(required_cols %in% names(rls_data$pct_change_region_wide))) {
        return(tags$em("No percentage-change data available for this region."))
      }

      df <- rls_data$pct_change_region_wide %>%
        dplyr::filter(spatial_group == input$region, comparison_type == "period") %>%
        dplyr::select(
          Metric = metric,
          Change = change_overall
        )

      if (nrow(df) == 0) {
        return(tags$em("No percentage-change data available for this region."))
      }

      out <- rls_fmt_pct_change_html(df$Change)

      return(
        tags$table(
          class = "table table-sm hab-table",
          tags$thead(
            tags$tr(
              tags$th("Metric"),
              tags$th("Change")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                tags$td(df$Metric[i]),
                tags$td(HTML(out[i]))
              )
            })
          )
        )
      )
    }

    df <- hab_metric_change |>
      dplyr::filter(region == input$region) |>
      dplyr::select(
        Metric = impact_metric,
        Change = percentage_change
      )
    
    vals <- df$Change
    
    # Detect “Surveys incomplete”
    is_incomplete <- grepl("Surveys incomplete", vals, ignore.case = TRUE)
    
    # Parse numeric
    num <- suppressWarnings(as.numeric(vals))
    has_num <- !is.na(num) & !is_incomplete
    
    # Arrows
    arrows <- ifelse(num < 0, "&#8595;", "&#8593;")
    
    # # Colour rules
    # colours <- ifelse(
    #   num <= -50, "#EB5757",
    #   ifelse(num <= -20, "#D4A017", "#3B7EA1")
    # )
    # Colour rules (default + special-case one metric)
    special_metric <- "Bluefin leatherjacket displacement*"
    
    colours <- ifelse(
      df$Metric == special_metric,
      # Special rule (based on magnitude, regardless of sign)
      ifelse(abs(num) < 120, "#3B7EA1",
             ifelse(abs(num) <= 150, "#D4A017", "#EB5757")),
      # Default rule (your existing thresholds; uses signed num)
      ifelse(num <= -50, "#EB5757",
             ifelse(num <= -20, "#D4A017", "#3B7EA1"))
    )
    
    # Build formatted column
    out <- rep("", length(vals))
    out[has_num] <- sprintf(
      "<span style='color:%s; font-weight:700;'>%s %s%%</span>",
      colours[has_num],
      arrows[has_num],
      scales::number(abs(num[has_num]), accuracy = 1)
    )
    out[is_incomplete] <- "<em>Surveys incomplete</em>"
    
    # ---- Build striped table manually ----
    tags$table(
      class = "table table-sm hab-table",
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          tags$th("Change")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Metric[i]),
            tags$td(HTML(out[i]))
          )
        })
      )
    )
  })
  
  make_top10_plot <- function(region_name, 
                              focal_period = c("Pre-bloom", "Bloom"),
                              title_lab = "Common species",
                              number_species,
                              split_status = FALSE,
                              facet_status = FALSE) {
    
    focal_period <- match.arg(focal_period)
    split_status <- isTRUE(split_status)
    facet_status <- isTRUE(facet_status)
    
    # Base colours (your existing theme)
    period_cols <- c(
      "Pre-bloom" = "#193b73",
      "Bloom"     = "#92bd83"
    )
    
    # ---- Data prep ----
    df_raw <- hab_data$region_top_species_average |>
      dplyr::filter(region == region_name)
    
    # Top N species within the focal period
    top_species <- df_raw |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # Data for plotting: either split by status or averaged across status
    if (split_status) {
      plot_df <- hab_data$region_top_species_average_status |>
        dplyr::filter(display_name %in% top_species) %>%
        dplyr::filter(region == region_name)
    } else {
      plot_df <- df_raw |>
        dplyr::filter(display_name %in% top_species)
    }
    
    # Extract sci/common and build markdown label
    plot_df <- plot_df |>
      tidyr::extract(
        display_name,
        into   = c("sci", "common"),
        regex  = "^(.*?)\\s*\\((.*?)\\)$",
        remove = FALSE
      ) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # Period order: ALWAYS Pre-bloom then Bloom
    plot_df$period <- factor(plot_df$period, levels = c("Pre-bloom", "Bloom"))
    
    # Species order: smallest at bottom, biggest at top for focal period
    species_order <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    # Arrange rows so dodging is stable
    plot_df <- plot_df |>
      dplyr::arrange(label, period, dplyr::across(dplyr::any_of("status")))
    
    dodge <- position_dodge(width = 0.8)
    
    # ============================
    #  A) split_status & no facet
    # ============================
    if (split_status && !facet_status) {
      
      # Build combined period:status variable
      plot_df <- plot_df |>
        glimpse()%>%
        dplyr::mutate(
          period_status = interaction(period, status, sep = ": ", drop = TRUE)
        )
      
      plot_df$period_status <- droplevels(plot_df$period_status)
      
      # Build palette dynamically from the actual levels
      status_alpha <- 0.45
      ps_levels    <- levels(plot_df$period_status)
      
      combo_cols_vec <- sapply(ps_levels, function(ps) {
        # split "Pre-bloom: Fished" into c("Pre-bloom", "Fished")
        parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
        per   <- parts[1]
        stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
        base_col <- unname(period_cols[per])
        if (!is.na(stat) && stat == "Fished") {
          scales::alpha(base_col, status_alpha)
        } else {
          base_col
        }
      })
      
      combo_cols <- setNames(combo_cols_vec, ps_levels)
      
      p <- ggplot(
        plot_df,
        aes(
          x    = average,
          y    = label,
          fill = period_status
        )
      ) +
        geom_col(position = dodge) +
        geom_errorbarh(
          aes(
            xmin = average - se,
            xmax = average + se
          ),
          position = dodge,
          height   = 0.3
        ) +
        labs(
          x     = "Average abundance per BRUV",
          y     = NULL,
          title = title_lab,
          fill  = NULL
        ) +
        scale_fill_manual(values = combo_cols)+
        plot_theme #+ scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      # =======================================
      #  B) non-split OR split + facet
      # =======================================
      
      if (split_status && facet_status) {
        # Build period_status for colour mapping
        plot_df <- plot_df |>
          dplyr::mutate(
            period_status = interaction(period, status, sep = ": ", drop = TRUE)
          )
        
        plot_df$period_status <- droplevels(plot_df$period_status)
        
        status_alpha <- 0.45
        ps_levels    <- levels(plot_df$period_status)
        
        combo_cols_vec <- sapply(ps_levels, function(ps) {
          parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
          per   <- parts[1]
          stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
          base_col <- unname(period_cols[per])
          if (!is.na(stat) && stat == "Fished") {
            scales::alpha(base_col, status_alpha)
          } else {
            base_col
          }
        })
        
        combo_cols <- setNames(combo_cols_vec, ps_levels)
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period_status
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          facet_wrap(~ status, nrow = 1) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = combo_cols)+
          plot_theme #+ scale_y_continuous(expand = expansion(mult = c(0, 0)))
        
      } else {
        # NOT split, NOT facet → original 2-colour period-only plot
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = period_cols)+
          plot_theme #+ scale_y_continuous(expand = expansion(mult = c(0, 0)))
      }
    }
    
    # Shared scales / theme
    p +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )
  }
  
  
  
  output$region_common_pre <- renderPlot({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      return(
        plot_top_taxa_rls(
          selection_df         = rls_data$top_occurrence_abundance_selection,
          spatial_level_value  = "region",
          group_value          = input$region,
          focal_period         = "Pre-bloom",
          title_lab            = "Most common species pre-bloom",
          number_species       = input$region_number_species
        )
      )
    }

    make_top10_plot(
      region_name    = input$region,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )
  }, height = function() if (identical(input$method, "Dive")) 950 else 550)

  output$region_common_post <- renderPlot({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      return(
        plot_top_taxa_rls(
          selection_df         = rls_data$top_occurrence_abundance_selection,
          spatial_level_value  = "region",
          group_value          = input$region,
          focal_period         = "Post-bloom",
          title_lab            = "Most common species post-bloom",
          number_species       = input$region_number_species
        )
      )
    }

    make_top10_plot(
      region_name    = input$region,
      focal_period   = "Bloom",
      title_lab      = "Most common species during bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )
  }, height = function() if (identical(input$method, "Dive")) 950 else 550)

  # Downloads -----

  region_common_results <- reactive({

    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      return(
        rls_data$top_occurrence_abundance_selection %>%
          dplyr::filter(spatial_level == "region", group_name == input$region) %>%
          dplyr::mutate(
            average_abundance = round(average_abundance, digits = 3),
            abundance_se       = round(abundance_se, digits = 3)
          )
      )
    }

    hab_data$region_top_species_average |>
      dplyr::filter(region == input$region) %>%
      dplyr::mutate(
        average = clean_number(average),
        se = clean_number(se)
      ) %>%
      dplyr::mutate(average = round(average, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))

  })
  
  region_common_results_name <- reactive({
    
    req(input$region)
    
    paste("Common_species", input$region, sep = "_")
    
  })
  
  output$region_common_download_results <- downloadHandler(
    filename = function() {
      paste0(region_common_results_name(), "_average_abundances", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(region_common_results(), file)
    }
  )
  
  
  region_common_plots <- reactive({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      p1 <- plot_top_taxa_rls(
        selection_df        = rls_data$top_occurrence_abundance_selection,
        spatial_level_value = "region",
        group_value         = input$region,
        focal_period        = "Pre-bloom",
        title_lab           = "Most common species pre-bloom",
        number_species      = input$region_number_species
      )

      p2 <- plot_top_taxa_rls(
        selection_df        = rls_data$top_occurrence_abundance_selection,
        spatial_level_value = "region",
        group_value         = input$region,
        focal_period        = "Post-bloom",
        title_lab           = "Most common species post-bloom",
        number_species      = input$region_number_species
      )

      # Pre/post side by side, each already stacking its own RLS methods
      # top to bottom - matches the on-screen layout.
      return(p1 | p2)
    }

    p1 <- make_top10_plot(
      region_name  = input$region,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )

    p2 <- make_top10_plot(
      region_name  = input$region,
      focal_period   = "Bloom",
      title_lab      = "Most common species during bloom",
      number_species = input$region_number_species,
      split_status   = input$region_species_status,
      facet_status   = input$region_species_facet
    )

    p1 + p2

  })

  output$region_common_download_plot <- downloadHandler(
    filename = function() {
      paste0(region_common_results_name(), "_most_common_species_plots", "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      is_dive <- identical(input$method, "Dive")
      ggplot2::ggsave(
        filename = file,
        plot = region_common_plots(),
        width  = if (is_dive) 14 else 16,
        height = if (is_dive) 12 else 5,
        dpi = 300
      )
    }
  )
  
  
  # ---- Survey progress: filtered to selected reporting region --------------
  
  # 1) Filter to selected region
  survey_region <- reactive({
    req(selected_region())
    df <- hab_data$survey_plan %>%
      dplyr::filter(reporting_region == selected_region())
    df[1, ]
  })
  
  twoValueBoxServer(
    "sites_progress",
    left_reactive  = reactive({ sites_planned }),
    right_reactive = reactive({ sites_completed })
  )
  
  twoValueBoxServer(
    "bruvs_progress",
    left_reactive  = reactive({ bruvs_planned }),
    right_reactive = reactive({ bruvs_completed })
  )
  
  twoValueBoxServer(
    "uvc_progress",
    left_reactive  = reactive({ uvc_planned }),
    right_reactive = reactive({ uvc_completed })
  )
  
  # ===== EXPLORE A MARINE PARK ==============================================
  
  # Populate marine park choices
  observe({
    req(marine_parks)
    updateSelectizeInput(
      session, "mp_park",
      choices  = marine_parks,
      selected = marine_parks[1],
      server   = TRUE
    )
  })
  
  # --- Tabbed card for marine parks (same metrics as regions) ---------------
  
  output$mp_tabset <- renderUI({
    req(input$mp_park)
    
    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          layout_columns(
            col_widths = c(6, 6),
            withSpinner(
              plotOutput(paste0("mp_plot_", id, "_main"), height = 400),
              color = getOption("spinner.color", default = "#0D576E"),
              type = 6
            ),
            withSpinner(
              plotOutput(paste0("mp_plot_", id, "_detail"), height = 400),
              color = getOption("spinner.color", default = "#0D576E"),
              type = 6
            )
          )
        )
      })
    )
  })
  
  # Renderers for each metric at park level
  lapply(names(metric_defs), function(metric_id) {
    local({
      id <- metric_id
      
      # Plot 1: overall pre/post
      output[[paste0("mp_plot_", id, "_main")]] <- renderPlot({
        req(input$mp_park)
        df <- dummy_metric_data(id, input$mp_park, n = 120)
        
        ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            height = 0,      # <— prevents any vertical jitter
            alpha = 0.35,
            size  = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = input$mp_park
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position  = "bottom",
            plot.subtitle    = element_text(margin = margin(b = 6)),
            panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
          )
      }) |>
        bindCache(input$mp_park, id) |>
        bindEvent(input$mp_park)
      
      # Plot 2: Inside vs Outside (func_groups gets group x zone)
      output[[paste0("mp_plot_", id, "_detail")]] <- renderPlot({
        req(input$mp_park)
        df <- dummy_metric_data(id, input$mp_park, n = 120)
        
        p <- ggplot(df, aes(x = period, y = value, fill = period)) +
          geom_boxplot(
            width = 0.6,
            outlier.shape = NA,
            alpha = 0.85,
            colour = "black"
          ) +
          geom_jitter(
            aes(colour = period),
            width = 0.15,
            height = 0,      # <— prevents any vertical jitter
            alpha = 0.35,
            size  = 1.2
          ) +
          scale_fill_manual(values = metric_period_cols) +
          scale_color_manual(values = metric_period_cols) +
          labs(
            x = NULL,
            y = metric_y_lab[[id]] %||% "Value",
            subtitle = paste(input$mp_park, "— Inside vs Outside")
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position  = "bottom",
            plot.subtitle    = element_text(margin = margin(b = 6)),
            panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
          )
        
        if (id == "func_groups") {
          p + facet_grid(group ~ zone)
        } else {
          p + facet_wrap(~ zone)
        }
      }) |>
        bindCache(input$mp_park, id) |>
        bindEvent(input$mp_park)
    })
  })
  
  output$mp_change_table <- renderTable({
    req(input$mp_park)
    
    df <- mp_metric_change |>
      dplyr::filter(park == input$mp_park) |>
      dplyr::select(
        Metric = metric,
        Inside  = inside_change,
        Outside = outside_change,
        Overall = overall_change
      )
    
    fmt_cell <- function(x) {
      ifelse(
        is.na(x),
        "",
        sprintf(
          "%s %s%%",
          ifelse(x < 0, "&#8595;", "&#8593;"),
          scales::number(abs(x), accuracy = 1)
        )
      )
    }
    
    data.frame(
      Metric  = df$Metric,
      Inside  = fmt_cell(df$Inside),
      Outside = fmt_cell(df$Outside),
      Overall = fmt_cell(df$Overall),
      check.names = FALSE
    )
  },
  sanitize.text.function = function(x) x
  )
  
  # ---- Survey progress for marine parks ------------------------------------
  
  mp_survey_row <- reactive({
    req(input$mp_park)
    df <- mp_survey_plan |>
      dplyr::filter(park == input$mp_park)
    df[1, ]
  })
  
  mp_has_bruvs <- reactive({
    grepl("BRUVS", mp_survey_row()$methods[[1]], ignore.case = TRUE)
  })
  
  mp_has_rov <- reactive({
    grepl("ROV", mp_survey_row()$methods[[1]], ignore.case = TRUE)
  })
  
  mp_sites_planned   <- reactive(mp_survey_row()$planned_number_sites)
  mp_sites_completed <- reactive(mp_survey_row()$complete_number_sites)
  
  mp_bruvs_planned <- reactive({
    if (!mp_has_bruvs()) return(0)
    mp_survey_row()$planned_number_drops
  })
  mp_bruvs_completed <- reactive({
    if (!mp_has_bruvs()) return(0)
    mp_survey_row()$complete_number_drops
  })
  
  mp_rov_planned <- reactive({
    if (!mp_has_rov()) return(0)
    mp_survey_row()$planned_number_transects
  })
  mp_rov_completed <- reactive({
    if (!mp_has_rov()) return(0)
    mp_survey_row()$complete_number_transects
  })
  
  twoValueBoxServer("mp_sites_progress",
                    left_reactive  = mp_sites_planned,
                    right_reactive = mp_sites_completed)
  twoValueBoxServer("mp_bruvs_progress",
                    left_reactive  = mp_bruvs_planned,
                    right_reactive = mp_bruvs_completed)
  twoValueBoxServer("mp_rov_progress",
                    left_reactive  = mp_rov_planned,
                    right_reactive = mp_rov_completed)
  
  output$mp_survey_value_boxes <- renderUI({
    df    <- mp_survey_row()
    pct   <- df$percent_sites_completed
    vb_col <- completion_theme(pct)
    
    has_rov_val <- mp_has_rov()
    
    sites_box <- twoValueBoxUI(
      id          = "mp_sites_progress",
      title       = "Sites",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("magnifying-glass", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    bruvs_box <- twoValueBoxUI(
      id          = "mp_bruvs_progress",
      title       = "BRUVS deployments",
      left_label  = "Planned",
      right_label = "Completed",
      icon        = icon("ship", class = "fa-xl"),
      theme_color = "secondary"
    )
    
    pct_box <- value_box(
      title       = "Sites completed",
      value       = sprintf("%.1f%%", pct),
      subtitle    = df$methods[[1]],
      theme_color = vb_col,
      showcase    = icon("percent", class = "fa-xl")
    )
    
    if (has_rov_val) {
      rov_box <- twoValueBoxUI(
        id          = "mp_rov_progress",
        title       = "ROV transects",
        left_label  = "Planned",
        right_label = "Completed",
        icon        = icon("video", class = "fa-xl"),
        theme_color = "secondary"
      )
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        sites_box,
        bruvs_box,
        rov_box,
        pct_box
      )
    } else {
      layout_columns(
        col_widths = c(4, 4, 4),
        sites_box,
        bruvs_box,
        pct_box
      )
    }
  })
  
  # 1. Summarise years by region ----
  years_by_region <- reactive({
    if (identical(input$method, "Dive")) {
      req(rls_data)

      rls_data$samples |>
        dplyr::mutate(year = lubridate::year(sampling_event_start_date)) |>
        dplyr::filter(!is.na(year)) |>
        dplyr::distinct(region, year) |>
        dplyr::group_by(region) |>
        dplyr::summarise(
          n_years       = dplyr::n(),
          years_sampled = paste(sort(unique(year)), collapse = ", "),
          .groups       = "drop"
        ) |>
        dplyr::ungroup() %>%
        dplyr::filter(region %in% input$region)
    } else {
      hab_data$year_dat |>
        dplyr::filter(method %in% "BRUVs") %>%
        dplyr::distinct(region, year) |>
        dplyr::group_by(region) |>
        dplyr::summarise(
          n_years       = dplyr::n(),
          years_sampled = paste(sort(unique(year)), collapse = ", "),
          .groups       = "drop"
        ) |>
        dplyr::ungroup() %>%
        dplyr::filter(region %in% input$region) #%>%
      #glimpse()
    }
  })
  
  # 2. Nicely formatted text for the selected region ----
  output$years_for_region <- renderText({
    req(input$region)
    
    yrs <- years_by_region() |>
      dplyr::filter(region == input$region) |>
      dplyr::pull(years_sampled)
    
    yrs
  })
  
  
  
  # ===== LOCATION SUMMARY (mirrors Region Summary) =============================
  
  # (A) Build a location list (optionally filtered by region)
  # BRUVS and Dive use different location vocabularies (RLS locations are
  # not the same set of names as the BRUV "reporting_name" locations, even
  # though their regions match), so the choices offered here depend on
  # which method is currently selected in this tab.
  locations_all <- reactive({

    hab_data$hab_combined_metadata |>
      dplyr::filter(!reporting_name %in% "NA") %>%
      dplyr::pull(reporting_name) |>
      unique() |>
      sort()
  })

  rls_locations_all <- reactive({
    req(rls_data)

    rls_data$samples |>
      dplyr::filter(!is.na(location), location != "") |>
      dplyr::pull(location) |>
      unique() |>
      sort()
  })

  # (B) Populate location choices - swap vocabulary when the BRUVS / Dive
  # switch for this tab changes.
  observe({
    req(input$methodlocation)

    locs <- if (identical(input$methodlocation, "Dive")) {
      rls_locations_all()
    } else {
      locations_all()
    }

    updateSelectizeInput(
      session, "location",
      choices  = locs,
      selected = locs[1] %||% NULL,
      server   = TRUE
    )
  })
  
  # (C) Summary text for location (needs a location summaries table)
  output$location_summary_text <- renderUI({
    req(input$location)

    txt <- if (identical(input$methodlocation, "Dive")) {
      rls_location_summary_text() |>
        dplyr::filter(reporting_location == input$location) |>
        dplyr::pull(summary)
    } else {
      hab_data$locations_summaries |>
        dplyr::filter(reporting_name == input$location) |>
        dplyr::pull(summary)
    }

    if (length(txt) == 0) txt <- "Add summary text for this location."

    HTML(markdown::markdownToHTML(text = txt, fragment.only = TRUE))
  })

  # (D) Years sampled for location - branches on the Location Summary tab's
  # BRUVS / Dive switch, same pattern as years_by_region above.
  years_by_location <- reactive({
    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      rls_data$samples |>
        dplyr::mutate(
          year            = lubridate::year(sampling_event_start_date),
          reporting_name  = location
        ) |>
        dplyr::filter(!is.na(year)) |>
        dplyr::distinct(reporting_name, year) |>
        dplyr::group_by(reporting_name) |>
        dplyr::summarise(
          n_years       = dplyr::n(),
          years_sampled = paste(sort(unique(year)), collapse = ", "),
          .groups       = "drop"
        )
    } else {
      hab_data$year_dat |>
        dplyr::filter(method %in% "BRUVs") |>
        dplyr::distinct(reporting_name, year) |>
        dplyr::group_by(reporting_name) |>
        dplyr::summarise(
          n_years       = dplyr::n(),
          years_sampled = paste(sort(unique(year)), collapse = ", "),
          .groups       = "drop"
        )
    }
  })

  output$years_for_location <- renderText({
    req(input$location)

    years_by_location() |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::pull(years_sampled)
  })
  
  # Map of deployments
  # Location-level survey effort map - filters to whichever method is
  # selected in this tab's BRUVS / Dive switch (input$methodlocation).
  # RLS locations use rls_data$sites (its own location vocabulary, with
  # lat/lon columns), while BRUVS keeps using hab_combined_metadata.
  location_deployments <- reactive({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      rls_data$sites %>%
        dplyr::filter(location %in% input$location) %>%
        dplyr::rename(longitude_dd = longitude, latitude_dd = latitude)
    } else {
      deployments <- hab_data$hab_combined_metadata %>%
        dplyr::filter(reporting_name %in% input$location, method %in% "BRUVs")

      coords <- sf::st_coordinates(deployments)
      coords_df <- as.data.frame(coords)
      colnames(coords_df) <- c("longitude_dd", "latitude_dd")

      dplyr::bind_cols(deployments, coords_df)
    }
  })

  loc_min_lat <- reactive({ min(location_deployments()$latitude_dd,  na.rm = TRUE) })
  loc_min_lon <- reactive({ min(location_deployments()$longitude_dd, na.rm = TRUE) })
  loc_max_lat <- reactive({ max(location_deployments()$latitude_dd,  na.rm = TRUE) })
  loc_max_lon <- reactive({ max(location_deployments()$longitude_dd, na.rm = TRUE) })

  # Add location survey map
  output$location_survey_effort <- renderLeaflet({
    req(input$location)

    is_dive <- identical(input$methodlocation, "Dive")

    method_cols <- if (is_dive) c("Dive" = "#C600FF") else c("BRUVs" = "#004DA7")

    pts <- if (is_dive) {
      req(rls_data)
      ensure_sf_ll(rls_data$sites, lon = "longitude", lat = "latitude") %>%
        dplyr::filter(location %in% input$location)
    } else {
      ensure_sf_ll(hab_data$hab_combined_metadata) %>%
        dplyr::filter(reporting_name %in% input$location, method %in% "BRUVs")
    }

    m <- base_map(current_zoom = 7)

    # Guard against a location with no geocoded points (would otherwise
    # crash fitBounds with Inf/-Inf, and leafgl::addGlPoints on an empty
    # geometry column).
    if (nrow(pts) > 0 &&
        is.finite(loc_min_lon()) && is.finite(loc_min_lat()) &&
        is.finite(loc_max_lon()) && is.finite(loc_max_lat())) {
      m <- m %>% fitBounds(loc_min_lon(), loc_min_lat(), loc_max_lon(), loc_max_lat())
    }

    point_fill <- if (is_dive) "#C600FF" else "#004DA7"

    if (nrow(pts) == 0) {
      warning(
        "No geocoded ", if (is_dive) "Dive" else "BRUVS",
        " points found for location '", input$location, "'."
      )
    } else {
      # Plain circle markers rather than leafgl (WebGL) points: leafgl is
      # meant for plotting thousands of points at once and has a known
      # quirk where points don't paint until the map is interacted with
      # (e.g. zoomed) right after fitBounds(). A single location's site
      # count is small enough that regular markers are both simpler and
      # more reliable here.
      m <- leaflet::addCircleMarkers(
        m, data = pts,
        radius = 6, fillColor = point_fill, fillOpacity = 1,
        weight = 1, color = "black", popup = pts$popup,
        group = "Sampling locations",
        options = leaflet::pathOptions(pane = "points")
      )
    }

    leaflet::addLegend(
      m,
      "topright",
      colors  = unname(method_cols),
      labels  = names(method_cols),
      title   = "Survey method",
      opacity = 1,
      group   = "Sampling locations",
      layerId = "methodLegend"
    ) %>%
      leaflet::hideGroup("Australian Marine Parks")
  })
  
  get_metric_plot_location <- function(metric_id, title_lab, wrap_width = 22, chosen_location) {
    
    txt <- hab_data$impact_data_location |>
      dplyr::filter(reporting_name == chosen_location, impact_metric == metric_id) |>
      dplyr::pull(impact)
    
    if (length(txt) == 0 || is.na(txt) || txt == "Surveys incomplete") {
      return(no_data_plot(stringr::str_wrap(title_lab, wrap_width)))
    }
    
    half_donut_with_dial(values = c(1, 1, 1), mode = "absolute", status = txt) +
      labs(title = stringr::str_wrap(title_lab, width = wrap_width)) +
      theme(
        plot.title  = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  make_impact_gauges_location <- function(location_name) {
    
    overall_status <- hab_data$overall_impact_location |>
      dplyr::filter(reporting_name == location_name) |>
      dplyr::pull(overall_impact)
    
    p1 <- get_metric_plot_location("species_richness",         "Species richness",                 chosen_location = location_name)
    p2 <- get_metric_plot_location("total_abundance",          "Total abundance",                  chosen_location = location_name)
    p3 <- get_metric_plot_location("shark_ray_richness",       "Shark and ray richness",           chosen_location = location_name)
    p4 <- get_metric_plot_location("reef_associated_richness", "Reef associated species richness", chosen_location = location_name)
    p5 <- get_metric_plot_location("fish_200_abundance",       "Fish > 200 mm abundance",          chosen_location = location_name)
    p6 <- get_metric_plot_location("thamnaconus_degeni",           "Bluefin leatherjacket displacement*",          chosen_location = location_name)
    # p7 <- get_metric_plot_location("shannon_diversity",        "Shannon diversity",          chosen_location = location_name)
    
    (p1 | p2 | p3) / (p4 | p5 | p6 #| 
                      #p7
    )
  }
  
  make_overall_impact_gauge_location <- function(location_name) {
    
    message(location_name)
    
    p1 <- get_metric_plot_location("shannon_diversity",        "Shannon diversity index",          chosen_location = location_name)
    p1
  }
  
  output$location_overall_impact_gauge <- renderPlot({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      # See the matching comment on output$overall_impact_gauge (Region
      # Summary) - same M1 fish Shannon diversity convention.
      return(
        get_metric_plot_rls(
          rls_data$pct_change_location, "spatial_group", input$location,
          this_metric_id = "m1_fish_shannon",
          title_lab = "M1 fish Shannon diversity index"
        )
      )
    }

    make_overall_impact_gauge_location(input$location)
  })

  output$location_impact_gauges <- renderPlot({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(make_impact_gauges_rls(rls_data$pct_change_location, "spatial_group", input$location))
    }

    make_impact_gauges_location(input$location)
  }, height = function() {
    if (identical(input$methodlocation, "Dive")) 780 else 350
  })
  
  output$location_change_table <- renderUI({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      required_cols <- c("spatial_group", "comparison_type", "metric", "change_overall", "change_fished", "change_no_take")
      if (nrow(rls_data$pct_change_location_wide) == 0 ||
          !all(required_cols %in% names(rls_data$pct_change_location_wide))) {
        return(tags$em("No percentage-change data available for this location."))
      }

      df <- rls_data$pct_change_location_wide %>%
        dplyr::filter(spatial_group == input$location, comparison_type == "period") %>%
        dplyr::select(
          Metric = metric,
          'Change Overall' = change_overall,
          'Change Fished'  = change_fished,
          'Change No-take' = change_no_take
        )

      if (nrow(df) == 0) {
        return(tags$em("No percentage-change data available for this location."))
      }

      out_overall <- rls_fmt_pct_change_html(df$`Change Overall`)
      out_fished  <- rls_fmt_pct_change_html(df$`Change Fished`)
      out_notake  <- rls_fmt_pct_change_html(df$`Change No-take`)

      return(
        tags$table(
          class = "table table-sm hab-table",
          tags$thead(
            tags$tr(
              tags$th("Metric"),
              tags$th("Change Overall"),
              tags$th("Change Fished"),
              tags$th("Change No-take")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                tags$td(df$Metric[i]),
                tags$td(HTML(out_overall[i])),
                tags$td(HTML(out_fished[i])),
                tags$td(HTML(out_notake[i]))
              )
            })
          )
        )
      )
    }

    df <- hab_metric_change_location |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::select(
        Metric = impact_metric,
        'Change Overall'= percentage_change,
        'Change Fished' = change_fished,
        'Change No-take' = change_no_take
      )

    no_status_locations <- c("Boston Bay", "Glenelg")
    show_status <- !input$location %in% no_status_locations
    
    fmt_change <- function(vals) {
      vals_chr <- as.character(vals)
      
      is_incomplete <- grepl("Surveys incomplete", vals_chr, ignore.case = TRUE) | is.na(vals_chr)
      
      num <- suppressWarnings(as.numeric(vals_chr))
      has_num <- !is.na(num) & !is_incomplete
      
      arrows  <- ifelse(num < 0, "&#8595;", "&#8593;")
      colours <- ifelse(num <= -50, "#EB5757",
                        ifelse(num <= -20, "#D4A017", "#3B7EA1"))
      
      out <- rep("", length(vals_chr))
      out[has_num] <- sprintf(
        "<span style='color:%s; font-weight:700;'>%s %s%%</span>",
        colours[has_num],
        arrows[has_num],
        scales::number(abs(num[has_num]), accuracy = 1)
      )
      out[is_incomplete] <- "<em>Surveys incomplete</em>"
      out
    }
    
    out_overall <- fmt_change(df$`Change Overall`)
    out_fished  <- fmt_change(df$`Change Fished`)
    out_notake  <- fmt_change(df$`Change No-take`)
    
    tags$table(
      class = "table table-sm hab-table",
      
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          tags$th("Change Overall"),
          if (show_status) tags$th("Change Fished"),
          if (show_status) tags$th("Change No-take")
        )
      ),
      
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Metric[i]),
            tags$td(HTML(out_overall[i])),
            if (show_status) tags$td(HTML(out_fished[i])),
            if (show_status) tags$td(HTML(out_notake[i]))
          )
        })
      )
    )
    
  })
  
  output$location_change_table_split <- renderUI({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      required_cols <- c("spatial_group", "comparison_type", "metric", "comparison_period", "change_overall")
      if (nrow(rls_data$pct_change_location_wide) == 0 ||
          !all(required_cols %in% names(rls_data$pct_change_location_wide))) {
        return(tags$em("No split-bloom-period percentage-change data available for this location."))
      }

      df <- rls_data$pct_change_location_wide %>%
        dplyr::filter(spatial_group == input$location, comparison_type == "period_split") %>%
        dplyr::select(
          Metric = metric,
          Period = comparison_period,
          change_overall
        )

      if (nrow(df) == 0) {
        return(tags$em("No split-bloom-period percentage-change data available for this location."))
      }

      df <- df %>%
        tidyr::pivot_wider(names_from = Period, values_from = change_overall)

      period_cols <- setdiff(names(df), "Metric")
      out_by_col  <- stats::setNames(
        lapply(period_cols, function(col) rls_fmt_pct_change_html(df[[col]])),
        period_cols
      )

      return(
        tags$table(
          class = "table table-sm hab-table",
          tags$thead(
            tags$tr(
              tags$th("Metric"),
              lapply(period_cols, tags$th)
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              tags$tr(
                tags$td(df$Metric[i]),
                lapply(period_cols, function(col) {
                  tags$td(HTML(out_by_col[[col]][i]))
                })
              )
            })
          )
        )
      )
    }

    df <- hab_metric_change_location_split |>
      dplyr::filter(reporting_name == input$location) |>
      dplyr::select(
        Metric = impact_metric,
        Period = bloom_period,
        percentage_change
      ) |>
      tidyr::pivot_wider(
        names_from = Period,
        values_from = percentage_change
      )

    fmt_change <- function(vals) {
      vals_chr <- as.character(vals)
      
      is_incomplete <- grepl("Surveys incomplete", vals_chr, ignore.case = TRUE) | is.na(vals_chr)
      
      num <- suppressWarnings(as.numeric(vals_chr))
      has_num <- !is.na(num) & !is_incomplete
      
      arrows  <- ifelse(num < 0, "&#8595;", "&#8593;")
      colours <- ifelse(num <= -50, "#EB5757",
                        ifelse(num <= -20, "#D4A017", "#3B7EA1"))
      
      out <- rep("", length(vals_chr))
      
      out[has_num] <- sprintf(
        "<span style='color:%s; font-weight:700;'>%s %s%%</span>",
        colours[has_num],
        arrows[has_num],
        scales::number(abs(num[has_num]), accuracy = 1)
      )
      
      out[is_incomplete] <- "<em>Surveys incomplete</em>"
      
      out
    }
    
    period_cols <- setdiff(names(df), "Metric")
    
    tags$table(
      class = "table table-sm hab-table",
      
      tags$thead(
        tags$tr(
          tags$th("Metric"),
          lapply(period_cols, tags$th)
        )
      ),
      
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          tags$tr(
            tags$td(df$Metric[i]),
            lapply(period_cols, function(col) {
              tags$td(HTML(fmt_change(df[[col]][i])))
            })
          )
        })
      )
    )
  })
  
  make_top10_plot_location <- function(location_name,
                                       focal_period = c("Pre-bloom", "Bloom"),
                                       title_lab = "Common species",
                                       number_species,
                                       split_status = FALSE,
                                       facet_status = FALSE) {
    
    focal_period <- match.arg(focal_period)
    split_status <- isTRUE(split_status)
    facet_status <- isTRUE(facet_status)
    
    # Base colours (your existing theme)
    period_cols <- c(
      "Pre-bloom" = "#193b73",
      "Bloom"     = "#92bd83"
    )
    
    # Same structure as your region table, just filtered by location
    df_raw <- hab_data$location_top_species_average |>
      dplyr::filter(reporting_name == location_name)
    
    # For choosing top species, collapse over status
    df_for_top <- df_raw |>
      dplyr::group_by(reporting_name, period, display_name) |>
      dplyr::summarise(
        average = mean(average, na.rm = TRUE),
        se      = sqrt(sum(se^2, na.rm = TRUE)),  # rough pooled SE
      )
    
    # Top N species within the focal period
    top_species <- df_for_top |>
      dplyr::filter(period == focal_period) |>
      dplyr::slice_max(order_by = average,
                       n = number_species,
                       with_ties = FALSE) |>
      dplyr::pull(display_name)
    
    # Data for plotting: either split by status or averaged across status
    if (split_status) {
      plot_df <- hab_data$location_top_species_average_status |>
        dplyr::filter(reporting_name == location_name) %>% 
        dplyr::filter(display_name %in% top_species)
    } else {
      plot_df <- df_raw |>
        dplyr::filter(display_name %in% top_species)
    }
    
    # Extract sci/common and build markdown label
    plot_df <- plot_df |>
      tidyr::extract(
        display_name,
        into   = c("sci", "common"),
        regex  = "^(.*?)\\s*\\((.*?)\\)$",
        remove = FALSE
      ) |>
      dplyr::mutate(
        label = paste0("*", sci, "*<br>(", common, ")")
      )
    
    # Period order: ALWAYS Pre-bloom then Bloom
    plot_df$period <- factor(plot_df$period, levels = c("Pre-bloom", "Bloom"))
    
    # Species order: smallest at bottom, biggest at top for focal period
    species_order <- plot_df |>
      dplyr::filter(period == focal_period) |>
      dplyr::arrange(average) |>
      dplyr::pull(label) |>
      unique()
    
    plot_df$label <- factor(plot_df$label, levels = species_order)
    
    # Arrange rows so dodging is stable
    plot_df <- plot_df |>
      dplyr::arrange(label, period, dplyr::across(dplyr::any_of("status")))
    
    dodge <- position_dodge(width = 0.8)
    
    # ============================
    #  A) split_status & no facet
    # ============================
    if (split_status && !facet_status) {
      
      # Build combined period:status variable
      plot_df <- plot_df |>
        dplyr::mutate(
          period_status = interaction(period, status, sep = ": ", drop = TRUE)
        )
      
      plot_df$period_status <- droplevels(plot_df$period_status)
      
      # Build palette dynamically from the actual levels
      status_alpha <- 0.45
      ps_levels    <- levels(plot_df$period_status)
      
      combo_cols_vec <- sapply(ps_levels, function(ps) {
        # split "Pre-bloom: Fished" into c("Pre-bloom", "Fished")
        parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
        per   <- parts[1]
        stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
        base_col <- unname(period_cols[per])
        if (!is.na(stat) && stat == "Fished") {
          scales::alpha(base_col, status_alpha)
        } else {
          base_col
        }
      })
      
      combo_cols <- setNames(combo_cols_vec, ps_levels)
      
      p <- ggplot(
        plot_df,
        aes(
          x    = average,
          y    = label,
          fill = period_status
        )
      ) +
        geom_col(position = dodge) +
        geom_errorbarh(
          aes(
            xmin = average - se,
            xmax = average + se
          ),
          position = dodge,
          height   = 0.3
        ) +
        labs(
          x     = "Average abundance per BRUV",
          y     = NULL,
          title = title_lab,
          fill  = NULL
        ) +
        scale_fill_manual(values = combo_cols)
      
    } else {
      # =======================================
      #  B) non-split OR split + facet
      # =======================================
      
      if (split_status && facet_status) {
        # Build period_status for colour mapping
        plot_df <- plot_df |>
          dplyr::mutate(
            period_status = interaction(period, status, sep = ": ", drop = TRUE)
          )
        
        plot_df$period_status <- droplevels(plot_df$period_status)
        
        status_alpha <- 0.45
        ps_levels    <- levels(plot_df$period_status)
        
        combo_cols_vec <- sapply(ps_levels, function(ps) {
          parts <- strsplit(ps, ": ", fixed = TRUE)[[1]]
          per   <- parts[1]
          stat  <- ifelse(length(parts) > 1, parts[2], NA_character_)
          base_col <- unname(period_cols[per])
          if (!is.na(stat) && stat == "Fished") {
            scales::alpha(base_col, status_alpha)
          } else {
            base_col
          }
        })
        
        combo_cols <- setNames(combo_cols_vec, ps_levels)
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period_status
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          facet_wrap(~ status, nrow = 1) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = combo_cols)
        
      } else {
        # NOT split, NOT facet → original 2-colour period-only plot
        
        p <- ggplot(
          plot_df,
          aes(
            x    = average,
            y    = label,
            fill = period
          )
        ) +
          geom_col(position = dodge) +
          geom_errorbarh(
            aes(
              xmin = average - se,
              xmax = average + se
            ),
            position = dodge,
            height   = 0.3
          ) +
          labs(
            x     = "Average abundance per BRUV",
            y     = NULL,
            title = title_lab,
            fill  = NULL
          ) +
          scale_fill_manual(values = period_cols)
      }
    }
    
    # Shared scales / theme
    p +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.y     = ggtext::element_markdown(size = 12)
      )+
      plot_theme #+ scale_y_continuous(expand = expansion(mult = c(0, 0)))
  }
  
  output$location_common_pre <- renderPlot({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(
        plot_top_taxa_rls(
          selection_df         = rls_data$top_occurrence_abundance_selection,
          spatial_level_value  = "location",
          group_value          = input$location,
          focal_period         = "Pre-bloom",
          title_lab            = "Most common species pre-bloom",
          number_species       = input$location_number_species
        )
      )
    }

    make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )
  }, height = function() if (identical(input$methodlocation, "Dive")) 950 else 550)

  output$location_common_post <- renderPlot({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(
        plot_top_taxa_rls(
          selection_df         = rls_data$top_occurrence_abundance_selection,
          spatial_level_value  = "location",
          group_value          = input$location,
          focal_period         = "Post-bloom",
          title_lab            = "Most common species post-bloom",
          number_species       = input$location_number_species
        )
      )
    }

    make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Bloom",
      title_lab      = "Most common species during bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )
  }, height = function() if (identical(input$methodlocation, "Dive")) 950 else 550)

  # Downloads -----
  
  location_common_results <- reactive({

    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(
        rls_data$top_occurrence_abundance_selection %>%
          dplyr::filter(spatial_level == "location", group_name == input$location) %>%
          dplyr::mutate(
            average_abundance = round(average_abundance, digits = 3),
            abundance_se       = round(abundance_se, digits = 3)
          )
      )
    }

    hab_data$location_top_species_average |>
      dplyr::filter(reporting_name == input$location)  %>%
      dplyr::mutate(
        average = clean_number(average),
        se = clean_number(se)
      )  %>%
      dplyr::mutate(average = round(average, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))

  })

  location_common_results_status <- reactive({

    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      # RLS's top-abundance data has no "status" (Fished / No-take) split -
      # nothing to return for this download while Dive is selected.
      validate(need(FALSE, "Not available for Dive data."))
    }

    hab_data$location_top_species_average_status |>
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(
        average = clean_number(average),
        se = clean_number(se)
      )  %>%
      dplyr::mutate(average = round(average, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))

  })
  
  location_common_results_name <- reactive({
    
    req(input$location)
    
    paste("Common_species", input$location, sep = "_")
    
  })
  
  output$location_common_download_results <- downloadHandler(
    filename = function() {
      paste0(location_common_results_name(), "_average_abundances", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(location_common_results(), file)
    }
  )
  
  output$location_common_download_results_status <- downloadHandler(
    filename = function() {
      paste0(location_common_results_name(), "_status_average_abundances", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(location_common_results_status(), file)
    }
  )
  
  
  location_common_plots <- reactive({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      p1 <- plot_top_taxa_rls(
        selection_df        = rls_data$top_occurrence_abundance_selection,
        spatial_level_value = "location",
        group_value         = input$location,
        focal_period        = "Pre-bloom",
        title_lab           = "Most common species pre-bloom",
        number_species      = input$location_number_species
      )

      p2 <- plot_top_taxa_rls(
        selection_df        = rls_data$top_occurrence_abundance_selection,
        spatial_level_value = "location",
        group_value         = input$location,
        focal_period        = "Post-bloom",
        title_lab           = "Most common species post-bloom",
        number_species      = input$location_number_species
      )

      # Pre/post side by side, each already stacking its own RLS methods
      # top to bottom - matches the on-screen layout.
      return(p1 | p2)
    }

    p1 <- make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Pre-bloom",
      title_lab      = "Most common species pre-bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )

    p2 <- make_top10_plot_location(
      location_name  = input$location,
      focal_period   = "Bloom",
      title_lab      = "Most common species during bloom",
      number_species = input$location_number_species,
      split_status   = input$location_species_status,
      facet_status   = input$location_species_facet
    )

    p1 + p2

  })

  output$location_common_download_plot <- downloadHandler(
    filename = function() {
      paste0(location_common_results_name(), "_most_common_species_plots", "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      is_dive <- identical(input$methodlocation, "Dive")
      ggplot2::ggsave(
        filename = file,
        plot = location_common_plots(),
        width  = if (is_dive) 14 else 12,
        height = if (is_dive) 12 else 6,
        dpi = 300
      )
    }
  )
  
  # ===== RLS (Dive) modelled-means ("Explore indicators") =================
  #
  # GLMMs are fitted per LOCATION only (script 11 / script 15's diagnostics
  # confirm there's no region-level modelled-means row), so this only ever
  # shows up on the Location Summary tab - see location_tabset below and
  # the Dive branch added to region_tabset above.
  #
  # This reuses the SAME generic UI helpers BRUV's own tabset uses
  # (metric_tab_body_ui() / metric_plot_with_downloads() /
  # add_metric_downloads() / metric_plot_id() / metric_plot_type_input_id())
  # with prefix "rls_loc". None of the 14 RLS metric ids match a named
  # case inside metric_tab_body_ui()'s switch(), so every one of them
  # falls into its default layout: a period plot + a period-by-status plot
  # side by side, plus a temporal trend plot underneath - exactly the
  # "main" / "status" / "year" shape this loop builds.
  #
  # Unlike BRUV (whose *_summary tables are plain observed means - see
  # richness_status_results() above), RLS already has real GLMM
  # predictions (script 11) with mean/se/lower/upper columns. So here the
  # "bars" mode plots the GLMM's modelled mean +/- 95% CI directly, and the
  # "boxplot" toggle overlays that same modelled mean on top of the raw
  # per-transect values (rls_data$samples) - mirroring BRUV's own
  # boxplot-overlays-a-mean pattern, just with a modelled mean instead of a
  # naive one.
  if (!is.null(rls_data)) {

    # Colours copied verbatim from script 11 (rls_glmm_models.R)'s own
    # `period_cols` / `status_cols` - the same script that fits the GLMMs
    # and made its own diagnostic plots of them - so the app matches "the
    # code" rather than inventing its own palette.
    rls_glmm_period_cols <- c(
      "Pre-bloom" = "#193b73",
      "Bloom"     = "#92bd83"
    )

    rls_glmm_status_cols <- c(
      "Fished"  = "#D98C3F",
      "No-take" = "#4FA08F"
    )

    # One tab per biological metric (metric_group), not per method x
    # metric combination - each of the three plots below is faceted by
    # facet_label (method, or invertebrate phylum for the M2-invert-only
    # abundance tab) so all methods show side by side within the tab.
    lapply(seq_len(nrow(rls_data$metric_groups)), function(i) {
      local({

        this_metric_group <- rls_data$metric_groups$metric_group[i]
        this_y_lab         <- rls_data$metric_groups$y_lab[i]
        prefix             <- "rls_loc"

        # Facet order within this tab. unique() preserves the row order
        # Brooke wrote in rls_metric_lookup (script 15) rather than
        # sorting alphabetically, so e.g. M1 fish / M2 fish / M2
        # invertebrates keeps that order even though it happens to also
        # be alphabetical.
        this_facet_levels <- unique(
          rls_data$metric_lookup$facet_label[rls_data$metric_lookup$metric_group == this_metric_group]
        )

        # ---- Raw per-transect values (for the boxplot toggle) ----
        raw_data <- reactive({
          req(input$location)
          rls_data$samples %>%
            dplyr::filter(metric_group == this_metric_group, location == input$location) %>%
            dplyr::mutate(
              period      = factor(period, levels = c("Pre-bloom", "Bloom")),
              facet_label = factor(facet_label, levels = this_facet_levels)
            )
        })

        # ---- Period (main) ----
        main_results <- reactive({
          req(input$location)
          rls_data$period_predictions %>%
            dplyr::filter(metric_group == this_metric_group, location == input$location) %>%
            dplyr::mutate(
              period      = factor(period, levels = c("Pre-bloom", "Bloom")),
              facet_label = factor(facet_label, levels = this_facet_levels)
            )
        })

        main_plot <- reactive({
          req(input$location)
          show_box <- metric_plot_type(input, prefix, this_metric_group)
          mean_se  <- main_results()

          if (show_box) {
            df <- raw_data()

            ggplot(df, aes(x = period, y = value, fill = period)) +
              geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black") +
              geom_jitter(aes(colour = period), width = 0.15, height = 0, alpha = 0.35, size = 1.2) +
              geom_pointrange(
                data = mean_se,
                aes(x = period, y = mean, ymin = lower, ymax = upper),
                inherit.aes = FALSE,
                colour = "black",
                linewidth = 0.6
              ) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_fill_manual(values = rls_glmm_period_cols, drop = FALSE) +
              scale_color_manual(values = rls_glmm_period_cols, drop = FALSE) +
              labs(x = NULL, y = this_y_lab) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "none",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

          } else {
            ggplot(mean_se, aes(x = period, y = mean, fill = period)) +
              geom_col(width = 0.6, colour = "black", alpha = 0.85) +
              geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.6) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_fill_manual(values = rls_glmm_period_cols, drop = FALSE) +
              labs(x = NULL, y = this_y_lab) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "none",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
          }
        })

        output[[metric_plot_id(prefix, this_metric_group, "main")]] <- renderPlot({
          main_plot()
        }) |>
          bindCache(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]]) |>
          bindEvent(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]])

        # ---- Period x status ----
        status_results <- reactive({
          req(input$location)
          rls_data$period_status_predictions %>%
            dplyr::filter(metric_group == this_metric_group, location == input$location) %>%
            dplyr::mutate(
              period      = factor(period, levels = c("Pre-bloom", "Bloom")),
              status      = factor(status, levels = c("Fished", "No-take")),
              facet_label = factor(facet_label, levels = this_facet_levels)
            )
        })

        status_plot <- reactive({
          req(input$location)
          show_box <- metric_plot_type(input, prefix, this_metric_group)
          mean_se  <- status_results() %>%
            dplyr::mutate(
              # %in% (not isTRUE()) because this needs to be vectorised
              # across all rows, matching script 11's own flagging logic.
              flag_label = dplyr::if_else(low_replication %in% TRUE, "*", ""),
              flag_y     = dplyr::if_else(is.finite(upper), upper, mean)
            )

          dodge <- position_dodge(width = 0.72)

          if (show_box) {
            # Coloured by status (not period) and dodged at each period, to
            # match plot_period_status_prediction() in script 11 - the same
            # function that made the GLMM's own diagnostic plots.
            df <- raw_data() %>%
              dplyr::filter(status %in% c("Fished", "No-take")) %>%
              dplyr::mutate(status = factor(status, levels = c("Fished", "No-take")))

            ggplot(df, aes(x = period, y = value, fill = status)) +
              geom_boxplot(
                position = position_dodge(width = 0.8),
                width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black"
              ) +
              geom_jitter(
                aes(colour = status),
                position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
                alpha = 0.35, size = 1.2
              ) +
              geom_pointrange(
                data = mean_se,
                aes(x = period, y = mean, ymin = lower, ymax = upper, group = status),
                position = position_dodge(width = 0.8),
                inherit.aes = FALSE,
                colour = "black",
                linewidth = 0.6
              ) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_fill_manual(values = rls_glmm_status_cols, drop = FALSE) +
              scale_color_manual(values = rls_glmm_status_cols, drop = FALSE) +
              labs(x = NULL, y = this_y_lab, fill = NULL) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "bottom",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.08)))

          } else {
            # Matches script 11's plot_period_status_prediction() exactly:
            # x = period, fill = status, status_cols, with an asterisk
            # flagging any Period x Status cell with too few sites.
            ggplot(mean_se, aes(x = period, y = mean, fill = status)) +
              geom_col(
                position = dodge, width = 0.62, colour = "black", alpha = 0.9
              ) +
              geom_errorbar(
                data = mean_se %>% dplyr::filter(is.finite(lower), is.finite(upper)),
                aes(ymin = lower, ymax = upper),
                position = dodge, width = 0.16, linewidth = 0.6
              ) +
              geom_text(
                aes(y = flag_y, label = flag_label, group = status),
                position = dodge, vjust = -0.5, size = 5
              ) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_fill_manual(values = rls_glmm_status_cols, drop = FALSE) +
              labs(x = NULL, y = this_y_lab, fill = NULL) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "bottom",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.08)))
          }
        })

        output[[metric_plot_id(prefix, this_metric_group, "status")]] <- renderPlot({
          status_plot()
        }) |>
          bindCache(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]]) |>
          bindEvent(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]])

        # ---- Temporal trend ("year") ----
        year_results <- reactive({
          req(input$location)
          rls_data$temporal_predictions %>%
            dplyr::filter(metric_group == this_metric_group, location == input$location) %>%
            dplyr::mutate(
              sampling_event_start_date = as.Date(sampling_event_start_date),
              period      = factor(period, levels = c("Pre-bloom", "Bloom")),
              facet_label = factor(facet_label, levels = this_facet_levels)
            ) %>%
            dplyr::arrange(sampling_event_start_date)
        })

        year_plot <- reactive({
          req(input$location)
          show_box <- metric_plot_type(input, prefix, this_metric_group)

          if (show_box) {
            df <- raw_data() %>%
              dplyr::mutate(sampling_event_start_date = as.Date(sampling_event_start_date))

            ggplot(
              df,
              aes(x = sampling_event_start_date, y = value, group = sampling_event_start_date, fill = period)
            ) +
              geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
              geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 1.2) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_fill_manual(values = rls_glmm_period_cols, drop = FALSE) +
              scale_color_manual(values = rls_glmm_period_cols, drop = FALSE) +
              scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
              labs(x = NULL, y = this_y_lab) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "none",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

          } else {
            df <- year_results()

            ggplot(df, aes(x = sampling_event_start_date, y = mean, fill = period)) +
              geom_col(width = 100, colour = "black", alpha = 0.85) +
              geom_errorbar(aes(ymin = lower, ymax = upper), width = 30, linewidth = 0.6) +
              facet_wrap(~facet_label, nrow = 1, scales = "free_y") +
              scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
              scale_fill_manual(values = rls_glmm_period_cols, drop = FALSE) +
              labs(x = NULL, y = this_y_lab) +
              theme_minimal(base_size = 16) +
              theme(
                legend.position  = "none",
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
              ) +
              plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
          }
        })

        output[[metric_plot_id(prefix, this_metric_group, "year")]] <- renderPlot({
          year_plot()
        }) |>
          bindCache(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]]) |>
          bindEvent(input$location, input[[metric_plot_type_input_id(prefix, this_metric_group)]])

        # ---- Downloads ----
        # width = 12 (vs. the usual 8) since these plots now have up to 3
        # facet panels side by side.
        add_metric_downloads(
          output, prefix = prefix, data_id = this_metric_group, plot_id = "main",
          results_reactive = main_results, raw_reactive = raw_data,
          plot_reactive = main_plot, download_label_reactive = reactive(input$location),
          width = 12, height = 5
        )

        add_metric_downloads(
          output, prefix = prefix, data_id = this_metric_group, plot_id = "status",
          results_reactive = status_results, raw_reactive = raw_data,
          plot_reactive = status_plot, download_label_reactive = reactive(input$location),
          width = 12, height = 5
        )

        add_metric_downloads(
          output, prefix = prefix, data_id = this_metric_group, plot_id = "year",
          results_reactive = year_results, raw_reactive = raw_data,
          plot_reactive = year_plot, download_label_reactive = reactive(input$location),
          width = 12, height = 6
        )

      })
    })
  }

  output$location_tabset <- renderUI({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      # One tab per biological metric (metric_group) rather than per
      # method x metric combination - see rls_metric_group_tab_body_ui()
      # and the GLMM reactive loop above for how the 3 methods (or 3
      # invertebrate phyla) get faceted together within each tab.
      rls_metric_group_defs <- setNames(
        rls_data$metric_groups$metric_group_label,
        rls_data$metric_groups$metric_group
      )

      return(
        bslib::navset_card_tab(
          !!!lapply(names(rls_metric_group_defs), function(id) {
            bslib::nav(
              title = rls_metric_group_defs[[id]],
              rls_metric_group_tab_body_ui(id, prefix = "rls_loc")
            )
          })
        )
      )
    }

    bslib::navset_card_tab(
      !!!lapply(names(metric_defs), function(id) {
        bslib::nav(
          title = metric_defs[[id]],
          metric_tab_body_ui(id, prefix = "loc")
        )
      })
    )
  })
  
  output$loc_plot_richness_main <- renderPlot({
    req(input$location)
    
    df <- hab_data$species_richness_samples %>% 
      dplyr::filter(reporting_name == input$location)
    
    df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
    
    mean_se <- hab_data$species_richness_summary_location %>% 
      dplyr::filter(reporting_name == input$location)
    
    ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
      geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black") +
      geom_jitter(aes(colour = period), width = 0.15, height = 0, alpha = 0.35, size = 1.2) +
      geom_pointrange(
        data = mean_se,
        aes(x = period, y = mean, ymin = mean - se, ymax = mean + se),
        inherit.aes = FALSE, colour = "black", linewidth = 0.6
      ) +
      scale_fill_manual(values = metric_period_cols) +
      scale_color_manual(values = metric_period_cols) +
      labs(x = NULL, y = metric_y_lab[["richness"]], subtitle = input$location) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
      plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
  }) |>
    bindCache(input$location) |>
    bindEvent(input$location)
  
  # RICHNESS (LOCATION) --------------------
  richness_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$species_richness_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) #%>%
    # group_by(campaignid) %>%
    # mutate(
    #   campaign_date = min(date),
    # ) %>%
    # ungroup()
  })
  
  richness_main_results_location <- reactive({
    req(input$location)
    
    hab_data$species_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  richness_status_results_location <- reactive({
    richness_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  richness_summary_year_location <- reactive({
    hab_data$species_richness_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      # group_by(campaignid) %>%
      # mutate(campaign_date = min(date)) %>%
      # ungroup() %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  
  # RICHNESS (LOCATION): main plot --------------------
  richness_main_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "richness")
    
    if (show_box) {
      df <- richness_main_raw_location()
      mean_se <- richness_main_results_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 0.15, height = 0, alpha = 0.35, size = 1.2) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean, ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      df <- richness_main_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(width = 0.6, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 0.6) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = paste0(input$location, ": Average species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$loc_plot_richness_main <- renderPlot({
    richness_main_plot_location()
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "richness")]])
  
  
  # RICHNESS (LOCATION):  status plot --------------------
  richness_status_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "richness")
    
    if (show_box) {
      
      df <- richness_main_raw_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]],
          # subtitle = paste0(input$location, ": Species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()+
            plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
        )
      
    } else {
      
      df <- richness_status_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = paste0(input$location, ": Average species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
  })
  
  # RICHNESS (LOCATION): Year plot --------------------
  richness_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "richness")
    
    if (show_box) {
      
      df <- richness_main_raw_location()
      mean_se <- richness_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = n_species_sample, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- richness_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["richness"]]#,
          # subtitle = paste0(input$location, ": Average species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  
  output$loc_plot_richness_main <- renderPlot({
    richness_main_plot_location()
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "richness")]])
  
  output$loc_plot_richness_status <- renderPlot({
    
    richness_status_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "richness")]])
  
  output$loc_plot_richness_year <- renderPlot({
    
    richness_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "richness")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "richness",
    plot_id = "main",
    results_reactive = richness_main_results_location,
    raw_reactive = richness_main_raw_location,
    plot_reactive = richness_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "richness",
    plot_id = "status",
    results_reactive = richness_status_results_location,
    raw_reactive = richness_main_raw_location,
    plot_reactive = richness_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # TOTAL ABUNDANCE (LOCATION) ------------
  total_abundance_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$total_abundance_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) #%>%
    # group_by(campaignid) %>%
    # mutate(campaign_date = min(date)) %>%
    # ungroup()
  })
  
  total_abundance_main_results_location <- reactive({
    req(input$location)
    
    hab_data$total_abundance_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  total_abundance_status_results_location <- reactive({
    total_abundance_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        n = sum(!is.na(total_abundance_sample)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  total_abundance_summary_year_location <- reactive({
    hab_data$total_abundance_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      # group_by(campaignid) %>%
      # mutate(campaign_date = min(date)) %>%
      # ungroup() %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se   = sd(total_abundance_sample, na.rm = TRUE) / sqrt(sum(!is.na(total_abundance_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # TOTAL ABUNDANCE (LOCATION): main plot ------------
  total_abundance_main_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "total_abundance")
    
    if (show_box) {
      
      # Filter for this region
      df <- total_abundance_main_raw_location()
      mean_se <- total_abundance_main_results_location()
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean,
              ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- total_abundance_main_results_location()
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = paste0(input$location, ": Average total abundance per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$loc_plot_total_abundance_main <- renderPlot({
    total_abundance_main_plot_location()
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]])
  
  # TOTAL ABUNDANCE (LOCATION): status plot ------------
  total_abundance_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "total_abundance")
    
    if (show_box) {
      
      df <- total_abundance_main_raw_location()
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = paste0(input$location, ": Total abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    } else {
      
      df <- total_abundance_status_results_location()
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = paste0(input$location,
          # ": Average total abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  
  output$loc_plot_total_abundance_status <- renderPlot({
    
    total_abundance_status_plot_location()
    
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]])
  
  # TOTAL ABUNDANCE (LOCATION): Year plot --------------------
  total_abundance_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "total_abundance")
    
    if (show_box) {
      
      df <- total_abundance_main_raw_location()
      mean_se <- total_abundance_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = total_abundance_sample, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- total_abundance_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["total_abundance"]]#,
          # subtitle = paste0(input$location, ": Average total abundance per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$loc_plot_total_abundance_year <- renderPlot({
    
    total_abundance_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "total_abundance")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "total_abundance",
    plot_id = "main",
    results_reactive = total_abundance_main_results_location,
    raw_reactive = total_abundance_main_raw_location,
    plot_reactive = total_abundance_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "total_abundance",
    plot_id = "status",
    results_reactive = total_abundance_status_results_location,
    raw_reactive = total_abundance_main_raw_location,
    plot_reactive = total_abundance_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  
  # SHARK & RAYS -------
  shark_ray_richness_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$shark_ray_richness_samples_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) #%>%
    # group_by(campaignid) %>%
    # mutate(campaign_date = min(date)) %>%
    # ungroup()
  })
  
  shark_ray_richness_main_results_location <- reactive({
    req(input$location)
    
    hab_data$shark_ray_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  shark_ray_richness_status_results_location <- reactive({
    shark_ray_richness_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  
  shark_ray_richness_summary_year_location <- reactive({
    hab_data$shark_ray_richness_samples_location %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      # group_by(campaignid) %>%
      # mutate(campaign_date = min(date)) %>%
      # ungroup() %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # SHARK & RAYS: main plot -----
  shark_ray_richness_main_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shark_ray_richness")
    
    if (show_box) {
      
      df <- shark_ray_richness_main_raw_location()
      
      mean_se <- shark_ray_richness_main_results_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shark_ray_richness_main_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = paste0(input$location, ": Average shark and ray species richness per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$loc_plot_shark_ray_richness_main <- renderPlot({
    
    shark_ray_richness_main_plot_location()
    
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]])
  
  # SHARK & RAYS: status plot -----
  
  shark_ray_richness_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shark_ray_richness")
    
    if (show_box) {
      df <- shark_ray_richness_main_raw_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = paste(input$location, "— Shark & ray species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shark_ray_richness_status_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = paste0(input$location, ": Average shark & ray species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$loc_plot_shark_ray_richness_status <- renderPlot({
    
    shark_ray_richness_status_plot_location()
    
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]])
  
  # SHARK & RAYS (LOCATION): Year plot --------------------
  shark_ray_richness_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shark_ray_richness")
    
    if (show_box) {
      
      df <- shark_ray_richness_main_raw_location()
      mean_se <- shark_ray_richness_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = n_species_sample, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shark_ray_richness_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["sharks_rays"]]#,
          # subtitle = paste0(input$location, ": Average shark and ray species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$loc_plot_shark_ray_richness_year <- renderPlot({
    
    shark_ray_richness_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shark_ray_richness")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "shark_ray_richness",
    plot_id = "main",
    results_reactive = shark_ray_richness_main_results_location,
    raw_reactive = shark_ray_richness_main_raw_location,
    plot_reactive = shark_ray_richness_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "shark_ray_richness",
    plot_id = "status",
    results_reactive = shark_ray_richness_status_results_location,
    raw_reactive = shark_ray_richness_main_raw_location,
    plot_reactive = shark_ray_richness_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # REEF_ASSOCIATED ----
  reef_associated_richness_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$reef_associated_richness_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))# %>%
    # group_by(campaignid) %>%
    # mutate(campaign_date = min(date)) %>%
    # ungroup()
  })
  
  reef_associated_richness_main_results_location <- reactive({
    req(input$location)
    
    hab_data$reef_associated_richness_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  reef_associated_richness_status_results_location <- reactive({
    reef_associated_richness_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se = sd(n_species_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(n_species_sample))),
        n = sum(!is.na(n_species_sample)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  reef_associated_richness_summary_year_location <- reactive({
    hab_data$reef_associated_richness_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      # group_by(campaignid) %>%
      # mutate(campaign_date = min(date)) %>%
      # ungroup() %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(n_species_sample, na.rm = TRUE),
        se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # REEF-ASSOCIATED: main plot -----
  reef_associated_richness_main_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "reef_associated_richness")
    
    if (show_box) {
      
      df <- reef_associated_richness_main_raw_location()
      
      mean_se <- reef_associated_richness_main_results_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- reef_associated_richness_main_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = paste0(input$location, ": Average reef associated species richness per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  
  output$loc_plot_reef_associated_richness_main <- renderPlot({
    
    reef_associated_richness_main_plot_location()
    
  })   |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]])
  
  # REEF-ASSOCIATED: status plot ---------------
  reef_associated_richness_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "reef_associated_richness")
    
    if (show_box) {
      
      df <- reef_associated_richness_main_raw_location()
      
      ggplot(df, aes(x = period, y = n_species_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = paste0(input$location, ": Reef-associated species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- reef_associated_richness_status_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = paste0(input$location, ": Average reef-associated species richness per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$loc_plot_reef_associated_richness_status <- renderPlot({
    reef_associated_richness_status_plot_location()
  })    |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]])
  
  # REEF-ASSOCIATED: Year plot ---------------
  reef_associated_richness_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "reef_associated_richness")
    
    if (show_box) {
      
      df <- reef_associated_richness_main_raw_location()
      mean_se <- reef_associated_richness_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = n_species_sample, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", 
              panel.grid.minor = element_blank(),           
              panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- reef_associated_richness_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["reef_associated_richness"]]#,
          # subtitle = paste0(input$location, ": Average reef associated species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", 
              panel.grid.minor = element_blank(),           
              panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  
  output$loc_plot_reef_associated_richness_year <- renderPlot({
    
    reef_associated_richness_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "reef_associated_richness")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "reef_associated_richness",
    plot_id = "main",
    results_reactive = reef_associated_richness_main_results_location,
    raw_reactive = reef_associated_richness_main_raw_location,
    plot_reactive = reef_associated_richness_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "reef_associated_richness",
    plot_id = "status",
    results_reactive = reef_associated_richness_status_results_location,
    raw_reactive = reef_associated_richness_main_raw_location,
    plot_reactive = reef_associated_richness_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # LARGE FISH -----
  fish_200_abundance_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$fish_200_abundance_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
  })
  
  fish_200_abundance_main_results_location <- reactive({
    req(input$location)
    
    hab_data$fish_200_abundance_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  fish_200_abundance_status_results_location <- reactive({
    fish_200_abundance_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se = sd(total_abundance_sample, na.rm = TRUE) /
          sqrt(sum(!is.na(total_abundance_sample))),
        n = sum(!is.na(total_abundance_sample)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  fish_200_abundance_summary_year_location <- reactive({
    hab_data$fish_200_abundance_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(total_abundance_sample, na.rm = TRUE),
        se   = sd(total_abundance_sample, na.rm = TRUE) / sqrt(sum(!is.na(total_abundance_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # LARGE FISH: main plot ------------
  fish_200_abundance_main_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "fish_200_abundance")
    
    if (show_box) {
      
      # Filter for this region
      df <- fish_200_abundance_main_raw_location()
      
      mean_se <- fish_200_abundance_main_results_location()
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(x = period, y = mean,
              ymin = mean - se, ymax = mean + se),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- fish_200_abundance_main_results_location()
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = paste0(input$location, ": Average total abundance per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$loc_plot_fish_200_abundance_main <- renderPlot({
    
    fish_200_abundance_main_plot_location()
    
  })     |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]])
  
  # ---------- LARGE FISH: status plot --------------------
  fish_200_abundance_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "fish_200_abundance")
    
    if (show_box) {
      df <- fish_200_abundance_main_raw_location()
      
      ggplot(df, aes(x = period, y = total_abundance_sample, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = paste0(input$location, ": Large fish (>200 mm) abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- fish_200_abundance_status_results_location()
      
      ggplot(df,
             aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = paste0(input$location, ": Average large fish (>200 mm) abundance per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    }
    
  })
  
  output$loc_plot_fish_200_abundance_status <- renderPlot({
    
    fish_200_abundance_status_plot_location()
    
  })     |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]])
  
  # LARGE FISH: Year plot ---------------
  fish_200_abundance_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "fish_200_abundance")
    
    if (show_box) {
      
      df <- fish_200_abundance_main_raw_location()
      mean_se <- fish_200_abundance_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = total_abundance_sample, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- fish_200_abundance_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["large_fish"]]#,
          # subtitle = paste0(input$location, ": Average reef associated species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  
  output$loc_plot_fish_200_abundance_year <- renderPlot({
    
    fish_200_abundance_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "fish_200_abundance")]])
  
  
  # Downloads -----
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "fish_200_abundance",
    plot_id = "main",
    results_reactive = fish_200_abundance_main_results_location,
    raw_reactive = fish_200_abundance_main_raw_location,
    plot_reactive = fish_200_abundance_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "fish_200_abundance",
    plot_id = "status",
    results_reactive = fish_200_abundance_status_results_location,
    raw_reactive = fish_200_abundance_main_raw_location,
    plot_reactive = fish_200_abundance_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # SHANNON DIVERSITY -------
  shannon_diversity_main_raw_location <- reactive({
    req(input$location)
    
    hab_data$shannon_diversity_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      # group_by(campaignid) %>%
      # mutate(campaign_date = min(date)) %>%
      # ungroup() %>%
      dplyr::mutate(shannon = round(shannon, digits = 3))
    
  })
  
  shannon_diversity_main_results_location <- reactive({
    req(input$location)
    
    hab_data$shannon_diversity_summary_location %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  shannon_diversity_status_results_location <- reactive({
    shannon_diversity_main_raw_location() %>%
      dplyr::group_by(period, status) %>%
      dplyr::summarise(
        mean = mean(shannon , na.rm = TRUE),
        se = sd(shannon , na.rm = TRUE) /
          sqrt(sum(!is.na(shannon ))),
        n = sum(!is.na(shannon )),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  shannon_diversity_summary_year_location <- reactive({
    hab_data$shannon_diversity_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period) %>%
      dplyr::summarise(
        mean = mean(shannon, na.rm = TRUE),
        se   = sd(shannon, na.rm = TRUE) / sqrt(sum(!is.na(shannon))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # SHANNON DIVERSITY: main plot -----
  shannon_diversity_main_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shannon_diversity")
    
    if (show_box) {
      
      df <- shannon_diversity_main_raw_location()
      
      mean_se <- shannon_diversity_main_results_location()
      
      ggplot(df, aes(x = period, y = shannon, fill = period)) +
        # boxplot (median + IQR + whiskers)
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        # raw points
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        # mean ± SE
        geom_pointrange(
          data = mean_se,
          aes(
            x    = period,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se
          ),
          inherit.aes = FALSE,
          colour = "black",
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shannon_diversity_main_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        # mean bar
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        # # mean ± SE
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = paste0(input$location, ": Average shannon diversity per sample")
        ) +
        # facet_wrap(~ zone) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",        # both bars already coloured by period
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  
  output$loc_plot_shannon_diversity_main <- renderPlot({
    
    shannon_diversity_main_plot_location()
    
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]])
  
  # SHANNON DIVERSITY: status plot -----
  
  shannon_diversity_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shannon_diversity")
    
    if (show_box) {
      df <- shannon_diversity_main_raw_location()
      
      ggplot(df, aes(x = period, y = shannon, fill = period)) +
        geom_boxplot(
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        
        # ⬇️ Add this
        geom_point(
          stat = "summary",
          fun = "mean",
          shape = 21,
          size = 3,
          fill = "white",
          colour = "black"
        ) +
        
        geom_jitter(
          aes(colour = period),
          width = 0.15,
          height = 0,      # <— prevents any vertical jitter
          alpha = 0.35,
          size = 1.2
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = paste0(input$location, ": shannon diversity per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shannon_diversity_status_results_location()
      
      ggplot(df, aes(x = period, y = mean, fill = period)) +
        geom_col(
          width  = 0.6,
          colour = "black",
          alpha  = 0.85
        ) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.2,
          linewidth = 0.6
        ) +
        facet_wrap(~ status, nrow = 1) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = paste0(input$location, ": Average shannon diversity per sample by status")
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "none",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
    
  })
  
  output$loc_plot_shannon_diversity_status <- renderPlot({
    
    shannon_diversity_status_plot_location()
    
  })  |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]])
  
  # Downloads ----
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "shannon_diversity",
    plot_id = "main",
    results_reactive = shannon_diversity_main_results_location,
    raw_reactive = shannon_diversity_main_raw_location,
    plot_reactive = shannon_diversity_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "shannon_diversity",
    plot_id = "status",
    results_reactive = shannon_diversity_status_results_location,
    raw_reactive = shannon_diversity_main_raw_location,
    plot_reactive = shannon_diversity_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # SHANNON DIVERSITY: Year plot ---------------
  shannon_diversity_year_plot_location <- reactive({
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "shannon_diversity")
    
    if (show_box) {
      
      df <- shannon_diversity_main_raw_location()
      mean_se <- shannon_diversity_summary_year_location()
      
      df$start_date <- as.Date(df$start_date)
      mean_se$start_date <- as.Date(mean_se$start_date)
      
      ggplot(df, aes(x = start_date, y = shannon, group = campaignid,  fill = period)) +
        geom_boxplot(width = 100, outlier.shape = NA, alpha = 0.85, colour = "black") +
        geom_jitter(aes(colour = period), width = 5, height = 0, alpha = 0.35, size = 2) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      df <- shannon_diversity_summary_year_location() %>%
        dplyr::mutate(start_date = as.Date(start_date))
      
      ggplot(df, aes(x = start_date, y = mean, group = campaignid, fill = period)) +
        geom_col(width = 100, colour = "black", alpha = 0.85) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 30, linewidth = 0.6) +
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year"
        ) +
        scale_fill_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["shannon_diversity"]]#,
          # subtitle = paste0(input$location, ": Average reef associated species richness per sample")
        ) +
        theme_minimal(base_size = 16) +
        theme(legend.position = "none", panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  
  output$loc_plot_shannon_diversity_year <- renderPlot({
    
    shannon_diversity_year_plot_location()
    
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "shannon_diversity")]])
  
  
  
  # ---------- Trophic Groups: two plots ------------
  trophic_main_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "trophic")
    
    if (show_box) {
      
      # Filter for this location
      df <- hab_data$trophic_groups_samples %>%
        dplyr::filter(reporting_name == input$location)
      
      mean_se <- hab_data$trophic_groups_summary_location %>%
        dplyr::filter(reporting_name == input$location)
      
      # Order periods
      df$period <- factor(df$period, levels = c("Pre-bloom", "Bloom"))
      mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
      
      # (Optional) order diet groups if you want a specific order
      diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet unknown")
      df$diet <- factor(df$diet, levels = diet_levels)
      mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
      
      dodge <- position_dodge(width = 0.75)
      
      ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
        geom_boxplot(
          position = dodge,
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          position = position_jitterdodge(
            jitter.width  = 0.15,
            jitter.height = 0,
            dodge.width   = 0.75
          ),
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(
            x    = diet,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se,
            group = period,
            colour = period
          ),
          position = dodge,
          inherit.aes = FALSE,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_color_manual(values = metric_period_cols) +
        labs(
          x = NULL,  # or "Diet group"
          y = metric_y_lab[["trophic"]]#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position  = "top",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      diet_levels <- names(diet_cols)
      
      # Start from the SUMMARY table (means per sample)
      mean_se <- hab_data$trophic_groups_summary_location %>%
        dplyr::filter(reporting_name == input$location) %>%
        dplyr::mutate(
          period = factor(period, levels = c("Pre-bloom", "Bloom")),
          diet   = factor(diet,   levels = diet_levels)
        )
      
      # -------- COUNT VIEW (mean-based) --------
      ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = diet_cols, drop = FALSE) +
        labs(
          x        = NULL,
          y        = metric_y_lab[["trophic"]],
          fill     = "Diet group"#,
          # subtitle = input$location
        ) +
        theme_minimal(base_size = 16) +
        # theme(panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
        # theme_minimal(base_size = 16) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          
          legend.position = "bottom",
          legend.direction = "horizontal",
          
          # smaller legend
          legend.title = element_text(size = 12),
          legend.text  = element_text(size = 10),
          
          # smaller keys and spacing
          legend.key.size = unit(0.7, "cm"),
          legend.spacing.x = unit(0.2, "cm")
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })   |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "trophic")]]) |>
    bindEvent(input$location, input[[metric_plot_type_input_id("loc", "trophic")]])
  
  
  output$loc_plot_trophic_main <- renderPlot({
    
    trophic_main_plot_location()
    
  }) 
  
  trophic_status_results_location <- reactive({
    hab_data$trophic_groups_samples %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(period, status, diet) %>%
      dplyr::summarise(
        mean = mean(n_individuals_sample , na.rm = TRUE),
        se = sd(n_individuals_sample , na.rm = TRUE) /
          sqrt(sum(!is.na(n_individuals_sample ))),
        n = sum(!is.na(n_individuals_sample )),
        .groups = "drop"
      )
  })
  
  trophic_status_plot_location <- reactive({
    
    req(input$location)
    
    show_box <- metric_plot_type(input, "loc", "trophic")
    
    if (show_box) {
      
      # Filter for this location
      df <- hab_data$trophic_groups_samples %>%
        dplyr::filter(reporting_name == input$location)
      
      mean_se <- trophic_status_results_location()
      
      # Order periods
      df$period     <- factor(df$period,     levels = c("Pre-bloom", "Bloom"))
      mean_se$period <- factor(mean_se$period, levels = c("Pre-bloom", "Bloom"))
      
      # Diet ordering
      diet_levels <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore", "Diet unknown")
      df$diet     <- factor(df$diet,     levels = diet_levels)
      mean_se$diet <- factor(mean_se$diet, levels = diet_levels)
      
      dodge <- position_dodge(width = 0.75)
      
      ggplot(df, aes(x = diet, y = n_individuals_sample, fill = period)) +
        geom_boxplot(
          position = dodge,
          width = 0.6,
          outlier.shape = NA,
          alpha = 0.85,
          colour = "black"
        ) +
        geom_jitter(
          aes(colour = period),
          position = position_jitterdodge(
            jitter.width  = 0.15,
            jitter.height = 0,
            dodge.width   = 0.75
          ),
          alpha = 0.35,
          size = 1.2
        ) +
        geom_pointrange(
          data = mean_se,
          aes(
            x    = diet,
            y    = mean,
            ymin = mean - se,
            ymax = mean + se,
            group = period,
            colour = period
          ),
          inherit.aes = FALSE,
          position = dodge,
          linewidth = 0.6
        ) +
        scale_fill_manual(values = metric_period_cols) +
        scale_colour_manual(values = metric_period_cols) +
        labs(
          x = NULL,
          y = metric_y_lab[["fish_200_abundance"]]#,
          # subtitle = input$location
        ) +
        facet_wrap(~ status) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),           panel.grid.major = element_blank()
        )+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
      
    } else {
      
      diet_levels <- names(diet_cols)
      
      # Start from the SUMMARY table (means per sample)
      mean_se <- trophic_status_results_location() %>%
        dplyr::mutate(
          period = factor(period, levels = c("Pre-bloom", "Bloom")),
          diet   = factor(diet,   levels = diet_levels)
        )
      
      # -------- COUNT VIEW (mean-based) --------
      ggplot(mean_se, aes(x = period, y = mean, fill = diet)) +
        geom_col(position = "stack") +
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = diet_cols, drop = FALSE) +
        labs(
          x        = NULL,
          y        = "Average no. species",
          fill     = "Diet group"#,
          # subtitle = input$location
        ) +
        facet_wrap(~ status) +
        theme_minimal(base_size = 16) +
        theme(panel.grid.minor = element_blank(),           panel.grid.major = element_blank())+
        plot_theme + scale_y_continuous(expand = expansion(mult = c(0, 0)))
    }
  })
  
  output$loc_plot_trophic_status <- renderPlot({
    
    trophic_status_plot_location()
    
  }) 
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "trophic",
    plot_id = "main",
    results_reactive = trophic_results_location,
    raw_reactive = trophic_results_location,
    plot_reactive = trophic_main_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "trophic",
    plot_id = "status",
    results_reactive = trophic_results_location,
    raw_reactive = trophic_results_location,
    plot_reactive = trophic_status_plot_location,
    download_label_reactive = reactive(input$location)
  )
  
  # Trophic group by year -----
  trophic_group_summary_year_location <- reactive({
    hab_data$trophic_groups_samples %>%
      dplyr::filter(!is.na(reporting_name)) %>%
      left_join(hab_data$hab_combined_metadata) %>%
      dplyr::filter(reporting_name == input$location) %>%
      dplyr::group_by(reporting_name, start_date, campaignid, period, diet) %>%
      dplyr::summarise(
        mean = mean(n_individuals_sample, na.rm = TRUE),
        se   = sd(n_individuals_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_individuals_sample))),
        num  = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(mean = round(mean, digits = 3)) %>%
      dplyr::mutate(se = round(se, digits = 3))
  })
  
  # diet_levels <- names(diet_cols)
  
  # ---------- Trophic Groups: two plots ------------
  trophic_year_plot_location <- reactive({
    
    diet_levels <- names(diet_cols)
    
    mean_se <- trophic_group_summary_year_location() %>%
      dplyr::mutate(
        period = factor(period, levels = c("Pre-bloom", "Bloom")),
        diet   = factor(diet, levels = diet_levels)
      )
    
    ggplot(mean_se, aes(x = start_date, y = mean, fill = diet)) +
      geom_col(position = "stack", width = 100) +
      scale_fill_manual(values = diet_cols, drop = FALSE) +
      labs(
        x = NULL,
        y = metric_y_lab[["trophic"]],
        fill = "Diet group"
      ) +
      theme_minimal(base_size = 16) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 10),
        legend.key.size = unit(0.7, "cm"),
        legend.spacing.x = unit(0.2, "cm")
      ) +
      plot_theme +
      scale_y_continuous(
        labels = scales::comma,
        expand = expansion(mult = c(0, 0))
      )
  }) |>
    bindCache(input$location, input[[metric_plot_type_input_id("loc", "trophic")]])
  
  
  output$loc_plot_trophic_year <- renderPlot({
    trophic_year_plot_location()
  })
  
  # Start from the SUMMARY table (means per sample)
  
  # COMBINED PLOT ----
  # main ----
  loc_plot_combined_main <- reactive({
    
    plot_theme <- theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5))
    
    p1 <- shannon_diversity_main_plot_location() +
      theme(axis.text.x = element_blank())+ plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0))) + theme(legend.title = element_blank())
    p2 <- richness_main_plot_location() +
      theme(axis.text.x = element_blank()) +plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0)))+ theme(legend.title = element_blank())
    p3 <- shark_ray_richness_main_plot_location() +
      theme(axis.text.x = element_blank())+ plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0)))+ theme(legend.title = element_blank())
    p4 <- reef_associated_richness_main_plot_location() +
      theme(axis.text.x = element_blank())+ plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0)))+ theme(legend.title = element_blank())
    p5 <- fish_200_abundance_main_plot_location()+ plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0)))+ theme(legend.title = element_blank())
    p6 <- trophic_main_plot_location() + plot_theme +  scale_y_continuous(expand = expansion(mult = c(0, 0)))
    
    # (p1 | p2 )/ (p3  | p4 ) / ( p5 | p6 ) + plot_annotation(tag_levels = 'A')
    
    (
      wrap_plots(
        (p1 | p2) /
          (p3 | p4) /
          (p5 | p6)
      ) +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = "collect")
    ) &
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.6, "cm"),
        legend.spacing.x = unit(0.1, "cm")
      )
  })
  
  output$loc_plot_combined_main <- renderPlot({
    
    loc_plot_combined_main()
    
  }) 
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "combined",
    plot_id = "main",
    results_reactive = shannon_diversity_main_results_location,
    raw_reactive = shannon_diversity_main_raw_location,
    plot_reactive = loc_plot_combined_main,
    download_label_reactive = reactive(input$location),
    height = 10
  )
  
  # By year ----
  loc_plot_combined_year <- reactive({
    
    plot_theme <- theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    p1 <- shannon_diversity_year_plot_location() +
      # theme(axis.text.x = element_blank())+
      plot_theme +
      # scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(legend.title = element_blank())
    
    p2 <- richness_year_plot_location() +
      # theme(axis.text.x = element_blank()) +
      plot_theme +  
      # scale_y_continuous(expand = expansion(mult = c(0, 0)))+ 
      theme(legend.title = element_blank())
    
    p3 <- shark_ray_richness_year_plot_location() +
      # theme(axis.text.x = element_blank())+ 
      plot_theme +  
      # scale_y_continuous(expand = expansion(mult = c(0, 0)))+ 
      theme(legend.title = element_blank())
    
    p4 <- reef_associated_richness_year_plot_location() +
      # theme(axis.text.x = element_blank())+ 
      plot_theme +  
      # scale_y_continuous(expand = expansion(mult = c(0, 0)))+ 
      theme(legend.title = element_blank())
    
    p5 <- fish_200_abundance_year_plot_location()+ 
      plot_theme +  
      # scale_y_continuous(expand = expansion(mult = c(0, 0)))+ 
      theme(legend.title = element_blank())
    
    p6 <- trophic_year_plot_location() +
      plot_theme +
      scale_y_continuous(expand = expansion(mult = c(0, 0)))
    
    # (p2 )/ (p3  | p4 ) / ( p5) + plot_annotation(tag_levels = 'A')
    
    (
      wrap_plots(
        (p1 | p2) /
          (p3 | p4) /
          (p5 | p6)
      ) +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = "collect")
    ) &
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.6, "cm"),
        legend.spacing.x = unit(0.1, "cm")
      )
  })
  
  output$loc_plot_combined_year <- renderPlot({
    
    loc_plot_combined_year()
    
  }) 
  
  add_metric_downloads(
    output,
    prefix = "loc",
    data_id = "combined",
    plot_id = "year",
    results_reactive = shannon_diversity_main_results_location,
    raw_reactive = shannon_diversity_main_raw_location,
    plot_reactive = loc_plot_combined_year,
    download_label_reactive = reactive(input$location),
    height = 10
  )
  
  # Tables -----
  
  # server.R
  campaign_table <- reactive({
    data.frame(
      No = seq_along(campaigns),
      Campaign = campaigns
    )
  })
  
  output$campaigns_table <- renderTable({
    campaign_table()
  })
  
  # Stacked plots -----
  
  
  region_stacked <- reactive({

    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)

      df_check <- rls_data$stacked_period %>%
        dplyr::filter(spatial_level == "region", group_name == input$region)

      validate(
        need(nrow(df_check) > 0, paste("No stacked plot data for:", input$region))
      )

      return(
        plot_stacked_taxa_rls(
          stacked_df           = rls_data$stacked_period,
          spatial_level_value  = "region",
          group_value          = input$region
        )
      )
    }

    df_check <- hab_data$species_stacked$plot_df %>%
      dplyr::filter(group_name == input$region)

    validate(
      need(nrow(df_check) > 0, paste("No stacked plot data for:", input$region))
    )

    plot_stacked_species(
      plot_df = hab_data$species_stacked$plot_df,
      other_labels = hab_data$species_stacked$other_labels,
      selected_name = input$region#,
      # palette = hab_data$species_palette
    )

  })
  
  output$region_stacked_plot <- renderPlot({
    region_stacked()
  }, height = function() if (identical(input$method, "Dive")) 1100 else 550)
  
  region_stacked_name <- reactive({
    
    req(input$region)
    
    paste("Stacked_assemblage", input$region, sep = "_")
    
  })
  
  region_stacked_results <- reactive({
    req(input$region)

    if (identical(input$method, "Dive")) {
      req(rls_data)
      return(
        rls_data$stacked_period %>%
          dplyr::filter(spatial_level == "region", group_name == input$region) %>%
          dplyr::mutate(percent = round(percent, digits = 3))
      )
    }

    df_check <- hab_data$species_stacked$plot_df %>%
      dplyr::filter(group_name == input$region) %>%
      dplyr::mutate(
        total_count = clean_number(total_count),
        percent = clean_number(percent)
      ) %>%
      dplyr::mutate(percent = round(percent, digits = 3))

  })
  
  output$region_stacked_download_results <- downloadHandler(
    filename = function() {
      paste0(region_stacked_name(), "_percentage_of_observations", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(region_stacked_results(), file)
    }
  )
  
  output$region_stacked_download_plot <- downloadHandler(
    filename = function() {
      paste0(region_stacked_name(), "_stacked_assemblage_plots", "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      is_dive <- identical(input$method, "Dive")
      ggplot2::ggsave(
        filename = file,
        plot = region_stacked(),
        width  = 8,
        height = if (is_dive) 13 else 5,
        dpi = 300
      )
    }
  )


  # Download location stacked plots and data -----
  location_stacked_name <- reactive({
    
    req(input$location)
    
    paste("Stacked_assemblage", input$location, sep = "_")
    
  })
  
  location_stacked_results <- reactive({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(
        rls_data$stacked_period %>%
          dplyr::filter(spatial_level == "location", group_name == input$location) %>%
          dplyr::mutate(percent = round(percent, digits = 3))
      )
    }

    df_check <- hab_data$location_species_stacked$plot_df %>%
      dplyr::filter(group_name == input$location)  %>%
      dplyr::mutate(
        total_count = clean_number(total_count),
        percent = clean_number(percent)
      ) %>%
      dplyr::mutate(percent = round(percent, digits = 3))

  })

  output$location_stacked_download_results <- downloadHandler(
    filename = function() {
      paste0(location_stacked_name(), "_percentage_of_observations", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(location_stacked_results(), file)
    }
  )

  output$location_stacked_download_plot <- downloadHandler(
    filename = function() {
      paste0(location_stacked_name(), "_stacked_assemblage_plots", "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      is_dive <- identical(input$methodlocation, "Dive")
      ggplot2::ggsave(
        filename = file,
        plot = location_stacked(),
        width  = 8,
        height = if (is_dive) 13 else 5,
        dpi = 300
      )
    }
  )

  # Location stacked plot ----

  location_stacked <- reactive({

    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      df_check <- location_stacked_results()

      validate(
        need(nrow(df_check) > 0, paste("No stacked plot data for:", input$location))
      )

      return(
        plot_stacked_taxa_rls(
          stacked_df           = rls_data$stacked_period,
          spatial_level_value  = "location",
          group_value          = input$location
        )
      )
    }

    df_check <- location_stacked_results()

    validate(
      need(nrow(df_check) > 0, paste("No stacked plot data for:", input$location))
    )

    plot_stacked_species(
      plot_df = hab_data$location_species_stacked$plot_df,
      other_labels = hab_data$location_species_stacked$other_labels,
      selected_name = input$location#,
      #palette = hab_data$species_palette
    )

  })

  output$location_stacked_plot <- renderPlot({
    location_stacked()
  }, height = function() if (identical(input$methodlocation, "Dive")) 1100 else 550)


  # Location stacked plot split by bloom----

  location_stacked_results_split <- reactive({
    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)
      return(
        rls_data$stacked_period_split %>%
          dplyr::filter(spatial_level == "location", group_name == input$location) %>%
          dplyr::mutate(percent = round(percent, digits = 3))
      )
    }

    df_check <- hab_data$location_species_stacked_split$plot_df %>%
      dplyr::filter(group_name == input$location)  %>%
      dplyr::mutate(
        total_count = clean_number(total_count),
        percent = clean_number(percent)
      ) %>%
      dplyr::mutate(percent = round(percent, digits = 3))

  })

  location_stacked_split <- reactive({

    req(input$location)

    if (identical(input$methodlocation, "Dive")) {
      req(rls_data)

      df_check <- location_stacked_results_split()

      validate(
        need(nrow(df_check) > 0, paste("No stacked plot data for:", input$location))
      )

      return(
        plot_stacked_taxa_rls(
          stacked_df           = rls_data$stacked_period_split,
          spatial_level_value  = "location",
          group_value          = input$location
        )
      )
    }

    df_check <- location_stacked_results_split()

    validate(
      need(nrow(df_check) > 0, paste("No stacked plot data for:", input$location))
    )

    plot_stacked_species(
      plot_df = hab_data$location_species_stacked_split$plot_df,
      other_labels = hab_data$location_species_stacked_split$other_labels,
      selected_name = input$location#,
      #palette = hab_data$species_palette
    )

  })

  output$location_stacked_plot_split <- renderPlot({
    location_stacked_split()
  }, height = function() if (identical(input$methodlocation, "Dive")) 1100 else 550)

  output$location_stacked_download_results_split <- downloadHandler(
    filename = function() {
      paste0(location_stacked_name(), "_percentage_of_observations_split", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(location_stacked_results_split(), file)
    }
  )

  output$location_stacked_download_plot_split <- downloadHandler(
    filename = function() {
      paste0(location_stacked_name(), "_stacked_assemblage_plots_split", "_", Sys.Date(), ".png"
      )
    },
    content = function(file) {
      is_dive <- identical(input$methodlocation, "Dive")
      ggplot2::ggsave(
        filename = file,
        plot = location_stacked_split(),
        width  = 8,
        height = if (is_dive) 13 else 6,
        dpi = 300
      )
    }
  )
}