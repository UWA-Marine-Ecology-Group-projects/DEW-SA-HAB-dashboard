#################################################################
# Concatenated GLMM prediction plots by location and survey method
#
# Creates three plot types:
#   1. Period
#   2. Period x Status
#   3. Temporal
#
# For each location, each plot type is saved separately for:
#   - M1 fish
#   - M2 fish
#   - M2 invertebrates
#
# Panel order:
#   M1 fish:
#     A. Total abundance
#     B. Species richness
#     C. B20 biomass
#     D. Shannon diversity
#
#   M2 fish:
#     A. Total abundance
#     B. Species richness
#     C. B20 biomass
#     D. Shannon diversity
#
#   M2 invertebrates:
#     A. Total abundance
#     B. Species richness
#     C. Shannon diversity
#     D. Echinodermata abundance
#     E. Arthropoda abundance
#     F. Mollusca abundance
#
# If a model/prediction is missing, a blank framed panel is retained.
# If the complete location x metric dataset contains >90% zero transects,
# the blank panel explicitly says this.
#################################################################

# ============================================================
# 0. Packages
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(stringr)
library(purrr)


# ============================================================
# 1. User settings
# ============================================================

# Change this only if your final modelling run uses a different tag.
analysis_tag <- "rls_glmm_results"

model_output_root <- file.path("model_results", analysis_tag)
plot_output_root <- file.path("plots", analysis_tag, "concatenated")

# A missing model with more than this proportion of zero transects
# will be labelled as a >90% zero response rather than a generic failure.
zero_threshold <- 0.90

# Used only for the Period x Status figure caption.
minimum_sites_per_status <- 2L

# Change to "2 years" if annual labels are too crowded.
temporal_date_breaks <- "1 year"

period_levels <- c("Pre-bloom", "Bloom")
status_levels <- c("Fished", "No-take")

period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

status_cols <- c(
  "Fished" = "#D98C3F",
  "No-take" = "#4FA08F"
)


# ============================================================
# 2. Read model outputs
# ============================================================

period_results <- readr::read_csv(
  file.path(model_output_root, "period_predictions.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Period = factor(Period, levels = period_levels),
    metric = as.character(metric),
    location = as.character(location)
  )

period_status_results <- readr::read_csv(
  file.path(model_output_root, "period_status_predictions.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Period = factor(Period, levels = period_levels),
    status = factor(status, levels = status_levels),
    metric = as.character(metric),
    location = as.character(location)
  )

temporal_results <- readr::read_csv(
  file.path(model_output_root, "temporal_predictions.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    sampling_event_start_date = as.Date(sampling_event_start_date),
    Period = factor(Period, levels = period_levels),
    metric = as.character(metric),
    location = as.character(location)
  )

model_diagnostics <- readr::read_csv(
  file.path(model_output_root, "model_diagnostics.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    metric = as.character(metric),
    location = as.character(location)
  )

# data_availability is useful because the Temporal diagnostic can exclude
# all-zero dates from the fitted model. Calculating zero proportion from the
# original availability table gives the TRUE zero proportion for the complete
# location x metric dataset.
data_availability <- readr::read_csv(
  file.path(model_output_root, "data_availability.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    metric = as.character(metric),
    location = as.character(location)
  )


# ============================================================
# 3. Overall zero proportion for every location x metric
# ============================================================

location_metric_zero_summary <- data_availability %>%
  group_by(location, metric) %>%
  summarise(
    n_transects = sum(n_transects, na.rm = TRUE),
    n_positive = sum(n_positive, na.rm = TRUE),
    prop_zero = if_else(
      n_transects > 0,
      1 - (n_positive / n_transects),
      NA_real_
    ),
    .groups = "drop"
  )


# ============================================================
# 4. Metric groupings and panel titles
# ============================================================

metric_sets <- list(
  M1 = c(
    "M1 fish total abundance",
    "M1 fish species richness",
    "M1 fish B20 biomass",
    "M1 fish Shannon diversity"
  ),
  
  M2_fish = c(
    "M2 fish total abundance",
    "M2 fish species richness",
    "M2 fish B20 biomass",
    "M2 fish Shannon diversity"
  ),
  
  M2_inverts = c(
    "M2 invertebrate total abundance",
    "M2 invertebrate species richness",
    "M2 invertebrate Shannon diversity",
    "M2 invertebrate Echinodermata abundance",
    "M2 invertebrate Arthropoda abundance",
    "M2 invertebrate Mollusca abundance"
  )
)

metric_labels <- c(
  "M1 fish total abundance" = "Total abundance",
  "M1 fish species richness" = "Species richness",
  "M1 fish B20 biomass" = "B20 biomass",
  "M1 fish Shannon diversity" = "Shannon diversity",
  
  "M2 fish total abundance" = "Total abundance",
  "M2 fish species richness" = "Species richness",
  "M2 fish B20 biomass" = "B20 biomass",
  "M2 fish Shannon diversity" = "Shannon diversity",
  
  "M2 invertebrate total abundance" = "Total abundance",
  "M2 invertebrate species richness" = "Species richness",
  "M2 invertebrate Shannon diversity" = "Shannon diversity",
  "M2 invertebrate Echinodermata abundance" = "Echinodermata abundance",
  "M2 invertebrate Arthropoda abundance" = "Arthropoda abundance",
  "M2 invertebrate Mollusca abundance" = "Mollusca abundance"
)

figure_titles <- c(
  "M1" = "M1 fish",
  "M2_fish" = "M2 fish",
  "M2_inverts" = "M2 invertebrates"
)

plot_type_titles <- c(
  "period" = "Period",
  "period_status" = "Period x Status",
  "temporal" = "Temporal"
)


# ============================================================
# 5. General helpers
# ============================================================

make_safe_filename <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}

panel_theme <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )


# Return the diagnostic row corresponding to the model that generated a plot.
# Period x Status predictions come from the Period model.
get_diagnostic_row <- function(location_name, metric_name, plot_type) {
  
  diagnostic_model_type <- if (plot_type == "temporal") {
    "Temporal"
  } else {
    "Period"
  }
  
  model_diagnostics %>%
    filter(
      location == location_name,
      metric == metric_name,
      model_type == diagnostic_model_type
    ) %>%
    slice(1)
}


get_zero_proportion <- function(location_name, metric_name) {
  
  x <- location_metric_zero_summary %>%
    filter(
      location == location_name,
      metric == metric_name
    )
  
  if (nrow(x) == 0) {
    return(NA_real_)
  }
  
  x$prop_zero[[1]]
}


# Work out what should be written in a blank placeholder panel.
get_missing_message <- function(location_name, metric_name, plot_type) {
  
  prop_zero <- get_zero_proportion(
    location_name = location_name,
    metric_name = metric_name
  )
  
  if (is.finite(prop_zero) && prop_zero > zero_threshold) {
    return(
      paste0(
        "Model not shown\n",
        round(prop_zero * 100),
        "% zero transects"
      )
    )
  }
  
  diag_row <- get_diagnostic_row(
    location_name = location_name,
    metric_name = metric_name,
    plot_type = plot_type
  )
  
  if (nrow(diag_row) == 0) {
    return("No model output available")
  }
  
  if (!is.na(diag_row$error[[1]]) && nzchar(diag_row$error[[1]])) {
    return("Model / prediction failed")
  }
  
  if (!isTRUE(diag_row$valid_model[[1]])) {
    return("Model did not pass\nconvergence checks")
  }
  
  "No predictions available"
}


make_blank_panel <- function(metric_name, message_text) {
  
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = message_text,
      size = 4.2,
      lineheight = 1.15
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(
      title = unname(metric_labels[[metric_name]])
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 12)
    )
}


# ============================================================
# 6. PERIOD panels
# ============================================================

plot_period_panel <- function(df_metric, metric_name) {
  
  ggplot(
    df_metric,
    aes(x = Period, y = estimate, fill = Period)
  ) +
    geom_col(
      width = 0.62,
      colour = "black",
      alpha = 0.9
    ) +
    geom_errorbar(
      data = df_metric %>%
        filter(is.finite(lower.CL), is.finite(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.18,
      linewidth = 0.6
    ) +
    scale_fill_manual(
      values = period_cols,
      drop = FALSE
    ) +
    labs(
      title = unname(metric_labels[[metric_name]]),
      y = "Predicted mean\n(95% CI)"
    ) +
    panel_theme +
    theme(legend.position = "none")
}


# ============================================================
# 7. PERIOD x STATUS panels
# ============================================================

plot_period_status_panel <- function(df_metric, metric_name) {
  
  plot_df <- df_metric %>%
    mutate(
      flag_label = if_else(low_replication %in% TRUE, "*", ""),
      flag_y = if_else(
        is.finite(upper.CL),
        upper.CL,
        estimate
      )
    )
  
  ggplot(
    plot_df,
    aes(x = Period, y = estimate, fill = status)
  ) +
    geom_col(
      position = position_dodge(width = 0.72),
      width = 0.62,
      colour = "black",
      alpha = 0.9
    ) +
    geom_errorbar(
      data = plot_df %>%
        filter(is.finite(lower.CL), is.finite(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      position = position_dodge(width = 0.72),
      width = 0.16,
      linewidth = 0.6
    ) +
    geom_text(
      aes(
        y = flag_y,
        label = flag_label,
        group = status
      ),
      position = position_dodge(width = 0.72),
      vjust = -0.5,
      size = 5
    ) +
    scale_fill_manual(
      values = status_cols,
      drop = FALSE
    ) +
    labs(
      title = unname(metric_labels[[metric_name]]),
      y = "Predicted mean\n(95% CI)"
    ) +
    panel_theme
}


# ============================================================
# 8. TEMPORAL panels
# ============================================================

plot_temporal_panel <- function(
    df_metric,
    metric_name,
    temporal_x_limits = NULL) {
  
  p <- ggplot(
    df_metric %>% arrange(sampling_event_start_date),
    aes(
      x = sampling_event_start_date,
      y = estimate,
      colour = Period
    )
  ) +
    geom_errorbar(
      data = df_metric %>%
        filter(is.finite(lower.CL), is.finite(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 18,
      linewidth = 0.5
    ) +
    geom_point(size = 2.4) +
    scale_colour_manual(
      values = period_cols,
      drop = FALSE
    ) +
    labs(
      title = unname(metric_labels[[metric_name]]),
      y = "Predicted mean\n(95% CI)"
    ) +
    panel_theme +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      )
    )
  
  # Give every metric within a location/method figure the same temporal span.
  if (!is.null(temporal_x_limits) &&
      length(temporal_x_limits) == 2 &&
      all(is.finite(as.numeric(temporal_x_limits)))) {
    
    p <- p +
      scale_x_date(
        limits = temporal_x_limits,
        date_breaks = temporal_date_breaks,
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.02))
      )
    
  } else {
    
    p <- p +
      scale_x_date(
        date_breaks = temporal_date_breaks,
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.02))
      )
  }
  
  p
}


# ============================================================
# 9. Create ONE metric panel, or a blank placeholder
# ============================================================

make_location_metric_panel <- function(
    location_name,
    metric_name,
    plot_type,
    temporal_x_limits = NULL) {
  
  results_object <- switch(
    plot_type,
    period = period_results,
    period_status = period_status_results,
    temporal = temporal_results,
    stop("Unknown plot_type: ", plot_type)
  )
  
  df_metric <- results_object %>%
    filter(
      location == location_name,
      metric == metric_name
    )
  
  diag_row <- get_diagnostic_row(
    location_name = location_name,
    metric_name = metric_name,
    plot_type = plot_type
  )
  
  # A valid model with predictions should be plotted even if the response
  # happens to contain >90% zeros. The >90% message is only used when the
  # model/prediction is actually missing.
  model_is_valid <- nrow(diag_row) > 0 &&
    isTRUE(diag_row$valid_model[[1]])
  
  if (nrow(df_metric) > 0 && model_is_valid) {
    
    return(
      switch(
        plot_type,
        period = plot_period_panel(
          df_metric = df_metric,
          metric_name = metric_name
        ),
        period_status = plot_period_status_panel(
          df_metric = df_metric,
          metric_name = metric_name
        ),
        temporal = plot_temporal_panel(
          df_metric = df_metric,
          metric_name = metric_name,
          temporal_x_limits = temporal_x_limits
        )
      )
    )
  }
  
  make_blank_panel(
    metric_name = metric_name,
    message_text = get_missing_message(
      location_name = location_name,
      metric_name = metric_name,
      plot_type = plot_type
    )
  )
}


# ============================================================
# 10. Assemble one complete concatenated figure
# ============================================================

make_concatenated_location_plot <- function(
    location_name,
    metric_vector,
    method_name,
    plot_type) {
  
  # For temporal figures, calculate one common date range across all
  # available metrics in this method so the panels line up in time.
  temporal_x_limits <- NULL
  
  if (plot_type == "temporal") {
    
    temporal_dates <- temporal_results %>%
      filter(
        location == location_name,
        metric %in% metric_vector,
        !is.na(sampling_event_start_date)
      ) %>%
      pull(sampling_event_start_date)
    
    if (length(temporal_dates) > 0) {
      temporal_x_limits <- range(temporal_dates, na.rm = TRUE)
    }
  }
  
  plot_list <- purrr::map(
    metric_vector,
    ~ make_location_metric_panel(
      location_name = location_name,
      metric_name = .x,
      plot_type = plot_type,
      temporal_x_limits = temporal_x_limits
    )
  )
  
  # Two columns gives A-D as a 2 x 2 layout and A-F as a 2 x 3 layout.
  combined <- patchwork::wrap_plots(
    plotlist = plot_list,
    ncol = 2,
    guides = if (plot_type %in% c("period_status", "temporal")) {
      "collect"
    } else {
      "keep"
    }
  ) +
    patchwork::plot_annotation(
      title = paste0(
        location_name,
        " - ",
        method_name,
        " - ",
        unname(plot_type_titles[[plot_type]])
      ),
      caption = if (plot_type == "period_status") {
        paste0(
          "* Status or Period x Status cell has <",
          minimum_sites_per_status,
          " sites"
        )
      } else {
        NULL
      },
      tag_levels = "A",
      theme = theme(
        plot.title = element_text(
          face = "bold",
          size = 15,
          hjust = 0
        ),
        plot.caption = element_text(
          size = 9,
          hjust = 0
        )
      )
    )
  
  if (plot_type %in% c("period_status", "temporal")) {
    combined <- combined &
      theme(
        legend.position = "bottom",
        plot.tag = element_text(
          face = "bold",
          size = 14
        )
      )
  } else {
    combined <- combined &
      theme(
        plot.tag = element_text(
          face = "bold",
          size = 14
        )
      )
  }
  
  combined
}


# ============================================================
# 11. Figure size helper
# ============================================================

get_figure_dimensions <- function(n_panels, plot_type) {
  
  if (plot_type == "temporal") {
    return(
      list(
        width = 12,
        height = if (n_panels == 6) 13 else 9
      )
    )
  }
  
  list(
    width = 10,
    height = if (n_panels == 6) 10.5 else 7.5
  )
}


# ============================================================
# 12. Save every location x method x plot-type figure
# ============================================================

all_locations <- model_diagnostics %>%
  distinct(location) %>%
  filter(!is.na(location), location != "") %>%
  pull(location) %>%
  sort()

plot_types_to_save <- c(
  "period",
  "period_status",
  "temporal"
)

for (plot_type in plot_types_to_save) {
  
  output_dir <- file.path(
    plot_output_root,
    plot_type
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  for (loc in all_locations) {
    
    for (method_key in names(metric_sets)) {
      
      metrics_this_method <- metric_sets[[method_key]]
      method_name <- unname(figure_titles[[method_key]])
      
      p <- make_concatenated_location_plot(
        location_name = loc,
        metric_vector = metrics_this_method,
        method_name = method_name,
        plot_type = plot_type
      )
      
      dims <- get_figure_dimensions(
        n_panels = length(metrics_this_method),
        plot_type = plot_type
      )
      
      filename <- paste0(
        make_safe_filename(loc),
        "__",
        make_safe_filename(method_name),
        "__",
        plot_type,
        ".png"
      )
      
      ggplot2::ggsave(
        filename = file.path(output_dir, filename),
        plot = p,
        width = dims$width,
        height = dims$height,
        dpi = 300,
        bg = "white"
      )
      
      message(
        "Saved: ",
        loc,
        " | ",
        method_name,
        " | ",
        plot_type
      )
    }
  }
}


#################################################################
# END
#################################################################