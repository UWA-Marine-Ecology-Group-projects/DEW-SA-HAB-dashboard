# ============================================================
# START DATE MODELS + FOUR PLOTS PER LOCATION
# ============================================================

library(dplyr)
library(purrr)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(tibble)
library(stringr)
library(patchwork)

# -----------------------------
# Colours and labels
# -----------------------------

metric_period_cols <- c(
  "Pre-bloom" = "#072759",
  "Bloom"     = "#e88e98"
)

status_cols <- c(
  "Fished"  = "#d95f02",
  "No-take" = "#1b9e77"
)

metric_y_lab <- list(
  shannon_diversity = "Avg. shannon\ndiversity index",
  richness = "Avg. species richness",
  sharks_rays = "Avg. shark and ray\nspecies richness",
  reef_associated_richness = "Avg. reef associated\nspecies richness",
  large_fish = "Avg. no. of fish > 200 mm",
  total_abundance = "Avg. total abundance"
)

metric_order <- c(
  "shannon_diversity",
  "richness",
  "sharks_rays",
  "reef_associated_richness",
  "large_fish",
  "total_abundance"
)

metric_lookup <- c(
  "Shannon diversity" = "shannon_diversity",
  "Species richness" = "richness",
  "Shark and ray richness" = "sharks_rays",
  "Reef associated species richness" = "reef_associated_richness",
  "Abundance > 200 mm" = "large_fish",
  "Total abundance" = "total_abundance"
)

plot_theme <- theme(
  axis.line.x = element_line(color = "black", linewidth = 0.5),
  axis.line.y = element_line(color = "black", linewidth = 0.5),
  panel.grid = element_blank()
)

# -----------------------------
# 1. Prepare data
# -----------------------------

prep_metric_data <- function(df, response_col) {
  df %>%
    filter(
      !is.na(.data[[response_col]]),
      !is.na(period),
      !is.na(status),
      !is.na(reporting_name),
      !is.na(start_date),
      !is.na(sample)
    ) %>%
    mutate(
      Period = factor(period, levels = c("Pre-bloom", "Bloom")),
      Status = factor(status, levels = c("Fished", "No-take")),
      start_date_date = as.Date(start_date),
      start_date_fct = factor(start_date_date),
      site = factor(sample)
    )
}

# Read in metadata -----
load("app_data/hab_data.Rdata")

metadata <- hab_data$hab_combined_metadata %>%
  dplyr::filter(method %in% "BRUVs")

abund_dat <- prep_metric_data(hab_data$total_abundance_samples, "total_abundance_sample")
rich_dat <- prep_metric_data(hab_data$species_richness_samples, "n_species_sample")
shark_dat <- prep_metric_data(hab_data$shark_ray_richness_samples %>% left_join(metadata), "n_species_sample")
reef_dat <- prep_metric_data(hab_data$reef_associated_richness_samples %>% left_join(metadata), "n_species_sample")
shannon_dat <- prep_metric_data(hab_data$shannon_diversity_samples %>% left_join(metadata), "shannon")
fish_200_dat <- prep_metric_data(hab_data$fish_200_abundance_samples %>% left_join(metadata), "total_abundance_sample")

# -----------------------------
# 2. Fit one region
# -----------------------------

fit_one_region <- function(df, response_col, metric_name, use_site = FALSE) {
  
  if (nrow(df) < 10) {
    stop("Not enough data")
  }
  
  area_name <- unique(df$reporting_name)[1]
  
  has_two_periods <- n_distinct(droplevels(df$Period)) >= 2
  has_two_status <- n_distinct(droplevels(df$Status)) >= 2
  has_two_dates <- n_distinct(droplevels(df$start_date_fct)) >= 2
  
  site_re <- if (use_site && "uwa_site_code" %in% names(df)) {
    " + (1 | uwa_site_code)"
  } else {
    ""
  }
  
  # Period model: start_date is random/blocking effect
  period_fixed <- case_when(
    has_two_periods & has_two_status ~ "Period * Status",
    has_two_periods ~ "Period",
    has_two_status ~ "Status",
    TRUE ~ "1"
  )
  
  period_re <- if (has_two_dates) {
    " + (1 | start_date_fct)"
  } else {
    ""
  }
  
  period_form <- as.formula(
    paste0(response_col, " ~ ", period_fixed, period_re, site_re)
  )
  
  period_model <- glmmTMB(
    period_form,
    data = df,
    family = nbinom2(link = "log")
  )
  
  # Temporal model: start_date is fixed factor
  temporal_fixed <- if (has_two_status && has_two_dates) {
    "start_date_fct * Status"
  } else if (has_two_dates) {
    "start_date_fct"
  } else if (has_two_status) {
    "Status"
  } else {
    "1"
  }
  
  temporal_form <- as.formula(
    paste0(response_col, " ~ ", temporal_fixed, site_re)
  )
  
  temporal_model <- glmmTMB(
    temporal_form,
    data = df,
    family = nbinom2(link = "log")
  )
  
  # ---- Period means
  period_means <- emmeans(period_model, ~ Period, type = "response") %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(reporting_name = area_name, metric = metric_name)
  
  period_status_means <- emmeans(period_model, ~ Period * Status, type = "response") %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(reporting_name = area_name, metric = metric_name)
  
  # ---- Start date means
  date_lookup <- df %>%
    distinct(start_date_fct, start_date_date, Period) %>%
    mutate(start_date_fct = as.character(start_date_fct))
  
  start_date_means <- emmeans(temporal_model, ~ start_date_fct, type = "response") %>%
    as.data.frame() %>%
    as_tibble() %>%
    left_join(date_lookup, by = "start_date_fct") %>%
    mutate(reporting_name = area_name, metric = metric_name)
  
  start_date_status_means <- emmeans(temporal_model, ~ start_date_fct * Status, type = "response") %>%
    as.data.frame() %>%
    as_tibble() %>%
    left_join(date_lookup, by = "start_date_fct") %>%
    mutate(reporting_name = area_name, metric = metric_name)
  
  list(
    period_model = period_model,
    temporal_model = temporal_model,
    period_means = period_means,
    period_status_means = period_status_means,
    start_date_means = start_date_means,
    start_date_status_means = start_date_status_means
  )
}

# -----------------------------
# 3. Run across regions
# -----------------------------

run_metric_models <- function(df, response_col, metric_name, use_site = FALSE) {
  
  split_dat <- split(df, df$reporting_name)
  
  outputs <- map(
    split_dat,
    ~ tryCatch(
      fit_one_region(.x, response_col, metric_name, use_site),
      error = function(e) e
    )
  )
  
  list(
    outputs = outputs,
    period_means = map_dfr(outputs, ~ if (!inherits(.x, "error")) .x$period_means),
    period_status_means = map_dfr(outputs, ~ if (!inherits(.x, "error")) .x$period_status_means),
    start_date_means = map_dfr(outputs, ~ if (!inherits(.x, "error")) .x$start_date_means),
    start_date_status_means = map_dfr(outputs, ~ if (!inherits(.x, "error")) .x$start_date_status_means),
    errors = imap_dfr(outputs, ~ {
      if (inherits(.x, "error")) {
        tibble(reporting_name = .y, metric = metric_name, error = .x$message)
      }
    })
  )
}

# -----------------------------
# 4. Run all metrics
# -----------------------------

abund_models <- run_metric_models(abund_dat, "total_abundance_sample", "Total abundance", use_site = TRUE)
rich_models <- run_metric_models(rich_dat, "n_species_sample", "Species richness")
shark_models <- run_metric_models(shark_dat, "n_species_sample", "Shark and ray richness")
reef_models <- run_metric_models(reef_dat, "n_species_sample", "Reef associated species richness")
shannon_models <- run_metric_models(shannon_dat, "shannon", "Shannon diversity")
fish_200_models <- run_metric_models(fish_200_dat, "total_abundance_sample", "Abundance > 200 mm")

period_results <- bind_rows(
  abund_models$period_means,
  rich_models$period_means,
  shark_models$period_means,
  reef_models$period_means,
  shannon_models$period_means,
  fish_200_models$period_means
) %>%
  mutate(metric_id = recode(metric, !!!metric_lookup))

period_status_results <- bind_rows(
  abund_models$period_status_means,
  rich_models$period_status_means,
  shark_models$period_status_means,
  reef_models$period_status_means,
  shannon_models$period_status_means,
  fish_200_models$period_status_means
) %>%
  mutate(metric_id = recode(metric, !!!metric_lookup))

start_date_results <- bind_rows(
  abund_models$start_date_means,
  rich_models$start_date_means,
  shark_models$start_date_means,
  reef_models$start_date_means,
  shannon_models$start_date_means,
  fish_200_models$start_date_means
) %>%
  mutate(metric_id = recode(metric, !!!metric_lookup))

start_date_status_results <- bind_rows(
  abund_models$start_date_status_means,
  rich_models$start_date_status_means,
  shark_models$start_date_status_means,
  reef_models$start_date_status_means,
  shannon_models$start_date_status_means,
  fish_200_models$start_date_status_means
) %>%
  mutate(metric_id = recode(metric, !!!metric_lookup))

# -----------------------------
# 5. Plot helpers
# -----------------------------

blank_panel <- function(panel_letter) {
  ggplot() +
    theme_void() +
    labs(tag = panel_letter) +
    theme(plot.tag = element_text(size = 18))
}

plot_period <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>% filter(metric_id == !!metric_id)
  
  if (nrow(metric_df) == 0) return(blank_panel(panel_letter))
  
  ggplot(metric_df, aes(x = Period, y = response, fill = Period)) +
    geom_col(width = 0.6, colour = "black", alpha = 0.85) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, linewidth = 0.6) +
    scale_fill_manual(values = metric_period_cols, drop = FALSE) +
    labs(x = NULL, y = metric_y_lab[[metric_id]], tag = panel_letter, fill = NULL) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(legend.position = "none")
}

plot_period_status <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>% filter(metric_id == !!metric_id)
  
  if (nrow(metric_df) == 0) return(blank_panel(panel_letter))
  
  ggplot(metric_df, aes(x = Period, y = response, fill = Status)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, colour = "black", alpha = 0.85) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  position = position_dodge(width = 0.7),
                  width = 0.18,
                  linewidth = 0.6) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    labs(x = NULL, y = metric_y_lab[[metric_id]], tag = panel_letter, fill = NULL) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(legend.position = "bottom")
}

plot_start_date <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>%
    filter(metric_id == !!metric_id) %>%
    arrange(start_date_date)
  
  if (nrow(metric_df) == 0) return(blank_panel(panel_letter))
  
  ggplot(metric_df, aes(x = start_date_date, y = response, fill = Period)) +
    geom_col(width = 25, colour = "black", alpha = 0.85) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 10, linewidth = 0.6) +
    scale_fill_manual(values = metric_period_cols, drop = FALSE) +
    scale_x_date(
      breaks = sort(unique(metric_df$start_date_date)),
      labels = scales::label_date("%b\n%Y"),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(x = NULL, y = metric_y_lab[[metric_id]], tag = panel_letter, fill = NULL) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "bottom")
}

plot_start_date_status <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>%
    filter(metric_id == !!metric_id) %>%
    arrange(start_date_date)
  
  if (nrow(metric_df) == 0) return(blank_panel(panel_letter))
  
  ggplot(metric_df, aes(x = start_date_date, y = response, fill = Status)) +
    geom_col(position = position_dodge(width = 30), width = 25, colour = "black", alpha = 0.85) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  position = position_dodge(width = 30),
                  width = 10,
                  linewidth = 0.6) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    scale_x_date(
      breaks = sort(unique(metric_df$start_date_date)),
      labels = scales::label_date("%b\n%Y"),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(x = NULL, y = metric_y_lab[[metric_id]], tag = panel_letter, fill = NULL) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "bottom")
}

# -----------------------------
# 6. Save four plot types
# -----------------------------

save_patchwork_plots <- function(results_df, plot_fun, output_dir, suffix, title_suffix) {
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (region in unique(results_df$reporting_name)) {
    
    plot_df <- results_df %>% filter(reporting_name == region)
    
    plots <- map2(
      metric_order,
      LETTERS[seq_along(metric_order)],
      ~ plot_fun(plot_df, .x, .y)
    )
    
    p <- wrap_plots(plots, ncol = 2, guides = "collect") +
      plot_annotation(title = paste(region, "-", title_suffix)) &
      theme(
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom"
      )
    
    safe_name <- region %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%
      str_replace_all("_$", "")
    
    ggsave(
      filename = file.path(output_dir, paste0(safe_name, "_", suffix, ".png")),
      plot = p,
      width = 8,
      height = 10,
      dpi = 300
    )
  }
}

save_patchwork_plots(
  period_results,
  plot_period,
  "plots/period_results",
  "period",
  "period means"
)

save_patchwork_plots(
  period_status_results,
  plot_period_status,
  "plots/period_status_results",
  "period_status",
  "period means by status"
)

save_patchwork_plots(
  start_date_results,
  plot_start_date,
  "plots/start_date_results",
  "start_date",
  "temporal results"
)

save_patchwork_plots(
  start_date_status_results,
  plot_start_date_status,
  "plots/start_date_status_results",
  "start_date_status",
  "temporal results by status"
)






# Checking

check_raw_dates <- function(df, response_col, region_name) {
  df %>%
    filter(reporting_name == region_name) %>%
    mutate(
      start_date_date = as.Date(start_date),
      year = format(start_date_date, "%Y")
    ) %>%
    group_by(year, start_date_date, period, status) %>%
    summarise(
      n_rows = n(),
      n_non_missing_response = sum(!is.na(.data[[response_col]])),
      .groups = "drop"
    ) %>%
    arrange(start_date_date)
}

check_raw_dates(
  hab_data$total_abundance_samples,
  "total_abundance_sample",
  "Aldinga - Aldinga Reef Sanctuary Zone"
)

t <- start_date_results %>%
  filter(reporting_name == "Aldinga - Aldinga Reef Sanctuary Zone") %>%
  count(metric, start_date_date, Period) %>%
  arrange(metric, start_date_date)
