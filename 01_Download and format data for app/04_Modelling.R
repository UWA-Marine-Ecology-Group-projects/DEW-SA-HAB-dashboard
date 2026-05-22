# Load libraries needed -----
library(dplyr)
library(purrr)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(tibble)
library(progressr)
library(sf)
library(stringr)

handlers(global = TRUE)
handlers("progress")

sf_use_s2(FALSE)

# Read in metadata -----
load("app_data/hab_data.Rdata")

metadata <- hab_data$hab_combined_metadata %>%
  dplyr::filter(method %in% "BRUVs")

unique(metadata$method)

glimpse(hab_data)
unique(metadata$reporting_name)

# -----------------------------
# 1. Prepare data ----
# -----------------------------

prep_metric_data <- function(df, response_col) {
  df %>%
    filter(
      !is.na(.data[[response_col]]),
      !is.na(period),
      !is.na(status),
      !is.na(reporting_name),
      !is.na(year),
      !is.na(sample)
    ) %>%
    mutate(
      Period = factor(period, levels = c("Pre-bloom", "Bloom")),
      Status = factor(status, levels = c("Fished", "No-take")),
      year = factor(year),
      site = factor(sample)
    )
}


unique((hab_data$total_abundance_samples)$reporting_name)
subset_names <- unique(metadata$reporting_name)[1:6]

abund_dat <- prep_metric_data(
  hab_data$total_abundance_samples,
  "total_abundance_sample") %>%
  glimpse()

unique(abund_dat$reporting_name)

rich_dat <- prep_metric_data(
  hab_data$species_richness_samples,
  "n_species_sample")  %>%
  glimpse()

shark_dat <- prep_metric_data(
  hab_data$shark_ray_richness_samples %>% left_join(metadata),
  "n_species_sample")  %>%
  glimpse()

reef_dat <- prep_metric_data(
  hab_data$reef_associated_richness_samples %>% left_join(metadata),
  "n_species_sample")  %>%
  glimpse()

shannon_dat <- prep_metric_data(
  hab_data$shannon_diversity_samples %>% left_join(metadata),
  "shannon")  %>%
  glimpse()

fish_200_dat <- prep_metric_data(
  hab_data$fish_200_abundance_samples %>% left_join(metadata),
  "total_abundance_sample")  %>%
  glimpse()

unique(abund_dat$site)
unique(abund_dat$uwa_site_code)

# -----------------------------
# 2. Fit one reporting_name model ----
# -----------------------------

fit_one_reporting_name <- function(df,
                                   response_col,
                                   metric_name,
                                   use_site = FALSE) {
  
  if (
    nrow(df) < 10 ||
    n_distinct(droplevels(df$year)) < 2
  ) {
    stop("Not enough data to fit model")
  }
  
  area_reporting_name <- df %>%
    distinct(reporting_name) %>%
    pull(reporting_name) %>%
    first()
  
  has_two_periods <- n_distinct(droplevels(df$Period)) >= 2
  has_two_status  <- n_distinct(droplevels(df$Status)) >= 2
  
  rand_effects <- if (use_site) {
    "(1 | uwa_site_code) + (1 | year)"
  } else {
    "(1 | year)"
  }
  
  fixed_effects <- case_when(
    has_two_periods & has_two_status  ~ "Period * Status",
    has_two_periods & !has_two_status ~ "Period",
    !has_two_periods & has_two_status ~ "Status",
    TRUE                              ~ "1"
  )
  
  model_used <- paste(fixed_effects, "+", rand_effects)
  
  form <- as.formula(
    paste0(response_col, " ~ ", fixed_effects, " + ", rand_effects)
  )
  
  message("Using formula: ", deparse(form))
  
  family_to_use <- nbinom2(link = "log")
  
  model <- glmmTMB(
    form,
    data = df,
    family = family_to_use
  )
  
  model_summary <- summary(model)
  
  # -----------------------------
  # Period means----
  # -----------------------------
  
  if (has_two_periods) {
    
    period_means <- emmeans(
      model,
      ~ Period,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
  } else {
    
    single_period <- df %>%
      distinct(Period) %>%
      pull(Period) %>%
      as.character() %>%
      .[1]
    
    period_means <- emmeans(
      model,
      ~ 1,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
    period_means[["Period"]] <- single_period
  }
  
  period_means <- period_means %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = ifelse(
        has_two_periods,
        "Period means",
        "Intercept only - single Period"
      )
    )
  
  # -----------------------------
  # Period x Status means----
  # -----------------------------
  
  if (has_two_periods && has_two_status) {
    
    period_status_means <- emmeans(
      model,
      ~ Period * Status,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
  } else if (has_two_periods && !has_two_status) {
    
    single_status <- df %>%
      distinct(Status) %>%
      pull(Status) %>%
      as.character() %>%
      .[1]
    
    period_status_means <- emmeans(
      model,
      ~ Period,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
    period_status_means[["Status"]] <- single_status
    
  } else if (!has_two_periods && has_two_status) {
    
    single_period <- df %>%
      distinct(Period) %>%
      pull(Period) %>%
      as.character() %>%
      .[1]
    
    period_status_means <- emmeans(
      model,
      ~ Status,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
    period_status_means[["Period"]] <- single_period
    
  } else {
    
    single_period <- df %>%
      distinct(Period) %>%
      pull(Period) %>%
      as.character() %>%
      .[1]
    
    single_status <- df %>%
      distinct(Status) %>%
      pull(Status) %>%
      as.character() %>%
      .[1]
    
    period_status_means <- emmeans(
      model,
      ~ 1,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
    period_status_means[["Period"]] <- single_period
    period_status_means[["Status"]] <- single_status
  }
  
  period_status_means <- period_status_means %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = case_when(
        has_two_periods & has_two_status  ~ "Period by Status",
        has_two_periods & !has_two_status ~ "Period only - single Status",
        !has_two_periods & has_two_status ~ "Status only - single Period",
        TRUE                              ~ "Intercept only - single Period and Status"
      )
    )
  
  list(
    model = model,
    model_summary = model_summary,
    period_means = period_means,
    period_status_means = period_status_means
  )
}

# -----------------------------
# 3. Run models across all areas with debugging + incremental saving----
# -----------------------------

run_metric_models <- function(df,
                              response_col,
                              metric_name,
                              use_site = FALSE,
                              output_dir = "model_outputs",
                              overwrite = FALSE) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  split_dat <- df %>%
    group_by(reporting_name) %>%
    group_split()
  
  names(split_dat) <- map_chr(split_dat, ~ unique(.x$reporting_name)[1])
  
  all_outputs <- list()
  
  for (area_name in names(split_dat)) {
    
    area_df <- split_dat[[area_name]]
    
    safe_area_name <- area_name %>%
      stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
      stringr::str_replace_all("_$", "")
    
    output_file <- file.path(
      output_dir,
      paste0(
        safe_area_name, "_",
        stringr::str_replace_all(metric_name, "[^A-Za-z0-9]+", "_"),
        ".rds"
      )
    )
    
    if (file.exists(output_file) && !overwrite) {
      message("\nSkipping already completed model: ", area_name)
      all_outputs[[area_name]] <- readRDS(output_file)
      next
    }
    
    message("Response summary:")
    print(summary(area_df[[response_col]]))
    
    message("Zeros:")
    print(sum(area_df[[response_col]] == 0, na.rm = TRUE))
    
    message("Cross table:")
    print(table(area_df$Period, area_df$Status))
    
    message("Rows per year:")
    print(table(area_df$year))
    
    if ("uwa_site_code" %in% names(area_df)) {
      
      message("Rows per site:")
      print(sort(table(area_df$uwa_site_code), decreasing = TRUE))
      
    }
    
    message("\n==================================================")
    message("Metric: ", metric_name)
    message("Location: ", area_name)
    message("Rows: ", nrow(area_df))
    message("Years: ", n_distinct(area_df$year), " | ", paste(sort(unique(area_df$year)), collapse = ", "))
    message("Sites/sample: ", n_distinct(area_df$site))
    
    if ("uwa_site_code" %in% names(area_df)) {
      message("Unique UWA site codes: ", n_distinct(area_df$uwa_site_code))
      message("UWA site codes: ", paste(sort(unique(area_df$uwa_site_code)), collapse = ", "))
    }
    
    message("Periods: ", n_distinct(area_df$Period), " | ", paste(unique(area_df$Period), collapse = ", "))
    message("Statuses: ", n_distinct(area_df$Status), " | ", paste(unique(area_df$Status), collapse = ", "))
    message("Response range: ", min(area_df[[response_col]], na.rm = TRUE), " to ", max(area_df[[response_col]], na.rm = TRUE))
    message("Starting model at: ", Sys.time())
    
    start_time <- Sys.time()
    
    result <- tryCatch(
      {
        fit <- fit_one_reporting_name(
          area_df,
          response_col,
          metric_name,
          use_site = use_site
        )
        
        list(
          result = fit,
          error = NULL,
          diagnostics = tibble(
            reporting_name = area_name,
            metric = metric_name,
            n_rows = nrow(area_df),
            n_years = n_distinct(area_df$year),
            n_sites = n_distinct(area_df$site),
            n_uwa_site_codes = if ("uwa_site_code" %in% names(area_df)) n_distinct(area_df$uwa_site_code) else NA_integer_,
            n_periods = n_distinct(area_df$Period),
            n_statuses = n_distinct(area_df$Status),
            start_time = start_time,
            end_time = Sys.time(),
            run_time_minutes = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
            status = "success",
            error_message = NA_character_
          )
        )
      },
      error = function(e) {
        list(
          result = NULL,
          error = e,
          diagnostics = tibble(
            reporting_name = area_name,
            metric = metric_name,
            n_rows = nrow(area_df),
            n_years = n_distinct(area_df$year),
            n_sites = n_distinct(area_df$site),
            n_uwa_site_codes = if ("uwa_site_code" %in% names(area_df)) n_distinct(area_df$uwa_site_code) else NA_integer_,
            n_periods = n_distinct(area_df$Period),
            n_statuses = n_distinct(area_df$Status),
            start_time = start_time,
            end_time = Sys.time(),
            run_time_minutes = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
            status = "error",
            error_message = e$message
          )
        )
      }
    )
    
    end_time <- Sys.time()
    
    message("Finished at: ", end_time)
    message("Runtime minutes: ", round(as.numeric(difftime(end_time, start_time, units = "mins")), 2))
    
    if (!is.null(result$error)) {
      message("ERROR: ", result$error$message)
    } else {
      message("SUCCESS")
    }
    
    saveRDS(result, output_file)
    message("Saved result to: ", output_file)
    
    all_outputs[[area_name]] <- result
  }
  
  period_means <- map_dfr(
    all_outputs,
    ~ if (is.null(.x$error) && !is.null(.x$result)) .x$result$period_means else NULL
  )
  
  period_status_means <- map_dfr(
    all_outputs,
    ~ if (is.null(.x$error) && !is.null(.x$result)) .x$result$period_status_means else NULL
  )
  
  model_errors <- imap_dfr(
    all_outputs,
    ~ {
      if (!is.null(.x$error)) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = .x$error$message
        )
      } else {
        NULL
      }
    }
  )
  
  model_diagnostics <- map_dfr(all_outputs, "diagnostics")
  
  saveRDS(
    list(
      model_outputs = all_outputs,
      period_means = period_means,
      period_status_means = period_status_means,
      model_errors = model_errors,
      model_diagnostics = model_diagnostics
    ),
    file.path(output_dir, paste0(stringr::str_replace_all(metric_name, "[^A-Za-z0-9]+", "_"), "_combined_results.rds"))
  )
  
  list(
    model_outputs = all_outputs,
    period_means = period_means,
    period_status_means = period_status_means,
    model_errors = model_errors,
    model_diagnostics = model_diagnostics
  )
}

# -----------------------------
# 4. Run metrics
# -----------------------------
# Total abundance----
# ------------------------------

abund_models <- run_metric_models(
  abund_dat,
  response_col = "total_abundance_sample",
  metric_name = "Total abundance",
  use_site = TRUE,
  output_dir = "model_outputs/abundance"
)

# Check diagnostics
abund_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
abund_models$model_errors

# -----------------------------
# Species richness----
# -----------------------------

rich_models <- run_metric_models(
  rich_dat,
  response_col = "n_species_sample",
  metric_name = "Species richness",
  use_site = FALSE,
  output_dir = "model_outputs/species_richness"
)

# Check diagnostics
rich_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
rich_models$model_errors


# -----------------------------
# Shark and ray richness----
# -----------------------------

shark_models <- run_metric_models(
  shark_dat,
  response_col = "n_species_sample",
  metric_name = "Shark and ray richness",
  use_site = FALSE,
  output_dir = "model_outputs/shark_ray_richness"
)

# Check diagnostics
shark_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
shark_models$model_errors


# -----------------------------
# Reef associated richness----
# -----------------------------

reef_models <- run_metric_models(
  reef_dat,
  response_col = "n_species_sample",
  metric_name = "Reef associated species richness",
  use_site = FALSE,
  output_dir = "model_outputs/reef_associated_richness"
)

# Check diagnostics
reef_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
reef_models$model_errors

# -----------------------------
# Shannon diversity        ----
# -----------------------------

shannon_models <- run_metric_models(
  shannon_dat,
  response_col = "shannon",
  metric_name = "Shannon diversity",
  use_site = FALSE,
  output_dir = "model_outputs/shannon_diversity"
)

# Check diagnostics
shannon_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
shannon_models$model_errors

# -----------------------------
#  Fish larger than 200 mm       ----
# -----------------------------

fish_200_models <- run_metric_models(
  fish_200_dat,
  response_col = "total_abundance_sample",
  metric_name = "Abundance > 200 mm",
  use_site = FALSE,
  output_dir = "model_outputs/200mm"
)

# Check diagnostics
fish_200_models$model_diagnostics %>%
  arrange(desc(run_time_minutes))

# Check errors
fish_200_models$model_errors

# # -----------------------------
# # 5. Combine dashboard-ready outputs----
# # -----------------------------
# 
period_results <- bind_rows(
  abund_models$period_means,
  rich_models$period_means,
  shark_models$period_means,
  reef_models$period_means,
  shannon_models$period_means,
  fish_200_models$period_means
)

period_status_results <- bind_rows(
  abund_models$period_status_means,
  rich_models$period_status_means,
  shark_models$period_status_means,
  reef_models$period_status_means,
  shannon_models$period_status_means,
  fish_200_models$period_status_means
)

# model_errors <- bind_rows(
#   abund_models$model_errors,
#   # rich_models$model_errors,
#   # shark_models$model_errors,
#   # reef_models$model_errors
# )
# 
# # Check any models that failed
# model_errors
# 
# # Plot 1: Pre-bloom vs Bloom for all areas
# # shared colours for pre/post everywhere
# metric_period_cols <- c(
#   "Pre-bloom"  = "#072759",  # same blue
#   "Bloom" = "#e88e98"   # same orange
# )
# 
# ggplot(period_results, aes(x = Period, y = response)) +
#   geom_point(size = 2) +
#   geom_errorbar(
#     aes(ymin = response - SE, ymax = response + SE),
#     width = 0.15
#   ) +
#   facet_wrap(metric ~ reporting_name, scales = "free_y") +
#   theme_bw()
# 
# # Trying to make it look like the dashboard ----
# ggplot(period_results, aes(x = Period, y = response, fill = Period)) +
#   geom_col(width = 0.6, colour = "black", alpha = 0.85) +
#   geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2, linewidth = 0.6) +
#   scale_fill_manual(values = metric_period_cols) +
#   theme_minimal(base_size = 16) +
#   theme(legend.position = "none",
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank()) +
#   facet_wrap(metric ~ reporting_name, scales = "free_y")
# 
# # Plot 2: Pre-bloom vs Bloom by Fished vs No-take
# ggplot(period_status_results, aes(x = Period, y = response, colour = Status)) +
#   geom_point(
#     position = position_dodge(width = 0.3),
#     size = 2
#   ) +
#   geom_errorbar(
#     aes(ymin = response - SE, ymax = response + SE),
#     position = position_dodge(width = 0.3),
#     width = 0.15
#   ) +
#   facet_wrap(metric ~ reporting_name, scales = "free_y") +
#   theme_bw()

# Create output folder
dir.create("plots/period_results", recursive = TRUE, showWarnings = FALSE)

# Get unique reporting regions
reporting_regions <- unique(period_results$reporting_name)
reporting_regions

# shared colours for pre/post everywhere
metric_period_cols <- c(
  "Pre-bloom"  = "#072759",  # same blue
  "Bloom" = "#e88e98"   # same orange
)

library(patchwork)

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

plot_one_metric <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>%
    filter(metric_id == !!metric_id)
  
  if (nrow(metric_df) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(tag = panel_letter) +
        theme(
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1)
        )
    )
  }
  
  ggplot(metric_df, aes(x = Period, y = response, fill = Period)) +
    geom_col(
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = response - SE, ymax = response + SE),
      width = 0.2,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = metric_period_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      tag = panel_letter
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1)
    )
}

status_cols <- c(
  "Fished"  = "#d95f02",
  "No-take" = "#1b9e77"
)

plot_one_metric_status <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>%
    filter(metric_id == !!metric_id) %>%
    mutate(
      Period = factor(Period, levels = c("Pre-bloom", "Bloom")),
      Status = factor(Status, levels = c("Fished", "No-take"))
    )
  
  if (nrow(metric_df) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(tag = panel_letter) +
        theme(
          plot.tag = element_text(size = 18),
          plot.tag.position = c(0, 1)
        )
    )
  }
  
  ggplot(metric_df, aes(x = Period, y = response, fill = Status)) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = response - SE, ymax = response + SE),
      position = position_dodge(width = 0.7),
      width = 0.18,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      fill = NULL,
      tag = panel_letter
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      plot.tag = element_text(size = 18),
      plot.tag.position = c(0, 1),
      legend.position = "bottom"
    )
}

dir.create("plots/period_status_results", recursive = TRUE, showWarnings = FALSE)

reporting_regions_status <- unique(period_status_results$reporting_name)

for (region in reporting_regions_status) {
  
  message("Plotting status plot: ", region)
  
  plot_df <- period_status_results %>%
    filter(reporting_name == region) %>%
    mutate(
      Period = factor(Period, levels = c("Pre-bloom", "Bloom")),
      Status = factor(Status, levels = c("Fished", "No-take")),
      metric_id = recode(metric, !!!metric_lookup)
    )
  
  plots <- purrr::map2(
    metric_order,
    LETTERS[seq_along(metric_order)],
    ~ plot_one_metric_status(plot_df, .x, .y)
  )
  
  p <- wrap_plots(plots, ncol = 2, guides = "collect") +
    plot_annotation(title = paste(region, "- by status")) &
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.position = "bottom"
    )
  
  safe_name <- region %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_$", "")
  
  ggsave(
    filename = file.path(
      "plots/period_status_results",
      paste0(safe_name, "_status.png")
    ),
    plot = p,
    width = 8,
    height = 10,
    dpi = 300
  )
}

# # Loop through regions
# plot_theme <- theme(
#   axis.line.x = element_line(color = "black", linewidth = 0.5),
#   axis.line.y = element_line(color = "black", linewidth = 0.5),
#   legend.position = "none", 
#   panel.grid.minor = element_blank(),
#   panel.grid.major = element_blank())
# 
# for (region in reporting_regions) {
#   
#   message("Plotting: ", region)
#   
#   plot_df <- period_results %>%
#     filter(reporting_name == region) %>%
#     dplyr::mutate(
#       Period = factor(Period, levels = c("Pre-bloom", "Bloom"))
#     )
#   
#   p <- ggplot(
#     plot_df,
#     aes(x = Period, y = response, fill = Period)
#   ) +
#     geom_col(
#       width = 0.6,
#       colour = "black",
#       alpha = 0.85
#     ) +
#     geom_errorbar(
#       aes(
#         ymin = response - SE,
#         ymax = response + SE
#       ),
#       width = 0.2,
#       linewidth = 0.6
#     ) +
#     scale_fill_manual(values = metric_period_cols) +
#     theme_minimal(base_size = 16) +
#     theme(
#       legend.position = "none",
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank()
#     ) +
#     facet_wrap(~ metric, scales = "free_y", ncol = 2) +
#     labs(
#       title = region#,
#       # x = NULL,
#       # y = NULL
#     )+
#     theme_minimal(base_size = 16) +
#     plot_theme
#   
#   # Safe filename
#   safe_name <- region %>%
#     stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
#     stringr::str_replace_all("_$", "")
#   
#   # Save plot
#   ggsave(
#     filename = file.path(
#       "plots/period_results",
#       paste0(safe_name, ".png")
#     ),
#     plot = p,
#     width = 5,
#     height = 8,
#     dpi = 300
#   )
#   
# }

for (region in reporting_regions) {
  
  message("Plotting: ", region)
  
  plot_df <- period_results %>%
    filter(reporting_name == region) %>%
    mutate(
      Period = factor(Period, levels = c("Pre-bloom", "Bloom")),
      metric_id = recode(metric, !!!metric_lookup)
    )
  
  plots <- purrr::map2(
    metric_order,
    LETTERS[seq_along(metric_order)],
    ~ plot_one_metric(plot_df, .x, .y)
  )
  
  p <- wrap_plots(plots, ncol = 2) +
    plot_annotation(title = region) &
    theme(
      plot.title = element_text(size = 18, hjust = 0.5)
    )
  
  safe_name <- region %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_$", "")
  
  ggsave(
    filename = file.path("plots/period_results", paste0(safe_name, ".png")),
    plot = p,
    width = 8,
    height = 10,
    dpi = 300
  )
}

# Create output folder
dir.create("plots/period_status_results",
           recursive = TRUE,
           showWarnings = FALSE)

# Get unique reporting regions
reporting_regions <- unique(period_status_results$reporting_name)

# Loop through regions
for (region in reporting_regions) {
  
  message("Plotting: ", region)
  
  plot_df <- period_status_results %>%
    filter(reporting_name == region)
  
  p <- ggplot(
    plot_df,
    aes(
      x = Period,
      y = response,
      colour = Status,
      group = Status
    )
  ) +
    geom_point(
      position = position_dodge(width = 0.3),
      size = 3
    ) +
    # geom_line(
    #   position = position_dodge(width = 0.3),
    #   linewidth = 0.8
    # ) +
    geom_errorbar(
      aes(
        ymin = response - SE,
        ymax = response + SE
      ),
      position = position_dodge(width = 0.3),
      width = 0.15,
      linewidth = 0.6
    ) +
    facet_wrap(~ metric, scales = "free_y") +
    theme_bw(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.position = "bottom"
    ) +
    labs(
      title = region,
      x = NULL,
      y = NULL
    )
  
  # Safe filename
  safe_name <- region %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_$", "")
  
  # Save plot
  ggsave(
    filename = file.path(
      "plots/period_status_results",
      paste0(safe_name, ".png")
    ),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
  
}



newdat <- expand.grid(
  year_scaled = seq(
    min(df$year_scaled),
    max(df$year_scaled),
    length.out = 100
  ),
  Status = unique(df$Status)
)

newdat$pred <- predict(
  trend_model,
  newdata = newdat,
  type = "response",
  re.form = NA
)


# MODELLING BY YEAR ----
library(dplyr)
library(purrr)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(tibble)
library(stringr)

# -----------------------------
# 1. Prepare data with numeric year
# -----------------------------

prep_metric_data_year <- function(df, response_col) {
  df %>%
    filter(
      !is.na(.data[[response_col]]),
      !is.na(status),
      !is.na(reporting_name),
      !is.na(year),
      !is.na(sample)
    ) %>%
    mutate(
      Status = factor(status, levels = c("Fished", "No-take")),
      year = as.numeric(as.character(year)),
      year_scaled = as.numeric(scale(year)),
      site = factor(sample)
    )
}

abund_dat <- prep_metric_data_year(
  hab_data$total_abundance_samples,
  "total_abundance_sample"
)

rich_dat <- prep_metric_data_year(
  hab_data$species_richness_samples,
  "n_species_sample"
)

shark_dat <- prep_metric_data_year(
  hab_data$shark_ray_richness_samples %>% left_join(metadata),
  "n_species_sample"
)

reef_dat <- prep_metric_data_year(
  hab_data$reef_associated_richness_samples %>% left_join(metadata),
  "n_species_sample"
)

# -----------------------------
# 2. Fit one reporting region using year as fixed effect
# -----------------------------

fit_one_reporting_name_year <- function(df,
                                        response_col,
                                        metric_name,
                                        use_site = FALSE) {
  
  if (
    nrow(df) < 20 ||
    n_distinct(df$year) < 2
  ) {
    stop("Not enough data to fit year model")
  }
  
  area_reporting_name <- df %>%
    distinct(reporting_name) %>%
    pull(reporting_name) %>%
    first()
  
  has_two_status <- n_distinct(droplevels(df$Status)) >= 2
  
  rand_effects <- if (use_site) {
    "(1 | uwa_site_code)"
  } else {
    NULL
  }
  
  fixed_effects <- if (has_two_status) {
    "year_scaled * Status"
  } else {
    "year_scaled"
  }
  
  rhs <- if (!is.null(rand_effects)) {
    paste(fixed_effects, "+", rand_effects)
  } else {
    fixed_effects
  }
  
  form <- as.formula(
    paste0(response_col, " ~ ", rhs)
  )
  
  message("Using formula: ", deparse(form))
  
  model <- glmmTMB(
    form,
    data = df,
    family = nbinom2(link = "log")
  )
  
  model_summary <- summary(model)
  
  # Prediction grid
  # TODO use this if I want all years in the sequence 
  # pred_years <- seq(
  #   min(df$year, na.rm = TRUE),
  #   max(df$year, na.rm = TRUE),
  #   by = 1
  # )
  
  pred_years <- sort(unique(df$year))
  
  year_mean <- mean(df$year, na.rm = TRUE)
  year_sd <- sd(df$year, na.rm = TRUE)
  
  if (has_two_status) {
    newdat <- expand.grid(
      year = pred_years,
      Status = levels(droplevels(df$Status))
    )
  } else {
    newdat <- data.frame(
      year = pred_years,
      Status = as.character(unique(df$Status)[1])
    )
  }
  
  newdat$year_scaled <- (newdat$year - year_mean) / year_sd
  
  pred <- predict(
    model,
    newdata = newdat,
    type = "link",
    se.fit = TRUE,
    re.form = NA
  )
  
  predictions <- newdat %>%
    mutate(
      fit = exp(pred$fit),
      lwr = exp(pred$fit - 1.96 * pred$se.fit),
      upr = exp(pred$fit + 1.96 * pred$se.fit),
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = rhs,
      has_two_status = has_two_status
    )
  
  list(
    model = model,
    model_summary = model_summary,
    predictions = predictions
  )
}

# -----------------------------
# 3. Run models across regions
# -----------------------------

run_metric_year_models <- function(df,
                                   response_col,
                                   metric_name,
                                   use_site = FALSE,
                                   output_dir = "model_outputs_year",
                                   overwrite = FALSE) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  split_dat <- split(df, df$reporting_name)
  
  all_outputs <- list()
  
  for (area_name in names(split_dat)) {
    
    area_df <- split_dat[[area_name]]
    
    safe_area_name <- area_name %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%
      str_replace_all("_$", "")
    
    safe_metric_name <- metric_name %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%
      str_replace_all("_$", "")
    
    output_file <- file.path(
      output_dir,
      paste0(safe_area_name, "_", safe_metric_name, ".rds")
    )
    
    if (file.exists(output_file) && !overwrite) {
      message("Skipping existing: ", area_name)
      all_outputs[[area_name]] <- readRDS(output_file)
      next
    }
    
    message("\n==================================================")
    message("Metric: ", metric_name)
    message("Location: ", area_name)
    message("Rows: ", nrow(area_df))
    message("Years: ", n_distinct(area_df$year), " | ", paste(sort(unique(area_df$year)), collapse = ", "))
    message("Statuses: ", paste(unique(area_df$Status), collapse = ", "))
    
    if ("uwa_site_code" %in% names(area_df)) {
      message("Unique UWA site codes: ", n_distinct(area_df$uwa_site_code))
    }
    
    start_time <- Sys.time()
    
    result <- tryCatch(
      {
        fit <- fit_one_reporting_name_year(
          area_df,
          response_col,
          metric_name,
          use_site = use_site
        )
        
        list(
          result = fit,
          error = NULL,
          diagnostics = tibble(
            reporting_name = area_name,
            metric = metric_name,
            n_rows = nrow(area_df),
            n_years = n_distinct(area_df$year),
            n_statuses = n_distinct(droplevels(area_df$Status)),
            n_sites = if ("uwa_site_code" %in% names(area_df)) n_distinct(area_df$uwa_site_code) else NA_integer_,
            start_time = start_time,
            end_time = Sys.time(),
            run_time_minutes = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
            status = "success",
            error_message = NA_character_
          )
        )
      },
      error = function(e) {
        list(
          result = NULL,
          error = e,
          diagnostics = tibble(
            reporting_name = area_name,
            metric = metric_name,
            n_rows = nrow(area_df),
            n_years = n_distinct(area_df$year),
            n_statuses = n_distinct(droplevels(area_df$Status)),
            n_sites = if ("uwa_site_code" %in% names(area_df)) n_distinct(area_df$uwa_site_code) else NA_integer_,
            start_time = start_time,
            end_time = Sys.time(),
            run_time_minutes = as.numeric(difftime(Sys.time(), start_time, units = "mins")),
            status = "error",
            error_message = e$message
          )
        )
      }
    )
    
    saveRDS(result, output_file)
    all_outputs[[area_name]] <- result
  }
  
  predictions <- map_dfr(
    all_outputs,
    ~ if (is.null(.x$error) && !is.null(.x$result)) .x$result$predictions else NULL
  )
  
  model_errors <- imap_dfr(
    all_outputs,
    ~ {
      if (!is.null(.x$error)) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = .x$error$message
        )
      } else {
        NULL
      }
    }
  )
  
  model_diagnostics <- map_dfr(all_outputs, "diagnostics")
  
  list(
    model_outputs = all_outputs,
    predictions = predictions,
    model_errors = model_errors,
    model_diagnostics = model_diagnostics
  )
}

# -----------------------------
# 4. Run all metrics
# -----------------------------

abund_year_models <- run_metric_year_models(
  abund_dat,
  response_col = "total_abundance_sample",
  metric_name = "Total abundance",
  use_site = TRUE,
  output_dir = "model_outputs_year/abundance"
)

rich_year_models <- run_metric_year_models(
  rich_dat,
  response_col = "n_species_sample",
  metric_name = "Species richness",
  use_site = FALSE,
  output_dir = "model_outputs_year/species_richness"
)

shark_year_models <- run_metric_year_models(
  shark_dat,
  response_col = "n_species_sample",
  metric_name = "Shark and ray richness",
  use_site = FALSE,
  output_dir = "model_outputs_year/shark_ray_richness"
)

reef_year_models <- run_metric_year_models(
  reef_dat,
  response_col = "n_species_sample",
  metric_name = "Reef associated species richness",
  use_site = FALSE,
  output_dir = "model_outputs_year/reef_associated_richness"
)

# -----------------------------
# 5. Combine predictions
# -----------------------------

year_predictions <- bind_rows(
  abund_year_models$predictions,
  rich_year_models$predictions,
  shark_year_models$predictions,
  reef_year_models$predictions
)

year_model_errors <- bind_rows(
  abund_year_models$model_errors,
  rich_year_models$model_errors,
  shark_year_models$model_errors,
  reef_year_models$model_errors
)

year_model_diagnostics <- bind_rows(
  abund_year_models$model_diagnostics,
  rich_year_models$model_diagnostics,
  shark_year_models$model_diagnostics,
  reef_year_models$model_diagnostics
)

saveRDS(year_predictions, "model_outputs_year/year_predictions.rds")
saveRDS(year_model_errors, "model_outputs_year/year_model_errors.rds")
saveRDS(year_model_diagnostics, "model_outputs_year/year_model_diagnostics.rds")

# -----------------------------
# 6. Plot one reporting region per file
# -----------------------------

dir.create("plots/year_predictions",
           recursive = TRUE,
           showWarnings = FALSE)

reporting_regions <- unique(year_predictions$reporting_name)

for (region in reporting_regions) {
  
  message("Plotting: ", region)
  
  plot_df <- year_predictions %>%
    filter(reporting_name == region)
  
  p <- ggplot(
    plot_df,
    aes(
      x = year,
      y = fit,
      colour = Status
    )
  ) +
    geom_errorbar(
      aes(ymin = lwr, ymax = upr),
      width = 0.15,
      linewidth = 0.6,
      position = position_dodge(width = 0.3)
    ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.3)
    ) +
    facet_wrap(~ metric, scales = "free_y") +
    theme_bw(base_size = 16) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = region,
      x = "Year",
      y = "Predicted value"
    )
  
  safe_name <- region %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_$", "")
  
  ggsave(
    filename = file.path(
      "plots/year_predictions",
      paste0(safe_name, "_year_predictions.png")
    ),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}

year_model_errors

year_model_diagnostics %>%
  arrange(desc(run_time_minutes))
