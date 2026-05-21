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
# 1. Prepare data
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

subset_names <- unique(metadata$reporting_name)[1:6]

abund_dat <- prep_metric_data(
  hab_data$total_abundance_samples,
  "total_abundance_sample") %>%
  # filter(reporting_name %in% c(subset_names)) %>%
  glimpse()

unique(abund_dat$reporting_name)

rich_dat <- prep_metric_data(
  hab_data$species_richness_samples,
  "n_species_sample"
)  %>%
  # filter(reporting_name %in% c(subset_names)) %>%
  glimpse()

shark_dat <- prep_metric_data(
  hab_data$shark_ray_richness_samples %>% left_join(metadata),
  "n_species_sample"
)  %>%
  # filter(reporting_name %in% c(subset_names)) %>%
  glimpse()

reef_dat <- prep_metric_data(
  hab_data$reef_associated_richness_samples %>% left_join(metadata),
  "n_species_sample"
)  %>%
  # filter(reporting_name %in% c(subset_names)) %>%
  glimpse()

unique(abund_dat$site)
unique(abund_dat$uwa_site_code)

# -----------------------------
# 2. Fit one reporting_name model
# -----------------------------

fit_one_reporting_name <- function(df,
                                   response_col,
                                   metric_name,
                                   use_site = FALSE) {
  
  # Skip areas without enough data
  if (
    nrow(df) < 20 ||
    n_distinct(df$Period) < 1 ||
    n_distinct(df$Status) < 1 ||
    n_distinct(df$year) < 1
  ) {
    stop("Not enough data to fit model")
  }
  
  area_reporting_name <- df %>%
    distinct(reporting_name) %>%
    pull(reporting_name) %>%
    first()
  
  has_two_status <- n_distinct(df$Status) > 1
  
  rand_effects <- if (use_site) {
    "(1 | uwa_site_code) + (1 | year)"
  } else {
    "(1 | year)"
  }
  
  # Build formula dynamically
  if (has_two_status) {
    
    fixed_effects <- "Period * Status"
    
    model_used <- if (use_site) {
      "Period * Status + site random effect + year random effect"
    } else {
      "Period * Status + year random effect"
    }
    
  } else {
    
    fixed_effects <- "Period"
    
    model_used <- if (use_site) {
      "Period only + site random effect + year random effect"
    } else {
      "Period only + year random effect"
    }
    
  }
  
  form <- as.formula(
    paste0(response_col, " ~ ", fixed_effects, " + ", rand_effects)
  )
  
  family_to_use <- #if (response_col == "n_species_sample") {
    nbinom2(link = "log")
  #} else {
  #  tweedie(link = "log")
  #}
  
  model <- glmmTMB(
    form,
    data = df,
    family = family_to_use,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS")
    )
  )
  
  model_summary <- summary(model)
  
  # Period means (always works)
  
  period_means <- emmeans(
    model,
    ~ Period,
    type = "response",
    weights = "proportional"
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = "Period averaged over Status"
    )
  
  
  # Period x Status means ONLY if Status exists
  
  if (has_two_status) {
    
    period_status_means <- emmeans(
      model,
      ~ Period * Status,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
    
  } else {
    
    period_status_means <- emmeans(
      model,
      ~ Period,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(
        Status = as.character(unique(df$Status)[1])
      )
    
  }
  
  
  # Add metadata after both pathways
  
  period_status_means <- period_status_means %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = ifelse(
        has_two_status,
        "Period by Status",
        "Period only (single Status)"
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
# 3. Run models across all areas
# -----------------------------

# run_metric_models <- function(df, response_col, metric_name, use_site = FALSE) {
#   
#   split_dat <- df %>%
#     group_by(reporting_name) %>%
#     group_split()
#   
#   names(split_dat) <- map_chr(split_dat, ~ unique(.x$reporting_name))
#   
#   # with_progress({
#     
#     # p <- progressor(
#     #   steps = length(split_dat)
#     # )
#     
#     model_outputs <- map(
#       split_dat,
#       ~ {
#         area_name <- unique(.x$reporting_name)
#         
#         # p(message = paste("Fitting", metric_name, "for", area_name))
#         
#         safely(
#           fit_one_reporting_name
#         )(
#           .x,
#           response_col,
#           metric_name,
#           use_site = use_site
#         )
#       }
#     )
#   # })
#   
#   period_means <- map_dfr(
#     model_outputs,
#     ~ if (is.null(.x$error)) .x$result$period_means else NULL
#   )
#   
#   period_status_means <- map_dfr(
#     model_outputs,
#     ~ if (is.null(.x$error)) .x$result$period_status_means else NULL
#   )
#   
#   model_errors <- imap_dfr(
#     model_outputs,
#     ~ {
#       if (!is.null(.x$error)) {
#         tibble(
#           reporting_name = .y,
#           metric = metric_name,
#           error = .x$error$message
#         )
#       } else {
#         NULL
#       }
#     }
#   )
#   
#   list(
#     model_outputs = model_outputs,
#     period_means = period_means,
#     period_status_means = period_status_means,
#     model_errors = model_errors
#   )
# }

# -----------------------------
# 4. Run both metrics
# -----------------------------
# 
# abund_models <- run_metric_models(
#   abund_dat,
#   response_col = "total_abundance_sample",
#   metric_name = "Total abundance",
#   use_site = TRUE
# )
# 
# rich_models <- run_metric_models(
#   rich_dat,
#   response_col = "n_species_sample",
#   metric_name = "Species richness",
#   use_site = FALSE
# )
# 
# shark_models <- run_metric_models(
#   shark_dat,
#   response_col = "n_species_sample",
#   metric_name = "Shark and ray richness",
#   use_site = FALSE
# )
# 
# reef_models <- run_metric_models(
#   reef_dat,
#   response_col = "n_species_sample",
#   metric_name = "Reef associated species richness",
#   use_site = FALSE
# )
# 
# # -----------------------------
# # 5. Combine dashboard-ready outputs
# # -----------------------------
# 
# period_results <- bind_rows(
#   abund_models$period_means,
#   rich_models$period_means,
#   shark_models$period_means,
#   reef_models$period_means
#   
# )
# 
# period_status_results <- bind_rows(
#   abund_models$period_status_means,
#   rich_models$period_status_means,
#   shark_models$period_status_means,
#   reef_models$period_status_means
# )
# 
# model_errors <- bind_rows(
#   abund_models$model_errors,
#   rich_models$model_errors,
#   shark_models$model_errors,
#   reef_models$model_errors
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


# Different way
# -----------------------------
# 3. Run models across all areas with debugging + incremental saving
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
# Total abundance
# -----------------------------

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
# Species richness
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
# Shark and ray richness
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
# Reef associated richness
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


# # -----------------------------
# # 5. Combine dashboard-ready outputs
# # -----------------------------

period_results <- bind_rows(
  abund_models$period_means,
  rich_models$period_means,
  shark_models$period_means,
  reef_models$period_means

)

period_status_results <- bind_rows(
  abund_models$period_status_means,
  rich_models$period_status_means,
  shark_models$period_status_means,
  reef_models$period_status_means
)

model_errors <- bind_rows(
  abund_models$model_errors,
  rich_models$model_errors,
  shark_models$model_errors,
  reef_models$model_errors
)

# Check any models that failed
model_errors

# Plot 1: Pre-bloom vs Bloom for all areas
# shared colours for pre/post everywhere
metric_period_cols <- c(
  "Pre-bloom"  = "#072759",  # same blue
  "Bloom" = "#e88e98"   # same orange
)

ggplot(period_results, aes(x = Period, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = response - SE, ymax = response + SE),
    width = 0.15
  ) +
  facet_wrap(metric ~ reporting_name, scales = "free_y") +
  theme_bw()

# Trying to make it look like the dashboard ----
ggplot(period_results, aes(x = Period, y = response, fill = Period)) +
  geom_col(width = 0.6, colour = "black", alpha = 0.85) +
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = metric_period_cols) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(metric ~ reporting_name, scales = "free_y")

# Plot 2: Pre-bloom vs Bloom by Fished vs No-take
ggplot(period_status_results, aes(x = Period, y = response, colour = Status)) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 2
  ) +
  geom_errorbar(
    aes(ymin = response - SE, ymax = response + SE),
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  facet_wrap(metric ~ reporting_name, scales = "free_y") +
  theme_bw()

# Create output folder
dir.create("plots/period_results", recursive = TRUE, showWarnings = FALSE)

# Get unique reporting regions
reporting_regions <- unique(period_results$reporting_name)
reporting_regions

# Loop through regions
for (region in reporting_regions) {
  
  message("Plotting: ", region)
  
  plot_df <- period_results %>%
    filter(reporting_name == region)
  
  p <- ggplot(
    plot_df,
    aes(x = Period, y = response, fill = Period)
  ) +
    geom_col(
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = response - SE,
        ymax = response + SE
      ),
      width = 0.2,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = metric_period_cols) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    ) +
    facet_wrap(~ metric, scales = "free_y") +
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
      "plots/period_results",
      paste0(safe_name, ".png")
    ),
    plot = p,
    width = 12,
    height = 8,
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
