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
library(tidyr)

sf::sf_use_s2()

# -----------------------------
# Colours and labels
# -----------------------------

metric_period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

status_cols <- c(
  "Fished" = "#d95f02",
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
      start_date_fct = droplevels(factor(start_date_date)),
      site = factor(sample)
    )
}

# Read in data
load("app_data/hab_data.Rdata")

metadata <- hab_data$hab_combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(method == "BRUVs")

abund_dat <- prep_metric_data(
  hab_data$total_abundance_samples,
  "total_abundance_sample"
) %>%
  sf::st_drop_geometry()

rich_dat <- prep_metric_data(
  hab_data$species_richness_samples,
  "n_species_sample"
) %>%
  sf::st_drop_geometry()

shark_dat <- prep_metric_data(
  hab_data$shark_ray_richness_samples %>%
    left_join(metadata),
  "n_species_sample"
) %>%
  sf::st_drop_geometry()

reef_dat <- prep_metric_data(
  hab_data$reef_associated_richness_samples %>%
    left_join(metadata),
  "n_species_sample"
) %>%
  sf::st_drop_geometry()

shannon_dat <- prep_metric_data(
  hab_data$shannon_diversity_samples %>%
    left_join(metadata),
  "shannon"
) %>%
  sf::st_drop_geometry()

fish_200_dat <- prep_metric_data(
  hab_data$fish_200_abundance_samples %>%
    left_join(metadata),
  "total_abundance_sample"
) %>%
  sf::st_drop_geometry()

# Optional zero summary for checking individual sampling dates
find_zero_dates <- function(df, response_col, threshold = 0.9) {
  df %>%
    group_by(reporting_name, start_date_date) %>%
    summarise(
      prop_zero = mean(.data[[response_col]] == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(skip_date = prop_zero >= threshold)
}

shark_zero_dates <- find_zero_dates(
  shark_dat,
  "n_species_sample"
)

reef_zero_dates <- find_zero_dates(
  reef_dat,
  "n_species_sample"
)

fish200_zero_dates <- find_zero_dates(
  fish_200_dat,
  "total_abundance_sample"
)

# -----------------------------
# 2. Fit one region
# -----------------------------

fit_one_region <- function(
    df,
    response_col,
    metric_name,
    use_site = FALSE
) {
  if (nrow(df) < 10) {
    stop("Not enough data")
  }

  # Skip the complete regional metric only when MORE than 90%
  # of all observations are zero. The >=90% rule below applies
  # separately to individual dates in the temporal model.
  prop_zero <- mean(
    df[[response_col]] == 0,
    na.rm = TRUE
  )

  if (prop_zero > 0.9) {
    return(
      list(
        skipped = TRUE,
        reason = "More than 90% zeros",
        prop_zero = prop_zero,
        reporting_name = unique(df$reporting_name)[1],
        metric = metric_name
      )
    )
  }

  area_name <- unique(df$reporting_name)[1]

  df <- df %>%
    mutate(
      Period = droplevels(Period),
      Status = droplevels(Status),
      start_date_fct = droplevels(start_date_fct)
    )

  has_two_periods <- n_distinct(df$Period) >= 2
  has_two_status <- n_distinct(df$Status) >= 2
  has_two_dates <- n_distinct(df$start_date_fct) >= 2

  has_multiple_sites <-
    use_site &&
    "uwa_site_code" %in% names(df) &&
    n_distinct(df$uwa_site_code, na.rm = TRUE) > 1

  site_re <- if (has_multiple_sites) {
    " + (1 | uwa_site_code)"
  } else {
    ""
  }

  message("Reporting region: ", area_name)
  message("Metric: ", metric_name)
  message("Number of observations: ", nrow(df))
  message("Number of sampling dates: ", n_distinct(df$start_date_fct))
  message("Number of statuses: ", n_distinct(df$Status))
  message("Including site random effect: ", has_multiple_sites)

  # ==========================================================
  # Period model: always nbinom2
  # ==========================================================

  period_fixed <- case_when(
    has_two_periods && has_two_status ~ "Period * Status",
    has_two_periods ~ "Period",
    has_two_status ~ "Status",
    TRUE ~ "1"
  )

  dates_per_period <- df %>%
    distinct(Period, start_date_fct) %>%
    count(Period, name = "n_dates")

  date_confounded_with_period <-
    has_two_periods &&
    all(dates_per_period$n_dates == 1)

  include_date_random_effect <-
    has_two_dates &&
    !date_confounded_with_period

  period_date_re <- if (include_date_random_effect) {
    " + (1 | start_date_fct)"
  } else {
    ""
  }

  period_form <- as.formula(
    paste0(
      response_col,
      " ~ ",
      period_fixed,
      period_date_re,
      site_re
    )
  )

  message("Period model: ", paste(deparse(period_form), collapse = ""))

  period_model <- glmmTMB(
    formula = period_form,
    data = df,
    family = nbinom2(link = "log")
  )

  # Period means
  if (has_two_periods) {
    period_means <- emmeans(
      period_model,
      ~ Period,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
  } else {
    single_period <- as.character(unique(df$Period)[1])

    period_means <- emmeans(
      period_model,
      ~ 1,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()

    period_means[["Period"]] <- single_period
  }

  period_means <- period_means %>%
    mutate(
      reporting_name = area_name,
      metric = metric_name
    )

  # Period-by-status means
  if (has_two_periods && has_two_status) {
    period_status_means <- emmeans(
      period_model,
      ~ Period * Status,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()
  } else if (has_two_periods) {
    single_status <- as.character(unique(df$Status)[1])

    period_status_means <- emmeans(
      period_model,
      ~ Period,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()

    period_status_means[["Status"]] <- single_status
  } else if (has_two_status) {
    single_period <- as.character(unique(df$Period)[1])

    period_status_means <- emmeans(
      period_model,
      ~ Status,
      type = "response"
    ) %>%
      as.data.frame() %>%
      as_tibble()

    period_status_means[["Period"]] <- single_period
  } else {
    single_period <- as.character(unique(df$Period)[1])
    single_status <- as.character(unique(df$Status)[1])

    period_status_means <- emmeans(
      period_model,
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
      reporting_name = area_name,
      metric = metric_name
    )

  # ==========================================================
  # Temporal model: always nbinom2
  # ==========================================================

  temporal_df <- df %>%
    group_by(start_date_fct) %>%
    mutate(
      prop_zero_date = mean(
        .data[[response_col]] == 0,
        na.rm = TRUE
      )
    ) %>%
    ungroup()

  # Exclude dates with 90% OR MORE zeros
  excluded_dates <- temporal_df %>%
    filter(prop_zero_date >= 0.9) %>%
    distinct(
      start_date_fct,
      start_date_date,
      prop_zero_date
    ) %>%
    mutate(
      reporting_name = area_name,
      metric = metric_name,
      exclusion_reason = "Not modelled\n(>=90% zeros)"
    )

  # Retain dates with less than 90% zeros
  temporal_df <- temporal_df %>%
    filter(prop_zero_date < 0.9) %>%
    mutate(
      start_date_fct = droplevels(start_date_fct),
      Period = droplevels(Period),
      Status = droplevels(Status)
    )

  message("Temporal observations remaining: ", nrow(temporal_df))
  message("Temporal dates excluded: ", nrow(excluded_dates))

  temporal_model <- NULL
  temporal_error <- NA_character_
  temporal_has_two_dates <- FALSE
  temporal_has_two_status <- FALSE
  temporal_has_complete_date_status <- FALSE
  start_date_means <- tibble()
  start_date_status_means <- tibble()

  if (nrow(temporal_df) == 0) {
    temporal_error <- paste(
      "No observations remained after excluding",
      "dates with 90% or more zeros"
    )

    message("Temporal model not fitted: ", temporal_error)
  } else {
    # Recalculate the factor structure AFTER removing dates.
    temporal_has_two_dates <-
      n_distinct(temporal_df$start_date_fct) >= 2

    temporal_has_two_status <-
      n_distinct(temporal_df$Status) >= 2

    if (temporal_has_two_dates && temporal_has_two_status) {
      temporal_date_status_check <- temporal_df %>%
        count(start_date_fct, Status) %>%
        complete(
          start_date_fct,
          Status,
          fill = list(n = 0)
        )

      temporal_has_complete_date_status <-
        all(temporal_date_status_check$n > 0)
    }

    temporal_has_multiple_sites <-
      use_site &&
      "uwa_site_code" %in% names(temporal_df) &&
      n_distinct(temporal_df$uwa_site_code, na.rm = TRUE) > 1

    temporal_site_re <- if (temporal_has_multiple_sites) {
      " + (1 | uwa_site_code)"
    } else {
      ""
    }

    temporal_fixed <- case_when(
      temporal_has_two_dates &&
        temporal_has_two_status &&
        temporal_has_complete_date_status ~
        "start_date_fct * Status",
      temporal_has_two_dates ~ "start_date_fct",
      temporal_has_two_status ~ "Status",
      TRUE ~ "1"
    )

    temporal_form <- as.formula(
      paste0(
        response_col,
        " ~ ",
        temporal_fixed,
        temporal_site_re
      )
    )

    message("Temporal model: ", paste(deparse(temporal_form), collapse = ""))

    temporal_fit <- tryCatch(
      glmmTMB(
        formula = temporal_form,
        data = temporal_df,
        family = nbinom2(link = "log")
      ),
      error = function(e) e
    )

    if (inherits(temporal_fit, "error")) {
      temporal_error <- conditionMessage(temporal_fit)
      message("Temporal model failed: ", temporal_error)
    } else {
      temporal_model <- temporal_fit

      # Keep errors from emmeans confined to the temporal model so
      # a temporal failure does not remove valid period results.
      temporal_emmeans <- tryCatch(
        {
          date_lookup <- temporal_df %>%
            distinct(start_date_fct, start_date_date) %>%
            mutate(start_date_fct = as.character(start_date_fct))

          period_lookup <- temporal_df %>%
            distinct(start_date_date, Period)

          if (temporal_has_two_dates) {
            date_means <- emmeans(
              temporal_model,
              ~ start_date_fct,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()
          } else {
            single_date <-
              as.character(unique(temporal_df$start_date_fct)[1])

            date_means <- emmeans(
              temporal_model,
              ~ 1,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()

            date_means[["start_date_fct"]] <- single_date
          }

          date_means <- date_means %>%
            mutate(start_date_fct = as.character(start_date_fct)) %>%
            left_join(date_lookup, by = "start_date_fct") %>%
            left_join(period_lookup, by = "start_date_date") %>%
            mutate(
              reporting_name = area_name,
              metric = metric_name
            )

          if (
            temporal_has_two_dates &&
              temporal_has_two_status &&
              temporal_has_complete_date_status
          ) {
            date_status_means <- emmeans(
              temporal_model,
              ~ start_date_fct * Status,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()
          } else if (temporal_has_two_dates) {
            date_status_means <- emmeans(
              temporal_model,
              ~ start_date_fct,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()

            date_status_means[["Status"]] <-
              "Not modelled by Status"
          } else if (temporal_has_two_status) {
            single_date <-
              as.character(unique(temporal_df$start_date_fct)[1])

            date_status_means <- emmeans(
              temporal_model,
              ~ Status,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()

            date_status_means[["start_date_fct"]] <- single_date
          } else {
            single_date <-
              as.character(unique(temporal_df$start_date_fct)[1])

            single_status <-
              as.character(unique(temporal_df$Status)[1])

            date_status_means <- emmeans(
              temporal_model,
              ~ 1,
              type = "response"
            ) %>%
              as.data.frame() %>%
              as_tibble()

            date_status_means[["start_date_fct"]] <- single_date
            date_status_means[["Status"]] <- single_status
          }

          date_status_means <- date_status_means %>%
            mutate(start_date_fct = as.character(start_date_fct)) %>%
            left_join(date_lookup, by = "start_date_fct") %>%
            left_join(period_lookup, by = "start_date_date") %>%
            mutate(
              reporting_name = area_name,
              metric = metric_name
            )

          list(
            start_date_means = date_means,
            start_date_status_means = date_status_means
          )
        },
        error = function(e) e
      )

      if (inherits(temporal_emmeans, "error")) {
        temporal_error <- conditionMessage(temporal_emmeans)
        message("Temporal marginal means failed: ", temporal_error)
      } else {
        start_date_means <- temporal_emmeans$start_date_means
        start_date_status_means <-
          temporal_emmeans$start_date_status_means
      }
    }
  }

  list(
    skipped = FALSE,
    period_model = period_model,
    temporal_model = temporal_model,
    period_means = period_means,
    period_status_means = period_status_means,
    start_date_means = start_date_means,
    start_date_status_means = start_date_status_means,
    has_complete_date_status = temporal_has_complete_date_status,
    temporal_has_two_dates = temporal_has_two_dates,
    temporal_has_two_status = temporal_has_two_status,
    excluded_dates = excluded_dates,
    temporal_error = temporal_error
  )
}

# -----------------------------
# 3. Run across regions
# -----------------------------

run_metric_models <- function(
    df,
    response_col,
    metric_name,
    use_site = TRUE
) {
  split_dat <- split(df, df$reporting_name)

  outputs <- map(
    split_dat,
    ~ tryCatch(
      fit_one_region(
        df = .x,
        response_col = response_col,
        metric_name = metric_name,
        use_site = use_site
      ),
      error = function(e) e
    )
  )

  list(
    outputs = outputs,

    zero_summary = imap_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          isTRUE(.x$skipped)
      ) {
        tibble(
          reporting_name = .x$reporting_name,
          metric = .x$metric,
          prop_zero = .x$prop_zero,
          percent_zero = round(.x$prop_zero * 100, 1),
          reason = .x$reason
        )
      }
    }),

    period_means = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        .x$period_means
      }
    }),

    period_status_means = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        .x$period_status_means
      }
    }),

    start_date_means = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        .x$start_date_means
      }
    }),

    start_date_status_means = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        .x$start_date_status_means
      }
    }),

    excluded_dates = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        .x$excluded_dates
      }
    }),

    errors = imap_dfr(outputs, ~ {
      if (inherits(.x, "error")) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = conditionMessage(.x)
        )
      }
    }),

    temporal_errors = imap_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped) &&
          length(.x$temporal_error) == 1 &&
          !is.na(.x$temporal_error)
      ) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = .x$temporal_error
        )
      }
    })
  )
}

# -----------------------------
# 4. Run all metrics
# -----------------------------

abund_models <- run_metric_models(
  abund_dat,
  "total_abundance_sample",
  "Total abundance",
  use_site = TRUE
)

rich_models <- run_metric_models(
  rich_dat,
  "n_species_sample",
  "Species richness",
  use_site = TRUE
)

shark_models <- run_metric_models(
  shark_dat,
  "n_species_sample",
  "Shark and ray richness",
  use_site = TRUE
)

reef_models <- run_metric_models(
  reef_dat,
  "n_species_sample",
  "Reef associated species richness",
  use_site = TRUE
)

shannon_models <- run_metric_models(
  shannon_dat,
  "shannon",
  "Shannon diversity",
  use_site = TRUE
)

fish_200_models <- run_metric_models(
  fish_200_dat,
  "total_abundance_sample",
  "Abundance > 200 mm",
  use_site = TRUE
)

# -----------------------------
# 5. Combine results
# -----------------------------

period_results <- bind_rows(
  abund_models$period_means,
  rich_models$period_means,
  shark_models$period_means,
  reef_models$period_means,
  shannon_models$period_means,
  fish_200_models$period_means
) %>%
  mutate(
    metric_id = recode(metric, !!!metric_lookup),
    Period = factor(Period, levels = c("Pre-bloom", "Bloom")),
    response = as.numeric(response),
    SE = as.numeric(SE),
    asymp.LCL = as.numeric(asymp.LCL),
    asymp.UCL = as.numeric(asymp.UCL)
  )

period_status_results <- bind_rows(
  abund_models$period_status_means,
  rich_models$period_status_means,
  shark_models$period_status_means,
  reef_models$period_status_means,
  shannon_models$period_status_means,
  fish_200_models$period_status_means
) %>%
  mutate(
    metric_id = recode(metric, !!!metric_lookup),
    Period = factor(Period, levels = c("Pre-bloom", "Bloom")),
    response = as.numeric(response),
    SE = as.numeric(SE),
    asymp.LCL = as.numeric(asymp.LCL),
    asymp.UCL = as.numeric(asymp.UCL)
  )

start_date_results <- bind_rows(
  abund_models$start_date_means,
  rich_models$start_date_means,
  shark_models$start_date_means,
  reef_models$start_date_means,
  shannon_models$start_date_means,
  fish_200_models$start_date_means
) %>%
  mutate(
    metric_id = recode(metric, !!!metric_lookup),
    response = as.numeric(response),
    SE = as.numeric(SE),
    asymp.LCL = as.numeric(asymp.LCL),
    asymp.UCL = as.numeric(asymp.UCL)
  )

start_date_status_results <- bind_rows(
  abund_models$start_date_status_means,
  rich_models$start_date_status_means,
  shark_models$start_date_status_means,
  reef_models$start_date_status_means,
  shannon_models$start_date_status_means,
  fish_200_models$start_date_status_means
) %>%
  mutate(
    metric_id = recode(metric, !!!metric_lookup),
    response = as.numeric(response),
    SE = as.numeric(SE),
    asymp.LCL = as.numeric(asymp.LCL),
    asymp.UCL = as.numeric(asymp.UCL)
  )

zero_summary <- bind_rows(
  abund_models$zero_summary,
  rich_models$zero_summary,
  shark_models$zero_summary,
  reef_models$zero_summary,
  shannon_models$zero_summary,
  fish_200_models$zero_summary
)

excluded_dates_df <- bind_rows(
  abund_models$excluded_dates,
  rich_models$excluded_dates,
  shark_models$excluded_dates,
  reef_models$excluded_dates,
  shannon_models$excluded_dates,
  fish_200_models$excluded_dates
) %>%
  mutate(metric_id = recode(metric, !!!metric_lookup))

model_errors <- bind_rows(
  abund_models$errors,
  rich_models$errors,
  shark_models$errors,
  reef_models$errors,
  shannon_models$errors,
  fish_200_models$errors
)

temporal_errors <- bind_rows(
  abund_models$temporal_errors,
  rich_models$temporal_errors,
  shark_models$temporal_errors,
  reef_models$temporal_errors,
  shannon_models$temporal_errors,
  fish_200_models$temporal_errors
)

# Replace infinite confidence limits before saving or plotting
period_results <- period_results %>%
  mutate(
    asymp.LCL = ifelse(is.finite(asymp.LCL), asymp.LCL, NA_real_),
    asymp.UCL = ifelse(is.finite(asymp.UCL), asymp.UCL, NA_real_)
  )

period_status_results <- period_status_results %>%
  mutate(
    asymp.LCL = ifelse(is.finite(asymp.LCL), asymp.LCL, NA_real_),
    asymp.UCL = ifelse(is.finite(asymp.UCL), asymp.UCL, NA_real_)
  )

start_date_results <- start_date_results %>%
  mutate(
    asymp.LCL = ifelse(is.finite(asymp.LCL), asymp.LCL, NA_real_),
    asymp.UCL = ifelse(is.finite(asymp.UCL), asymp.UCL, NA_real_)
  )

start_date_status_results <- start_date_status_results %>%
  mutate(
    asymp.LCL = ifelse(is.finite(asymp.LCL), asymp.LCL, NA_real_),
    asymp.UCL = ifelse(is.finite(asymp.UCL), asymp.UCL, NA_real_)
  )

# -----------------------------
# 6. Save model outputs
# -----------------------------

dir.create(
  "model_results",
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_excel_csv(
  period_results,
  "model_results/period_results.csv"
)

readr::write_excel_csv(
  period_status_results,
  "model_results/period_status_results.csv"
)

readr::write_excel_csv(
  start_date_results,
  "model_results/start_date_results.csv"
)

readr::write_excel_csv(
  start_date_status_results,
  "model_results/start_date_status_results.csv"
)

readr::write_excel_csv(
  zero_summary,
  "model_results/zero_summary.csv"
)

readr::write_excel_csv(
  excluded_dates_df,
  "model_results/excluded_dates.csv"
)

readr::write_excel_csv(
  model_errors,
  "model_results/model_errors.csv"
)

readr::write_excel_csv(
  temporal_errors,
  "model_results/temporal_errors.csv"
)

writexl::write_xlsx(
  list(
    period_results = period_results,
    period_status_results = period_status_results,
    start_date_results = start_date_results,
    start_date_status_results = start_date_status_results,
    zero_summary = zero_summary,
    excluded_dates = excluded_dates_df,
    model_errors = model_errors,
    temporal_errors = temporal_errors
  ),
  "model_results/model_results.xlsx"
)

# -----------------------------
# 7. Plot helpers
# -----------------------------

blank_panel <- function(panel_letter, label = "More than 90% zeros") {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = label,
      size = 6,
      fontface = "italic"
    ) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void() +
    labs(tag = panel_letter) +
    theme(plot.tag = element_text(size = 18))
}

plot_period <- function(df, metric_id, panel_letter) {
  metric_df <- df %>%
    filter(.data$metric_id == !!metric_id)

  if (nrow(metric_df) == 0) {
    return(blank_panel(panel_letter))
  }

  ggplot(metric_df, aes(x = Period, y = response, fill = Period)) +
    geom_col(width = 0.6, colour = "black", alpha = 0.85) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 0.2,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = metric_period_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      tag = panel_letter,
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(legend.position = "none")
}

plot_period_status <- function(df, metric_id, panel_letter) {
  metric_df <- df %>%
    filter(.data$metric_id == !!metric_id)

  if (nrow(metric_df) == 0) {
    return(blank_panel(panel_letter))
  }

  ggplot(metric_df, aes(x = Period, y = response, fill = Status)) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      position = position_dodge(width = 0.7),
      width = 0.18,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      tag = panel_letter,
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(legend.position = "bottom")
}

plot_start_date <- function(df, metric_id, panel_letter) {
  metric_df <- df %>%
    filter(.data$metric_id == !!metric_id) %>%
    arrange(start_date_date)

  if (nrow(metric_df) == 0) {
    return(blank_panel(panel_letter))
  }

  excluded_plot_df <- excluded_dates_df %>%
    filter(
      reporting_name == unique(metric_df$reporting_name),
      metric == unique(metric_df$metric)
    )

  annotation_y <- max(metric_df$response, na.rm = TRUE) * 0.35

  if (!is.finite(annotation_y) || annotation_y == 0) {
    annotation_y <- 0.1
  }

  ggplot(metric_df, aes(x = start_date_date, y = response, fill = Period)) +
    geom_col(width = 120, colour = "black", alpha = 0.85) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 40,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = metric_period_cols, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      tag = panel_letter,
      fill = NULL
    ) +
    geom_text(
      data = excluded_plot_df,
      aes(
        x = start_date_date,
        y = annotation_y,
        label = ">=90% zeros"
      ),
      inherit.aes = FALSE,
      size = 3,
      fontface = "italic",
      angle = 90,
      hjust = 0.5,
      vjust = 0.5
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
}

plot_start_date_status <- function(df, metric_id, panel_letter) {
  metric_df <- df %>%
    filter(.data$metric_id == !!metric_id) %>%
    arrange(start_date_date)

  if (nrow(metric_df) == 0) {
    return(blank_panel(panel_letter))
  }

  ggplot(metric_df, aes(x = start_date_date, y = response, fill = Status)) +
    geom_col(
      position = position_dodge(width = 120),
      width = 100,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      position = position_dodge(width = 120),
      width = 35,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(
      x = NULL,
      y = metric_y_lab[[metric_id]],
      tag = panel_letter,
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    plot_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
}

# -----------------------------
# 8. Save four plot types
# -----------------------------

save_patchwork_plots <- function(
    results_df,
    plot_fun,
    output_dir,
    suffix,
    title_suffix,
    width = 8
) {
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  for (region in unique(results_df$reporting_name)) {
    plot_df <- results_df %>%
      filter(reporting_name == region)

    plots <- map2(
      metric_order,
      LETTERS[seq_along(metric_order)],
      ~ plot_fun(plot_df, .x, .y)
    )

    p <- wrap_plots(
      plots,
      ncol = 2,
      guides = "collect"
    ) &
      theme(
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom"
      )

    safe_name <- region %>%
      str_replace_all("[^A-Za-z0-9]+", "_") %>%
      str_replace_all("_$", "")

    ggsave(
      filename = file.path(
        output_dir,
        paste0(safe_name, "_", suffix, ".png")
      ),
      plot = p,
      width = width,
      height = 10,
      dpi = 300
    )
  }
}

save_patchwork_plots(
  period_results,
  plot_period,
  "plots/20260724_nbinom2/period_results",
  "period",
  "period means"
)

save_patchwork_plots(
  period_status_results,
  plot_period_status,
  "plots/20260724_nbinom2/period_status_results",
  "period_status",
  "period means by status"
)

save_patchwork_plots(
  start_date_results,
  plot_start_date,
  "plots/20260724_nbinom2/start_date_results",
  "start_date",
  "temporal results"
)

save_patchwork_plots(
  start_date_status_results,
  plot_start_date_status,
  "plots/20260724_nbinom2/start_date_status_results",
  "start_date_status",
  "temporal results by status",
  width = 12
)

# -----------------------------
# 9. Optional checks
# -----------------------------

expected_locations <- sort(unique(abund_dat$reporting_name))
actual_locations <- sort(unique(start_date_results$reporting_name))
missing_temporal_locations <- setdiff(expected_locations, actual_locations)

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


uncertainty_issues <- bind_rows(
  period_results %>%
    mutate(result_type = "Period"),
  
  period_status_results %>%
    mutate(result_type = "Period by status"),
  
  start_date_results %>%
    mutate(result_type = "Temporal"),
  
  start_date_status_results %>%
    mutate(result_type = "Temporal by status")
) %>%
  filter(
    is.na(SE) |
      !is.finite(SE) |
      is.na(asymp.LCL) |
      !is.finite(asymp.LCL) |
      is.na(asymp.UCL) |
      !is.finite(asymp.UCL)
  ) %>%
  select(
    reporting_name,
    metric,
    result_type,
    any_of(c(
      "Period",
      "Status",
      "start_date_date"
    )),
    response,
    SE,
    asymp.LCL,
    asymp.UCL
  )

uncertainty_issues