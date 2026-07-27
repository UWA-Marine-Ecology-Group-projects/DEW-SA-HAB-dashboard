# ============================================================
# START DATE MODELS + FOUR PLOTS PER LOCATION
# Automatic negative-binomial -> Poisson fallback for counts
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
# Model-selection settings
# -----------------------------

# nbinom2 approaches a Poisson model as theta becomes very large.
# This threshold is deliberately conservative and can be adjusted.
theta_poisson_threshold <- 1e6

# A Poisson fallback is accepted only when its approximate Pearson
# dispersion ratio is no greater than this value.
poisson_dispersion_limit <- 1.5

# When nbinom2 is at the Poisson boundary, allow the simpler Poisson
# model when its AIC is no more than this amount above the nbinom2 AIC.
poisson_aic_tolerance <- 2

plot_output_root <- "plots/20260727_auto_family"

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
    dplyr::mutate(
      status = if_else(sample %in% "OASO04_2510", "Fished", status),
      status = if_else(uwa_site_code %in% "45", "No-take", status)
    ) %>%
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
  filter(method == "BRUVs") %>%
  dplyr::mutate(
    status = if_else(sample %in% "OASO04_2510", "Fished", status),
    status = if_else(uwa_site_code %in% "45", "No-take", status)
  )

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
# 2. Family-selection helpers
# -----------------------------

combine_messages <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    NA_character_
  } else {
    paste(unique(values), collapse = " | ")
  }
}

formula_text <- function(x) {
  paste(deparse(x), collapse = "")
}

fit_glmmTMB_safely <- function(formula, data, family) {
  captured_warnings <- character()

  fit <- withCallingHandlers(
    tryCatch(
      glmmTMB(
        formula = formula,
        data = data,
        family = family
      ),
      error = function(e) e
    ),
    warning = function(w) {
      captured_warnings <<- c(
        captured_warnings,
        conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(fit, "error")) {
    return(
      list(
        model = NULL,
        error = conditionMessage(fit),
        warnings = combine_messages(captured_warnings)
      )
    )
  }

  list(
    model = fit,
    error = NA_character_,
    warnings = combine_messages(captured_warnings)
  )
}

assess_model_fit <- function(model) {
  if (is.null(model)) {
    return(
      list(
        converged = FALSE,
        pdHess = FALSE,
        finite_standard_errors = FALSE,
        AIC = NA_real_,
        logLik = NA_real_,
        valid = FALSE
      )
    )
  }

  fixed_se <- tryCatch(
    sqrt(diag(vcov(model)$cond)),
    error = function(e) NA_real_
  )

  converged <- isTRUE(model$fit$convergence == 0)
  pd_hess <- isTRUE(model$sdr$pdHess)
  finite_se <- length(fixed_se) > 0 && all(is.finite(fixed_se))

  model_aic <- tryCatch(
    as.numeric(AIC(model)),
    error = function(e) NA_real_
  )

  model_loglik <- tryCatch(
    as.numeric(logLik(model)),
    error = function(e) NA_real_
  )

  list(
    converged = converged,
    pdHess = pd_hess,
    finite_standard_errors = finite_se,
    AIC = model_aic,
    logLik = model_loglik,
    valid = converged &&
      pd_hess &&
      finite_se &&
      is.finite(model_aic)
  )
}

pearson_dispersion_ratio <- function(model) {
  if (is.null(model)) {
    return(NA_real_)
  }

  pearson_residuals <- tryCatch(
    residuals(model, type = "pearson"),
    error = function(e) NA_real_
  )

  residual_df <- tryCatch(
    df.residual(model),
    error = function(e) NA_real_
  )

  if (
    length(pearson_residuals) == 0 ||
      !is.finite(residual_df) ||
      residual_df <= 0 ||
      all(!is.finite(pearson_residuals))
  ) {
    return(NA_real_)
  }

  sum(pearson_residuals^2, na.rm = TRUE) / residual_df
}

standardise_emmeans <- function(x) {
  output <- x %>%
    as.data.frame() %>%
    as_tibble()

  estimate_column <- intersect(
    c("response", "rate", "prob", "emmean"),
    names(output)
  )[1]

  lower_column <- intersect(
    c("asymp.LCL", "lower.CL"),
    names(output)
  )[1]

  upper_column <- intersect(
    c("asymp.UCL", "upper.CL"),
    names(output)
  )[1]

  if (
    is.na(estimate_column) ||
      is.na(lower_column) ||
      is.na(upper_column)
  ) {
    stop(
      "Could not identify the estimate or confidence-limit columns ",
      "returned by emmeans. Columns were: ",
      paste(names(output), collapse = ", ")
    )
  }

  names(output)[names(output) == estimate_column] <- "response"
  names(output)[names(output) == lower_column] <- "asymp.LCL"
  names(output)[names(output) == upper_column] <- "asymp.UCL"

  output
}

fit_count_model <- function(
    formula,
    data,
    area_name,
    metric_name,
    model_type,
    theta_threshold = theta_poisson_threshold,
    poisson_dispersion_max = poisson_dispersion_limit,
    aic_tolerance = poisson_aic_tolerance
) {
  nb_fit <- fit_glmmTMB_safely(
    formula = formula,
    data = data,
    family = nbinom2(link = "log")
  )

  poisson_fit <- fit_glmmTMB_safely(
    formula = formula,
    data = data,
    family = poisson(link = "log")
  )

  nb_check <- assess_model_fit(nb_fit$model)
  poisson_check <- assess_model_fit(poisson_fit$model)

  nb_theta <- if (is.null(nb_fit$model)) {
    NA_real_
  } else {
    tryCatch(
      as.numeric(sigma(nb_fit$model)),
      error = function(e) NA_real_
    )
  }

  poisson_dispersion <- pearson_dispersion_ratio(
    poisson_fit$model
  )

  nb_at_poisson_boundary <-
    !is.na(nb_theta) &&
    (is.infinite(nb_theta) || nb_theta >= theta_threshold)

  poisson_dispersion_ok <-
    is.finite(poisson_dispersion) &&
    poisson_dispersion <= poisson_dispersion_max

  poisson_acceptable <-
    poisson_check$valid &&
    poisson_dispersion_ok

  aic_supports_poisson <-
    !is.finite(nb_check$AIC) ||
    !is.finite(poisson_check$AIC) ||
    poisson_check$AIC <= nb_check$AIC + aic_tolerance

  selected_model <- NULL
  family_code <- NA_character_
  selected_family <- NA_character_
  selection_reason <- NA_character_
  model_error <- NA_character_

  if (nb_check$valid && !nb_at_poisson_boundary) {
    selected_model <- nb_fit$model
    family_code <- "nbinom2"
    selected_family <- "Negative binomial (nbinom2)"
    selection_reason <- paste(
      "Negative binomial converged with finite uncertainty",
      "and did not approach the Poisson boundary"
    )
  } else if (!nb_check$valid && poisson_acceptable) {
    selected_model <- poisson_fit$model
    family_code <- "poisson"
    selected_family <- "Poisson"
    selection_reason <- paste(
      "Poisson fallback used because the negative binomial fit",
      "did not provide a reliable Hessian and standard errors;",
      "the Poisson dispersion check was acceptable"
    )
  } else if (
    nb_check$valid &&
      nb_at_poisson_boundary &&
      poisson_acceptable &&
      aic_supports_poisson
  ) {
    selected_model <- poisson_fit$model
    family_code <- "poisson"
    selected_family <- "Poisson"
    selection_reason <- paste(
      "Poisson used because the negative binomial dispersion parameter",
      "approached the Poisson boundary and the simpler Poisson model",
      "passed convergence, uncertainty, dispersion and AIC checks"
    )
  } else if (nb_check$valid) {
    selected_model <- nb_fit$model
    family_code <- "nbinom2"
    selected_family <- "Negative binomial (nbinom2)"

    if (nb_at_poisson_boundary && !poisson_dispersion_ok) {
      selection_reason <- paste(
        "Negative binomial retained because the Poisson model showed",
        "more residual dispersion than allowed"
      )
    } else if (nb_at_poisson_boundary && !aic_supports_poisson) {
      selection_reason <- paste(
        "Negative binomial retained because the Poisson AIC was",
        "meaningfully poorer"
      )
    } else {
      selection_reason <- paste(
        "Negative binomial retained because the Poisson fallback",
        "did not pass all selection checks"
      )
    }
  } else {
    model_error <- combine_messages(
      if (!is.na(nb_fit$error)) {
        paste0("Negative binomial error: ", nb_fit$error)
      },
      if (!nb_check$pdHess) {
        "Negative binomial Hessian was not positive definite"
      },
      if (!nb_check$finite_standard_errors) {
        "Negative binomial standard errors were not finite"
      },
      if (!is.na(poisson_fit$error)) {
        paste0("Poisson error: ", poisson_fit$error)
      },
      if (!poisson_check$pdHess) {
        "Poisson Hessian was not positive definite"
      },
      if (!poisson_check$finite_standard_errors) {
        "Poisson standard errors were not finite"
      },
      if (
        poisson_check$valid &&
          !poisson_dispersion_ok
      ) {
        paste0(
          "Poisson dispersion ratio was ",
          round(poisson_dispersion, 3),
          ", above the allowed value of ",
          poisson_dispersion_max
        )
      }
    )

    selection_reason <- "Neither family passed the required checks"
  }

  selected_check <- assess_model_fit(selected_model)

  diagnostics <- tibble(
    reporting_name = area_name,
    metric = metric_name,
    model_type = model_type,
    formula = formula_text(formula),
    n_observations = nrow(data),
    n_dates = n_distinct(data$start_date_fct),
    n_periods = n_distinct(data$Period),
    n_statuses = n_distinct(data$Status),
    n_sites = if ("uwa_site_code" %in% names(data)) {
      n_distinct(data$uwa_site_code, na.rm = TRUE)
    } else {
      NA_integer_
    },
    family_code = family_code,
    selected_family = selected_family,
    selection_reason = selection_reason,
    selected_model_valid = selected_check$valid,
    selected_pdHess = selected_check$pdHess,
    selected_finite_standard_errors =
      selected_check$finite_standard_errors,
    selected_AIC = selected_check$AIC,
    selected_logLik = selected_check$logLik,
    nb_valid = nb_check$valid,
    nb_converged = nb_check$converged,
    nb_pdHess = nb_check$pdHess,
    nb_finite_standard_errors =
      nb_check$finite_standard_errors,
    nb_theta = nb_theta,
    nb_at_poisson_boundary = nb_at_poisson_boundary,
    nb_AIC = nb_check$AIC,
    nb_logLik = nb_check$logLik,
    nb_warnings = nb_fit$warnings,
    nb_error = nb_fit$error,
    poisson_valid = poisson_check$valid,
    poisson_converged = poisson_check$converged,
    poisson_pdHess = poisson_check$pdHess,
    poisson_finite_standard_errors =
      poisson_check$finite_standard_errors,
    poisson_dispersion_ratio = poisson_dispersion,
    poisson_dispersion_ok = poisson_dispersion_ok,
    poisson_AIC = poisson_check$AIC,
    poisson_logLik = poisson_check$logLik,
    poisson_minus_nb_AIC = poisson_check$AIC - nb_check$AIC,
    poisson_warnings = poisson_fit$warnings,
    poisson_error = poisson_fit$error,
    model_error = model_error
  )

  list(
    model = selected_model,
    family_code = family_code,
    family_label = selected_family,
    selection_reason = selection_reason,
    diagnostics = diagnostics,
    error = model_error
  )
}

fit_gaussian_model <- function(
    formula,
    data,
    area_name,
    metric_name,
    model_type
) {
  gaussian_fit <- fit_glmmTMB_safely(
    formula = formula,
    data = data,
    family = gaussian(link = "identity")
  )

  gaussian_check <- assess_model_fit(gaussian_fit$model)

  selected_model <- if (gaussian_check$valid) {
    gaussian_fit$model
  } else {
    NULL
  }

  model_error <- if (gaussian_check$valid) {
    NA_character_
  } else {
    combine_messages(
      gaussian_fit$error,
      if (!gaussian_check$pdHess) {
        "Gaussian Hessian was not positive definite"
      },
      if (!gaussian_check$finite_standard_errors) {
        "Gaussian standard errors were not finite"
      }
    )
  }

  diagnostics <- tibble(
    reporting_name = area_name,
    metric = metric_name,
    model_type = model_type,
    formula = formula_text(formula),
    n_observations = nrow(data),
    n_dates = n_distinct(data$start_date_fct),
    n_periods = n_distinct(data$Period),
    n_statuses = n_distinct(data$Status),
    n_sites = if ("uwa_site_code" %in% names(data)) {
      n_distinct(data$uwa_site_code, na.rm = TRUE)
    } else {
      NA_integer_
    },
    family_code = if (gaussian_check$valid) "gaussian" else NA_character_,
    selected_family = if (gaussian_check$valid) "Gaussian" else NA_character_,
    selection_reason = paste(
      "Gaussian was specified because Shannon diversity is a",
      "continuous response rather than count data"
    ),
    selected_model_valid = gaussian_check$valid,
    selected_pdHess = gaussian_check$pdHess,
    selected_finite_standard_errors =
      gaussian_check$finite_standard_errors,
    selected_AIC = gaussian_check$AIC,
    selected_logLik = gaussian_check$logLik,
    nb_valid = NA,
    nb_converged = NA,
    nb_pdHess = NA,
    nb_finite_standard_errors = NA,
    nb_theta = NA_real_,
    nb_at_poisson_boundary = NA,
    nb_AIC = NA_real_,
    nb_logLik = NA_real_,
    nb_warnings = NA_character_,
    nb_error = NA_character_,
    poisson_valid = NA,
    poisson_converged = NA,
    poisson_pdHess = NA,
    poisson_finite_standard_errors = NA,
    poisson_dispersion_ratio = NA_real_,
    poisson_dispersion_ok = NA,
    poisson_AIC = NA_real_,
    poisson_logLik = NA_real_,
    poisson_minus_nb_AIC = NA_real_,
    poisson_warnings = NA_character_,
    poisson_error = NA_character_,
    model_error = model_error
  )

  list(
    model = selected_model,
    family_code = if (gaussian_check$valid) "gaussian" else NA_character_,
    family_label = if (gaussian_check$valid) "Gaussian" else NA_character_,
    selection_reason = paste(
      "Gaussian was specified because Shannon diversity is a",
      "continuous response rather than count data"
    ),
    diagnostics = diagnostics,
    error = model_error
  )
}

fit_model_with_strategy <- function(
    formula,
    data,
    area_name,
    metric_name,
    model_type,
    family_strategy
) {
  if (family_strategy == "count_auto") {
    return(
      fit_count_model(
        formula = formula,
        data = data,
        area_name = area_name,
        metric_name = metric_name,
        model_type = model_type
      )
    )
  }

  if (family_strategy == "gaussian") {
    return(
      fit_gaussian_model(
        formula = formula,
        data = data,
        area_name = area_name,
        metric_name = metric_name,
        model_type = model_type
      )
    )
  }

  stop("Unknown family strategy: ", family_strategy)
}

# -----------------------------
# 3. Fit one region
# -----------------------------

fit_one_region <- function(
    df,
    response_col,
    metric_name,
    use_site = FALSE,
    family_strategy = "count_auto"
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
  # Period model
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

  message("Period model: ", formula_text(period_form))

  period_fit <- fit_model_with_strategy(
    formula = period_form,
    data = df,
    area_name = area_name,
    metric_name = metric_name,
    model_type = "Period",
    family_strategy = family_strategy
  )

  period_model <- period_fit$model
  period_error <- period_fit$error
  period_means <- tibble()
  period_status_means <- tibble()

  if (!is.null(period_model)) {
    period_emmeans <- tryCatch(
      {
        if (has_two_periods) {
          calculated_period_means <- emmeans(
            period_model,
            ~ Period,
            type = "response"
          ) %>%
            standardise_emmeans()
        } else {
          single_period <- as.character(unique(df$Period)[1])

          calculated_period_means <- emmeans(
            period_model,
            ~ 1,
            type = "response"
          ) %>%
            standardise_emmeans()

          calculated_period_means[["Period"]] <- single_period
        }

        if (has_two_periods && has_two_status) {
          calculated_period_status_means <- emmeans(
            period_model,
            ~ Period * Status,
            type = "response"
          ) %>%
            standardise_emmeans()
        } else if (has_two_periods) {
          single_status <- as.character(unique(df$Status)[1])

          calculated_period_status_means <- emmeans(
            period_model,
            ~ Period,
            type = "response"
          ) %>%
            standardise_emmeans()

          calculated_period_status_means[["Status"]] <- single_status
        } else if (has_two_status) {
          single_period <- as.character(unique(df$Period)[1])

          calculated_period_status_means <- emmeans(
            period_model,
            ~ Status,
            type = "response"
          ) %>%
            standardise_emmeans()

          calculated_period_status_means[["Period"]] <- single_period
        } else {
          single_period <- as.character(unique(df$Period)[1])
          single_status <- as.character(unique(df$Status)[1])

          calculated_period_status_means <- emmeans(
            period_model,
            ~ 1,
            type = "response"
          ) %>%
            standardise_emmeans()

          calculated_period_status_means[["Period"]] <- single_period
          calculated_period_status_means[["Status"]] <- single_status
        }

        list(
          period_means = calculated_period_means,
          period_status_means = calculated_period_status_means
        )
      },
      error = function(e) e
    )

    if (inherits(period_emmeans, "error")) {
      period_error <- combine_messages(
        period_error,
        paste0(
          "Period marginal means failed: ",
          conditionMessage(period_emmeans)
        )
      )
    } else {
      period_means <- period_emmeans$period_means %>%
        mutate(
          reporting_name = area_name,
          metric = metric_name,
          model_family = period_fit$family_label,
          family_code = period_fit$family_code,
          family_selection_reason = period_fit$selection_reason
        )

      period_status_means <- period_emmeans$period_status_means %>%
        mutate(
          reporting_name = area_name,
          metric = metric_name,
          model_family = period_fit$family_label,
          family_code = period_fit$family_code,
          family_selection_reason = period_fit$selection_reason
        )
    }
  }

  # ==========================================================
  # Temporal model
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

  temporal_fit <- NULL
  temporal_model <- NULL
  temporal_error <- NA_character_
  temporal_diagnostics <- tibble()
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

    message("Temporal model: ", formula_text(temporal_form))

    temporal_fit <- fit_model_with_strategy(
      formula = temporal_form,
      data = temporal_df,
      area_name = area_name,
      metric_name = metric_name,
      model_type = "Temporal",
      family_strategy = family_strategy
    )

    temporal_model <- temporal_fit$model
    temporal_error <- temporal_fit$error
    temporal_diagnostics <- temporal_fit$diagnostics

    if (!is.null(temporal_model)) {
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
              standardise_emmeans()
          } else {
            single_date <-
              as.character(unique(temporal_df$start_date_fct)[1])

            date_means <- emmeans(
              temporal_model,
              ~ 1,
              type = "response"
            ) %>%
              standardise_emmeans()

            date_means[["start_date_fct"]] <- single_date
          }

          date_means <- date_means %>%
            mutate(start_date_fct = as.character(start_date_fct)) %>%
            left_join(date_lookup, by = "start_date_fct") %>%
            left_join(period_lookup, by = "start_date_date")

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
              standardise_emmeans()
          } else if (temporal_has_two_dates) {
            date_status_means <- emmeans(
              temporal_model,
              ~ start_date_fct,
              type = "response"
            ) %>%
              standardise_emmeans()

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
              standardise_emmeans()

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
              standardise_emmeans()

            date_status_means[["start_date_fct"]] <- single_date
            date_status_means[["Status"]] <- single_status
          }

          date_status_means <- date_status_means %>%
            mutate(start_date_fct = as.character(start_date_fct)) %>%
            left_join(date_lookup, by = "start_date_fct") %>%
            left_join(period_lookup, by = "start_date_date")

          list(
            start_date_means = date_means,
            start_date_status_means = date_status_means
          )
        },
        error = function(e) e
      )

      if (inherits(temporal_emmeans, "error")) {
        temporal_error <- combine_messages(
          temporal_error,
          paste0(
            "Temporal marginal means failed: ",
            conditionMessage(temporal_emmeans)
          )
        )
      } else {
        start_date_means <- temporal_emmeans$start_date_means %>%
          mutate(
            reporting_name = area_name,
            metric = metric_name,
            model_family = temporal_fit$family_label,
            family_code = temporal_fit$family_code,
            family_selection_reason =
              temporal_fit$selection_reason
          )

        start_date_status_means <-
          temporal_emmeans$start_date_status_means %>%
          mutate(
            reporting_name = area_name,
            metric = metric_name,
            model_family = temporal_fit$family_label,
            family_code = temporal_fit$family_code,
            family_selection_reason =
              temporal_fit$selection_reason
          )
      }
    }
  }

  list(
    skipped = FALSE,
    period_model = period_model,
    temporal_model = temporal_model,
    period_family = period_fit$family_label,
    temporal_family = if (!is.null(temporal_fit)) {
      temporal_fit$family_label
    } else {
      NA_character_
    },
    period_diagnostics = period_fit$diagnostics,
    temporal_diagnostics = temporal_diagnostics,
    period_means = period_means,
    period_status_means = period_status_means,
    start_date_means = start_date_means,
    start_date_status_means = start_date_status_means,
    has_complete_date_status = temporal_has_complete_date_status,
    temporal_has_two_dates = temporal_has_two_dates,
    temporal_has_two_status = temporal_has_two_status,
    excluded_dates = excluded_dates,
    period_error = period_error,
    temporal_error = temporal_error
  )
}

# -----------------------------
# 4. Run across regions
# -----------------------------

run_metric_models <- function(
    df,
    response_col,
    metric_name,
    use_site = TRUE,
    family_strategy = "count_auto"
) {
  split_dat <- split(df, df$reporting_name)

  outputs <- map(
    split_dat,
    ~ tryCatch(
      fit_one_region(
        df = .x,
        response_col = response_col,
        metric_name = metric_name,
        use_site = use_site,
        family_strategy = family_strategy
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

    model_diagnostics = map_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped)
      ) {
        bind_rows(
          .x$period_diagnostics,
          .x$temporal_diagnostics
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

    period_errors = imap_dfr(outputs, ~ {
      if (
        is.list(.x) &&
          !inherits(.x, "error") &&
          !isTRUE(.x$skipped) &&
          length(.x$period_error) == 1 &&
          !is.na(.x$period_error)
      ) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = .x$period_error
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
# 5. Run all metrics
# -----------------------------

# Count responses use automatic nbinom2 -> Poisson selection.
abund_models <- run_metric_models(
  abund_dat,
  "total_abundance_sample",
  "Total abundance",
  use_site = TRUE,
  family_strategy = "count_auto"
)

rich_models <- run_metric_models(
  rich_dat,
  "n_species_sample",
  "Species richness",
  use_site = TRUE,
  family_strategy = "count_auto"
)

shark_models <- run_metric_models(
  shark_dat,
  "n_species_sample",
  "Shark and ray richness",
  use_site = TRUE,
  family_strategy = "count_auto"
)

reef_models <- run_metric_models(
  reef_dat,
  "n_species_sample",
  "Reef associated species richness",
  use_site = TRUE,
  family_strategy = "count_auto"
)

fish_200_models <- run_metric_models(
  fish_200_dat,
  "total_abundance_sample",
  "Abundance > 200 mm",
  use_site = TRUE,
  family_strategy = "count_auto"
)

# Shannon diversity is continuous rather than a count, so it is not
# appropriate to switch it between negative binomial and Poisson.
shannon_models <- run_metric_models(
  shannon_dat,
  "shannon",
  "Shannon diversity",
  use_site = TRUE,
  family_strategy = "gaussian"
)

# -----------------------------
# 6. Combine results
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

period_errors <- bind_rows(
  abund_models$period_errors,
  rich_models$period_errors,
  shark_models$period_errors,
  reef_models$period_errors,
  shannon_models$period_errors,
  fish_200_models$period_errors
)

temporal_errors <- bind_rows(
  abund_models$temporal_errors,
  rich_models$temporal_errors,
  shark_models$temporal_errors,
  reef_models$temporal_errors,
  shannon_models$temporal_errors,
  fish_200_models$temporal_errors
)

model_family_summary <- bind_rows(
  abund_models$model_diagnostics,
  rich_models$model_diagnostics,
  shark_models$model_diagnostics,
  reef_models$model_diagnostics,
  shannon_models$model_diagnostics,
  fish_200_models$model_diagnostics
) %>%
  arrange(metric, reporting_name, model_type)

model_family_counts <- model_family_summary %>%
  count(
    metric,
    model_type,
    family_code,
    selected_family,
    name = "n_models"
  ) %>%
  arrange(metric, model_type, family_code)

# Replace infinite confidence limits before saving or plotting.
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
# 7. Save model outputs
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
  model_family_summary,
  "model_results/model_family_summary.csv"
)

readr::write_excel_csv(
  model_family_counts,
  "model_results/model_family_counts.csv"
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
  period_errors,
  "model_results/period_errors.csv"
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
    model_families = model_family_summary,
    family_counts = model_family_counts,
    zero_summary = zero_summary,
    excluded_dates = excluded_dates_df,
    model_errors = model_errors,
    period_errors = period_errors,
    temporal_errors = temporal_errors
  ),
  "model_results/model_results.xlsx"
)

# -----------------------------
# 8. Plot helpers
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
# 9. Save four plot types
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
  file.path(plot_output_root, "period_results"),
  "period",
  "period means"
)

save_patchwork_plots(
  period_status_results,
  plot_period_status,
  file.path(plot_output_root, "period_status_results"),
  "period_status",
  "period means by status"
)

save_patchwork_plots(
  start_date_results,
  plot_start_date,
  file.path(plot_output_root, "start_date_results"),
  "start_date",
  "temporal results"
)

save_patchwork_plots(
  start_date_status_results,
  plot_start_date_status,
  file.path(plot_output_root, "start_date_status_results"),
  "start_date_status",
  "temporal results by status",
  width = 12
)

# -----------------------------
# 10. Optional checks
# -----------------------------

expected_locations <- sort(unique(abund_dat$reporting_name))
actual_locations <- sort(unique(start_date_results$reporting_name))
missing_temporal_locations <- setdiff(
  expected_locations,
  actual_locations
)

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
      n_non_missing_response = sum(
        !is.na(.data[[response_col]])
      ),
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
    model_family,
    family_code,
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
model_family_counts
