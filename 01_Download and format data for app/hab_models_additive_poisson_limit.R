# ============================================================
# START DATE MODELS + FOUR PLOTS PER LOCATION
# Count metrics: Poisson first, negative binomial if overdispersed
# Shannon diversity: Gaussian identity model
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
# Model-family and output settings
# -----------------------------

analysis_tag <- "20260730_additive_poisson_limit"
plot_output_root <- file.path("plots", analysis_tag)
model_output_root <- file.path("model_results", analysis_tag)

# A Poisson model is treated as overdispersed when its Pearson
# dispersion ratio is greater than this value. This is a pragmatic
# screening threshold rather than a universal statistical cut-off.
poisson_dispersion_threshold <- 1.5

# If an nbinom2 model has an extremely large theta, its extra-dispersion
# term is effectively zero and the model has approached its Poisson limit.
# When the Poisson model is otherwise valid, retain Poisson rather than
# rejecting both models because the unnecessary nbinom2 parameter failed.
nbinom2_poisson_limit_threshold <- 1e6

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

metric_family_assignments <- tribble(
  ~metric,                              ~metric_id,                       ~family_code, ~model_family,                         ~link,
  "Species richness",                  "richness",                      "auto_count", "Poisson; nbinom2 if overdispersed",  "log",
  "Shark and ray richness",            "sharks_rays",                   "auto_count", "Poisson; nbinom2 if overdispersed",  "log",
  "Reef associated species richness",  "reef_associated_richness",       "auto_count", "Poisson; nbinom2 if overdispersed",  "log",
  "Total abundance",                   "total_abundance",                "auto_count", "Poisson; nbinom2 if overdispersed",  "log",
  "Abundance > 200 mm",                "large_fish",                     "auto_count", "Poisson; nbinom2 if overdispersed",  "log",
  "Shannon diversity",                 "shannon_diversity",              "gaussian",   "gaussian (identity link)",            "identity"
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
# 2. Model-family and fitting helpers
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

get_family_details <- function(family_code) {

  if (identical(family_code, "auto_count")) {
    return(
      list(
        family = NULL,
        label = "Poisson; nbinom2 if overdispersed",
        link = "log",
        reason = paste(
          "Poisson was fitted first; nbinom2 was used when",
          "the Poisson model was overdispersed or invalid"
        )
      )
    )
  }

  if (identical(family_code, "poisson")) {
    return(
      list(
        family = poisson(link = "log"),
        label = "Poisson (log link)",
        link = "log",
        reason = "Poisson with a log link was fitted"
      )
    )
  }

  if (identical(family_code, "nbinom2")) {
    return(
      list(
        family = nbinom2(link = "log"),
        label = "Negative binomial nbinom2 (log link)",
        link = "log",
        reason = "Negative binomial nbinom2 was fitted"
      )
    )
  }

  if (identical(family_code, "gaussian")) {
    return(
      list(
        family = gaussian(link = "identity"),
        label = "gaussian (identity link)",
        link = "identity",
        reason = "gaussian identity model was specified for Shannon diversity"
      )
    )
  }

  stop(
    "Unknown family code: ", family_code,
    ". Use 'auto_count', 'poisson', 'nbinom2', or 'gaussian'."
  )
}

fit_glmmTMB_safely <- function(formula, data, family_object) {
  captured_warnings <- character()

  fit <- withCallingHandlers(
    tryCatch(
      glmmTMB(
        formula = formula,
        data = data,
        family = family_object
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
      all(is.na(pearson_residuals)) ||
      !is.finite(residual_df) ||
      residual_df <= 0
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

model_error_text <- function(fit_result, model_check, prefix) {
  combine_messages(
    if (!is.na(fit_result$error)) {
      paste0(prefix, " error: ", fit_result$error)
    },
    if (!model_check$converged) {
      paste0(prefix, " did not converge")
    },
    if (!model_check$pdHess) {
      paste0(prefix, " Hessian was not positive definite")
    },
    if (!model_check$finite_standard_errors) {
      paste0(prefix, " standard errors were not finite")
    },
    if (!is.finite(model_check$AIC)) {
      paste0(prefix, " AIC was not finite")
    }
  )
}

fit_selected_family_model <- function(
    formula,
    data,
    area_name,
    metric_name,
    model_type,
    family_code
) {

  requested_family <- get_family_details(family_code)

  # ----------------------------------------------------------
  # Continuous Shannon-diversity response: fixed Gaussian model
  # ----------------------------------------------------------
  if (identical(family_code, "gaussian")) {

    gaussian_fit <- fit_glmmTMB_safely(
      formula = formula,
      data = data,
      family_object = gaussian(link = "identity")
    )

    gaussian_check <- assess_model_fit(gaussian_fit$model)
    selected_model <- if (gaussian_check$valid) gaussian_fit$model else NULL

    model_error <- if (gaussian_check$valid) {
      NA_character_
    } else {
      model_error_text(gaussian_fit, gaussian_check, "gaussian model")
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
      requested_family_code = family_code,
      family_code = "gaussian",
      selected_family = "gaussian (identity link)",
      link = "identity",
      selection_reason = requested_family$reason,
      poisson_dispersion_ratio = NA_real_,
      dispersion_threshold = poisson_dispersion_threshold,
      poisson_overdispersed = NA,
      poisson_valid = NA,
      poisson_AIC = NA_real_,
      poisson_warnings = NA_character_,
      poisson_error = NA_character_,
      nbinom2_fitted = FALSE,
      nbinom2_valid = NA,
      nbinom2_AIC = NA_real_,
      nbinom2_theta = NA_real_,
      nbinom2_warnings = NA_character_,
      nbinom2_error = NA_character_,
      selected_model_valid = gaussian_check$valid,
      selected_converged = gaussian_check$converged,
      selected_pdHess = gaussian_check$pdHess,
      selected_finite_standard_errors = gaussian_check$finite_standard_errors,
      selected_AIC = gaussian_check$AIC,
      selected_logLik = gaussian_check$logLik,
      model_warnings = gaussian_fit$warnings,
      model_error = model_error
    )

    return(
      list(
        model = selected_model,
        candidate_model = gaussian_fit$model,
        valid = gaussian_check$valid,
        family_code = "gaussian",
        family_label = "gaussian (identity link)",
        family_link = "identity",
        selection_reason = requested_family$reason,
        diagnostics = diagnostics,
        error = model_error
      )
    )
  }

  # ----------------------------------------------------------
  # Count responses: fit Poisson first
  # ----------------------------------------------------------
  poisson_fit <- fit_glmmTMB_safely(
    formula = formula,
    data = data,
    family_object = poisson(link = "log")
  )

  poisson_check <- assess_model_fit(poisson_fit$model)
  poisson_dispersion <- pearson_dispersion_ratio(poisson_fit$model)

  poisson_overdispersed <- is.finite(poisson_dispersion) &&
    poisson_dispersion > poisson_dispersion_threshold

  # Fit nbinom2 if the Poisson model is overdispersed or invalid.
  fit_nbinom2 <- !poisson_check$valid || poisson_overdispersed

  nbinom2_fit <- list(
    model = NULL,
    error = NA_character_,
    warnings = NA_character_
  )
  nbinom2_check <- assess_model_fit(NULL)

  if (fit_nbinom2) {
    nbinom2_fit <- fit_glmmTMB_safely(
      formula = formula,
      data = data,
      family_object = nbinom2(link = "log")
    )
    nbinom2_check <- assess_model_fit(nbinom2_fit$model)
  }

  nbinom2_theta <- if (is.null(nbinom2_fit$model)) {
    NA_real_
  } else {
    tryCatch(as.numeric(sigma(nbinom2_fit$model)), error = function(e) NA_real_)
  }

  if (poisson_check$valid && !poisson_overdispersed) {
    selected_model <- poisson_fit$model
    selected_check <- poisson_check
    selected_family_code <- "poisson"
    selected_family_label <- "Poisson (log link)"
    selected_link <- "log"
    selection_reason <- paste0(
      "Poisson retained: Pearson dispersion ratio = ",
      round(poisson_dispersion, 3),
      ", not greater than the threshold of ",
      poisson_dispersion_threshold
    )
    model_error <- NA_character_

  } else if (nbinom2_check$valid) {
    selected_model <- nbinom2_fit$model
    selected_check <- nbinom2_check
    selected_family_code <- "nbinom2"
    selected_family_label <- "Negative binomial nbinom2 (log link)"
    selected_link <- "log"
    selection_reason <- if (poisson_overdispersed) {
      paste0(
        "nbinom2 selected: Poisson Pearson dispersion ratio = ",
        round(poisson_dispersion, 3),
        ", greater than the threshold of ",
        poisson_dispersion_threshold
      )
    } else {
      "nbinom2 selected because the Poisson model was invalid"
    }
    model_error <- NA_character_

  } else if (
    poisson_check$valid &&
      !nbinom2_check$valid &&
      is.finite(nbinom2_theta) &&
      nbinom2_theta > nbinom2_poisson_limit_threshold
  ) {
    # The negative-binomial dispersion parameter is extremely large,
    # indicating that nbinom2 has approached its Poisson limit. Keep
    # the valid Poisson model instead of rejecting both candidates.
    selected_model <- poisson_fit$model
    selected_check <- poisson_check
    selected_family_code <- "poisson"
    selected_family_label <- "Poisson (log link)"
    selected_link <- "log"

    selection_reason <- paste0(
      "Poisson retained because the nbinom2 dispersion parameter ",
      "was extremely large (theta = ",
      signif(nbinom2_theta, 4),
      "), exceeding the Poisson-limit threshold of ",
      format(nbinom2_poisson_limit_threshold, scientific = TRUE),
      ". The negative-binomial model had approached its Poisson limit."
    )

    model_error <- NA_character_

  } else {
    selected_model <- NULL
    selected_check <- if (fit_nbinom2) {
      nbinom2_check
    } else {
      poisson_check
    }

    selected_family_code <- if (fit_nbinom2) {
      "nbinom2"
    } else {
      "poisson"
    }

    selected_family_label <- if (fit_nbinom2) {
      "Negative binomial nbinom2 (log link)"
    } else {
      "Poisson (log link)"
    }

    selected_link <- "log"

    selection_reason <- if (poisson_overdispersed) {
      paste0(
        "Poisson was flagged as overdispersed (ratio = ",
        round(poisson_dispersion, 3),
        "), but the nbinom2 replacement was invalid and did not ",
        "clearly approach the Poisson limit"
      )
    } else {
      "Neither the Poisson model nor the nbinom2 replacement was valid"
    }

    model_error <- combine_messages(
      model_error_text(
        poisson_fit,
        poisson_check,
        "Poisson model"
      ),
      if (fit_nbinom2) {
        model_error_text(
          nbinom2_fit,
          nbinom2_check,
          "nbinom2 model"
        )
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
    requested_family_code = family_code,
    family_code = selected_family_code,
    selected_family = selected_family_label,
    link = selected_link,
    selection_reason = selection_reason,
    poisson_dispersion_ratio = poisson_dispersion,
    dispersion_threshold = poisson_dispersion_threshold,
    poisson_overdispersed = poisson_overdispersed,
    poisson_valid = poisson_check$valid,
    poisson_AIC = poisson_check$AIC,
    poisson_warnings = poisson_fit$warnings,
    poisson_error = poisson_fit$error,
    nbinom2_fitted = fit_nbinom2,
    nbinom2_valid = if (fit_nbinom2) nbinom2_check$valid else NA,
    nbinom2_AIC = if (fit_nbinom2) nbinom2_check$AIC else NA_real_,
    nbinom2_theta = nbinom2_theta,
    nbinom2_poisson_limit_threshold =
      nbinom2_poisson_limit_threshold,
    nbinom2_at_poisson_limit =
      is.finite(nbinom2_theta) &&
      nbinom2_theta > nbinom2_poisson_limit_threshold,
    nbinom2_warnings = nbinom2_fit$warnings,
    nbinom2_error = nbinom2_fit$error,
    selected_model_valid = !is.null(selected_model),
    selected_converged = selected_check$converged,
    selected_pdHess = selected_check$pdHess,
    selected_finite_standard_errors = selected_check$finite_standard_errors,
    selected_AIC = selected_check$AIC,
    selected_logLik = selected_check$logLik,
    model_warnings = combine_messages(
      poisson_fit$warnings,
      nbinom2_fit$warnings
    ),
    model_error = model_error
  )

  list(
    model = selected_model,
    candidate_model = if (!is.null(selected_model)) {
      selected_model
    } else if (!is.null(nbinom2_fit$model)) {
      nbinom2_fit$model
    } else {
      poisson_fit$model
    },
    valid = !is.null(selected_model),
    family_code = selected_family_code,
    family_label = selected_family_label,
    family_link = selected_link,
    selection_reason = selection_reason,
    diagnostics = diagnostics,
    error = model_error
  )
}

# -----------------------------
# 3. Fit one region
# -----------------------------

fit_one_region <- function(
    df,
    response_col,
    metric_name,
    use_site = FALSE,
    family_code
) {
  if (nrow(df) < 10) {
    stop("Not enough data")
  }
  
  requested_family <- get_family_details(family_code)
  
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
        metric = metric_name,
        family_code = family_code,
        model_family = requested_family$label,
        model_link = requested_family$link
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
  
  # period_fixed <- case_when(
  #   has_two_periods && has_two_status ~ "Period * Status",
  #   has_two_periods ~ "Period",
  #   has_two_status ~ "Status",
  #   TRUE ~ "1"
  # )
  
  # Check whether any observed Period × Status combination
  # contains only zero response values
  period_status_zero_check <- df %>%
    group_by(Period, Status) %>%
    summarise(
      n_samples = n(),
      n_positive = sum(
        .data[[response_col]] > 0,
        na.rm = TRUE
      ),
      all_zero = all(
        .data[[response_col]] == 0,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      Period,
      Status,
      fill = list(
        n_samples = 0L,
        n_positive = 0L,
        all_zero = NA
      )
    )
  
  # Count observed Period × Status cells that contain all zeros
  n_all_zero_period_status_cells <- period_status_zero_check %>%
    filter(
      n_samples > 0,
      all_zero %in% TRUE
    ) %>%
    nrow()
  
  # Check whether any Period × Status combination is absent entirely
  has_missing_period_status_cell <- any(
    period_status_zero_check$n_samples == 0
  )
  
  # Use the additive model when exactly one observed cell is all zero.
  # The additive model does not attempt to estimate the
  # Period × Status interaction.
  use_additive_period_model <-
    has_two_periods &&
    has_two_status &&
    n_all_zero_period_status_cells == 1
  
  period_fixed <- case_when(
    
    # Full interaction when the design is complete and no cell is all zero
    has_two_periods &&
      has_two_status &&
      !use_additive_period_model &&
      !has_missing_period_status_cell ~
      "Period * Status",
    
    # Additive model when one Period × Status cell contains all zeros
    has_two_periods &&
      has_two_status &&
      use_additive_period_model ~
      "Period + Status",
    
    # Also avoid the interaction if a combination is entirely absent
    has_two_periods &&
      has_two_status &&
      has_missing_period_status_cell ~
      "Period + Status",
    
    has_two_periods ~ "Period",
    has_two_status ~ "Status",
    TRUE ~ "1"
  )
  
  period_structure_reason <- case_when(
    
    use_additive_period_model ~
      paste0(
        "Additive Period + Status model used because ",
        n_all_zero_period_status_cells,
        " observed Period x Status cell contained all zeros"
      ),
    
    has_missing_period_status_cell ~
      paste(
        "Additive Period + Status model used because",
        "at least one Period x Status combination was absent"
      ),
    
    has_two_periods && has_two_status ~
      "Full Period x Status interaction used",
    
    has_two_periods ~
      "Period-only fixed effect used",
    
    has_two_status ~
      "Status-only fixed effect used",
    
    TRUE ~
      "Intercept-only fixed effect used"
  )
  
  message("Period fixed effects: ", period_fixed)
  message("Period structure reason: ", period_structure_reason)
  message(
    "All-zero Period x Status cells: ",
    n_all_zero_period_status_cells
  )
  
  print(period_status_zero_check)
  
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
  
  period_fit <- fit_selected_family_model(
    formula = period_form,
    data = df,
    area_name = area_name,
    metric_name = metric_name,
    model_type = "Period",
    family_code = family_code
  )
  
  period_fit$diagnostics <- period_fit$diagnostics %>%
    mutate(
      fixed_effect_structure = period_fixed,
      structure_reason = period_structure_reason,
      n_all_zero_period_status_cells =
        n_all_zero_period_status_cells,
      missing_period_status_cell =
        has_missing_period_status_cell
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
          model_link = period_fit$family_link,
          family_selection_reason = period_fit$selection_reason
        )
      
      period_status_means <- period_emmeans$period_status_means %>%
        mutate(
          reporting_name = area_name,
          metric = metric_name,
          model_family = period_fit$family_label,
          family_code = period_fit$family_code,
          model_link = period_fit$family_link,
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
  
  # Exclude complete dates with 90% OR MORE zeros.
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
      family_code = family_code,
      model_family = requested_family$label,
      model_link = requested_family$link,
      exclusion_reason = "Not modelled\n(>=90% zeros)"
    )
  
  # Retain dates with less than 90% zeros.
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
  temporal_date_status_check <- tibble()
  n_all_zero_date_status_cells <- 0L
  temporal_has_all_zero_date_status_cell <- FALSE
  use_additive_temporal_model <- FALSE
  temporal_fixed <- NA_character_
  temporal_structure_reason <- NA_character_
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
    
    # Check whether every retained date contains both statuses, and
    # whether any observed Date x Status cell contains only zeros.
    if (temporal_has_two_dates && temporal_has_two_status) {
      temporal_date_status_check <- temporal_df %>%
        group_by(start_date_fct, Status) %>%
        summarise(
          n_samples = n(),
          n_positive = sum(
            .data[[response_col]] > 0,
            na.rm = TRUE
          ),
          percent_zero = mean(
            .data[[response_col]] == 0,
            na.rm = TRUE
          ) * 100,
          all_zero = all(
            .data[[response_col]] == 0,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        tidyr::complete(
          start_date_fct,
          Status,
          fill = list(
            n_samples = 0L,
            n_positive = 0L,
            percent_zero = NA_real_,
            all_zero = NA
          )
        )
      
      temporal_has_complete_date_status <-
        all(temporal_date_status_check$n_samples > 0)
      
      n_all_zero_date_status_cells <- temporal_date_status_check %>%
        filter(
          n_samples > 0,
          all_zero %in% TRUE
        ) %>%
        nrow()
      
      temporal_has_all_zero_date_status_cell <-
        n_all_zero_date_status_cells > 0
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
    
    # Use an additive Date + Status model when all Date x Status
    # combinations are represented but at least one observed cell is all zero.
    # This avoids estimating a separate interaction coefficient for a cell
    # whose fitted mean is being driven towards zero.
    use_additive_temporal_model <-
      temporal_has_two_dates &&
      temporal_has_two_status &&
      temporal_has_complete_date_status &&
      temporal_has_all_zero_date_status_cell
    
    temporal_fixed <- case_when(
      temporal_has_two_dates &&
        temporal_has_two_status &&
        temporal_has_complete_date_status &&
        !temporal_has_all_zero_date_status_cell ~
        "start_date_fct * Status",
      
      use_additive_temporal_model ~
        "start_date_fct + Status",
      
      # If one or more statuses are absent from a date, do not estimate
      # a date-by-status interaction or an overall status effect.
      temporal_has_two_dates ~
        "start_date_fct",
      
      temporal_has_two_status ~
        "Status",
      
      TRUE ~
        "1"
    )
    
    temporal_structure_reason <- case_when(
      use_additive_temporal_model ~
        paste0(
          "Additive date + Status model used because ",
          n_all_zero_date_status_cells,
          " observed date x Status cell(s) contained all zeros"
        ),
      
      temporal_has_two_dates &&
        temporal_has_two_status &&
        temporal_has_complete_date_status ~
        "Full date x Status interaction used",
      
      temporal_has_two_dates &&
        temporal_has_two_status &&
        !temporal_has_complete_date_status ~
        paste(
          "Date-only model used because Status was not",
          "represented at every retained sampling date"
        ),
      
      temporal_has_two_dates ~
        "Date-only fixed effect used",
      
      temporal_has_two_status ~
        "Status-only fixed effect used",
      
      TRUE ~
        "Intercept-only fixed effect used"
    )
    
    message("Temporal fixed effects: ", temporal_fixed)
    message("Temporal structure reason: ", temporal_structure_reason)
    message(
      "All-zero date x Status cells: ",
      n_all_zero_date_status_cells
    )
    
    if (nrow(temporal_date_status_check) > 0) {
      print(temporal_date_status_check)
    }
    
    temporal_form <- as.formula(
      paste0(
        response_col,
        " ~ ",
        temporal_fixed,
        temporal_site_re
      )
    )
    
    message("Temporal model: ", formula_text(temporal_form))
    
    temporal_fit <- fit_selected_family_model(
      formula = temporal_form,
      data = temporal_df,
      area_name = area_name,
      metric_name = metric_name,
      model_type = "Temporal",
      family_code = family_code
    )
    
    # Record why the interaction, additive model, or date-only model was used.
    temporal_fit$diagnostics <- temporal_fit$diagnostics %>%
      mutate(
        fixed_effect_structure = temporal_fixed,
        structure_reason = temporal_structure_reason,
        n_all_zero_date_status_cells =
          n_all_zero_date_status_cells,
        complete_date_status_design =
          temporal_has_complete_date_status
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
          
          # Predicted Date x Status combinations can still be calculated from
          # an additive model. In that case, the status difference is assumed
          # to be the same at every date because no interaction was fitted.
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
            model_link = temporal_fit$family_link,
            family_selection_reason =
              temporal_fit$selection_reason,
            fixed_effect_structure = temporal_fixed,
            structure_reason = temporal_structure_reason,
            n_all_zero_date_status_cells =
              n_all_zero_date_status_cells
          )
        
        start_date_status_means <-
          temporal_emmeans$start_date_status_means %>%
          mutate(
            reporting_name = area_name,
            metric = metric_name,
            model_family = temporal_fit$family_label,
            family_code = temporal_fit$family_code,
            model_link = temporal_fit$family_link,
            family_selection_reason =
              temporal_fit$selection_reason,
            fixed_effect_structure = temporal_fixed,
            structure_reason = temporal_structure_reason,
            n_all_zero_date_status_cells =
              n_all_zero_date_status_cells
          )
      }
    }
  }
  
  list(
    skipped = FALSE,
    period_model = period_model,
    temporal_model = temporal_model,
    period_candidate_model = period_fit$candidate_model,
    temporal_candidate_model = if (!is.null(temporal_fit)) {
      temporal_fit$candidate_model
    } else {
      NULL
    },
    period_family = period_fit$family_label,
    temporal_family = if (!is.null(temporal_fit)) {
      temporal_fit$family_label
    } else {
      NA_character_
    },
    period_family_link = period_fit$family_link,
    temporal_family_link = if (!is.null(temporal_fit)) {
      temporal_fit$family_link
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
    family_code
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
        family_code = family_code
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
          family_code = .x$family_code,
          model_family = .x$model_family,
          model_link = .x$model_link,
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
          family_code = family_code,
          model_family = get_family_details(family_code)$label,
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
          family_code = family_code,
          model_family = get_family_details(family_code)$label,
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
          family_code = family_code,
          model_family = get_family_details(family_code)$label,
          error = .x$temporal_error
        )
      }
    })
  )
}


# -----------------------------
# 5. Run all metrics
# -----------------------------

# Count responses: Poisson first; nbinom2 if the fitted Poisson
# model is overdispersed or invalid.
rich_models <- run_metric_models(
  rich_dat,
  "n_species_sample",
  "Species richness",
  use_site = TRUE,
  family_code = "auto_count"
)

shark_models <- run_metric_models(
  shark_dat,
  "n_species_sample",
  "Shark and ray richness",
  use_site = TRUE,
  family_code = "auto_count"
)

reef_models <- run_metric_models(
  reef_dat,
  "n_species_sample",
  "Reef associated species richness",
  use_site = TRUE,
  family_code = "auto_count"
)

abund_models <- run_metric_models(
  abund_dat,
  "total_abundance_sample",
  "Total abundance",
  use_site = TRUE,
  family_code = "auto_count"
)

fish_200_models <- run_metric_models(
  fish_200_dat,
  "total_abundance_sample",
  "Abundance > 200 mm",
  use_site = TRUE,
  family_code = "auto_count"
)

# Shannon diversity is continuous rather than a count.
shannon_models <- run_metric_models(
  shannon_dat,
  "shannon",
  "Shannon diversity",
  use_site = TRUE,
  family_code = "gaussian"
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
    selected_model_valid,
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
  model_output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_excel_csv(
  period_results,
  file.path(model_output_root, "period_results.csv")
)

readr::write_excel_csv(
  period_status_results,
  file.path(model_output_root, "period_status_results.csv")
)

readr::write_excel_csv(
  start_date_results,
  file.path(model_output_root, "start_date_results.csv")
)

readr::write_excel_csv(
  start_date_status_results,
  file.path(model_output_root, "start_date_status_results.csv")
)

readr::write_excel_csv(
  model_family_summary,
  file.path(model_output_root, "model_family_summary.csv")
)

readr::write_excel_csv(
  model_family_counts,
  file.path(model_output_root, "model_family_counts.csv")
)

readr::write_excel_csv(
  metric_family_assignments,
  file.path(model_output_root, "metric_family_assignments.csv")
)

readr::write_excel_csv(
  zero_summary,
  file.path(model_output_root, "zero_summary.csv")
)

readr::write_excel_csv(
  excluded_dates_df,
  file.path(model_output_root, "excluded_dates.csv")
)

readr::write_excel_csv(
  model_errors,
  file.path(model_output_root, "model_errors.csv")
)

readr::write_excel_csv(
  period_errors,
  file.path(model_output_root, "period_errors.csv")
)

readr::write_excel_csv(
  temporal_errors,
  file.path(model_output_root, "temporal_errors.csv")
)

writexl::write_xlsx(
  list(
    period_results = period_results,
    period_status_results = period_status_results,
    start_date_results = start_date_results,
    start_date_status_results = start_date_status_results,
    model_families = model_family_summary,
    family_counts = model_family_counts,
    family_assignments = metric_family_assignments,
    zero_summary = zero_summary,
    excluded_dates = excluded_dates_df,
    model_errors = model_errors,
    period_errors = period_errors,
    temporal_errors = temporal_errors
  ),
  file.path(model_output_root, "model_results.xlsx")
)

# -----------------------------
# 8. Plot helpers
# -----------------------------

# blank_panel <- function(panel_letter, label = "More than 90% zeros") {
#   ggplot() +
#     annotate(
#       "text",
#       x = 0,
#       y = 0,
#       label = label,
#       size = 6,
#       fontface = "italic"
#     ) +
#     xlim(-1, 1) +
#     ylim(-1, 1) +
#     theme_void() +
#     labs(tag = panel_letter) +
#     theme(plot.tag = element_text(size = 18))
# }

# -----------------------------
# Blank-panel helpers
# -----------------------------

blank_panel <- function(
    panel_letter,
    label = "No length data available"
) {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = label,
      size = 5,
      fontface = "italic",
      lineheight = 1.1
    ) +
    xlim(-1, 1) +
    ylim(-1, 1) +
    theme_void() +
    labs(tag = panel_letter) +
    theme(
      plot.tag = element_text(size = 18)
    )
}


metric_name_from_id <- function(metric_id) {
  
  metric_position <- match(
    metric_id,
    unname(metric_lookup)
  )
  
  if (is.na(metric_position)) {
    return(NA_character_)
  }
  
  names(metric_lookup)[metric_position]
}


get_model_error <- function(
    error_table,
    region,
    metric_name
) {
  
  if (is.null(error_table) || nrow(error_table) == 0) {
    return(NA_character_)
  }
  
  matching_error <- error_table %>%
    filter(
      reporting_name == region,
      metric == metric_name
    )
  
  if (nrow(matching_error) == 0) {
    return(NA_character_)
  }
  
  matching_error$error[[1]]
}


get_blank_panel_label <- function(
    region,
    metric_id,
    model_type = c("Period", "Temporal")
) {
  
  model_type <- match.arg(model_type)
  
  metric_name <- metric_name_from_id(metric_id)
  
  # First check whether the complete regional metric really
  # was skipped because more than 90% of observations were zero.
  zero_record <- zero_summary %>%
    filter(
      reporting_name == region,
      metric == metric_name
    )
  
  if (nrow(zero_record) > 0) {
    
    return(
      paste0(
        "Not modelled\n(",
        round(zero_record$percent_zero[[1]], 1),
        "% zeros)"
      )
    )
  }
  
  # Errors affecting the complete region, such as insufficient data.
  general_error <- get_model_error(
    error_table = model_errors,
    region = region,
    metric_name = metric_name
  )
  
  # Errors specific to the period or temporal model.
  specific_error <- if (model_type == "Period") {
    
    get_model_error(
      error_table = period_errors,
      region = region,
      metric_name = metric_name
    )
    
  } else {
    
    get_model_error(
      error_table = temporal_errors,
      region = region,
      metric_name = metric_name
    )
  }
  
  error_text <- dplyr::coalesce(
    specific_error,
    general_error
  )
  
  if (is.na(error_text)) {
    return("No length data available")
  }
  
  if (
    str_detect(
      error_text,
      regex("Not enough data", ignore_case = TRUE)
    )
  ) {
    return("Not modelled\n(fewer than 10 observations)")
  }
  
  if (
    str_detect(
      error_text,
      regex(
        "No observations remained",
        ignore_case = TRUE
      )
    )
  ) {
    return(
      paste0(
        "No dates modelled\n",
        "(all dates had >=90% zeros)"
      )
    )
  }
  
  if (
    str_detect(
      error_text,
      regex("Hessian", ignore_case = TRUE)
    )
  ) {
    return("Model failed\n(Hessian problem)")
  }
  
  if (
    str_detect(
      error_text,
      regex(
        "standard errors were not finite",
        ignore_case = TRUE
      )
    )
  ) {
    return(
      paste0(
        "Model failed\n",
        "(uncertainty could not be estimated)"
      )
    )
  }
  
  if (
    str_detect(
      error_text,
      regex("did not converge", ignore_case = TRUE)
    )
  ) {
    return("Model did not converge")
  }
  
  if (
    str_detect(
      error_text,
      regex("marginal means failed", ignore_case = TRUE)
    )
  ) {
    return(
      paste0(
        "Model fitted, but\n",
        "marginal means failed"
      )
    )
  }
  
  "Model result unavailable\n(see model error tables)"
}

add_plot_confidence_limits <- function(df) {
  df %>%
    mutate(
      plot_LCL = case_when(
        metric_id == "shannon_diversity" ~ pmax(asymp.LCL, 0),
        TRUE ~ asymp.LCL
      ),
      lower_limit_truncated = (
        metric_id == "shannon_diversity" &
          !is.na(asymp.LCL) &
          asymp.LCL < 0
      )
    )
}

# plot_period <- function(df, metric_id, panel_letter) {
#   metric_df <- df %>%
#     filter(.data$metric_id == !!metric_id)
#   
#   # if (nrow(metric_df) == 0) {
#   #   return(blank_panel(panel_letter))
#   # }
#   
#   if (nrow(metric_df) == 0) {
#     
#     region <- unique(df$reporting_name)[1]
#     
#     return(
#       blank_panel(
#         panel_letter = panel_letter,
#         label = get_blank_panel_label(
#           region = region,
#           metric_id = metric_id,
#           model_type = "Period"
#         )
#       )
#     )
#   }
#   
#   ggplot(metric_df, aes(x = Period, y = response, fill = Period)) +
#     geom_col(width = 0.6, colour = "black", alpha = 0.85) +
#     geom_errorbar(
#       aes(ymin = asymp.LCL, ymax = asymp.UCL),
#       width = 0.2,
#       linewidth = 0.6
#     ) +
#     scale_fill_manual(values = metric_period_cols, drop = FALSE) +
#     labs(
#       x = NULL,
#       y = metric_y_lab[[metric_id]],
#       tag = panel_letter,
#       fill = NULL
#     ) +
#     theme_minimal(base_size = 16) +
#     plot_theme +
#     theme(legend.position = "none")
# }

plot_period <- function(df, metric_id, panel_letter) {
  
  metric_df <- df %>%
    filter(.data$metric_id == !!metric_id) %>%
    add_plot_confidence_limits()
  
  # Existing blank-panel code here
  
  ggplot(metric_df, aes(x = Period, y = response, fill = Period)) +
    geom_col(
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = plot_LCL,
        ymax = asymp.UCL
      ),
      width = 0.2,
      linewidth = 0.6
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
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
    filter(.data$metric_id == !!metric_id) %>%
    add_plot_confidence_limits()

  if (nrow(metric_df) == 0) {
    region <- unique(df$reporting_name)[1]
    return(
      blank_panel(
        panel_letter = panel_letter,
        label = get_blank_panel_label(
          region = region,
          metric_id = metric_id,
          model_type = "Period"
        )
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
      aes(ymin = plot_LCL, ymax = asymp.UCL),
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
  
  # if (nrow(metric_df) == 0) {
  #   return(blank_panel(panel_letter))
  # }
  
  if (nrow(metric_df) == 0) {
    
    region <- unique(df$reporting_name)[1]
    
    return(
      blank_panel(
        panel_letter = panel_letter,
        label = get_blank_panel_label(
          region = region,
          metric_id = metric_id,
          model_type = "Temporal"
        )
      )
    )
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
    region <- unique(df$reporting_name)[1]
    return(
      blank_panel(
        panel_letter = panel_letter,
        label = get_blank_panel_label(
          region = region,
          metric_id = metric_id,
          model_type = "Temporal"
        )
      )
    )
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

# save_patchwork_plots(
#   start_date_status_results,
#   plot_start_date_status,
#   file.path(plot_output_root, "start_date_status_results"),
#   "start_date_status",
#   "temporal results by status",
#   width = 12
# )

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
    model_link,
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

readr::write_excel_csv(
  uncertainty_issues,
  file.path(model_output_root, "uncertainty_issues.csv")
)

uncertainty_issues
model_family_counts




model <- shannon_models$outputs[["Offshore Ardrossan - Offshore Ardrossan Sanctuary Zone"]]$period_model

par(mfrow = c(1, 2))

plot(
  fitted(model),
  residuals(model, type = "pearson"),
  xlab = "Fitted values",
  ylab = "Pearson residuals"
)
abline(h = 0, lty = 2)

qqnorm(residuals(model, type = "pearson"))
qqline(residuals(model, type = "pearson"))

library(DHARMa)

sim_res <- simulateResiduals(
  fittedModel = model,
  n = 1000
)

par(mfrow = c(2, 2))
plot(sim_res)
testUniformity(sim_res)
testDispersion(sim_res)
testOutliers(sim_res)

shannon_dat %>%
  summarise(
    n = n(),
    n_zero = sum(shannon == 0, na.rm = TRUE),
    percent_zero = mean(shannon == 0, na.rm = TRUE) * 100,
    minimum = min(shannon, na.rm = TRUE)
  )
