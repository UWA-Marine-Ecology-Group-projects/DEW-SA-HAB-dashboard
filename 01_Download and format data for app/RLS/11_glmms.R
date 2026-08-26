#################################################################
# UVC / RLS GLMM analysis for South Australian monitoring
#
# Analysis unit:
#   - one row = one transect-level metric value
#   - the two adjacent blocks have already been combined upstream
#
# Outputs required:
#   1. Period predicted means: Pre-bloom vs Bloom
#   2. Period x Status predicted means: Fished vs No-take within Period
#   3. Temporal predicted means: one estimate for every OBSERVED
#      sampling_event_start_date; dates are treated categorically
#
# Spatial outputs:
#   - Location: GLMMs are fitted separately to each location
#   - Region: equal-weight averages of the location-level predictions
#             are calculated. This is deliberate so a location with
#             more sites/transects does not dominate its region.
#
# Distribution choices:
#   - Species richness, abundance and B20: Tweedie with log link
#     because the transect-level values are non-negative, may contain
#     zeros, and can be non-integer after adjacent blocks are averaged.
#   - Shannon diversity: Gaussian with identity link. Check residuals.
#
# Important design decisions:
#   - Every transect contributes equally to its location-level GLMM.
#   - Site is a random intercept because sites are revisited through time.
#   - Sampling-event start date is a random intercept in Period models
#     when there is enough replication, to account for shared conditions
#     among transects sampled during the same date/event.
#   - Transect is NOT a random effect because transect IDs are unique and
#     each transect contributes one response value per metric.
#   - Temporal models use sampling date as a FIXED categorical effect and
#     therefore only estimate dates that were actually sampled.
#   - Temporal models are overall only: no Date x Status interaction.
#   - Period x Status predictions are still returned when a status has
#     <2 sites, but these rows are explicitly flagged as low replication.
#################################################################

# ============================================================
# 0. Packages
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(ggplot2)
library(glmmTMB)
library(emmeans)
library(writexl)

# Reproducibility for any simulation-based diagnostics run later.
set.seed(123)


# ============================================================
# 1. User settings
# ============================================================

metric_input_dir <- "data/rls_metrics_for_modelling"

analysis_tag <- "uvc_glmm_tweedie_transect_level"
model_output_root <- file.path("model_results", analysis_tag)
plot_output_root <- file.path("plots", analysis_tag)

# Easy plotting switch requested for M2 B20.
# The M2 B20 model is still fitted and saved when this is FALSE;
# it is simply excluded from plot generation.
plot_m2_b20 <- TRUE

# Status predictions are still calculated below this threshold,
# but are flagged so they are not mistaken for strongly replicated inference.
minimum_sites_per_status <- 2L

# Confidence level for model predictions.
confidence_level <- 0.95

period_levels <- c("Pre-bloom", "Bloom")
status_levels <- c("Fished", "No-take")

# Keep colours consistent with the RLS figures already being produced.
period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

status_cols <- c(
  "Fished" = "#D98C3F",
  "No-take" = "#4FA08F"
)

# Metrics to retain from the full abundance file.
abundance_metrics_to_model <- c(
  "M1 fish total abundance",
  "M2 fish total abundance",
  "M2 invertebrate total abundance",
  "M2 invertebrate Echinodermata abundance",
  "M2 invertebrate Arthropoda abundance",
  "M2 invertebrate Mollusca abundance"
)

# Plot order only. This does not change what is modelled.
plot_metric_order <- c(
  "M1 fish species richness",
  "M2 fish species richness",
  "M2 invertebrate species richness",
  "M1 fish Shannon diversity",
  "M2 fish Shannon diversity",
  "M2 invertebrate Shannon diversity",
  "M1 fish B20 biomass",
  "M2 fish B20 biomass",
  "M1 fish total abundance",
  "M2 fish total abundance",
  "M2 invertebrate total abundance",
  "M2 invertebrate Echinodermata abundance",
  "M2 invertebrate Arthropoda abundance",
  "M2 invertebrate Mollusca abundance"
)

if (!plot_m2_b20) {
  plot_metric_order <- setdiff(
    plot_metric_order,
    "M2 fish B20 biomass"
  )
}


dir.create(model_output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_output_root, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 2. Helper functions for reading and standardising data
# ============================================================

# Standardise management-status spelling before modelling.
normalise_status <- function(x) {
  x_original <- as.character(x)
  x_clean <- stringr::str_to_lower(stringr::str_trim(x_original))
  
  dplyr::case_when(
    x_clean %in% c("no-take", "no take", "no_take", "notake") ~ "No-take",
    x_clean %in% c("fished", "fishing") ~ "Fished",
    TRUE ~ x_original
  )
}


# Some joins in the metric-building scripts can leave status.x/status.y.
# Resolve those automatically so the model script accepts either form.
resolve_status_column <- function(data, source_name) {
  
  if ("status" %in% names(data)) {
    return(data)
  }
  
  status_candidates <- intersect(
    c("status.x", "status.y"),
    names(data)
  )
  
  if (length(status_candidates) == 0) {
    stop(
      source_name,
      " does not contain status, status.x or status.y."
    )
  }
  
  if (length(status_candidates) == 1) {
    data$status <- data[[status_candidates[[1]]]]
    return(data)
  }
  
  data$status <- dplyr::coalesce(
    as.character(data[[status_candidates[[1]]]]),
    as.character(data[[status_candidates[[2]]]])
  )
  
  data
}


# Read one saved transect-level metric table and convert it to a common
# structure so every metric can be processed by the same model functions.
load_metric_table <- function(
    path,
    response_col,
    family_code,
    keep_metrics = NULL) {
  
  if (!file.exists(path)) {
    stop(
      "Missing input file: ", path,
      "\nRun the metric-creation script first."
    )
  }
  
  data <- readr::read_rds(path) %>%
    resolve_status_column(source_name = path)
  
  required_columns <- c(
    "transect",
    "site_code",
    "metric",
    "location",
    "region",
    "status",
    "period",
    "sampling_event_start_date",
    response_col
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in ", path, ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (!is.null(keep_metrics)) {
    data <- data %>%
      filter(as.character(metric) %in% keep_metrics)
  }
  
  data %>%
    transmute(
      transect = as.character(transect),
      site_code = as.character(site_code),
      site_name = if ("site_name" %in% names(data)) {
        as.character(site_name)
      } else {
        as.character(site_code)
      },
      location = as.character(location),
      region = as.character(region),
      status = normalise_status(status),
      Period = factor(as.character(period), levels = period_levels),
      sampling_event_start_date = as.Date(sampling_event_start_date),
      metric = as.character(metric),
      response = as.numeric(.data[[response_col]]),
      family_code = family_code,
      source_file = path
    ) %>%
    filter(
      !is.na(response),
      !is.na(location), location != "",
      !is.na(region), region != "",
      !is.na(site_code), site_code != "",
      !is.na(transect), transect != "",
      !is.na(Period),
      !is.na(status), status != "",
      !is.na(sampling_event_start_date)
    )
}


# ============================================================
# 3. Read the transect-level metric files
# ============================================================

# These file names and response columns match the metric-building scripts.
richness_dat <- load_metric_table(
  path = file.path(metric_input_dir, "species_richness.rds"),
  response_col = "species_richness",
  family_code = "tweedie"
)

shannon_dat <- load_metric_table(
  path = file.path(metric_input_dir, "shannon_diversity.rds"),
  response_col = "shannon",
  family_code = "gaussian"
)

b20_dat <- load_metric_table(
  path = file.path(metric_input_dir, "b20.rds"),
  response_col = "b20_kg",
  family_code = "tweedie"
)

abundance_dat <- load_metric_table(
  path = file.path(metric_input_dir, "abundance.rds"),
  response_col = "abundance",
  family_code = "tweedie",
  keep_metrics = abundance_metrics_to_model
)

all_dat <- bind_rows(
  richness_dat,
  shannon_dat,
  b20_dat,
  abundance_dat
) %>%
  mutate(
    metric = factor(
      metric,
      levels = unique(c(plot_metric_order, as.character(metric)))
    ),
    status = factor(status, levels = status_levels),
    Period = factor(Period, levels = period_levels)
  )


# ============================================================
# 4. Data checks before fitting models
# ============================================================

# 4a. Responses must be non-negative for all selected metrics.
negative_response_check <- all_dat %>%
  filter(response < 0)

if (nrow(negative_response_check) > 0) {
  stop(
    "Negative responses were found. Inspect negative_response_check before modelling."
  )
}


# 4b. Each transect should contribute one value per metric.
#     The same transect can legitimately appear once in several different metrics.
duplicate_metric_transects <- all_dat %>%
  count(metric, transect, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(duplicate_metric_transects) > 0) {
  stop(
    "Some metric x transect combinations occur more than once. ",
    "Inspect duplicate_metric_transects before modelling."
  )
}


# 4c. Status is expected to be fixed for a site through time.
site_status_check <- all_dat %>%
  distinct(site_code, status) %>%
  count(site_code, name = "n_statuses") %>%
  filter(n_statuses > 1)

if (nrow(site_status_check) > 0) {
  stop(
    "At least one site has more than one management status. ",
    "Inspect site_status_check before modelling."
  )
}


# 4d. Each location should map to one region.
location_region_check <- all_dat %>%
  distinct(location, region) %>%
  count(location, name = "n_regions") %>%
  filter(n_regions > 1)

if (nrow(location_region_check) > 0) {
  stop(
    "At least one location maps to more than one region. ",
    "Inspect location_region_check before modelling."
  )
}


# 4e. Confirm the target M2 invertebrate phylum metrics were found.
missing_abundance_metrics <- setdiff(
  abundance_metrics_to_model,
  unique(as.character(abundance_dat$metric))
)

if (length(missing_abundance_metrics) > 0) {
  warning(
    "The following requested abundance metrics were not found and will not be modelled: ",
    paste(missing_abundance_metrics, collapse = ", ")
  )
}


# A compact data-availability table is useful when interpreting model failures.
data_availability <- all_dat %>%
  group_by(metric, location, region, Period, status) %>%
  summarise(
    n_transects = n(),
    n_sites = n_distinct(site_code),
    n_dates = n_distinct(sampling_event_start_date),
    n_positive = sum(response > 0, na.rm = TRUE),
    prop_zero = mean(response == 0, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# 5. Model-family and model-fitting helpers
# ============================================================

get_family_object <- function(family_code) {
  
  if (family_code == "tweedie") {
    return(glmmTMB::tweedie(link = "log"))
  }
  
  if (family_code == "gaussian") {
    return(gaussian(link = "identity"))
  }
  
  stop("Unknown family_code: ", family_code)
}


get_family_label <- function(family_code) {
  dplyr::case_when(
    family_code == "tweedie" ~ "Tweedie (log link)",
    family_code == "gaussian" ~ "Gaussian (identity link)",
    TRUE ~ family_code
  )
}


combine_messages <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0) {
    NA_character_
  } else {
    paste(unique(x), collapse = " | ")
  }
}


formula_text <- function(x) {
  paste(deparse(x), collapse = "")
}


fit_glmmTMB_safely <- function(formula, data, family_object) {
  
  captured_warnings <- character()
  
  fit <- withCallingHandlers(
    tryCatch(
      glmmTMB::glmmTMB(
        formula = formula,
        data = data,
        family = family_object
      ),
      error = function(e) e
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (inherits(fit, "error")) {
    return(list(
      model = NULL,
      error = conditionMessage(fit),
      warnings = combine_messages(captured_warnings)
    ))
  }
  
  list(
    model = fit,
    error = NA_character_,
    warnings = combine_messages(captured_warnings)
  )
}


assess_model_fit <- function(model) {
  
  if (is.null(model)) {
    return(list(
      converged = FALSE,
      pdHess = FALSE,
      finite_standard_errors = FALSE,
      valid = FALSE,
      AIC = NA_real_,
      logLik = NA_real_
    ))
  }
  
  fixed_se <- tryCatch(
    sqrt(diag(vcov(model)$cond)),
    error = function(e) NA_real_
  )
  
  converged <- isTRUE(model$fit$convergence == 0)
  pd_hess <- isTRUE(model$sdr$pdHess)
  finite_se <- length(fixed_se) > 0 && all(is.finite(fixed_se))
  
  model_aic <- tryCatch(as.numeric(AIC(model)), error = function(e) NA_real_)
  model_loglik <- tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  
  list(
    converged = converged,
    pdHess = pd_hess,
    finite_standard_errors = finite_se,
    valid = converged && pd_hess && finite_se && is.finite(model_aic),
    AIC = model_aic,
    logLik = model_loglik
  )
}


# Standardise the column names returned by emmeans/regrid/contrast.
standardise_emmeans_summary <- function(x) {
  
  out <- x %>%
    as.data.frame() %>%
    as_tibble()
  
  estimate_col <- intersect(
    c("response", "emmean", "estimate", "rate", "prob"),
    names(out)
  )[1]
  
  lower_col <- intersect(
    c("asymp.LCL", "lower.CL"),
    names(out)
  )[1]
  
  upper_col <- intersect(
    c("asymp.UCL", "upper.CL"),
    names(out)
  )[1]
  
  if (is.na(estimate_col)) {
    stop(
      "Could not identify the estimate column returned by emmeans. Columns: ",
      paste(names(out), collapse = ", ")
    )
  }
  
  names(out)[names(out) == estimate_col] <- "estimate"
  
  if (!is.na(lower_col)) {
    names(out)[names(out) == lower_col] <- "lower.CL"
  } else {
    out$lower.CL <- NA_real_
  }
  
  if (!is.na(upper_col)) {
    names(out)[names(out) == upper_col] <- "upper.CL"
  } else {
    out$upper.CL <- NA_real_
  }
  
  out
}


# ============================================================
# 6. Build the Period-model formula for one location
# ============================================================

build_period_formula <- function(df, family_code) {
  
  df <- df %>%
    mutate(
      Period = droplevels(Period),
      status = droplevels(status),
      SamplingDate = droplevels(factor(sampling_event_start_date)),
      Site = droplevels(factor(site_code))
    )
  
  has_two_periods <- n_distinct(df$Period) >= 2
  has_two_statuses <- n_distinct(df$status) >= 2
  
  # Examine Period x Status cells before deciding whether the interaction
  # is supportable. Missing cells make a full interaction non-estimable.
  period_status_cells <- df %>%
    group_by(Period, status) %>%
    summarise(
      n_transects = n(),
      n_sites = n_distinct(site_code),
      n_positive = sum(response > 0, na.rm = TRUE),
      all_zero = all(response == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (has_two_periods && has_two_statuses) {
    complete_cells <- tidyr::expand_grid(
      Period = levels(df$Period),
      status = levels(df$status)
    ) %>%
      left_join(period_status_cells, by = c("Period", "status")) %>%
      mutate(n_transects = replace_na(n_transects, 0L))
    
    has_missing_cell <- any(complete_cells$n_transects == 0)
  } else {
    complete_cells <- period_status_cells
    has_missing_cell <- FALSE
  }
  
  # With a log-link Tweedie, an observed interaction cell containing only
  # zeros can drive its cell mean toward the boundary and destabilise a
  # full interaction. In that case retain Period and Status additively.
  # For Gaussian Shannon models this boundary issue does not apply.
  has_all_zero_cell <- family_code == "tweedie" &&
    any(period_status_cells$all_zero %in% TRUE)
  
  fixed_effect <- case_when(
    has_two_periods && has_two_statuses &&
      !has_missing_cell && !has_all_zero_cell ~ "Period * status",
    
    has_two_periods && has_two_statuses ~ "Period + status",
    
    has_two_periods ~ "Period",
    has_two_statuses ~ "status",
    TRUE ~ "1"
  )
  
  structure_reason <- case_when(
    has_two_periods && has_two_statuses &&
      !has_missing_cell && !has_all_zero_cell ~
      "Full Period x Status interaction fitted",
    
    has_two_periods && has_two_statuses && has_missing_cell ~
      "Additive Period + Status fitted because at least one Period x Status cell was absent",
    
    has_two_periods && has_two_statuses && has_all_zero_cell ~
      "Additive Period + Status fitted because at least one Tweedie Period x Status cell contained all zeros",
    
    has_two_periods ~ "Period-only fixed effect fitted because only one Status level was present",
    has_two_statuses ~ "Status-only fixed effect fitted because only one Period level was present",
    TRUE ~ "Intercept-only fixed effect fitted"
  )
  
  # Site random intercept: repeated visits to the same site are correlated.
  site_re <- if (n_distinct(df$Site) > 1) {
    " + (1 | Site)"
  } else {
    ""
  }
  
  # Date random intercept: transects sampled on the same event/date can share
  # short-term environmental conditions. Do not include the random date term
  # when Period is represented by exactly one date per Period, because then
  # Period and Date are completely confounded.
  dates_per_period <- df %>%
    distinct(Period, SamplingDate) %>%
    count(Period, name = "n_dates")
  
  date_confounded_with_period <- has_two_periods &&
    nrow(dates_per_period) == n_distinct(df$Period) &&
    all(dates_per_period$n_dates == 1)
  
  include_date_re <- n_distinct(df$SamplingDate) > 1 &&
    !date_confounded_with_period
  
  date_re <- if (include_date_re) {
    " + (1 | SamplingDate)"
  } else {
    ""
  }
  
  model_formula <- as.formula(
    paste0(
      "response ~ ",
      fixed_effect,
      site_re,
      date_re
    )
  )
  
  list(
    data = df,
    formula = model_formula,
    fixed_effect = fixed_effect,
    structure_reason = structure_reason,
    has_two_periods = has_two_periods,
    has_two_statuses = has_two_statuses,
    has_missing_cell = has_missing_cell,
    has_all_zero_cell = has_all_zero_cell,
    include_site_re = n_distinct(df$Site) > 1,
    include_date_re = include_date_re,
    date_confounded_with_period = date_confounded_with_period,
    cell_summary = period_status_cells
  )
}


# If the full interaction passes the design checks but still fails numerically,
# use an additive Period + Status fallback rather than silently dropping the
# entire location/metric. The diagnostics record that this fallback occurred.
make_additive_period_formula <- function(period_details) {
  
  df <- period_details$data
  
  site_re <- if (period_details$include_site_re) {
    " + (1 | Site)"
  } else {
    ""
  }
  
  date_re <- if (period_details$include_date_re) {
    " + (1 | SamplingDate)"
  } else {
    ""
  }
  
  as.formula(
    paste0(
      "response ~ Period + status",
      site_re,
      date_re
    )
  )
}


# ============================================================
# 7. Extract Period and Period x Status predictions
# ============================================================

extract_period_predictions <- function(model, df, family_code) {
  
  has_two_periods <- n_distinct(df$Period) >= 2
  has_two_statuses <- n_distinct(df$status) >= 2
  
  # Build the smallest EMM grid needed for the factors that actually vary.
  emm_factors <- c(
    if (has_two_periods) "Period",
    if (has_two_statuses) "status"
  )
  
  emm_formula <- if (length(emm_factors) == 0) {
    ~ 1
  } else {
    as.formula(paste("~", paste(emm_factors, collapse = " * ")))
  }
  
  # regrid(transform = "response") is important for Tweedie models:
  # subsequent weighted averages are then averages of RESPONSE-SCALE means,
  # not averages on the log-link scale.
  base_emm <- emmeans::emmeans(model, emm_formula)
  base_response <- emmeans::regrid(base_emm, transform = "response")
  
  base_grid <- as.data.frame(base_response) %>%
    as_tibble()
  
  if (!"Period" %in% names(base_grid)) {
    base_grid$Period <- as.character(unique(df$Period)[1])
  }
  
  if (!"status" %in% names(base_grid)) {
    base_grid$status <- as.character(unique(df$status)[1])
  }
  
  base_grid <- base_grid %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    )
  
  # ----------------------------------------------------------
  # A. Overall Period means
  # ----------------------------------------------------------
  # Within a location, statuses are weighted according to the number of
  # transects actually sampled in that Period. This preserves the decision
  # that every transect contributes equally, rather than forcing Fished and
  # No-take to contribute 50:50 when the sampling design was unequal.
  period_counts <- df %>%
    count(Period, status, name = "n_transects") %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    )
  
  observed_periods <- unique(period_counts$Period)
  
  period_methods <- purrr::map(
    observed_periods,
    function(period_value) {
      
      target_counts <- period_counts %>%
        filter(Period == period_value)
      
      target_counts <- target_counts %>%
        mutate(weight = n_transects / sum(n_transects))
      
      coefs <- rep(0, nrow(base_grid))
      
      for (i in seq_len(nrow(target_counts))) {
        
        idx <- which(
          base_grid$Period == period_value &
            base_grid$status == target_counts$status[[i]]
        )
        
        if (length(idx) != 1) {
          stop(
            "Could not match an emmeans Period x Status cell for ",
            period_value, " / ", target_counts$status[[i]]
          )
        }
        
        coefs[idx] <- target_counts$weight[[i]]
      }
      
      coefs
    }
  )
  
  names(period_methods) <- observed_periods
  
  period_emm <- emmeans::contrast(
    base_response,
    method = period_methods,
    adjust = "none"
  )
  
  period_predictions <- summary(
    period_emm,
    infer = c(TRUE, FALSE),
    level = confidence_level
  ) %>%
    standardise_emmeans_summary() %>%
    transmute(
      Period = as.character(contrast),
      estimate,
      SE,
      lower.CL,
      upper.CL
    )
  
  # ----------------------------------------------------------
  # B. Period x Status means
  # ----------------------------------------------------------
  # Return only combinations that were actually observed in the location.
  # We do NOT output unobserved Period x Status combinations.
  observed_period_status <- df %>%
    distinct(Period, status) %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    )
  
  period_status_predictions <- summary(
    base_response,
    infer = c(TRUE, FALSE),
    level = confidence_level
  ) %>%
    standardise_emmeans_summary()
  
  if (!"Period" %in% names(period_status_predictions)) {
    period_status_predictions$Period <- as.character(unique(df$Period)[1])
  }
  
  if (!"status" %in% names(period_status_predictions)) {
    period_status_predictions$status <- as.character(unique(df$status)[1])
  }
  
  period_status_predictions <- period_status_predictions %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    ) %>%
    inner_join(
      observed_period_status,
      by = c("Period", "status")
    ) %>%
    select(
      Period,
      status,
      estimate,
      SE,
      lower.CL,
      upper.CL
    )
  
  # Replication flags for interpretation.
  status_replication <- df %>%
    group_by(status) %>%
    summarise(
      n_sites_status = n_distinct(site_code),
      n_transects_status = n(),
      .groups = "drop"
    ) %>%
    mutate(status = as.character(status))
  
  period_status_replication <- df %>%
    group_by(Period, status) %>%
    summarise(
      n_sites_period_status = n_distinct(site_code),
      n_transects_period_status = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    )
  
  period_status_predictions <- period_status_predictions %>%
    left_join(status_replication, by = "status") %>%
    left_join(
      period_status_replication,
      by = c("Period", "status")
    ) %>%
    mutate(
      low_rep_status = n_sites_status < minimum_sites_per_status,
      low_rep_period_status =
        n_sites_period_status < minimum_sites_per_status,
      low_replication = low_rep_status | low_rep_period_status
    )
  
  list(
    period = period_predictions,
    period_status = period_status_predictions
  )
}


# ============================================================
# 8. Build and fit the categorical temporal model
# ============================================================

build_temporal_formula <- function(df) {
  
  df <- df %>%
    mutate(
      SamplingDate = droplevels(factor(sampling_event_start_date)),
      Site = droplevels(factor(site_code))
    )
  
  has_two_dates <- n_distinct(df$SamplingDate) >= 2
  include_site_re <- n_distinct(df$Site) > 1
  
  fixed_effect <- if (has_two_dates) {
    "SamplingDate"
  } else {
    "1"
  }
  
  site_re <- if (include_site_re) {
    " + (1 | Site)"
  } else {
    ""
  }
  
  model_formula <- as.formula(
    paste0(
      "response ~ ",
      fixed_effect,
      site_re
    )
  )
  
  list(
    data = df,
    formula = model_formula,
    fixed_effect = fixed_effect,
    has_two_dates = has_two_dates,
    include_site_re = include_site_re
  )
}


extract_temporal_predictions <- function(model, df) {
  
  has_two_dates <- n_distinct(df$SamplingDate) >= 2
  
  temporal_emm <- if (has_two_dates) {
    emmeans::emmeans(model, ~ SamplingDate)
  } else {
    emmeans::emmeans(model, ~ 1)
  }
  
  temporal_response <- emmeans::regrid(
    temporal_emm,
    transform = "response"
  )
  
  out <- summary(
    temporal_response,
    infer = c(TRUE, FALSE),
    level = confidence_level
  ) %>%
    standardise_emmeans_summary()
  
  if (!"SamplingDate" %in% names(out)) {
    out$SamplingDate <- as.character(unique(df$SamplingDate)[1])
  }
  
  date_lookup <- df %>%
    distinct(SamplingDate, sampling_event_start_date, Period) %>%
    mutate(
      SamplingDate = as.character(SamplingDate),
      Period = as.character(Period)
    )
  
  date_replication <- df %>%
    group_by(SamplingDate) %>%
    summarise(
      n_transects = n(),
      n_sites = n_distinct(site_code),
      n_statuses = n_distinct(status),
      n_positive = sum(response > 0, na.rm = TRUE),
      all_zero_date = all(response == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(SamplingDate = as.character(SamplingDate))
  
  out %>%
    mutate(SamplingDate = as.character(SamplingDate)) %>%
    left_join(date_lookup, by = "SamplingDate") %>%
    left_join(date_replication, by = "SamplingDate") %>%
    transmute(
      sampling_event_start_date,
      Period,
      estimate,
      SE,
      lower.CL,
      upper.CL,
      n_transects,
      n_sites,
      n_statuses,
      n_positive,
      all_zero_date
    ) %>%
    arrange(sampling_event_start_date)
}


# ============================================================
# 9. Diagnostics helper
# ============================================================

make_model_diagnostic <- function(
    metric_name,
    location_name,
    region_name,
    family_code,
    model_type,
    model_formula,
    fixed_effect,
    structure_reason,
    fit_result,
    fit_check,
    df,
    extra_note = NA_character_) {
  
  tibble(
    metric = metric_name,
    location = location_name,
    region = region_name,
    model_type = model_type,
    family_code = family_code,
    model_family = get_family_label(family_code),
    formula = formula_text(model_formula),
    fixed_effect_structure = fixed_effect,
    structure_reason = structure_reason,
    n_transects = nrow(df),
    n_sites = n_distinct(df$site_code),
    n_dates = n_distinct(df$sampling_event_start_date),
    n_periods = n_distinct(df$Period),
    n_statuses = n_distinct(df$status),
    n_positive = sum(df$response > 0, na.rm = TRUE),
    prop_zero = mean(df$response == 0, na.rm = TRUE),
    converged = fit_check$converged,
    pdHess = fit_check$pdHess,
    finite_standard_errors = fit_check$finite_standard_errors,
    valid_model = fit_check$valid,
    AIC = fit_check$AIC,
    logLik = fit_check$logLik,
    warnings = fit_result$warnings,
    error = fit_result$error,
    note = extra_note
  )
}


# ============================================================
# 10. Handle an all-zero location/metric without crashing
# ============================================================

# A Tweedie/GLMM is not meaningfully identifiable if every response in the
# complete location x metric dataset is exactly zero. Retain the sampled
# combinations as descriptive zeros, flag them, and do not pretend that a
# model-based standard error was estimated.
make_all_zero_outputs <- function(df) {
  
  period <- df %>%
    distinct(Period) %>%
    transmute(
      Period = as.character(Period),
      estimate = 0,
      SE = NA_real_,
      lower.CL = NA_real_,
      upper.CL = NA_real_
    )
  
  status_replication <- df %>%
    group_by(status) %>%
    summarise(
      n_sites_status = n_distinct(site_code),
      n_transects_status = n(),
      .groups = "drop"
    ) %>%
    mutate(status = as.character(status))
  
  period_status <- df %>%
    group_by(Period, status) %>%
    summarise(
      n_sites_period_status = n_distinct(site_code),
      n_transects_period_status = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Period = as.character(Period),
      status = as.character(status)
    ) %>%
    left_join(status_replication, by = "status") %>%
    mutate(
      estimate = 0,
      SE = NA_real_,
      lower.CL = NA_real_,
      upper.CL = NA_real_,
      low_rep_status = n_sites_status < minimum_sites_per_status,
      low_rep_period_status =
        n_sites_period_status < minimum_sites_per_status,
      low_replication = low_rep_status | low_rep_period_status
    ) %>%
    select(
      Period,
      status,
      estimate,
      SE,
      lower.CL,
      upper.CL,
      n_sites_status,
      n_transects_status,
      n_sites_period_status,
      n_transects_period_status,
      low_rep_status,
      low_rep_period_status,
      low_replication
    )
  
  temporal <- df %>%
    group_by(sampling_event_start_date, Period) %>%
    summarise(
      n_transects = n(),
      n_sites = n_distinct(site_code),
      n_statuses = n_distinct(status),
      n_positive = 0L,
      all_zero_date = TRUE,
      .groups = "drop"
    ) %>%
    transmute(
      sampling_event_start_date,
      Period = as.character(Period),
      estimate = 0,
      SE = NA_real_,
      lower.CL = NA_real_,
      upper.CL = NA_real_,
      n_transects,
      n_sites,
      n_statuses,
      n_positive,
      all_zero_date
    )
  
  list(period = period, period_status = period_status, temporal = temporal)
}


# ============================================================
# 11. Fit one metric within one LOCATION
# ============================================================

fit_one_location_metric <- function(df) {
  
  metric_name <- as.character(unique(df$metric)[1])
  location_name <- unique(df$location)[1]
  region_name <- unique(df$region)[1]
  family_code <- unique(df$family_code)[1]
  
  if (length(unique(df$region)) != 1) {
    stop("Location maps to more than one region: ", location_name)
  }
  
  if (length(unique(df$family_code)) != 1) {
    stop("More than one family_code found for metric: ", metric_name)
  }
  
  message("------------------------------------------------------------")
  message("Metric:   ", metric_name)
  message("Location: ", location_name)
  message("Family:   ", get_family_label(family_code))
  message("Transects: ", nrow(df))
  
  # With fewer than 3 transects there is not enough information for a useful
  # mixed-model variance estimate. Record the failure rather than forcing it.
  if (nrow(df) < 3) {
    
    diagnostic <- tibble(
      metric = metric_name,
      location = location_name,
      region = region_name,
      model_type = c("Period", "Temporal"),
      family_code = family_code,
      model_family = get_family_label(family_code),
      formula = NA_character_,
      fixed_effect_structure = NA_character_,
      structure_reason = "Model not fitted: fewer than 3 transects",
      n_transects = nrow(df),
      n_sites = n_distinct(df$site_code),
      n_dates = n_distinct(df$sampling_event_start_date),
      n_periods = n_distinct(df$Period),
      n_statuses = n_distinct(df$status),
      n_positive = sum(df$response > 0, na.rm = TRUE),
      prop_zero = mean(df$response == 0, na.rm = TRUE),
      converged = FALSE,
      pdHess = FALSE,
      finite_standard_errors = FALSE,
      valid_model = FALSE,
      AIC = NA_real_,
      logLik = NA_real_,
      warnings = NA_character_,
      error = "Fewer than 3 transects",
      note = NA_character_
    )
    
    return(list(
      period_model = NULL,
      temporal_model = NULL,
      period = tibble(),
      period_status = tibble(),
      temporal = tibble(),
      diagnostics = diagnostic
    ))
  }
  
  # Entire location/metric is zero: retain descriptive zeros but do not claim
  # a model was identifiable.
  if (all(df$response == 0, na.rm = TRUE)) {
    
    zero_outputs <- make_all_zero_outputs(df)
    
    diagnostic <- tibble(
      metric = metric_name,
      location = location_name,
      region = region_name,
      model_type = c("Period", "Temporal"),
      family_code = family_code,
      model_family = get_family_label(family_code),
      formula = NA_character_,
      fixed_effect_structure = NA_character_,
      structure_reason = "Model not fitted: all responses were zero",
      n_transects = nrow(df),
      n_sites = n_distinct(df$site_code),
      n_dates = n_distinct(df$sampling_event_start_date),
      n_periods = n_distinct(df$Period),
      n_statuses = n_distinct(df$status),
      n_positive = 0L,
      prop_zero = 1,
      converged = FALSE,
      pdHess = FALSE,
      finite_standard_errors = FALSE,
      valid_model = FALSE,
      AIC = NA_real_,
      logLik = NA_real_,
      warnings = NA_character_,
      error = "All responses were zero; descriptive zero retained",
      note = "estimate_source = descriptive all-zero data"
    )
    
    # Add the same identifiers used by model-based outputs. This matters
    # because all-zero location/metric combinations still contribute a real
    # sampled zero to equal-weight regional means.
    add_zero_ids <- function(x) {
      x %>%
        mutate(
          metric = metric_name,
          location = location_name,
          region = region_name,
          family_code = family_code,
          model_family = get_family_label(family_code),
          spatial_level = "location",
          group_name = location_name,
          estimate_source = "descriptive_all_zero"
        )
    }
    
    return(list(
      period_model = NULL,
      temporal_model = NULL,
      period = add_zero_ids(zero_outputs$period),
      period_status = add_zero_ids(zero_outputs$period_status),
      temporal = add_zero_ids(zero_outputs$temporal),
      diagnostics = diagnostic
    ))
  }
  
  family_object <- get_family_object(family_code)
  
  # ----------------------------------------------------------
  # 11A. Period / Period x Status model
  # ----------------------------------------------------------
  period_details <- build_period_formula(df, family_code)
  
  period_fit <- fit_glmmTMB_safely(
    formula = period_details$formula,
    data = period_details$data,
    family_object = family_object
  )
  
  period_check <- assess_model_fit(period_fit$model)
  period_formula_used <- period_details$formula
  period_fixed_used <- period_details$fixed_effect
  period_reason_used <- period_details$structure_reason
  period_fallback_note <- NA_character_
  
  # Numerical fallback: if a full interaction fails, fit the additive model.
  if (
    !period_check$valid &&
    identical(period_details$fixed_effect, "Period * status")
  ) {
    
    additive_formula <- make_additive_period_formula(period_details)
    
    additive_fit <- fit_glmmTMB_safely(
      formula = additive_formula,
      data = period_details$data,
      family_object = family_object
    )
    
    additive_check <- assess_model_fit(additive_fit$model)
    
    if (additive_check$valid) {
      period_fit <- additive_fit
      period_check <- additive_check
      period_formula_used <- additive_formula
      period_fixed_used <- "Period + status"
      period_reason_used <-
        "Additive Period + Status fallback used because the full interaction model was numerically invalid"
      period_fallback_note <- "Full interaction failed; additive fallback retained"
    }
  }
  
  period_diagnostic <- make_model_diagnostic(
    metric_name = metric_name,
    location_name = location_name,
    region_name = region_name,
    family_code = family_code,
    model_type = "Period",
    model_formula = period_formula_used,
    fixed_effect = period_fixed_used,
    structure_reason = period_reason_used,
    fit_result = period_fit,
    fit_check = period_check,
    df = df,
    extra_note = period_fallback_note
  )
  
  period_predictions <- tibble()
  period_status_predictions <- tibble()
  
  if (period_check$valid) {
    
    extracted_period <- tryCatch(
      extract_period_predictions(
        model = period_fit$model,
        df = period_details$data,
        family_code = family_code
      ),
      error = function(e) e
    )
    
    if (inherits(extracted_period, "error")) {
      period_diagnostic$error <- combine_messages(
        period_diagnostic$error,
        paste0(
          "emmeans extraction failed: ",
          conditionMessage(extracted_period)
        )
      )
    } else {
      period_predictions <- extracted_period$period %>%
        mutate(estimate_source = "model")
      
      period_status_predictions <- extracted_period$period_status %>%
        mutate(estimate_source = "model")
    }
  }
  
  # ----------------------------------------------------------
  # 11B. Temporal model: categorical observed sampling dates only
  # ----------------------------------------------------------
  # A Tweedie model with a log link can become numerically unstable when an
  # entire categorical date contains zeros and nothing else, because the
  # date-specific fitted mean is pushed to the boundary at zero. We do NOT
  # discard those sampled dates. Instead, all-zero dates are retained in the
  # final output as observed/descriptive zeros (with no model-based SE), while
  # the remaining sampled dates are fitted with the temporal GLMM. Dates with
  # a mixture of zeros and positive observations remain in the Tweedie model.
  temporal_date_zero_summary <- df %>%
    group_by(sampling_event_start_date) %>%
    summarise(
      all_zero_date = all(response == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  all_zero_temporal_dates <- if (family_code == "tweedie") {
    temporal_date_zero_summary %>%
      filter(all_zero_date) %>%
      pull(sampling_event_start_date)
  } else {
    as.Date(character())
  }
  
  temporal_model_df <- if (length(all_zero_temporal_dates) > 0) {
    df %>%
      filter(!sampling_event_start_date %in% all_zero_temporal_dates)
  } else {
    df
  }
  
  # The complete location x metric all-zero case was handled above, so a
  # Tweedie dataset reaching here must still have at least one modelled date.
  temporal_details <- build_temporal_formula(temporal_model_df)
  
  temporal_fit <- fit_glmmTMB_safely(
    formula = temporal_details$formula,
    data = temporal_details$data,
    family_object = family_object
  )
  
  temporal_check <- assess_model_fit(temporal_fit$model)
  
  temporal_structure_reason <- paste0(
    "Sampling date fitted as a categorical fixed effect; no Date x Status interaction",
    if (length(all_zero_temporal_dates) > 0) {
      paste0(
        "; ",
        length(all_zero_temporal_dates),
        " all-zero Tweedie date(s) retained descriptively at zero"
      )
    } else {
      ""
    }
  )
  
  temporal_diagnostic <- make_model_diagnostic(
    metric_name = metric_name,
    location_name = location_name,
    region_name = region_name,
    family_code = family_code,
    model_type = "Temporal",
    model_formula = temporal_details$formula,
    fixed_effect = temporal_details$fixed_effect,
    structure_reason = temporal_structure_reason,
    fit_result = temporal_fit,
    fit_check = temporal_check,
    df = temporal_model_df,
    extra_note = if (length(all_zero_temporal_dates) > 0) {
      paste0(
        "Temporal model excludes ",
        length(all_zero_temporal_dates),
        " all-zero date(s) from fitting only; those sampled dates are retained as descriptive zero predictions"
      )
    } else {
      NA_character_
    }
  )
  
  temporal_predictions <- tibble()
  
  if (temporal_check$valid) {
    
    extracted_temporal <- tryCatch(
      extract_temporal_predictions(
        model = temporal_fit$model,
        df = temporal_details$data
      ),
      error = function(e) e
    )
    
    if (inherits(extracted_temporal, "error")) {
      temporal_diagnostic$error <- combine_messages(
        temporal_diagnostic$error,
        paste0(
          "emmeans extraction failed: ",
          conditionMessage(extracted_temporal)
        )
      )
    } else {
      temporal_predictions <- extracted_temporal %>%
        mutate(estimate_source = "model")
    }
  }
  
  # Append the all-zero dates so EVERY sampled date remains in the output.
  # SE/CI are intentionally NA because these rows are observed zeros rather
  # than model-estimated marginal means.
  if (length(all_zero_temporal_dates) > 0) {
    
    descriptive_zero_dates <- df %>%
      filter(sampling_event_start_date %in% all_zero_temporal_dates) %>%
      group_by(sampling_event_start_date, Period) %>%
      summarise(
        n_transects = n(),
        n_sites = n_distinct(site_code),
        n_statuses = n_distinct(status),
        n_positive = 0L,
        all_zero_date = TRUE,
        .groups = "drop"
      ) %>%
      transmute(
        sampling_event_start_date,
        Period = as.character(Period),
        estimate = 0,
        SE = NA_real_,
        lower.CL = NA_real_,
        upper.CL = NA_real_,
        n_transects,
        n_sites,
        n_statuses,
        n_positive,
        all_zero_date,
        estimate_source = "descriptive_all_zero_date"
      )
    
    temporal_predictions <- bind_rows(
      temporal_predictions,
      descriptive_zero_dates
    ) %>%
      arrange(sampling_event_start_date)
  }
  
  # Add common identifiers to all successful/descriptive output rows.
  add_ids <- function(x) {
    if (nrow(x) == 0) {
      return(x)
    }
    
    x %>%
      mutate(
        metric = metric_name,
        location = location_name,
        region = region_name,
        family_code = family_code,
        model_family = get_family_label(family_code),
        spatial_level = "location",
        group_name = location_name
      )
  }
  
  list(
    period_model = period_fit$model,
    temporal_model = temporal_fit$model,
    period = add_ids(period_predictions),
    period_status = add_ids(period_status_predictions),
    temporal = add_ids(temporal_predictions),
    diagnostics = bind_rows(
      period_diagnostic,
      temporal_diagnostic
    )
  )
}


# ============================================================
# 12. Run every metric x location combination
# ============================================================

location_groups <- all_dat %>%
  group_by(metric, location) %>%
  group_split(.keep = TRUE)

location_fits <- purrr::map(
  location_groups,
  ~ tryCatch(
    fit_one_location_metric(.x),
    error = function(e) {
      
      metric_name <- as.character(unique(.x$metric)[1])
      location_name <- unique(.x$location)[1]
      region_name <- unique(.x$region)[1]
      family_code <- unique(.x$family_code)[1]
      
      list(
        period_model = NULL,
        temporal_model = NULL,
        period = tibble(),
        period_status = tibble(),
        temporal = tibble(),
        diagnostics = tibble(
          metric = metric_name,
          location = location_name,
          region = region_name,
          model_type = "General model error",
          family_code = family_code,
          model_family = get_family_label(family_code),
          formula = NA_character_,
          fixed_effect_structure = NA_character_,
          structure_reason = NA_character_,
          n_transects = nrow(.x),
          n_sites = n_distinct(.x$site_code),
          n_dates = n_distinct(.x$sampling_event_start_date),
          n_periods = n_distinct(.x$Period),
          n_statuses = n_distinct(.x$status),
          n_positive = sum(.x$response > 0, na.rm = TRUE),
          prop_zero = mean(.x$response == 0, na.rm = TRUE),
          converged = FALSE,
          pdHess = FALSE,
          finite_standard_errors = FALSE,
          valid_model = FALSE,
          AIC = NA_real_,
          logLik = NA_real_,
          warnings = NA_character_,
          error = conditionMessage(e),
          note = NA_character_
        )
      )
    }
  )
)

location_period_results <- map_dfr(location_fits, "period")
location_period_status_results <- map_dfr(location_fits, "period_status")
location_temporal_results <- map_dfr(location_fits, "temporal")
model_diagnostics <- map_dfr(location_fits, "diagnostics")


# Save the fitted model objects as RDS so individual models can be inspected
# later without refitting everything.
model_index <- map_dfr(
  seq_along(location_fits),
  function(i) {
    x <- location_groups[[i]]
    tibble(
      fit_index = i,
      metric = as.character(unique(x$metric)[1]),
      location = unique(x$location)[1],
      region = unique(x$region)[1]
    )
  }
)

saveRDS(
  list(
    index = model_index,
    fits = location_fits
  ),
  file.path(model_output_root, "location_model_objects.rds")
)


# ============================================================
# 13. Create REGION predictions with equal LOCATION weights
# ============================================================

# Why aggregate location predictions instead of simply pooling all regional
# transects in one GLMM?
#
# If a region has two locations and one location contains many more sites or
# transects, pooling would allow that location to dominate the regional mean.
# The explicit design choice was that locations should have equal weight.
# Therefore:
#   regional mean = mean(location predicted means)
# for locations that ACTUALLY contain data for the requested Period,
# Period x Status cell, or exact sampling date.
#
# This also prevents regional temporal outputs from predicting a location on
# a date when that location was not sampled.

z_crit <- qnorm(1 - (1 - confidence_level) / 2)


summarise_equal_location <- function(df, group_vars) {
  
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      estimate = mean(estimate, na.rm = TRUE),
      n_locations_contributing = n_distinct(location[!is.na(estimate)]),
      all_location_SE_available = all(is.finite(SE[!is.na(estimate)])),
      SE = if (
        n_locations_contributing > 0 &&
        all_location_SE_available
      ) {
        sqrt(sum(SE[!is.na(estimate)]^2)) /
          n_locations_contributing
      } else {
        NA_real_
      },
      estimate_source = if_else(
        all(estimate_source == "model"),
        "equal_weight_location_models",
        "equal_weight_includes_descriptive_all_zero"
      ),
      .groups = "drop"
    ) %>%
    mutate(
      lower.CL = estimate - z_crit * SE,
      upper.CL = estimate + z_crit * SE,
      # Tweedie means cannot be negative. The regional CI is a response-scale
      # normal approximation, so truncate only the lower confidence limit.
      lower.CL = if_else(
        family_code == "tweedie" & !is.na(lower.CL),
        pmax(lower.CL, 0),
        lower.CL
      )
    )
}


# ------------------------------------------------------------
# 13A. Regional Period means
# ------------------------------------------------------------

region_period_expected <- all_dat %>%
  distinct(metric, region, location, Period) %>%
  mutate(Period = as.character(Period)) %>%
  count(metric, region, Period, name = "n_locations_sampled")

region_period_results <- location_period_results %>%
  summarise_equal_location(
    group_vars = c(
      "metric",
      "region",
      "family_code",
      "model_family",
      "Period"
    )
  ) %>%
  left_join(
    region_period_expected,
    by = c("metric", "region", "Period")
  ) %>%
  mutate(
    spatial_level = "region",
    group_name = region,
    incomplete_location_models =
      n_locations_contributing < n_locations_sampled
  )


# ------------------------------------------------------------
# 13B. Regional Period x Status means
# ------------------------------------------------------------

region_period_status_expected <- all_dat %>%
  distinct(metric, region, location, Period, status) %>%
  mutate(
    Period = as.character(Period),
    status = as.character(status)
  ) %>%
  count(
    metric,
    region,
    Period,
    status,
    name = "n_locations_sampled"
  )

region_status_replication <- all_dat %>%
  group_by(metric, region, status) %>%
  summarise(
    n_sites_status = n_distinct(site_code),
    n_transects_status = n(),
    .groups = "drop"
  ) %>%
  mutate(status = as.character(status))

region_period_status_replication <- all_dat %>%
  group_by(metric, region, Period, status) %>%
  summarise(
    n_sites_period_status = n_distinct(site_code),
    n_transects_period_status = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Period = as.character(Period),
    status = as.character(status)
  )

# Flag complete location/status confounding for a Period: e.g. one location
# contributes only Fished and the other only No-take, with no location that
# contains both statuses. Means can still be shown, but a regional Status
# contrast would not cleanly separate management status from location.
region_status_location_confounding <- all_dat %>%
  distinct(metric, region, Period, location, status) %>%
  mutate(
    Period = as.character(Period),
    status = as.character(status)
  ) %>%
  group_by(metric, region, Period, location) %>%
  summarise(
    n_statuses_location = n_distinct(status),
    .groups = "drop"
  ) %>%
  group_by(metric, region, Period) %>%
  summarise(
    any_location_has_both_statuses = any(n_statuses_location >= 2),
    .groups = "drop"
  ) %>%
  left_join(
    all_dat %>%
      distinct(metric, region, Period, status) %>%
      mutate(Period = as.character(Period)) %>%
      count(metric, region, Period, name = "n_statuses_region"),
    by = c("metric", "region", "Period")
  ) %>%
  mutate(
    status_location_confounded =
      n_statuses_region >= 2 & !any_location_has_both_statuses
  ) %>%
  select(metric, region, Period, status_location_confounded)

region_period_status_results <- location_period_status_results %>%
  summarise_equal_location(
    group_vars = c(
      "metric",
      "region",
      "family_code",
      "model_family",
      "Period",
      "status"
    )
  ) %>%
  left_join(
    region_period_status_expected,
    by = c("metric", "region", "Period", "status")
  ) %>%
  left_join(
    region_status_replication,
    by = c("metric", "region", "status")
  ) %>%
  left_join(
    region_period_status_replication,
    by = c("metric", "region", "Period", "status")
  ) %>%
  left_join(
    region_status_location_confounding,
    by = c("metric", "region", "Period")
  ) %>%
  mutate(
    low_rep_status = n_sites_status < minimum_sites_per_status,
    low_rep_period_status =
      n_sites_period_status < minimum_sites_per_status,
    low_replication = low_rep_status | low_rep_period_status,
    incomplete_location_models =
      n_locations_contributing < n_locations_sampled,
    spatial_level = "region",
    group_name = region
  )


# ------------------------------------------------------------
# 13C. Regional temporal means
# ------------------------------------------------------------

# Only locations actually sampled on an exact date are averaged. A region with
# two locations therefore has:
#   - an equal average of two locations if both were sampled that date;
#   - the sampled location only if the other location was not sampled.
# Nothing is predicted for an unsampled location/date combination.
region_temporal_expected <- all_dat %>%
  distinct(metric, region, location, sampling_event_start_date) %>%
  count(
    metric,
    region,
    sampling_event_start_date,
    name = "n_locations_sampled"
  )

region_temporal_replication <- all_dat %>%
  group_by(metric, region, sampling_event_start_date) %>%
  summarise(
    Period = as.character(first(Period)),
    n_transects = n(),
    n_sites = n_distinct(site_code),
    n_statuses = n_distinct(status),
    n_positive = sum(response > 0, na.rm = TRUE),
    all_zero_date = all(response == 0, na.rm = TRUE),
    .groups = "drop"
  )

region_temporal_results <- location_temporal_results %>%
  summarise_equal_location(
    group_vars = c(
      "metric",
      "region",
      "family_code",
      "model_family",
      "sampling_event_start_date",
      "Period"
    )
  ) %>%
  left_join(
    region_temporal_expected,
    by = c("metric", "region", "sampling_event_start_date")
  ) %>%
  left_join(
    region_temporal_replication %>%
      select(
        metric,
        region,
        sampling_event_start_date,
        n_transects,
        n_sites,
        n_statuses,
        n_positive,
        all_zero_date
      ),
    by = c("metric", "region", "sampling_event_start_date")
  ) %>%
  mutate(
    incomplete_location_models =
      n_locations_contributing < n_locations_sampled,
    spatial_level = "region",
    group_name = region
  )


# ============================================================
# 14. Combine location and region outputs
# ============================================================

period_results <- bind_rows(
  location_period_results,
  region_period_results
) %>%
  mutate(
    Period = factor(Period, levels = period_levels),
    metric = factor(metric, levels = levels(all_dat$metric))
  ) %>%
  arrange(spatial_level, group_name, metric, Period)

period_status_results <- bind_rows(
  location_period_status_results,
  region_period_status_results
) %>%
  mutate(
    Period = factor(Period, levels = period_levels),
    status = factor(status, levels = status_levels),
    metric = factor(metric, levels = levels(all_dat$metric))
  ) %>%
  arrange(spatial_level, group_name, metric, Period, status)

temporal_results <- bind_rows(
  location_temporal_results,
  region_temporal_results
) %>%
  mutate(
    Period = factor(Period, levels = period_levels),
    metric = factor(metric, levels = levels(all_dat$metric))
  ) %>%
  arrange(
    spatial_level,
    group_name,
    metric,
    sampling_event_start_date
  )


# ============================================================
# 15. Save tables
# ============================================================

readr::write_excel_csv(
  period_results,
  file.path(model_output_root, "period_predictions.csv")
)

readr::write_excel_csv(
  period_status_results,
  file.path(model_output_root, "period_status_predictions.csv")
)

readr::write_excel_csv(
  temporal_results,
  file.path(model_output_root, "temporal_predictions.csv")
)

readr::write_excel_csv(
  model_diagnostics,
  file.path(model_output_root, "model_diagnostics.csv")
)

readr::write_excel_csv(
  data_availability,
  file.path(model_output_root, "data_availability.csv")
)

readr::write_excel_csv(
  duplicate_metric_transects,
  file.path(model_output_root, "duplicate_metric_transects.csv")
)

readr::write_excel_csv(
  site_status_check,
  file.path(model_output_root, "site_status_check.csv")
)

writexl::write_xlsx(
  list(
    period_predictions = period_results,
    period_status = period_status_results,
    temporal_predictions = temporal_results,
    model_diagnostics = model_diagnostics,
    data_availability = data_availability
  ),
  file.path(model_output_root, "uvc_glmm_prediction_results.xlsx")
)


# ============================================================
# 16. Plot helpers
# ============================================================

make_safe_filename <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}


prediction_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.4),
    axis.line.y = element_line(colour = "black", linewidth = 0.4),
    legend.position = "right"
  )


plot_period_prediction <- function(df) {
  
  ggplot(df, aes(x = Period, y = estimate, fill = Period)) +
    geom_col(
      width = 0.62,
      colour = "black",
      alpha = 0.9
    ) +
    geom_errorbar(
      data = df %>% filter(is.finite(lower.CL), is.finite(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.18,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = period_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = "Predicted mean (95% CI)",
      title = unique(df$group_name),
      subtitle = unique(as.character(df$metric)),
      fill = NULL
    ) +
    prediction_theme +
    theme(legend.position = "none")
}


plot_period_status_prediction <- function(df) {
  
  # After location and regional results are bound together,
  # status_location_confounded is NA for location rows and TRUE/FALSE for
  # regional rows. Build the annotation from independent flags so a result
  # can show both * (low replication) and dagger (Location confounding).
  plot_df <- df %>%
    mutate(
      status_location_confounded = tidyr::replace_na(
        status_location_confounded,
        FALSE
      ),
      flag_label = paste0(
        if_else(low_replication %in% TRUE, "*", ""),
        if_else(status_location_confounded, "\u2020", "")
      ),
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
      aes(y = flag_y, label = flag_label, group = status),
      position = position_dodge(width = 0.72),
      vjust = -0.5,
      size = 5
    ) +
    scale_fill_manual(values = status_cols, drop = FALSE) +
    labs(
      x = NULL,
      y = "Predicted mean (95% CI)",
      title = unique(df$group_name),
      subtitle = unique(as.character(df$metric)),
      fill = NULL,
      caption = paste0(
        "* status or Period x Status cell has <",
        minimum_sites_per_status,
        " sites; \u2020 regional Status comparison is confounded with Location"
      )
    ) +
    prediction_theme
}


plot_temporal_prediction <- function(df) {
  
  plot_df <- df %>%
    arrange(sampling_event_start_date)
  
  ggplot(
    plot_df,
    aes(
      x = sampling_event_start_date,
      y = estimate,
      colour = Period
    )
  ) +
    geom_errorbar(
      data = plot_df %>%
        filter(is.finite(lower.CL), is.finite(upper.CL)),
      aes(ymin = lower.CL, ymax = upper.CL),
      width = 18,
      linewidth = 0.55
    ) +
    geom_point(size = 3) +
    scale_colour_manual(values = period_cols, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(
      x = NULL,
      y = "Predicted mean (95% CI)",
      title = unique(df$group_name),
      subtitle = unique(as.character(df$metric)),
      colour = NULL
    ) +
    prediction_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}


# ============================================================
# 17. Save plots
# ============================================================

save_prediction_plots <- function(
    results_df,
    plot_fun,
    result_type) {
  
  plot_df <- results_df %>%
    filter(
      as.character(metric) %in% plot_metric_order,
      !is.na(estimate)
    )
  
  if (nrow(plot_df) == 0) {
    return(invisible(NULL))
  }
  
  split_groups <- plot_df %>%
    group_by(spatial_level, group_name, metric) %>%
    group_split(.keep = TRUE)
  
  purrr::walk(
    split_groups,
    function(x) {
      
      spatial_level_value <- unique(x$spatial_level)[1]
      group_name_value <- unique(x$group_name)[1]
      metric_value <- as.character(unique(x$metric)[1])
      
      output_dir <- file.path(
        plot_output_root,
        spatial_level_value,
        result_type
      )
      
      dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      filename <- paste0(
        make_safe_filename(group_name_value),
        "__",
        make_safe_filename(metric_value),
        "__",
        result_type,
        ".png"
      )
      
      ggplot2::ggsave(
        filename = file.path(output_dir, filename),
        plot = plot_fun(x),
        width = if (result_type == "temporal") 9 else 6.5,
        height = 5.5,
        dpi = 300,
        bg = "white"
      )
    }
  )
  
  invisible(NULL)
}


save_prediction_plots(
  period_results,
  plot_period_prediction,
  "period"
)

save_prediction_plots(
  period_status_results,
  plot_period_status_prediction,
  "period_status"
)

save_prediction_plots(
  temporal_results,
  plot_temporal_prediction,
  "temporal"
)


# ============================================================
# 18. Useful summary checks after the run
# ============================================================

# Models that should be inspected before inference.
model_problems <- model_diagnostics %>%
  filter(
    !valid_model |
      !is.na(error) |
      !is.na(warnings)
  ) %>%
  arrange(metric, location, model_type)

readr::write_excel_csv(
  model_problems,
  file.path(model_output_root, "model_problems_to_check.csv")
)


# Low-replication Period x Status predictions.
low_replication_results <- period_status_results %>%
  filter(low_replication %in% TRUE)

readr::write_excel_csv(
  low_replication_results,
  file.path(model_output_root, "low_replication_period_status.csv")
)


# Regional Status comparisons that are completely confounded with Location.
regional_status_location_confounded <- period_status_results %>%
  filter(
    spatial_level == "region",
    status_location_confounded %in% TRUE
  )

readr::write_excel_csv(
  regional_status_location_confounded,
  file.path(
    model_output_root,
    "regional_status_location_confounded.csv"
  )
)


# Regional temporal dates where not all sampled locations produced a valid
# location-level prediction (usually because one location model failed).
incomplete_regional_temporal <- temporal_results %>%
  filter(
    spatial_level == "region",
    incomplete_location_models %in% TRUE
  )

readr::write_excel_csv(
  incomplete_regional_temporal,
  file.path(
    model_output_root,
    "incomplete_regional_temporal_predictions.csv"
  )
)


# Print concise end-of-run summaries to the console.
model_diagnostics %>%
  count(model_type, family_code, valid_model) %>%
  print(n = Inf)

period_status_results %>%
  count(spatial_level, low_replication) %>%
  print(n = Inf)


# ============================================================
# 19. Optional DHARMa diagnostic helper
# ============================================================

# This is intentionally not run automatically for every model because there
# can be many location x metric models and simulation diagnostics are slow.
# Use it for models highlighted in model_problems_to_check.csv and for a
# representative selection of otherwise valid models.
#
# Example:
#   dh <- run_dharma_check(
#     metric_name = "M1 fish species richness",
#     location_name = "Metro",
#     model_type = "period",
#     nsim = 1000
#   )
#   plot(dh$simulation)
#   dh$uniformity
#   dh$dispersion
#   dh$outliers

run_dharma_check <- function(
    metric_name,
    location_name,
    model_type = c("period", "temporal"),
    nsim = 1000) {
  
  model_type <- match.arg(model_type)
  
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop("Install DHARMa first: install.packages('DHARMa')")
  }
  
  idx <- model_index %>%
    filter(
      metric == metric_name,
      location == location_name
    )
  
  if (nrow(idx) != 1) {
    stop(
      "Could not uniquely identify metric/location in model_index."
    )
  }
  
  fit_object <- location_fits[[idx$fit_index[[1]]]]
  
  model <- if (model_type == "period") {
    fit_object$period_model
  } else {
    fit_object$temporal_model
  }
  
  if (is.null(model)) {
    stop("The requested model is NULL / was not successfully fitted.")
  }
  
  sim <- DHARMa::simulateResiduals(
    fittedModel = model,
    n = nsim
  )
  
  list(
    simulation = sim,
    uniformity = DHARMa::testUniformity(sim),
    dispersion = DHARMa::testDispersion(sim),
    outliers = DHARMa::testOutliers(sim)
  )
}


#################################################################
# END OF SCRIPT
#################################################################