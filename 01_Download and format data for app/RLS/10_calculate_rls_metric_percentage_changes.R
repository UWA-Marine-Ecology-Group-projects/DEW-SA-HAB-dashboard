#################################################################
# 10. Calculate percentage changes in observed RLS metrics
#
# Calculates, for RLS locations and regions:
#   1. Pre-bloom -> Bloom percentage change, overall
#   2. Pre-bloom -> Bloom percentage change, by status
#   3. Pre-bloom -> each Bloom period_split percentage change, overall
#   4. Pre-bloom -> each Bloom period_split percentage change, by status
#
# Metrics:
#   - Species richness: M1 fish, M2 fish, M2 invertebrates
#   - Total abundance: M1 fish, M2 fish, M2 invertebrates
#   - M2 invertebrate abundance by phylum
#   - B20 biomass: M1 fish, M2 fish
#   - Shannon diversity: M1 fish, M2 fish, M2 invertebrates
#
# Percentage change is:
#   ((comparison mean / pre-bloom mean) * 100) - 100
#
# Therefore:
#    0 = no change
#  -50 = 50% lower than pre-bloom
#  +25 = 25% higher than pre-bloom
#################################################################

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

# -----------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------

metric_input_dir <- "data/rls_metrics_for_modelling"
output_dir <- "outputs/rls_metric_percentage_changes"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

status_levels <- c("No-take", "Fished")


metric_sources <- tibble::tribble(
  ~metric_group,               ~path,                                                   ~value_col,          ~filter_type,
  "species_richness",         file.path(metric_input_dir, "species_richness.rds"),    "species_richness", "all",
  "total_abundance",          file.path(metric_input_dir, "total_abundance.rds"),     "abundance",        "all",
  "invert_phylum_abundance",  file.path(metric_input_dir, "abundance.rds"),           "abundance",        "invert_phylum",
  "b20",                      file.path(metric_input_dir, "b20.rds"),                 "b20_kg",           "all",
  "shannon_diversity",        file.path(metric_input_dir, "shannon_diversity.rds"),   "shannon",          "all"
)

# -----------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------

# Some joins can create status.x/status.y instead of a single status column.
# Resolve those automatically where possible.
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
      " does not contain a `status` column. ",
      "Status is required to calculate No-take and Fished changes."
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

# Read one saved metric table and convert it to a common structure.
load_metric_table <- function(
    metric_group,
    path,
    value_col,
    filter_type) {


  data <- readr::read_rds(path) %>%
    resolve_status_column(source_name = path)

  required_columns <- c(
    "transect",
    "metric",
    "location",
    "region",
    "status",
    "period",
    "period_split",
    "site_name",
    "site_code",
    value_col
  )

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "Missing column(s) in ",
      path,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # The full abundance file contains both the three total-abundance metrics
  # and the M2 invertebrate phylum-specific metrics. Keep only phyla here so
  # the total M2 invertebrate abundance is not duplicated.
  if (filter_type == "invert_phylum") {
    data <- data %>%
      dplyr::filter(
        stringr::str_detect(
          as.character(metric),
          "^M2 invertebrate .+ abundance$"
        ),
        as.character(metric) != "M2 invertebrate total abundance"
      )
  }

  data %>%
    dplyr::transmute(
      transect = as.character(transect),
      location = as.character(location),
      region = as.character(region),
      site_name = as.character(site_name),
      site_code = as.character(site_code),
      status = as.character(status),
      period = as.character(period),
      period_split = as.character(period_split),
      metric_group = .env$metric_group,
      metric = as.character(metric),
      value = as.numeric(.data[[value_col]])
    )
}

# Expand the common sample table to location and region rows so every later
# operation can be run once.
expand_spatial_levels <- function(data) {

  dplyr::bind_rows(
    data %>%
      dplyr::transmute(
        spatial_level = "location",
        spatial_group = location,
        transect,
        status,
        period,
        period_split,
        metric_group,
        metric,
        value
      ),
    data %>%
      dplyr::transmute(
        spatial_level = "region",
        spatial_group = region,
        transect,
        status,
        period,
        period_split,
        metric_group,
        metric,
        value
      )
  ) %>%
    dplyr::filter(
      !is.na(spatial_group),
      stringr::str_trim(spatial_group) != ""
    )
}

# Mean, SD, SE and transect count for any requested grouping.
summarise_metric_values <- function(data, grouping_vars) {

  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_vars))
    ) %>%
    dplyr::summarise(
      n_transects = sum(!is.na(value)),
      mean_value = mean(value, na.rm = TRUE),
      sd_value = stats::sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mean_value = dplyr::if_else(
        n_transects == 0,
        NA_real_,
        mean_value
      ),
      sd_value = dplyr::if_else(
        n_transects > 1,
        sd_value,
        NA_real_
      ),
      se_value = dplyr::if_else(
        n_transects > 1,
        sd_value / sqrt(n_transects),
        NA_real_
      )
    )
}

# Add descriptive percentage-change fields and the same impact categories used
# in the previous HAB percentage calculations.
add_change_fields <- function(data) {

  data %>%
    dplyr::mutate(
      absolute_change = comparison_mean - pre_bloom_mean,

      # Percentage change from a zero baseline is mathematically undefined.
      percentage = dplyr::if_else(
        !is.na(pre_bloom_mean) &
          !is.na(comparison_mean) &
          pre_bloom_mean != 0,
        comparison_mean / pre_bloom_mean * 100,
        NA_real_
      ),

      percentage_change = percentage - 100,

      baseline_zero = !is.na(pre_bloom_mean) & pre_bloom_mean == 0,

      impact = dplyr::case_when(
        is.na(pre_bloom_mean) | is.na(comparison_mean) ~ "Surveys incomplete",
        pre_bloom_mean == 0 ~ "Undefined - pre-bloom zero",
        percentage >= 80 ~ "Low",
        percentage >= 50 ~ "Medium",
        percentage < 50 ~ "High",
        TRUE ~ "Surveys incomplete"
      )
    )
}

# Generic pre-bloom comparison.
#
# comparison_var = "period"
#   compares Pre-bloom with the broad Bloom period.
#
# comparison_var = "period_split"
#   compares the same Pre-bloom baseline with each observed Bloom split period.
#
# by_status = FALSE
#   pools No-take and Fished transects within each location/region.
#
# by_status = TRUE
#   calculates the pre-to-bloom change separately for No-take and Fished.
calculate_percent_changes <- function(
    data,
    comparison_var = c("period", "period_split"),
    by_status = FALSE) {

  comparison_var <- match.arg(comparison_var)

  working_data <- data

  if (by_status) {
    working_data <- working_data %>%
      dplyr::filter(status %in% status_levels)
  }

  key_vars <- c(
    "spatial_level",
    "spatial_group",
    "metric_group",
    "metric"
  )

  if (by_status) {
    key_vars <- c(key_vars, "status")
  }

  summary_data <- working_data %>%
    dplyr::filter(!is.na(.data[[comparison_var]])) %>%
    summarise_metric_values(
      grouping_vars = c(key_vars, comparison_var)
    )

  pre_bloom <- summary_data %>%
    dplyr::filter(.data[[comparison_var]] == "Pre-bloom") %>%
    dplyr::select(
      dplyr::all_of(key_vars),
      pre_bloom_mean = mean_value,
      pre_bloom_se = se_value,
      n_pre_bloom = n_transects
    )

  if (comparison_var == "period") {

    # Make one broad Bloom comparison row for every group/status combination,
    # including groups where one side of the comparison is missing.
    group_keys <- working_data %>%
      dplyr::distinct(
        dplyr::across(dplyr::all_of(key_vars))
      )

    bloom <- summary_data %>%
      dplyr::filter(period == "Bloom") %>%
      dplyr::select(
        dplyr::all_of(key_vars),
        comparison_mean = mean_value,
        comparison_se = se_value,
        n_comparison = n_transects
      )

    output <- group_keys %>%
      dplyr::left_join(
        pre_bloom,
        by = key_vars
      ) %>%
      dplyr::left_join(
        bloom,
        by = key_vars
      ) %>%
      dplyr::mutate(
        comparison_period = "Bloom"
      )

  } else {

    # For period_split, only create comparisons for split Bloom periods that
    # were actually observed for that group. Each uses the same Pre-bloom
    # baseline for that group/status.
    comparison <- summary_data %>%
      dplyr::filter(
        .data[[comparison_var]] != "Pre-bloom"
      ) %>%
      dplyr::select(
        dplyr::all_of(key_vars),
        comparison_period = dplyr::all_of(comparison_var),
        comparison_mean = mean_value,
        comparison_se = se_value,
        n_comparison = n_transects
      )

    output <- comparison %>%
      dplyr::left_join(
        pre_bloom,
        by = key_vars
      )
  }

  if (!by_status) {
    output$status <- "Overall"
  }

  output %>%
    dplyr::mutate(
      comparison_type = dplyr::if_else(
        comparison_var == "period",
        "period",
        "period_split"
      ),
      comparison_scope = dplyr::if_else(
        by_status,
        "by_status",
        "overall"
      )
    ) %>%
    add_change_fields() %>%
    dplyr::select(
      spatial_level,
      spatial_group,
      metric_group,
      metric,
      comparison_type,
      comparison_period,
      comparison_scope,
      status,
      pre_bloom_mean,
      pre_bloom_se,
      n_pre_bloom,
      comparison_mean,
      comparison_se,
      n_comparison,
      absolute_change,
      percentage,
      percentage_change,
      baseline_zero,
      impact
    )
}

# -----------------------------------------------------------------
# 3. Read and combine all transect-level metrics
# -----------------------------------------------------------------

metric_samples <- purrr::pmap_dfr(
  metric_sources,
  load_metric_table
)

# Check which status values are present before restricting status comparisons
# to the two requested categories.
status_check <- metric_samples %>%
  dplyr::count(status, sort = TRUE)

print(status_check)

spatial_samples <- expand_spatial_levels(metric_samples)

# -----------------------------------------------------------------
# 4. Calculate all four comparison types
# -----------------------------------------------------------------

period_overall <- calculate_percent_changes(
  data = spatial_samples,
  comparison_var = "period",
  by_status = FALSE
)

period_status <- calculate_percent_changes(
  data = spatial_samples,
  comparison_var = "period",
  by_status = TRUE
)

period_split_overall <- calculate_percent_changes(
  data = spatial_samples,
  comparison_var = "period_split",
  by_status = FALSE
)

period_split_status <- calculate_percent_changes(
  data = spatial_samples,
  comparison_var = "period_split",
  by_status = TRUE
)

rls_metric_changes <- dplyr::bind_rows(
  period_overall,
  period_status,
  period_split_overall,
  period_split_status
) %>%
  dplyr::arrange(
    spatial_level,
    spatial_group,
    metric_group,
    metric,
    comparison_type,
    comparison_period,
    status
  )

# -----------------------------------------------------------------
# 5. Make one convenient wide table for Overall, No-take and Fished
# -----------------------------------------------------------------

rls_metric_changes_wide <- rls_metric_changes %>%
  dplyr::mutate(
    status_key = dplyr::case_when(
      status == "Overall" ~ "overall",
      status == "No-take" ~ "no_take",
      status == "Fished" ~ "fished",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(status_key)) %>%
  dplyr::select(
    spatial_level,
    spatial_group,
    metric_group,
    metric,
    comparison_type,
    comparison_period,
    status_key,
    pre_bloom_mean,
    comparison_mean,
    change = percentage_change
  ) %>%
  tidyr::pivot_wider(
    names_from = status_key,
    values_from = c(
      pre_bloom_mean,
      comparison_mean,
      change
    ),
    names_glue = "{.value}_{status_key}"
  )

# Make the expected columns even if one status is completely absent.
for (column_name in c(
  "pre_bloom_mean_overall",
  "pre_bloom_mean_no_take",
  "pre_bloom_mean_fished",
  "comparison_mean_overall",
  "comparison_mean_no_take",
  "comparison_mean_fished",
  "change_overall",
  "change_no_take",
  "change_fished"
)) {
  if (!column_name %in% names(rls_metric_changes_wide)) {
    rls_metric_changes_wide[[column_name]] <- NA_real_
  }
}

rls_metric_changes_wide <- rls_metric_changes_wide %>%
  # dplyr::mutate(
  #   # Positive values mean the No-take percentage change was more positive
  #   # (or less negative) than the Fished percentage change.
  #   difference_no_take_minus_fished = change_no_take - change_fished
  # ) %>%
  dplyr::arrange(
    spatial_level,
    spatial_group,
    metric_group,
    metric,
    comparison_type,
    comparison_period
  )
# -----------------------------------------------------------------
# 6. Save outputs
# -----------------------------------------------------------------

readr::write_rds(
  rls_metric_changes,
  file.path(
    output_dir,
    "rls_metric_percentage_changes_long.rds"
  )
)

readr::write_csv(
  rls_metric_changes,
  file.path(
    output_dir,
    "rls_metric_percentage_changes_long.csv"
  )
)

readr::write_rds(
  rls_metric_changes_wide,
  file.path(
    output_dir,
    "rls_metric_percentage_changes_wide.rds"
  )
)

readr::write_csv(
  rls_metric_changes_wide,
  file.path(
    output_dir,
    "rls_metric_percentage_changes_wide.csv"
  )
)

# Also save easy-to-inspect CSVs separately for locations and regions without
# duplicating any calculation code.
purrr::walk(
  c("location", "region"),
  function(spatial_level_value) {

    rls_metric_changes %>%
      dplyr::filter(
        spatial_level == spatial_level_value
      ) %>%
      readr::write_csv(
        file.path(
          output_dir,
          paste0(
            "rls_metric_percentage_changes_",
            spatial_level_value,
            ".csv"
          )
        )
      )

    rls_metric_changes_wide %>%
      dplyr::filter(
        spatial_level == spatial_level_value
      ) %>%
      readr::write_csv(
        file.path(
          output_dir,
          paste0(
            "rls_metric_percentage_changes_",
            spatial_level_value,
            "_wide.csv"
          )
        )
      )
  }
)

# -----------------------------------------------------------------
# 7. Checks
# -----------------------------------------------------------------

# Number of comparison rows produced for each metric family.
rls_metric_changes %>%
  dplyr::count(
    spatial_level,
    metric_group,
    comparison_type,
    comparison_scope
  ) %>%
  print(n = Inf)

# Any comparison where percentage change could not be calculated.
percentage_change_issues <- rls_metric_changes %>%
  dplyr::filter(is.na(percentage_change)) %>%
  dplyr::select(
    spatial_level,
    spatial_group,
    metric_group,
    metric,
    comparison_type,
    comparison_period,
    status,
    pre_bloom_mean,
    comparison_mean,
    baseline_zero,
    impact
  )

print(percentage_change_issues, n = Inf)

test <- rls_metric_changes_wide %>%
  dplyr::filter(comparison_type %in% "period")
