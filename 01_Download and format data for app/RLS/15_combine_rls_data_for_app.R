#################################################################
# 15. Combine formatted RLS (dive) outputs into one object for the app
#
# WHAT THIS SCRIPT DOES
#   Reads outputs that scripts 01-14 (and 02_format_rls_survey_metadata.R)
#   have ALREADY written to disk, and packages them into a single list
#   object called `rls_data`, saved as app_data/rls_data.Rdata.
#
#   This mirrors how `hab_data` is built for the BRUV side of the
#   dashboard in "03_Format BRUVS and RLS data for summaries and plots.R" -
#   global.R will load both objects and the app will read from whichever
#   one matches the "BRUVS" / "Dive" switch the user has selected.
#
# WHAT THIS SCRIPT DOES NOT DO
#   It does NOT re-run any of the raw-data download, formatting or GLMM
#   modelling scripts (01-14). If you change one of those, re-run it
#   (and any script downstream of it) BEFORE re-running this script, or
#   the app will keep showing stale numbers.
#
# WHEN TO RE-RUN
#   Any time the underlying RLS outputs change (new data, re-run models,
#   new percentage change numbers, new plot data). It's quick because it
#   just reads existing files - no modelling happens here.
#
# BEFORE RUNNING FOR THE FIRST TIME
#   Just scripts 01-14 (and 02_format_rls_survey_metadata.R) need to have
#   already been run, so their output files exist on disk (see the paths
#   in section 0 below). This script is entirely self-contained within
#   the RLS pipeline - it does NOT read app_data/hab_data.Rdata or any
#   other BRUVS-side ("03_Format BRUVS and RLS data for summaries and
#   plots.R") output. RLS uses its own region/location vocabulary
#   throughout (see rls_native_regions/rls_native_locations below), which
#   is intentionally different from BRUVS' - confirmed with Brooke.
#
# THINGS TO CHECK WHEN YOU FIRST RUN THIS (I could not run R myself while
# writing this, so please check these and tell me if anything errors or
# looks wrong so I can fix it):
#   1. That the file paths below match your project (they're copied from
#      the write_rds()/write_csv() calls in scripts 02, 04-11).
#   2. That `rls_metric_lookup` below (metric_id / metric_label) matches
#      the *actual* values in the `metric` column of
#      model_results/rls_glmm_results/period_predictions.csv - run:
#        unique(readr::read_csv("model_results/rls_glmm_results/period_predictions.csv")$metric)
#      and compare against `rls_metric_lookup$metric_label`.
#   3. That the internal region/location consistency checks in the
#      diagnostics section near the bottom come back empty (i.e. the GLMM
#      output's region/location values line up with rls_samples and
#      rls_sites) - if any of those print a non-empty set, the region/
#      location dropdowns won't be able to filter the RLS data correctly.
#################################################################

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

# ============================================================
# 0. Paths - copied from where scripts 02, 04-11 write their outputs
# ============================================================

metric_dir     <- "data/rls_metrics_for_modelling"   # scripts 04-07
glmm_dir       <- "model_results/rls_glmm_results"   # script 11
pct_change_dir <- "data/rls_metric_percentage_changes" # script 10 (NOTE: script 10 itself
# writes to "outputs/rls_metric_percentage_changes", but the files that already exist on
# disk are under "data/rls_metric_percentage_changes" - using the ones on disk. If you
# re-run script 10, double check it writes/copies to this folder, or update this path.)
stacked_dir    <- "plots/rls_stacked_relative_abundance" # script 09
top_dir        <- "plots/rls_top_occurrence_abundance"   # script 08
tidy_dir       <- "data/tidy"                          # script 02/03
lookup_dir     <- "data/lookups"

dir.create(lookup_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(
  "Can't find data/rls_metrics_for_modelling - run scripts 04-07 first" = dir.exists(metric_dir),
  "Can't find model_results/rls_glmm_results - run script 11 first"    = dir.exists(glmm_dir)
)

# ============================================================
# 1. Metric lookup table
#
# One row per plottable RLS metric. This drives the "Explore indicators"
# tabset for Dive, and is how we translate between the metric labels used
# in the GLMM output ("M1 fish species richness") and a short, Shiny-safe
# id ("m1_fish_richness") used for input/output names, mirroring how
# global.R's `metric_defs` / `metric_data_key()` work for BRUVS.
#
# PLEASE CHECK metric_label against your actual GLMM output (see note #2
# at the top of this file) - add/remove/relabel rows as needed.
# ============================================================

#   Brooke asked for the "Explore indicators" tabset to have one tab per
#   *biological* metric (e.g. "Species richness"), with the M1 fish / M2
#   fish / M2 invertebrates plots faceted side by side within that one
#   tab, rather than a separate tab per method x metric combination.
#   `metric_group` is the tab-grouping key, `metric_group_label` is the
#   tab title, and `facet_label` is what each row facets by within its
#   tab (the method name for most groups; the invertebrate phylum for
#   the M2-invertebrate-only abundance rows, since those don't vary by
#   method at all).

# The 11 non-phylum metric rows are fixed - these method x metric
# combinations don't change.
rls_metric_lookup_fixed <- tibble::tribble(
  ~metric_id,                    ~metric_label,                                ~metric_group,             ~metric_group_label,                 ~dataset,             ~facet_label,        ~y_lab,
  "m1_fish_richness",            "M1 fish species richness",                  "species_richness",        "Species richness",                  "M1 fish",            "M1 fish",           "Avg. species richness",
  "m2_fish_richness",            "M2 fish species richness",                  "species_richness",        "Species richness",                  "M2 fish",            "M2 fish",           "Avg. species richness",
  "m2_invert_richness",          "M2 invertebrate species richness",          "species_richness",        "Species richness",                  "M2 invertebrates",   "M2 invertebrates",  "Avg. species richness",
  "m1_fish_shannon",             "M1 fish Shannon diversity",                 "shannon_diversity",       "Shannon diversity index",           "M1 fish",            "M1 fish",           "Avg. Shannon diversity index",
  "m2_fish_shannon",             "M2 fish Shannon diversity",                 "shannon_diversity",       "Shannon diversity index",           "M2 fish",            "M2 fish",           "Avg. Shannon diversity index",
  "m2_invert_shannon",           "M2 invertebrate Shannon diversity",         "shannon_diversity",       "Shannon diversity index",           "M2 invertebrates",   "M2 invertebrates",  "Avg. Shannon diversity index",
  "m1_fish_b20",                 "M1 fish B20 biomass",                       "b20",                     "B20 biomass",                       "M1 fish",            "M1 fish",           "Avg. B20 biomass (kg)",
  "m2_fish_b20",                 "M2 fish B20 biomass",                       "b20",                     "B20 biomass",                       "M2 fish",            "M2 fish",           "Avg. B20 biomass (kg)",
  "m1_fish_total_abundance",     "M1 fish total abundance",                   "total_abundance",         "Total abundance",                   "M1 fish",            "M1 fish",           "Avg. total abundance",
  "m2_fish_total_abundance",     "M2 fish total abundance",                   "total_abundance",         "Total abundance",                   "M2 fish",            "M2 fish",           "Avg. total abundance",
  "m2_invert_total_abundance",   "M2 invertebrate total abundance",           "total_abundance",         "Total abundance",                   "M2 invertebrates",   "M2 invertebrates",  "Avg. total abundance"
)

#   Brooke asked (2026-09-09): "Can we please only use the three invert
#   phyla that the glmms report?" - the phylum names above were typed in
#   by hand early on and don't necessarily match what script 11 actually
#   modelled. Rather than hardcode phylum names again (which will just
#   go stale the next time the GLMMs change), derive these rows directly
#   from the `metric` column of period_predictions.csv - i.e. whatever
#   phyla the GLMMs actually report is what shows up here, automatically.
#
#   This mirrors the exact filter script 10 uses to identify per-phylum
#   invertebrate abundance metrics (see
#   01_Download and format data for app/RLS/10_calculate_rls_metric_percentage_changes.R,
#   ~line 130-142): metric matches "M2 invertebrate <phylum> abundance"
#   but is NOT "M2 invertebrate total abundance".
#   Brooke asked again (2026-09-11) for species richness and Shannon
#   diversity of the same three phyla, so scripts 04 and 05 now produce
#     "M2 invertebrate <phylum> species richness"
#     "M2 invertebrate <phylum> Shannon diversity"
#   using exactly the same naming convention as the abundance metrics.
#   All three families are therefore derived by one helper below, and each
#   becomes its own tab in "Explore indicators", faceted by phylum.
period_predictions_path <- file.path(glmm_dir, "period_predictions.csv")

# Used only when period_predictions.csv is missing or has no phylum metrics.
default_invert_phyla <- c("Echinodermata", "Arthropoda", "Mollusca")

# Build the rls_metric_lookup rows for ONE family of per-phylum metrics.
#
# `metric_suffix` is the text a metric name ends with, e.g. "abundance" for
# "M2 invertebrate Mollusca abundance". `whole_dataset_labels` are the
# whole-dataset metrics that share that suffix and must NOT be mistaken for
# a phylum (e.g. "M2 invertebrate total abundance").
#
# `metric_id_suffix` is appended to the Shiny-safe id. The abundance rows
# deliberately pass "" so their ids stay exactly as they were
# ("m2_invert_mollusca"), since those ids are already in use.
make_phylum_lookup_rows <- function(
    glmm_metrics,
    metric_suffix,
    whole_dataset_labels,
    metric_id_suffix,
    metric_group,
    metric_group_label,
    y_lab,
    fallback_phyla = default_invert_phyla) {

  # metric_suffix is plain text with no regular-expression metacharacters.
  pattern <- paste0("^M2 invertebrate (.+) ", metric_suffix, "$")

  metric_labels <- glmm_metrics[
    stringr::str_detect(glmm_metrics, pattern) &
      !(glmm_metrics %in% whole_dataset_labels)
  ]

  if (length(metric_labels) == 0) {
    warning(
      "No 'M2 invertebrate <phylum> ", metric_suffix, "' metrics found in ",
      period_predictions_path,
      " - falling back to hardcoded ",
      paste(fallback_phyla, collapse = "/"),
      " rows. Check that scripts 04, 05, 07 and 11 have all been re-run and ",
      "that the metric-naming convention hasn't changed."
    )
    phylum_names <- fallback_phyla
    metric_labels <- paste0(
      "M2 invertebrate ", phylum_names, " ", metric_suffix
    )
  } else {
    # Pull the phylum name out of "M2 invertebrate <phylum> <suffix>"
    phylum_names <- stringr::str_match(metric_labels, pattern)[, 2]
  }

  tibble::tibble(
    metric_id          = paste0(
      "m2_invert_", tolower(phylum_names), metric_id_suffix
    ),
    metric_label       = metric_labels,
    metric_group       = metric_group,
    metric_group_label = metric_group_label,
    dataset            = "M2 invertebrates",
    facet_label        = phylum_names,
    y_lab              = y_lab
  )
}

if (file.exists(period_predictions_path)) {
  glmm_metrics <- unique(readr::read_csv(period_predictions_path, show_col_types = FALSE)$metric)
} else {
  warning(
    "Can't find ", period_predictions_path, " - falling back to hardcoded ",
    "Echinodermata/Arthropoda/Mollusca invert-phylum rows. Run script 11 first, then ",
    "re-run this script, to pick up the phyla the GLMMs actually report."
  )
  glmm_metrics <- character()
}

rls_metric_lookup_phyla <- dplyr::bind_rows(

  make_phylum_lookup_rows(
    glmm_metrics         = glmm_metrics,
    metric_suffix        = "abundance",
    whole_dataset_labels = "M2 invertebrate total abundance",
    metric_id_suffix     = "",
    metric_group         = "invert_phylum_abundance",
    metric_group_label   = "Invertebrate abundance by phylum",
    y_lab                = "Avg. abundance"
  ),

  make_phylum_lookup_rows(
    glmm_metrics         = glmm_metrics,
    metric_suffix        = "species richness",
    whole_dataset_labels = "M2 invertebrate species richness",
    metric_id_suffix     = "_richness",
    metric_group         = "invert_phylum_richness",
    metric_group_label   = "Invertebrate species richness by phylum",
    y_lab                = "Avg. species richness"
  ),

  make_phylum_lookup_rows(
    glmm_metrics         = glmm_metrics,
    metric_suffix        = "Shannon diversity",
    whole_dataset_labels = "M2 invertebrate Shannon diversity",
    metric_id_suffix     = "_shannon",
    metric_group         = "invert_phylum_shannon",
    metric_group_label   = "Invertebrate Shannon diversity by phylum",
    y_lab                = "Avg. Shannon diversity index"
  )
)

rls_metric_lookup <- dplyr::bind_rows(rls_metric_lookup_fixed, rls_metric_lookup_phyla)

# One row per tab (7 biological metrics), in the order the tabs should
# appear - used to build the "Explore indicators" tabset for Dive.
# metric_group is kept as a plain character column (not a factor) so it
# can be used directly as a Shiny input/output id downstream.
#
# The two "by phylum" richness/diversity groups sit immediately after their
# whole-dataset equivalents so the tabs read
# richness -> richness by phylum -> Shannon -> Shannon by phylum.
rls_metric_group_order <- c(
  "species_richness", "invert_phylum_richness",
  "shannon_diversity", "invert_phylum_shannon",
  "b20",
  "total_abundance", "invert_phylum_abundance"
)

# Guard against a metric_group appearing in the lookup but not in the order
# above - match() would return NA, the tab would sort last, and the gauge
# grid's .group_order would silently break.
unordered_metric_groups <- setdiff(
  unique(rls_metric_lookup$metric_group),
  rls_metric_group_order
)

if (length(unordered_metric_groups) > 0) {
  stop(
    "These metric_group values are missing from rls_metric_group_order: ",
    paste(unordered_metric_groups, collapse = ", ")
  )
}

rls_metric_groups <- rls_metric_lookup %>%
  dplyr::distinct(metric_group, metric_group_label, y_lab) %>%
  dplyr::mutate(metric_group = factor(metric_group, levels = rls_metric_group_order)) %>%
  dplyr::arrange(metric_group) %>%
  dplyr::mutate(metric_group = as.character(metric_group))

# ============================================================
# 2. Raw (sample/transect-level) metric data - for the "show boxplots
#    instead of bars" toggle and for "download raw data" buttons.
#
# Each file already contains rows for M1 fish / M2 fish / M2 invertebrates
# (metric column), one row per transect, per scripts 04-07.
# ============================================================

read_metric_rds <- function(path, value_col, out_col = "value") {
  if (!file.exists(path)) {
    warning("Missing expected file: ", path)
    return(tibble::tibble())
  }
  df <- readr::read_rds(path)
  if (!value_col %in% names(df)) {
    warning(
      "Expected column '", value_col, "' not found in ", path,
      " - found: ", paste(names(df), collapse = ", ")
    )
    return(df)
  }
  df %>% dplyr::rename(!!out_col := dplyr::all_of(value_col))
}

species_richness_samples <- read_metric_rds(file.path(metric_dir, "species_richness.rds"), "species_richness")
shannon_diversity_samples <- read_metric_rds(file.path(metric_dir, "shannon_diversity.rds"), "shannon")
b20_samples <- read_metric_rds(file.path(metric_dir, "b20.rds"), "b20_kg")
total_abundance_samples <- read_metric_rds(file.path(metric_dir, "total_abundance.rds"), "abundance")
invert_phylum_abundance_samples <- read_metric_rds(file.path(metric_dir, "abundance.rds"), "abundance")

# One long table, one row per transect x metric, with a metric_id column
# added so it can be filtered the same way as the GLMM outputs below.
rls_samples <- dplyr::bind_rows(
  species_richness_samples,
  shannon_diversity_samples,
  b20_samples,
  total_abundance_samples,
  invert_phylum_abundance_samples
) %>%
  dplyr::left_join(
    rls_metric_lookup %>% dplyr::select(metric_id, metric_label, metric_group, facet_label),
    by = c("metric" = "metric_label")
  )

if (any(is.na(rls_samples$metric_id))) {
  missing_metrics <- rls_samples %>%
    dplyr::filter(is.na(metric_id)) %>%
    dplyr::distinct(metric) %>%
    dplyr::pull(metric)
  warning(
    "Some metric labels in the raw sample data don't match rls_metric_lookup - ",
    "these rows will have a blank metric_id and won't show up anywhere in the app: ",
    paste(missing_metrics, collapse = ", ")
  )
}

# ============================================================
# 3. GLMM modelled means - period / period x status / temporal
#
# These come straight from script 11's saved predictions, already
# location- and region-level. Column names are standardised so server.R
# can treat them the same way as the BRUV `hab_data$<metric>_summary`
# tables (mean/se instead of estimate/SE), and metric_id is added.
# ============================================================

read_predictions <- function(path) {
  if (!file.exists(path)) {
    warning("Missing expected file: ", path)
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

standardise_predictions <- function(df) {
  if (nrow(df) == 0) return(df)

  df <- df %>%
    dplyr::rename(
      period = Period,
      mean   = estimate,
      se     = SE,
      lower  = `lower.CL`,
      upper  = `upper.CL`
    ) %>%
    dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom"))) %>%
    dplyr::left_join(
      rls_metric_lookup %>% dplyr::select(metric_id, metric_label, metric_group, facet_label),
      by = c("metric" = "metric_label")
    )

  if ("status" %in% names(df)) {
    df <- df %>% dplyr::mutate(status = factor(status, levels = c("Fished", "No-take")))
  }

  df
}

rls_period_predictions <- standardise_predictions(
  read_predictions(file.path(glmm_dir, "period_predictions.csv"))
)

rls_period_status_predictions <- standardise_predictions(
  read_predictions(file.path(glmm_dir, "period_status_predictions.csv"))
)

rls_temporal_predictions <- read_predictions(file.path(glmm_dir, "temporal_predictions.csv"))
if (nrow(rls_temporal_predictions) > 0) {
  rls_temporal_predictions <- rls_temporal_predictions %>%
    # NOTE: the CSV's "Period" column was previously left un-renamed here,
    # unlike standardise_predictions() above which renames it to lowercase
    # "period". server.R's temporal ("year") plots filter/factor on
    # lowercase "period" - without this rename, that code silently falls
    # back to the *function* lubridate::period() instead of erroring
    # loudly, which is a confusing failure. Fixed to match
    # standardise_predictions()'s naming exactly.
    dplyr::rename(
      period = Period,
      mean   = estimate,
      se     = SE,
      lower  = `lower.CL`,
      upper  = `upper.CL`
    ) %>%
    dplyr::mutate(
      sampling_event_start_date = as.Date(sampling_event_start_date),
      period = factor(period, levels = c("Pre-bloom", "Bloom"))
    ) %>%
    dplyr::left_join(
      rls_metric_lookup %>% dplyr::select(metric_id, metric_label, metric_group, facet_label),
      by = c("metric" = "metric_label")
    )
}

# ============================================================
# 4. Percentage change tables (already computed by script 10 - observed
#    pre-bloom vs bloom means, NOT the GLMM output above). This is the
#    "% change compared to pre-bloom levels" table + impact classification
#    (Low/Medium/High), matching hab_data$hab_metric_change for BRUVS.
# ============================================================

read_pct_change <- function(fname) {
  path <- file.path(pct_change_dir, fname)
  if (!file.exists(path)) {
    warning("Missing expected file: ", path)
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

rls_pct_change_long          <- read_pct_change("rls_metric_percentage_changes_long.csv")
rls_pct_change_wide          <- read_pct_change("rls_metric_percentage_changes_wide.csv")
rls_pct_change_region        <- read_pct_change("rls_metric_percentage_changes_region.csv")
rls_pct_change_region_wide   <- read_pct_change("rls_metric_percentage_changes_region_wide.csv")
rls_pct_change_location      <- read_pct_change("rls_metric_percentage_changes_location.csv")
rls_pct_change_location_wide <- read_pct_change("rls_metric_percentage_changes_location_wide.csv")

# Attach metric_id/metric_group/metric_group_label/facet_label to every one
# of the six pct-change tables (not just the "long" one) so the app's %
# change tables and impact gauges can group/order rows the same way as the
# "Explore indicators" GLMM tabset (metric_group_label as the row/section
# label, facet_label - method or invert phylum - as the sub-label), and so
# the gauges can look up a metric's `impact` category by metric_id.
# Also orders rows by metric_group (in rls_metric_group_order: richness,
# richness by phylum, Shannon, Shannon by phylum, b20, total abundance,
# abundance by phylum) then facet_label, matching rls_metric_groups' tab
# order.
#
# Brooke asked (2026-09-09): "Can we remove the extra phyla's from the
# percent change table" - script 10 computes a percentage change for every
# invertebrate phylum it finds in the raw data, which is a superset of the
# phyla the GLMMs actually modelled (rls_metric_lookup's invert-phylum rows
# - now derived from period_predictions.csv, see the fix above). With a
# left_join, those extra phyla stayed in the pct-change tables with blank/NA
# metric_id/metric_group_label/facet_label instead of being dropped. Switched
# to inner_join so the pct-change tables (and therefore the "% change"
# tables and impact gauges built from them) only ever show metrics that are
# in rls_metric_lookup - i.e. exactly the same set of metrics as the
# "Explore indicators" GLMM tabset, no more.
attach_metric_lookup <- function(df) {
  if (nrow(df) == 0) return(df)

  # NOTE: script 10's own output already has its own `metric_group` column
  # (same vocabulary: species_richness/invert_phylum_richness/
  # shannon_diversity/invert_phylum_shannon/b20/total_abundance/
  # invert_phylum_abundance), so it's deliberately left out
  # of the join below - joining it again would collide with the existing
  # column and dplyr would silently rename both to metric_group.x/
  # metric_group.y instead of erroring, leaving no plain `metric_group`
  # column for the mutate()/arrange() below to use (this is exactly what
  # happened the first time - `object 'metric_group' not found`). We just
  # reuse df's own metric_group column directly instead.
  dropped_metrics <- setdiff(unique(df$metric), rls_metric_lookup$metric_label)
  if (length(dropped_metrics) > 0) {
    message(
      "attach_metric_lookup(): dropping ", length(dropped_metrics),
      " metric(s) not in rls_metric_lookup (e.g. invertebrate phyla the GLMMs ",
      "didn't model): ", paste(dropped_metrics, collapse = ", ")
    )
  }

  df %>%
    dplyr::inner_join(
      rls_metric_lookup %>%
        dplyr::select(metric_id, metric_label, metric_group_label, facet_label),
      by = c("metric" = "metric_label")
    ) %>%
    dplyr::mutate(metric_group = factor(metric_group, levels = rls_metric_group_order)) %>%
    dplyr::arrange(metric_group, facet_label) %>%
    dplyr::mutate(metric_group = as.character(metric_group))
}

rls_pct_change_long          <- attach_metric_lookup(rls_pct_change_long)
rls_pct_change_wide          <- attach_metric_lookup(rls_pct_change_wide)
rls_pct_change_region        <- attach_metric_lookup(rls_pct_change_region)
rls_pct_change_region_wide   <- attach_metric_lookup(rls_pct_change_region_wide)
rls_pct_change_location      <- attach_metric_lookup(rls_pct_change_location)
rls_pct_change_location_wide <- attach_metric_lookup(rls_pct_change_location_wide)

# ============================================================
# 5. Stacked (relative) abundance plot data - location level only.
#
# NOTE: script 09 currently only produces LOCATION-level stacked
# abundance data (`spatial_levels_to_plot <- c("location")`, region is
# commented out). BRUVS' "Stacked abundance plot" card lives on the
# Region Summary tab. We will need to decide together in the next phase
# whether to (a) add region back into script 09's spatial_levels_to_plot
# and re-run it, or (b) only show this plot on the Dive Location Summary
# tab for now. Reading whatever exists here either way.
# ============================================================

read_stacked <- function(fname) {
  path <- file.path(stacked_dir, fname)
  if (!file.exists(path)) {
    warning("Missing expected file: ", path)
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE)
}

rls_stacked_period       <- read_stacked("period_stacked_plot_data.csv")
rls_stacked_period_split <- read_stacked("period_split_stacked_plot_data.csv")

# ============================================================
# 6. Top occurrence / abundance plot data (pre-bloom & bloom focus) -
#    matches BRUVS' "Common species" pre/post plots.
#
# Script 08 saves these per dataset_id x spatial_level, in
# plots/rls_top_occurrence_abundance/<dataset_id>/<spatial_level>/summaries/.
# We combine every one of those csvs into two long tables here.
# ============================================================

read_all_matching <- function(root, pattern) {
  files <- list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    warning("No files matching '", pattern, "' found under ", root)
    return(tibble::tibble())
  }
  purrr::map_dfr(files, readr::read_csv, show_col_types = FALSE)
}

rls_top_occurrence_abundance_summary <- read_all_matching(top_dir, "^summary_by_separate_period\\.csv$")
rls_top_occurrence_abundance_selection <- read_all_matching(top_dir, "^summary_used_for_focus_period_filters\\.csv$")

# ============================================================
# 7. RLS site locations for maps
#
# `sa_sites` (data/tidy/sa_sites.rds) has region/location/status per
# site_code but NO coordinates (it's built from a shapefile then geometry
# is dropped in script 02).
#
# Coordinates are taken primarily from the survey list files
# (data/tidy/rls_m1_survey_list.rds / rls_m2_survey_list.rds) - these are
# the SAME files every metric in this script is ultimately built from, so
# they're guaranteed to cover every site_code that actually has data.
# rls_dive_sites.csv (project root) is used only to fill in coordinates
# for any site_code the survey lists don't cover.
#
# This uses the RLS pipeline's OWN location/region names (matches
# rls_samples$location / $region) - NOT hab_combined_metadata's
# reporting_name. See the diagnostics below for why that distinction matters.
# ============================================================

sa_sites_path        <- file.path(tidy_dir, "sa_sites.rds")
m1_survey_list_path  <- file.path(tidy_dir, "rls_m1_survey_list.rds")
m2_survey_list_path  <- file.path(tidy_dir, "rls_m2_survey_list.rds")
dive_sites_path      <- "rls_dive_sites.csv"

if (!file.exists(sa_sites_path)) {
  warning(
    "Missing ", sa_sites_path,
    " - rls_data$sites will be empty and RLS maps won't have any points."
  )
  rls_sites <- tibble::tibble()
} else {
  sa_sites_tbl <- readr::read_rds(sa_sites_path)

  # Coordinates from the survey lists - one row per site_code, first
  # non-missing lat/lon kept.
  survey_coords <- purrr::map_dfr(
    c(m1_survey_list_path, m2_survey_list_path),
    function(p) {
      if (!file.exists(p)) return(tibble::tibble())
      readr::read_rds(p) %>%
        dplyr::select(dplyr::any_of(c("site_code", "site_name", "latitude", "longitude")))
    }
  ) %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
    dplyr::distinct(site_code, .keep_all = TRUE)

  # Fill in any site_code the survey lists didn't cover from
  # rls_dive_sites.csv.
  if (file.exists(dive_sites_path)) {
    dive_sites_tbl <- readr::read_csv(dive_sites_path, show_col_types = FALSE)

    survey_coords <- dplyr::bind_rows(
      survey_coords,
      dive_sites_tbl %>% dplyr::anti_join(survey_coords, by = "site_code")
    )
  }

  rls_sites <- sa_sites_tbl %>%
    dplyr::left_join(survey_coords, by = "site_code") %>%
    dplyr::mutate(site_name = dplyr::coalesce(site_name, site_name_lookup)) %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
    dplyr::mutate(
      popup = paste0(
        "<b>", site_name, "</b><br>",
        location, " (", region, ")<br>",
        "Status: ", status
      )
    )

  n_sites_no_coords <- sa_sites_tbl %>%
    dplyr::anti_join(survey_coords, by = "site_code") %>%
    nrow()

  if (n_sites_no_coords > 0) {
    cat(
      "\nNote:", n_sites_no_coords, "site_code(s) in sa_sites.rds have no",
      "matching coordinates in the survey lists or rls_dive_sites.csv, and",
      "were dropped from rls_data$sites (they won't show up on the RLS maps).\n"
    )
  }
}

# ============================================================
# 7b. Multivariate community composition (script 13's dashboard export)
#
# 13_rls_multivariate_pco_and)_caps.R writes its PCoA/CAP scores,
# species vectors, site centroids and PERMANOVA results as tidy CSVs
# into outputs/multivariate/app/, already combined across all three
# datasets (M1 fish / M2 fish / M2 invertebrates). The app rebuilds
# the ordinations natively from these rather than displaying script
# 13's PNGs, so it can theme them like the rest of the dashboard and
# offer CSV downloads of the underlying scores.
#
# All of this is optional: if script 13 hasn't been run (or has been
# run without the export layer), these come back as empty tibbles and
# the app simply doesn't show the "Community composition" section.
# That is why each read is guarded rather than a stopifnot() - an
# out-of-date multivariate export should never stop the rest of the
# dashboard from building.
#
# Every location x method x ordination has a row in
# `multivariate_meta`, INCLUDING ones that could not be produced
# (available = FALSE, with a `reason`), so the app can explain an
# empty panel rather than silently showing nothing. As at the first
# export: all 30 PCoA and all 30 Period CAPs are available, and 15 of
# 30 Status CAPs - the five locations with only one management status
# present (Eastern Spencer Gulf, Metro, Southern Fleurieu, Southern
# Yorke, Upper GSV) have no Status CAP for any method.
# ============================================================

multivariate_dir <- "outputs/multivariate/app"   # script 13's export layer

read_multivariate_csv <- function(file_name) {

  path <- file.path(multivariate_dir, file_name)

  if (!file.exists(path)) {
    warning(
      "Missing ", path,
      " - the app's 'Community composition' section will be empty. ",
      "Re-run 13_rls_multivariate_pco_and)_caps.R to create it."
    )
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

rls_multivariate_scores        <- read_multivariate_csv("multivariate_ordination_scores.csv")
rls_multivariate_vectors       <- read_multivariate_csv("multivariate_species_vectors.csv")
rls_multivariate_centroids     <- read_multivariate_csv("multivariate_site_centroids.csv")
rls_multivariate_meta          <- read_multivariate_csv("multivariate_ordination_meta.csv")
rls_multivariate_period_status <- read_multivariate_csv("multivariate_period_status_scores.csv")
rls_multivariate_permanova_period <- read_multivariate_csv("multivariate_permanova_period.csv")
rls_multivariate_permanova_status <- read_multivariate_csv("multivariate_permanova_status.csv")

# scale_species_vectors() in script 13 builds its short italic arrow
# label with the regex "[A-Z][a-z]+\\s+[a-z]+$", which only matches a
# clean "Genus species" ending. Taxa like "Haliotidae Haliotis rubra
# complex" or "Temnopleuridae Holopneustes sp (red)" don't match and
# come through with an empty label (they are blank on script 13's own
# PNGs too - this is not introduced here). Fall back to the full
# scientific name so the app never draws an unlabelled arrow.
if (nrow(rls_multivariate_vectors) > 0 &&
    all(c("label", "scientific") %in% names(rls_multivariate_vectors))) {

  n_missing_label <- sum(
    is.na(rls_multivariate_vectors$label) | rls_multivariate_vectors$label == ""
  )

  if (n_missing_label > 0) {
    message(
      n_missing_label,
      " species vector(s) had no short label (taxon name isn't a plain ",
      "'Genus species') - using the full scientific name instead."
    )
  }

  rls_multivariate_vectors <- rls_multivariate_vectors %>%
    dplyr::mutate(
      label = dplyr::if_else(
        is.na(label) | label == "",
        scientific,
        label
      )
    )
}


# ============================================================
# 8. Diagnostics - internal RLS consistency checks (region/location
#    names used in the GLMM output vs. the RLS pipeline's own raw sample
#    data and site table). This script is self-contained within the RLS
#    pipeline - it does NOT compare against BRUVS' vocabulary
#    (hab_data$hab_combined_metadata) at all, since RLS uses its own
#    region/location names throughout (rls_native_regions/
#    rls_native_locations below), which are a genuinely different
#    vocabulary from BRUVS' - confirmed with Brooke. If any of the
#    print()s below come back non-empty, the region/location dropdowns
#    won't be able to filter the RLS data correctly - please check and
#    let me know.
#
#    NOTE: per script 11's own header comment, GLMMs are fitted per
#    LOCATION only - there is no region-level modelled-means row. Every
#    row's `spatial_level` should read "location", with `region` present
#    only as a lookup column showing which region that location sits in.
#    That means the Dive "Region Summary" tab won't have a modelled-means
#    equivalent unless we decide how to combine its locations (this is a
#    Phase 2 decision, not something this script tries to solve).
# ============================================================

# The vocabulary the RLS pipeline itself uses (from the raw sample data /
# GLMM output / rls_sites above) - this is what the RLS side of the app
# (dropdowns, summary text lookup, maps, etc.) uses throughout.
rls_native_regions   <- sort(unique(rls_samples$region))
rls_native_locations <- sort(unique(rls_samples$location))

cat("\n--- RLS GLMM output: spatial_level values found (expect only 'location') ---\n")
print(table(rls_period_predictions$spatial_level, useNA = "ifany"))

glmm_regions   <- sort(unique(rls_period_predictions$region))
glmm_locations <- sort(unique(rls_period_predictions$location))

cat("\n--- RLS region/location name check (internal consistency) ---\n")
cat("Regions in GLMM output but not in rls_samples$region (should be empty):\n")
print(setdiff(glmm_regions, rls_native_regions))
cat("Locations in GLMM output but not in rls_samples$location (should be empty):\n")
print(setdiff(glmm_locations, rls_native_locations))
cat("Locations in GLMM output but not in rls_sites (the map site table - should be empty):\n")
print(setdiff(glmm_locations, sort(unique(rls_sites$location))))
cat("---------------------------------------\n\n")

# Script 13 takes `location` from sa_sites.rds, while everything else in
# this script takes it from data/rls_metrics_for_modelling/. Those two
# agreed exactly at the time of writing (10 locations, no differences
# either way), but if they ever drift the multivariate section would
# silently show nothing for the affected locations - the app looks its
# ordinations up by location name. So check it explicitly rather than
# trusting it to stay true.
if (nrow(rls_multivariate_meta) > 0 && "location" %in% names(rls_multivariate_meta)) {

  multivariate_locations <- sort(unique(rls_multivariate_meta$location))

  cat("--- Multivariate (script 13) location name check ---\n")
  cat("Locations in the multivariate export but not in rls_samples (should be empty):\n")
  print(setdiff(multivariate_locations, rls_native_locations))
  cat("Locations in rls_samples but with no multivariate export (empty = every location has one):\n")
  print(setdiff(rls_native_locations, multivariate_locations))
  cat("---------------------------------------\n\n")

  cat("--- Multivariate ordinations available ---\n")
  print(
    rls_multivariate_meta %>%
      dplyr::count(ordination, available, name = "n")
  )

  unavailable_multivariate <- rls_multivariate_meta %>%
    dplyr::filter(!available)

  if (nrow(unavailable_multivariate) > 0) {
    cat(
      "\n", nrow(unavailable_multivariate),
      " ordination(s) unavailable - the app will show the recorded reason ",
      "instead of an empty panel. Locations affected:\n",
      sep = ""
    )
    print(sort(unique(unavailable_multivariate$location)))
  }
  cat("---------------------------------------\n\n")
}

# ============================================================
# 9. Region & location narrative summary text - dummy/template lookups
#
# These are separate, hand-edited csv files (NOT baked into rls_data.Rdata)
# so that a collaborator can open them, add text, save, and have the app
# pick up the change without anyone re-running this script. They live
# alongside the existing BRUV summary text lookups in data/lookups/.
#
# Running this section is "safe" to repeat: it adds a blank placeholder
# row for any NEW region/location it finds, but never overwrites text
# that's already been filled in.
# ============================================================

upsert_summary_lookup <- function(path, id_col, id_values, placeholder_prefix) {

  id_values <- sort(unique(id_values[!is.na(id_values) & id_values != ""]))

  if (file.exists(path)) {
    existing <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    existing <- tibble::tibble(!!id_col := character(), summary = character())
  }

  new_ids <- setdiff(id_values, existing[[id_col]])

  if (length(new_ids) > 0) {
    new_rows <- tibble::tibble(
      !!id_col := new_ids,
      summary   = paste0(placeholder_prefix, new_ids)
    )
    existing <- dplyr::bind_rows(existing, new_rows)
  }

  existing <- existing %>%
    dplyr::filter(.data[[id_col]] %in% id_values) %>%
    dplyr::arrange(.data[[id_col]])

  readr::write_csv(existing, path)
  existing
}

rls_region_summary_lookup <- upsert_summary_lookup(
  path               = file.path(lookup_dir, "SA-HAB-Summary Text - rls_region_summary_text.csv"),
  id_col             = "region",
  id_values          = rls_native_regions,
  placeholder_prefix = "Add Dive/RLS summary text for "
)

# NOTE: uses `rls_native_locations` (the RLS pipeline's own location names,
# e.g. "Carrickalinga") - deliberately not BRUVS' location vocabulary,
# which is a genuinely different list of names, confirmed with Brooke.
rls_location_summary_lookup <- upsert_summary_lookup(
  path               = file.path(lookup_dir, "SA-HAB-Summary Text - rls_location_summary_text.csv"),
  id_col             = "reporting_location",
  id_values          = rls_native_locations,
  placeholder_prefix = "Add Dive/RLS summary text for "
)

# ============================================================
# 10. Assemble and save
# ============================================================

rls_data <- list(
  metric_lookup                          = rls_metric_lookup,
  metric_groups                          = rls_metric_groups,

  samples                                = rls_samples,

  period_predictions                     = rls_period_predictions,
  period_status_predictions              = rls_period_status_predictions,
  temporal_predictions                   = rls_temporal_predictions,

  pct_change_long                        = rls_pct_change_long,
  pct_change_wide                        = rls_pct_change_wide,
  pct_change_region                      = rls_pct_change_region,
  pct_change_region_wide                 = rls_pct_change_region_wide,
  pct_change_location                    = rls_pct_change_location,
  pct_change_location_wide               = rls_pct_change_location_wide,

  stacked_period                         = rls_stacked_period,
  stacked_period_split                   = rls_stacked_period_split,

  top_occurrence_abundance_summary       = rls_top_occurrence_abundance_summary,
  top_occurrence_abundance_selection     = rls_top_occurrence_abundance_selection,

  sites                                  = rls_sites,

  # Multivariate community composition (script 13's export layer).
  # `multivariate_meta` is the one to consult first in the app: it has a
  # row for every location x method x ordination, including the ones that
  # couldn't be produced (available = FALSE plus a `reason`), and carries
  # the pre-formatted axis labels so the app's axes match script 13's own
  # figures exactly.
  multivariate_scores                    = rls_multivariate_scores,
  multivariate_vectors                   = rls_multivariate_vectors,
  multivariate_centroids                 = rls_multivariate_centroids,
  multivariate_meta                      = rls_multivariate_meta,
  multivariate_period_status             = rls_multivariate_period_status,
  multivariate_permanova_period          = rls_multivariate_permanova_period,
  multivariate_permanova_status          = rls_multivariate_permanova_status,

  region_summary_lookup_path             = file.path(lookup_dir, "SA-HAB-Summary Text - rls_region_summary_text.csv"),
  location_summary_lookup_path           = file.path(lookup_dir, "SA-HAB-Summary Text - rls_location_summary_text.csv")
)

dir.create("app_data", recursive = TRUE, showWarnings = FALSE)
save(rls_data, file = "app_data/rls_data.Rdata")

cat("\nSaved app_data/rls_data.Rdata with", length(rls_data), "elements.\n")
cat("Row counts:\n")
purrr::iwalk(rls_data, function(x, name) {
  if (is.data.frame(x)) cat(" -", name, ":", nrow(x), "rows\n")
})
