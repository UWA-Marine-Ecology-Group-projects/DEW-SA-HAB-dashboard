# ============================================================
# 05. All-locations PCO and Bloom-effect CAP
#     (one point per location x status x sampling date)
#
# For each dataset - BRUVs, RLS M1 fish, RLS M2 fish and RLS M2
# invertebrates - this makes two figures with every location on
# one plot:
#
#   1. PCO (unconstrained) of community composition
#   2. Bloom-effect CAP (constrained on Period: Pre-bloom vs Bloom)
#   3. Bloom CAP1 through time
#   4. Bloom CAP1 vs Status CAP1 (locations with both statuses only;
#      the Status CAP is a visualisation, no p-value - see section 4b)
#
# Each point is one location x management status x sampling date.
# Abundance is averaged so that every transect (RLS) or deployment
# (BRUV) carries equal weight:
#
#   RLS blocks -> transect (mean; BRUV deployments are already one unit)
#     -> location x status x date (one pooled mean over all transects /
#        deployments, NOT a mean of site means)
#
# Note this differs from the per-location analyses (RLS/13,
# modelling/03), which average to site x sampling event first. Where
# sites had unequal numbers of replicates on a trip, sites with more
# replicates pull the point towards them here.
#
# Same data and same distance as the per-location analyses
# (RLS/13 and modelling/03): the inputs are zero-filled, abundance
# is log2(x) + 1 transformed (vegan::decostand "log"), and
# composition is compared with modified Gower (altGower). The
# data loaders and the distance function are taken straight from
# those scripts, so nothing here can drift from them.
#
# THE CAP AND LOCATION
#   With every location in one model, differences between
#   locations dwarf the bloom effect, and locations were not all
#   sampled equally in both periods. So by default the CAP is
#   fitted as  distance ~ period + Condition(location) : location
#   differences are removed first, and CAP1 is the shift in
#   composition from Pre-bloom to Bloom that is shared across
#   locations. The permutation test likewise only swaps Period
#   labels within a location. Set CAP_CONDITION_ON_LOCATION to
#   FALSE to fit plain  distance ~ period  instead.
#
#   The PCO is unconstrained, so it will mostly show how the
#   locations differ from one another - that is expected.
#
# A CAVEAT ON THE p-VALUE
#   Points from the same location x status are repeated visits to
#   the same places, so they are not independent. Restricting
#   permutations to within location handles the location part of
#   that, but treat the p-value as a guide, not as a substitute
#   for the per-location PERMANOVAs in script 13 / modelling/03.
#
# RUN ORDER (from the project root)
#   1. 03_Format BRUVS and RLS data for summaries and plots.R
#      (writes sa_bruv_metadata.csv)
#   2. The RLS pipeline up to RLS/03 (writes the
#      data/tidy/rls_*_complete_count.rds files)
#   3. This script
#
# OUTPUTS: outputs/multivariate/all_locations/
#   <dataset>_all_locations_PCO.png / _CAP_Period.png / _CAP_Period_time.png
#     / _CAP_Bloom_vs_Status.png
#   all_methods_all_locations_<PCO|CAP_Period|CAP_Period_time|CAP_Bloom_vs_Status>.png
#   all_methods_all_locations_CAP_Period_time_impact_score.png    (coloured by
#   all_methods_all_locations_CAP_Period_time_impact_category.png  location impact,
#                                        see section 4c)
#   all_methods_all_locations_CAP_Period_time_simpson_score.png     (coloured by
#   all_methods_all_locations_CAP_Period_time_simpson_category.png   inverse Simpson
#                                        change only)
#   cap_time_by_indicator/all_methods/ the four methods together: overall +
#                                       richness, abundance, Simpson, Shannon
#   cap_time_by_indicator/<method>/   one Bloom CAP-through-time figure per
#                                       indicator (plus 00_overall_impact),
#                                       markers filled by Low/Medium/High
#   location_indicator_impacts.csv      (impact category per indicator)
#   location_impact_scores.csv / location_simpson_scores.csv
#                                       (the scores behind those colours)
#                                       (the four methods in one figure)
#   ...and the same set again in all_locations/complete_sites_only/,
#   using only dates where every site of a location x status was sampled
#   <dataset>_all_locations_scores.csv  (PCO + CAP scores per point)
#   <dataset>_all_locations_site_check.csv  (sites behind every point,
#                                        and which were missing)
#   all_locations_design_summary.csv    (points and sites per location)
#   all_locations_cap_tests.csv         (CAP permutation test results)
# ============================================================

library(tidyverse)
library(vegan)
library(patchwork)


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

OUTPUT_DIR <- file.path("outputs", "multivariate", "all_locations")

CAP_CONDITION_ON_LOCATION <- TRUE

N_SPECIES_VECTORS <- 10      # arrows drawn on each plot; 0 for none

# Which sites each point is built from. A point is the mean across all
# transects / deployments of one location x status on that date. If a
# date is missing some of that location x status's sites, the point moves
# partly because different places were averaged, not because the
# community changed. So the whole analysis is run twice (sections 7 and 8
# at the bottom):
#
#   "all"            - every site-event as recorded, written to OUTPUT_DIR.
#                      The site check CSVs flag the incomplete points.
#   "complete_dates" - for each location x status, only the dates at which
#                      EVERY site of that location x status was sampled, so
#                      every point in a trajectory comes from the same
#                      sites. Written to OUTPUT_DIR/complete_sites_only.
COMPLETE_SITES_DIR <- file.path(OUTPUT_DIR, "complete_sites_only")

RLS_SITES_RDS <- "data/tidy/sa_sites.rds"

RLS_DATASETS <- tribble(
  ~prefix,       ~label,             ~count_rds,
  "M1",          "M1 fish",          "data/tidy/rls_m1_complete_count.rds",
  "M2_cryptic",  "M2 fish",          "data/tidy/rls_m2_fish_complete_count.rds",
  "M2_inverts",  "M2 invertebrates", "data/tidy/rls_m2_inverts_complete_count.rds"
)


# ------------------------------------------------------------
# Shared functions from the existing multivariate scripts
#
# Sourcing modelling/03 with BRUV_ENGINE_ONLY defines
# build_bruv_raw_data() without running the BRUV pipeline, and it
# in turn sources RLS/13 as a function library (safe_altgower(),
# fit_species_envfit(), scale_species_vectors(),
# status_period_layers(), get_site_colours(), ...).
# ------------------------------------------------------------

BRUV_ENGINE_ONLY <- TRUE

source(
  file.path(
    "01_Download and format data for app",
    "modelling",
    "03_bruv_multivariate_pco_and_caps.R"
  )
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 1. Load each dataset in the shared column contract
#    (location, site_name, status, period, transect,
#     sampling_event, sampling_event_start_date, scientific, total)
# ------------------------------------------------------------

# Same join as run_dataset_pipeline() in RLS/13: sa_sites.rds is the
# source of truth for location and status.
load_rls_raw_data <- function(count_rds, sites_rds = RLS_SITES_RDS) {

  site_meta <- read_rds(sites_rds) %>%
    mutate(site_code = as.character(site_code)) %>%
    distinct(site_code, .keep_all = TRUE)

  read_rds(count_rds) %>%
    mutate(site_code = as.character(site_code)) %>%
    select(-any_of(c("location", "region", "status", "site_name_lookup"))) %>%
    left_join(site_meta, by = "site_code")
}

datasets <- c(
  list(BRUV = list(label = "BRUVs", raw = build_bruv_raw_data())),
  RLS_DATASETS %>%
    split(.$prefix) %>%
    map(~ list(label = .x$label, raw = load_rls_raw_data(.x$count_rds)))
)


# ------------------------------------------------------------
# 2. Average to location x status x sampling date
# ------------------------------------------------------------

# Which sites make up each location x status x date point, compared with
# all the sites that location x status was ever sampled at.
check_site_sets <- function(site_event) {

  site_event %>%
    distinct(location, status, period, survey_date, site_name) %>%
    group_by(location, status) %>%
    mutate(all_sites = paste(sort(unique(site_name)), collapse = ", "),
           n_sites_total = n_distinct(site_name)) %>%
    group_by(location, status, period, survey_date, all_sites, n_sites_total) %>%
    summarise(
      sites   = paste(sort(unique(site_name)), collapse = ", "),
      n_sites = n_distinct(site_name),
      .groups = "drop"
    ) %>%
    mutate(
      complete      = n_sites == n_sites_total,
      missing_sites = map2_chr(
        strsplit(all_sites, ", "), strsplit(sites, ", "),
        ~ paste(setdiff(.x, .y), collapse = ", ")
      )
    ) %>%
    select(location, status, period, survey_date, n_sites, n_sites_total,
           complete, sites, missing_sites) %>%
    arrange(location, status, survey_date)
}

build_status_date_data <- function(raw_data, site_set = "all") {

  replicate_data <- raw_data %>%
    filter(
      !is.na(location), !is.na(status), !is.na(period),
      !is.na(sampling_event_start_date)
    ) %>%
    # RLS blocks -> transect (a no-op for BRUVs, where each deployment
    # is already one transect). This is the replicate level.
    group_by(location, status, period, site_name, sampling_event,
             sampling_event_start_date, transect, scientific) %>%
    summarise(total = mean(total, na.rm = TRUE), .groups = "drop") %>%
    mutate(survey_date = as.Date(sampling_event_start_date))

  site_check <- check_site_sets(replicate_data)

  if (site_set == "complete_dates") {
    keep <- site_check %>%
      filter(complete) %>%
      distinct(location, status, survey_date)

    message(
      "  complete_dates: kept ", nrow(keep), " of ",
      nrow(site_check), " location x status x date points."
    )

    replicate_data <- replicate_data %>%
      semi_join(keep, by = c("location", "status", "survey_date"))
  } else if (site_set != "all") {
    stop("site_set must be 'all' or 'complete_dates'.")
  }

  averaged <- replicate_data %>%
    # transect / deployment -> location x status x date, as ONE pooled
    # mean, so every transect or deployment carries equal weight (a site
    # with more replicates on a trip counts for more).
    group_by(location, status, period, survey_date, scientific) %>%
    summarise(
      total        = mean(total, na.rm = TRUE),
      n_sites      = n_distinct(site_name),
      n_replicates = n_distinct(transect),
      .groups = "drop"
    ) %>%
    mutate(id = paste(location, status, survey_date, sep = " | "))

  meta <- averaged %>%
    group_by(id, location, status, period, survey_date) %>%
    summarise(n_sites = max(n_sites), n_replicates = max(n_replicates), .groups = "drop") %>%
    mutate(
      period = factor(period, levels = c("Pre-bloom", "Bloom")),
      status = factor(status, levels = c("Fished", "No-take"))
    ) %>%
    arrange(location, status, survey_date)

  # A date belongs to exactly one period, so each id should appear once.
  stopifnot(!anyDuplicated(meta$id))

  assemblage <- averaged %>%
    select(id, scientific, total) %>%
    pivot_wider(names_from = scientific, values_from = total, values_fill = 0) %>%
    arrange(match(id, meta$id)) %>%
    column_to_rownames("id") %>%
    as.data.frame()

  stopifnot(identical(rownames(assemblage), meta$id))

  # Drop only taxa never recorded (same rule as RLS/13).
  assemblage <- assemblage[, colSums(assemblage) > 0, drop = FALSE]

  assemblage_log2 <- vegan::decostand(assemblage, method = "log", logbase = 2)

  list(
    meta            = meta,
    site_check      = site_check,
    assemblage_log2 = assemblage_log2,
    dist            = safe_altgower(assemblage_log2, context = "all locations")
  )
}


# ------------------------------------------------------------
# 3. PCO and Bloom CAP
# ------------------------------------------------------------

run_pco <- function(built) {

  pcoa <- cmdscale(built$dist, k = 2, eig = TRUE, add = TRUE)
  pct  <- pcoa_percent_variance(pcoa)

  scores <- tibble(
    id    = rownames(pcoa$points),
    axis1 = pcoa$points[, 1],
    axis2 = pcoa$points[, 2]
  ) %>%
    left_join(built$meta, by = "id")

  vectors <- NULL
  if (N_SPECIES_VECTORS > 0) {
    ord <- pcoa$points
    colnames(ord) <- c("axis1", "axis2")
    vectors <- fit_species_envfit(
      ord, built$assemblage_log2, "axis1", "axis2", top_n = N_SPECIES_VECTORS
    ) %>%
      scale_species_vectors(scores, "axis1", "axis2")
  }

  list(
    scores  = scores,
    vectors = vectors,
    x_lab   = paste0("PCO1 (", pct[1], "% of total variation)"),
    y_lab   = paste0("PCO2 (", pct[2], "% of total variation)")
  )
}

run_cap_period <- function(built) {

  meta <- built$meta
  dist_obj <- built$dist

  if (n_distinct(meta$period) < 2) {
    message("  Bloom CAP skipped: only one period present.")
    return(NULL)
  }

  cap_model <- if (CAP_CONDITION_ON_LOCATION) {
    vegan::capscale(
      dist_obj ~ period + Condition(location),
      data = meta, comm = built$assemblage_log2, add = TRUE
    )
  } else {
    vegan::capscale(
      dist_obj ~ period,
      data = meta, comm = built$assemblage_log2, add = TRUE
    )
  }

  # Period labels are only swapped within a location.
  set.seed(123)
  cap_test <- anova(cap_model, permutations = 999, strata = meta$location)

  site_scores <- as.data.frame(vegan::scores(cap_model, display = "sites"))

  scores <- tibble(
    id    = rownames(site_scores),
    axis1 = site_scores$CAP1,
    axis2 = site_scores$MDS1
  ) %>%
    left_join(meta, by = "id")

  # Percentages of the variation left once location is removed
  # (or of all variation, if not conditioning).
  denom   <- cap_model$CCA$tot.chi + cap_model$CA$tot.chi
  cap_pct <- round(100 * cap_model$CCA$eig[1] / denom, 1)
  mds_pct <- round(100 * cap_model$CA$eig[1] / denom, 1)
  of_what <- if (CAP_CONDITION_ON_LOCATION) "within-location variation" else "total variation"

  # Same species-vector approach as get_period_cap_scores() in RLS/13.
  vectors <- NULL
  if (N_SPECIES_VECTORS > 0) {
    sp <- as.data.frame(vegan::scores(cap_model, display = "species"))
    if (all(c("CAP1", "MDS1") %in% names(sp))) {
      vectors <- sp %>%
        transmute(
          axis1 = CAP1, axis2 = MDS1,
          scientific = rownames(sp),
          vector_length = sqrt(CAP1^2 + MDS1^2)
        ) %>%
        arrange(desc(vector_length)) %>%
        slice_head(n = N_SPECIES_VECTORS) %>%
        scale_species_vectors(scores, "axis1", "axis2")
    }
  }

  list(
    scores  = scores,
    vectors = vectors,
    x_lab   = paste0("CAP1 - Bloom effect (", cap_pct, "% of ", of_what, ")"),
    y_lab   = paste0("MDS1 (", mds_pct, "% of ", of_what, ")"),
    test    = tibble(
      model      = if (CAP_CONDITION_ON_LOCATION) "period + Condition(location)" else "period",
      n_points   = nrow(meta),
      n_locations = n_distinct(meta$location),
      F_value    = cap_test$F[1],
      p_value    = cap_test$`Pr(>F)`[1],
      cap1_pct   = cap_pct
    )
  )
}


# ------------------------------------------------------------
# 4. Plot: every location on one plot
#    colour = location, shape/line type = status, fill = period
#    (same encoding as the per-location figures, with location
#    in place of site). Arrows join consecutive dates within each
#    location x status.
# ------------------------------------------------------------

plot_all_locations <- function(ord, subtitle = NULL) {

  scores <- ord$scores

  trajectories <- scores %>%
    arrange(location, status, survey_date) %>%
    group_by(location, status) %>%
    mutate(xend = lead(axis1), yend = lead(axis2)) %>%
    ungroup() %>%
    filter(!is.na(xend))

  p <- ggplot(scores, aes(x = axis1, y = axis2, colour = location)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey85") +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey85") +
    geom_segment(
      data = trajectories,
      aes(xend = xend, yend = yend, linetype = status),
      arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
      linewidth = 0.5, alpha = 0.6
    ) +
    geom_point(aes(shape = status, fill = period), size = 2.8, stroke = 0.9) +
    status_period_layers() +
    scale_colour_manual(values = get_site_colours(n_distinct(scores$location))) +
    guides(colour = guide_legend(ncol = 1, override.aes = list(shape = 16, linetype = 0))) +
    labs(
      x = ord$x_lab, y = ord$y_lab,
      colour = "Location", shape = "Status", fill = "Period", linetype = "Status",
      subtitle = subtitle
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.subtitle = element_text(size = 10, colour = "grey25"),
      legend.text   = element_text(size = 9)
    )

  if (!is.null(ord$vectors) && nrow(ord$vectors) > 0) {
    p <- p + species_vector_layers(ord$vectors)
  }

  p
}


# Bloom CAP1 through time: each location x status as a line across its
# sampling dates, same encoding as above (the all-locations version of
# script 13's "CAP_Period_time" figure).
plot_cap_time_all_locations <- function(cap, subtitle = NULL) {

  scores <- arrange(cap$scores, location, status, survey_date)

  ggplot(scores, aes(
    x = survey_date, y = axis1,
    group = interaction(location, status), colour = location
  )) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
    geom_line(aes(linetype = status), linewidth = 0.6, alpha = 0.7) +
    geom_point(aes(shape = status, fill = period), size = 2.8, stroke = 0.9) +
    status_period_layers() +
    scale_colour_manual(values = get_site_colours(n_distinct(scores$location))) +
    guides(colour = guide_legend(ncol = 1, override.aes = list(shape = 16, linetype = 0))) +
    scale_x_date(
      date_breaks = "2 years", date_labels = "%Y",
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    labs(
      x = "Sampling date", y = cap$x_lab,
      colour = "Location", shape = "Status", fill = "Period", linetype = "Status",
      subtitle = subtitle
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(size = 10, colour = "grey25"),
      legend.text   = element_text(size = 9)
    )
}


# ------------------------------------------------------------
# 4b. Status CAP, and Bloom CAP1 vs Status CAP1
#
# The Status CAP is fitted only to locations that have BOTH statuses -
# in a one-status location, status and location are the same thing, so
# once location is conditioned out there is no status contrast left.
# It uses the same model shape as the Bloom CAP:
#   distance ~ status + Condition(location)
# so its CAP1 is the Fished vs No-take difference shared across
# locations.
#
# No p-value is given for it. Status is fixed for a whole location x
# status series, so permuting status between individual dates (what
# anova() would do) is not a valid test - the same reason script 13
# only tests Status by permuting whole sites. Use the per-location
# Status PERMANOVAs for inference; this is a visualisation.
# ------------------------------------------------------------

run_cap_status <- function(built) {

  two_status_locations <- built$meta %>%
    group_by(location) %>%
    filter(n_distinct(status) == 2) %>%
    pull(location) %>%
    unique()

  if (length(two_status_locations) == 0) {
    message("  Status CAP skipped: no location has both statuses.")
    return(NULL)
  }

  keep     <- built$meta$location %in% two_status_locations
  meta     <- built$meta[keep, ]
  dist_obj <- stats::as.dist(as.matrix(built$dist)[keep, keep])
  comm     <- built$assemblage_log2[keep, , drop = FALSE]
  comm     <- comm[, colSums(comm) > 0, drop = FALSE]

  cap_model <- vegan::capscale(
    dist_obj ~ status + Condition(location),
    data = meta, comm = comm, add = TRUE
  )

  site_scores <- as.data.frame(vegan::scores(cap_model, display = "sites"))

  denom   <- cap_model$CCA$tot.chi + cap_model$CA$tot.chi
  cap_pct <- round(100 * cap_model$CCA$eig[1] / denom, 1)

  list(
    scores = tibble(id = rownames(site_scores), status_CAP1 = site_scores$CAP1),
    x_lab  = paste0("Status CAP1 (", cap_pct, "% of within-location variation)"),
    n_locations = length(two_status_locations)
  )
}

# Each point's Bloom CAP1 against its Status CAP1 (the all-locations
# version of script 13's "Bloom vs protection" figure). Only points from
# locations with both statuses appear, since only they have a Status CAP
# score.
plot_bloom_vs_status <- function(cap, cap_status, subtitle = NULL) {

  scores <- cap$scores %>%
    inner_join(cap_status$scores, by = "id") %>%
    arrange(location, status, survey_date)

  trajectories <- scores %>%
    group_by(location, status) %>%
    mutate(xend = lead(axis1), yend = lead(status_CAP1)) %>%
    ungroup() %>%
    filter(!is.na(xend))

  ggplot(scores, aes(x = axis1, y = status_CAP1, colour = location)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey70") +
    geom_segment(
      data = trajectories,
      aes(xend = xend, yend = yend, linetype = status),
      arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
      linewidth = 0.5, alpha = 0.6
    ) +
    geom_point(aes(shape = status, fill = period), size = 2.8, stroke = 0.9) +
    status_period_layers() +
    scale_colour_manual(values = get_site_colours(n_distinct(scores$location))) +
    guides(colour = guide_legend(ncol = 1, override.aes = list(shape = 16, linetype = 0))) +
    labs(
      x = cap$x_lab, y = cap_status$x_lab,
      colour = "Location", shape = "Status", fill = "Period", linetype = "Status",
      subtitle = subtitle
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.subtitle = element_text(size = 10, colour = "grey25"),
      legend.text   = element_text(size = 9)
    )
}


# ------------------------------------------------------------
# 4c. Location impact scores, for colouring the CAP-through-time plot
#
# The score is the dashboard's own "% of pre-bloom" figure for each
# indicator (mean during the bloom / mean pre-bloom x 100), averaged
# across indicators for each location - the same calculation as
# overall_impact_location in script 03. 100 = no change, below 100 =
# decline. Read from the app data, so it is exactly what the dashboard
# shows:
#   BRUVs: hab_data$impact_data_location           (script 03)
#   RLS:   rls_data$pct_change_location, Pre-bloom vs Bloom,
#          both statuses pooled                    (scripts 10 + 15)
#
# Each RLS panel is coloured using that method's own indicators only
# (e.g. the M1 fish panel uses the M1 fish indicators), so a colour
# describes the data in the panel it sits in.
#
# Categories use the dashboard thresholds: 80% or more of pre-bloom =
# Low, 50-80% = Medium, below 50% = High.
# ------------------------------------------------------------

IMPACT_EXCLUDE_METRICS <- c(
  # Thamnaconus degeni - left out of the score on request.
  "thamnaconus_degeni",
  # BRUV Shannon diversity: inverse Simpson replaced it in the app, so
  # counting both would weight diversity twice. (RLS Shannon is kept -
  # it is RLS's only diversity indicator.) Delete this line to include it.
  "shannon_diversity"
)

IMPACT_COLOURS <- c(Low = "#3B7EA1", Medium = "#F2C94C", High = "#EB5757")  # = dashboard gauges

classify_impact <- function(pct) {
  factor(
    dplyr::case_when(
      is.na(pct)  ~ NA_character_,
      pct >= 80   ~ "Low",
      pct >= 50   ~ "Medium",
      TRUE        ~ "High"
    ),
    levels = names(IMPACT_COLOURS)
  )
}

load_location_impacts <- function() {

  app_env <- new.env()
  parts   <- list()

  if (file.exists("app_data/hab_data.Rdata")) {
    load("app_data/hab_data.Rdata", envir = app_env)
    parts$bruv <- tibble::as_tibble(app_env$hab_data$impact_data_location) %>%
      transmute(
        dataset    = "BRUVs",
        location   = reporting_name,
        metric     = impact_metric,
        percentage = percentage
      )
  } else {
    warning("app_data/hab_data.Rdata not found - no BRUV impact scores.")
  }

  if (file.exists("app_data/rls_data.Rdata")) {
    load("app_data/rls_data.Rdata", envir = app_env)
    rd <- app_env$rls_data
    parts$rls <- tibble::as_tibble(rd$pct_change_location) %>%
      filter(comparison_type == "period", comparison_scope == "overall") %>%
      left_join(distinct(rd$metric_lookup, metric_id, dataset), by = "metric_id") %>%
      transmute(
        dataset,
        location   = spatial_group,
        metric     = metric_id,
        percentage = percentage
      )
  } else {
    warning("app_data/rls_data.Rdata not found - no RLS impact scores.")
  }

  bind_rows(parts) %>%
    # Drop excluded indicators, and comparisons with no defined
    # percentage (e.g. a pre-bloom mean of zero).
    filter(!metric %in% IMPACT_EXCLUDE_METRICS, is.finite(percentage)) %>%
    group_by(dataset, location) %>%
    summarise(
      impact_score = mean(percentage),
      n_indicators = n(),
      indicators   = paste(sort(metric), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(impact = classify_impact(impact_score))
}

location_impacts <- load_location_impacts()
safe_write_csv(location_impacts, file.path(OUTPUT_DIR, "location_impact_scores.csv"))

# Inverse Simpson only: the same "% of pre-bloom" figure, but for each
# method's whole-dataset inverse Simpson diversity alone (no averaging
# across indicators), with the same Low / Medium / High cut-offs.
#   BRUVs: hab_data$impact_data_location, simpson_diversity (script 03)
#   RLS:   script 10's location CSV. Read directly, because script 15
#          drops the RLS Simpson metrics before they reach rls_data.
# The phylum-level invertebrate Simpson metrics are not used.
RLS_SIMPSON_METRICS <- c(
  "M1 fish Simpson diversity"         = "M1 fish",
  "M2 fish Simpson diversity"         = "M2 fish",
  "M2 invertebrate Simpson diversity" = "M2 invertebrates"
)

load_simpson_impacts <- function() {

  parts <- list()

  if (file.exists("app_data/hab_data.Rdata")) {
    app_env <- new.env()
    load("app_data/hab_data.Rdata", envir = app_env)
    parts$bruv <- tibble::as_tibble(app_env$hab_data$impact_data_location) %>%
      filter(impact_metric == "simpson_diversity") %>%
      transmute(dataset = "BRUVs", location = reporting_name, percentage)
  }

  rls_csv <- file.path("outputs", "rls_metric_percentage_changes",
                       "rls_metric_percentage_changes_location.csv")
  if (file.exists(rls_csv)) {
    parts$rls <- readr::read_csv(rls_csv, show_col_types = FALSE) %>%
      filter(
        comparison_type == "period", comparison_scope == "overall",
        metric %in% names(RLS_SIMPSON_METRICS)
      ) %>%
      transmute(
        dataset  = unname(RLS_SIMPSON_METRICS[metric]),
        location = spatial_group,
        percentage
      )
  } else {
    warning("Missing ", rls_csv, " - re-run RLS/10. No RLS Simpson scores.")
  }

  bind_rows(parts) %>%
    filter(is.finite(percentage)) %>%
    transmute(dataset, location, impact_score = percentage, impact = classify_impact(percentage))
}

simpson_impacts <- load_simpson_impacts()
safe_write_csv(simpson_impacts, file.path(OUTPUT_DIR, "location_simpson_scores.csv"))

# Bloom CAP1 through time, with lines and point outlines coloured by the
# location's impact score instead of by location. colour_by is
# "continuous" (the score itself) or "category" (Low / Medium / High).
# Each line is labelled with its location at its last date, since colour
# no longer identifies the location.
#
# score_tbl is location_impacts (all indicators) or simpson_impacts
# (inverse Simpson only); score_name is the legend wording for it.
plot_cap_time_by_impact <- function(cap_scores, dataset_label,
                                    colour_by  = c("continuous", "category"),
                                    score_tbl  = location_impacts,
                                    score_name = "Overall impact",
                                    y_lab      = "Bloom CAP1") {

  colour_by <- match.arg(colour_by)

  scores <- cap_scores %>%
    left_join(
      filter(score_tbl, dataset == dataset_label) %>%
        select(location, impact_score, impact),
      by = "location"
    ) %>%
    arrange(location, status, survey_date)

  unmatched <- setdiff(unique(scores$location), score_tbl$location[score_tbl$dataset == dataset_label])
  if (length(unmatched) > 0) {
    message("  ", dataset_label, ": no impact score for ", paste(unmatched, collapse = "; "),
            " - drawn in grey.")
  }

  end_labels <- scores %>%
    group_by(location) %>%
    slice_max(survey_date, n = 1, with_ties = FALSE) %>%
    ungroup()

  colour_var <- if (colour_by == "continuous") "impact_score" else "impact"

  # For the categories, give locations with no score their own
  # "Not assessed" level, so every panel has the same four legend keys
  # and patchwork merges them into one legend.
  if (colour_by == "category") {
    scores <- scores %>%
      mutate(impact = factor(
        tidyr::replace_na(as.character(impact), "Not assessed"),
        levels = c(names(IMPACT_COLOURS), "Not assessed")
      ))
    end_labels <- end_labels %>%
      select(-impact) %>%
      left_join(distinct(scores, location, impact), by = "location")
  }

  pre_bloom <- filter(scores, period == "Pre-bloom")
  bloom     <- filter(scores, period == "Bloom")

  # Pre-bloom points are open (outline in the impact colour); bloom points
  # are FILLED with the impact colour, larger, with a thin black outline so
  # amber still shows against the white background. Period is therefore
  # shown by open vs filled, rather than by a separate fill legend.
  p <- ggplot(scores, aes(
    x = survey_date, y = axis1,
    group = interaction(location, status),
    colour = .data[[colour_var]]
  )) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
    # show.legend = TRUE so every panel draws every key and the four
    # panels' legends match (and so merge) - see plot_cap_time_impact_fill().
    geom_line(aes(linetype = status), linewidth = 0.7, alpha = 0.85, show.legend = TRUE) +
    geom_point(
      data = pre_bloom, aes(shape = status),
      fill = "white", size = 2.4, stroke = 0.9,
      show.legend = FALSE
    ) +
    geom_point(
      data = bloom, aes(shape = status, fill = .data[[colour_var]]),
      colour = "black", size = 3.8, stroke = 0.5,
      show.legend = TRUE
    ) +
    ggrepel::geom_text_repel(
      data = end_labels, aes(label = location),
      size = 2.6, nudge_x = 60, direction = "y", hjust = 0,
      segment.size = 0.2, min.segment.length = 0, show.legend = FALSE
    ) +
    scale_shape_manual(values = c("Fished" = 21, "No-take" = 24)) +
    scale_linetype_manual(values = c("Fished" = "dashed", "No-take" = "solid")) +
    scale_x_date(
      date_breaks = "2 years", date_labels = "%Y",
      expand = expansion(mult = c(0.02, 0.18))   # room for the labels
    ) +
    labs(
      x = "Sampling date", y = y_lab,
      shape = "Status", linetype = "Status",
      caption = "Open symbols = pre-bloom; filled symbols = bloom, filled with the location's impact colour"
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.text  = element_text(size = 9),
      plot.caption = element_text(size = 10, colour = "grey30", hjust = 0)
    )

  if (colour_by == "continuous") {
    # Same colours as the categories, blended: red at 0%, amber at 50%,
    # blue from 80% up, darker blue for increases. Scores above 150% are
    # drawn at the 150% colour. One scale drives both the line/outline
    # colour and the bloom-point fill.
    p + scale_colour_gradientn(
      aesthetics = c("colour", "fill"),
      colours = c("#EB5757", "#F2C94C", "#3B7EA1", "#1F4E6B"),
      values  = scales::rescale(c(0, 50, 80, 150), from = c(0, 150)),
      limits  = c(0, 150),
      oob     = scales::squish,
      breaks  = c(0, 50, 80, 100, 150),
      labels  = c("0%", "50%", "80%", "100%", "150%+"),
      na.value = "grey70",
      name    = paste0(score_name, "\n(% of pre-bloom)")
    ) +
      guides(
        colour = guide_colourbar(barheight = unit(6, "cm")),
        shape  = guide_legend(override.aes = list(fill = "grey50", colour = "black"))
      )
  } else {
    p + scale_colour_manual(
      aesthetics = c("colour", "fill"),
      values   = c(IMPACT_COLOURS, "Not assessed" = "grey70"),
      limits   = c(names(IMPACT_COLOURS), "Not assessed"),
      drop     = FALSE,
      name     = score_name,
      labels   = c(Low = "Low (\u226580%)", Medium = "Medium (50-80%)",
                   High = "High (<50%)", "Not assessed" = "Not assessed")
    ) +
      guides(
        colour = guide_legend(override.aes = list(shape = 21, linetype = 0, size = 4)),
        shape  = guide_legend(override.aes = list(fill = "grey50", colour = "black"))
      )
  }
}


# ------------------------------------------------------------
# 4d. Impact category for EACH indicator, and a CAP-through-time plot
#     with every marker filled by the impact category
#
# One row per method x location x indicator: the dashboard's "% of
# pre-bloom" for that indicator alone, put into Low / Medium / High with
# the same cut-offs as above.
#   BRUVs: every indicator in hab_data$impact_data_location (all of them,
#          T. degeni and Shannon included - each gets its own figure).
#   RLS:   every indicator the dashboard shows (rls_data$pct_change_location)
#          plus each method's whole-dataset inverse Simpson from script 10.
#
# Note for T. degeni: the same cut-offs are applied, so an INCREASE in
# T. degeni reads as "Low impact" here, as it does for every indicator.
# ------------------------------------------------------------

# e.g. "shark_ray_richness" -> "Shark ray richness"
tidy_indicator_label <- function(x) {
  x %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_replace("200mm", ">200 mm") %>%
    stringr::str_to_sentence()
}

# File- and folder-safe name, e.g. "M2 invertebrates" -> "m2_invertebrates"
slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_remove_all("^_|_$")
}

load_indicator_impacts <- function() {

  app_env <- new.env()
  parts   <- list()

  if (file.exists("app_data/hab_data.Rdata")) {
    load("app_data/hab_data.Rdata", envir = app_env)
    parts$bruv <- tibble::as_tibble(app_env$hab_data$impact_data_location) %>%
      transmute(
        dataset         = "BRUVs",
        location        = reporting_name,
        indicator_id    = impact_metric,
        indicator_group = impact_metric,
        indicator_label = tidy_indicator_label(impact_metric),
        percentage
      )
  }

  if (file.exists("app_data/rls_data.Rdata")) {
    load("app_data/rls_data.Rdata", envir = app_env)
    rd <- app_env$rls_data
    parts$rls <- tibble::as_tibble(rd$pct_change_location) %>%
      filter(comparison_type == "period", comparison_scope == "overall") %>%
      left_join(distinct(rd$metric_lookup, metric_id, dataset), by = "metric_id") %>%
      transmute(
        dataset,
        location        = spatial_group,
        indicator_id    = metric_id,
        indicator_group = metric_group,
        indicator_label = as.character(metric),
        percentage
      )
  }

  rls_csv <- file.path("outputs", "rls_metric_percentage_changes",
                       "rls_metric_percentage_changes_location.csv")
  if (file.exists(rls_csv)) {
    parts$rls_simpson <- readr::read_csv(rls_csv, show_col_types = FALSE) %>%
      filter(
        comparison_type == "period", comparison_scope == "overall",
        metric %in% names(RLS_SIMPSON_METRICS)
      ) %>%
      transmute(
        dataset         = unname(RLS_SIMPSON_METRICS[metric]),
        location        = spatial_group,
        indicator_id    = slug(metric),
        indicator_group = "simpson_diversity",
        indicator_label = metric,
        percentage
      )
  }

  bind_rows(parts) %>%
    filter(is.finite(percentage), !is.na(dataset)) %>%
    mutate(impact = classify_impact(percentage))
}

indicator_impacts <- load_indicator_impacts()
safe_write_csv(indicator_impacts, file.path(OUTPUT_DIR, "location_indicator_impacts.csv"))

# Bloom CAP1 through time with EVERY marker filled by the location's
# impact category for one indicator (or the overall score). Lines use the
# same colour. Pre-bloom markers are smaller and faded; bloom markers are
# larger, fully opaque, with a black outline - so the bloom points stand
# out while every point still shows its category.
plot_cap_time_impact_fill <- function(cap_scores, impact_tbl, title = NULL, subtitle = NULL,
                                      y_lab = "Bloom CAP1") {

  levels_all <- c(names(IMPACT_COLOURS), "Not assessed")

  scores <- cap_scores %>%
    left_join(select(impact_tbl, location, impact), by = "location") %>%
    mutate(impact = factor(
      tidyr::replace_na(as.character(impact), "Not assessed"),
      levels = levels_all
    )) %>%
    arrange(location, status, survey_date)

  end_labels <- scores %>%
    group_by(location) %>%
    slice_max(survey_date, n = 1, with_ties = FALSE) %>%
    ungroup()

  ggplot(scores, aes(x = survey_date, y = axis1, group = interaction(location, status))) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
    # show.legend = TRUE: draw a legend key for every impact level and
    # status even when this panel has none of it (ggplot2 3.5+ otherwise
    # leaves those keys blank). That keeps the four panels' legends
    # identical, so patchwork merges them into one. The faded pre-bloom
    # layer is kept out of the legend so the keys show the solid colour.
    geom_line(aes(colour = impact, linetype = status), linewidth = 0.6, alpha = 0.6,
              show.legend = TRUE) +
    geom_point(
      data = filter(scores, period == "Pre-bloom"),
      aes(shape = status, fill = impact),
      colour = "grey35", size = 2.4, stroke = 0.4, alpha = 0.55,
      show.legend = FALSE
    ) +
    geom_point(
      data = filter(scores, period == "Bloom"),
      aes(shape = status, fill = impact),
      colour = "black", size = 3.8, stroke = 0.7,
      show.legend = TRUE
    ) +
    ggrepel::geom_text_repel(
      data = end_labels, aes(label = location, colour = impact),
      size = 2.6, nudge_x = 60, direction = "y", hjust = 0,
      segment.size = 0.2, min.segment.length = 0, show.legend = FALSE
    ) +
    scale_shape_manual(values = c("Fished" = 21, "No-take" = 24)) +
    scale_linetype_manual(values = c("Fished" = "dashed", "No-take" = "solid")) +
    scale_fill_manual(
      aesthetics = c("fill", "colour"),
      values = c(IMPACT_COLOURS, "Not assessed" = "grey70"),
      limits = levels_all, drop = FALSE,
      labels = c(Low = "Low (\u226580%)", Medium = "Medium (50-80%)",
                 High = "High (<50%)", "Not assessed" = "Not assessed"),
      name = "Impact"
    ) +
    guides(
      fill  = guide_legend(override.aes = list(shape = 21, size = 4, colour = "black", alpha = 1, linetype = 0)),
      shape = guide_legend(override.aes = list(fill = "grey50", colour = "black", alpha = 1))
    ) +
    scale_x_date(
      date_breaks = "2 years", date_labels = "%Y",
      expand = expansion(mult = c(0.02, 0.18))
    ) +
    labs(
      x = "Sampling date", y = y_lab,
      shape = "Status", linetype = "Status",
      title = title, subtitle = subtitle,
      caption = "Marker fill = impact category. Small faded markers = pre-bloom; large outlined markers = bloom."
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10, colour = "grey25"),
      plot.caption  = element_text(size = 10, colour = "grey30", hjust = 0),
      legend.text   = element_text(size = 9)
    )
}


# ------------------------------------------------------------
# 5. Run every dataset for one site set
#    (writes the per-method figures, scores, site checks, CAP tests
#    and the combined four-method figures into output_dir)
# ------------------------------------------------------------

run_all_datasets <- function(site_set, output_dir) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  message("\n##### Site set: ", site_set, " -> ", output_dir, " #####")


  design_summary <- list()
  cap_tests      <- list()

  # Each dataset's three plots, kept for the combined four-method figures.
  # Bloom CAP scores per method, for the impact-coloured versions.
  cap_scores_by_method     <- list()
  cap_axis_label_by_method <- list()

  combined_plots <- list(
    PCO = list(), CAP_Period = list(), CAP_Period_time = list(), CAP_Bloom_vs_Status = list()
  )

  for (prefix in names(datasets)) {

    label <- datasets[[prefix]]$label
    message("=== ", label, " ===")

    result <- tryCatch({

      built <- build_status_date_data(datasets[[prefix]]$raw, site_set = site_set)

      design_summary[[prefix]] <- built$meta %>%
        group_by(location) %>%
        summarise(
          n_points       = n(),
          n_dates        = n_distinct(survey_date),
          statuses       = paste(sort(unique(as.character(status))), collapse = ", "),
          n_pre_bloom    = sum(period == "Pre-bloom"),
          n_bloom        = sum(period == "Bloom"),
          max_sites_per_point = max(n_sites),
          min_replicates_per_point = min(n_replicates),
          max_replicates_per_point = max(n_replicates),
          .groups = "drop"
        ) %>%
        mutate(dataset = label, .before = 1)

      # Site check: which points are not built from the full site set.
      incomplete <- filter(built$site_check, !complete)
      message(
        "  Site check: ", nrow(incomplete), " of ", nrow(built$site_check),
        " points are missing at least one of their location x status's sites",
        if (site_set == "complete_dates") " (before dropping them)" else "",
        ". See ", prefix, "_all_locations_site_check.csv"
      )
      safe_write_csv(
        mutate(built$site_check, dataset = label, .before = 1),
        file.path(output_dir, paste0(prefix, "_all_locations_site_check.csv"))
      )

      n_label <- paste0(
        nrow(built$meta), " location x status x date points from ",
        n_distinct(built$meta$location), " locations",
        if (site_set == "complete_dates") " (complete-site dates only)" else ""
      )

      pco <- run_pco(built)
      pco_plot <- plot_all_locations(pco, n_label)
      combined_plots$PCO[[label]] <- pco_plot
      save_plot(
        pco_plot,
        file.path(output_dir, paste0(prefix, "_all_locations_PCO.png")),
        width = 14, height = 10
      )

      cap <- run_cap_period(built)
      if (!is.null(cap)) {
        cap_tests[[prefix]] <- mutate(cap$test, dataset = label, .before = 1)

        cap_subtitle <- paste0(
          n_label, "  |  CAP permutation test (Period, permuted within location): ",
          "F = ", round(cap$test$F_value, 2), ", p = ", format.pval(cap$test$p_value, digits = 3)
        )

        cap_plot      <- plot_all_locations(cap, cap_subtitle)
        cap_time_plot <- plot_cap_time_all_locations(cap, cap_subtitle)
        combined_plots$CAP_Period[[label]]      <- cap_plot
        combined_plots$CAP_Period_time[[label]] <- cap_time_plot
        cap_scores_by_method[[label]]           <- cap$scores
        cap_axis_label_by_method[[label]]       <- cap$x_lab   # "CAP1 - Bloom effect (x% of ...)"

        save_plot(
          cap_plot,
          file.path(output_dir, paste0(prefix, "_all_locations_CAP_Period.png")),
          width = 14, height = 10
        )

        save_plot(
          cap_time_plot,
          file.path(output_dir, paste0(prefix, "_all_locations_CAP_Period_time.png")),
          width = 14, height = 8
        )
      }

      # An error here shouldn't lose the plots already saved for this method.
      cap_status <- tryCatch(run_cap_status(built), error = function(e) {
        message("  Status CAP failed: ", conditionMessage(e))
        NULL
      })
      if (!is.null(cap) && !is.null(cap_status)) {

        bvs_subtitle <- paste0(
          "Locations with both statuses only (", cap_status$n_locations, ")  |  ",
          "Bloom CAP: F = ", round(cap$test$F_value, 2), ", p = ",
          format.pval(cap$test$p_value, digits = 3),
          "  |  Status CAP: visualisation only, no p-value (see per-location PERMANOVAs)"
        )

        bvs_plot <- plot_bloom_vs_status(cap, cap_status, bvs_subtitle)
        combined_plots$CAP_Bloom_vs_Status[[label]] <- bvs_plot

        save_plot(
          bvs_plot,
          file.path(output_dir, paste0(prefix, "_all_locations_CAP_Bloom_vs_Status.png")),
          width = 14, height = 10
        )
      }

      # Every point with both sets of scores, for checking or re-plotting.
      scores_out <- pco$scores %>%
        select(id, location, status, period, survey_date, n_sites, n_replicates,
               PCO1 = axis1, PCO2 = axis2)
      if (!is.null(cap)) {
        scores_out <- scores_out %>%
          left_join(select(cap$scores, id, CAP1 = axis1, MDS1 = axis2), by = "id")
      }
      if (!is.null(cap_status)) {
        scores_out <- scores_out %>%
          left_join(cap_status$scores, by = "id")   # NA for one-status locations
      }
      safe_write_csv(scores_out, file.path(output_dir, paste0(prefix, "_all_locations_scores.csv")))

      TRUE
    }, error = function(e) {
      message("ERROR for ", label, ": ", conditionMessage(e))
      FALSE
    })
  }

  # ------------------------------------------------------------
  # 6. Combined figures: the four methods side by side, one figure per
  #    plot type. Each panel keeps its own axes and location legend (the
  #    ordinations and the locations differ between methods, so they can't
  #    share them). The method name labels each panel.
  # ------------------------------------------------------------

  method_order <- c("BRUVs", "M1 fish", "M2 fish", "M2 invertebrates")

  combined_sizes <- list(
    PCO             = c(width = 28, height = 20),
    CAP_Period      = c(width = 28, height = 20),
    CAP_Period_time = c(width = 28, height = 16),
    CAP_Bloom_vs_Status = c(width = 28, height = 20)
  )

  for (plot_type in names(combined_plots)) {

    panels <- combined_plots[[plot_type]]
    panels <- panels[intersect(method_order, names(panels))]

    if (length(panels) == 0) next

    panels <- imap(panels, ~ .x + labs(title = .y) +
                     theme(plot.title = element_text(face = "bold", size = 15)))

    save_plot(
      wrap_plots(panels, ncol = 2),
      file.path(output_dir, paste0("all_methods_all_locations_", plot_type, ".png")),
      width  = combined_sizes[[plot_type]][["width"]],
      height = combined_sizes[[plot_type]][["height"]]
    )
  }

  # Four more versions of the combined CAP-over-time figure, coloured by
  # each location's impact rather than by location: the mean across
  # indicators, and inverse Simpson alone - each as a continuous score
  # and as Low / Medium / High.
  colour_versions <- tribble(
    ~score,     ~colour_by,    ~file_suffix,         ~score_name,
    "overall",  "continuous",  "impact_score",       "Mean of indicators",
    "overall",  "category",    "impact_category",    "Overall impact",
    "simpson",  "continuous",  "simpson_score",      "Inverse Simpson",
    "simpson",  "category",    "simpson_category",   "Inverse Simpson impact"
  )

  for (i in seq_len(nrow(colour_versions))) {

    colour_by   <- colour_versions$colour_by[i]
    file_suffix <- colour_versions$file_suffix[i]
    score_tbl   <- if (colour_versions$score[i] == "simpson") simpson_impacts else location_impacts

    labels_present <- intersect(method_order, names(cap_scores_by_method))
    if (length(labels_present) == 0) next

    panels <- map(labels_present, function(lbl) {
      plot_cap_time_by_impact(
        cap_scores_by_method[[lbl]], lbl, colour_by,
        score_tbl = score_tbl, score_name = colour_versions$score_name[i],
        y_lab = cap_axis_label_by_method[[lbl]]
      ) +
        labs(title = lbl) +
        theme(plot.title = element_text(face = "bold", size = 15))
    })

    save_plot(
      # One shared colour legend: the scale is identical in every panel.
      wrap_plots(panels, ncol = 2) + plot_layout(guides = "collect"),
      file.path(output_dir, paste0("all_methods_all_locations_CAP_Period_time_", file_suffix, ".png")),
      width = 28, height = 20
    )
  }

  # ---- Folder: one CAP-through-time figure per method x indicator ----
  # cap_time_by_indicator/<method>/00_overall_impact.png is the combined
  # score (mean across indicators, as above); the rest are one indicator
  # each. Every marker is filled with that location's impact category.
  indicator_dir <- file.path(output_dir, "cap_time_by_indicator")

  for (lbl in intersect(method_order, names(cap_scores_by_method))) {

    method_dir <- file.path(indicator_dir, slug(lbl))
    dir.create(method_dir, recursive = TRUE, showWarnings = FALSE)

    cap_scores <- cap_scores_by_method[[lbl]]

    # Combined score first.
    overall_tbl <- filter(location_impacts, dataset == lbl)
    save_plot(
      plot_cap_time_impact_fill(
        cap_scores, overall_tbl,
        y_lab    = cap_axis_label_by_method[[lbl]]
      ),
      file.path(method_dir, "00_overall_impact.png"),
      width = 14, height = 9
    )

    method_indicators <- indicator_impacts %>%
      filter(dataset == lbl) %>%
      distinct(indicator_id, indicator_label) %>%
      arrange(indicator_label)

    for (j in seq_len(nrow(method_indicators))) {
      ind_id <- method_indicators$indicator_id[j]
      ind_tbl <- filter(indicator_impacts, dataset == lbl, indicator_id == ind_id)

      save_plot(
        plot_cap_time_impact_fill(
          cap_scores, ind_tbl,
          y_lab    = cap_axis_label_by_method[[lbl]]
        ),
        file.path(method_dir, sprintf("%02d_%s.png", j, slug(ind_id))),
        width = 14, height = 9
      )
    }

    message("  ", lbl, ": ", nrow(method_indicators) + 1, " figures in ", method_dir)
  }

  # ---- The four methods together, same style ----
  # cap_time_by_indicator/all_methods/: the overall score, and each
  # indicator that all four methods measure (indicator_group is the shared
  # key: BRUV impact_metric / RLS metric_group). Indicators only one method
  # has (e.g. B20, sharks and rays, phylum metrics) are in the method
  # folders only.
  shared_groups <- tribble(
    ~group,               ~label,
    "overall",            "Overall impact",
    "species_richness",   "Species richness",
    "total_abundance",    "Total abundance",
    "simpson_diversity",  "Inverse Simpson diversity",
    "shannon_diversity",  "Shannon diversity"
  )

  all_methods_dir <- file.path(indicator_dir, "all_methods")
  dir.create(all_methods_dir, recursive = TRUE, showWarnings = FALSE)
  labels_present <- intersect(method_order, names(cap_scores_by_method))

  for (g in seq_len(nrow(shared_groups))) {

    grp <- shared_groups$group[g]

    panels <- map(labels_present, function(lbl) {
      impact_tbl <- if (grp == "overall") {
        filter(location_impacts, dataset == lbl)
      } else {
        filter(indicator_impacts, dataset == lbl, indicator_group == grp)
      }
      plot_cap_time_impact_fill(
        cap_scores_by_method[[lbl]], impact_tbl, title = lbl,
        y_lab = cap_axis_label_by_method[[lbl]]
      ) +
        theme(plot.caption = element_blank())
    })

    save_plot(
      wrap_plots(panels, ncol = 2) +
        plot_layout(guides = "collect") +
        plot_annotation(
          caption = "Marker fill = impact category. Small faded markers = pre-bloom; large outlined markers = bloom.",
          theme   = theme(plot.caption = element_text(size = 12, colour = "grey30", hjust = 0))
        ),
      file.path(all_methods_dir, sprintf("%02d_%s.png", g - 1, grp)),
      width = 28, height = 20
    )
  }

  design_summary <- bind_rows(design_summary)
  cap_tests      <- bind_rows(cap_tests)

  safe_write_csv(design_summary, file.path(output_dir, "all_locations_design_summary.csv"))
  safe_write_csv(cap_tests,      file.path(output_dir, "all_locations_cap_tests.csv"))

  print(as.data.frame(cap_tests), row.names = FALSE)
  message("Figures and tables written to ", output_dir)

  invisible(list(design_summary = design_summary, cap_tests = cap_tests))
}


# ------------------------------------------------------------
# 7. All site-events, as recorded
# ------------------------------------------------------------

results_all_sites <- run_all_datasets("all", OUTPUT_DIR)


# ------------------------------------------------------------
# 8. Only location x status x date points where ALL of that
#    location x status's sites were sampled
#
# Same plots again, but each trajectory is built from the same sites
# throughout. Compare all_locations_design_summary.csv in the two
# folders to see how many points (and which locations) dropped out.
# ------------------------------------------------------------

results_complete_sites <- run_all_datasets("complete_dates", COMPLETE_SITES_DIR)

