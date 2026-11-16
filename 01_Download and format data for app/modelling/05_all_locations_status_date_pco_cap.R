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

