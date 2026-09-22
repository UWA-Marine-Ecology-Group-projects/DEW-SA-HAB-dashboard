# ============================================================
# BRUV MULTIVARIATE: PCoA, CAPs and the two PERMANOVAs
#
# The BRUV equivalent of
#   01_Download and format data for app/RLS/13_rls_multivariate_pco_and_caps.R
#
# It does NOT contain its own copy of the statistics. It sources
# script 13 as a function library (see MULTIVARIATE_ENGINE_ONLY
# below) and hands it BRUV data reshaped into the same column
# contract the RLS pipeline uses. So the modified-Gower distance,
# the PCoA, both CAPs, the within-site Period PERMANOVA, the
# whole-site Status permutation test and the dashboard export
# layer are all literally the same code that produced the RLS
# results - there is no second implementation to drift.
#
# Everything BRUV-specific lives in build_bruv_raw_data() below.
#
# Run from the project root, after
#   01_Download and format data for app/03_Format BRUVS and RLS data for summaries and plots.R
# has been run (it writes sa_bruv_metadata.csv, which this reads).
#
#   source("01_Download and format data for app/modelling/03_bruv_multivariate_pco_and_caps.R")
#
# ------------------------------------------------------------
# HOW BRUV DATA IS MAPPED ONTO THE RLS DESIGN
#
# RLS                                  BRUV
# -----------------------------------  ---------------------------------
# location                             reporting_name (matches the GLMMs)
# site_name                            uwa_site_code (matches the GLMM
#                                      site random effect)
# transect (the replicate within a     campaignid + sample (one BRUV
#   site x event)                        deployment)
# block (the replicate within a        no equivalent - the first
#   transect)                            averaging step is a no-op
# sampling_event                       campaignid + event_number
# sampling_event_start_date            start_date of that event
# total (abundance)                    count (MaxN)
#
# The analysis unit is therefore site x sampling event: MaxN is
# averaged across the deployments made at one site during one
# sampling event, exactly as RLS averages blocks within transects
# and transects within a site x event. That keeps Period a
# within-site factor and Status fixed at the site level, which is
# what the two different PERMANOVA designs in script 13 assume.
#
# TAXA: all taxa as recorded, including genus-level "spp" records
# and Unknown genera, and including invertebrates (Portunidae,
# Ovalipidae and so on). Only the taxonomy harmonisations that
# script 03 already applies are applied here, so the same animal
# is not split across two names.
#
# ZERO FILLING: sa_count_bruv holds only the taxa that were seen,
# with no zero rows. Averaging that directly across deployments
# would give a mean over the deployments where a taxon was
# PRESENT, not a mean abundance per deployment. So this script
# builds a complete deployment x taxon frame with explicit zeros
# first, mirroring how the RLS *_complete_count.rds files are
# built in RLS/03 (tidyr::crossing() then replace_na(0)). The 22
# deployments that recorded nothing are kept as all-zero rows,
# matching the RLS treatment of empty surveys.
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(tibble)


# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

BRUV_METADATA_CSV <- "sa_bruv_metadata.csv"
BRUV_COUNT_RDS    <- "data/raw/sa_count_bruv.RDS"

BRUV_OUTPUT_DIR     <- file.path("outputs", "multivariate", "BRUV_with_status")
BRUV_APP_EXPORT_DIR <- file.path("outputs", "multivariate", "bruv_app")

BRUV_DATASET_PREFIX <- "BRUV"
BRUV_DATASET_LABEL  <- "BRUVs"

# Six uwa_site_codes carry BOTH management statuses across their
# deployments: one at Aldinga (36 Fished vs 1 No-take, which looks
# like a single mislabelled drop) and five at Port Gibbon, where
# the split is close to even and the deployments look like they
# straddle the sanctuary boundary.
#
# The whole-site Status permutation test requires one status per
# site, so something has to give.
#
#   TRUE  - split such a code into two site units, "<code> (Fished)"
#           and "<code> (No-take)", so every deployment keeps its
#           recorded status. Honest about the labels, but the two
#           halves are spatially adjacent, so they are not fully
#           independent replicates of Status.
#   FALSE - give the whole code its most common status and relabel
#           the minority deployments. Keeps site identical to the
#           GLMM site random effect, but at Port Gibbon the modal
#           status is close to a coin toss.
#
# Either way the affected codes are written to
# BRUV_mixed_status_sites.csv in the output directory - read it
# before interpreting the Port Gibbon Status result.
SPLIT_MIXED_STATUS_SITES <- TRUE


# ------------------------------------------------------------
# Load the shared analysis engine
#
# MULTIVARIATE_ENGINE_ONLY tells script 13 to define its functions
# and stop, instead of running the three RLS datasets.
# ------------------------------------------------------------

MULTIVARIATE_ENGINE_ONLY <- TRUE

source(
  file.path(
    "01_Download and format data for app",
    "RLS",
    "13_rls_multivariate_pco_and_caps.R"
  )
)

# dataset_label_for() refuses a prefix it has no dashboard label
# for, so register the BRUV one rather than editing script 13.
DATASET_LABELS[[BRUV_DATASET_PREFIX]] <- BRUV_DATASET_LABEL


# ============================================================
# BRUV DATA PREP
#
# Produces a data frame with exactly the columns script 13's
# build_assemblage_data() and build_site_level_data() expect:
#
#   location, site_name, status, period, transect,
#   sampling_event, sampling_event_start_date,
#   family, genus, species, scientific, total
#
# plus site_code, campaignid and sample, which are carried for
# traceability and ignored by the engine.
# ============================================================

build_bruv_raw_data <- function(
    metadata_csv = BRUV_METADATA_CSV,
    count_rds    = BRUV_COUNT_RDS,
    split_mixed_status_sites = SPLIT_MIXED_STATUS_SITES,
    output_dir   = BRUV_OUTPUT_DIR
) {

  if (!file.exists(metadata_csv)) {
    stop(
      "Cannot find ", metadata_csv, ". It is written by ",
      "'01_Download and format data for app/",
      "03_Format BRUVS and RLS data for summaries and plots.R', ",
      "which needs to be run first."
    )
  }

  if (!file.exists(count_rds)) {
    stop("Cannot find ", count_rds, ".")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ----------------------------------------------------------
  # Metadata: one row per successful BRUV deployment
  #
  # sa_bruv_metadata.csv has already been filtered to
  # successful_count == "Yes" and joined to the reporting regions
  # by script 03.
  # ----------------------------------------------------------

  meta_raw <- readr::read_csv(
    metadata_csv,
    show_col_types = FALSE,
    progress = FALSE
  )

  required_meta_cols <- c(
    "campaignid", "sample", "date", "status", "period",
    "reporting_name", "uwa_site_code", "event_number", "start_date"
  )

  missing_meta_cols <- setdiff(required_meta_cols, names(meta_raw))

  if (length(missing_meta_cols) > 0) {
    stop(
      "sa_bruv_metadata.csv is missing column(s): ",
      paste(missing_meta_cols, collapse = ", "),
      ". Re-run script 03."
    )
  }

  deployments <- meta_raw %>%
    dplyr::mutate(
      campaignid = as.character(campaignid),
      sample     = as.character(sample)
    ) %>%
    # The same two status corrections the GLMM script applies in
    # prep_metric_data(), so the multivariate and the models
    # disagree about no deployment.
    dplyr::mutate(
      status = dplyr::if_else(sample %in% "OASO04_2510", "Fished", status),
      status = dplyr::if_else(
        as.character(uwa_site_code) %in% "45", "No-take", status
      )
    ) %>%
    # A deployment with no reporting_name sits outside every
    # reporting unit, and one with no uwa_site_code cannot be
    # placed in a site, so neither can contribute a site x event.
    dplyr::filter(
      !is.na(reporting_name),
      !is.na(uwa_site_code)
    ) %>%
    dplyr::mutate(
      location       = reporting_name,
      site_code      = as.character(uwa_site_code),
      transect       = paste(campaignid, sample, sep = "_"),
      sampling_event = paste(campaignid, event_number, sep = "_"),
      sampling_event_start_date = as.Date(start_date)
    ) %>%
    dplyr::select(
      campaignid, sample, location, site_code, status, period,
      transect, sampling_event, sampling_event_start_date
    ) %>%
    dplyr::distinct()

  if (anyDuplicated(deployments$transect) > 0) {
    stop(
      "campaignid + sample does not uniquely identify a deployment ",
      "in sa_bruv_metadata.csv, so deployments cannot be used as ",
      "the replicate within a site x sampling event."
    )
  }

  message(
    "BRUV metadata: ", nrow(deployments), " deployment(s) across ",
    dplyr::n_distinct(deployments$location), " reporting name(s)."
  )

  # ----------------------------------------------------------
  # Resolve sites that carry more than one status
  # ----------------------------------------------------------

  mixed_status <- deployments %>%
    dplyr::filter(!is.na(status)) %>%
    dplyr::count(location, site_code, status, name = "n_deployments") %>%
    dplyr::group_by(site_code) %>%
    dplyr::filter(dplyr::n_distinct(status) > 1) %>%
    dplyr::ungroup()

  if (nrow(mixed_status) > 0) {

    message(
      "NOTE: ", dplyr::n_distinct(mixed_status$site_code),
      " uwa_site_code(s) carry more than one status. Handling: ",
      if (split_mixed_status_sites) {
        "splitting each into one site unit per status."
      } else {
        "assigning each its most common status."
      }
    )

    safe_write_csv(
      mixed_status %>%
        dplyr::mutate(
          handling = if (split_mixed_status_sites) "split" else "modal"
        ) %>%
        dplyr::arrange(location, site_code, status),
      file.path(output_dir, "BRUV_mixed_status_sites.csv")
    )
  }

  mixed_codes <- unique(mixed_status$site_code)

  if (split_mixed_status_sites) {

    deployments <- deployments %>%
      dplyr::mutate(
        site_name = dplyr::if_else(
          site_code %in% mixed_codes & !is.na(status),
          paste0(site_code, " (", status, ")"),
          site_code
        )
      )

  } else {

    # Most common status per code; ties break alphabetically so the
    # result does not depend on row order.
    modal_status <- deployments %>%
      dplyr::filter(!is.na(status)) %>%
      dplyr::count(site_code, status, name = "n_deployments") %>%
      dplyr::arrange(site_code, dplyr::desc(n_deployments), status) %>%
      dplyr::group_by(site_code) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(site_code, modal_status = status)

    deployments <- deployments %>%
      dplyr::left_join(modal_status, by = "site_code") %>%
      dplyr::mutate(
        status    = dplyr::coalesce(modal_status, status),
        site_name = site_code
      ) %>%
      dplyr::select(-modal_status)
  }

  # build_assemblage_data() keys each row on site_name +
  # sampling_event and stops on duplicates, so check the things
  # that would make that key ambiguous before it gets there.
  event_check <- deployments %>%
    dplyr::group_by(site_name, sampling_event) %>%
    dplyr::summarise(
      n_status = dplyr::n_distinct(status),
      n_period = dplyr::n_distinct(period),
      n_date   = dplyr::n_distinct(sampling_event_start_date),
      .groups  = "drop"
    ) %>%
    dplyr::filter(n_status > 1 | n_period > 1 | n_date > 1)

  if (nrow(event_check) > 0) {
    print(as.data.frame(event_check))
    stop(
      nrow(event_check),
      " site x sampling event combination(s) carry more than one ",
      "status, period or start date. Resolve these in the metadata ",
      "before running the multivariate analysis."
    )
  }

  # ----------------------------------------------------------
  # Counts: MaxN per deployment per taxon
  # ----------------------------------------------------------

  count_raw <- readRDS(count_rds) %>%
    dplyr::mutate(
      campaignid = as.character(campaignid),
      sample     = as.character(sample)
    ) %>%
    # The taxonomy harmonisations script 03 applies to bruv_count.
    # These are name fixes, not filters: without them the same
    # animal would enter the assemblage matrix under two names.
    dplyr::mutate(
      genus = dplyr::if_else(genus %in% "Plagusia", "Guinusia", genus),
      genus = dplyr::if_else(genus %in% "Pelates", "Helotes", genus),
      species = dplyr::if_else(
        species %in% "georgianus" & genus %in% "Pseudocaranx",
        "spp",
        species
      )
    ) %>%
    dplyr::mutate(
      transect   = paste(campaignid, sample, sep = "_"),
      scientific = paste(family, genus, species)
    ) %>%
    dplyr::semi_join(deployments, by = "transect")

  # After harmonisation two rows can describe the same taxon in the
  # same deployment. They are separate records of separate
  # individuals, so they are summed, matching how RLS/03 summarises
  # its counts before completing them.
  duplicate_records <- count_raw %>%
    dplyr::count(transect, scientific, name = "n_rows") %>%
    dplyr::filter(n_rows > 1)

  if (nrow(duplicate_records) > 0) {
    message(
      "NOTE: ", nrow(duplicate_records),
      " deployment x taxon combination(s) have more than one count ",
      "row and have been summed. Listed in BRUV_duplicate_count_rows.csv."
    )
    safe_write_csv(
      duplicate_records,
      file.path(output_dir, "BRUV_duplicate_count_rows.csv")
    )
  }

  count_summary <- count_raw %>%
    dplyr::group_by(transect, family, genus, species, scientific) %>%
    dplyr::summarise(total = max(count, na.rm = TRUE), .groups = "drop")

  # ----------------------------------------------------------
  # Zero fill: every deployment x every taxon
  #
  # Mirrors the tidyr::crossing() + replace_na(0) step that builds
  # the RLS *_complete_count.rds files. Without it, the average
  # across deployments below would be an average over presences.
  # ----------------------------------------------------------

  species_list <- count_summary %>%
    dplyr::distinct(family, genus, species, scientific)

  complete_count <- tidyr::crossing(
    deployments %>% dplyr::distinct(transect),
    species_list
  ) %>%
    dplyr::left_join(
      count_summary,
      by = c("transect", "family", "genus", "species", "scientific")
    ) %>%
    dplyr::mutate(total = tidyr::replace_na(total, 0)) %>%
    dplyr::left_join(deployments, by = "transect")

  stopifnot(
    nrow(complete_count) ==
      dplyr::n_distinct(complete_count$transect) *
      dplyr::n_distinct(complete_count$scientific)
  )

  empty_deployments <- setdiff(
    deployments$transect,
    count_summary$transect
  )

  message(
    "BRUV counts: ", nrow(species_list), " taxa; ",
    length(empty_deployments),
    " deployment(s) recorded nothing and are carried as all-zero rows."
  )

  complete_count %>%
    dplyr::select(
      location, site_name, site_code, status, period,
      transect, sampling_event, sampling_event_start_date,
      campaignid, sample,
      family, genus, species, scientific, total
    )
}


# ============================================================
# BRUV PIPELINE
#
# The same sequence as script 13's run_dataset_pipeline(), minus
# its RLS-specific loader (that function reads two .rds files and
# joins them by site_code; BRUV data is assembled above instead).
# Every analysis call below is script 13's own function.
# ============================================================

run_bruv_pipeline <- function(
    raw_data,
    output_dir     = BRUV_OUTPUT_DIR,
    dataset_prefix = BRUV_DATASET_PREFIX
) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  locations <- raw_data %>%
    dplyr::filter(!is.na(location)) %>%
    dplyr::distinct(location) %>%
    dplyr::arrange(location) %>%
    dplyr::pull(location)

  message("=== ", dataset_prefix, ": ", length(locations), " location(s) found ===")

  # Unconstrained ordination, saved for every location.
  for (loc in locations) {
    tryCatch(
      make_status_pco_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (status PCO) for ", loc, ": ", conditionMessage(e))
    )
  }

  # Period (+ status:period interaction) - within-site test.
  period_list <- list()

  for (loc in locations) {
    result <- tryCatch(
      test_period_effect(raw_data, loc),
      error = function(e) {
        message("ERROR (period PERMANOVA) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) period_list[[loc]] <- result
  }

  period_results <- if (length(period_list) > 0) {
    dplyr::bind_rows(period_list)
  } else {
    empty_period_results()
  }

  safe_write_csv(
    period_results,
    file.path(output_dir, paste0(dataset_prefix, "_period_PERMANOVA_results.csv"))
  )

  # Status - whole-site permutation test.
  status_list <- list()

  for (loc in locations) {
    result <- tryCatch(
      test_status_effect(raw_data, loc),
      error = function(e) {
        message("ERROR (status PERMANOVA) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) status_list[[loc]] <- result
  }

  status_results <- if (length(status_list) > 0) {
    dplyr::bind_rows(status_list)
  } else {
    empty_status_results()
  }

  safe_write_csv(
    status_results,
    file.path(output_dir, paste0(dataset_prefix, "_status_PERMANOVA_results.csv"))
  )

  # CAP plots, each gated on its own test, exactly as for RLS.
  significant_period_locations <- period_results %>%
    dplyr::filter(term == "period", p_value <= 0.05) %>%
    dplyr::pull(location)

  for (loc in significant_period_locations) {
    tryCatch(
      make_cap_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (period CAP) for ", loc, ": ", conditionMessage(e))
    )
  }

  significant_status_locations <- status_results %>%
    dplyr::filter(p_value <= 0.05) %>%
    dplyr::pull(location)

  for (loc in significant_status_locations) {
    status_permanova_p <- status_results %>%
      dplyr::filter(location == loc) %>%
      dplyr::slice(1) %>%
      dplyr::pull(p_value)

    tryCatch(
      make_status_cap_plot(
        raw_data, loc, output_dir, dataset_prefix,
        status_permanova_p = status_permanova_p
      ),
      error = function(e) message("ERROR (status CAP) for ", loc, ": ", conditionMessage(e))
    )
  }

  for (loc in significant_status_locations) {
    tryCatch(
      make_period_status_cap_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (period vs status CAP) for ", loc, ": ", conditionMessage(e))
    )
  }

  # ----------------------------------------------------------
  # Dashboard export, built for EVERY location including the ones
  # that could not be tested, so the app can show the ordination
  # with its p-value beside it or explain why there is none.
  # ----------------------------------------------------------
  message("--- ", dataset_prefix, ": building dashboard export tables ---")

  export_list <- list()

  for (loc in locations) {
    export_list[[loc]] <- tryCatch(
      export_location_multivariate(raw_data, loc, dataset_prefix),
      error = function(e) {
        message("ERROR (dashboard export) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
  }

  app_export <- combine_location_exports(export_list)

  invisible(
    list(
      dataset    = dataset_prefix,
      period     = period_results,
      status     = status_results,
      app_export = app_export
    )
  )
}


# ============================================================
# RUN
# ============================================================

bruv_raw_data <- build_bruv_raw_data()

bruv_results <- tryCatch(
  run_bruv_pipeline(bruv_raw_data),
  error = function(e) {
    message("FATAL DATASET ERROR: ", conditionMessage(e))
    NULL
  }
)

# Same seven tidy CSVs as the RLS export, same schema, written to
# their own folder so nothing the RLS side produced is touched.
bruv_app_multivariate_export <- write_app_multivariate_exports(
  list(bruv_results),
  output_dir = BRUV_APP_EXPORT_DIR
)


# ============================================================
# QUICK CHECKS
#
# Run these after the script finishes rather than trusting the
# absence of errors.
# ============================================================

# How many site x sampling events went into each location, and how
# many sites and deployments they came from.
bruv_design_summary <- bruv_raw_data %>%
  dplyr::distinct(
    location, site_name, status, period,
    sampling_event, sampling_event_start_date, transect
  ) %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(
    n_deployments  = dplyr::n_distinct(transect),
    n_sites        = dplyr::n_distinct(site_name),
    n_site_events  = dplyr::n_distinct(paste(site_name, sampling_event)),
    n_statuses     = dplyr::n_distinct(status),
    n_periods      = dplyr::n_distinct(period),
    sites_in_both_periods = sum(
      tapply(period, site_name, dplyr::n_distinct) > 1
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_site_events))

print(as.data.frame(bruv_design_summary), row.names = FALSE)

safe_write_csv(
  bruv_design_summary,
  file.path(BRUV_OUTPUT_DIR, "BRUV_design_summary.csv")
)

# Which ordinations could be produced, and the recorded reason
# where one could not.
if (!is.null(bruv_results)) {
  bruv_results$app_export$meta %>%
    dplyr::count(ordination, available) %>%
    as.data.frame() %>%
    print(row.names = FALSE)

  bruv_results$app_export$meta %>%
    dplyr::filter(!available) %>%
    dplyr::select(location, ordination, reason) %>%
    as.data.frame() %>%
    print(row.names = FALSE)
}
