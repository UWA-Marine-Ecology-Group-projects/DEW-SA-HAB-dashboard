#################################################################
# 04. Package the BRUV multivariate results for the app
#
# WHAT THIS SCRIPT DOES
#   Reads the seven tidy CSVs that
#   "modelling/03_bruv_multivariate_pco_and_caps.R" writes to
#   outputs/multivariate/bruv_app/ and saves them as one list,
#   `bruv_mv_data`, in app_data/bruv_multivariate_data.Rdata.
#
#   This is the BRUV counterpart of section 7b of
#   "RLS/15_combine_rls_data_for_app.R". The tables have exactly the same
#   names and columns as rls_data$multivariate_* (both come from the same
#   script 13 export code), so the app's "Community composition" card uses
#   one set of plotting code for both methods.
#
#   It is a separate file, rather than a section of script 03, because the
#   multivariate script needs script 03's output (sa_bruv_metadata.csv)
#   first - adding it to hab_data would make the two depend on each other.
#
# WHAT THIS SCRIPT DOES NOT DO
#   No statistics are run here. If the BRUV data or metadata change, re-run
#   script 03, then modelling/03, then this script.
#
# RUN ORDER
#   1. 01_Download and format data for app/03_Format BRUVS and RLS data for summaries and plots.R
#   2. 01_Download and format data for app/modelling/03_bruv_multivariate_pco_and_caps.R
#   3. this script
#   4. restart the app (bindCache is app-scoped, a browser reload is not enough)
#
# Run from the project root - every path is root-relative.
#################################################################

library(tidyverse)

multivariate_dir <- "outputs/multivariate/bruv_app"

read_multivariate_csv <- function(file_name) {

  path <- file.path(multivariate_dir, file_name)

  if (!file.exists(path)) {
    warning(
      "Missing ", path,
      " - the BRUVS 'Community composition' card will be empty. ",
      "Re-run modelling/03_bruv_multivariate_pco_and_caps.R to create it."
    )
    return(tibble::tibble())
  }

  readr::read_csv(path, show_col_types = FALSE)
}

bruv_mv_scores           <- read_multivariate_csv("multivariate_ordination_scores.csv")
bruv_mv_vectors          <- read_multivariate_csv("multivariate_species_vectors.csv")
bruv_mv_centroids        <- read_multivariate_csv("multivariate_site_centroids.csv")
bruv_mv_meta             <- read_multivariate_csv("multivariate_ordination_meta.csv")
bruv_mv_period_status    <- read_multivariate_csv("multivariate_period_status_scores.csv")
bruv_mv_permanova_period <- read_multivariate_csv("multivariate_permanova_period.csv")
bruv_mv_permanova_status <- read_multivariate_csv("multivariate_permanova_status.csv")

# Same fallback as script 15: a taxon whose name is not a plain
# "Genus species" (e.g. a genus-level "spp" record) gets no short label from
# script 13, so use the full scientific name rather than draw an unlabelled
# arrow.
if (nrow(bruv_mv_vectors) > 0 &&
    all(c("label", "scientific") %in% names(bruv_mv_vectors))) {

  n_missing_label <- sum(is.na(bruv_mv_vectors$label) | bruv_mv_vectors$label == "")

  if (n_missing_label > 0) {
    message(
      n_missing_label,
      " species vector(s) had no short label - using the full scientific name instead."
    )
  }

  bruv_mv_vectors <- bruv_mv_vectors %>%
    dplyr::mutate(
      label = dplyr::if_else(is.na(label) | label == "", scientific, label)
    )
}

# ---- Checks ----------------------------------------------------------------

# The app looks ordinations up by location name, and the BRUVS location
# dropdown uses reporting_name. Anything listed here would silently show
# "not available" in the app.
if (file.exists("app_data/hab_data.Rdata") && nrow(bruv_mv_meta) > 0) {

  load("app_data/hab_data.Rdata")

  app_locations <- hab_data$hab_combined_metadata %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(method %in% "BRUVs", !is.na(reporting_name)) %>%
    dplyr::distinct(reporting_name) %>%
    dplyr::pull(reporting_name) %>%
    sort()

  cat("\n--- BRUV multivariate location name check ---\n")
  cat("In the multivariate export but not in the app's BRUVS locations (should be empty):\n")
  print(setdiff(unique(bruv_mv_meta$location), app_locations))
  cat("BRUVS locations in the app with no multivariate export:\n")
  print(setdiff(app_locations, unique(bruv_mv_meta$location)))
}

if (nrow(bruv_mv_meta) > 0) {
  cat("\n--- BRUV ordinations available ---\n")
  print(dplyr::count(bruv_mv_meta, ordination, available, name = "n"))

  unavailable <- bruv_mv_meta %>% dplyr::filter(!available)
  if (nrow(unavailable) > 0) {
    cat("\nUnavailable (the app shows the recorded reason instead):\n")
    print(dplyr::select(unavailable, location, ordination, reason), n = Inf)
  }
}

# ---- Save ------------------------------------------------------------------

# Element names match rls_data$multivariate_* exactly - the app's helpers
# rely on that.
bruv_mv_data <- list(
  multivariate_scores           = bruv_mv_scores,
  multivariate_vectors          = bruv_mv_vectors,
  multivariate_centroids        = bruv_mv_centroids,
  multivariate_meta             = bruv_mv_meta,
  multivariate_period_status    = bruv_mv_period_status,
  multivariate_permanova_period = bruv_mv_permanova_period,
  multivariate_permanova_status = bruv_mv_permanova_status
)

dir.create("app_data", recursive = TRUE, showWarnings = FALSE)
save(bruv_mv_data, file = "app_data/bruv_multivariate_data.Rdata")

cat("\nSaved app_data/bruv_multivariate_data.Rdata\n")
purrr::iwalk(bruv_mv_data, function(x, name) cat(" -", name, ":", nrow(x), "rows\n"))
