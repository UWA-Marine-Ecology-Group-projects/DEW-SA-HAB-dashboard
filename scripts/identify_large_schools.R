# Identify fish species forming large schools (>50 individuals of the same
# species recorded together) in more than one transect.
#
# Uses the already-cleaned RLS count data (output of "Format and Clean RLS
# data.R"): data/tidy/rls_m1_count_and_length.rds (Method 1 - diver-estimated
# fish counts/sizes along the full transect) and
# data/tidy/rls_m2_fish_count_and_length.rds (Method 2 - cryptic fish in 1x1m
# blocks). Invertebrates are excluded since the question is about fish.

library(dplyr)
library(tidyr)
library(readr)

stopifnot(
  "Run this from the project root (DEW-SA-HAB-dashboard) - data/tidy/... not found" =
    file.exists("data/tidy/rls_m1_count_and_length.rds")
)

m1 <- read_rds("data/tidy/rls_m1_count_and_length.rds")
m2_fish <- read_rds("data/tidy/rls_m2_fish_count_and_length.rds")

cat("Columns available in m1: ", paste(names(m1), collapse = ", "), "\n\n")
cat("Columns available in m2_fish: ", paste(names(m2_fish), collapse = ", "), "\n\n")

# A "transect" is uniquely identified by (survey_id, transect label) - combine
# them defensively in case the raw "transect" label is reused across surveys.
# Sum counts per species within a transect (a school spanning multiple blocks
# of the same transect should be counted together).
summarise_schools <- function(df, method_label) {
  df %>%
    dplyr::mutate(transect_uid = paste(survey_id, transect)) %>%
    dplyr::group_by(transect_uid, survey_id, site_name, survey_date, scientific, portal_name) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(method = method_label)
}

m1_agg <- summarise_schools(m1, "M1_fish")
m2_fish_agg <- summarise_schools(m2_fish, "M2_cryptic_fish")

combined <- dplyr::bind_rows(m1_agg, m2_fish_agg)

# Large school threshold: more than 50 individuals of the same species
# counted together in one transect
large_schools <- combined %>%
  dplyr::filter(total > 50) %>%
  dplyr::arrange(scientific, dplyr::desc(total))

# Species that formed a large school (>50) in MORE THAN ONE transect
result <- large_schools %>%
  dplyr::group_by(scientific, portal_name, method) %>%
  dplyr::summarise(
    n_transects_with_large_school = dplyr::n_distinct(transect_uid),
    max_count_in_a_transect = max(total),
    mean_count_across_qualifying_transects = round(mean(total), 1),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_transects_with_large_school > 1) %>%
  dplyr::arrange(dplyr::desc(n_transects_with_large_school), dplyr::desc(max_count_in_a_transect))

dir.create("scripts", showWarnings = FALSE)
readr::write_csv(result, "scripts/large_school_species_summary.csv")
readr::write_csv(large_schools, "scripts/large_school_transect_detail.csv")

cat("\n=== Species forming schools of >50 individuals in MORE THAN ONE transect ===\n")
print(result, n = Inf)

cat("\nWrote scripts/large_school_species_summary.csv and scripts/large_school_transect_detail.csv\n")
cat("DONE\n")
