##Loading libraries
library(CheckEM)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Read in data 
benthos <- read_rds("data/raw/rls_benthos_summarised.rds")


# # --- Summarise to counts per campaign/opcode/label ----
# benthos_clean <- benthos_final %>%
#   dplyr::mutate(count = 1) %>%
#   dplyr::group_by(campaignid, opcode, across(starts_with("level")), species) %>%
#   dplyr::summarise(count = sum(count), .groups = "drop") %>%
#   dplyr::rename(period = opcode) %>%
#   glimpse()
# 
# # --- Write out ----
# write.csv(benthos_clean,
#           file = "data/tidy/all_datasets_benthos-count.csv",
#           row.names = FALSE)
# 
# # ================================================================
# # Metric 1: Morphospecies richness (level_3, living things only)
# # ================================================================
# # Richness = the survey_id of distinct level_3 categories recorded within each
# # campaign/opcode (i.e. within each image/point-count sample), after dropping
# # anything that isn't a living thing - unknowns, rock, and sand.
# #
# # `non_living_level_3` is a regex, not a hardcoded list, so it catches minor
# # spelling variants (e.g. "Unscoreable" vs "Unscorable"). Check the two print
# # statements below against your actual level_3 values (printed earlier via
# # `unique(benthos_final$level_3)`) and extend the pattern if anything that
# # should be excluded slips through, e.g. "cobble", "boulder", "cryptic".
# 
# non_living_level_3 <- regex("unknown|unscor|rock|sand", ignore_case = TRUE)
# 
# # Sanity check - review what's being dropped vs kept before trusting the
# # richness survey_ids below.
# benthos_final %>%
#   filter(str_detect(level_3, non_living_level_3)) %>%
#   distinct(level_3) %>%
#   arrange(level_3) %>%
#   print(n = Inf)
# 
# benthos_final %>%
#   filter(!is.na(level_3), !str_detect(level_3, non_living_level_3)) %>%
#   distinct(level_3) %>%
#   arrange(level_3) %>%
#   print(n = Inf)
# 
# morphospecies_richness <- benthos_final %>%
#   filter(!is.na(level_3), !str_detect(level_3, non_living_level_3)) %>%
#   dplyr::group_by(campaignid, opcode) %>%
#   dplyr::summarise(morphospecies_richness = n_distinct(level_3), .groups = "drop") %>%
#   dplyr::rename(period = opcode) %>%
#   glimpse()
# 
# write_csv(morphospecies_richness, "data/tidy/morphospecies_richness.csv")
# 
# # ================================================================
# # Metric 2: % cover of benthic habitats (level_2)
# # ================================================================
# # "Benthic habitats" = every level_2 category, unfiltered - unlike Metric 1,
# # nothing is dropped here. Unmatched/NA level_2 values are kept as their own
# # category (rather than dropped) so percentages still sum to 100% per
# # campaign/opcode.
# 
# benthic_habitat_cover <- benthos_final %>%
#   dplyr::mutate(level_2 = tidyr::replace_na(level_2, "Unmatched/unscorable")) %>%
#   dplyr::group_by(campaignid, opcode) %>%
#   dplyr::mutate(n_points = dplyr::n()) %>%
#   dplyr::group_by(campaignid, opcode, level_2) %>%
#   dplyr::summarise(n_annotations = dplyr::n(),
#                    n_points      = dplyr::first(n_points),
#                    percent_cover = 100 * n_annotations / n_points,
#                    .groups = "drop") %>%
#   dplyr::rename(period = opcode) %>%
#   glimpse()
# 
# write_csv(benthic_habitat_cover, "data/tidy/benthic_habitat_percent_cover.csv")
# 
# # Wide version - one row per campaign/opcode, one column per habitat -
# # handy for stats (e.g. multivariate work) or a dashboard table.
# benthic_habitat_cover_wide <- benthic_habitat_cover %>%
#   dplyr::select(campaignid, period, level_2, percent_cover) %>%
#   tidyr::pivot_wider(names_from = level_2, values_from = percent_cover, values_fill = 0) %>%
#   glimpse()
# 
# write_csv(benthic_habitat_cover_wide, "data/tidy/benthic_habitat_percent_cover_wide.csv")
# 
# # Sanity check - % cover should sum to (very close to) 100% per campaign/opcode
# benthic_habitat_cover %>%
#   dplyr::group_by(campaignid, period) %>%
#   dplyr::summarise(total_percent = sum(percent_cover), .groups = "drop") %>%
#   dplyr::filter(abs(total_percent - 100) > 0.01)
# 
# # --- Sanity checks ----
# benthos %>%
#   count(opcode, name = "n_annotations") %>%
#   filter(n_annotations != 80)
# 
# benthos %>%
#   count(opcode, name = "n_annotations") %>%
#   arrange(n_annotations)