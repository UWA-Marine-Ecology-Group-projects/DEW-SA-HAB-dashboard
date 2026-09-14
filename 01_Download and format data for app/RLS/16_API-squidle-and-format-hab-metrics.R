###Example for extracting data from individual datasets in SQ+ including segments from SAMBot
##Written by jacquomo.monk@utas.edu.au
##Date 15/07/2024
##Modified 10/09/2026: each annotation_set_id is now cached to disk as soon as it's
##downloaded and tidied. If the script errors partway through, just re-run it --
##anything already cached is skipped, so only the missing/failed ids get re-fetched.
##Modified 10/09/2026 (2): added morphospecies richness (level_3) and % cover of
##benthic habitats (level_2) metrics - see the two new sections below benthos_clean.
##Also fixed benthos_final's select() to keep `opcode`, which benthos_clean (and the
##new metrics) group by but which was being silently dropped.
##Modified 14/09/2026: benthos_final now drops the underlying individual wherever an
##"Epiphyte"-tagged annotation shares the same image + x/y, so each point is scored
##once (RLS-standard). point_x/point_y are kept in the select() so this can be done.

##Clean up environment
rm(list=ls())
Sys.setenv(CURL_SSL_BACKEND = 'openssl')

##Load required packages
# if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if (!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
# if (!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
#install.packages('remotes')
library('remotes')
options(timeout=9999999)

# remotes::install_github("GlobalArchiveManual/CheckEM")
##Loading libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(CheckEM)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
#
#install.packages("devtools")
# devtools::install_github("sajessop/SQAPI")
library(SQAPI)

# Step 1: Create an instance of SQAPI -----
##Load API token
api <- SQAPI$new()

# Get SA annotation sets ----
ids <- c(19311, 19340, 19341, 19342, 19529, 19605,
         19606, 19607, 19608, 19609, 19685, 19678,
         19731, 19732, 19736) # 15 datasets

# ================================================================
# Step 2: Fetch + tidy each dataset, caching each one to disk as it finishes
# ================================================================
# WARNING - this takes a while to run the FIRST time. On any re-run, ids that
# already have a cached .rds file below are skipped entirely -- only ids that
# are missing (never fetched, or failed last time) get downloaded again.

cache_dir <- "data/raw/benthos_annotation_sets"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

cache_path <- function(annotation_set_id) {
  file.path(cache_dir, paste0("annotation_set_", annotation_set_id, ".rds"))
}

fetch_annotation_set <- function(annotation_set_id) {
  req <- export(
    api = api,
    endpoint = paste0("api/annotation_set/", annotation_set_id, "/export"),
    template = "dataframe.csv"
  )
  
  pars_export <- parse_api(req)
  df <- pars_export$objects
  
  df %>%
    jsonlite::flatten(recursive = TRUE) %>%
    clean_names() %>%
    dplyr::rename(campaignid = point_media_deployment_campaign_key,
                  uuid = label_uuid) %>%
    dplyr::mutate(annotation_set_id = annotation_set_id) %>%
    identity()
}

failed_ids <- c()

for (annotation_set_id in ids) {
  
  out_file <- cache_path(annotation_set_id)
  
  if (file.exists(out_file)) {
    message("Skipping annotation_set_id ", annotation_set_id, " - already cached at ", out_file)
    next
  }
  
  message("Fetching annotation_set_id: ", annotation_set_id)
  
  result <- tryCatch({
    fetch_annotation_set(annotation_set_id)
  }, error = function(e) {
    message("  FAILED for id ", annotation_set_id, ": ", conditionMessage(e))
    failed_ids <<- c(failed_ids, annotation_set_id)
    NULL
  })
  
  if (!is.null(result)) {
    saveRDS(result, out_file)  # <-- saved immediately, so a later crash doesn't lose this dataset
    message("  Saved: ", out_file)
  }
}

if (length(failed_ids) > 0) {
  message("The following annotation_set_ids failed this run: ", paste(failed_ids, collapse = ", "))
  message("Re-run this script to retry them - datasets already cached will be skipped.")
}

# --- Load every cached dataset (this run's + any from earlier runs) and merge ----
cached_files <- cache_path(ids)
have_cache <- file.exists(cached_files)

if (!all(have_cache)) {
  warning("No cached data yet for annotation_set_id(s): ",
          paste(ids[!have_cache], collapse = ", "),
          ". Re-run the script to fetch them before continuing (rest of script will proceed with what's available).")
}

all_benthos_raw <- cached_files[have_cache] %>%
  purrr::set_names(ids[have_cache]) %>%
  purrr::map(readRDS)

# --- Merge everything together ----
benthos <- dplyr::bind_rows(all_benthos_raw) %>%
  glimpse()

# ================================================================
# Everything below runs ONCE, on the merged dataset
# ================================================================

# Words that would falsely look like "Genus species" (Title Case + lowercase word)
# but are actually just CATAMI descriptor terms, not real taxa.
# Extend this list as new data reveals more false positives.
non_species_words <- c("algae", "turf", "matrix", "understory",
                       "gravel", "mud", "sand", "silt", "fragile", 
                       "forms", "stars", "calcareous", "urchins", 
                       "origin", "fishes", "anemones", "worms", "point")

is_species_name <- function(x) {
  words <- str_split(str_trim(x), "\\s+")[[1]]
  if (length(words) != 2) return(FALSE)
  
  genus_ok   <- str_detect(words[1], "^[A-Z][a-z]+$")
  epithet_ok <- words[2] == "spp" || str_detect(words[2], "^[a-z]+$")
  
  genus_ok && epithet_ok && !(tolower(words[2]) %in% non_species_words)
}

# --- Split lineage labels into level_ columns + species ----
benthos_split <- benthos %>%
  mutate(
    label_clean = str_trim(label_lineage_names),
    label_clean = str_remove(label_clean, "^[0-9]+(\\.[0-9]+)*\\s*"),
    segments    = str_split(label_clean, "\\s*>\\s*"),
    last_seg    = map_chr(segments, ~ str_trim(.x[length(.x)])),
    species     = if_else(map_lgl(last_seg, is_species_name), last_seg, NA_character_),
    segments    = map2(segments, species, ~ if (!is.na(.y)) .x[-length(.x)] else .x))

max_depth <- max(lengths(benthos_split$segments))

benthos_final <- benthos_split %>%
  mutate(segments = map(segments, ~ { length(.x) <- max_depth; .x })) %>%
  unnest_wider(segments, names_sep = "_") %>%
  rename_with(~ str_replace(., "segments_", "level_"),
              starts_with("level_") | starts_with("segments_")) %>%
  select(-label_clean, -last_seg) %>%
  # NOTE (10/09/2026): added `opcode` to this select() - benthos_clean below
  # (and the two per-opcode metrics further down) group by opcode, but it was
  # being dropped here, which would error as soon as those blocks ran.
  dplyr::select(point_media_key, point_media_deployment_name, annotation_set_id, annotation_set_name,
                point_id, point_x, point_y, point_pose_lon, point_pose_lat,
                point_pose_timestamp, starts_with("level"), species, tag_names) %>%
  ungroup() %>%
  dplyr::filter(level_1 %in% c("Biota", "Physical")) %>%
  dplyr::filter(!level_2 %in% c("Bioturbation", "Fishes", "General Unknown Biology", NA)) %>% # TODO need to decide if Matrix is kept as substrate or macroalgae?
  # --- Epiphytes: keep the epiphyte, drop the individual underneath it ----
  # Epiphytes are tagged "Epiphyte" in Squidle and share the x/y of the point they are
  # growing on, which is why some quadrats end up with more than 20 annotations. Within
  # each image x point, if anything there is tagged as an epiphyte, keep only the
  # epiphyte row(s) and drop the underlying individual - so each point is scored once,
  # RLS-style.
  dplyr::mutate(is_epiphyte = str_detect(replace_na(tag_names, ""), regex("epiphyte", ignore_case = TRUE))) %>%
  dplyr::group_by(point_media_key, point_x, point_y) %>%
  dplyr::filter(is_epiphyte | !any(is_epiphyte)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-is_epiphyte) %>%
  glimpse()

names(benthos_final)

# --- Sanity check: any x/y still scored more than once? ----
# These are points where the epiphyte tag was missed, or where BOTH annotations at a
# point were tagged "Epiphyte" - worth chasing up in Squidle rather than dropping blind.
tagged_more_than_once <- benthos_final %>%
  dplyr::add_count(annotation_set_id, annotation_set_name, point_media_deployment_name, point_media_key, point_x, point_y, name = "n_at_point") %>%
  dplyr::filter(n_at_point > 1) %>%
  dplyr::arrange(point_media_key, point_x, point_y) %>%
  dplyr::select(annotation_set_id, annotation_set_name, point_media_deployment_name, point_media_key, point_x, point_y, tag_names, starts_with("level"), species)

write_csv(tagged_more_than_once, "squidle_points_tagged_more_than_once.csv")

unique(benthos_final$level_1)
unique(benthos_final$level_2) %>% sort()
unique(benthos_final$level_3) %>% sort()
unique(benthos_final$level_4) %>% sort()
unique(benthos_final$level_5) %>% sort()
unique(benthos_final$level_6) %>% sort()
unique(benthos_final$species) %>% sort() # check that this looks ok
unique(benthos_final$tag_names) %>% sort()
# 
# unique_morphospecies <- benthos_final %>%
#   mutate(n = 1) %>%
#   group_by(level_1, level_2, level_3, level_4, level_5, level_6, species) %>%
#   dplyr::summarise(number = sum(n))
# 
# write_csv(unique_morphospecies, "unique_morphospecies.csv")

# Questions
# bellidilia undecimspinosa - need to fix
# why so many blank cells
# how to group annotations

# # --- Check for anything that didn't match ----
# # Should ideally all be "open water"/unscorable-type labels - if not, chase down the mapping
# benthos_missing <- benthos %>%
#   filter(is.na(caab_code)) %>%
#   distinct(label_lineage_names)
# 
# benthos_missing2 <- benthos_final %>%
#   filter(is.na(level_2)) %>%
#   distinct(across(starts_with("level")))
# 
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
# # Richness = the number of distinct level_3 categories recorded within each
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
# # richness numbers below.
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