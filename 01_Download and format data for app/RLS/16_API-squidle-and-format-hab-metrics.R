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
##"Epiphyte"-tagged annotation sits on the same point, so each point is scored once
##(RLS-standard).
##Modified 15/09/2026: where the Epiphyte tag is missing from both annotations on a
##point (or applied to both), the epiphyte is now inferred from growth form - see
##`epiphyte_rank`. Both the guessed points and the ones no rule could resolve are
##written out to data/tidy/ for checking/fixing in Squidle.
##Modified 15/09/2026 (2): group by `point_id` rather than image + x/y. An epiphyte and
##the individual under it are two annotations on ONE point, so point_id is the exact
##key (checked: it never repeats across annotation sets). This also avoids lumping
##together the ~190 annotations that have no x/y (point_has_xy = FALSE).
##Modified 15/09/2026 (3): added Rule 3 - annotations left on a point that are identical
##down to level_3 are collapsed to a single row (level_3 is as deep as this analysis
##goes). Columns below level_3 are set to NA where the collapsed rows disagreed.
##Modified 15/09/2026 (4): added Rule 4 - anything still doubled up after that defaults
##to the first annotation, so benthos_final is guaranteed one row per point. Those points
##are listed in data/tidy/epiphyte_points_unresolved.csv, as the pick is arbitrary.
##Modified 15/09/2026 (5): split point_media_deployment_name into survey_id/site_code/date
##and added a naming check against rls_dive_sites.csv - format, valid site code, and
##whether the images actually sit at that site on that date. Problems (none at present)
##go to data/tidy/deployment_name_problems.csv.
##Modified 15/09/2026 (6): moved that split and check up to sit directly on `benthos`, so
##the deployment and coverage checks run on the RAW annotations before any label
##filtering and nothing they report is an artefact of what we chose to drop.
##Modified 15/09/2026 (7): those checks are now one `q_*` dataframe per question in the
##query list sent to the data owner, each written to data/tidy/ under the same name:
##  q_a3_photos_scored_twice               489 photos scored in two annotation sets
##  q_a4_photos_with_extra_points            4 photos carrying a second set of points
##  q_b1_b2_photos_named_after_another_site 66 photos named after a different site
##  q_b3_photos_listed_under_two_transects  10 files uploaded twice, under two transects
##  q_d1_images_per_transect               410 transects, photos scored in each
##Photos are grouped on `point_media_id`, NOT `point_media_key` - the key is the filename
##and is not unique, which is question B3 itself.

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

sa_sites <- read_rds("data/tidy/sa_sites.rds") # Made in script 2

# Step 1: Create an instance of SQAPI -----
##Load API token
# api <- SQAPI$new()

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
# Split point_media_deployment_name into survey_id / site_code / date
# ================================================================
# The format is <9-digit survey_id>_<RLS site code>_<YYYY-MM-DD>,
# e.g. "923401321_GSV131_2022-03-23". `survey_id` is the transect/deployment.
# This is done up here because the coverage and naming checks below need it, and both
# of those have to run on the RAW annotations - before any label filtering, so that
# nothing they report is an artefact of what we chose to drop.
# too_few/too_many are set so a badly formed name does NOT error here - it falls through
# to `deployment_name_problems` below, which is where you want to see it.
benthos <- benthos %>%
  tidyr::separate_wider_delim(
    point_media_deployment_name,
    delim       = "_",
    names       = c("survey_id", "site_code", "date"),
    too_few     = "align_start",
    too_many    = "merge",
    cols_remove = FALSE) %>%
  dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))  # NA if not a valid date

# ================================================================
# Data queries for the annotators - one dataframe per question
# ================================================================
# Each `q_*` object below is the evidence behind one survey_ided question in the query list
# sent to the data owner, and is written to data/tidy/ under the same name. Rerun this
# block after any fix at source to see what is left.
#
# NOTE on identifying a photo: `point_media_key` is the FILENAME and is not unique -
# the same file can be uploaded twice as two separate records (question B3).
# `point_media_id` is the actual image record and is what the per-photo checks group by.

dir.create("data/tidy", recursive = TRUE, showWarnings = FALSE)

# one row per photo, used by A3 and A4 below
photo_summary <- benthos %>%
  dplyr::group_by(point_media_id, point_media_key, survey_id, site_code, date) %>%
  dplyr::summarise(n_points          = dplyr::n(),
                   n_positions       = dplyr::n_distinct(paste(point_x, point_y)),
                   n_annotation_sets = dplyr::n_distinct(annotation_set_id),
                   annotation_sets   = paste(sort(unique(annotation_set_name)), collapse = " | "),
                   annotators        = paste(sort(unique(user_full_name)), collapse = " | "),
                   last_updated      = max(as.Date(substr(updated_at, 1, 10))),
                   .groups = "drop")

# --- A3: photos scored in more than one annotation set ----
# The same photo scored twice, usually by two people. The two scorings use different
# point positions, so nothing downstream merges them and the photo counts twice.
q_a3_photos_scored_twice <- photo_summary %>%
  dplyr::filter(n_annotation_sets > 1) %>%
  dplyr::arrange(site_code, date, survey_id, point_media_key)

write_csv(q_a3_photos_scored_twice, "data/issues/q_photos_scored_twice.csv")

# --- A4: photos carrying a second set of points ----
# A photo with a heavy epiphyte load also has more than 20 points, but those extra points
# SHARE an x/y with the point beneath them. A second set of points has every point at its
# own position - so `n_positions == n_points` is what separates the two.
q_a4_photos_with_extra_points <- photo_summary %>%
  dplyr::filter(n_annotation_sets == 1,
                n_points > 20,
                n_positions == n_points) %>%
  dplyr::arrange(site_code, date, survey_id, point_media_key)

write_csv(q_a4_photos_with_extra_points, "data/issues/q_photos_with_extra_points.csv")

# --- B1 and B2: photos whose filename names a different site ----
# The site code at the start of the filename is only treated as a disagreement when it is
# a REAL site code from rls_dive_sites.csv. That avoids the two false positives you would
# otherwise get: zero-padded codes (GSV05 for GSV5) and site names that run straight into
# the code (GSV26 + "2ndValleyBoatShed" reading as "GSV262").
# sa_sites <- readr::read_csv("rls_dive_sites.csv", show_col_types = FALSE)

q_b1_b2_photos_named_after_another_site <- benthos %>%
  dplyr::distinct(point_media_id, point_media_key, point_media_deployment_name,
                  survey_id, site_code, date) %>%
  dplyr::mutate(site_in_filename = str_extract(point_media_key, "^[A-Z]{2,4}[0-9]{1,3}")) %>%
  dplyr::filter(site_in_filename %in% sa_sites$site_code,
                site_in_filename != site_code) %>%
  dplyr::left_join(dplyr::select(sa_sites, site_code, site_name_in_metadata = site_name_lookup),
                   by = "site_code") %>%
  dplyr::left_join(dplyr::select(sa_sites, site_in_filename = site_code,
                                 site_name_in_filename = site_name_lookup),
                   by = "site_in_filename") %>%
  dplyr::arrange(survey_id, point_media_key)

write_csv(q_b1_b2_photos_named_after_another_site,
          "data/issues/q_photos_named_after_another_site.csv")

# --- B3: the same photo file present twice, under two transects ----
# Two separate image records for one filename, each scored with its own points.
q_b3_photos_listed_under_two_transects <- benthos %>%
  dplyr::distinct(point_media_key, point_media_id, survey_id, site_code, date, annotation_set_name) %>%
  dplyr::add_count(point_media_key, name = "n_entries") %>%
  dplyr::filter(n_entries > 1) %>%
  dplyr::arrange(point_media_key, survey_id)

write_csv(q_b3_photos_listed_under_two_transects,
          "data/issues/q_photos_listed_under_two_transects.csv")

# ================================================================
# D1-D3: how many photos per transect
# ================================================================
# An RLS photo-quadrat transect is nominally 20 photos x 20 points.
expected_images  <- 20
image_tolerance  <- 5    # flag transects outside 15-25 photos - tune to taste

q_d1_images_per_transect <- benthos %>%
  dplyr::group_by(site_code, date, survey_id) %>%
  dplyr::summarise(n_images        = dplyr::n_distinct(point_media_id),
                   n_points        = dplyr::n(),
                   annotation_sets = paste(sort(unique(annotation_set_name)), collapse = " | "),
                   .groups         = "drop") %>%
  dplyr::mutate(flag = dplyr::case_when(
    n_images < expected_images - image_tolerance ~ "fewer photos than expected",
    n_images > expected_images + image_tolerance ~ "more photos than expected",
    TRUE                                         ~ NA_character_)) %>%
  dplyr::arrange(n_images)

write_csv(q_d1_images_per_transect, "data/issues/q_images_per_transect.csv")

# the full spread - worth eyeballing rather than trusting the flag alone
q_d1_images_per_transect %>% dplyr::count(n_images) %>% print(n = Inf)

# rolled up per site x date. Most site x date combinations are several transects, so the
# ~20 is per transect, not per survey - this shows how many transects each survey got.
q_d1_images_per_site_date <- q_d1_images_per_transect %>%
  dplyr::group_by(site_code, date) %>%
  dplyr::summarise(n_transects = dplyr::n(),
                   n_images    = sum(n_images),
                   .groups     = "drop") %>%
  dplyr::arrange(n_images)

write_csv(q_d1_images_per_site_date, "data/tidy/q_d1_images_per_site_date.csv")

# --- summary of everything above ----
message("\n--- Queries for the annotators -------------------------------")
message("A3  photos scored in two annotation sets : ", nrow(q_a3_photos_scored_twice))
message("A4  photos with a second set of points   : ", nrow(q_a4_photos_with_extra_points))
message("B1/B2 photos named after another site    : ", nrow(q_b1_b2_photos_named_after_another_site),
        " in ", dplyr::n_distinct(q_b1_b2_photos_named_after_another_site$survey_id), " transects")
message("B3  photos listed under two transects    : ",
        dplyr::n_distinct(q_b3_photos_listed_under_two_transects$point_media_key),
        " files, ", nrow(q_b3_photos_listed_under_two_transects), " entries")
message("D1  transects                            : ", nrow(q_d1_images_per_transect),
        " | median photos ", stats::median(q_d1_images_per_transect$n_images),
        " | flagged ", sum(!is.na(q_d1_images_per_transect$flag)))
message("--------------------------------------------------------------\n")


# ================================================================
# Check every deployment was named correctly
# ================================================================
# Five things have to hold for a name to be trusted: the three parts are there, the
# survey_id looks like a deployment survey_id, the date parses, the site code is a real RLS
# site, and the images actually sit at that site on that date. The last two are the ones
# worth having - a name can be perfectly formed and still point at the wrong site.
deployment_check <- benthos %>%   # sa_sites is read in the query block above
  dplyr::mutate(image_date = as.Date(substr(point_pose_timestamp, 1, 10))) %>%
  dplyr::group_by(point_media_deployment_name, survey_id, site_code, date) %>%
  dplyr::summarise(n_points        = dplyr::n(),
                   date_matches    = any(date == image_date, na.rm = TRUE),
                   image_dates     = paste(sort(unique(image_date)), collapse = ", "),
                   lat             = mean(point_pose_lat, na.rm = TRUE),
                   lon             = mean(point_pose_lon, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(sa_sites, by = "site_code") %>%
  # Flat-earth distance in km. Good to well under a percent over a few km at these
  # latitudes, which is all this needs to be - it is a sanity check, not navigation.
  dplyr::mutate(
    km_from_site = 111 * sqrt((lat - latitude)^2 +
                              ((lon - longitude) * cos(lat * pi / 180))^2),
    problem = dplyr::case_when(
      str_count(point_media_deployment_name, "_") != 2 ~ "name is not survey_id_site_date",
      !str_detect(survey_id, "^[0-9]{9}$")                ~ "survey_id is not 9 digits",
      is.na(date)                                      ~ "date is not a valid YYYY-MM-DD",
      is.na(latitude)                                  ~ "site code is not in rls_dive_sites.csv",
      !date_matches                                    ~ "name date matches no image timestamp",
      km_from_site > 2                                 ~ "images are >2 km from the named site",
      TRUE                                             ~ NA_character_))

deployment_name_problems <- deployment_check %>%
  dplyr::filter(!is.na(problem)) %>%
  dplyr::arrange(problem, point_media_deployment_name) %>%
  dplyr::select(point_media_deployment_name, problem, survey_id, site_code, date,
                image_dates, site_name, km_from_site, n_points)

write_csv(deployment_name_problems, "data/tidy/deployment_name_problems.csv")

message("Deployments checked: ", nrow(deployment_check),
        " | named incorrectly: ", nrow(deployment_name_problems))

# Worth a look even when nothing is flagged - if the largest distance starts creeping up,
# the 2 km threshold above is the thing to revisit.
deployment_check %>%
  dplyr::slice_max(km_from_site, n = 10, na_rm = TRUE) %>%
  dplyr::select(point_media_deployment_name, site_name, km_from_site)


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

# ================================================================
# Epiphyte handling - where a point carries more than one annotation, which do we keep?
# ================================================================
# An epiphyte is annotated on the SAME point (same `point_id`, same x/y) as the
# individual it is growing on, so a quadrat can end up with more than 20 annotations.
# We want to keep the epiphyte and drop the individual underneath it. Two rules,
# applied in order, within each point_id:
#
#   1. The "Epiphyte" tag, wherever it actually splits the point (the correct case).
#   2. Where the tag is missing from both, or has been put on both, fall back on the
#      growth form via `epiphyte_rank` below - the higher rank is taken as the epiphyte.
#
# Rule 2 is a stopgap until the tags are fixed in Squidle, and it is deliberately
# conservative: where both annotations sit in the same rank (e.g. two canopy-forming
# browns) nothing is dropped and the point is left in `epiphyte_points_unresolved`
# below for someone to look at.
epiphyte_forms   <- regex("filamentous|filiform|turfing algae|encrusting|articulated calcareous|calcareous",
                          ignore_case = TRUE)   # usually the epiphyte
understory_forms <- regex("dictyopteris|dictyotaceae", ignore_case = TRUE) # occasionally epiphytic
canopy_forms     <- regex("large canopy-forming", ignore_case = TRUE)      # usually the thing grown ON
matrix_forms     <- regex("matrix", ignore_case = TRUE)                    # treated as substrate

benthos_points <- benthos_split %>%
  mutate(segments = map(segments, ~ { length(.x) <- max_depth; .x })) %>%
  unnest_wider(segments, names_sep = "_") %>%
  rename_with(~ str_replace(., "segments_", "level_"),
              starts_with("level_") | starts_with("segments_")) %>%
  select(-label_clean, -last_seg) %>%
  # NOTE (10/09/2026): added `opcode` to this select() - benthos_clean below
  # (and the two per-opcode metrics further down) group by opcode, but it was
  # being dropped here, which would error as soon as those blocks ran.
  dplyr::select(point_media_key, point_media_deployment_name, survey_id, site_code, date,
                annotation_set_id, annotation_set_name,
                point_id, point_x, point_y, point_pose_lon, point_pose_lat,
                point_pose_timestamp, starts_with("level"), species, tag_names) %>%
  ungroup() %>%
  dplyr::filter(level_1 %in% c("Biota", "Physical")) %>%
  dplyr::filter(!level_2 %in% c("Bioturbation", "Fishes", "General Unknown Biology", NA)) %>% # TODO need to decide if Matrix is kept as substrate or macroalgae?
  # --- Rule 1: the "Epiphyte" tag, wherever it splits the point ----
  tidyr::unite("lineage", starts_with("level"), species, sep = " > ", na.rm = TRUE, remove = FALSE) %>%
  dplyr::mutate(
    is_epiphyte   = str_detect(replace_na(tag_names, ""), regex("epiphyte", ignore_case = TRUE)),
    epiphyte_rank = dplyr::case_when(
      level_1 == "Physical"                    ~ -3L,  # bare substrate - always underneath
      str_detect(lineage, matrix_forms)        ~ -2L,
      str_detect(lineage, epiphyte_forms)      ~  2L,
      str_detect(lineage, understory_forms)    ~  1L,
      str_detect(lineage, canopy_forms)        ~ -1L,
      TRUE                                     ~  0L)) %>%
  dplyr::group_by(point_id) %>%
  dplyr::filter(is_epiphyte | !any(is_epiphyte)) %>%
  dplyr::ungroup() %>%
  glimpse()

# --- FLAG: points where the epiphyte tag is missing or has been applied to both ----
# Rule 1 couldn't separate these, so they need fixing in Squidle. Rule 2 below makes a
# best guess in the meantime - this is the list of guesses being made.
epiphyte_points_to_check <- benthos_points %>%
  dplyr::add_count(point_id, name = "n_at_point") %>%
  dplyr::filter(n_at_point > 1) %>%
  dplyr::arrange(point_media_key, point_id, dplyr::desc(epiphyte_rank)) %>%
  dplyr::select(point_media_key, point_id, point_x, point_y, n_at_point, tag_names, epiphyte_rank, lineage)

dir.create("data/tidy", recursive = TRUE, showWarnings = FALSE)
write_csv(epiphyte_points_to_check, "data/tidy/epiphyte_points_to_check.csv")

message("Points with an unusable epiphyte tag: ",
        dplyr::n_distinct(epiphyte_points_to_check$point_id))

# --- Rule 2: fall back on growth form, then drop the underlying individual ----
# --- Rule 3: collapse whatever is left to level_3, which is as deep as we analyse ----
# Most of what Rule 2 can't separate is two annotations that are the SAME to level_3 and
# only differ below it (e.g. Cystophora expansa vs Phyllotricha decipens, both
# "Erect coarse branching"). At level_3 those are one and the same thing, so we keep a
# single row. Where the collapsed rows disagreed below level_3, the deeper columns are
# set to NA rather than arbitrarily keeping whichever sorted first - so benthos_resolved
# never carries a species attribution that was picked by accident.
benthos_resolved <- benthos_points %>%
  dplyr::group_by(point_id) %>%
  dplyr::filter(epiphyte_rank == max(epiphyte_rank)) %>%
  dplyr::group_by(point_id, level_1, level_2, level_3) %>%
  # across() can't see grouping variables, so starts_with("level") here already means
  # level_4 and below - level_1:level_3 are constant in the group and are left alone
  dplyr::mutate(dplyr::across(c(starts_with("level"), species, tag_names),
                              ~ if (dplyr::n_distinct(.x) > 1) NA_character_ else .x)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-lineage, -is_epiphyte, -epiphyte_rank) %>%
  glimpse()

# --- FLAG: points no rule could resolve ----
# These are genuinely different things at level_3 (e.g. Articulated calcareous vs
# Filamentous / filiform), sitting in the same epiphyte rank, so no rule can say which
# is the epiphyte. Rule 4 below just takes the first annotation, so this file is the
# record of every point where that coin was flipped - they still want fixing in Squidle.
epiphyte_points_unresolved <- benthos_resolved %>%
  dplyr::add_count(point_id, name = "n_at_point") %>%
  dplyr::filter(n_at_point > 1) %>%
  dplyr::arrange(point_media_key, point_id) %>%
  dplyr::select(point_media_key, point_id, point_x, point_y, n_at_point, tag_names,
                starts_with("level"), species)

write_csv(epiphyte_points_unresolved, "data/tidy/epiphyte_points_unresolved.csv")

message("Points resolved by taking the first annotation: ",
        dplyr::n_distinct(epiphyte_points_unresolved$point_id))

# --- Rule 4: whatever is still doubled up, default to the first annotation ----
# Note the order is the order SQ+ exported the annotations in, which is arbitrary - it is
# NOT reliably the epiphyte (checked across the tagged points: 61% first, 39% last, and
# it flips between annotation sets). This is a tie-break to guarantee one row per point,
# not an inference, which is why every point it touches is listed in the file above.
benthos_final <- benthos_resolved %>%
  dplyr::group_by(point_id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  glimpse()

names(benthos_final)

# --- Sanity check: should now be exactly one row per point ----
stopifnot(!any(duplicated(benthos_final$point_id)))

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
#   dplyr::summarise(survey_id = sum(n))
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