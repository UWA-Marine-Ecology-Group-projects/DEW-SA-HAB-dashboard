###Example for extracting data from individual datasets in SQ+ including segments from SAMBot
##Written by jacquomo.monk@utas.edu.au
##Date 15/07/2024

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

remotes::install_github("GlobalArchiveManual/CheckEM")
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
ids <- c(19311, 19340, 19341, 19342, 19529, 19605, 19606, 19607, 19608, 19609, 19685, 19678, 19731, 19732, 19736) # 15 datasets

# --- Loop purely to fetch + tidy each dataset ----
# WARNING - this takes a while to run
all_benthos_raw <- list()
failed_ids <- c()

for (annotation_set_id in ids) {
  
  message("Fetching annotation_set_id: ", annotation_set_id)
  
  result <- tryCatch({
    
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
    
  }, error = function(e) {
    message("  FAILED for id ", annotation_set_id, ": ", conditionMessage(e))
    failed_ids <<- c(failed_ids, annotation_set_id)
    NULL
  })
  
  if (!is.null(result)) {
    all_benthos_raw[[as.character(annotation_set_id)]] <- result
  }
}

if (length(failed_ids) > 0) {
  message("The following annotation_set_ids failed: ", paste(failed_ids, collapse = ", "))
}

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
                       "gravel", "mud", "sand", "silt")

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
    segments    = map2(segments, species, ~ if (!is.na(.y)) .x[-length(.x)] else .x)
  )

max_depth <- max(lengths(benthos_split$segments))

benthos_final <- benthos_split %>%
  mutate(segments = map(segments, ~ { length(.x) <- max_depth; .x })) %>%
  unnest_wider(segments, names_sep = "_") %>%
  rename_with(~ str_replace(., "segments_", "level_"),
              starts_with("level_") | starts_with("segments_")) %>%
  select(-label_clean, -last_seg) %>%
  dplyr::select(campaignid, annotation_set_id, point_media_deployment_name,
                point_id, point_pose_lon, point_pose_lat,
                point_pose_timestamp, starts_with("level"), species) %>%
  glimpse()

names(benthos_final)

unique(benthos_final$level_1)
unique(benthos_final$level_2)
unique(benthos_final$level_3)
unique(benthos_final$level_4)
unique(benthos_final$level_5)
unique(benthos_final$level_6)
unique(benthos_final$species) # check that this looks ok

# --- Check for anything that didn't match ----
# Should ideally all be "open water"/unscorable-type labels - if not, chase down the mapping
benthos_missing <- benthos %>%
  filter(is.na(caab_code)) %>%
  distinct(label_lineage_names)

benthos_missing2 <- benthos_final %>%
  filter(is.na(level_2)) %>%
  distinct(across(starts_with("level")))

# --- Summarise to counts per campaign/opcode/label ----
benthos_clean <- benthos_final %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(campaignid, opcode, across(starts_with("level")), species) %>%
  dplyr::summarise(count = sum(count), .groups = "drop") %>%
  dplyr::rename(period = opcode) %>%
  glimpse()

# --- Write out ----
write.csv(benthos_clean,
          file = "data/tidy/all_datasets_benthos-count.csv",
          row.names = FALSE)

# --- Sanity checks ----
benthos %>%
  count(opcode, name = "n_annotations") %>%
  filter(n_annotations != 80)

benthos %>%
  count(opcode, name = "n_annotations") %>%
  arrange(n_annotations)