# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
# library(devtools)
library(dplyr)
library(googlesheets4)
# library(httr)
library(sf)
library(stringr)
library(tidyverse)

rls <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv", skip = 71) %>%
  dplyr::select(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth, family, reporting_name, size_class, total) %>%
  tidyr::separate(reporting_name, into = c("genus", "species"), remove = FALSE) %>%
  dplyr::rename(count = total, depth_m = depth) %>%
  glimpse()

rls_metadata <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv", skip = 71) %>%
  dplyr::rename(depth_m = depth, latitude_dd = latitude, longitude_dd = longitude) %>%
  glimpse()

# TODO Need to fix synonyms
synonyms_in_rls <- dplyr::left_join(rls, CheckEM::aus_synonyms)  %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(!is.na(genus_correct)) %>%
  dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
  dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
  dplyr::select('old name', 'new name') %>% # taken out sample
  dplyr::distinct()

rls_with_synonyms_changed <- dplyr::left_join(rls, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  mutate(family = str_replace_all(family, "[^[:alnum:]]", "")) %>%
  mutate(genus = str_replace_all(genus, "[^[:alnum:]]", "")) %>%
  mutate(species = str_replace_all(species, c("[^[:alnum:]]" = "", "pusillusdoriferus" = "pusillus doriferus"))) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::group_by(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth_m, family, genus, species, scientific) %>%
  dplyr::slice(which.max(count)) %>%
  ungroup()

# Species not in list ----
# TODO should check regions too
count_species_not_in_list <- rls_with_synonyms_changed %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct(family, genus, species) 

# TODO fix these up with synonyms!

rls_count <- rls_with_synonyms_changed %>%
  glimpse()

rls_length <- rls_with_synonyms_changed %>%
  dplyr::mutate(length_mm = 10 * size_class) %>%
  glimpse()

write.csv(rls_metadata, "data/raw/sa_metadata_rls.csv", row.names = FALSE)
saveRDS(rls_metadata, "data/raw/sa_metadata_rls.RDS")

write.csv(rls_count, "data/raw/sa_count_rls.csv", row.names = FALSE)
saveRDS(rls_count, "data/raw/sa_count_rls.RDS")

write.csv(rls_length, "data/raw/sa_length_rls.csv", row.names = FALSE)
saveRDS(rls_length, "data/raw/sa_length_rls.RDS")