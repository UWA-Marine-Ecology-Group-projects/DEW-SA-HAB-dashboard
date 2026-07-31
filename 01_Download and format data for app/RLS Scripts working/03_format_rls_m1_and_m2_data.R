#################################################################
# Format and Clean RLS M1 Fish data

# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
library(dplyr)
library(sf)
library(stringr)
library(readr)
library(tidyr)
library(googlesheets4)

# Read in DEW species list ----
dew_species <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing") %>%
  rename(portal_name = genus_species) %>%
  mutate(genus_species = portal_name)
2

# CheckEM life history list ----
lh <- CheckEM::australia_life_history

# Sites from DEW ----
sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

# Read in data ----
cols_to_remove <- c("ecoregion", "country", "area", "realm", "geom", 'visibility', "hour", "survey_latitude", 'survey_longitude', "diver", "method", "taxon", "program", "location", "site_code", "latitude", "longitude") # duplicated with metadata

## survey lists ----
sl_m1 <- readRDS("data/tidy/rls_m1_survey_list.rds")
sl_m2 <- readRDS("data/tidy/rls_m2_survey_list.rds")

## abundance and length ----
m1 <- read_csv("data/raw/RLS/ep_M1_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove)) %>%
  dplyr::filter(survey_id %in% unique(sl_m1$survey_id)) 

m2_fish <- read_csv("data/raw/RLS/ep_M2_cryptic_fish_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove))  %>%
  dplyr::filter(survey_id %in% unique(sl_m2$survey_id)) 

m2_inverts <- read_csv("data/raw/RLS/ep_M2_inverts_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove))  %>%
  dplyr::filter(survey_id %in% unique(sl_m2$survey_id)) %>%
  dplyr::select(-biomass)

# Checking out the data
summary(m1)
summary(m2_fish)
summary(m2_inverts)

unique(m2_fish$class)
unique(m2_inverts$class)

# Check Zeros ----
m1_no_species <- m1 %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) # 92 blocks without species

# M1 fish check ----
surveys_not_present_in_m1_data <- anti_join(sl_m1, m1) 
# 5000363 Block 2 is not in M1 fish data (should it be "No species found"?)
# 5000366 Block 2 same

write_csv(surveys_not_present_in_m1_data, "surveys_not_present_in_m1_data.csv")

# M2 fish check ----
m2_fish_no_species <- m2_fish %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) # 22 blocks without species

surveys_not_present_in_m2_fish_data <- anti_join(sl_m2, m2_fish) 
write_csv(surveys_not_present_in_m2_fish_data, "surveys_not_present_in_m2_fish_data.csv")

# M2 inverts check ----
m2_inverts_no_species <- m2_inverts %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) # 22 blocks without species

surveys_not_present_in_m2_invert_data <- anti_join(sl_m2, m2_inverts) 
write_csv(surveys_not_present_in_m2_invert_data, "surveys_not_present_in_m2_inverts_data.csv")

# test <- anti_join(m2_fish_no_species, m2_inverts_no_species) # they are exactly the same!
# TODO something weird is happening here I think

# TODO Have emailed Sophie to see if this is a mistake - need someway to tell if 2 blocks are always done for all methods

# Tidy species names ----
# Start with method 1 ----
m1_species <- m1 %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>% # remove zeros - use survey list to add in zeros
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>% # remove dots from species names (spp.)
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>% # Make genus Unknown if it is a family name
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown"))# replace blanks

# Check CheckEM's synonyms 
# synonyms_in_m1 <- dplyr::left_join(m1_species, CheckEM::aus_synonyms) %>%
#   dplyr::filter(!is.na(genus_correct)) %>%
#   dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
#   dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
#   dplyr::select('old name', 'new name') %>%
#   dplyr::distinct()

species_in_multiple_classes <- m1_species %>%
  dplyr::distinct(phylum, class, order, family, genus, species) %>%
  group_by(family, genus, species) %>%
  count() %>%
  filter(n > 1)

m1_clean <- dplyr::left_join(m1_species, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>% # replace synonyms
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  
  # Sasha's name changes
  dplyr::mutate(genus = str_replace_all(genus, "Ascarosepion", "Sepia")) %>%
  dplyr::mutate(family = if_else(genus %in% "Neatypus", "Microcanthidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Ophiclinus", "Ophiclinidae", family)) %>%
  dplyr::mutate(genus = if_else(genus %in% "Pelates", "Helotes", genus)) %>%
  dplyr::mutate(species = if_else(genus %in% "Pseudocaranx", "spp", species)) %>%
  dplyr::mutate(species = if_else(genus %in% "Cochleoceps", "spp", species)) %>%
  dplyr::mutate(genus = if_else(genus %in% "Cochleoceps", "Unknown", genus)) %>%
  dplyr::mutate(genus = if_else(recorded_species_name %in% "Nesogobius spp.", "Unknown", genus)) %>%
  dplyr::mutate(species = if_else(species %in% "gigas", "spp", species)) %>%
  dplyr::mutate(genus_fam = if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(species = if_else(genus %in% "Heteroclinus", "spp", species)) %>%
  dplyr::mutate(portal_name = paste(genus_fam, species)) %>%
  dplyr::rename(rls_recorded_name = recorded_species_name, 
                rls_reporting_name = reporting_name) %>%
  dplyr::filter(!family %in% "Unknown") %>% # have removed unknowns at the family level
  dplyr::select(-c(phylum, class, order, genus_fam)) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::filter(!scientific %in% "Monacanthidae Unknown spp")
# dplyr::filter(!class %in% "Teleostei") # removed species that had multiple classes for Cheilodactylus spectabilis  

# check species not in CheckEM life history list ----
m1_species_new_not_observed <- m1_clean %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

# Method 2 Fish----
m2_species <- m2_fish %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 

# species_in_multiple_classes <- m2_species %>%
#   dplyr::distinct(phylum, class, order, family, genus, species) %>%
#   group_by(family, genus, species) %>%
#   count() %>%
#   filter(n > 1) # none

# Check for synonyms using CheckEM list
# synonyms_in_m2 <- dplyr::left_join(m2_species, CheckEM::aus_synonyms) %>%
#   dplyr::filter(!is.na(genus_correct)) %>%
#   dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
#   dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
#   dplyr::select('old name', 'new name') %>%
#   dplyr::distinct()

m2_species_new <- dplyr::left_join(m2_species, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  
  # Sasha's synonyms
  dplyr::mutate(genus = str_replace_all(genus, "Ascarosepion", "Sepia")) %>%
  dplyr::mutate(species = if_else(genus %in% "Heteroclinus", "spp", species)) %>%
  dplyr::mutate(species = if_else(species %in% "gracilis", "spp", species)) %>%
  dplyr::mutate(species = if_else(species %in% "ningulus", "spp", species)) %>%
  dplyr::mutate(species = if_else(species %in% "sp 4 [groovedcheek]", "spp", species)) %>%
  dplyr::mutate(species = if_else(species %in% "Platycephalus", "spp", species)) %>%
  dplyr::mutate(species = if_else(species %in% "gigas", "spp", species)) %>%
  
  dplyr::mutate(species = if_else(species %in% "pipefish", "spp", species)) %>%
  dplyr::mutate(genus = if_else(genus %in% "Unidentified", "Unknown", genus)) %>%
  
  dplyr::mutate(family = if_else(genus %in% "Peronedys", "Ophiclinidae", family)) %>%
  dplyr::mutate(family = if_else(genus %in% "Ophiclinus", "Ophiclinidae", family)) %>%
  
  dplyr::mutate(genus_fam = if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(portal_name = paste(genus_fam, species)) %>%
  dplyr::rename(rls_recorded_name = recorded_species_name, rls_reporting_name = reporting_name) %>%
  dplyr::filter(!family %in% "Unknown") %>%
  dplyr::select(-c(phylum, class, order, genus_fam)) %>%
  dplyr::filter(!species %in% "portusjacksoni egg")

m2_species_new_not_observed <- m2_species_new %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

# Save cleaned data ----
write_rds(m1_clean, "data/tidy/rls_m1_count_and_length.rds")
write_rds(m2_fish_clean, "data/tidy/rls_m2_fish_count_and_length.rds")
write_rds(m2_inverts_clean, "data/tidy/rls_m2_inverts_count_and_length.rds")