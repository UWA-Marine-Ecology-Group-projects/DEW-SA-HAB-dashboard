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

test <- anti_join(m2_fish_no_species, m2_inverts_no_species)

# Tidy species names ----
# Start with method 1 ----
m1_species <- m1 %>%
  # distinct(phylum, class, order, family, recorded_species_name, species_name, reporting_name) %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  # dplyr::filter(!species_name == reporting_name) %>% # I think reporting name is the same as species name except for spps.
  # dplyr::filter(!recorded_species_name == species_name) %>% # the only ones that are changed are synonym changes I think
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 




