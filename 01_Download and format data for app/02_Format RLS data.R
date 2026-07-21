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

sf_use_s2(FALSE)

# Sites that DEW want ----
sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

dew_species <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing") %>%
  rename(portal_name = genus_species)
2

# TODO review
# Key fields are “Site_name” and “Location_g” for the two tabs on the portal. I’ve also added a “BRUVSRepor” column to show how they map to the BRUVS reporting regions, they actually fit in quite well but I think it may be too broad a scale to sensibly present the results. Maybe start with the location and site groupings, and then we can look at reporting region down the track if we need to

# Life history ----
lh <- CheckEM::australia_life_history

# Read in data sets ----
cols_to_remove <- c("country", "area", "realm", "geom", 'visibility', "hour", "survey_latitude", 'survey_longitude', "diver", "method", "taxon")

survey_list <- read_csv("data/raw/RLS/ep_survey_list_SA.csv") %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code))

# Animals ----
m1          <- read_csv("data/raw/RLS/ep_M1_SA.csv") %>% dplyr::select(-cols_to_remove) %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code))

m2_fish     <- read_csv("data/raw/RLS/ep_M2_cryptic_fish_SA.csv") %>% dplyr::select(-cols_to_remove) %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code))

m2_inverts  <- read_csv("data/raw/RLS/ep_M2_inverts_SA.csv") %>% dplyr::select(-cols_to_remove) %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code))

summary(m1)

summary(m2_fish)
unique(m2_fish$class)

summary(m2_inverts)
unique(m2_inverts$class)

# # Going to ignore these two for now ----
# m3          <- read_csv("data/raw/RLS/ep_M3_isq_SA.csv")
# m0          <- read_csv("data/raw/RLS/ep_M0_off_transect_sighting_SA.csv")

# Checking out data ----
names(m1)
names(survey_list)

unique(m1$location) # 3 locations
unique(m2_fish$location) # 3 locations
unique(m2_inverts$location) # 3 locations
unique(survey_list$location) # 3 locations

length(unique(m1$site_name)) # 70 sites
length(unique(m2_fish$site_name)) # 70 sites
length(unique(m2_inverts$site_name)) # 70 sites
length(unique(survey_list$site_name)) # 70 sites

length(unique(m1$site_code)) # 70 sites
length(unique(m2_fish$site_code)) # 70 sites
length(unique(m2_inverts$site_code)) # 70 sites
length(unique(survey_list$site_code)) # 70 sites

length(unique(survey_list$survey_id)) # 1843 surveys
length(unique(m1$survey_id)) # 1797 surveys (but includes two blocks?)
length(unique(m2_fish$survey_id)) # 1424 surveys (but includes two blocks?)
length(unique(m2_inverts$survey_id)) # 1828 surveys (but includes two blocks?)

# Format data ----
# what I think I need to do
# 1. Check species are correct - change names - using Sasha's scripts - check out the different cols for naming and see if they are different
# 2. Check they are all the species they are meant to be, do I need to split for sharks and rays vs. fish?
# 3. Split data into pre-bloom and post-bloom.
# 4. Make sure that the summarising categories are in the data e.g. site, reporting_region, period
# 5. Clarify how I am going to calculate the metrics e.g. if each block is averaged per site and then a region is also the average of all blocks in the region or if is the average of the sites (I think the former)
# 6. Calc metrics - start with Species richness and diversity metrics first

# Start with method 1 ----
m1_species <- m1 %>%
  distinct(phylum, class, order, family, recorded_species_name, species_name, reporting_name) %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  # dplyr::filter(!species_name == reporting_name) %>% # I think reporting name is the same as species name except for spps.
  # dplyr::filter(!recorded_species_name == species_name) %>% # the only ones that are changed are synonym changes I think
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 


unique(m1_species$genus) %>% sort()
unique(m1_species$species) %>% sort()

length(unique(m1_species$recorded_species_name))
length(unique(m1_species$reporting_name))
length(unique(m1_species$species_name))

synonyms_in_m1 <- dplyr::left_join(m1_species, CheckEM::aus_synonyms) %>%
  dplyr::filter(!is.na(genus_correct)) %>%
  dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
  dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
  dplyr::select('old name', 'new name') %>%
  dplyr::distinct()

m1_species_new <- dplyr::left_join(m1_species, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
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
  dplyr::mutate(portal_name = paste(genus_fam, species)) %>%
  dplyr::rename(rls_recorded_name = recorded_species_name, rls_reporting_name = reporting_name) %>%
  dplyr::distinct(phylum, class, order, family, genus, species, portal_name, rls_reporting_name) %>% #rls_recorded_name
  left_join(dew_species) %>%
  dplyr::filter(!family %in% "Unknown") %>%
  dplyr::filter(!class %in% "Teleostei") # removed species that had multiple classes for Cheilodactylus spectabilis  

species_in_multiple_classes <- m1_species_new %>%
  dplyr::distinct(phylum, class, order, family, genus, species) %>%
  group_by(family, genus, species) %>%
  count() %>%
  filter(n > 1)

m1_species_new_not_observed <- m1_species_new %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

# write_sheet(m1_species_new, "https://docs.google.com/spreadsheets/d/1M5UgtuoN6YAYKnXcB3UpbVn-ugvKGcwPbkNYUI-t_AM/edit?usp=sharing", sheet = "M1")


# Method 2 ----
m2_species <- m2_fish %>%
  distinct(phylum, class, order, family, recorded_species_name, species_name, reporting_name) %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  # dplyr::filter(!species_name == reporting_name) %>% # I think reporting name is the same as species name except for spps.
  # dplyr::filter(!recorded_species_name == species_name) %>% # the only ones that are changed are synonym changes I think
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 

unique(m2_species$genus) %>% sort()
unique(m2_species$species) %>% sort()

length(unique(m2_species$recorded_species_name))
length(unique(m2_species$reporting_name))

synonyms_in_m2 <- dplyr::left_join(m2_species, CheckEM::aus_synonyms) %>%
  dplyr::filter(!is.na(genus_correct)) %>%
  dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
  dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
  dplyr::select('old name', 'new name') %>%
  dplyr::distinct()

m2_species_new <- dplyr::left_join(m2_species, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
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
  dplyr::distinct(phylum, class, order, family, genus, species, portal_name, rls_reporting_name) %>% #rls_recorded_name
  left_join(dew_species) %>%
  dplyr::filter(!family %in% "Unknown")

species_in_multiple_classes <- m2_species_new %>%
  dplyr::distinct(phylum, class, order, family, genus, species) %>%
  group_by(family, genus, species) %>%
  count() %>%
  filter(n > 1)

m2_species_new_not_observed <- m2_species_new %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

# write_sheet(m2_species_new, "https://docs.google.com/spreadsheets/d/1M5UgtuoN6YAYKnXcB3UpbVn-ugvKGcwPbkNYUI-t_AM/edit?usp=sharing", sheet = "M2_cryptic")

# M2 inverts ----

m2_species_inverts <- m2_inverts %>%
  distinct(phylum, class, order, family, recorded_species_name, species_name, reporting_name) %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  # dplyr::filter(!species_name == reporting_name) %>% # I think reporting name is the same as species name except for spps.
  # dplyr::filter(!recorded_species_name == species_name) %>% # the only ones that are changed are synonym changes I think
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 

unique(m2_species_inverts$genus) %>% sort()
unique(m2_species_inverts$species) %>% sort()

m2_species_new_inverts <- m2_species_inverts %>%
  
  dplyr::mutate(genus = if_else(genus %in% "Ascarosepion", "Sepia", genus)) %>%
  dplyr::mutate(order = if_else(genus %in% "Turbo", "Vetigastropoda", order)) %>%
  
  dplyr::mutate(species = if_else(species %in% "tasmaniae", "spp", species)) %>%
  
  dplyr::mutate(genus = if_else(genus %in% "Flabellina", "Flabellinidae", genus)) %>%
  
  dplyr::mutate(species = if_else(genus %in% "Pagurus", "spp", species)) %>%
  dplyr::mutate(genus = if_else(genus %in% "Pagurus", "Unknown", genus)) %>%
  
  dplyr::mutate(species = if_else(species %in% "pelagicus", "armatus", species)) %>%
  
  dplyr::mutate(species = if_else(genus %in% "Pseudoceros", "spp", species)) %>%
  dplyr::mutate(genus = if_else(genus %in% "Pseudoceros", "Pseudobiceros", genus)) %>%
  
  dplyr::mutate(species = if_else(species %in% "porosissimus", "spp", species)) %>%
  
  dplyr::mutate(species = if_else(genus %in% "Amblypneustes", "spp", species)) %>%
  dplyr::mutate(species = if_else(genus %in% "Pyura", "spp", species)) %>%
  

  
  dplyr::mutate(genus_fam = if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(portal_name = paste(genus_fam, species)) %>%
  dplyr::rename(rls_recorded_name = recorded_species_name, rls_reporting_name = reporting_name) %>%
  dplyr::distinct(phylum, class, order, family, genus, species, portal_name, rls_reporting_name) %>% #rls_recorded_name
  left_join(dew_species) %>%
  dplyr::filter(!order %in% c("Articulata", "Trochida")) %>%
  dplyr::filter(!family %in% "Unknown")

m2_species_new_not_observed_inverts <- m2_species_new_inverts %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

species_in_multiple_classes <- m2_species_new_inverts %>%
  dplyr::distinct(phylum, class, order, family, genus, species) %>%
  group_by(family, genus, species) %>%
  count() %>%
  filter(n > 1)


# write_sheet(m2_species_new_inverts, "https://docs.google.com/spreadsheets/d/1M5UgtuoN6YAYKnXcB3UpbVn-ugvKGcwPbkNYUI-t_AM/edit?usp=sharing", sheet = "M2_inverts")

# Metrics ----# Metrics -recorded_species_name---
# Species richness (fish and inverts separately)
# B20 (or similar – Tim’s metric?)
# Shannon diversity (fish and inverts separately)
# Abundance/richness of functional/diet groups? To be determined – will create master list of traits for all species
# Percent cover of canopy forming macroalgae


# Check to see original sites ----
# rls_dive_sites <- survey_list %>%
#   distinct(site_code, site_name, latitude, longitude)
# 
# write_csv(rls_dive_sites, "rls_dive_sites.csv")
