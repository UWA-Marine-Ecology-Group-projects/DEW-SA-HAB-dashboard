#################################################################
# Format and Clean RLS data

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
cols_to_remove <- c("ecoregion", "country", "area", "realm", "geom", 'visibility', "hour", "survey_latitude", 'survey_longitude', "diver", "method", "taxon", "location", "site_code", "latitude", "longitude") # duplicated with metadata

## survey lists ----
sl_m1 <- readRDS("data/tidy/rls_m1_survey_list.rds") %>% dplyr::mutate(id = paste(survey_id, block))
sl_m2 <- readRDS("data/tidy/rls_m2_survey_list.rds") %>% dplyr::mutate(id = paste(survey_id, block))

## abundance and length ----
m1 <- read_csv("data/raw/RLS/ep_M1_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove)) %>%
  dplyr::filter(survey_id %in% unique(sl_m1$survey_id))  %>% dplyr::mutate(id = paste(survey_id, block)) %>%
  dplyr::left_join(sl_m1) # for sampling event

m2_fish <- read_csv("data/raw/RLS/ep_M2_cryptic_fish_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove))  %>%
  dplyr::filter(survey_id %in% unique(sl_m2$survey_id))  %>% dplyr::mutate(id = paste(survey_id, block)) %>%
  dplyr::left_join(sl_m2) # for sampling event

m2_inverts <- read_csv("data/raw/RLS/ep_M2_inverts_SA.csv") %>% 
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::select(!all_of(cols_to_remove))  %>%
  dplyr::filter(survey_id %in% unique(sl_m2$survey_id)) %>%
  dplyr::select(-biomass) %>% dplyr::mutate(id = paste(survey_id, block)) %>%
  dplyr::left_join(sl_m2) # for sampling event

# Checking out the data
summary(m1)
summary(m2_fish)
summary(m2_inverts)

unique(m2_fish$class)
unique(m2_inverts$class)

# Check Zeros ----
m1_zeros <- m1 %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) # 92 blocks without species

# M1 fish check ----
surveys_not_present_in_m1_data <- anti_join(sl_m1, m1) 
# 5000363 Block 2 is not in M1 fish data (should it be "No species found"?)
# 5000366 Block 2 same
# 
# test <- m1_clean %>%
#   dplyr::filter(survey_id %in% unique(surveys_not_present_in_m1_data$survey_id)) %>%
#   dplyr::group_by(survey_id, site_name, survey_date, block) %>%
#   dplyr::summarise(n = n())

manual_fixes_zeros_m1 <- surveys_not_present_in_m1_data %>%
  dplyr::filter(site_code %in% c("GSV117")) %>% # Add in zero where missing
  dplyr::select(survey_id, site_name, site_code, depth, program, block, id, survey_date, transect, sampling_event, period, period_split, sampling_event_start_date) %>%
  dplyr::mutate(recorded_species_name = "No species found",
                species_name = "No species found")


m1_all_zeros <- bind_rows(m1_zeros, manual_fixes_zeros_m1) %>%
  dplyr::mutate(total = 0, size_class = NA)

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

# Combine M2 surveys to find true zeros ----
m2_abundance <- bind_rows(m2_fish, m2_inverts) %>%
  distinct(survey_id, site_name, survey_date, block) #3382 blocks with some kind of abundance

surveys_not_present_in_m2_all <- anti_join(sl_m2, m2_abundance) 
write_csv(surveys_not_present_in_m2_all, "surveys_not_present_in_either_m2_datasets.csv")

# Checked these with Sophie and the 2025/2026 sites are true zeros, but we are not sure about the others so we will remove them, because we can not tell if they should be zeros or they were not completed

surveys_to_remove <- surveys_not_present_in_m2_all %>%
  dplyr::filter(survey_date < "2025-01-01")

# Make M2 zero data ----
m2_inverts_zeros <- m2_inverts %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) 

m2_inverts_all_zeros <- surveys_not_present_in_m2_invert_data %>%
  dplyr::select(survey_id, site_name, site_code, depth, program, block, id, survey_date, site_code, latitude, longitude, sampling_event, location, mpa, transect, period, period_split, sampling_event_start_date) %>%
  anti_join(surveys_to_remove) %>%
  dplyr::mutate(recorded_species_name = "No species found",
                species_name = "No species found") %>%
  bind_rows(m2_inverts_zeros, .) %>%
  dplyr::mutate(total = 0, size_class = NA)

m2_fish_zeros <- m2_fish %>%
  dplyr::filter(recorded_species_name %in% c("No species found")) 

m2_fish_all_zeros <- surveys_not_present_in_m2_fish_data %>%
  dplyr::select(survey_id, site_code, site_name, depth, program, block, id, survey_date, site_code, latitude, longitude, sampling_event, location, mpa, transect, period, period_split, sampling_event_start_date) %>%
  anti_join(surveys_to_remove) %>%
  dplyr::mutate(recorded_species_name = "No species found",
                species_name = "No species found") %>%
  bind_rows(m2_fish_zeros, .) %>%
  dplyr::mutate(total = 0, size_class = NA)


# Check number of surveys
length(unique(sl_m1$id)) # 3642
length(unique(sl_m2$id)) # 3398

length(unique(m1$id)) # 3634 (3642 - 3634 = 8) # TODO sophie thinks drop the extras that we don't know if they are actual zeros or not
length(unique(m2_fish$id)) # 2280 (3398 - 2280 = 1118)
length(unique(m2_inverts$id)) # 3359 (3398 - 3359 = 39)

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

# species_in_multiple_classes <- m1_species %>%
#   dplyr::distinct(phylum, class, order, family, genus, species) %>%
#   group_by(family, genus, species) %>%
#   count() %>%
#   filter(n > 1)

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
  
  dplyr::mutate(class = if_else(family %in% "Cheilodactylidae", "Actinopterygii", class)) %>%
  dplyr::mutate(order = if_else(family %in% "Cheilodactylidae", "Perciformes", order)) %>%
  
  dplyr::filter(!family %in% "Unknown") %>% # have removed unknowns at the family level
  
  dplyr::filter(phylum %in% "Chordata") %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii")) %>%
  
  dplyr::select(-c(genus_fam)) %>% # phylum, class, order, 
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::filter(!scientific %in% "Monacanthidae Unknown spp")
# dplyr::filter(!class %in% "Teleostei") # removed species that had multiple classes for Cheilodactylus spectabilis  

unique(m1_clean$phylum)
unique(m1_clean$class)
unique(m1_clean$order)

# check species not in CheckEM life history list ----
m1_species_not_observed <- m1_clean %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

# Sasha - Helotes over Pelates

# Method 2 Fish----
m2_fish_species <- m2_fish %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 

# species_in_multiple_classes <- m2_fish_species %>%
#   dplyr::distinct(phylum, class, order, family, genus, species) %>%
#   group_by(family, genus, species) %>%
#   count() %>%
#   filter(n > 1) # none

# Check for synonyms using CheckEM list
# synonyms_in_m2 <- dplyr::left_join(m2_fish_species, CheckEM::aus_synonyms) %>%
#   dplyr::filter(!is.na(genus_correct)) %>%
#   dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
#   dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
#   dplyr::select('old name', 'new name') %>%
#   dplyr::distinct()

m2_fish_clean <- dplyr::left_join(m2_fish_species, CheckEM::aus_synonyms) %>%
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
  
  dplyr::mutate(family = if_else(genus %in% "Neosebastes", "Neosebastidae", family)) %>%
  
  dplyr::mutate(genus_fam = if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(portal_name = paste(genus_fam, species)) %>%
  dplyr::rename(rls_recorded_name = recorded_species_name, rls_reporting_name = reporting_name) %>%
  dplyr::filter(!family %in% "Unknown") %>%
  dplyr::select(-c(genus_fam)) %>% # phylum, class, order, 
  dplyr::filter(!species %in% "portusjacksoni egg") %>%
  dplyr::mutate(
    scientific = paste(family, genus, species)
  )

m2_fish_not_observed <- m2_fish_clean %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))
# None

unique(m2_fish_clean$phylum)
unique(m2_fish_clean$class)
unique(m2_fish_clean$order)

# M2 inverts ----
m2_species_inverts <- m2_inverts %>%
  dplyr::filter(!recorded_species_name %in% c("No species found")) %>%
  tidyr::separate(species_name, into = c("genus", "species"), extra = "merge") %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  dplyr::mutate(genus = if_else(family == genus, "Unknown", genus)) %>%
  tidyr::replace_na(list(family = "Unknown", genus = "Unknown")) 

species_in_multiple_classes <- m2_species_inverts %>%
  dplyr::distinct(
    phylum, class, order,
    family, genus, species
  ) %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(family, genus, species, phylum, class, order)

# Find families assigned to more than one class/order combination
ambiguous_families <- m2_species_inverts %>%
  dplyr::distinct(family, class, order) %>%
  dplyr::count(family, name = "n_classifications") %>%
  dplyr::filter(n_classifications > 1) %>%
  dplyr::pull(family)

family_taxonomy_lookup <- m2_species_inverts %>%
  dplyr::filter(family %in% ambiguous_families) %>%
  dplyr::count(family, class, order, name = "n") %>%
  dplyr::group_by(family) %>%
  dplyr::arrange(
    dplyr::desc(n),
    class,
    order
  ) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    family,
    preferred_class = class,
    preferred_order = order
  )

m2_inverts_clean <- m2_species_inverts %>%
  
  dplyr::left_join(
    family_taxonomy_lookup,
    by = "family"
  ) %>%
  dplyr::mutate(
    class = dplyr::coalesce(preferred_class, class),
    order = dplyr::coalesce(preferred_order, order)
  ) %>%
  dplyr::select(
    -preferred_class,
    -preferred_order
  ) %>%
  
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
  # dplyr::distinct(phylum, class, order, family, genus, species, portal_name, rls_reporting_name) %>% #rls_recorded_name
  # left_join(dew_species) %>%
  # dplyr::filter(!order %in% c("Articulata", "Trochida")) %>%
  dplyr::select(-c(genus_fam)) %>% # phylum, class, order, 
  dplyr::filter(!family %in% "Unknown") %>%
  dplyr::mutate(
    scientific = paste(family, genus, species)
  )

m2_species_not_observed_inverts <- m2_inverts_clean %>%
  dplyr::distinct(family, genus, species) %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species"))

unique(m2_inverts_clean$phylum)
unique(m2_inverts_clean$class)
unique(m2_inverts_clean$order)

# Find common species ----
species_in_m1_m2_fish <- semi_join(m1_clean %>% distinct(family, genus, species, portal_name),
                                   m2_fish_clean %>% distinct(family, genus, species, portal_name)) 

nrow(species_in_m1_m2_fish) # 37 species that are in both

species_in_m1_m2_inverts <- semi_join(m1_clean %>% distinct(family, genus, species, portal_name), 
                                      m2_inverts_clean%>% distinct(family, genus, species, portal_name))

nrow(species_in_m1_m2_inverts) # 0 species that are in both

species_in_m2_both <- semi_join(m2_fish_clean %>% distinct(family, genus, species, portal_name),
                                m2_inverts_clean%>% distinct(family, genus, species, portal_name)) 

nrow(species_in_m2_both) # 0 species that are in both

# Save cleaned data ----
write_rds(m1_clean, "data/tidy/rls_m1_count_and_length.rds")
write_rds(m2_fish_clean, "data/tidy/rls_m2_fish_count_and_length.rds")
write_rds(m2_inverts_clean, "data/tidy/rls_m2_inverts_count_and_length.rds")

# Save empty surveys -----
write_rds(m1_all_zeros, "data/tidy/rls_m1_zeros.rds")
write_rds(m2_fish_all_zeros, "data/tidy/rls_m2_fish_zeros.rds")
write_rds(m2_inverts_all_zeros, "data/tidy/rls_m2_inverts_zeros.rds")

# Complete count ----
# a dataframe with zeros for each species - no need to have 'No species recorded' because all rows for that block will be zero!
length(unique(m1_clean$scientific)) # 131 species of fish
length(unique(m1_clean$id)) # 3538 surveys with fish
length(unique(m1_all_zeros$id)) # 93 surveys without fish
length(unique(m1_clean$id)) + length(unique(m1_all_zeros$id)) # 3631 surveys
length(unique(m1_clean$transect)) + length(unique(m1_all_zeros$transect)) # 1,869 surveys
3631 * length(unique(m1_clean$scientific)) # 475,661 rows

# Need to combine size data for this dataframe ----
# Summarise the observed abundance and biomass for each species in each block
m1_count_summary <- m1_clean %>%
  dplyr::group_by(
    survey_id, site_name, survey_date, depth, program, block, id, sampling_event,
    phylum, class, order, family, genus, species,
    # rls_recorded_name, rls_reporting_name,
    scientific, portal_name
  ) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    biomass_sum = sum(biomass, na.rm = TRUE),
    .groups = "drop"
  )

# One row for every survey/block, including surveys where no fish were recorded
m1_surveys <- dplyr::bind_rows(
  m1_clean %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    ),
  m1_all_zeros %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    )
) %>%
  dplyr::distinct()

m1_transects <- m1_surveys %>%
  dplyr::distinct(transect) %>%
  nrow()

m1_blocks <- m1_surveys %>%
  dplyr::distinct(id) %>%
  nrow()

# List of fish
m1_species_list <- m1_clean %>%
  dplyr::filter(
    !rls_recorded_name %in%
      c("No species found", "No species recorded")
  ) %>%
  dplyr::distinct(
    phylum, class, order, family, genus, species,
    scientific, portal_name
  )

# Create every survey/block × species combination,
# then add the observed abundance and biomass
m1_complete_count <- tidyr::crossing(
  m1_surveys,
  m1_species_list
) %>%
  dplyr::left_join(
    m1_count_summary,
    by = c(
      "survey_id", "site_name", "survey_date", "depth",
      "program", "block", "id", "sampling_event",
      "phylum", "class", "order", "family", "genus", "species",
      "scientific", "portal_name"
    )
  ) %>%
  dplyr::mutate(
    total = tidyr::replace_na(total, 0),
    biomass_sum = tidyr::replace_na(biomass_sum, 0)
  )  %>%
  left_join(sl_m1)

length(unique(m1_complete_count$id)) 
length(unique(m1_complete_count$scientific))
nrow(m1_complete_count)

nrow(m1_complete_count) ==
  length(unique(m1_complete_count$id)) *
  length(unique(m1_complete_count$scientific))

# Summarise observed M2 fish abundance and biomass
m2_fish_count_summary <- m2_fish_clean %>%
  dplyr::group_by(
    survey_id, site_name, survey_date, depth, program, block, id, sampling_event,
    phylum, class, order, family, genus, species,
    scientific, portal_name
  ) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    biomass_sum = sum(biomass, na.rm = TRUE),
    .groups = "drop"
  )

# One row per M2 survey/block, including blocks with no cryptic fish
m2_fish_surveys <- dplyr::bind_rows(
  m2_fish_clean %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    ),
  m2_fish_all_zeros %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    )
) %>%
  dplyr::distinct()

m2_fish_transects <- m2_fish_surveys %>%
  dplyr::distinct(transect) %>%
  nrow()

m2_fish_blocks <- m2_fish_surveys %>%
  dplyr::distinct(id) %>%
  nrow()

# Unique corrected fish IDs
m2_fish_species_list <- m2_fish_clean %>%
  dplyr::distinct(
    phylum, class, order, family, genus, species,
    scientific, portal_name
  )

# Every survey/block × every M2 fish ID
m2_fish_complete_count <- tidyr::crossing(
  m2_fish_surveys,
  m2_fish_species_list
) %>%
  dplyr::left_join(
    m2_fish_count_summary,
    by = c(
      "survey_id", "site_name", "survey_date", "depth",
      "program", "block", "id", "sampling_event",
      "phylum", "class", "order", "family",
      "genus", "species", "scientific", "portal_name"
    )
  ) %>%
  dplyr::mutate(
    total = tidyr::replace_na(total, 0),
    biomass_sum = tidyr::replace_na(biomass_sum, 0)
  ) %>% left_join(sl_m2)

# Summarise observed M2 invertebrate abundance
m2_inverts_count_summary <- m2_inverts_clean %>%
  dplyr::group_by(
    survey_id, site_name, survey_date, depth, program, block, id, sampling_event,
    phylum, class, order, family, genus, species,
    scientific, portal_name
  ) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    .groups = "drop"
  )

# One row per M2 survey/block, including blocks with no invertebrates
m2_inverts_surveys <- dplyr::bind_rows(
  m2_inverts_clean %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    ),
  m2_inverts_all_zeros %>%
    dplyr::distinct(
      survey_id, site_name, survey_date, depth, sampling_event,
      program, block, id, transect
    )
) %>%
  dplyr::distinct()

m2_inverts_transects <- m2_inverts_surveys %>%
  dplyr::distinct(transect) %>%
  nrow()

m2_inverts_blocks <- m2_inverts_surveys %>%
  dplyr::distinct(id) %>%
  nrow()

# Unique corrected invertebrate IDs
m2_inverts_species_list <- m2_inverts_clean %>%
  dplyr::distinct(
    phylum, class, order, family, genus, species,
    scientific, portal_name
  )

# Every survey/block × every M2 invertebrate ID
m2_inverts_complete_count <- tidyr::crossing(
  m2_inverts_surveys,
  m2_inverts_species_list
) %>%
  dplyr::left_join(
    m2_inverts_count_summary,
    by = c(
      "survey_id", "site_name", "survey_date", "depth",
      "program", "block", "id", "sampling_event",
      "phylum", "class", "order", "family",
      "genus", "species", "scientific", "portal_name"
    )
  ) %>%
  dplyr::mutate(
    total = tidyr::replace_na(total, 0)
  ) %>% left_join(sl_m2)

# M2 fish
nrow(m2_fish_complete_count)

nrow(m2_fish_complete_count) ==
  dplyr::n_distinct(m2_fish_complete_count$id) *
  dplyr::n_distinct(m2_fish_complete_count$scientific)

# M2 invertebrates
nrow(m2_inverts_complete_count)

nrow(m2_inverts_complete_count) ==
  dplyr::n_distinct(m2_inverts_complete_count$id) *
  dplyr::n_distinct(m2_inverts_complete_count$scientific)

m2_fish_complete_count %>%
  dplyr::count(id, scientific) %>%
  dplyr::filter(n != 1)

m2_inverts_complete_count %>%
  dplyr::count(id, scientific) %>%
  dplyr::filter(n != 1)

# Save complete data ----
write_rds(m1_complete_count, "data/tidy/rls_m1_complete_count.rds")
write_rds(m2_fish_complete_count, "data/tidy/rls_m2_fish_complete_count.rds")
write_rds(m2_inverts_complete_count, "data/tidy/rls_m2_inverts_complete_count.rds")

# Save final survey lists ----
m1_surveys_with_meta <- m1_surveys %>%
  left_join(sl_m1)

length(unique(m1_surveys_with_meta$transect))

test <- m1_surveys_with_meta %>%
  dplyr::select(-c(block, id)) %>%
  dplyr::distinct()

dups <- test %>%
  group_by(survey_id) %>%
  dplyr::summarise(m = n())

m2_fish_surveys_with_meta <- m2_fish_surveys %>%
  left_join(sl_m2)

m2_inverts_surveys_with_meta <- m2_inverts_surveys %>%
  left_join(sl_m2)

write_rds(m1_surveys_with_meta , "data/tidy/rls_m1_surveys_final.rds")
write_rds(m2_fish_surveys_with_meta, "data/tidy/rls_m2_fish_surveys_final.rds")
write_rds(m2_inverts_surveys_with_meta, "data/tidy/rls_m2_inverts_surveys_final.rds")

# TODO create a complete length dataframe ---