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

cols_to_remove <- c("country", "area", "realm", "geom", 'visibility', "hour", "survey_latitude", 'survey_longitude', "diver", "method", "taxon", "program")

m1 <- read_csv("data/raw/RLS/ep_M1_SA.csv") %>% 
  dplyr::select(!all_of(cols_to_remove))
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::filter(survey_id %in% unique(survey_list$survey_id)) 



