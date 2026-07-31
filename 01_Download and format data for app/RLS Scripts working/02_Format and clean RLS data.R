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

# Sites from DEW ----
sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

dew_species <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing") %>%
  rename(portal_name = genus_species) %>%
  mutate(genus_species = portal_name)
2

# CheckEM life history list ----
lh <- CheckEM::australia_life_history

# Read in data ----
survey_list <- read_csv("data/raw/RLS/ep_survey_list.csv") %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) 


cols_to_remove <- c("country", "area", "realm", "geom", 'visibility', "hour", "survey_latitude", 'survey_longitude', "diver", "method", "taxon")



unique(survey_list$methods)

check <- survey_list %>%
  distinct(survey_id, site_code, survey_date, depth) %>%
  group_by(site_code, survey_date) %>%
  summarise(n = n())

hist(check$n)

plot(survey_list$survey_date, survey_list$depth)