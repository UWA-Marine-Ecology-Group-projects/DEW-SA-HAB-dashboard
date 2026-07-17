# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
library(dplyr)
library(sf)
library(stringr)
library(readr)

sf_use_s2(FALSE)

# Sites that DEW want ----
sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

# TODO review
# Key fields are “Site_name” and “Location_g” for the two tabs on the portal. I’ve also added a “BRUVSRepor” column to show how they map to the BRUVS reporting regions, they actually fit in quite well but I think it may be too broad a scale to sensibly present the results. Maybe start with the location and site groupings, and then we can look at reporting region down the track if we need to

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
# 1. Check species are correct - change names - using Sasha's scripts
# 2. Check they are all the species they are meant to be, do I need to split for sharks and rays vs. fish?
# 3. Split data into pre-bloom and post-bloom.
# 4. Make sure that the summarising categories are in the data e.g. site, reporting_region, period
# 5. Clarify how I am going to calculate the metrics e.g. if each block is averaged per site and then a region is also the average of all blocks in the region or if is the average of the sites (I think the former)
# 6. Calc metrics - start with Species richness and diversity metrics first
# 







# Metrics ----
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
