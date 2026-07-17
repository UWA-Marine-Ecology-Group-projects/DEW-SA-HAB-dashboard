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
  select(site_code, site_name)

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

# TODO check with Sophie - what the 70 sites were?

# Format data ----


# Check to see original sites ----
# rls_dive_sites <- survey_list %>%
#   distinct(site_code, site_name, latitude, longitude)
# 
# write_csv(rls_dive_sites, "rls_dive_sites.csv")
