#################################################################
# Create Species Richness from RLS M1 and M2 data

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

# Species Richness (Calculated per block) ----
## M1 ----
m1_fish_sr_samples <- read_rds("data/tidy/rls_m1_complete_count.rds") %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, program, block, id) %>%
  dplyr::summarise(total_abundance = sum(total), 
                   species_richness = sum(total > 0))

hist(m1_fish_sr_samples$species_richness)
hist(m1_fish_sr_samples$total_abundance)
summary(m1_fish_sr_samples)

## M2 Fish ----
m2_fish_sr_samples <- read_rds("data/tidy/rls_m2_fish_complete_count.rds") %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, program, block, id) %>%
  dplyr::summarise(total_abundance = sum(total), 
                   species_richness = sum(total > 0))

hist(m2_fish_sr_samples$species_richness)
hist(m2_fish_sr_samples$total_abundance)
summary(m2_fish_sr_samples)

## M2 Inverts ----
m2_inverts_sr_samples <- read_rds("data/tidy/rls_m2_inverts_complete_count.rds") %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, program, block, id) %>%
  dplyr::summarise(total_abundance = sum(total), 
                   species_richness = sum(total > 0))

hist(m2_fish_sr_samples$species_richness)
hist(m2_fish_sr_samples$total_abundance)
summary(m2_fish_sr_samples)
