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

# Function
calculate_species_richness <- function(data, dataset_name = "dataset") {
  
  sample_cols <- c(
    "survey_id", "site_name", "survey_date", "depth", "program" #, "block", "id"
  )
  
  genus_cols <- c(sample_cols, "family", "genus")
  
  # Combine abundance across blocks for each taxon
  data_summarised <- data %>%
    dplyr::group_by(
      survey_id, site_name, survey_date, depth, program,
      family, genus, species, scientific
    ) %>%
    dplyr::summarise(
      total = sum(total, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify sample/genus combinations containing both:
  # 1. an observed spp record, and
  # 2. an observed species-level record
  samples_with_both <- data_summarised %>%
    dplyr::group_by(
      survey_id, site_name, survey_date, depth, program,
      family, genus
    ) %>%
    dplyr::summarise(
      spp_present = any(species == "spp" & total > 0),
      identified_species_present = any(
        species != "spp" & total > 0
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      spp_present,
      identified_species_present
    )
  
  # Print a message to the console
  if (nrow(samples_with_both) > 0) {
    
    n_samples <- samples_with_both %>%
      dplyr::distinct(
        dplyr::across(dplyr::all_of(sample_cols))
      ) %>%
      nrow()
    
    message(
      dataset_name, ": found ",
      nrow(samples_with_both),
      " sample/genus combinations across ",
      n_samples,
      " samples containing both an spp record and an ",
      "identified species. The spp records will be removed."
    )
    
  } else {
    
    message(
      dataset_name,
      ": no samples contained both an spp record and an ",
      "identified species from the same genus."
    )
  }
  
  # Remove spp only where an identified species from the
  # same genus occurs in the same sample
  richness <- data_summarised %>%
    dplyr::group_by(
      survey_id, site_name, survey_date, depth, program,
      family, genus
    ) %>%
    dplyr::mutate(
      identified_species_present = any(
        species != "spp" & total > 0
      )
    ) %>%
    dplyr::filter(
      !(
        species == "spp" &
          total > 0 &
          identified_species_present
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      survey_id, site_name, survey_date, depth, program
    ) %>%
    dplyr::summarise(
      species_richness = dplyr::n_distinct(
        scientific[total > 0]
      ),
      .groups = "drop"
    )
  
  attr(richness, "samples_with_both") <- samples_with_both
  return(richness)
}

# Read in survey-lists to get grouping variables ----
sl_m1 <- read_rds("data/tidy/rls_m1_survey_list.rds") %>%
  select(-block) %>%
  distinct()

sl_m2 <- read_rds("data/tidy/rls_m2_survey_list.rds") %>%
  select(-block) %>%
  distinct()

# Species Richness per sample (Not calculated per block!) ----
## M1 fish ----
m1_fish_sr_samples <- read_rds("data/tidy/rls_m1_complete_count.rds") %>%
  calculate_species_richness(dataset_name = "M1 fish") %>%
  left_join(sl_m1)

hist(m1_fish_sr_samples$species_richness)
summary(m1_fish_sr_samples)

m1_spp_conflicts <- attr(m1_fish_sr_samples,"samples_with_both")
m1_spp_conflicts

## M2 fish ----
m2_fish_sr_samples <- read_rds("data/tidy/rls_m2_fish_complete_count.rds") %>%
  calculate_species_richness(dataset_name = "M2 fish") %>%
  left_join(sl_m2)

hist(m2_fish_sr_samples$species_richness)
summary(m2_fish_sr_samples)

m2_fish_spp_conflicts <- attr(m2_fish_sr_samples, "samples_with_both")
m2_fish_spp_conflicts

## M2 inverts ----
m2_inverts_sr_samples <- read_rds("data/tidy/rls_m2_inverts_complete_count.rds") %>%
  calculate_species_richness(dataset_name = "M2 invertebrates") %>%
  left_join(sl_m2)

hist(m2_inverts_sr_samples$species_richness)
summary(m2_inverts_sr_samples)

m2_inverts_spp_conflicts <- attr(m2_inverts_sr_samples, "samples_with_both")
m2_inverts_spp_conflicts

# Calculate averages per site/sampling event ----
m1_fish_site_sr_average <- m1_fish_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num = n(),
                   .groups = "drop")

m2_fish_site_sr_average <- m2_fish_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num = n(),
                   .groups = "drop")

m2_inverts_site_sr_average <- m2_inverts_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num = n(),
                   .groups = "drop")
