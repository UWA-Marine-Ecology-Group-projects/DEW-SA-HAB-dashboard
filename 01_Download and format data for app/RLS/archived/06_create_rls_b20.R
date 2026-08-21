#################################################################
# Create Species Richness from RLS M1 and M2 data

# Load libraries needed -----
library(dplyr)
library(sf)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)

# read in cleaned data ----
m1_clean <- read_rds("data/tidy/rls_m1_count_and_length.rds") %>% rename(biomass_g = biomass)
m2_clean <- read_rds("data/tidy/rls_m2_fish_count_and_length.rds") %>% rename(biomass_g = biomass)

# read in empty surveys -----
m1_zeros <- read_rds("data/tidy/rls_m1_zeros.rds")
m2_zeros <- read_rds("data/tidy/rls_m2_fish_zeros.rds")

# Create surveys ----
# One row for every survey/block, including surveys where no fish were recorded
m1_surveys_blocks <- dplyr::bind_rows(
  m1_clean %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id),
  m1_zeros %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id)) %>%
  dplyr::distinct()

m2_surveys_blocks <- dplyr::bind_rows(
  m2_clean %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id),
  m2_zeros %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id)) %>%
  dplyr::distinct()

m1_surveys_transects <- m1_surveys_blocks %>%
  dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program)

m2_surveys_transects <- m2_surveys_blocks %>%
  dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program)

# Create block B20 ----
m1_b20_blocks <- m1_clean %>%
  dplyr::filter(size_class >= 20) %>%
  ungroup() %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id) %>%
  dplyr::summarise(b20_g = sum(biomass_g)) %>%
  ungroup() %>%
  full_join(m1_surveys_blocks) %>%
  replace_na(list(b20_g = 0))

m2_b20_blocks <- m2_clean %>%
  dplyr::filter(size_class >= 20) %>%
  ungroup() %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id) %>%
  dplyr::summarise(b20_g = sum(biomass_g)) %>%
  ungroup() %>%
  full_join(m2_surveys_blocks) %>%
  replace_na(list(b20_g = 0))
  
# Calculate sample B20 ----
m1_b20_samples <- m1_b20_blocks %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program) %>%
  dplyr::summarise(
    mean_b20_g = mean(b20_g, na.rm = TRUE),
    block_sd = stats::sd(b20_g, na.rm = TRUE),
    n_blocks = dplyr::n_distinct(block),
    .groups = "drop"
  ) %>%
  dplyr::mutate(b20_kg = mean_b20_g/1000) %>%
  dplyr::full_join(m1_surveys_transects) %>%
  dplyr::mutate(
    b20_kg = dplyr::coalesce(b20_kg, 0),
    n_blocks = dplyr::coalesce(n_blocks, 0L)
  )

m2_b20_samples <- m2_b20_blocks %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program) %>%
  dplyr::summarise(
    mean_b20_g = mean(b20_g, na.rm = TRUE),
    block_sd = stats::sd(b20_g, na.rm = TRUE),
    n_blocks = dplyr::n_distinct(block),
    .groups = "drop"
  ) %>%
  dplyr::mutate(b20_kg = mean_b20_g/1000) %>%
  dplyr::full_join(m2_surveys_transects) %>%
  dplyr::mutate(
    b20_kg = dplyr::coalesce(b20_kg, 0),
    n_blocks = dplyr::coalesce(n_blocks, 0L)
  )

hist(m1_b20_samples$b20_kg)
hist(m2_b20_samples$b20_kg)
