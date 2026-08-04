#################################################################
# Format and Clean RLS metadata

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

# Functions ----
# Groups sampling dates into the same sampling event if they are within 1 week of eachother
# Groups sampling dates into the same sampling event if they are within
# one week of each other, and assigns the first date of each event
add_sampling_event <- function(data) {
  data %>%
    arrange(site_name, survey_date) %>%
    group_by(site_name) %>%
    mutate(
      sampling_event = cumsum(
        is.na(lag(survey_date)) |
          survey_date - lag(survey_date) > 7
      )
    ) %>%
    group_by(site_name, sampling_event) %>%
    mutate(
      sampling_event_start_date = min(survey_date, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      period = if_else(
        sampling_event_start_date < as.Date("2025-03-01"),
        "Pre-bloom",
        "Bloom"
      ),
      start_year_month = format(
        sampling_event_start_date,
        "%Y-%m"
      ),
      period_split = case_when(
        period == "Bloom" ~ paste("Bloom", start_year_month),
        TRUE ~ period
      )
    )
}

# Sites from DEW ----
sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

# Read in survey list data ----
survey_list <- read_csv("data/raw/RLS/ep_survey_list.csv") %>%
  dplyr::filter(site_code %in% unique(sa_sites$site_code)) %>%
  dplyr::mutate(methods = 
                  if_else((site_code %in% "GSV191" & survey_date %in% c("2005-04-11", "2005-04-12")), "2", methods))

# NOTE survey list does not have block - assume they always have 2?
length(unique(survey_list$survey_id))
length(unique(survey_list$survey_id)) * 2

unique(survey_list$methods)

# Split survey list into method specific lists ---
survey_list_expanded <- survey_list %>%
  separate_rows(methods, sep = ",\\s*") %>%
  mutate(survey_date = as.Date(survey_date))

unique(survey_list_expanded$methods) %>% sort()

# NOTE Method 12 is a debris survey, 13 is PQ data, and method 0 is off transect sightings 31/07

sl_m1_raw <- survey_list_expanded %>%
  filter(methods == 1) #%>%
# dplyr::filter()

sl_m2_raw <- survey_list_expanded %>%
  filter(methods == 2)

sl_m3_raw <- survey_list_expanded %>%
  filter(methods == 3)

# Basic checks ----
check <- survey_list %>%
  distinct(survey_id, site_code, survey_date, depth) %>%
  group_by(site_code, survey_date) %>%
  summarise(n = n())

hist(check$n)

plot(survey_list$survey_date, survey_list$depth)

unique(survey_list$depth) # not always 1-4 as the transect some 0, 8 and 9's

# Check for sampling events - transects that are split over multiple days
# e.g. Corny Point Outside in Feb 2004 was sampled on the 10th and 11th (should be grouped into one sampling event) 

dates_m1 <- sl_m1_raw %>%
  distinct(site_name, survey_date) %>%
  add_sampling_event()

# check dups
dates_m1 %>%
  group_by(site_name, sampling_event) %>%
  summarise(n = n()) %>%
  filter(n > 1)

dates_m2 <- sl_m2_raw %>%
  distinct(site_name, survey_date) %>%
  add_sampling_event()

# check dups
dates_m2 %>%
  group_by(site_name, sampling_event) %>%
  summarise(n = n()) %>%
  filter(n > 1)

dates_m3 <- sl_m3_raw %>%
  distinct(site_name, survey_date) %>%
  add_sampling_event()

# check dups
dates_m3 %>%
  group_by(site_name, sampling_event) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# Survey lits with sampling event ----
cols_to_keep <- c("survey_id", "location", "mpa", "site_code", "site_name", 
                  "latitude", "longitude", "depth", "survey_date", "sampling_event", "program",
                  "period", "start_year_month", "period_split", "sampling_event_start_date")

sl_m1 <- left_join(sl_m1_raw, dates_m1, by = c("site_name", "survey_date")) %>%
  select(all_of(cols_to_keep)) %>%
  tidyr::uncount(weights = 2, .id = "block") %>%
  dplyr::filter(!survey_id %in% c("923406553", "923406567")) # Lost data sheet - have removed

# For ATRC M2, only 1 block before 2016
sl_m2 <- left_join(sl_m2_raw, dates_m2, by = c("site_name", "survey_date")) %>%
  select(all_of(cols_to_keep)) %>%
  # tidyr::uncount(weights = 2, .id = "block")
  tidyr::uncount(weights = if_else(program == "ATRC" & survey_date < as.Date("2016-01-01"), 1L, 2L), .id = "block") %>%
  dplyr::filter(!survey_id %in% c("923406553", "923406567")) # Lost data sheet - have removed
# was 3698 rows with fix = 3402

sl_m2 %>%
  count(program, survey_date, survey_id, name = "n_blocks") %>%
  count(program, survey_date < as.Date("2016-01-01"), n_blocks)

sl_m3 <- left_join(sl_m3_raw, dates_m3, by = c("site_name", "survey_date")) %>%
  select(all_of(cols_to_keep)) %>%
  tidyr::uncount(weights = 2, .id = "block") # TODO check habitat method, if it has two blocks

# # Remove intermediate objects from the environment ----
# rm(sl_m1_raw, sl_m2_raw, sl_m3_raw, check, dates_m1, dates_m2, dates_m3, sa_sites, survey_list, survey_list_expanded)

names(sl_m1)

# Save tidy dataframes ----
write_rds(sl_m1, "data/tidy/rls_m1_survey_list.rds")
write_rds(sl_m2, "data/tidy/rls_m2_survey_list.rds")
write_rds(sl_m3, "data/tidy/rls_m3_survey_list.rds")
