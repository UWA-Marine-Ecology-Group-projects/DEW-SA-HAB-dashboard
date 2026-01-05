# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
# library(devtools)
library(dplyr)
library(googlesheets4)
# library(httr)
library(sf)
library(stringr)
library(tidyverse)
# library(RJSONIO)
library(leaflet)

sf_use_s2(FALSE)

# Theme for plotting ----
# ggplot_theme <- 
#   ggplot2::theme_bw() +
#   ggplot2::theme( # use theme_get() to see available options
#     panel.grid = ggplot2::element_blank(),
#     panel.border = ggplot2::element_blank(),
#     axis.line = ggplot2::element_line(colour = "black"),
#     panel.grid.major = ggplot2::element_blank(),
#     panel.grid.minor = ggplot2::element_blank(),
#     legend.background = ggplot2::element_blank(),
#     legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
#     legend.text = ggplot2::element_text(size = 12),
#     legend.title = ggplot2::element_blank(),
#     # legend.position = "top",
#     text = ggplot2::element_text(size = 12),
#     strip.text.y = ggplot2::element_text(size = 12, angle = 0),
#     axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
#     axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
#     axis.text.y = ggplot2::element_text(size = 12),
#     axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
#     axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
#     axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
#     strip.background = ggplot2::element_blank(),
#     
#     strip.text = ggplot2::element_text(size = 14, angle = 0),
#     
#     plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
#   )

# Shapefiles ----
# Reporting regions -----
regions_shp <- st_read("data/spatial/Reporting_regions_30102025.shp", quiet = TRUE) %>%
  dplyr::rename(region = RegionName)

# Ensure WGS84 for Leaflet 
regions_shp <- st_transform(regions_shp, 4326)  # TODO put this in a shapefile list

# Reporting locations ----
locations_shp <- st_read("data/spatial/Locations_SZ_groupings.shp") %>%
  dplyr::rename(reporting_location = LocationNa, reporting_sanctuary = SanctuaryZ) %>%
  dplyr::select(-c(Shape_Leng, Shape_Area)) %>%
  dplyr::mutate(reporting_name = paste(reporting_location, "-", reporting_sanctuary, "Sanctuary Zone", sep = " ")) |>
  dplyr::mutate(reporting_name = str_replace_all(reporting_name, " - NA Sanctuary Zone", ""))

# Ensure WGS84 for Leaflet 
locations_shp <- st_transform(locations_shp, 4326)  # TODO put this in a shapefile list

# Read in state marineparks ----
state_mp <- read_sf("data/spatial/CONSERVATION_StateMarineParkNW_Zoning_GDA94.shp") %>%
  clean_names() %>%
  dplyr::mutate(zone = case_when(
    zone_type %in% "HPZ" ~ "Habitat Protection",
    zone_type %in% "SZ" ~ "Sanctuary (no-take)",
    zone_type %in% "GMUZ" ~ "General Managed Use",
    zone_type %in% "RAZ" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_L" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_D" ~ "Restricted Access (no-take)")) %>% 
  dplyr::mutate(name = paste0(resname, ". Zone: ", zone_name, " (", zone, ")")) 

state_mp$zone <- fct_relevel(state_mp$zone, 
                             "Restricted Access (no-take)", 
                             "Sanctuary (no-take)", 
                             "Habitat Protection", 
                             "General Managed Use")

sa_state_mp <- st_cast(state_mp, "POLYGON")

saveRDS(sa_state_mp, "app_data/spatial/sa_state_mp.RDS") # TODO put this in a shapefile list

# ---- Load data from Google Sheets ----
summary_sheet <- "https://docs.google.com/spreadsheets/d/1YReZDi7TRzlCTNdU0ganthAWa8TTcfG-eZtIObRM45k/edit?gid=0#gid=0"
# 
# # ---- Data loaders ----
# scores <-  read_sheet(temp_scores_sheet) 
# 2

regions_summaries <- read_sheet(summary_sheet, "region_summary_text") 
2

locations_summaries <- read_sheet(summary_sheet, "location_summary_text") %>%
  left_join(., locations_shp) 

# ---- Color mapping ----
# TODO move this to global instead of here, is very quick to load
ordered_levels <- c("High", "Medium", "Low")

# pal_vals <- c(  "High" = "#E74C3C",   # red
#                 "Medium"      = "#febf26",   # orange
#                 "Low" = "#3b9243" )   # dark green)

pal_vals <- c(  "High" = "#EB5757",   # red
                "Medium"      = "#F2C94C",   # orange
                "Low" = "#3B7EA1" )   # dark green)

pal_factor <- colorFactor(palette = pal_vals, domain = ordered_levels, ordered = TRUE)

# Survey tracking ----
survey_plan <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1QxTP_s58cbhLYB4GIuS39wK1c3QfBu8TGbUhV9rD3FY/edit?gid=1319001580#gid=1319001580",
  sheet = "reporting_region_summary")

# Fish Species Lists ----
species_list <- CheckEM::australia_life_history

fish_species <- species_list %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii", "Myxini"))

dew_species <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing")

# TODO brooke to check species names in here e.g. spp, and spelling

# Read in BRUV and RLS data ----
bruv_metadata <- readRDS("data/raw/sa_metadata_bruv.RDS") %>%
  dplyr::rename(latitude_dd = latitude, longitude_dd = longitude, depth_m = depth) %>%
  dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(date = paste(str_sub(date, 1, 4), str_sub(date, 5, 6), str_sub(date, 7, 8), sep = "-")) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  dplyr::glimpse() %>%
  dplyr::mutate(year = str_sub(date, 1, 4)) %>%
  dplyr::mutate(period = if_else(year > 2024, "Bloom", "Pre-bloom")) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::mutate(status = if_else(status %in% "No-Take", "No-take", status))

# unique(bruv_metadata$year) %>% sort()

unique(bruv_metadata$location) %>% sort()

rls_metadata <- readRDS("data/raw/sa_metadata_rls.RDS") %>%
  dplyr::rename(date = survey_date, sample = survey_id) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  glimpse()

bruv_count <- readRDS("data/raw/sa_count_bruv.RDS")
rls_count <- readRDS("data/raw/sa_count_rls.RDS")

bruv_length <- readRDS("data/raw/sa_length_bruv.RDS")
rls_length <- readRDS("data/raw/sa_length_rls.RDS")

# Start to format data ----
# Fix sanctuary locations in the BRUV metadata ----
bruv_metadata_sf <- bruv_metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

bruv_metadata_sf <- st_transform(bruv_metadata_sf, st_crs(state_mp))

bruv_metadata_locs <- st_join(bruv_metadata_sf, state_mp %>% st_cast("POLYGON")) %>%
  dplyr::mutate(location = resname) %>%
  glimpse()

unique(bruv_metadata_locs$location)

# Fix sanctuary locations in the BRUV metadata ----
rls_metadata_sf <- rls_metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

rls_metadata_sf <- st_transform(rls_metadata_sf, st_crs(state_mp))
rls_metadata_locs <- st_join(rls_metadata_sf, state_mp %>% st_cast("POLYGON")) %>%
  dplyr::mutate(location = resname) %>%
  glimpse()

unique(rls_metadata_locs$location)

# Add reporting regions to the metadata ----
reporting_regions <- st_transform(regions_shp, st_crs(state_mp))
reporting_locations <- st_transform(locations_shp, st_crs(state_mp))

rls_metadata_with_regions <- st_join(rls_metadata_locs, reporting_regions) %>%
  st_join(reporting_locations) %>%
  glimpse()

bruv_metadata_with_regions <- st_join(bruv_metadata_locs, reporting_regions) %>%
  st_join(reporting_locations) %>%
  glimpse()

combined_metadata <- bind_rows(rls_metadata_with_regions %>% dplyr::mutate(method = "UVC"), 
                               bruv_metadata_with_regions %>% dplyr::mutate(method = "BRUVs")#,
                               # bloom_temp_campaign %>% dplyr::mutate(method = "BRUVs")
                               ) %>%
  select(campaignid, sample, date, location, region, geometry, depth_m, method, successful_count, successful_length, status, reporting_location, reporting_sanctuary, reporting_name) %>%
  dplyr::mutate(year = as.numeric(str_sub(date, 1, 4))) %>%
  dplyr::mutate(period = if_else(year > 2024, "Bloom", "Pre-bloom")) %>%
  dplyr::filter(!is.na(region))

unique(combined_metadata$period)
unique(combined_metadata$successful_count)
unique(combined_metadata$successful_length)

successful_length_drops <- combined_metadata %>%
  dplyr::filter(successful_length %in% "Yes")

# combined length ----
# bruv_length_regions_post <- bruv_length %>%
#   left_join(bruv_metadata_with_regions) %>%
#   dplyr::select(campaignid, sample, family, genus, species, region, count, length) %>%
#   dplyr::filter(campaignid %in% c("202110-202205_SA_MarineParkMonitoring_StereoBRUVS", "2015-16_SA_MPA_UpperGSV_StereoBRUVS")) %>%
#   dplyr::mutate(campaignid = "Fake campaigns") %>%
#   dplyr::mutate(date = "2026-01-01") %>%
#   dplyr::mutate(method = "BRUVs") %>%
#   dplyr::mutate(period = "Bloom")

combined_length <- bruv_length %>%
  left_join(bruv_metadata_with_regions) %>%
  dplyr::select(campaignid, sample, family, genus, species, region, count, length, date, period, reporting_location, reporting_sanctuary, reporting_name) %>%
  dplyr::mutate(method = "BRUVs") %>%
  semi_join(successful_length_drops)

# Create metrics for dashboard ----
# Number of deploymnets by region ----
hab_number_bruv_deployments <- combined_metadata %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::group_by(period, region) %>%
  dplyr::summarise(number = n()) %>%
  ungroup() %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(region))

hab_number_rls_deployments <- combined_metadata %>%
  dplyr::filter(method %in% "UVC") %>%
  dplyr::group_by(period, region) %>%
  dplyr::summarise(number = n()) %>%
  ungroup() %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(region))

# Number of fish -----
bruv_count_regions <- bruv_count %>%
  left_join(combined_metadata) %>%
  dplyr::select(sample, family, genus, species, region, count, period, reporting_location, reporting_sanctuary, reporting_name) %>%
  dplyr::mutate(method = "BRUVs") %>%
  semi_join(combined_metadata) %>%
  ungroup()

rls_count_regions_pre <- rls_count %>%
  left_join(combined_metadata) %>%
  dplyr::select(sample, family, genus, species, region, count, reporting_location, reporting_sanctuary) %>%
  dplyr::mutate(method = "UVC") %>%
  dplyr::mutate(period = "Pre-bloom") %>%
  semi_join(combined_metadata)

combined_count <- bind_rows(bruv_count_regions, rls_count_regions_pre) %>%
  dplyr::mutate(genus_species = paste(genus, species)) 

hab_number_of_fish <- combined_count %>%
  semi_join(fish_species) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = sum(count)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(region))

# Number of fish species ----
hab_number_of_fish_species <- combined_count %>%
  semi_join(fish_species) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = n_distinct(paste(family, genus, species, sep = "_"))) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

# Number of non-fish species ----
hab_number_of_nonfish_species <- combined_count %>%
  anti_join(fish_species) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = n_distinct(paste(family, genus, species, sep = "_"))) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

# Depths surveyed ----
hab_min_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = min(depth_m)) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

hab_max_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = max(depth_m)) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

# Average depth ----
hab_mean_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(number = mean(depth_m)) %>%
  dplyr::filter(!is.na(region)) %>%
  glimpse()

# Years sampled ----
year_dat <- combined_metadata %>% 
  sf::st_drop_geometry()  %>%
  dplyr::filter(!is.na(region)) %>%
  glimpse()

unique(year_dat$year)

hab_min_year <- year_dat %>%
  group_by(region, period) %>%
  dplyr::summarise(number = min(year)) %>%
  glimpse()

hab_max_year <- year_dat %>%
  group_by(region, period) %>%
  dplyr::summarise(number = max(year)) %>%
  glimpse()

# Data for plots ----
# TODO does this need to be an average per sample?
region_top_species <- combined_count %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::group_by(region, period, family, genus, species, genus_species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(dew_species %>% select(genus_species, common_name)) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, common_name, australian_common_name, total_number, region, period) %>%
  dplyr::mutate(common_name = if_else(is.na(common_name), australian_common_name, common_name)) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", common_name, ")")) %>%
  dplyr::group_by(region, period) %>%
  dplyr::slice_max(order_by = total_number, n = 20) %>%
  dplyr::select(region, period, display_name, total_number)  %>%
  ungroup()

# Have removed family from this because there were lots of species in multiple families!
# region_top_species_average <- combined_count %>%
#   full_join(combined_metadata) %>%
#   dplyr::filter(method %in% "BRUVs") %>%
#   dplyr::mutate(genus = if_else(genus %in% "Unknown", family, genus)) %>%
#   dplyr::select(campaignid, sample, genus, species, genus_species, count) %>%
#   tidyr::complete(nesting(campaignid, sample), nesting(genus, species, genus_species)) %>%
#   dplyr::mutate(id = paste(campaignid, sample)) %>%
#   dplyr::filter(!is.na(species)) %>%
#   replace_na(list(count = 0)) %>%
#   # dplyr::mutate(species_id = paste(genus, species, genus_species)) %>%
#   dplyr::left_join(combined_metadata) %>%
#   dplyr::group_by(region, period, genus, species, genus_species) %>%
#   dplyr::summarise(
#     average = mean(count, na.rm = TRUE),
#     se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(dew_species %>% dplyr::select(genus_species, common_name)) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(genus, species, common_name, australian_common_name, average, se, region, period) %>%
#   dplyr::mutate(common_name = dplyr::if_else(is.na(common_name), australian_common_name, common_name)) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", common_name, ")")) %>%
#   dplyr::group_by(region, period) %>%
#   dplyr::slice_max(order_by = average, n = 20) %>%
#   dplyr::select(region, period, display_name, average, se) %>%
#   dplyr::ungroup() %>%
#   tidyr::complete(tidyr::nesting(region, display_name), period) %>%
#   tidyr::replace_na(list(average = 0, se = 0))

region_top_species_average <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  # keep region/period/status here so we don't have to join later
  dplyr::select(
    campaignid, sample, region, period, status,
    genus, species, genus_species, count
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample, region, period, status),
    tidyr::nesting(genus, species, genus_species)
  ) %>%
  dplyr::filter(!is.na(species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(region, period, status, genus, species, genus_species) %>%
  dplyr::summarise(
    average = mean(count, na.rm = TRUE),
    se      = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    dew_species %>% dplyr::select(genus_species, common_name),
    by = "genus_species"
  ) %>%
  dplyr::left_join(
    species_list,
    by = c("genus", "species")
  ) %>%
  dplyr::select(
    genus, species, common_name, australian_common_name,
    average, se, region, period, status
  ) %>%
  dplyr::mutate(
    common_name = dplyr::if_else(is.na(common_name), australian_common_name, common_name),
    display_name = paste0(genus, " ", species, " (", common_name, ")")
  )

location_top_species_average <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  # keep region/period/status here so we don't have to join later
  dplyr::select(
    campaignid, sample, reporting_name, period, status,
    genus, species, genus_species, count
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample, reporting_name, period, status),
    tidyr::nesting(genus, species, genus_species)
  ) %>%
  dplyr::filter(!is.na(species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(reporting_name, period, status, genus, species, genus_species) %>%
  dplyr::summarise(
    average = mean(count, na.rm = TRUE),
    se      = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    dew_species %>% dplyr::select(genus_species, common_name),
    by = "genus_species"
  ) %>%
  dplyr::left_join(
    species_list,
    by = c("genus", "species")
  ) %>%
  dplyr::select(
    genus, species, common_name, australian_common_name,
    average, se, reporting_name, period, status
  ) %>%
  dplyr::mutate(
    common_name = dplyr::if_else(is.na(common_name), australian_common_name, common_name),
    display_name = paste0(genus, " ", species, " (", common_name, ")")
  )

trophic_groups_samples <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  # keep region/period/status here so we don't have to join later
  dplyr::select(
    campaignid, sample, region, period, status,
    genus, species, genus_species, count
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample, region, period, status),
    tidyr::nesting(genus, species, genus_species)
  ) %>%
  dplyr::filter(!is.na(species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::left_join(dew_species) %>%
  dplyr::mutate(diet = if_else(diet %in% c("NA", NA), "Diet missing", diet)) %>%
  dplyr::group_by(campaignid, sample, region, period, status, diet) %>%
  dplyr::summarise(n_individuals_sample = sum(count))
  
nrow(combined_metadata %>% filter(method %in% "BRUVs")) * 5

trophic_groups_summary <- trophic_groups_samples %>%
  dplyr::group_by(region, period, diet) %>%
  dplyr::summarise(
    mean = mean(n_individuals_sample, na.rm = TRUE),
    se   = sd(n_individuals_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_individuals_sample))),
    num = n(),
    .groups = "drop"
  )

# Calculate Impacts for Trophic Group abundance ----
trophic_groups_impacts <- trophic_groups_summary %>%
  dplyr::select(-se, -num) %>%
  tidyr::complete(region, period, diet) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  clean_names() %>%
  dplyr::mutate(percentage = bloom/pre_bloom*100) %>%
  dplyr::mutate(impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  )) %>%
  mutate(impact_metric = "trophic_group")

# Trophic group species richness
trophic_groups_richness_samples <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::select(
    campaignid, sample, region, period, status,
    genus, species, genus_species, count
  ) %>%
  dplyr::filter(!is.na(species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::left_join(dew_species) %>%
  dplyr::mutate(diet = if_else(diet %in% c("NA", NA), "Diet missing", diet)) %>%
  dplyr::group_by(campaignid, sample, region, period, status, diet) %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct(genus, species) %>% # removed family due to inconsistencies
  dplyr::summarise(n_species_sample = dplyr::n(), .groups = "drop") %>%
  ungroup() %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::select(
    campaignid, sample, region, period, status,
    diet, n_species_sample
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample, region, period, status),
    tidyr::nesting(diet)
  ) %>%
  dplyr::filter(!is.na(diet)) %>%
  tidyr::replace_na(list(n_species_sample = 0))

nrow(combined_metadata %>% filter(method %in% "BRUVs")) * 5

trophic_groups_richness_summary <- trophic_groups_richness_samples %>%
  dplyr::group_by(region, period, diet) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_species_sample))),
    num = n(),
    .groups = "drop"
  )

trophic_groups_richness_summary_status <- trophic_groups_richness_samples %>%
  dplyr::group_by(region, period, status, diet) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_species_sample))),
    num = n(),
    .groups = "drop"
  )


# store as before
# hab_data$region_top_species_average <- region_top_species_average

test <- region_top_species_average %>%
  group_by(region, period, display_name) %>%
  dplyr::summarise(n = n()) %>%
  filter(n > 1)

# Species Richness ----
species_richness_samples <- combined_count %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  
  dplyr::filter(!genus %in% "Unknown") %>%
  dplyr::filter(!species %in% "spp") %>%
  
  dplyr::group_by(region, period, sample) %>%
  dplyr::distinct(genus, species) %>% # removed family due to inconsistencies
  dplyr::summarise(n_species_sample = dplyr::n(), .groups = "drop") %>%
  ungroup() %>%
  dplyr::filter(!is.na(region))%>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  replace_na(list(n_species_sample = 0))

species_richness_summary <- species_richness_samples %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_species_sample))),
    num = n(),
    .groups = "drop"
  )

# Calculate Impacts for Species Richness ----
species_richness_impacts <- species_richness_summary %>%
  dplyr::select(-se, -num) %>%
  tidyr::complete(region, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  clean_names() %>%
  dplyr::mutate(percentage = bloom/pre_bloom*100) %>%
  dplyr::mutate(impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  )) %>%
  mutate(impact_metric = "species_richness")

# Total abundance ----
total_abundance_samples <- combined_count %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::group_by(region, period, sample) %>%
  dplyr::summarise(total_abundance_sample = sum(count), .groups = "drop") %>%
  ungroup() %>%
  dplyr::filter(!is.na(region)) %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  tidyr::replace_na(list(total_abundance_sample = 0))

total_abundance_summary <- total_abundance_samples %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(total_abundance_sample, na.rm = TRUE),
    se   = sd(total_abundance_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(total_abundance_sample))),
    .groups = "drop"
  ) %>%
  ungroup()

# Calculate Impacts for Total Abundance ----
total_abundance_impacts <- total_abundance_summary %>%
  dplyr::select(-se) %>%
  tidyr::complete(region, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  clean_names() %>%
  dplyr::mutate(percentage = bloom/pre_bloom*100) %>%
  dplyr::mutate(impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  )) %>%
  mutate(impact_metric = "total_abundance")

# Shark and Ray richness ----
# This needs to include zeros, to show where no species were observed
# 1. All BRUV samples (one row per region–period–sample)
all_bruv_samples <- combined_metadata %>%
  sf::st_drop_geometry() %>%              # we just need attributes here
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::select(region, period, sample, status) %>%
  dplyr::distinct()

# 2. Shark/ray richness only where they occur
shark_ray_richness_nonzero <- combined_count %>%
  dplyr::filter(count > 0, method %in% "BRUVs") %>%
  dplyr::left_join(species_list, by = c("family", "genus", "species")) %>%
  dplyr::left_join(combined_metadata) %>%
  dplyr::filter(class %in% "Elasmobranchii") %>%
  dplyr::group_by(region, period, status, sample) %>%
  dplyr::distinct(genus, species) %>%
  dplyr::summarise(
    n_species_sample = dplyr::n(),
    .groups = "drop"
  ) %>%
  ungroup()

nrow(shark_ray_richness_nonzero)

# 3. Join back to all samples → fill missing richness with 0
shark_ray_richness_samples <- all_bruv_samples %>%
  dplyr::left_join(
    shark_ray_richness_nonzero,
    by = c("region", "period", "sample", "status")
  ) %>%
  tidyr::replace_na(list(n_species_sample = 0)) %>%
  dplyr::filter(!is.na(region))

# 4. Summarise by region + period
shark_ray_richness_summary <- shark_ray_richness_samples %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_species_sample))),
    .groups = "drop"
  )

# 5. Impact calculation (unchanged)
shark_ray_richness_impacts <- shark_ray_richness_summary %>%
  dplyr::select(-se) %>%
  tidyr::complete(region, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(percentage = bloom / pre_bloom * 100) %>%
  dplyr::mutate(impact = dplyr::case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default       = "Surveys incomplete"
  )) %>%
  dplyr::mutate(impact_metric = "shark_ray_richness")


# Reef associated richness ----
reef_associated_richness_samples <- combined_count %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::left_join(dew_species) %>%
  dplyr::mutate(functional_group %in% "reef-associated") %>%
  dplyr::full_join(combined_metadata %>% dplyr::filter(method %in% "BRUVs")) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(region, period, status, sample, reporting_name) %>%
  dplyr::distinct(genus, species) %>%
  dplyr::summarise(n_species_sample = dplyr::n(), .groups = "drop") %>%
  ungroup() %>%
  dplyr::filter(!is.na(region))

reef_associated_richness_summary <- reef_associated_richness_samples %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(n_species_sample))),
    .groups = "drop"
  ) %>%
  ungroup()

reef_associated_richness_impacts <- reef_associated_richness_summary %>%
  dplyr::select(-se) %>%
  tidyr::complete(region, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  clean_names() %>%
  dplyr::mutate(percentage = bloom/pre_bloom*100) %>%
  dplyr::mutate(impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  )) %>%
  mutate(impact_metric = "reef_associated_richness")

# Fish greater than 200 mm abundance ----

fish_200_abundance_samples <- combined_length %>%
  dplyr::mutate(count = as.numeric(count), length = as.numeric(length)) %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(length > 200) %>%
  dplyr::group_by(region, period, sample, reporting_name) %>%
  dplyr::summarise(total_abundance_sample = sum(count)) %>%
  ungroup() %>%
  dplyr::full_join(combined_metadata %>% dplyr::filter(method %in% "BRUVs")) %>%
  tidyr::replace_na(list(total_abundance_sample = 0))  %>%
  dplyr::filter(!is.na(region)) %>%
  semi_join(successful_length_drops)

fish_200_abundance_summary <- fish_200_abundance_samples %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(total_abundance_sample, na.rm = TRUE),
    se   = sd(total_abundance_sample, na.rm = TRUE) /
      sqrt(sum(!is.na(total_abundance_sample))),
    .groups = "drop"
  ) %>%
  ungroup()

fish_200_abundance_impacts <- fish_200_abundance_summary %>%
  dplyr::select(-se) %>%
  tidyr::complete(region, period) %>%
  tidyr::pivot_wider(names_from = period, values_from = mean) %>%
  clean_names() %>%
  dplyr::mutate(percentage = bloom/pre_bloom*100) %>%
  dplyr::mutate(impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  )) %>%
  mutate(impact_metric = "fish_200_abundance")


impact_data <- bind_rows(species_richness_impacts, 
                         total_abundance_impacts,
                         shark_ray_richness_impacts,
                         reef_associated_richness_impacts,
                         fish_200_abundance_impacts#,
                         #trophic_groups_impacts
                         )

overall_impact <- impact_data %>%
  group_by(region) %>%
  dplyr::summarise(percentage = mean(percentage)) %>%
  ungroup() %>%
  dplyr::mutate(overall_impact = case_when(
    percentage > 80 ~ "Low",
    percentage > 50 & percentage < 80 ~ "Medium",
    percentage < 50 ~ "High",
    .default = "Surveys incomplete"
  ))


# Location data ----
all_bruv_samples_loc <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::select(reporting_name, period, sample, status) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(reporting_name))

calc_impacts <- function(summary_df, group_col, metric_id) {
  summary_df %>%
    dplyr::select({{ group_col }}, period, mean) %>%
    tidyr::complete({{ group_col }}, period) %>%
    tidyr::pivot_wider(names_from = period, values_from = mean) %>%
    CheckEM::clean_names() %>%
    dplyr::mutate(
      percentage = bloom / pre_bloom * 100,
      impact = dplyr::case_when(
        is.na(pre_bloom) | is.na(bloom) ~ "Surveys incomplete",
        percentage > 80                 ~ "Low",
        percentage > 50 & percentage < 80 ~ "Medium",
        percentage < 50                 ~ "High",
        TRUE                            ~ "Surveys incomplete"
      ),
      impact_metric = metric_id
    )
}

species_richness_summary_location <- species_richness_samples %>%
  dplyr::filter(!is.na(reporting_name)) %>%   # reporting_name exists after your full_join(combined_metadata)
  dplyr::group_by(reporting_name, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
    num  = dplyr::n(),
    .groups = "drop"
  )

species_richness_impacts_location <- calc_impacts(
  summary_df = species_richness_summary_location,
  group_col  = reporting_name,
  metric_id  = "species_richness"
)

total_abundance_summary_location <- total_abundance_samples %>%
  dplyr::filter(!is.na(reporting_name)) %>%
  dplyr::group_by(reporting_name, period) %>%
  dplyr::summarise(
    mean = mean(total_abundance_sample, na.rm = TRUE),
    se   = sd(total_abundance_sample, na.rm = TRUE) / sqrt(sum(!is.na(total_abundance_sample))),
    .groups = "drop"
  )

total_abundance_impacts_location <- calc_impacts(
  summary_df = total_abundance_summary_location,
  group_col  = reporting_name,
  metric_id  = "total_abundance"
)

shark_ray_richness_nonzero_loc <- combined_count %>%
  dplyr::filter(count > 0, method %in% "BRUVs") %>%
  dplyr::left_join(species_list) %>%
  dplyr::left_join(combined_metadata %>% sf::st_drop_geometry()) %>%  # sample should identify the deployment
  dplyr::filter(class %in% "Elasmobranchii") %>%
  dplyr::filter(!is.na(reporting_name)) %>%
  dplyr::group_by(reporting_name, period, status, sample) %>%
  dplyr::distinct(genus, species) %>%
  dplyr::summarise(n_species_sample = dplyr::n(), .groups = "drop")

shark_ray_richness_samples_location <- all_bruv_samples_loc %>%
  dplyr::left_join(
    shark_ray_richness_nonzero_loc,
    by = c("reporting_name", "period", "sample", "status")
  ) %>%
  tidyr::replace_na(list(n_species_sample = 0))

shark_ray_richness_summary_location <- shark_ray_richness_samples_location %>%
  dplyr::group_by(reporting_name, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
    .groups = "drop"
  )

shark_ray_richness_impacts_location <- calc_impacts(
  summary_df = shark_ray_richness_summary_location,
  group_col  = reporting_name,
  metric_id  = "shark_ray_richness"
)

reef_associated_richness_summary_location <- reef_associated_richness_samples %>%
  dplyr::filter(!is.na(reporting_name)) %>%
  dplyr::group_by(reporting_name, period) %>%
  dplyr::summarise(
    mean = mean(n_species_sample, na.rm = TRUE),
    se   = sd(n_species_sample, na.rm = TRUE) / sqrt(sum(!is.na(n_species_sample))),
    .groups = "drop"
  )

reef_associated_richness_impacts_location <- calc_impacts(
  summary_df = reef_associated_richness_summary_location,
  group_col  = reporting_name,
  metric_id  = "reef_associated_richness"
)

fish_200_abundance_summary_location <- fish_200_abundance_samples %>%
  dplyr::filter(!is.na(reporting_name)) %>%
  dplyr::group_by(reporting_name, period) %>%
  dplyr::summarise(
    mean = mean(total_abundance_sample, na.rm = TRUE),
    se   = sd(total_abundance_sample, na.rm = TRUE) / sqrt(sum(!is.na(total_abundance_sample))),
    .groups = "drop"
  )

fish_200_abundance_impacts_location <- calc_impacts(
  summary_df = fish_200_abundance_summary_location,
  group_col  = reporting_name,
  metric_id  = "fish_200_abundance"
)

impact_data_location <- dplyr::bind_rows(
  species_richness_impacts_location,
  total_abundance_impacts_location,
  shark_ray_richness_impacts_location,
  reef_associated_richness_impacts_location,
  fish_200_abundance_impacts_location
)

overall_impact_location <- impact_data_location %>%
  dplyr::group_by(reporting_name) %>%
  dplyr::summarise(percentage = mean(percentage, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    overall_impact = dplyr::case_when(
      is.na(percentage)           ~ "Surveys incomplete",
      percentage > 80             ~ "Low",
      percentage > 50 & percentage < 80 ~ "Medium",
      percentage < 50             ~ "High",
      TRUE                        ~ "Surveys incomplete"
    )
  )

# hab_metric_change_location <- impact_data_location %>%
#   dplyr::transmute(
#     reporting_name,
#     impact_metric,
#     percentage_change = dplyr::case_when(
#       is.na(percentage) ~ "Surveys incomplete",
#       TRUE ~ as.character(round(percentage - 100, 1))  # e.g. +20 means 120% of pre
#     )
#   )

# Combined data
hab_data <- structure(
  list(
    # 
    # # Temporary scores from googlesheet
    # scores = scores,
    
    # Summary Text
    regions_summaries = regions_summaries,
    locations_summaries = locations_summaries,
    
    # Shapefiles
    regions_shp = regions_shp,  # TODO put this in a shapefile list with state_mp
    
    # For plotting
    pal_vals = pal_vals, # TODO move this to global instead of here, is very quick to load
    pal_factor = pal_factor, # TODO move this to global instead of here, is very quick to load
    ordered_levels = ordered_levels, # TODO move this to global instead of here, is very quick to load
    
    # Googlesheet tracker of survey plans
    survey_plan = survey_plan,
    
    # DFs for valueboxes
    hab_max_depth = hab_max_depth,
    hab_max_year = hab_max_year,
    hab_mean_depth = hab_mean_depth,
    hab_min_depth = hab_min_depth,
    hab_min_year = hab_min_year,
    year_dat = year_dat,
    hab_number_bruv_deployments = hab_number_bruv_deployments,
    hab_number_of_fish = hab_number_of_fish,
    hab_number_of_fish_species = hab_number_of_fish_species,
    hab_number_of_nonfish_species = hab_number_of_nonfish_species,
    hab_number_rls_deployments = hab_number_rls_deployments,
    
    # Dataframes
    hab_combined_metadata = combined_metadata,
    region_top_species = region_top_species,
    region_top_species_average = region_top_species_average,
    
    location_top_species_average = location_top_species_average,
    
    impact_data = impact_data,
    overall_impact = overall_impact,
    
    impact_data_location = impact_data_location,
    overall_impact_location = overall_impact_location,
    # hab_metric_change_location = hab_metric_change_location,
    
    # Tabset plots
    species_richness_samples = species_richness_samples,
    species_richness_summary = species_richness_summary,
    
    total_abundance_samples = total_abundance_samples,
    total_abundance_summary = total_abundance_summary,
    
    shark_ray_richness_samples = shark_ray_richness_samples,
    shark_ray_richness_summary = shark_ray_richness_summary,
    
    reef_associated_richness_samples = reef_associated_richness_samples,
    reef_associated_richness_summary = reef_associated_richness_summary,
    
    fish_200_abundance_samples = fish_200_abundance_samples,
    fish_200_abundance_summary = fish_200_abundance_summary,
    
    trophic_groups_summary = trophic_groups_summary,
    trophic_groups_samples = trophic_groups_samples,
    
    trophic_groups_richness_summary = trophic_groups_richness_summary,
    trophic_groups_richness_samples = trophic_groups_richness_samples,
    
    trophic_groups_richness_summary_status = trophic_groups_richness_summary_status
    
  ), class = "data")

save(hab_data, file = here::here("app_data/hab_data.Rdata"))

