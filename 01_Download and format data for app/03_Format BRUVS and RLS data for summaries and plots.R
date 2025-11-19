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

# ---- Load data from Google Sheets ----
temp_scores_sheet <- "https://docs.google.com/spreadsheets/d/1YReZDi7TRzlCTNdU0ganthAWa8TTcfG-eZtIObRM45k/edit?gid=0#gid=0"

# ---- Data loaders ----
scores <-  read_sheet(temp_scores_sheet) 
2

regions_summaries <- read_sheet(temp_scores_sheet, "summary_text") 

# Shapefiles ----
# Reporting regions -----
regions_shp <- st_read("data/spatial/Reporting_regions_30102025.shp", quiet = TRUE) %>%
  dplyr::rename(region = RegionName)

# Ensure WGS84 for Leaflet 
regions_shp <- st_transform(regions_shp, 4326)  # TODO put this in a shapefile list

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

# ---- Color mapping ----
# TODO move this to global instead of here, is very quick to load
ordered_levels <- c("High", "Medium", "Low")

pal_vals <- c(  "High" = "#E74C3C",   # red
                "Medium"      = "#febf26",   # orange
                "Low" = "#3b9243" )   # dark green)

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

2

# TODO brooke to check species names in here e.g. spp, and spelling

# Read in BRUV and RLS data ----
bruv_metadata <- readRDS("data/raw/sa_metadata_bruv.RDS") %>%
  dplyr::rename(latitude_dd = latitude, longitude_dd = longitude, depth_m = depth) %>%
  dplyr::mutate(depth_m = as.numeric(depth_m)) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(date = paste(str_sub(date, 1, 4), str_sub(date, 5, 6), str_sub(date, 7, 8), sep = "-")) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  dplyr::glimpse()

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

rls_metadata_with_regions <- st_join(rls_metadata_locs, reporting_regions) %>%
  glimpse()

bruv_metadata_with_regions <- st_join(bruv_metadata_locs, reporting_regions) %>%
  glimpse()

bloom_temp_campaign <- bruv_metadata_with_regions %>%
  dplyr::filter(campaignid %in% "202110-202205_SA_MarineParkMonitoring_StereoBRUVS") %>%
  dplyr::mutate(campaignid = "Fake campaign") %>%
  dplyr::mutate(date = "2026-01-01")

combined_metadata <- bind_rows(rls_metadata_with_regions %>% dplyr::mutate(method = "UVC"), 
                               bruv_metadata_with_regions %>% dplyr::mutate(method = "BRUVs"),
                               bloom_temp_campaign %>% dplyr::mutate(method = "BRUVs")) %>%
  select(campaignid, sample, date, location, region, geometry, depth_m, method) %>%
  dplyr::mutate(year = as.numeric(str_sub(date, 1, 4))) %>%
  dplyr::mutate(period = if_else(year > 2024, "Bloom", "Pre-bloom"))

unique(combined_metadata$period)

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
bruv_count_regions_pre <- bruv_count %>%
  left_join(bruv_metadata_with_regions) %>%
  dplyr::select(sample, family, genus, species, region, count) %>%
  dplyr::mutate(method = "BRUVs") %>%
  dplyr::mutate(period = "Pre-bloom")

# Temp create some bloom data
bruv_count_regions_post <- bruv_count %>%
  left_join(bruv_metadata_with_regions) %>%
  dplyr::select(sample, family, genus, species, region, count) %>%
  dplyr::filter(campaignid %in% "202110-202205_SA_MarineParkMonitoring_StereoBRUVS") %>%
  dplyr::mutate(campaignid = "Fake campaign") %>%
  dplyr::mutate(date = "2026-01-01") %>%
  dplyr::mutate(method = "BRUVs") %>%
  dplyr::mutate(period = "Bloom")

rls_count_regions_pre <- rls_count %>%
  left_join(rls_metadata_with_regions) %>%
  dplyr::select(sample, family, genus, species, region, count) %>%
  dplyr::mutate(method = "UVC") %>%
  dplyr::mutate(period = "Pre-bloom")

combined_count <- bind_rows(bruv_count_regions_pre, bruv_count_regions_post, rls_count_regions_pre)

hab_number_of_fish <- combined_count %>%
  semi_join(fish_species) %>%
  dplyr::group_by(region, period) %>%
  summarise(number = sum(count)) %>%
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
  summarise(number = min(depth_m)) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

hab_max_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region, period) %>%
  summarise(number = max(depth_m)) %>%
  dplyr::filter(!is.na(region)) %>% 
  glimpse()

# Average depth ----
hab_mean_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region, period) %>%
  summarise(number = mean(depth_m)) %>%
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
  dplyr::group_by(region, period, family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(genus_species = paste(genus, species)) %>%
  dplyr::left_join(dew_species %>% select(genus_species, common_name)) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, common_name, australian_common_name, total_number, region, period) %>%
  dplyr::mutate(common_name = if_else(is.na(common_name), australian_common_name, common_name)) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", common_name, ")")) %>%
  dplyr::group_by(region, period) %>%
  dplyr::slice_max(order_by = total_number, n = 20) %>%
  dplyr::select(region, period, display_name, total_number) 

region_top_species_average <- combined_count %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::group_by(region, period, family, genus, species)%>%
  dplyr::summarise(average = mean(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(genus_species = paste(genus, species)) %>%
  dplyr::left_join(dew_species %>% select(genus_species, common_name)) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, common_name, australian_common_name, average, region, period) %>%
  dplyr::mutate(common_name = if_else(is.na(common_name), australian_common_name, common_name)) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", common_name, ")")) %>%
  dplyr::group_by(region, period) %>%
  dplyr::slice_max(order_by = average, n = 20) %>%
  dplyr::select(region, period, display_name, average) 

# Combined data
hab_data <- structure(
  list(
    
    # Temporary scores from googlesheet
    scores = scores,
    
    # Summary Text
    regions_summaries = regions_summaries,
    
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
    hab_number_bruv_deployments = hab_number_bruv_deployments,
    hab_number_of_fish = hab_number_of_fish,
    hab_number_of_fish_species = hab_number_of_fish_species,
    hab_number_of_nonfish_species = hab_number_of_nonfish_species,
    hab_number_rls_deployments = hab_number_rls_deployments,
    
    # Dataframes
    hab_combined_metadata = combined_metadata,
    region_top_species = region_top_species,
    region_top_species_average = region_top_species_average
  ), class = "data")

save(hab_data, file = here::here("app_data/hab_data.Rdata"))

