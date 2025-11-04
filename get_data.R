#####################################################################
# Project: API query for Australian BRUV synthesis dashboard
# Data:    2024 BRUV Syntheses metadata, count, length and covariates
# Task:    Use GlobalArchive API to query data and save as an RDS
# Author:  Brooke Gibbons
# Date:    July 2024
#####################################################################

# Other Constraints
# The citation in a list of references is: "Reef Life Survey (RLS); Institute for Marine and Antarctic Studies (IMAS); Parks Victoria; Department of Primary Industries (DPI), New South Wales Government; Parks and Wildlife Tasmania; Department for Environment and Water (DEWNR), South Australia, Integrated Marine Observing System (IMOS), [year-of-data-download], IMOS - National Reef Monitoring Network - Survey metadata, [data-access-URL], accessed [date-of-access]."
# 
# Please also cite the associated data paper when using this data: Edgar, G., Stuart-Smith, R. Systematic global assessment of reef fish communities by the Reef Life Survey program. Sci Data 1, 140007 (2014). https://doi.org/10.1038/sdata.2014.6
# 
# Please also utilise and cite this paper when using this data: Graham J. Edgar, Antonia Cooper, Susan C. Baker, William Barker, Neville S. Barrett, Mikel A. Becerro, Amanda E. Bates, Danny Brock, Daniela M. Ceccarelli, Ella Clausius, Marlene Davey, Tom R. Davis, Paul B. Day, Andrew Green, Samuel R. Griffiths, Jamie Hicks, Ivan A. Hinojosa, Ben K. Jones, Stuart Kininmonth, Meryl F. Larkin, Natali Lazzari, Jonathan S. Lefcheck, Scott D. Ling, Peter Mooney, Elizabeth Oh, Alejandro Perez-Matus, Jacqueline B. Pocklington, Rodrigo Riera, Jose A. Sanabria-Fernandez, Yanir Seroussi, Ian Shaw, Derek Shields, Joe Shields, Margo Smith, German A. Soler, Jemina Stuart-Smith, John Turnbull, Rick D. Stuart-Smith. Establishing the ecological basis for conservation of shallow marine life using Reef Life Survey, Biological Conservation, Volume 252, 2020, https://doi.org/10.1016/j.biocon.2020.108855
# 
# Please also utilise and cite this paper when using this data: Edgar, G., & Barrett, N. (2012). An assessment of population responses of common inshore fishes and invertebrates following declaration of five Australian marine protected areas. Environmental Conservation, 39(3), 271-281. doi:10.1017/S0376892912000185
# 
# Usage Constraints
# Data, products and services from IMOS are provided "as is" without any warranty as to fitness for a particular purpose.
# 
# By using this data you are accepting the license agreement and terms specified above. You accept all risks and responsibility for losses, damages, costs and other consequences resulting directly or indirectly from using this site and any information or material available from it.

# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
library(sf)
library(googlesheets4)

sf_use_s2(FALSE)

# Theme for plotting ----
ggplot_theme <- 
  ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(size = 14, angle = 0),
    
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )

# Read in marineparks ----
state.mp <- read_sf("data/spatial/CONSERVATION_StateMarineParkNW_Zoning_GDA94.shp") %>%
  clean_names() %>%
  dplyr::mutate(zone = case_when(
    zone_type %in% "HPZ" ~ "Habitat Protection",
    zone_type %in% "SZ" ~ "Sanctuary (no-take)",
    zone_type %in% "GMUZ" ~ "General Managed Use",
    zone_type %in% "RAZ" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_L" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_D" ~ "Restricted Access (no-take)"
  )) %>% 
  dplyr::mutate(name = paste0(resname, ". Zone: ", zone_name, " (", zone, ")")) 

unique(state.mp$zone)
unique(state.mp$name)

state.mp$zone <- fct_relevel(state.mp$zone, 
                             "Restricted Access (no-take)", 
                             "Sanctuary (no-take)", 
                             "Habitat Protection", 
                             "General Managed Use")

sa.state.mp <- st_cast(state.mp, "POLYGON")

unique(sa.state.mp$zone)

saveRDS(sa.state.mp, "app_data/spatial/sa.state.mp.RDS")

# Set your API token to access GlobalArchive data shared with you ----
# It is extremely important that you keep your API token out of your scripts, and github repository!
# This function will ask you to put your API token in the console
# It will then create a folder in your project folder called "secrets" and saves your API token to use in the functions later
# The function adds the token into the .gitignore file so it will never be put in version control with Git
CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# # Load the metadata, count and length ----
# # This way does not include the zeros where a species isn't present - it returns a much smaller dataframe
# CheckEM::ga_api_all_data(synthesis_id = "19",
#                          token = token,
#                          dir = "data/raw/",
#                          include_zeros = FALSE)

rls <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv", skip = 71) %>%
  dplyr::select(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth, family, reporting_name, size_class, total) %>%
  tidyr::separate(reporting_name, into = c("genus", "species"), remove = FALSE) %>%
  dplyr::rename(count = total, depth_m = depth) %>%
  glimpse()

rls_metadata <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv", skip = 71) %>%
  dplyr::rename(depth_m = depth, latitude_dd = latitude, longitude_dd = longitude) %>%
  glimpse()

# TODO Need to fix synonyms
synonyms_in_rls <- dplyr::left_join(rls, CheckEM::aus_synonyms)  %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(!is.na(genus_correct)) %>%
  dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
  dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
  dplyr::select('old name', 'new name') %>% # taken out sample
  dplyr::distinct()

rls_with_synonyms_changed <- dplyr::left_join(rls, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  mutate(family = str_replace_all(family, "[^[:alnum:]]", "")) %>%
  mutate(genus = str_replace_all(genus, "[^[:alnum:]]", "")) %>%
  mutate(species = str_replace_all(species, c("[^[:alnum:]]" = "", "pusillusdoriferus" = "pusillus doriferus"))) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::group_by(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth_m, family, genus, species, scientific) %>%
  dplyr::slice(which.max(count)) %>%
  ungroup()

# Species not in list ----
# TODO should check regions too
count_species_not_in_list <- rls_with_synonyms_changed %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct(family, genus, species) 

# TODO fix these up with synonyms!

rls_count <- rls_with_synonyms_changed %>%
  glimpse()

rls_length <- rls_with_synonyms_changed %>%
  dplyr::mutate(length_mm = 10 * size_class) %>%
  glimpse()

# The campaigns we care about are:
campaign_list <- c("2015-16_SA_MPA_UpperGSV_StereoBRUVS",
                   "2015-201706_SA_MPA_StereoBRUVS",
                   "201712-201806_SA_MarineParkMonitoring_StereoBRUVS",
                   "201810-201903_SA_DEW_SAWater_Penneshaw_StereoBRUVS",
                   "201811_Comm_PearsonExpedition_StereoBRUVS",
                   "201811_SA_MPA_PearsonExpedition",
                   "201812-201906_SA_MarineParkMonitoring_StereoBRUVS",
                   "201905-201909_SA_DEW_SAWater_Sleaford Monitoring",
                   "202008-202008_SA_DEW_SAWater Monitoring_StereoBRUVS",
                   "202011-202011_SA Commonwealth Marine Park Monitoring_StereoBRUVS",
                   "202012-202105_SA_MarineParkMonitoring_StereoBRUVS",
                   "202110-202205_SA_MarineParkMonitoring_StereoBRUVS",
                   "202111-202205_SA Commonwealth Marine Park Monitoring_StereoBRUVS")


## Load in data again to save time ----
metadata <- readRDS("data/raw/metadata.RDS") %>% filter(campaignid %in% c(campaign_list))
count <- readRDS("data/raw/count.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
length <- readRDS("data/raw/length.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
benthos <- readRDS("data/raw/benthos_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))
relief <- readRDS("data/raw/relief_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))

species_list <- ga_api_species_list(token = token)

fish_species <- species_list %>%
  dplyr::filter(class_value %in% c("Actinopterygii", "Elasmobranchii", "Myxini"))

unique(species_list$class_value)

# Fix locations in the metadata ----
metadata_sf <- metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

metadata_sf <- st_transform(metadata_sf, st_crs(state.mp))
metadata_locs <- st_join(metadata_sf, state.mp %>% st_cast("POLYGON")) %>%
  dplyr::mutate(location = resname) %>%
  glimpse()

unique(metadata_locs$location)

rls_metadata_sf <- rls_metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

rls_metadata_sf <- st_transform(rls_metadata_sf, st_crs(state.mp))
rls_metadata_locs <- st_join(rls_metadata_sf, state.mp %>% st_cast("POLYGON")) %>%
  dplyr::mutate(location = resname) %>%
  glimpse()

unique(rls_metadata_locs$location)

# Get HAB regions ----
hab_regions <- read_sf("data/spatial/Reporting_regions_30102025.shp") %>%
  dplyr::rename(region = RegionName) %>%
  glimpse()

hab_regions <- st_transform(hab_regions, st_crs(state.mp))

rls_metadata_with_regions <- st_join(rls_metadata_locs, hab_regions) %>%
  glimpse()

metadata_with_regions <- st_join(metadata_locs, hab_regions) %>%
  glimpse()

combined_metadata <- bind_rows(rls_metadata_with_regions %>% dplyr::mutate(method = "UVC"), 
                               metadata_with_regions %>% dplyr::mutate(method = "BRUVs")) %>%
  select(sample_url, survey_id, date_time, survey_date, location, region, geometry, depth_m, method)

names(combined_metadata) %>% sort()

# # Create metrics for dashboard ----
# # Number of deployments ----
# number_of_deployments <- nrow(metadata)
# number_of_deployments
# 
# number_of_deployments_rls <- nrow(rls_metadata)

# HAB -----

hab_number_bruv_deployments <- metadata_with_regions %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(number = n()) %>%
  ungroup() %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(region))

hab_number_rls_deployments <- rls_metadata_with_regions %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(number = n()) %>%
  ungroup() %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(!is.na(region))


# Number of fish -----
bruv_count_regions <- count %>%
  left_join(metadata_with_regions) %>%
  dplyr::select(sample_url, family, genus, species, region, count)

rls_count_regions <- rls_count %>%
  left_join(rls_metadata_with_regions) %>%
  dplyr::select(survey_id, family, genus, species, region, count)# %>%
# dplyr::rename(sample_url = survey_id)

combined_count <- bind_rows(bruv_count_regions, rls_count_regions)

hab_number_of_fish <- combined_count %>%
  semi_join(fish_species) %>%
  dplyr::group_by(region) %>%
  summarise(number = sum(count)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(region))

# Number of fish species ----
hab_number_of_fish_species <- combined_count %>%
  semi_join(fish_species) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(number = n_distinct(paste(family, genus, species, sep = "_"))) %>%
  dplyr::filter(!is.na(region))

# Number of non-fish species ----
hab_number_of_nonfish_species <- combined_count %>%
  anti_join(fish_species) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(number = n_distinct(paste(family, genus, species, sep = "_"))) %>%
  dplyr::filter(!is.na(region))

# # Number of length measurements -----
# number_of_measurements <- length %>%
#   semi_join(fish_species) %>%
#   dplyr::filter(!is.na(length_mm)) %>%
#   summarise(number_of_measurements = sum(count)) %>%
#   pull()
# 
# number_of_measurements
# 
# number_of_measurements_rls <- rls_length %>%
#   semi_join(fish_species) %>%
#   dplyr::filter(!is.na(length_mm)) %>%
#   summarise(number_of_measurements = sum(count)) %>%
#   pull()
# 
# number_of_measurements_rls

# Depths surveyed ----
hab_min_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region) %>%
  summarise(number = min(depth_m)) %>%
  dplyr::filter(!is.na(region))

hab_max_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region) %>%
  summarise(number = max(depth_m)) %>%
  dplyr::filter(!is.na(region))

# Average depth ----
hab_mean_depth <- combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(!depth_m == 0) %>%
  dplyr::group_by(region) %>%
  summarise(number = mean(depth_m)) %>%
  dplyr::filter(!is.na(region))

# Years sampled ----
year_dat <- combined_metadata %>% 
  dplyr::mutate(date_time = if_else(is.na(date_time), as.character(survey_date), as.character(date_time))) %>%
  dplyr::mutate(year = str_sub(date_time, 1, 4))%>%
  sf::st_drop_geometry()  %>%
  dplyr::filter(!is.na(region))

unique(year_dat$year)

hab_min_year <- year_dat %>%
  group_by(region) %>%
  dplyr::summarise(number = min(year))

hab_max_year <- year_dat %>%
  group_by(region) %>%
  dplyr::summarise(number = max(year))

# # Deployments with benthos ----
# deployments_benthos <- benthos %>%
#   distinct(sample_url) %>%
#   nrow(.)
# 
# deployments_relief <- relief %>%
#   distinct(sample_url) %>%
#   nrow(.)

# Dataframes for plotting -----
simple_metadata <- metadata %>%
  distinct(sample_url, date_time, depth_m)

rls_simple_metadata <- rls_metadata %>%
  distinct(survey_id, survey_date, depth_m)

# # Deployment locations ----
# deployment_locations <- metadata %>%
#   distinct(sample_url, campaignid, sample, latitude_dd, longitude_dd, date_time, depth_m, location) %>%
#   dplyr::mutate(year = as.numeric(str_sub(date_time, 1, 4))) %>%
#   dplyr::mutate(popup = paste0(
#     "<b>Campaign:</b> ", campaignid, "<br/>",
#     "<b>Sample:</b> ", sample, "<br/>",
#     "<b>Year:</b> ", year, "<br/>",
#     "<b>Depth (m):</b> ", round(depth_m, 1))) %>%
#   dplyr::select(sample_url, depth_m, popup, longitude_dd, latitude_dd, year) %>%
#   left_join(metadata_locs)
# 
# deployment_locations <- st_as_sf(deployment_locations, coords = c("longitude_dd", "latitude_dd")) %>%
#   dplyr::mutate(
#     longitude_dd = sf::st_coordinates(.)[, 1],
#     latitude_dd  = sf::st_coordinates(.)[, 2]
#   ) %>% mutate(method = "stereo-BRUVs")
# 
# names(deployment_locations)
# 
# mean_lon <- mean(metadata$longitude_dd)
# mean_lat <- mean(metadata$latitude_dd)
# unique(deployment_locations$location)
# 
# # Deployment locations RLS ----
# deployment_locations_rls <- rls_metadata %>%
#   distinct(survey_id, location, site_code, site_name, latitude_dd, longitude_dd, survey_date, depth_m) %>%
#   dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
#   dplyr::mutate(popup = paste0(
#     "<b>Location:</b> ", location, "<br/>",
#     "<b>Site code:</b> ", site_code, "<br/>",
#     "<b>Site name:</b> ", site_name, "<br/>",
#     "<b>Year:</b> ", year, "<br/>",
#     "<b>Depth (m):</b> ", round(depth_m, 1))) %>%
#   dplyr::select(survey_id, depth_m, popup, longitude_dd, latitude_dd, year) %>%
#   left_join(rls_metadata_locs)
# 
# deployment_locations_rls = st_as_sf(deployment_locations_rls, coords = c("longitude_dd", "latitude_dd")) %>%
#   dplyr::mutate(
#     longitude_dd = sf::st_coordinates(.)[, 1],
#     latitude_dd  = sf::st_coordinates(.)[, 2]
#   )%>% mutate(method = "UVC")
# 
# names(deployment_locations_rls)
# 
# mean_lon_rls <- mean(rls_metadata$longitude_dd)
# mean_lat_rls <- mean(rls_metadata$latitude_dd)
# 
# unique(deployment_locations_rls$location)
# 
# # Top 20 species ----
# top_species <- count %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(family, genus, species, australian_common_name, total_number) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::slice_max(order_by = total_number, n = 20) %>%
#   dplyr::select(display_name, total_number) %>%
#   dplyr::mutate(method = "stereo-BRUVs")
# 
# names(top_species)
# top_species
# 
# top_species_rls <- rls_count %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(family, genus, species, australian_common_name, total_number) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::slice_max(order_by = total_number, n = 20) %>%
#   dplyr::select(display_name, total_number) %>%
#   dplyr::mutate(method = "UVC")
# 
# names(top_species_rls)
# top_species_rls
# 
# top_species_combined <- bind_rows(top_species, top_species_rls)
# 
# top_species_bruvs_location <- count %>% 
#   left_join(metadata_locs) %>%
#   dplyr::group_by(location, family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(location) %>%
#   dplyr::slice_max(order_by = total_number, n = 20) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(method = "stereo-BRUVs")
# 
# top_species_rls_location <- rls_count %>%
#   dplyr::select(!location) %>%
#   left_join(rls_metadata_locs) %>%
#   dplyr::group_by(location, family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(location) %>%
#   dplyr::slice_max(order_by = total_number, n = 20) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(method = "UVC")
# 
# top_species_location <- bind_rows(top_species_bruvs_location, top_species_rls_location) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::select(display_name, total_number, method, location) 
# 
# # Top 200 species names ----
# top_species_names <- count %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(family, genus, species, australian_common_name, total_number) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   ungroup() %>%
#   dplyr::slice_max(order_by = total_number, n = 200) %>%
#   dplyr::pull(display_name)
# 
# top_species_bruv <- count %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(family, genus, species, australian_common_name, total_number) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))
# 
# top_species_rls <- rls_count %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(total_number = sum(count)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(family, genus, species, australian_common_name, total_number) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))
# 
# top_species_names_combined <- bind_rows(top_species_bruv, top_species_rls) %>%
#   dplyr::group_by(display_name, family, genus, species) %>%
#   dplyr::summarise(total_number = sum(total_number)) %>%
#   ungroup() %>%
#   dplyr::slice_max(order_by = total_number, n = 500) %>%
#   dplyr::pull(display_name)
# 
# # Bubble data ----
# bubble_data <- count %>%
#   dplyr::left_join(metadata_locs) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::select(sample_url, display_name, count, location) %>%
#   dplyr::glimpse()
# 
# bubble_data_rls <- rls_count %>%
#   dplyr::select(-c(location )) %>%
#   dplyr::left_join(rls_metadata_locs) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::select(survey_id, display_name, count, location) %>%
#   dplyr::glimpse()
# 
# # Total abundance and species richness bubble data ----
# metric_bubble_data <- count %>%
#   dplyr::left_join(metadata) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(sample_url, family, genus, species, count, longitude_dd, latitude_dd, date_time, depth_m, status, successful_count, australian_common_name, location) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::group_by(sample_url) %>%
#   dplyr::summarise(total_abundance = sum(count, na.rm = TRUE),
#                    species_richness = n_distinct(family, genus, species)) %>%
#   full_join(metadata) %>%
#   replace_na(list(total_abundance = 0, species_richness = 0)) %>%
#   dplyr::select(sample_url, total_abundance, species_richness) %>%
#   pivot_longer(!c(sample_url), names_to = "metric", values_to = "value") %>%
#   dplyr::left_join(metadata_locs) %>%
#   left_join(metadata %>% select(!location)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(sample_url, metric, value, longitude_dd, latitude_dd, location) %>%
#   glimpse()
# 
# # Total abundance and species richness bubble data RLS----
# metric_bubble_data_rls <- rls_count %>%
#   dplyr::left_join(rls_metadata) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::select(survey_id, family, genus, species, count, longitude_dd, latitude_dd, depth_m, australian_common_name) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::group_by(survey_id) %>%
#   dplyr::summarise(total_abundance = sum(count, na.rm = TRUE),
#                    species_richness = n_distinct(family, genus, species)) %>%
#   full_join(rls_metadata) %>%
#   replace_na(list(total_abundance = 0, species_richness = 0)) %>%
#   dplyr::select(survey_id, total_abundance, species_richness) %>%
#   pivot_longer(!c(survey_id), names_to = "metric", values_to = "value") %>%
#   dplyr::left_join(rls_metadata) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(survey_id, metric, value, longitude_dd, latitude_dd) %>%
#   left_join(rls_metadata_locs) %>%
#   dplyr::select(survey_id, metric, value, longitude_dd, latitude_dd, location) %>% 
#   glimpse()
# 
# # Fishes of Australia codes ----
# dbca_googlesheet_url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"
# 
# foa_species_codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "fishes_of_australia") %>%
#   CheckEM::clean_names() %>%
#   dplyr::select(-c(number)) %>%
#   dplyr::mutate(species = case_when(
#     genus %in% "Ophthalmolepis" & species %in% "lineolata" ~ "lineolatus",
#     .default = as.character(species)
#   )) %>%
#   dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
#   dplyr::left_join(CheckEM::australia_life_history) %>%
#   dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
#   dplyr::select(display_name, url)
# 2
# 
# foa_genus_codes <- readRDS("data/genus_foa_codes.RDS") %>%
#   dplyr::mutate(species = "spp") %>%
#   dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
#   dplyr::left_join(CheckEM::australia_life_history) %>%
#   dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
#   dplyr::select(display_name, url)
# 
# foa_codes <- bind_rows(foa_species_codes, foa_genus_codes) %>%
#   dplyr::select(display_name, url) 
# 
# foa_codes <- data.table::data.table(foa_codes)
# 
# # Get rls status ----
# rls_metadata_sf <- rls_metadata %>%
#   st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)
# 
# rls_metadata_sf <- st_transform(rls_metadata_sf, st_crs(state.mp))
# 
# metadata_with_status <- st_join(rls_metadata_sf, state.mp %>% st_cast("POLYGON")) %>%
#   dplyr::mutate(status = case_when(
#     zone_type %in% c("SZ", "RAZ_L") ~ "No-Take",
#     .default = "Fished"
#   )) %>%
#   mutate(sample_url = as.character(survey_id)) %>%
#   glimpse()
# 
# unique(metadata_with_status$status)
# 
# # length of top 200 ----
# format_rls_length <- rls_length %>% 
#   mutate(sample_url = as.character(survey_id)) %>%
#   dplyr::rename(latitude_dd = latitude) %>%
#   dplyr::rename(longitude_dd = longitude) %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   dplyr::select(sample_url, display_name, count, length_mm, longitude_dd, latitude_dd) %>%
#   dplyr::mutate(method = "UVC") %>%
#   left_join(metadata_with_status %>% dplyr::select(sample_url, status)) %>%
#   left_join(rls_metadata_locs %>% 
#               dplyr::select(survey_id, location) %>% 
#               distinct() %>% 
#               dplyr::mutate(sample_url = as.character(survey_id)))
# 
# names(format_rls_length)
# 
# length_combined <- length %>%
#   dplyr::left_join(species_list) %>%
#   dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
#   left_join(metadata %>% dplyr::select(sample_url, sample, campaignid, status, longitude_dd, latitude_dd)) %>%
#   dplyr::mutate(method = "stereo-BRUVs") %>%
#   left_join(metadata_locs) %>%
#   dplyr::select(sample_url, display_name, count, length_mm, status, longitude_dd, latitude_dd, method, location) %>%
#   bind_rows(format_rls_length)
# 
# # test <- length_combined %>% filter(is.na(latitude_dd))
# 
# # Add state and zones to length data for histograms -----
# coastal_waters <- st_read("data/spatial/Coastal_Waters_areas_(AMB2020).shp")
# # marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp")
# 
# length_sf <- length_combined %>%
#   st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)
# 
# length_sf <- st_transform(length_sf, st_crs(coastal_waters))
# 
# length_with_jurisdiction <- st_join(length_sf, coastal_waters %>% select(catlim)) %>%
#   rename(jurisdiction = catlim) %>%
#   replace_na(list(jurisdiction = "Federal Waters"))
# 
# # unique(length_with_jurisdiction$jurisdiction)
# 
# # names(length_with_jurisdiction)
# 
# # Plots ----
# date_hist <- simple_metadata |>
#   mutate(year = format(date_time, "%Y")) |>
#   count(year) |>
#   ggplot(aes(x = year, y = n)) +
#   geom_bar(stat = "identity", fill = "#0c3978", color = "black") +
#   xlab("Year") +
#   ylab("Number of deployments") +
#   ggplot_theme +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# date_hist_rls <- rls_simple_metadata |>
#   dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
#   count(year) |>
#   ggplot(aes(x = year, y = n)) +
#   geom_bar(stat = "identity", fill = "#0c3978", color = "black") +
#   xlab("Year") +
#   ylab("Number of surveys") +
#   ggplot_theme +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# depth_hist <- ggplot(simple_metadata, aes(x = depth_m)) +
#   geom_histogram(binwidth = 5, fill = "#0c3978", color = "black") +
#   xlab("Depth (m)") +
#   ylab("Number of deployments") +
#   scale_x_continuous(
#     breaks = seq(
#       floor(min(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
#       ceiling(max(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
#       by = 50
#     )
#   ) +
#   ggplot_theme
# 
# depth_hist_rls <- ggplot(rls_simple_metadata, aes(x = depth_m)) +
#   geom_histogram(binwidth = 5, fill = "#0c3978", color = "black") +
#   xlab("Depth (m)") +
#   ylab("Number of surveys") +
#   ggplot_theme
# 
# # Combined plots ----
# bruv_years <- simple_metadata |>
#   mutate(year = as.numeric(format(date_time, "%Y"))) |>
#   count(year) |>
#   mutate(method = "stereo-BRUVs")
# 
# rls_years <- rls_simple_metadata |>
#   dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
#   count(year) |>
#   mutate(method = "UVC")
# 
# date_hist_combined <- bind_rows(bruv_years, rls_years)|>
#   ggplot(aes(x = year, y = n, fill = method)) +
#   geom_bar(stat = "identity", color = "black") + #fill = "#0c3978", 
#   xlab("Year") +
#   ylab("Number of deployments") +
#   scale_fill_manual(values = c("#f89f00", "#0c3978")) +
#   ggplot_theme +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# date_hist_combined
# 
# # Park dataframes ----
# bruv_years_park <- metadata_locs |>
#   mutate(year = as.numeric(format(date_time, "%Y"))) |>
#   dplyr::group_by(year, location) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   mutate(method = "stereo-BRUVs")
# 
# rls_years_park <- rls_metadata_locs |>
#   dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
#   dplyr::group_by(year, location) %>%
#   dplyr::summarise(n = n()) %>%
#   ungroup() %>%
#   mutate(method = "UVC")
# 
# years_park <- bind_rows(bruv_years_park, rls_years_park)
# 
# # Combined depth plots ----
# depth_combined <- bind_rows(metadata_locs %>% mutate(method = "stereo-BRUVs"), 
#                             rls_metadata_locs %>% mutate(method = "UVC")) %>% 
#   dplyr::select(depth_m, method, location)
# 
# depth_hist_combined <- ggplot(depth_combined, aes(x = depth_m, fill = method)) +
#   geom_histogram(binwidth = 5, color = "black") +
#   xlab("Depth (m)") +
#   ylab("Number of surveys") +
#   scale_fill_manual(values = c("#f89f00", "#0c3978")) +
#   ggplot_theme
# 
# depth_hist_combined
# 
# # Create final dataframes and save ----
# values <- structure(
#   list(
#     number_of_deployments = number_of_deployments,
#     number_of_fish = number_of_fish,
#     number_of_nonfish_species = number_of_nonfish_species,
#     number_of_fish_species = number_of_fish_species,
#     number_of_measurements = number_of_measurements,
#     min_depth = min_depth,
#     max_depth = max_depth,
#     mean_depth = mean_depth,
#     mean_lon = mean_lon,
#     mean_lat = mean_lat,
#     top_species_names_combined = top_species_names_combined,
#     min_year = min_year,
#     max_year = max_year,
#     deployments_relief = deployments_relief,
#     deployments_benthos = deployments_benthos,
#     number_of_deployments_rls = number_of_deployments_rls,
#     number_of_fish_rls = number_of_fish_rls,
#     number_of_nonfish_species_rls = number_of_nonfish_species_rls,
#     number_of_fish_species_rls = number_of_fish_species_rls,
#     number_of_measurements_rls = number_of_measurements_rls,
#     min_depth_rls = min_depth_rls,
#     max_depth_rls = max_depth_rls,
#     mean_depth_rls = mean_depth_rls,
#     mean_lon_rls = mean_lon_rls,
#     mean_lat_rls = mean_lat_rls,
#     min_year_rls = min_year_rls,
#     max_year_rls = max_year_rls
#   ),
#   class = "data"
# )
# 
# save(values, file = here::here("app_data/values.Rdata"))
# 
# dataframes <- structure(
#   list(
#     top_species = top_species,
#     top_species_combined = top_species_combined,
#     top_species_location = top_species_location,
#     deployment_locations = deployment_locations,
#     metric_bubble_data = metric_bubble_data,
#     bubble_data = bubble_data,
#     foa_codes = foa_codes,
#     length_with_jurisdiction = length_with_jurisdiction,
#     simple_metadata = simple_metadata,
#     depth_combined = depth_combined,
#     
#     deployment_locations_rls = deployment_locations_rls,
#     metric_bubble_data_rls = metric_bubble_data_rls,
#     bubble_data_rls = bubble_data_rls,
#     rls_simple_metadata = rls_simple_metadata,
#     years_park = years_park
#     # length_with_jurisdiction = length_with_jurisdiction,
#   ),
#   class = "data"
# )
# 
# save(dataframes, file = here::here("app_data/dataframes.Rdata"))
# 
# plots <- structure(
#   list(
#     date_hist = date_hist,
#     depth_hist = depth_hist,
#     date_hist_rls = date_hist_rls,
#     depth_hist_rls = depth_hist_rls,
#     date_hist_combined = date_hist_combined,
#     depth_hist_combined = depth_hist_combined
#   ),
#   class = "data"
# )
# 
# save(plots, file = here::here("app_data/plots.Rdata"))
# 

hab_dataframes <- structure(
  list(
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
    hab_combined_metadata = combined_metadata
  )
)

save(hab_dataframes, file = here::here("app_data/hab_dataframes.Rdata"))
