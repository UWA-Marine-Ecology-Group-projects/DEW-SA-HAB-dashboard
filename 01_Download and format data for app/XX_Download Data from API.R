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


# ## Load in data again to save time ----
# metadata <- readRDS("data/raw/metadata.RDS") %>% filter(campaignid %in% c(campaign_list))
# count <- readRDS("data/raw/count.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
# length <- readRDS("data/raw/length.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
# benthos <- readRDS("data/raw/benthos_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))
# relief <- readRDS("data/raw/relief_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))








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


