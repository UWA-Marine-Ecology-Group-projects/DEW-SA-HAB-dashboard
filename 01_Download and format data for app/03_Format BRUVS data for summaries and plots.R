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

state.mp$zone <- fct_relevel(state.mp$zone, 
                             "Restricted Access (no-take)", 
                             "Sanctuary (no-take)", 
                             "Habitat Protection", 
                             "General Managed Use")

sa.state.mp <- st_cast(state.mp, "POLYGON")

saveRDS(sa.state.mp, "app_data/spatial/sa.state.mp.RDS") # TODO put this in a shapefile list

# ---- Color mapping ----
ordered_levels <- c("High", "Medium", "Low")

pal_vals <- c(  "High" = "#E74C3C",   # red
                "Medium"      = "#febf26",   # orange
                "Low" = "#3b9243" )   # dark green)

pal_factor <- colorFactor(palette = pal_vals, domain = ordered_levels, ordered = TRUE)

# Survey tracking ----
survey_plan <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1QxTP_s58cbhLYB4GIuS39wK1c3QfBu8TGbUhV9rD3FY/edit?gid=1319001580#gid=1319001580",
  sheet = "reporting_region_summary"   # or "Sheet1" etc.
)

# Combined data
hab_data <- structure(
  list(
    scores = scores,
    regions_summaries = regions_summaries,
    regions_shp = regions_shp,  # TODO put this in a shapefile list
    pal_vals = pal_vals,
    pal_factor = pal_factor,
    ordered_levels = ordered_levels,
    survey_plan = survey_plan
  ),
  class = "data"
)

save(hab_data, file = here::here("app_data/hab_data.Rdata"))