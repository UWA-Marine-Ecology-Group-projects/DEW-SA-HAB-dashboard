library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)
library(fontawesome)
library(scales)
library(leafgl)
library(colourvalues)
library(shinyWidgets)
library(sf)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggtext)
library(tidytext)
# library(leafsync)
library(leaflet.extras2)
library(rmapshaper)
library(shinycssloaders)

source("R/helpers.R")          # defines filter_by_park(), ensure_sf_ll(), base_map(), etc.
source("R/mod_park_summary.R") # defines mod_park_summary_ui/server (contains session$onFlushed INSIDE)

# Load data
load("app_data/values.Rdata")
load("app_data/dataframes.Rdata")
load("app_data/plots.Rdata")
load("app_data/hab_data.Rdata")

# Suppose the park names live in dataframes$parks$park (adjust as needed)
all_deployments <- bind_rows(dataframes$deployment_locations, dataframes$deployment_locations_rls) %>% 
  distinct(location)

parks <- sort(unique(all_deployments$location))  # or your parks df

# Spatial files for maps ----
commonwealth.mp <- readRDS("app_data/spatial/commonwealth.mp.RDS") %>%
  st_as_sf() %>%
  dplyr::filter(NetName %in% "South-west") %>%
  dplyr::filter(ResName %in% c("Great Australian Bight",
                               "Southern Kangaroo Island",
                               "Western Eyre",
                               "Western Kangaroo Island"))

# unique(commonwealth.mp$NetName)
# state.mp <- readRDS("app_data/spatial/state.mp.RDS")

state.mp <- readRDS("app_data/spatial/sa.state.mp.RDS")

# state.mp  <- rmapshaper::ms_simplify(state.mp, keep = 0.5, keep_shapes = TRUE)
# commonwealth.mp <- rmapshaper::ms_simplify(commonwealth.mp, keep = 0.5, keep_shapes = TRUE)

# Pallettes for maps ----
state.pal <- colorFactor(c("#f18080", # Restricted Access Zone (RAZ)
                           "#69a802", # Sanctuary Zone (SZ)
                           "#799CD2", # Habitat Protection (HPZ)
                           "#BED4EE" # General Managed Use Zone (GMUZ)
), state.mp$zone)

# unique(state.mp$zone_type)

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)

unique(commonwealth.mp$zone)

css = HTML("
  .leaflet-top, .leaflet-bottom {
    z-index: 0 !important;
  }

  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
  }
  
    .dropdown-menu {
    z-index: 2000 !important;
    }
  
  .bslib-card {
    overflow: visible !important;
  }
  
    .bslib-layout-column-wrap {
    overflow: visible !important;
  }
")