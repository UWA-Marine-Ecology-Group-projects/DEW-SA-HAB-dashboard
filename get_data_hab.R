# ---- Packages ----
# library(shiny)
# library(bslib)
library(sf)
library(dplyr)
library(stringr)
# library(leaflet)
library(googlesheets4)
# library(ggplot2)

# ---- Auth (Google Sheets) ----
# Uncomment the next line the first time (or if using a service account, use gs4_auth(path="service-account.json"))
# gs4_auth()

# ---- CONFIG: set your Google Sheets here ----
SCORES_SHEET <- "https://docs.google.com/spreadsheets/d/1YReZDi7TRzlCTNdU0ganthAWa8TTcfG-eZtIObRM45k/edit?gid=0#gid=0"

# ---- Data loaders ----
scores <-  read_sheet(SCORES_SHEET) 

summary <- read_sheet(SCORES_SHEET, "summary_text") 

shp <- st_read("data/spatial/Reporting_regions_30102025.shp", quiet = TRUE) %>%
  dplyr::rename(region = RegionName)
# Ensure WGS84 for Leaflet
shp <- st_transform(shp, 4326)

# ---- Color mapping ----
ordered_levels <- c("Very Poor", "Poor", "Good", "Very Good")
pal_vals <- c(  "Very Poor" = "#E74C3C",   # red
                "Poor"      = "#febf26",   # orange
                "Good"      = "#9fcc3b",   # light green
                "Very Good" = "#3b9243" )   # dark green)
pal_factor <- colorFactor(palette = pal_vals, domain = ordered_levels, ordered = TRUE)


# Combined data
hab_data <- structure(
  list(
    scores = scores,
    shp = shp,
    pal_vals = pal_vals,
    pal_factor = pal_factor,
    ordered_levels = ordered_levels
  ),
  class = "data"
)

save(hab_data, file = here::here("app_data/hab_data.Rdata"))
