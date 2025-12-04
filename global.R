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
library(tidytext)
# library(leafsync)
library(leaflet.extras2)
library(rmapshaper)
library(shinycssloaders)
library(htmltools)
library(ggforce)
library(patchwork)
library(googlesheets4)
library(rmarkdown)
library(ggtext)
library(DT)

source("R/helpers.R")          # defines filter_by_park(), ensure_sf_ll(), base_map(), etc.
source("R/mod_park_summary.R") # defines mod_park_summary_ui/server (contains session$onFlushed INSIDE)
source("R/half_donut_with_dial.R")

# Load data
load("app_data/values.Rdata")
load("app_data/dataframes.Rdata")
load("app_data/plots.Rdata")
load("app_data/hab_data.Rdata")

# ---- global constants (one-off dashboard values) ----
sites_planned    <- sum(hab_data$survey_plan$planned_number_sites)
sites_completed  <- sum(hab_data$survey_plan$complete_number_sites)

bruvs_planned    <- sum(hab_data$survey_plan$planned_number_drops)
bruvs_completed  <- sum(hab_data$survey_plan$complete_number_drops)

uvc_planned    <- sum(hab_data$survey_plan$planned_number_transects)
uvc_completed  <- sum(hab_data$survey_plan$complete_number_transects)

percent_completed   <- round((sites_completed/sites_planned*100),1)
vb_col <- "secondary"

# Suppose the park names live in dataframes$parks$park (adjust as needed)
all_deployments <- bind_rows(dataframes$deployment_locations, dataframes$deployment_locations_rls) %>% 
  distinct(location)

# parks <- sort(unique(all_deployments$location))  # or your parks df
parks <- c("Encounter", "Sir Joseph Banks Group", "Southern Spencer Gulf", "Eastern Spencer Gulf", "Upper Gulf St Vincent", "Upper Spencer Gulf", "Lower Yorke Peninsula", "Franklin Harbor")

# Spatial files for maps ----
commonwealth.mp <- readRDS("app_data/spatial/commonwealth.mp.RDS") %>%
  st_as_sf() %>%
  dplyr::filter(NetName %in% "South-west") %>%
  dplyr::filter(ResName %in% c("Great Australian Bight",
                               "Southern Kangaroo Island",
                               "Western Eyre",
                               "Western Kangaroo Island"))

state_mp <- readRDS("app_data/spatial/sa_state_mp.RDS")

# state_mp  <- rmapshaper::ms_simplify(state_mp, keep = 0.5, keep_shapes = TRUE)
# commonwealth.mp <- rmapshaper::ms_simplify(commonwealth.mp, keep = 0.5, keep_shapes = TRUE)

# Pallettes for maps ----
state.pal <- colorFactor(c("#f18080", # Restricted Access Zone (RAZ)
                           "#69a802", # Sanctuary Zone (SZ)
                           "#799CD2", # Habitat Protection (HPZ)
                           "#BED4EE" # General Managed Use Zone (GMUZ)
), state_mp$zone)

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

# ---- data for the table ----
indicator_tbl <- tribble(
  ~Indicator,                          ~Description,
  "Species richness",                  "Number of species recorded",
  "Total abundance",                   "Count of individuals",
  "Shark and ray richness",            "Number of shark and ray species",
  "Reef associated species richness",  "Number of reef associated fish species",
  "Fish greater than 200mm abundance", "Count of individuals >200mm long",
  "Overall impact",                    "Average of the above five indicators"
)

# plot_dummy_time <- function(region) {
#   df <- data.frame(day = 1:30, value = cumsum(rnorm(30, 0.1, 0.4)))
#   ggplot(df, aes(day, value)) +
#     geom_line() +
#     labs(title = paste("Time series —", region), x = "Day", y = "Index") +
#     theme_minimal(base_size = 12)
# }
# 
# plot_dummy_comp <- function(region) {
#   df <- data.frame(group = c("Inshore", "Mid-shelf", "Offshore"),
#                    score = runif(3, 0.3, 0.9))
#   ggplot(df, aes(group, score)) +
#     geom_col() +
#     coord_cartesian(ylim = c(0, 1)) +
#     labs(title = paste("Habitat condition —", region), x = NULL, y = "Score") +
#     theme_minimal(base_size = 12)
# }
# 
# plot_dummy_rank <- function(region) {
#   df <- data.frame(cat = c("Fish", "Seagrass", "Reef", "Mangrove"),
#                    risk = sample(1:4, 4, replace = TRUE))
#   ggplot(df, aes(reorder(cat, risk), risk)) +
#     geom_point(size = 3) +
#     coord_flip() +
#     scale_y_continuous(breaks = 1:4, labels = c("Low","Low-mid","Mid-high","High")) +
#     labs(title = paste("Pressure ranking —", region), x = NULL, y = "Risk") +
#     theme_minimal(base_size = 12)
# }

# ---- Example ----
# segs <- c("Very Poor", "Poor", "Good", "Very Good")
vals <- c(1, 1, 1, 1)
# cols <- c(
#   "Very Poor" = "#E74C3C",   # red
#   "Poor"      = "#febf26",   # orange
#   "Good"      = "#9fcc3b",   # light green
#   "Very Good" = "#3b9243"    # dark green
# )

half_donut_with_dial(
  values = c(1, 1, 1),
  mode = "absolute",
  status = "Low"
)

# ---- Two-value value_box helpers --------------------------------------------
twoValueBoxUI <- function(id,
                          title,
                          left_label  = "Pre-bloom",
                          right_label = "Bloom",
                          icon        = icon("ship", class = "fa-xl"),
                          theme_color = "secondary",
                          height = 125) {
  ns <- NS(id)
  value_box(
    title = title,
    theme_color = theme_color,
    showcase = icon,
    height = height,
    class = "pp-title-center",
    value = div(
      class = "pp-wrap",
      div(class="pp-col",
          span(class="pp-lab", left_label),
          span(class="pp-val", uiOutput(ns("left_val")))
      ),
      div(class="pp-col",
          span(class="pp-lab", right_label),
          span(class="pp-val", uiOutput(ns("right_val")))
      )
    )
  )
}

# ---- GLOBAL ADDITIONS (put near other helpers) ----
metric_defs <- c(
  richness      = "Species richness",
  total_abundance = "Total abundance",
  
  sharks_rays   = "Shark and ray species richness",
  reef_associated_richness = "Reef associated fish species richness",
 
  large_fish    = "Count of large fish (>200 mm)",
  # cti           = "Community temperature index",
  # func_groups   = "Abundance by functional group",
  trophic       = "Abundance by trophic level"
)

metric_y_lab <- list(
  richness      = "No. species",
  total_abundance = "No. individuals",
  
  sharks_rays = "No. species",
  reef_associated_richness   = "No. species",
  
  large_fish    = "No. individuals",
  trophic       = "No. individuals"
)

# shared colours for pre/post everywhere
metric_period_cols <- c(
  "Pre-bloom"  = "#0c3978",  # same blue
  "Bloom" = "#f89f00"   # same orange
)

metric_groups <- function(metric_id) {
  switch(metric_id,
         func_groups = c("Carnivore", "Herbivore", "Omnivore")#,
         # trophic     = c("Low TL", "Mid TL", "High TL"),
         # c("Inshore", "Mid-shelf", "Offshore")
  )
}


# Deterministic dummy data for pre/post per region+metric+Inside/Outside
dummy_metric_data <- function(metric_id, region, n = 120) {
  stopifnot(metric_id %in% names(metric_defs))
  set.seed(sum(utf8ToInt(paste(metric_id, region))))  # stable per metric/region
  
  base_mean <- switch(metric_id,
                      richness      = 60,
                      site_attached = 40,
                      sharks_rays   = 10,
                      large_fish    = 50,
                      func_groups   = 300,
                      cti           = 18.5,
                      trophic       = 3.1
  )
  base_sd <- switch(metric_id,
                    richness      = 10,
                    site_attached = 10,
                    sharks_rays   = 5,
                    large_fish    = 20,
                    func_groups   = 90,
                    cti           = 0.6,
                    trophic       = 0.25
  )
  shift <- switch(metric_id,
                  richness      = -6,
                  site_attached = -12,
                  sharks_rays   = -5,
                  large_fish    = -5,
                  func_groups   = -45,
                  cti           = +0.4,
                  trophic       = +0.15
  )
  
  # small offset so Outside is a bit lower on average
  zone_offset <- c("Inside" = 0, "Outside" = -5)
  
  make_zone_df <- function(z) {
    pre  <- rnorm(n, mean = base_mean + zone_offset[[z]], sd = base_sd)
    post <- rnorm(n, mean = base_mean + shift + zone_offset[[z]], sd = base_sd)
    
    df <- tibble::tibble(
      region = region,
      metric = metric_id,
      zone   = z,
      period = factor(rep(c("pre-bloom", "post-bloom"), each = n),
                      levels = c("pre-bloom", "post-bloom")),
      value  = c(pre, post)
    )
    
    # 👉 functional groups, not trophic
    if (metric_id == "func_groups") {
      func_groups_vec <- c("Carnivore", "Herbivore", "Omnivore", "Planktivore")
      df$group <- sample(func_groups_vec, size = nrow(df), replace = TRUE)
    }
    
    df
  }
  
  dplyr::bind_rows(
    make_zone_df("Inside"),
    make_zone_df("Outside")
  )
}


# ---- Dummy % change table for HAB impacts -----------------------------------

# Metrics to match the screenshot
hab_metrics <- c(
  "Species Richness",
  "Total abundance",
  "Shark and ray richness",
  "Reef associated species richness",
  "Fish greater than 200mm abundance"
)

# All regions available in the HAB data
hab_regions <- sort(unique(hab_data$regions_summaries$region))

# Deterministic dummy % change data for each region x metric
set.seed(2025)


hab_metric_change <- tidyr::expand_grid(
  region = hab_regions,
  metric = hab_metrics
) |>
  dplyr::mutate(
    # negative = decline, positive = increase
    inside_change  = round(runif(dplyr::n(), -70, -10)), # -70% … -10%
    outside_change = round(runif(dplyr::n(), -70, -10)),
  ) |>
  dplyr::mutate(
    overall_change = round((inside_change + outside_change) / 2)
  )

hab_metric_change <- hab_data$impact_data %>%
  mutate(
    percentage = (bloom / pre_bloom) * 100,
    percentage_change = (bloom - pre_bloom) / pre_bloom * 100
  ) %>%
  dplyr::mutate(percentage_change = if_else(is.na(percentage_change), "Surveys incomplete", as.character(percentage_change))) %>%
  dplyr::mutate(impact_metric = case_when(
    impact_metric %in% "species_richness" ~ "Species richness",
    impact_metric %in% "total_abundance" ~ "Total abundance",
    impact_metric %in% "shark_ray_richness" ~ "Shark and ray richness",
    impact_metric %in% "reef_associated_richness" ~ "Reef associated species richness",
    impact_metric %in% "fish_200_abundance" ~ "Fish greater than 200mm abundance"
  ))

# unique(hab_metric_change$impact_metric)

# ==== DUMMY COMMON-SPECIES DATA FOR EACH REGION =============================
# # Paste the species list as a single string (omit the "genus_species" header)
species_text <- "
Acanthaluteres brownii
Acanthaluteres spilomelanurus
Acanthaluteres vittiger
Achoerodus gouldii
Anoplocapros amygdaloides
Anoplocapros lenticularis
Aplodactylus arctidens
Aptychotrema vincentiana
Aracana aurita
Aracana ornata
Arripis georgianus
Arripis truttaceus
Asymbolus vincenti
Atherinidae spp
Austrolabrus maculatus
Bathytoshia brevicaudata
Bodianus frenchii
Brachaluteres jacksonianus
Caesioperca lepidoptera
Caesioperca rasor
Callorhinchus milii
Carcharhinus brachyurus
Carcharodon carcharias
Centroberyx gerrardi
Centroberyx lineatus
Chelmonops curiosus
Chirodactylus spectabilis
Chironemus georgianus
Chironemus maculosus
Chrysophrys auratus
Coris auricularis
Dactylophora nigricans
Dinolestes lewini
Diodon nicthemerus
Dotalabrus aurantiacus
Enoplosus armatus
Eubalichthys bucephalus
Eubalichthys cyanoura
Eubalichthys gunnii
Eubalichthys mosaicus
Eupetrichthys angustipes
Furgaleus macki
Galeorhinus galeus
Genypterus tigerinus
Girella zebra
Gobiesocidae spp
Gobiidae spp
Gymnapistes marmoratus
Gymnothorax prasinus
Haletta semifasciata
Helcogramma decurrens
Heterodontus portusjacksoni
Heteroscarus acroptilus
Hippocampus spp
Hyperlophus vittatus
Hypoplectrodes nigroruber
Hyporhamphus melanochir
Iso rhothophilus
Kyphosus sydneyanus
Latropiscis purpurissatus
Leptoichthys fistularius
Lotella rhacina
Meuschenia australis
Meuschenia flavolineata
Meuschenia freycineti
Meuschenia galii
Meuschenia hippocrepis
Meuschenia scaber
Meuschenia venusta
Monacanthidae juvenile
Mustelus antarcticus
Myliobatis tenuicaudatus
Neatypus obliquus
Nelusetta ayraud
Nemadactylus macropterus
Nemadactylus valenciennesi
Neoodax balteatus
Neosebastes bougainvillii
Neosebastes pandus
Neosebastes scorpaenoides
Notolabrus fucicola
Notolabrus parilus
Notolabrus tetricus
Olisthops cyanomelas
Omegophora armilla
Omegophora cyanopunctata
Ophthalmolepis lineolatus
Oplegnathus woodwardi
Othos dentex
Parapercis haackei
Parapercis ramsayi
Paraplesiops meleagris
Parascyllium ferrugineum
Parascyllium variolatum
Parequula melbournensis
Parma victoriae
Pelates octolineatus
Pempheris klunzingeri
Pempheris multiradiata
Pentaceropsis recurvirostris
Phyllopteryx taeniolatus
Pictilabrus laticlavius
Platycephalus aurimaculatus
Platycephalus bassensis
Platycephalus grandispinis
Platycephalus speculator
Pseudocaranx spp
Pseudogoniistius nigripes
Pseudolabrus rubicundus
Pseudophycis bachus
Pseudophycis barbata
Pseudorhombus arsius
Scobinichthys granulatus
Scomber australasicus
Scorpis aequipinnis
Scorpis georgiana
Scorpis lineolata
Seriola hippos
Seriola lalandi
Sillaginodes punctatus
Sillago spp
Siphamia cephalotes
Siphonognathus attenuatus
Siphonognathus beddomei
Siphonognathus caninis
Siphonognathus tanyourus
Sphyraena forsteri
Sphyraena novaehollandiae
Sphyrna zygaena
Spiniraja whitleyi
Squalus megalops
Sutorectus tentaculatus
Syngnathidae spp
Tetractenos glaber
Thamnaconus degeni
Thyrsites atun
Tilodon sexfasciatus
Torquigener pleurogramma
Trachichthys australis
Trachinops noarlungae
Trachurus declivis
Trachurus novaezelandiae
Trygonoptera mucosa
Trygonorrhina dumerilii
Upeneichthys vlamingii
Urolophus gigas
Urolophus spp
Vincentia conspersa
Coscinasterias muricata
Diogenidae spp
Jasus edwardsii
Leptomithrax gaimardii
Melicertus latisulcatus
Naxia aurita
Nectocarcinus integrifrons
Neophoca cinerea
Macroctopus maorum
Octopodidae spp
Ovalipes australiensis
Ozius truncatus
Pectinidae spp
Phalacrocorax spp
Plagusia chabrus
Portunus armatus
Pycnogonidae spp
Sagaminopteron ornatum
Sepia apama
Sepioteuthis australis
Tursiops spp
"

hab_species <- scan(text = species_text, what = character(), sep = "\n", quiet = TRUE)

# drop any accidental blanks / "Nil Nil" if present
hab_species <- hab_species[nchar(hab_species) > 0 & hab_species != "Nil Nil"]

# length(hab_species)  # just to sanity check

set.seed(4242)

# Pick a set of species that tend to be common everywhere
global_common_species <- sample(hab_species, 15)

# # Base lambda per region x species (bigger for globally common spp)
# base_abund <- tidyr::expand_grid(
#   region  = hab_regions,
#   species = hab_species
# ) |>
#   dplyr::mutate(
#     base_lambda = ifelse(
#       species %in% global_common_species,
#       runif(dplyr::n(), 30, 80),  # common spp
#       runif(dplyr::n(), 2, 20)    # rarer spp
#     )
#   )

# # Pre-bloom counts
# pre_counts <- base_abund |>
#   dplyr::transmute(
#     region,
#     species,
#     period = "Pre-bloom",
#     count  = rpois(dplyr::n(), lambda = base_lambda)
#   )

# # Post-bloom counts (generally lower; region-specific severity)
# severity_by_region <- runif(length(hab_regions), 0.25, 0.75)  # 25–75% of pre
# names(severity_by_region) <- hab_regions
# 
# post_counts <- base_abund |>
#   dplyr::mutate(
#     lambda_post = base_lambda * severity_by_region[region]
#   ) |>
#   dplyr::transmute(
#     region,
#     species,
#     period = "Post-bloom",
#     count  = pmax(0L, rpois(dplyr::n(), lambda = lambda_post))
#   )
# 
# # Final long table used for plotting
# hab_species_counts <- dplyr::bind_rows(pre_counts, post_counts)

# Columns: region, species, period ("Pre-bloom"/"Post-bloom"), count

# # Replace with your sheet URL or ID
# survey_plan <- googlesheets4::read_sheet(
#   "https://docs.google.com/spreadsheets/d/1QxTP_s58cbhLYB4GIuS39wK1c3QfBu8TGbUhV9rD3FY/edit?gid=1319001580#gid=1319001580",
#   sheet = "reporting_region_summary"   # or "Sheet1" etc.
# )

# Helper: map % complete -> value_box colour
completion_theme <- function(p) {
  if (is.na(p)) {
    "secondary"
  } else if (p < 50) {
    "danger"   # red
  } else if (p < 100) {
    "warning"  # yellow
  } else {
    "green"  # green
  }
}

# ==== MARINE PARK DUMMY DATA =================================================

marine_parks <- parks   # just an alias for clarity

## --- Survey / effort plan per marine park -----------------------------------

set.seed(3001)

# mp_survey_plan <- tibble::tibble(
#   park = marine_parks,
#   methods = sample(
#     c("BRUVS", "BRUVS, ROV"),
#     replace = TRUE,
#     prob = c(0.7, 0.3)
#   ),
#   planned_number_sites = sample(seq(6, 40, by = 2),
#                                 length(marine_parks),
#                                 replace = TRUE)
# ) |>
#   dplyr::mutate(
#     planned_number_drops = planned_number_sites *
#       sample(c(3L, 4L, 5L), dplyr::n(), replace = TRUE),
#     planned_number_transects = dplyr::if_else(
#       grepl("ROV", methods),
#       sample(seq(10, 40, by = 2), dplyr::n(), replace = TRUE),
#       0L
#     ),
#     prop_done_sites = runif(dplyr::n(), 0, 1),
#     complete_number_sites = round(planned_number_sites * prop_done_sites),
#     complete_number_drops = round(planned_number_drops *
#                                     runif(dplyr::n(), 0, 1)),
#     complete_number_transects = dplyr::if_else(
#       planned_number_transects > 0,
#       round(planned_number_transects * runif(dplyr::n(), 0, 1)),
#       0L
#     ),
#     percent_sites_completed = round(
#       100 * complete_number_sites / pmax(planned_number_sites, 1),
#       1
#     )
#   )

## --- % change by metric (Inside / Outside / Overall) ------------------------

mp_metric_change <- tidyr::expand_grid(
  park   = marine_parks,
  metric = hab_metrics    # reuse the same metric labels as HAB summary
) |>
  dplyr::mutate(
    inside_change  = round(runif(dplyr::n(), -70, -10)),
    outside_change = round(runif(dplyr::n(), -70, -10))
  ) |>
  dplyr::mutate(
    overall_change = round((inside_change + outside_change) / 2)
  )

## --- Common species per park (pre/post) -------------------------------------

# Reuse the species list you already defined: hab_species, global_common_species

mp_base_abund <- tidyr::expand_grid(
  park    = marine_parks,
  species = hab_species
) |>
  dplyr::mutate(
    base_lambda = ifelse(
      species %in% global_common_species,
      runif(dplyr::n(), 30, 80),
      runif(dplyr::n(), 2, 20)
    )
  )

# Pre-bloom counts
pre_mp <- mp_base_abund |>
  dplyr::transmute(
    park,
    species,
    period = "Pre-bloom",
    count  = rpois(dplyr::n(), lambda = base_lambda)
  )

# Post-bloom counts with park-specific severity
severity_by_park <- runif(length(marine_parks), 0.25, 0.75)
names(severity_by_park) <- marine_parks

post_mp <- mp_base_abund |>
  dplyr::mutate(
    lambda_post = base_lambda * severity_by_park[park]
  ) |>
  dplyr::transmute(
    park,
    species,
    period = "Post-bloom",
    count  = pmax(0L, rpois(dplyr::n(), lambda = lambda_post))
  )

mp_species_counts <- dplyr::bind_rows(pre_mp, post_mp)
# cols: park, species, period, count
