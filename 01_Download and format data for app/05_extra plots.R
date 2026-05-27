library(readr)
library(dplyr)
library(ggplot2)
library(CheckEM)
library(forcats)

# Read in data -----
load("app_data/hab_data.Rdata")

combined_count <- readRDS("raw_count.RDS") %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  distinct()

dew_species <- read_csv("data/lookups/SA-HAB-Functional Traits.csv")
species_list <- CheckEM::australia_life_history

combined_metadata <- hab_data$hab_combined_metadata %>%
  dplyr::filter(method %in% "BRUVs") %>%
  ungroup()

dates <- combined_metadata %>%
  distinct(campaignid, sample, start_month, reporting_name, period) %>%
  dplyr::mutate(period = case_when(
    period %in% "Bloom" ~ paste(period, start_month),
    .default = period
  ))

unique(dates$period)
unique(combined_count$scientific) # 221 species

top_species_average_overall <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::select(
    campaignid, sample,
    scientific, count, family, genus, species
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample),
    tidyr::nesting(scientific, family, genus, species)
  ) %>%
  left_join(dates) %>%
  dplyr::filter(!scientific %in% "NA NA NA") %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(reporting_name, scientific, family, genus, species) %>%
  dplyr::summarise(
    overall_average = mean(count, na.rm = TRUE),
    overall_se      = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    overall_n_samples_present = sum(count > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(genus_species = paste(genus, species)) %>%
  dplyr::left_join(
    dew_species %>% dplyr::select(genus_species, common_name),
    by = "genus_species"
  ) %>%
  dplyr::left_join(
    species_list,
    by = c("family", "genus", "species")
  ) %>%
  dplyr::select(
    family, genus, species, common_name, australian_common_name,
    overall_average, overall_se, overall_n_samples_present, reporting_name
  ) %>%
  dplyr::mutate(
    common_name = dplyr::if_else(is.na(common_name), australian_common_name, common_name),
    display_name = paste0(genus, " ", species, " (", common_name, ")")
  ) 


top5_region_species <- top_species_average_overall %>%
  group_by(reporting_name) %>%
  slice_max(overall_average, n = 5, with_ties = FALSE) %>%
  ungroup()

top_species_average <- combined_count %>%
  full_join(combined_metadata) %>%
  dplyr::filter(method %in% "BRUVs") %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::select(
    campaignid, sample,
    scientific, count, family, genus, species
  ) %>%
  tidyr::complete(
    tidyr::nesting(campaignid, sample),
    tidyr::nesting(scientific, family, genus, species)
  ) %>%
  left_join(dates) %>%
  dplyr::filter(!scientific %in% "NA NA NA") %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(reporting_name, period, scientific, family, genus, species) %>%
  dplyr::summarise(
    average = mean(count, na.rm = TRUE),
    se      = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    n_samples_present = sum(count > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(genus = dplyr::if_else(genus %in% "Unknown", family, genus)) %>%
  dplyr::mutate(genus_species = paste(genus, species)) %>%
  dplyr::left_join(
    dew_species %>% dplyr::select(genus_species, common_name),
    by = "genus_species"
  ) %>%
  dplyr::left_join(
    species_list,
    by = c("family", "genus", "species")
  ) %>%
  dplyr::select(
    family, genus, species, common_name, australian_common_name,
    average, se, n_samples_present, reporting_name, period
  ) %>%
  dplyr::mutate(
    common_name = dplyr::if_else(is.na(common_name), australian_common_name, common_name),
    display_name = paste0(genus, " ", species, " (", common_name, ")")
  ) 

# Species averages -----
dat <- top_species_average

title_lab = "Common species"

# Base colours (your existing theme)
period_cols <- c(
  "Pre-bloom" = "#072759",
  "Bloom"     = "#e88e98"
)

# # Get top 5 species per region based on overall abundance ----

# Join back to plotting data ----
plot_data <- top_species_average %>%
  semi_join(
    top5_region_species,
    by = c("reporting_name", "display_name")
  ) %>%
  left_join(
    top5_region_species,
    by = c("reporting_name", "display_name")
  ) %>%
  group_by(reporting_name) %>%
  mutate(
    # Most abundant species at the TOP
    display_name = fct_reorder(display_name, overall_average, .desc = FALSE)
  ) %>%
  ungroup() #%>%
  #filter(reporting_name %in% c("Glenelg", "Aldinga - Aldinga Reef Sanctuary Zone"))

# Period ordering ----
# Always:
# 1. Pre-bloom first
# 2. Bloom periods alphabetical afterwards

period_levels <- c(
  "Pre-bloom",
  sort(unique(plot_data$period[plot_data$period != "Pre-bloom"]))
)

plot_data <- plot_data %>%
  mutate(period = factor(period, levels = period_levels))

bloom_periods <- setdiff(period_levels, "Pre-bloom")

bloom_cols <- grDevices::colorRampPalette(
  c("#e88e98", "#f8d7da")
)(length(bloom_periods))

fill_cols <- c(
  "Pre-bloom" = "#072759",
  setNames(bloom_cols, bloom_periods)
)

scale_fill_manual(
  values = fill_cols,
  breaks = period_levels,
  drop = FALSE
)

# # Plot ----

pd <- position_dodge(width = 0.8, reverse = TRUE)


library(purrr)

# Unique reporting names
reporting_names <- unique(plot_data$reporting_name)

# Create output folder
dir.create("plots/species", recursive = TRUE, showWarnings = FALSE)

# Loop through regions
for(i in reporting_names){
  
  dat_i <- plot_data %>%
    filter(reporting_name == i)
  
  species_order <- dat_i %>%
    distinct(display_name, overall_average) %>%
    arrange(overall_average) %>%   # smallest first, largest last
    pull(display_name)
  
  dat_i <- dat_i %>%
    mutate(
      display_name = factor(display_name, levels = species_order)
    )
  
  p <- ggplot(
    dat_i,
    aes(
      x = display_name,
      y = average,
      fill = period
    )
  ) +
    geom_col(
      position = pd,
      width = 0.7
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(0, average - se),
        ymax = average + se
      ),
      position = pd,
      width = 0.2
    ) +
    coord_flip() +
    scale_fill_manual(
      values = fill_cols,
      breaks = period_levels
    ) +
    labs(
      # title = i,
      x = NULL,
      y = "Average abundance",
      fill = NULL
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
  
  print(p)
  
  # Save plot
  ggsave(
    filename = paste0(
      "plots/species/",
      gsub("[^A-Za-z0-9]", "_", i),
      ".png"
    ),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

