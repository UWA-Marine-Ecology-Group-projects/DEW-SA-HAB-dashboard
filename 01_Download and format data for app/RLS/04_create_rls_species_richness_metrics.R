#################################################################
# Create Species Richness from RLS M1 and M2 data

# Load libraries needed -----
library(dplyr)
library(sf)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)

# Read in locations and regions ----
sa_sites <- sf::read_sf(
  "dev/Dive_sites_2026_07_14.shp"
) %>%
  CheckEM::clean_names() %>%
  select(
    site_code,
    site_name,
    location_g,
    bruvsrepor
  )

# Calculates richness of individual blocks to then be averaged ----
calculate_block_species_richness <- function(data, dataset_name = "dataset") {
  
  # Combine duplicate taxon records within each block
  data_summarised <- data %>%
    dplyr::group_by(survey_id, survey_date, depth, block, family, genus, species, scientific) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")
  
  # Find spp and identified-species conflicts within the same block
  samples_with_both <- data_summarised %>%
    dplyr::group_by(survey_id, survey_date, depth, block, family, genus) %>%
    dplyr::summarise(spp_present = any(species == "spp" & total > 0),
                     identified_species_present = any(species != "spp" & total > 0),
                     .groups = "drop") %>%
    dplyr::filter(spp_present, identified_species_present)
  
  if (nrow(samples_with_both) > 0) {
    
    n_blocks <- samples_with_both %>%
      dplyr::distinct(survey_id, survey_date, depth, block) %>%
      nrow()
    
    message(dataset_name, ": found ", nrow(samples_with_both), " block/genus combinations across ", n_blocks, " blocks containing both an spp record and an identified species. The spp records will be removed.")
    
  } else {
    message(dataset_name, ": no blocks contained both an spp record and an ", "identified species from the same genus.")
  }
  
  # Calculate species richness separately for every block
  richness <- data_summarised %>%
    dplyr::group_by(survey_id, survey_date, depth, block, family, genus) %>%
    dplyr::mutate(identified_species_present = any(species != "spp" & total > 0)) %>%
    dplyr::filter(!(species == "spp" & total > 0 & identified_species_present)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(survey_id, survey_date, depth, block) %>%
    dplyr::summarise(species_richness = dplyr::n_distinct(scientific[total > 0], na.rm = TRUE), .groups = "drop")
  
  attr(richness, "samples_with_both") <- samples_with_both
  
  richness
}

# Read in survey-lists to get grouping variables ----
sl_m1 <- read_rds("data/tidy/rls_m1_survey_list.rds") %>%
  select(-block) %>%
  distinct()

sl_m2 <- read_rds("data/tidy/rls_m2_survey_list.rds") %>%
  select(-block) %>%
  distinct()

# Species Richness per sample (Not calculated per block!) ----
## M1 fish (First average blocks together)----
m1_fish_sr_blocks <- read_rds("data/tidy/rls_m1_complete_count.rds") %>%
  calculate_block_species_richness(dataset_name = "M1 fish")

m1_fish_spp_conflicts <- attr(m1_fish_sr_blocks, "samples_with_both")

m1_fish_sr_samples <- m1_fish_sr_blocks %>%
  dplyr::group_by(survey_id, survey_date, depth) %>%
  dplyr::summarise(species_richness = mean(species_richness, na.rm = TRUE),
                   n_blocks = dplyr::n_distinct(block),
                   block_sd = stats::sd(species_richness, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(sl_m1)

hist(m1_fish_sr_samples$species_richness)
summary(m1_fish_sr_samples)

## M2 fish  (First average blocks together) ----
m2_fish_sr_blocks <- read_rds("data/tidy/rls_m2_fish_complete_count.rds") %>%
  calculate_block_species_richness(dataset_name = "M2 fish")

m2_fish_spp_conflicts <- attr(m2_fish_sr_blocks, "samples_with_both")

m2_fish_sr_samples <- m2_fish_sr_blocks %>%
  dplyr::group_by(survey_id, survey_date, depth) %>%
  dplyr::summarise(species_richness = mean(species_richness, na.rm = TRUE),
                   n_blocks = dplyr::n_distinct(block),
                   block_sd = stats::sd(species_richness, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::left_join(sl_m2)

hist(m2_fish_sr_samples$species_richness)

## M2 inverts  (First average blocks together) ----
m2_inverts_sr_blocks <- read_rds("data/tidy/rls_m2_inverts_complete_count.rds") %>%
  calculate_block_species_richness(dataset_name = "M2 invertebrates")

m2_inverts_spp_conflicts <- attr(m2_inverts_sr_blocks, "samples_with_both")

m2_inverts_sr_samples <- m2_inverts_sr_blocks %>%
  dplyr::group_by(survey_id, survey_date, depth) %>%  # not block
  dplyr::summarise(species_richness = mean(species_richness, na.rm = TRUE),
                   n_blocks = dplyr::n_distinct(block),
                   block_sd = stats::sd(species_richness, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(sl_m2, by = c("survey_id", "survey_date", "depth"))

hist(m2_inverts_sr_samples$species_richness)

## Calculate averages per site/sampling event ----
m1_fish_site_sr_average <- m1_fish_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude, 
                  period, period_split, sampling_event_start_date) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

m2_fish_site_sr_average <- m2_fish_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude, 
                  period, period_split, sampling_event_start_date) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

m2_inverts_site_sr_average <- m2_inverts_sr_samples %>%
  ungroup() %>%
  dplyr::group_by(site_name, site_code, sampling_event, latitude, longitude, 
                  period, period_split, sampling_event_start_date) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

## Save formatted data ----
write_rds(m1_fish_site_sr_average, "data/tidy/rls_m1_fish_speciesrichness_average_per_site.rds")
write_rds(m2_fish_site_sr_average, "data/tidy/rls_m2_fish_speciesrichness_average_per_site.rds")
write_rds(m2_inverts_site_sr_average, "data/tidy/rls_m2_inverts_speciesrichness_average_per_site.rds")

## Calculate averages per location/sampling event ----
m1_fish_location_sr_average <- m1_fish_sr_samples %>%
  ungroup() %>%
  left_join(sa_sites) %>%
  dplyr::group_by(location_g, period, period_split, start_year_month) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

m2_fish_location_sr_average <- m2_fish_sr_samples %>%
  ungroup() %>%
  left_join(sa_sites) %>%
  dplyr::group_by(location_g, period, period_split, start_year_month) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

m2_inverts_location_sr_average <- m2_inverts_sr_samples %>%
  ungroup() %>%
  left_join(sa_sites) %>%
  dplyr::group_by(location_g, period, period_split, start_year_month) %>%
  dplyr::summarise(mean = mean(species_richness, na.rm = TRUE),
                   se   = sd(species_richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(species_richness))),
                   num_transects = n(),
                   .groups = "drop") #%>%
#dplyr::filter(num_transects > 3)

# Creating summarised values for period and for period split -----
summarise_site_richness <- function(data, period_variable) {
  
  period_variable <- rlang::ensym(period_variable)
  
  # Average transects
  site_period_summary <- data %>%
    dplyr::group_by(
      site_name,
      site_code,
      latitude,
      longitude,
      !!period_variable
    ) %>%
    dplyr::summarise(
      mean_species_richness = mean(
        species_richness,
        na.rm = TRUE
      ),
      sd = sd(
        species_richness,
        na.rm = TRUE
      ),
      se = sd(
        species_richness,
        na.rm = TRUE
      ) / sqrt(sum(!is.na(species_richness))),
      
      n_transects = sum(
        !is.na(species_richness)
      ),
      .groups = "drop"
    )
  
  return(site_period_summary)
}

# Creating summarised values for period and for period split -----
summarise_location_richness <- function(data, period_variable) {
  
  period_variable <- rlang::ensym(period_variable)
  
  # Average transects
  location_period_summary <- data %>%
    dplyr::group_by(
      location_g,
      !!period_variable
    ) %>%
    dplyr::summarise(
      mean_species_richness = mean(
        species_richness,
        na.rm = TRUE
      ),
      sd = sd(
        species_richness,
        na.rm = TRUE
      ),
      se = sd(
        species_richness,
        na.rm = TRUE
      ) / sqrt(sum(!is.na(species_richness))),
      
      n_transects = sum(
        !is.na(species_richness)
      ),
      .groups = "drop"
    )
  
  return(location_period_summary)
}

## Period comparisons ----
m1_fish_site_period <- m1_fish_sr_samples %>%
  summarise_site_richness(period)

m2_fish_site_period <- m2_fish_sr_samples %>%
  summarise_site_richness(period)

m2_inverts_site_period <- m2_inverts_sr_samples %>%
  summarise_site_richness(period)

m1_fish_location_period <- m1_fish_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period)

m2_fish_location_period <- m2_fish_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period)

m2_inverts_location_period <- m2_inverts_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period)

## Multiple Period comparisons ----
m1_fish_site_period_split <- m1_fish_sr_samples %>%
  summarise_site_richness(period_split)

m2_fish_site_period_split <- m2_fish_sr_samples %>%
  summarise_site_richness(period_split)

m2_inverts_site_period_split <- m2_inverts_sr_samples %>%
  summarise_site_richness(period_split)

### locations ----
m1_fish_location_period_split <- m1_fish_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period_split)

m2_fish_location_period_split <- m2_fish_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period_split)

m2_inverts_location_period_split <- m2_inverts_sr_samples %>%
  left_join(sa_sites) %>%
  summarise_location_richness(period_split)

# TODO percentage changes of sites

# Plot observed species richness by site ----
##  Plot settings ----
plot_output_root <- file.path(
  "plots",
  "rls_species_richness_observed"
)

plot_dirs <- c(
  period = file.path(plot_output_root, "period"),
  period_split = file.path(plot_output_root, "period_split"),
  temporal = file.path(plot_output_root, "temporal")
)

purrr::walk(
  unname(plot_dirs),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

metric_levels <- c(
  "M1 fish species richness",
  "M2 fish species richness",
  "M2 invertebrate species richness"
)

metric_period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

observed_plot_theme <- theme(
  axis.line.x = element_line(
    colour = "black",
    linewidth = 0.5
  ),
  axis.line.y = element_line(
    colour = "black",
    linewidth = 0.5
  ),
  panel.grid = element_blank(),
  strip.text = element_text(
    face = "bold",
    size = 13
  )
)

# Broad period plotting data ----

period_plot_data <- dplyr::bind_rows(
  
  m1_fish_site_period %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_site_period %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_site_period %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    !is.na(site_code),
    !is.na(period),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

# Split-period plotting data ----
period_split_plot_data <- dplyr::bind_rows(
  
  m1_fish_site_period_split %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_site_period_split %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_site_period_split %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    !is.na(site_code),
    !is.na(period_split),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = dplyr::if_else(
      period_split == "Pre-bloom",
      "Pre-bloom",
      "Bloom"
    )
  )

# Arrange the split periods chronologically
period_split_levels <- c(
  "Pre-bloom",
  period_split_plot_data$period_split %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)

period_split_plot_data <- period_split_plot_data %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    period_split = factor(
      period_split,
      levels = period_split_levels
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

# Temporal plotting data ----

temporal_plot_data <- dplyr::bind_rows(
  
  m1_fish_site_sr_average %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      sampling_event,
      sampling_event_start_date =
        as.Date(sampling_event_start_date),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_site_sr_average %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      sampling_event,
      sampling_event_start_date =
        as.Date(sampling_event_start_date),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_site_sr_average %>%
    dplyr::transmute(
      site_name,
      site_code = as.character(site_code),
      sampling_event,
      sampling_event_start_date =
        as.Date(sampling_event_start_date),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    !is.na(site_code),
    !is.na(sampling_event_start_date),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

plot_observed_period <- function(data) {
  
  ggplot(
    data,
    aes(
      x = period,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      legend.position = "none"
    )
}

plot_observed_period_split <- function(data) {
  
  ggplot(
    data,
    aes(
      x = period_split,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = 0.7,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_replace(
          x,
          "^Bloom ",
          ""
        )
      }
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = "Period",
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom"
    )
}

plot_observed_temporal <- function(data) {
  
  event_dates <- sort(
    unique(data$sampling_event_start_date)
  )
  
  # Width is measured in days for a Date x-axis
  if (length(event_dates) > 1) {
    
    date_gaps <- as.numeric(
      diff(event_dates)
    )
    
    date_gaps <- date_gaps[
      is.finite(date_gaps) &
        date_gaps > 0
    ]
    
    if (length(date_gaps) > 0) {
      
      bar_width <- min(
        120,
        max(
          5,
          min(date_gaps) * 0.7
        )
      )
      
    } else {
      
      bar_width <- 30
    }
    
  } else {
    
    bar_width <- 30
  }
  
  errorbar_width <- bar_width * 0.35
  
  ggplot(
    data,
    aes(
      x = sampling_event_start_date,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = bar_width,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = errorbar_width,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      ncol = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(
        mult = c(0.03, 0.03)
      ),
      guide = guide_axis(
        check.overlap = TRUE
      )
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1
      ),
      legend.position = "bottom"
    )
}

# Site lookup ----

site_lookup <- dplyr::bind_rows(
  period_plot_data %>%
    dplyr::select(site_code, site_name),
  
  period_split_plot_data %>%
    dplyr::select(site_code, site_name),
  
  temporal_plot_data %>%
    dplyr::select(site_code, site_name)
) %>%
  dplyr::filter(
    !is.na(site_code)
  ) %>%
  dplyr::arrange(
    site_code,
    site_name
  ) %>%
  dplyr::distinct(
    site_code,
    .keep_all = TRUE
  )

site_codes <- site_lookup$site_code

# Save one set of plots ----
save_site_species_richness_plots <- function(site_code_value) {
  
  site_name_value <- site_lookup %>%
    dplyr::filter(
      site_code == site_code_value
    ) %>%
    dplyr::pull(site_name)
  
  if (length(site_name_value) == 0) {
    
    site_name_value <- site_code_value
    
  } else {
    
    site_name_value <- site_name_value[[1]]
  }
  
  site_title <- paste0(
    site_name_value,
    " (",
    site_code_value,
    ")"
  )
  
  safe_site_name <- site_name_value %>%
    stringr::str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    stringr::str_replace_all(
      "^_|_$",
      ""
    )
  
  safe_site_code <- site_code_value %>%
    stringr::str_replace_all(
      "[^A-Za-z0-9_-]+",
      "_"
    ) %>%
    stringr::str_replace_all(
      "^_|_$",
      ""
    )
  
  safe_site_id <- paste0(
    safe_site_name,
    "_",
    safe_site_code
  )
  
  site_period_data <- period_plot_data %>%
    dplyr::filter(
      site_code == site_code_value
    )
  
  site_period_split_data <- period_split_plot_data %>%
    dplyr::filter(
      site_code == site_code_value
    )
  
  site_temporal_data <- temporal_plot_data %>%
    dplyr::filter(
      site_code == site_code_value
    ) %>%
    dplyr::arrange(
      metric,
      sampling_event_start_date
    )
  
  period_plot <- plot_observed_period(
    data = site_period_data#,
    # site_title = site_title
  )
  
  period_split_plot <- plot_observed_period_split(
    data = site_period_split_data#,
    # site_title = site_title
  )
  
  temporal_plot <- plot_observed_temporal(
    data = site_temporal_data#,
    # site_title = site_title
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["period"]],
      paste0(
        safe_site_id,
        "_sr_period.png"
      )
    ),
    plot = period_plot,
    width = 15,
    height = 5.5,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["period_split"]],
      paste0(
        safe_site_id,
        "_sr_period_split.png"
      )
    ),
    plot = period_split_plot,
    width = 17,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["temporal"]],
      paste0(
        safe_site_id,
        "_sr_temporal.png"
      )
    ),
    plot = temporal_plot,
    width = 9,
    height = 14,
    dpi = 300,
    bg = "white"
  )
  
  invisible(NULL)
}

# Run all sites ----

plot_log <- purrr::map_dfr(
  site_codes,
  function(site_code_value) {
    
    message(
      "Creating plots for site: ",
      site_code_value
    )
    
    tryCatch(
      {
        save_site_species_richness_plots(
          site_code_value
        )
        
        tibble::tibble(
          site_code = site_code_value,
          status = "Saved",
          error = NA_character_
        )
      },
      error = function(e) {
        
        tibble::tibble(
          site_code = site_code_value,
          status = "Failed",
          error = conditionMessage(e)
        )
      }
    )
  }
)

readr::write_csv(
  plot_log,
  file.path(
    plot_output_root,
    "plot_log.csv"
  )
)

plot_log %>%
  dplyr::count(status)

plot_log %>%
  dplyr::filter(status == "Failed")

# Plots species richness by location ----
##  Plot settings ----
plot_output_root <- file.path(
  "plots",
  "rls_species_richness_observed_location"
)

plot_dirs <- c(
  period = file.path(plot_output_root, "period"),
  period_split = file.path(plot_output_root, "period_split"),
  temporal = file.path(plot_output_root, "temporal")
)

purrr::walk(
  unname(plot_dirs),
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

metric_levels <- c(
  "M1 fish species richness",
  "M2 fish species richness",
  "M2 invertebrate species richness"
)

metric_period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

observed_plot_theme <- theme(
  axis.line.x = element_line(
    colour = "black",
    linewidth = 0.5
  ),
  axis.line.y = element_line(
    colour = "black",
    linewidth = 0.5
  ),
  panel.grid = element_blank(),
  strip.text = element_text(
    face = "bold",
    size = 13
  )
)

# Broad period plotting data ----

period_plot_data_location <- dplyr::bind_rows(
  
  m1_fish_location_period %>%
    dplyr::transmute(
      location_g,
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_location_period %>%
    dplyr::transmute(
      location_g,
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_location_period %>%
    dplyr::transmute(
      location_g,
      period,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    # !is.na(site_code),
    !is.na(period),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

# Split-period plotting data ----
period_split_plot_data_location <- dplyr::bind_rows(
  
  m1_fish_location_period_split %>%
    dplyr::transmute(
      location_g,
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_location_period_split %>%
    dplyr::transmute(
      location_g,
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_location_period_split %>%
    dplyr::transmute(
      location_g,
      period_split,
      estimate = mean_species_richness,
      se = se,
      n_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    # !is.na(site_code),
    !is.na(period_split),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = dplyr::if_else(
      period_split == "Pre-bloom",
      "Pre-bloom",
      "Bloom"
    )
  )

# Arrange the split periods chronologically
period_split_levels <- c(
  "Pre-bloom",
  period_split_plot_data_location$period_split %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)

period_split_plot_data_location <- period_split_plot_data_location %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    period_split = factor(
      period_split,
      levels = period_split_levels
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

# Temporal plotting data ----

temporal_plot_data_location <- dplyr::bind_rows(
  
  m1_fish_location_sr_average %>%
    dplyr::transmute(
      location_g,
      sampling_event_start_date = as.Date(paste0(start_year_month, "-01")),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[1]]
    ),
  
  m2_fish_location_sr_average %>%
    dplyr::transmute(
      location_g,
      sampling_event_start_date = as.Date(paste0(start_year_month, "-01")),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[2]]
    ),
  
  m2_inverts_location_sr_average %>%
    dplyr::transmute(
      location_g,
      sampling_event_start_date = as.Date(paste0(start_year_month, "-01")),
      period,
      period_split,
      estimate = mean,
      se,
      num_transects,
      metric = metric_levels[[3]]
    )
  
) %>%
  dplyr::filter(
    # !is.na(site_code),
    # !is.na(sampling_event_start_date),
    !is.na(estimate)
  ) %>%
  dplyr::mutate(
    period = factor(
      period,
      levels = c("Pre-bloom", "Bloom")
    ),
    metric = factor(
      metric,
      levels = metric_levels
    )
  )

# plot_observed_period <- function(data) {
  
  ggplot(
    data,
    aes(
      x = period,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = 0.6,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      legend.position = "none"
    )
}

# plot_observed_period_split <- function(data) {
  
  ggplot(
    data,
    aes(
      x = period_split,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = 0.7,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      nrow = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_replace(
          x,
          "^Bloom ",
          ""
        )
      }
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = "Period",
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "bottom"
    )
}

# plot_observed_temporal <- function(data) {
  
  event_dates <- sort(
    unique(data$sampling_event_start_date)
  )
  
  # Width is measured in days for a Date x-axis
  if (length(event_dates) > 1) {
    
    date_gaps <- as.numeric(
      diff(event_dates)
    )
    
    date_gaps <- date_gaps[
      is.finite(date_gaps) &
        date_gaps > 0
    ]
    
    if (length(date_gaps) > 0) {
      
      bar_width <- min(
        120,
        max(
          5,
          min(date_gaps) * 0.7
        )
      )
      
    } else {
      
      bar_width <- 30
    }
    
  } else {
    
    bar_width <- 30
  }
  
  errorbar_width <- bar_width * 0.35
  
  ggplot(
    data,
    aes(
      x = sampling_event_start_date,
      y = estimate,
      fill = period
    )
  ) +
    geom_col(
      width = bar_width,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(estimate - se, 0),
        ymax = estimate + se
      ),
      width = errorbar_width,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      ncol = 1,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(
      values = metric_period_cols,
      drop = FALSE
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(
        mult = c(0.03, 0.03)
      ),
      guide = guide_axis(
        check.overlap = TRUE
      )
    ) +
    scale_y_continuous(
      expand = expansion(
        mult = c(0, 0.08)
      )
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1
      ),
      legend.position = "bottom"
    )
}

# Site lookup ----

location_lookup <- dplyr::bind_rows(
  period_plot_data_location %>%
    dplyr::select(location_g),
  
  period_split_plot_data_location %>%
    dplyr::select(location_g),
  
  temporal_plot_data_location %>%
    dplyr::select(location_g)
) %>%
  dplyr::arrange(
    location_g
  ) %>%
  dplyr::distinct(
    location_g,
    .keep_all = TRUE
  )

location_codes <- location_lookup$location_g

# Save one set of plots ----
save_location_species_richness_plots <- function(location_code_value) {
  
  location_title <- paste0(
    location_code_value
  )
  
  safe_location_name <- location_code_value %>%
    stringr::str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    stringr::str_replace_all(
      "^_|_$",
      ""
    )
  
  location_period_data <- period_plot_data_location %>%
    dplyr::filter(
      location_g == location_code_value
    )
  
  location_period_split_data <- period_split_plot_data_location %>%
    dplyr::filter(
      location_g == location_code_value
    )
  
  location_temporal_data <- temporal_plot_data_location %>%
    dplyr::filter(
      location_g == location_code_value
    ) %>%
    dplyr::arrange(
      metric,
      sampling_event_start_date
    )
  
  period_plot <- plot_observed_period(
    data = location_period_data#,
    # site_title = site_title
  )
  
  period_split_plot <- plot_observed_period_split(
    data = location_period_split_data#,
    # site_title = site_title
  )
  
  temporal_plot <- plot_observed_temporal(
    data = location_temporal_data#,
    # site_title = site_title
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["period"]],
      paste0(
        safe_location_name,
        "_sr_period.png"
      )
    ),
    plot = period_plot,
    width = 15,
    height = 5.5,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["period_split"]],
      paste0(
        safe_location_name,
        "_sr_period_split.png"
      )
    ),
    plot = period_split_plot,
    width = 17,
    height = 6,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(
      plot_dirs[["temporal"]],
      paste0(
        safe_location_name,
        "_sr_temporal.png"
      )
    ),
    plot = temporal_plot,
    width = 9,
    height = 14,
    dpi = 300,
    bg = "white"
  )
  
  invisible(NULL)
}

# Run all sites ----

plot_log <- purrr::map_dfr(
  location_codes,
  function(location_code_value) {
    
    message(
      "Creating plots for location: ",
      location_code_value
    )
    
    tryCatch(
      {
        save_location_species_richness_plots(
          location_code_value
        )
        
        tibble::tibble(
          location_code = location_code_value,
          status = "Saved",
          error = NA_character_
        )
      },
      error = function(e) {
        
        tibble::tibble(
          site_code = location_code_value,
          status = "Failed",
          error = conditionMessage(e)
        )
      }
    )
  }
)

readr::write_csv(
  plot_log,
  file.path(
    plot_output_root,
    "plot_log.csv"
  )
)

plot_log %>%
  dplyr::count(status)

plot_log %>%
  dplyr::filter(status == "Failed")