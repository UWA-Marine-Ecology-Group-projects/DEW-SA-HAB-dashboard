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
  ) %>%
  rename(region = bruvsrepor)

# Convert a YYYY-MM value to the first day of that month.
month_to_date <- function(x) {
  x <- as.character(x)
  as.Date(ifelse(is.na(x), NA_character_, paste0(x, "-01")))
}

# read in cleaned data ----
m1_clean <- read_rds("data/tidy/rls_m1_count_and_length.rds") %>% rename(biomass_g = biomass)
m2_clean <- read_rds("data/tidy/rls_m2_fish_count_and_length.rds") %>% rename(biomass_g = biomass)

# read in empty surveys -----
m1_zeros <- read_rds("data/tidy/rls_m1_zeros.rds")
m2_zeros <- read_rds("data/tidy/rls_m2_fish_zeros.rds")

# Create surveys ----
# One row for every survey/block, including surveys where no fish were recorded
m1_surveys_blocks <- dplyr::bind_rows(
  m1_clean %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split),
  m1_zeros %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split)) %>%
  dplyr::distinct()

m2_surveys_blocks <- dplyr::bind_rows(
  m2_clean %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split),
  m2_zeros %>%
    dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split)) %>%
  dplyr::distinct()

m1_surveys_transects <- m1_surveys_blocks %>%
  dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, period, period_split)

m2_surveys_transects <- m2_surveys_blocks %>%
  dplyr::distinct(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, period, period_split)

# Create block B20 ----
m1_b20_blocks <- m1_clean %>%
  dplyr::filter(size_class >= 20) %>%
  ungroup() %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split) %>%
  dplyr::summarise(b20_g = sum(biomass_g)) %>%
  ungroup() %>%
  full_join(m1_surveys_blocks) %>%
  replace_na(list(b20_g = 0))

m2_b20_blocks <- m2_clean %>%
  dplyr::filter(size_class >= 20) %>%
  ungroup() %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, block, id, period, period_split) %>%
  dplyr::summarise(b20_g = sum(biomass_g)) %>%
  ungroup() %>%
  full_join(m2_surveys_blocks) %>%
  replace_na(list(b20_g = 0))
  
# Calculate sample B20 ----
m1_b20_samples <- m1_b20_blocks %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, period, period_split) %>%
  dplyr::summarise(
    mean_b20_g = mean(b20_g, na.rm = TRUE),
    block_sd = stats::sd(b20_g, na.rm = TRUE),
    n_blocks = dplyr::n_distinct(block),
    .groups = "drop"
  ) %>%
  dplyr::mutate(b20_kg = mean_b20_g/1000) %>%
  dplyr::full_join(m1_surveys_transects) %>%
  dplyr::mutate(
    b20_kg = dplyr::coalesce(b20_kg, 0),
    n_blocks = dplyr::coalesce(n_blocks, 0L)
  )

m2_b20_samples <- m2_b20_blocks %>%
  dplyr::group_by(survey_id, site_name, survey_date, depth, sampling_event, sampling_event_start_date, program, period, period_split) %>%
  dplyr::summarise(
    mean_b20_g = mean(b20_g, na.rm = TRUE),
    block_sd = stats::sd(b20_g, na.rm = TRUE),
    n_blocks = dplyr::n_distinct(block),
    .groups = "drop"
  ) %>%
  dplyr::mutate(b20_kg = mean_b20_g/1000) %>%
  dplyr::full_join(m2_surveys_transects) %>%
  dplyr::mutate(
    b20_kg = dplyr::coalesce(b20_kg, 0),
    n_blocks = dplyr::coalesce(n_blocks, 0L)
  )

hist(m1_b20_samples$b20_kg)
hist(m2_b20_samples$b20_kg)

# M1 B20 by region ----
m1_region_b20_average <- m1_b20_samples %>%
  ungroup() %>%
  left_join(sa_sites) %>%
  dplyr::mutate(start_year_month = str_sub(sampling_event_start_date, 1, 7)) %>%
  # time_date = month_to_date(start_year_month) %>%
  dplyr::group_by(region, period, period_split, start_year_month) %>%
  dplyr::summarise(mean = mean(b20_kg, na.rm = TRUE),
                   se   = sd(b20_kg, na.rm = TRUE) /
                     sqrt(sum(!is.na(b20_kg))),
                   num_transects = n(),
                   .groups = "drop") 

# TODO why does period have NAs

m2_region_b20_average <- m2_b20_samples %>%
  ungroup() %>%
  left_join(sa_sites) %>%
  dplyr::mutate(start_year_month = str_sub(sampling_event_start_date, 1, 7)) %>%
  # time_date = month_to_date(start_year_month) %>%
  dplyr::group_by(region, period, period_split, start_year_month) %>%
  dplyr::summarise(mean = mean(b20_kg, na.rm = TRUE),
                   se   = sd(b20_kg, na.rm = TRUE) /
                     sqrt(sum(!is.na(b20_kg))),
                   num_transects = n(),
                   .groups = "drop") 


# -----------------------------------------------------------------
# 3. Plot functions
# -----------------------------------------------------------------

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
      values = period_cols,
      drop = FALSE
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(\u00B1 SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(legend.position = "none")
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
      values = period_cols,
      drop = FALSE
    ) +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_replace(x, "^Bloom ", "")
      }
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = "Period",
      y = "Average species richness\n(\u00B1 SE)",
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
  
  event_dates <- sort(unique(data$time_date))
  
  # Width is measured in days for a Date x-axis.
  if (length(event_dates) > 1) {
    
    date_gaps <- as.numeric(diff(event_dates))
    date_gaps <- date_gaps[is.finite(date_gaps) & date_gaps > 0]
    
    if (length(date_gaps) > 0) {
      bar_width <- min(120, max(5, min(date_gaps) * 0.7))
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
      x = time_date,
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
      values = period_cols,
      drop = FALSE
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.03, 0.03)),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = NULL,
      y = "Average species richness\n(\u00B1 SE)",
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

# Save a plot only when that group has data for the requested plot type.
save_plot_if_present <- function(data, plot_function, filename, width, height) {
  
  if (nrow(data) == 0) {
    return(FALSE)
  }
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot_function(data),
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  
  TRUE
}