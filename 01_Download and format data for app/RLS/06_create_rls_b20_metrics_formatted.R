#################################################################
# Create and plot observed B20 biomass from RLS M1 and M2 fish data
#################################################################

library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(tidyr)

# -----------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------

metric_levels <- c(
  "M1 fish B20 biomass",
  "M2 fish B20 biomass"
)

period_levels <- c("Pre-bloom", "Bloom")

period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

plot_output_roots <- c(
  site = file.path("plots", "rls_b20_ste"),
  location = file.path("plots", "rls_b20_location"),
  region = file.path("plots", "rls_b20_region")
)

plot_types <- c("period", "period_split", "temporal")

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

# -----------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------

# Return the first non-missing value while retaining the input type.
first_non_missing <- function(x) {

  keep <- !is.na(x)

  if (is.character(x)) {
    keep <- keep & nzchar(trimws(x))
  }

  if (any(keep)) {
    return(x[which(keep)[1]])
  }

  # This creates one typed NA, including for Date vectors.
  x[NA_integer_][1]
}

# Convert a YYYY-MM value to the first day of that month.
month_to_date <- function(x) {
  x <- as.character(x)
  as.Date(ifelse(is.na(x), NA_character_, paste0(x, "-01")))
}

make_safe_filename <- function(x) {
  
  output <- x %>%
    as.character() %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
  
  dplyr::if_else(
    is.na(output) | output == "",
    "unnamed_group",
    output
  )
}

# Create one row for every surveyed block, including empty blocks.
make_block_registry <- function(clean_data, zero_data) {
  
  block_keys <- c(
    "block"
  )
  
  metadata_vars <- c(
    "transect",
    "site_code",
    "site_name",
    "sampling_event",
    "sampling_event_start_date",
    "program",
    "period",
    "period_split"
  )
  
  block_records <- dplyr::bind_rows(
    clean_data %>%
      dplyr::select(dplyr::any_of(c(block_keys, metadata_vars))),
    zero_data %>%
      dplyr::select(dplyr::any_of(c(block_keys, metadata_vars)))
  ) %>%
    distinct()
}

# Sum biomass of fish in size classes >= 20 cm within each block.
calculate_b20_blocks <- function(clean_data, block_registry) {

  observed_b20 <- clean_data %>%
    dplyr::filter(
      !is.na(size_class),
      size_class >= 20
    ) %>%
    ungroup() %>%
    dplyr::group_by(transect, block) %>%
    dplyr::summarise(
      b20_g = sum(biomass_g, na.rm = TRUE),
      .groups = "drop"
    ) %>% glimpse()
  
  # Start from the complete block registry. A block with no fish >= 20 cm
  # therefore receives B20 biomass of zero.
  block_registry %>%
    dplyr::left_join(observed_b20) %>%
    dplyr::mutate(
      b20_g = dplyr::coalesce(b20_g, 0)
    )
}

# Average blocks to make one B20 value for every surveyed transect/sample.
average_b20_blocks <- function(block_data, metric_name) {
  
  sample_keys <- c(
    "transect"
  )
  
  message("block vars")
  
  metadata_vars <- setdiff(
    names(block_data),
    c(sample_keys, "block", "b20_g")
  ) %>% glimpse()
  
  block_data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(sample_keys))
    ) %>%
    dplyr::summarise(
      mean_b20_g = mean(b20_g, na.rm = TRUE),
      block_sd_g = stats::sd(b20_g, na.rm = TRUE),
      n_blocks = dplyr::n_distinct(block),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      b20_kg = mean_b20_g / 1000,
      block_sd_kg = block_sd_g / 1000,
      metric = metric_name
    ) 
}

# Perform all repeated clean data -> block B20 -> sample B20 steps.
prepare_b20_dataset <- function(
    clean_path,
    zero_path,
    metric_name) {
  
  clean_data <- readr::read_rds(clean_path) %>%
    dplyr::rename(biomass_g = biomass) 
  
  zero_data <- readr::read_rds(zero_path) #%>%
    # select(survey_id, site_name, transect, block, total, sampling_event)
  
  block_registry <- make_block_registry(
    clean_data = clean_data,
    zero_data = zero_data
  )
  
  block_b20 <- calculate_b20_blocks(
    clean_data = clean_data,
    block_registry = block_registry
  )
  
  transect_registry <- block_registry %>%
    distinct(transect, site_code, site_name)
  
  sample_b20 <- average_b20_blocks(
    block_data = block_b20,
    metric_name = metric_name
  ) %>%
    left_join(transect_registry)
  
  list(
    blocks = block_b20,
    samples = sample_b20
  )
}

# Add site code, location and region once, using the shapefile as the
# canonical lookup. If site_code is absent, a unique site_name match is used.
add_spatial_lookup <- function(data, lookup) {
  
  if (!"site_code" %in% names(data)) {
    data$site_code <- NA_character_
  }
  
  if (!"site_name" %in% names(data)) {
    data$site_name <- NA_character_
  }
  
  unique_name_lookup <- lookup %>%
    dplyr::filter(!is.na(site_name), site_name != "") %>%
    dplyr::add_count(site_name, name = "n_name") %>%
    dplyr::filter(n_name == 1) %>%
    dplyr::transmute(
      site_name,
      site_code_from_name = site_code
    )
  
  lookup_by_code <- lookup %>%
    dplyr::filter(!is.na(site_code), site_code != "") %>%
    dplyr::distinct(site_code, .keep_all = TRUE) %>%
    dplyr::rename(site_name_lookup = site_name)
  
  data %>%
    dplyr::mutate(
      site_code = dplyr::na_if(
        trimws(as.character(site_code)),
        ""
      ),
      site_name = dplyr::na_if(
        trimws(as.character(site_name)),
        ""
      )
    ) %>%
    dplyr::left_join(
      unique_name_lookup,
      by = "site_name"
    ) %>%
    dplyr::mutate(
      site_code = dplyr::coalesce(
        site_code,
        site_code_from_name
      )
    ) %>%
    dplyr::select(
      -site_code_from_name,
      -dplyr::any_of(c("location", "region"))
    ) %>%
    dplyr::left_join(
      lookup_by_code,
      by = "site_code"
    ) %>%
    dplyr::mutate(
      site_name = dplyr::coalesce(
        site_name,
        site_name_lookup
      )
    ) %>%
    dplyr::select(-site_name_lookup)
}

# Convert samples into common site, location and region columns.
expand_spatial_levels <- function(data) {
  
  dplyr::bind_rows(
    
    data %>%
      dplyr::transmute(
        spatial_level = "site",
        group_id = dplyr::coalesce(
          as.character(site_code),
          as.character(site_name)
        ),
        group_name = dplyr::coalesce(
          as.character(site_name),
          as.character(site_code)
        ),
        time_id = as.character(sampling_event),
        time_date = as.Date(sampling_event_start_date),
        metric,
        b20_kg,
        period = as.character(period),
        period_split = as.character(period_split)
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "location",
        group_id = as.character(location),
        group_name = as.character(location),
        time_id = as.character(start_year_month),
        time_date = month_to_date(start_year_month),
        metric,
        b20_kg,
        period = as.character(period),
        period_split = as.character(period_split)
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "region",
        group_id = as.character(region),
        group_name = as.character(region),
        time_id = as.character(start_year_month),
        time_date = month_to_date(start_year_month),
        metric,
        b20_kg,
        period = as.character(period),
        period_split = as.character(period_split)
      )
  ) %>%
    dplyr::filter(
      !is.na(group_id),
      group_id != ""
    )
}

# Generic mean, SD, SE and sample-size summary for any spatial grouping.
summarise_b20 <- function(
    data,
    group_vars,
    retain_vars = character()) {
  
  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(retain_vars),
        first_non_missing
      ),
      n_transects = sum(!is.na(b20_kg)),
      estimate = dplyr::if_else(
        n_transects > 0,
        mean(b20_kg, na.rm = TRUE),
        NA_real_
      ),
      sd = dplyr::if_else(
        n_transects > 1,
        stats::sd(b20_kg, na.rm = TRUE),
        NA_real_
      ),
      se = sd / sqrt(n_transects),
      .groups = "drop"
    )
}

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
      drop = FALSE,
      na.value = "grey75"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = NULL,
      y = "Average B20 biomass (kg)\n(\u00B1 SE)",
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
      drop = FALSE,
      na.value = "grey75"
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
      y = "Average B20 biomass (kg)\n(\u00B1 SE)",
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
      drop = FALSE,
      na.value = "grey75"
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
      y = "Average B20 biomass (kg)\n(\u00B1 SE)",
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
save_plot_if_present <- function(
    data,
    plot_function,
    filename,
    width,
    height) {
  
  if (nrow(data) == 0 || all(is.na(data$estimate))) {
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

# -----------------------------------------------------------------
# 4. Read spatial lookup
# -----------------------------------------------------------------

sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  CheckEM::clean_names() %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    site_code = as.character(site_code),
    site_name_lookup = site_name,
    region = bruvsrepor,
    location = location_g
  ) %>%
  dplyr::distinct(site_code, .keep_all = TRUE)

# -----------------------------------------------------------------
# 5. Calculate sample-level B20 for M1 and M2 fish
# -----------------------------------------------------------------
sl_m1 <- readr::read_rds("data/tidy/rls_m1_surveys_final.rds") %>%
  dplyr::select(-c(block, id, location)) %>%
  dplyr::distinct()

sl_m2_fish <- readr::read_rds("data/tidy/rls_m2_fish_surveys_final.rds") %>%
  dplyr::select(-c(block, id, location)) %>%
  dplyr::distinct()

m1_b20 <- prepare_b20_dataset(
  clean_path = "data/tidy/rls_m1_count_and_length.rds",
  zero_path = "data/tidy/rls_m1_zeros.rds",
  metric_name = metric_levels[[1]]
)

m2_b20 <- prepare_b20_dataset(
  clean_path = "data/tidy/rls_m2_fish_count_and_length.rds",
  zero_path = "data/tidy/rls_m2_fish_zeros.rds",
  metric_name = metric_levels[[2]]
)


# Retain familiar object names for inspection.
m1_b20_blocks <- m1_b20$blocks
m2_b20_blocks <- m2_b20$blocks

m1_b20_samples <- m1_b20$samples %>%
  left_join(sl_m1) # 1821

m2_b20_samples <- m2_b20$samples %>%
  left_join(sl_m2_fish) # 1837

nrow(m1_b20_samples)
nrow(m2_b20_samples)

# Combine M1 and M2 now, so all later operations are performed once.
b20_samples <- dplyr::bind_rows(m1_b20_samples, m2_b20_samples) %>%
  left_join(sa_sites) %>%
  dplyr::mutate(metric = factor(metric, levels = metric_levels))

# Sites listed here could not be matched to a location and/or region. They
# can still produce site plots, but cannot be included at broader levels.
unmatched_spatial_rows <- b20_samples %>%
  dplyr::filter(is.na(location) | is.na(region)) %>%
  dplyr::distinct(site_code, site_name, location, region)

if (nrow(unmatched_spatial_rows) > 0) {
  message(
    nrow(unmatched_spatial_rows),
    " site lookup rows are missing location and/or region. ",
    "Inspect unmatched_spatial_rows."
  )
}

# Rows remaining here had neither period nor a usable period_split value in
# the source clean/zero metadata. They will appear grey in temporal plots.
missing_period_rows <- b20_samples %>%
  dplyr::filter(is.na(period)) %>%
  dplyr::distinct(
    metric,
    survey_id,
    site_code,
    site_name,
    survey_date,
    sampling_event,
    sampling_event_start_date,
    period_split
  )

if (nrow(missing_period_rows) > 0) {
  message(
    nrow(missing_period_rows),
    " B20 sample rows still have missing period metadata. ",
    "Inspect missing_period_rows."
  )
}

# Optional diagnostics:
# hist(m1_b20_samples$b20_kg)
# hist(m2_b20_samples$b20_kg)

# -----------------------------------------------------------------
# 6. Make one common table for site, location and region
# -----------------------------------------------------------------

spatial_samples <- expand_spatial_levels(b20_samples)

period_split_levels <- c(
  "Pre-bloom",
  spatial_samples$period_split %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)

# Broad pre-bloom versus bloom summaries.
period_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period)) %>%
  summarise_b20(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "metric",
      "period"
    )
  ) %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::mutate(
    period = factor(period, levels = period_levels),
    metric = factor(metric, levels = metric_levels)
  )

# Pre-bloom plus individual bloom-period summaries.
period_split_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period_split)) %>%
  summarise_b20(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "metric",
      "period_split"
    )
  ) %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::mutate(
    period = dplyr::if_else(
      period_split == "Pre-bloom",
      "Pre-bloom",
      "Bloom"
    ),
    period = factor(period, levels = period_levels),
    period_split = factor(
      period_split,
      levels = period_split_levels
    ),
    metric = factor(metric, levels = metric_levels)
  )

# Site temporal summaries use sampling events. Location and region temporal
# summaries use year-month. Period fields are retained rather than used as
# grouping fields, so a missing value cannot create a duplicate time point.
temporal_summary <- spatial_samples %>%
  dplyr::filter(!is.na(time_date)) %>%
  summarise_b20(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "metric",
      "time_id",
      "time_date"
    ),
    retain_vars = c("period", "period_split")
  ) %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::mutate(
    period = factor(period, levels = period_levels),
    period_split = factor(
      period_split,
      levels = period_split_levels
    ),
    metric = factor(metric, levels = metric_levels)
  )

# -----------------------------------------------------------------
# 7. Save site/sampling-event summaries
# -----------------------------------------------------------------

site_temporal_output <- temporal_summary %>%
  dplyr::filter(spatial_level == "site") %>%
  dplyr::transmute(
    site_name = group_name,
    site_code = group_id,
    sampling_event = type.convert(time_id, as.is = TRUE),
    sampling_event_start_date = time_date,
    period = as.character(period),
    period_split = as.character(period_split),
    mean = estimate,
    se,
    num_transects = n_transects,
    metric
  )

site_average_paths <- c(
  "M1 fish B20 biomass" =
    "data/tidy/rls_m1_fish_b20_average_per_site.rds",
  "M2 fish B20 biomass" =
    "data/tidy/rls_m2_fish_b20_average_per_site.rds"
)

purrr::iwalk(
  site_average_paths,
  function(path, metric_name) {
    
    site_temporal_output %>%
      dplyr::filter(as.character(metric) == metric_name) %>%
      dplyr::select(-metric) %>%
      readr::write_rds(path)
  }
)

# -----------------------------------------------------------------
# 8. Create output directories once
# -----------------------------------------------------------------

purrr::walk(
  unname(plot_output_roots),
  function(root) {
    
    purrr::walk(
      file.path(root, plot_types),
      function(path) {
        dir.create(
          path,
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
    )
  }
)

# One lookup table covers sites, locations and regions.
group_lookup <- dplyr::bind_rows(
  period_summary %>%
    dplyr::select(spatial_level, group_id, group_name),
  period_split_summary %>%
    dplyr::select(spatial_level, group_id, group_name),
  temporal_summary %>%
    dplyr::select(spatial_level, group_id, group_name)
) %>%
  dplyr::arrange(spatial_level, group_id, group_name) %>%
  dplyr::distinct(spatial_level, group_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    file_stub = dplyr::if_else(
      spatial_level == "site",
      paste(group_name, group_id, sep = "_"),
      group_id
    ),
    safe_id = make_safe_filename(file_stub)
  )

# -----------------------------------------------------------------
# 9. Save period, split-period and temporal plots for one group
# -----------------------------------------------------------------

save_b20_plots <- function(
    spatial_level_value,
    group_id_value,
    safe_id_value) {
  
  output_root <- unname(
    plot_output_roots[[spatial_level_value]]
  )
  
  group_period_data <- period_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    )
  
  group_period_split_data <- period_split_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    )
  
  group_temporal_data <- temporal_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    ) %>%
    dplyr::arrange(metric, time_date)
  
  saved <- c(
    period = save_plot_if_present(
      data = group_period_data,
      plot_function = plot_observed_period,
      filename = file.path(
        output_root,
        "period",
        paste0(safe_id_value, "_b20_period.png")
      ),
      width = 12,
      height = 5.5
    ),
    period_split = save_plot_if_present(
      data = group_period_split_data,
      plot_function = plot_observed_period_split,
      filename = file.path(
        output_root,
        "period_split",
        paste0(safe_id_value, "_b20_period_split.png")
      ),
      width = 14,
      height = 6
    ),
    temporal = save_plot_if_present(
      data = group_temporal_data,
      plot_function = plot_observed_temporal,
      filename = file.path(
        output_root,
        "temporal",
        paste0(safe_id_value, "_b20_temporal.png")
      ),
      width = 9,
      height = 10
    )
  )
  
  if (!any(saved)) {
    stop("No plot data were available for this group.")
  }
  
  names(saved)[saved]
}

# -----------------------------------------------------------------
# 10. Run all sites, locations and regions with one loop
# -----------------------------------------------------------------

plot_log <- purrr::pmap_dfr(
  group_lookup,
  function(
    spatial_level,
    group_id,
    group_name,
    file_stub,
    safe_id) {
    
    message(
      "Creating B20 plots for ",
      spatial_level,
      ": ",
      group_name
    )
    
    tryCatch(
      {
        plots_saved <- save_b20_plots(
          spatial_level_value = spatial_level,
          group_id_value = group_id,
          safe_id_value = safe_id
        )
        
        tibble::tibble(
          spatial_level = spatial_level,
          group_id = group_id,
          group_name = group_name,
          status = "Saved",
          plots_saved = paste(plots_saved, collapse = ", "),
          error = NA_character_
        )
      },
      error = function(e) {
        
        tibble::tibble(
          spatial_level = spatial_level,
          group_id = group_id,
          group_name = group_name,
          status = "Failed",
          plots_saved = NA_character_,
          error = conditionMessage(e)
        )
      }
    )
  }
)

# Save one log within each spatial output folder.
purrr::iwalk(
  plot_output_roots,
  function(root, spatial_level_value) {
    
    plot_log %>%
      dplyr::filter(
        spatial_level == spatial_level_value
      ) %>%
      readr::write_csv(
        file.path(root, "plot_log.csv")
      )
  }
)

plot_log %>%
  dplyr::count(spatial_level, status)

plot_log %>%
  dplyr::filter(status == "Failed")
