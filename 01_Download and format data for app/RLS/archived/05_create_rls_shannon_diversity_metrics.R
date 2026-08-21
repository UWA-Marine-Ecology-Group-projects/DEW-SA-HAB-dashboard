#################################################################
# Create and plot observed Shannon diversity from RLS M1 and M2 data
#################################################################

library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------

metric_levels <- c(
  "M1 fish Shannon diversity",
  "M2 fish Shannon diversity",
  "M2 invertebrate Shannon diversity"
)

period_levels <- c("Pre-bloom", "Bloom")

period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

# Keep the existing output-folder names.
plot_output_roots <- c(
  site = file.path("plots", "rls_shannon_site"),
  location = file.path("plots", "rls_shannon_location"),
  region = file.path("plots", "rls_shannon_region")
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

# Calculates Shannon diversity for individual blocks before blocks are averaged.
calculate_block_diversity <- function(data, dataset_name = "dataset") {
  
  # Keep every block so blocks with no individuals are not lost after
  # filtering total > 0. They are assigned Shannon diversity = 0 below.
  all_blocks <- data %>%
    dplyr::distinct(
      survey_id,
      survey_date,
      depth,
      block
    )
  
  # Find spp and identified-species conflicts within the same block.
  samples_with_both <- data %>%
    dplyr::group_by(
      survey_id,
      survey_date,
      depth,
      block,
      family,
      genus
    ) %>%
    dplyr::summarise(
      spp_present = any(species == "spp" & total > 0),
      identified_species_present = any(species != "spp" & total > 0),
      .groups = "drop"
    ) %>%
    dplyr::filter(spp_present, identified_species_present)
  
  if (nrow(samples_with_both) > 0) {
    
    n_blocks <- samples_with_both %>%
      dplyr::distinct(survey_id, survey_date, depth, block) %>%
      nrow()
    
    message(
      dataset_name,
      ": found ",
      nrow(samples_with_both),
      " block/genus combinations across ",
      n_blocks,
      paste0(
        " blocks containing both an spp record and an identified ",
        "species. The spp records will be removed."
      )
    )
    
  } else {
    
    message(
      dataset_name,
      paste0(
        ": no blocks contained both an spp record and an ",
        "identified species from the same genus."
      )
    )
  }
  
  positive_taxa <- data %>%
    dplyr::group_by(
      survey_id,
      survey_date,
      depth,
      block,
      family,
      genus
    ) %>%
    dplyr::mutate(
      identified_species_present = any(species != "spp" & total > 0)
    ) %>%
    dplyr::filter(
      !(species == "spp" & total > 0 & identified_species_present)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total > 0)
  
  block_diversity <- positive_taxa %>%
    dplyr::group_by(survey_id, survey_date, depth, block) %>%
    dplyr::summarise(
      total_abundance = sum(total),
      shannon = {
        p <- total / sum(total)
        -sum(p * log(p))
      },
      .groups = "drop"
    )
  
  diversity <- all_blocks %>%
    dplyr::left_join(
      block_diversity,
      by = c("survey_id", "survey_date", "depth", "block")
    ) %>%
    dplyr::mutate(
      total_abundance = dplyr::coalesce(total_abundance, 0),
      # Operational convention retained from the original script:
      # a sampled block containing no individuals has diversity zero.
      shannon = dplyr::coalesce(shannon, 0)
    )
  
  attr(diversity, "samples_with_both") <- samples_with_both
  
  diversity
}

# Performs the repeated count-file -> block diversity -> sample diversity steps.
prepare_diversity_dataset <- function(
    count_path,
    survey_list,
    dataset_name,
    metric_name) {
  
  block_diversity <- readr::read_rds(count_path) %>%
    calculate_block_diversity(dataset_name = dataset_name)
  
  conflicts <- attr(block_diversity, "samples_with_both")
  
  sample_diversity <- block_diversity %>%
    dplyr::group_by(survey_id, survey_date, depth) %>%
    dplyr::summarise(
      # Use a temporary name so block_sd is calculated from the individual
      # block values rather than from the newly summarised sample mean.
      mean_shannon = mean(shannon, na.rm = TRUE),
      block_sd = stats::sd(shannon, na.rm = TRUE),
      n_blocks = dplyr::n_distinct(block),
      .groups = "drop"
    ) %>%
    dplyr::rename(shannon = mean_shannon) %>%
    # A full join preserves the original script's treatment of samples that
    # occur in the survey list but are absent from the count table.
    dplyr::full_join(
      survey_list,
      by = c("survey_id", "survey_date", "depth")
    ) %>%
    dplyr::mutate(
      shannon = dplyr::coalesce(shannon, 0),
      n_blocks = dplyr::coalesce(n_blocks, 0L),
      metric = metric_name
    )
  
  list(
    samples = sample_diversity,
    conflicts = conflicts
  )
}

# Convert a YYYY-MM value to the first day of that month.
month_to_date <- function(x) {
  x <- as.character(x)
  as.Date(ifelse(is.na(x), NA_character_, paste0(x, "-01")))
}

# Convert one sample table into common site, location and region columns.
# This means all subsequent summaries and plot-saving code can be run once.
expand_spatial_levels <- function(data) {
  
  dplyr::bind_rows(
    
    data %>%
      dplyr::transmute(
        spatial_level = "site",
        group_id = as.character(site_code),
        group_name = dplyr::coalesce(
          as.character(site_name),
          as.character(site_code)
        ),
        latitude,
        longitude,
        time_id = as.character(sampling_event),
        time_date = as.Date(sampling_event_start_date),
        metric,
        shannon,
        period = as.character(period),
        period_split = as.character(period_split)
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "location",
        group_id = as.character(location_g),
        group_name = as.character(location_g),
        latitude = NA_real_,
        longitude = NA_real_,
        time_id = as.character(start_year_month),
        time_date = month_to_date(start_year_month),
        metric,
        shannon,
        period = as.character(period),
        period_split = as.character(period_split)
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "region",
        group_id = as.character(region),
        group_name = as.character(region),
        latitude = NA_real_,
        longitude = NA_real_,
        time_id = as.character(start_year_month),
        time_date = month_to_date(start_year_month),
        metric,
        shannon,
        period = as.character(period),
        period_split = as.character(period_split)
      )
  ) %>%
    dplyr::filter(
      !is.na(group_id),
      group_id != ""
    )
}

# Generic mean, SD, SE and sample-size summary for any grouping columns.
summarise_diversity <- function(data, group_vars) {
  
  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::summarise(
      n_transects = sum(!is.na(shannon)),
      estimate = ifelse(
        n_transects > 0,
        mean(shannon, na.rm = TRUE),
        NA_real_
      ),
      sd = ifelse(
        n_transects > 1,
        stats::sd(shannon, na.rm = TRUE),
        NA_real_
      ),
      se = sd / sqrt(n_transects),
      .groups = "drop"
    )
}

make_safe_filename <- function(x) {
  
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
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
      drop = FALSE
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = NULL,
      y = "Average Shannon diversity index\n(\u00B1 SE)",
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
      y = "Average Shannon diversity index\n(\u00B1 SE)",
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
      y = "Average Shannon diversity index\n(\u00B1 SE)",
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

# -----------------------------------------------------------------
# 4. Read metadata and survey lists
# -----------------------------------------------------------------

sa_sites <- sf::read_sf(
  "dev/Dive_sites_2026_07_14.shp"
) %>%
  CheckEM::clean_names() %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    site_code = as.character(site_code),
    site_name_lookup = site_name,
    location_g,
    region = bruvsrepor
  ) %>%
  dplyr::distinct(site_code, .keep_all = TRUE)

sl_m1 <- readr::read_rds(
  "data/tidy/rls_m1_survey_list.rds"
) %>%
  dplyr::select(-block) %>%
  dplyr::distinct()

sl_m2 <- readr::read_rds(
  "data/tidy/rls_m2_survey_list.rds"
) %>%
  dplyr::select(-block) %>%
  dplyr::distinct()

# -----------------------------------------------------------------
# 5. Calculate Shannon diversity for the three datasets
# -----------------------------------------------------------------

m1_fish <- prepare_diversity_dataset(
  count_path = "data/tidy/rls_m1_complete_count.rds",
  survey_list = sl_m1,
  dataset_name = "M1 fish",
  metric_name = metric_levels[[1]]
)

m2_fish <- prepare_diversity_dataset(
  count_path = "data/tidy/rls_m2_fish_complete_count.rds",
  survey_list = sl_m2,
  dataset_name = "M2 fish",
  metric_name = metric_levels[[2]]
)

m2_inverts <- prepare_diversity_dataset(
  count_path = "data/tidy/rls_m2_inverts_complete_count.rds",
  survey_list = sl_m2,
  dataset_name = "M2 invertebrates",
  metric_name = metric_levels[[3]]
)

# Retain the conflict tables for inspection.
spp_conflicts <- list(
  m1_fish = m1_fish$conflicts,
  m2_fish = m2_fish$conflicts,
  m2_inverts = m2_inverts$conflicts
)

# Combine the metrics now, rather than repeating every later operation.
diversity_samples <- dplyr::bind_rows(
  m1_fish$samples,
  m2_fish$samples,
  m2_inverts$samples
) %>%
  dplyr::mutate(
    site_code = as.character(site_code),
    sampling_event_start_date = as.Date(sampling_event_start_date),
    metric = factor(metric, levels = metric_levels)
  ) %>%
  # Use the shapefile as the single source for location and region.
  dplyr::select(-dplyr::any_of(c("location_g", "region"))) %>%
  dplyr::left_join(sa_sites, by = "site_code") %>%
  dplyr::mutate(
    site_name = dplyr::coalesce(site_name, site_name_lookup)
  ) %>%
  dplyr::select(-site_name_lookup)

# -----------------------------------------------------------------
# 6. Make one common table for site, location and region
# -----------------------------------------------------------------

spatial_samples <- expand_spatial_levels(diversity_samples)

period_split_levels <- c(
  "Pre-bloom",
  spatial_samples$period_split %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)

# Broad pre-bloom versus bloom summaries. At location and region level,
# these are means of all available transect-level Shannon values, matching
# the weighting used in the original site summaries: groups with more
# sampled transects contribute more observations.
period_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period)) %>%
  summarise_diversity(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "latitude",
      "longitude",
      "metric",
      "period"
    )
  ) %>%
  dplyr::filter(!is.na(estimate)) %>%
  dplyr::mutate(
    period = factor(period, levels = period_levels),
    metric = factor(metric, levels = metric_levels)
  )

# Pre-bloom plus individual bloom-month summaries.
period_split_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period_split)) %>%
  summarise_diversity(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "latitude",
      "longitude",
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

# Sampling-event summaries for sites and monthly summaries for
# locations and regions.
temporal_summary <- spatial_samples %>%
  dplyr::filter(!is.na(time_date)) %>%
  summarise_diversity(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
      "latitude",
      "longitude",
      "metric",
      "time_id",
      "time_date",
      "period",
      "period_split"
    )
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
# 7. Save site/sampling-event summaries in the existing file format
# -----------------------------------------------------------------

site_temporal_output <- temporal_summary %>%
  dplyr::filter(spatial_level == "site") %>%
  dplyr::transmute(
    site_name = group_name,
    site_code = group_id,
    sampling_event = type.convert(time_id, as.is = TRUE),
    latitude,
    longitude,
    period = as.character(period),
    period_split = as.character(period_split),
    sampling_event_start_date = time_date,
    mean = estimate,
    se,
    num_transects = n_transects,
    metric
  )

site_average_paths <- c(
  "M1 fish Shannon diversity" =
    "data/tidy/rls_m1_fish_shannon_average_per_site.rds",
  "M2 fish Shannon diversity" =
    "data/tidy/rls_m2_fish_shannon_average_per_site.rds",
  "M2 invertebrate Shannon diversity" =
    "data/tidy/rls_m2_inverts_shannon_average_per_site.rds"
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
# Build it from the actual plot summaries so groups with no usable data
# are not sent to the saving loop.
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

save_shannon_diversity_plots <- function(
    spatial_level_value,
    group_id_value,
    safe_id_value) {
  
  output_root <- unname(plot_output_roots[[spatial_level_value]])
  
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
        paste0(safe_id_value, "_shannon_period.png")
      ),
      width = 15,
      height = 5.5
    ),
    period_split = save_plot_if_present(
      data = group_period_split_data,
      plot_function = plot_observed_period_split,
      filename = file.path(
        output_root,
        "period_split",
        paste0(safe_id_value, "_shannon_period_split.png")
      ),
      width = 17,
      height = 6
    ),
    temporal = save_plot_if_present(
      data = group_temporal_data,
      plot_function = plot_observed_temporal,
      filename = file.path(
        output_root,
        "temporal",
        paste0(safe_id_value, "_shannon_temporal.png")
      ),
      width = 9,
      height = 14
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
  function(spatial_level, group_id, group_name, file_stub, safe_id) {
    
    message(
      "Creating plots for ",
      spatial_level,
      ": ",
      group_name
    )
    
    tryCatch(
      {
        plots_saved <- save_shannon_diversity_plots(
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
      dplyr::filter(spatial_level == spatial_level_value) %>%
      readr::write_csv(file.path(root, "plot_log.csv"))
  }
)

plot_log %>%
  dplyr::count(spatial_level, status)

plot_log %>%
  dplyr::filter(status == "Failed")

