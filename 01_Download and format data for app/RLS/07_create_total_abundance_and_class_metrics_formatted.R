#################################################################
# Create and plot observed total abundance from RLS M1 and M2 data
#
# Metrics:
#   - M1 fish total abundance
#   - M2 fish total abundance
#   - M2 invertebrate total abundance
#   - M2 invertebrate abundance for each value in the `phylum` column
#################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(readr)
library(tidyr)
library(purrr)

# -----------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------

period_levels <- c("Pre-bloom", "Bloom")

period_cols <- c(
  "Pre-bloom" = "#193b73",
  "Bloom" = "#92bd83"
)

plot_output_roots <- c(
  site = file.path("plots", "rls_abundance_site"),
  location = file.path("plots", "rls_abundance_location"),
  region = file.path("plots", "rls_abundance_region")
)

plot_families <- c("total_abundance", "m2_inverts")
plot_types <- c("period", "period_split", "temporal")

observed_plot_theme <- theme(
  axis.line.x = element_line(colour = "black", linewidth = 0.5),
  axis.line.y = element_line(colour = "black", linewidth = 0.5),
  panel.grid = element_blank(),
  strip.text = element_text(face = "bold", size = 13)
)

# -----------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------

# Calculate total abundance within each block.
#
# Unlike species richness, do NOT remove "spp" records where identified
# species from the same genus are also present. For abundance, all counted
# individuals should contribute to the total.
calculate_block_total_abundance <- function(data, metric_name) {
  data %>%
    dplyr::group_by(transect, block) %>%
    dplyr::summarise(
      abundance = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(metric = metric_name)
}

# Calculate total M2 invertebrate abundance and abundance by phylum.
# Missing phylum x block combinations are explicitly filled with zero.
calculate_block_invert_abundance <- function(
    data,
    total_metric_name = "M2 invertebrate total abundance") {
  
  if (!"phylum" %in% names(data)) {
    stop("The M2 invertebrate dataset does not contain a `phylum` column.")
  }
  
  data <- data %>%
    dplyr::mutate(phylum = as.character(phylum))
  
  invert_phylumes <- data %>%
    dplyr::filter(
      !is.na(phylum),
      stringr::str_trim(phylum) != ""
    ) %>%
    dplyr::distinct(phylum) %>%
    dplyr::arrange(phylum) %>%
    dplyr::pull(phylum)
  
  # Total abundance includes every invertebrate record, even if phylum is NA.
  total_abundance <- calculate_block_total_abundance(
    data = data,
    metric_name = total_metric_name
  )
  
  if (length(invert_phylumes) == 0) {
    warning(
      "No non-missing invertebrate phylumes were found. ",
      "Only total M2 invertebrate abundance will be calculated."
    )
    return(total_abundance)
  }
  
  block_keys <- data %>%
    dplyr::distinct(transect, block)
  
  phylum_abundance_observed <- data %>%
    dplyr::filter(
      !is.na(phylum),
      stringr::str_trim(phylum) != ""
    ) %>%
    dplyr::group_by(transect, block, phylum) %>%
    dplyr::summarise(
      abundance = sum(total, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create every surveyed block x phylum combination so absent phylumes are 0.
  phylum_lookup <- tibble::tibble(
    phylum = invert_phylumes,
    .join_key = 1L
  )
  
  phylum_abundance <- block_keys %>%
    dplyr::mutate(.join_key = 1L) %>%
    dplyr::left_join(
      phylum_lookup,
      by = ".join_key",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-.join_key) %>%
    dplyr::left_join(
      phylum_abundance_observed,
      by = c("transect", "block", "phylum")
    ) %>%
    dplyr::mutate(
      abundance = tidyr::replace_na(abundance, 0),
      metric = paste0("M2 invertebrate ", phylum, " abundance")
    ) %>%
    dplyr::select(transect, block, metric, abundance)
  
  dplyr::bind_rows(total_abundance, phylum_abundance)
}

# Repeated block abundance -> transect abundance -> metadata join.
prepare_abundance_dataset <- function(
    count_path,
    survey_list,
    metric_name,
    block_calculator = c("total", "inverts")) {
  
  block_calculator <- match.arg(block_calculator)
  count_data <- readr::read_rds(count_path)
  
  if (block_calculator == "total") {
    block_abundance <- calculate_block_total_abundance(
      data = count_data,
      metric_name = metric_name
    )
  } else {
    block_abundance <- calculate_block_invert_abundance(
      data = count_data,
      total_metric_name = metric_name
    )
  }
  
  # Average blocks to obtain one abundance value per transect and metric.
  sample_abundance <- block_abundance %>%
    dplyr::group_by(transect, metric) %>%
    dplyr::summarise(
      mean_abundance = mean(abundance, na.rm = TRUE),
      block_sd = stats::sd(abundance, na.rm = TRUE),
      n_blocks = dplyr::n_distinct(block),
      .groups = "drop"
    ) %>%
    dplyr::rename(abundance = mean_abundance) %>%
    dplyr::left_join(survey_list, by = "transect")
  
  list(samples = sample_abundance, blocks = block_abundance)
}

month_to_date <- function(x) {
  x <- as.character(x)
  as.Date(ifelse(is.na(x), NA_character_, paste0(x, "-01")))
}

# Convert transect-level values into common site, location and region columns.
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
        time_id = as.character(sampling_event),
        time_date = as.Date(sampling_event_start_date),
        metric,
        abundance,
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
        abundance,
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
        abundance,
        period = as.character(period),
        period_split = as.character(period_split)
      )
  ) %>%
    dplyr::filter(!is.na(group_id), group_id != "")
}

# Generic mean, SD, SE and sample-size summary for any grouping columns.
summarise_abundance <- function(data, group_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      n_transects = sum(!is.na(abundance)),
      estimate = ifelse(
        n_transects > 0,
        mean(abundance, na.rm = TRUE),
        NA_real_
      ),
      sd = ifelse(
        n_transects > 1,
        stats::sd(abundance, na.rm = TRUE),
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
  ggplot(data, aes(x = period, y = estimate, fill = period)) +
    geom_col(width = 0.6, colour = "black", alpha = 0.85) +
    geom_errorbar(
      aes(ymin = pmax(estimate - se, 0), ymax = estimate + se),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      ncol = 3,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(values = period_cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL,
      y = "Average abundance\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(legend.position = "none")
}

plot_observed_period_split <- function(data) {
  ggplot(data, aes(x = period_split, y = estimate, fill = period)) +
    geom_col(width = 0.7, colour = "black", alpha = 0.85) +
    geom_errorbar(
      aes(ymin = pmax(estimate - se, 0), ymax = estimate + se),
      width = 0.2,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      ncol = 3,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(values = period_cols, drop = FALSE) +
    scale_x_discrete(
      labels = function(x) stringr::str_replace(x, "^Bloom ", "")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = "Period",
      y = "Average abundance\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

plot_observed_temporal <- function(data) {
  event_dates <- sort(unique(data$time_date))
  
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
  
  ggplot(data, aes(x = time_date, y = estimate, fill = period)) +
    geom_col(
      width = bar_width,
      colour = "black",
      alpha = 0.85
    ) +
    geom_errorbar(
      aes(ymin = pmax(estimate - se, 0), ymax = estimate + se),
      width = errorbar_width,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(metric),
      ncol = 3,
      scales = "free_y",
      drop = FALSE
    ) +
    scale_fill_manual(values = period_cols, drop = FALSE) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.03, 0.03)),
      guide = guide_axis(check.overlap = TRUE)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      x = NULL,
      y = "Average abundance\n(± SE)",
      fill = NULL
    ) +
    theme_minimal(base_size = 15) +
    observed_plot_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom"
    )
}

save_plot_if_present <- function(data, plot_function, filename, width, height) {
  if (nrow(data) == 0) return(FALSE)
  
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

unique(sa_sites$location)
unique(sa_sites$region)

sl_m1 <- readr::read_rds("data/tidy/rls_m1_surveys_final.rds") %>%
  dplyr::select(-c(block, id)) %>%
  dplyr::distinct()

sl_m2_fish <- readr::read_rds("data/tidy/rls_m2_fish_surveys_final.rds") %>%
  dplyr::select(-c(block, id)) %>%
  dplyr::distinct()

sl_m2_inverts <- readr::read_rds("data/tidy/rls_m2_inverts_surveys_final.rds") %>%
  dplyr::select(-c(block, id)) %>%
  dplyr::distinct()

# -----------------------------------------------------------------
# 5. Calculate abundance for the three datasets
# -----------------------------------------------------------------

m1_fish <- prepare_abundance_dataset(
  count_path = "data/tidy/rls_m1_complete_count.rds",
  survey_list = sl_m1,
  metric_name = "M1 fish total abundance",
  block_calculator = "total"
)

m2_fish <- prepare_abundance_dataset(
  count_path = "data/tidy/rls_m2_fish_complete_count.rds",
  survey_list = sl_m2_fish,
  metric_name = "M2 fish total abundance",
  block_calculator = "total"
)

m2_inverts <- prepare_abundance_dataset(
  count_path = "data/tidy/rls_m2_inverts_complete_count.rds",
  survey_list = sl_m2_inverts,
  metric_name = "M2 invertebrate total abundance",
  block_calculator = "inverts"
)

# Totals first, then M2 invertebrate phylumes alphabetically.
invert_phylum_metric_levels <- m2_inverts$samples %>%
  dplyr::filter(metric != "M2 invertebrate total abundance") %>%
  dplyr::distinct(metric) %>%
  dplyr::arrange(metric) %>%
  dplyr::pull(metric)

total_metric_levels <- c(
  "M1 fish total abundance",
  "M2 fish total abundance",
  "M2 invertebrate total abundance"
)

metric_levels <- c(
  total_metric_levels,
  invert_phylum_metric_levels
)

# Check transect counts for total metrics.
m1_fish$samples %>%
  dplyr::filter(metric == "M1 fish total abundance") %>%
  nrow()

m2_fish$samples %>%
  dplyr::filter(metric == "M2 fish total abundance") %>%
  nrow()

m2_inverts$samples %>%
  dplyr::filter(metric == "M2 invertebrate total abundance") %>%
  nrow()

# Check which M2 invertebrate phylum metrics were created.
invert_phylum_metric_levels

# Combine all metrics now, rather than repeating every later operation.
abundance_samples <- dplyr::bind_rows(
  m1_fish$samples,
  m2_fish$samples,
  m2_inverts$samples
) %>%
  dplyr::mutate(
    site_code = as.character(site_code),
    sampling_event_start_date = as.Date(sampling_event_start_date),
    metric = factor(metric, levels = metric_levels)
  ) %>%
  dplyr::select(-dplyr::any_of(c("location", "region"))) %>%
  dplyr::left_join(sa_sites, by = "site_code") %>%
  dplyr::mutate(
    site_name = dplyr::coalesce(site_name, site_name_lookup)
  ) %>%
  dplyr::select(-site_name_lookup)

# Full transect-level dataset, including M2 phylum-specific abundance.
readr::write_rds(
  abundance_samples,
  "data/rls_metrics_for_modelling/abundance.rds"
)

# Total-abundance-only dataset.
abundance_samples %>%
  dplyr::filter(
    as.character(metric) %in% c(
      "M1 fish total abundance",
      "M2 fish total abundance",
      "M2 invertebrate total abundance"
    )
  ) %>%
  readr::write_rds(
    "data/rls_metrics_for_modelling/total_abundance.rds"
  )

# -----------------------------------------------------------------
# 6. Make one common table for site, location and region
# -----------------------------------------------------------------

spatial_samples <- expand_spatial_levels(abundance_samples)

period_split_levels <- c(
  "Pre-bloom",
  spatial_samples$period_split %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)

period_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period)) %>%
  summarise_abundance(
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

period_split_summary <- spatial_samples %>%
  dplyr::filter(!is.na(period_split)) %>%
  summarise_abundance(
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
    period_split = factor(period_split, levels = period_split_levels),
    metric = factor(metric, levels = metric_levels)
  )

temporal_summary <- spatial_samples %>%
  dplyr::filter(!is.na(time_date)) %>%
  summarise_abundance(
    group_vars = c(
      "spatial_level",
      "group_id",
      "group_name",
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
    period_split = factor(period_split, levels = period_split_levels),
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
    period = as.character(period),
    period_split = as.character(period_split),
    sampling_event_start_date = time_date,
    mean = estimate,
    se,
    num_transects = n_transects,
    metric = as.character(metric)
  )

# One combined file keeps all total and phylum-specific metrics.
readr::write_rds(
  site_temporal_output,
  "data/tidy/rls_abundance_average_per_site.rds"
)

# -----------------------------------------------------------------
# 8. Create output directories once
# -----------------------------------------------------------------

purrr::walk(
  unname(plot_output_roots),
  function(root) {
    purrr::walk(
      plot_families,
      function(plot_family) {
        purrr::walk(
          file.path(root, plot_family, plot_types),
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
  }
)

# -----------------------------------------------------------------
# 9. One lookup table for sites, locations and regions
# -----------------------------------------------------------------

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
# 10. Split and save plots for one group
# -----------------------------------------------------------------

# The same summaries are used for both plot families:
#   1. total_abundance = the three overall method totals
#   2. m2_inverts      = M2 invertebrate phylum-specific abundances
#
# Both are faceted in 3 columns. Plot height increases automatically
# when the M2 invertebrate phylum plot needs more than one facet row.
filter_plot_family <- function(data, plot_family) {
  if (plot_family == "total_abundance") {
    data %>%
      dplyr::filter(
        as.character(metric) %in% total_metric_levels
      ) %>%
      dplyr::mutate(
        metric = factor(
          as.character(metric),
          levels = total_metric_levels
        )
      )
  } else if (plot_family == "m2_inverts") {
    data %>%
      dplyr::filter(
        as.character(metric) %in% invert_phylum_metric_levels
      ) %>%
      dplyr::mutate(
        metric = factor(
          as.character(metric),
          levels = invert_phylum_metric_levels
        )
      )
  } else {
    stop("Unknown plot family: ", plot_family)
  }
}

# Height for a 3-column facet layout.
facet_plot_height <- function(data, row_height = 4.5, minimum = 5.5) {
  n_metrics <- dplyr::n_distinct(data$metric)
  
  if (n_metrics == 0) {
    return(minimum)
  }
  
  n_rows <- ceiling(n_metrics / 3)
  max(minimum, n_rows * row_height)
}

save_abundance_plot_family <- function(
    spatial_level_value,
    group_id_value,
    safe_id_value,
    plot_family) {
  
  output_root <- unname(plot_output_roots[[spatial_level_value]])
  
  group_period_data <- period_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    ) %>%
    filter_plot_family(plot_family)
  
  group_period_split_data <- period_split_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    ) %>%
    filter_plot_family(plot_family)
  
  group_temporal_data <- temporal_summary %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_id == group_id_value
    ) %>%
    filter_plot_family(plot_family) %>%
    dplyr::arrange(metric, time_date)
  
  filename_stub <- if (plot_family == "total_abundance") {
    "total_abundance"
  } else {
    "m2_invert_abundance"
  }
  
  saved <- c(
    period = save_plot_if_present(
      data = group_period_data,
      plot_function = plot_observed_period,
      filename = file.path(
        output_root,
        plot_family,
        "period",
        paste0(
          safe_id_value,
          "_",
          filename_stub,
          "_period.png"
        )
      ),
      width = 15,
      height = facet_plot_height(group_period_data)
    ),
    
    period_split = save_plot_if_present(
      data = group_period_split_data,
      plot_function = plot_observed_period_split,
      filename = file.path(
        output_root,
        plot_family,
        "period_split",
        paste0(
          safe_id_value,
          "_",
          filename_stub,
          "_period_split.png"
        )
      ),
      width = 17,
      height = facet_plot_height(group_period_split_data)
    ),
    
    temporal = save_plot_if_present(
      data = group_temporal_data,
      plot_function = plot_observed_temporal,
      filename = file.path(
        output_root,
        plot_family,
        "temporal",
        paste0(
          safe_id_value,
          "_",
          filename_stub,
          "_temporal.png"
        )
      ),
      width = 15,
      height = facet_plot_height(
        group_temporal_data,
        row_height = 5.5,
        minimum = 6
      )
    )
  )
  
  names(saved)[saved]
}

# Save both plot families for one site/location/region.
save_abundance_plots <- function(
    spatial_level_value,
    group_id_value,
    safe_id_value) {
  
  saved_total <- save_abundance_plot_family(
    spatial_level_value = spatial_level_value,
    group_id_value = group_id_value,
    safe_id_value = safe_id_value,
    plot_family = "total_abundance"
  )
  
  saved_inverts <- save_abundance_plot_family(
    spatial_level_value = spatial_level_value,
    group_id_value = group_id_value,
    safe_id_value = safe_id_value,
    plot_family = "m2_inverts"
  )
  
  saved <- c(
    paste0("total_abundance/", saved_total),
    paste0("m2_inverts/", saved_inverts)
  )
  
  # A group can legitimately have no M2 invertebrate data, but every group
  # reaching this loop should normally have at least one abundance metric.
  if (length(saved) == 0) {
    stop("No plot data were available for this group.")
  }
  
  saved
}

# -----------------------------------------------------------------
# 11. Run all sites, locations and regions with one loop
# -----------------------------------------------------------------

plot_log <- purrr::pmap_dfr(
  group_lookup,
  function(spatial_level, group_id, group_name, file_stub, safe_id) {
    message("Creating plots for ", spatial_level, ": ", group_name)
    
    tryCatch(
      {
        plots_saved <- save_abundance_plots(
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