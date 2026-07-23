# ============================================================
# Top occurrence and abundance plots by reporting location
# ============================================================
#
# What this script makes:
#   Four plots for each reporting location:
#     1. Occurrence, Pre-bloom focus
#     2. Occurrence, Post-bloom/Bloom focus
#     3. Abundance, Pre-bloom focus
#     4. Abundance, Post-bloom/Bloom focus
#
# Focus-period filtering logic:
#   - A Pre-bloom focus plot selects species using Pre-bloom values only.
#   - A Post-bloom focus plot selects species using all Bloom/Post-bloom
#     deployments combined.
#   - Once species are selected, the plotted bars still show Pre-bloom and
#     each Bloom time point separately.
#
# Example:
#   For Glenelg Pre-bloom occurrence, the script keeps species that are
#   present in at least 15% of Glenelg Pre-bloom deployments. It does not
#   remove those species just because they are below 15% during Bloom.
#
# ============================================================


# ---- Packages ----
# Install any missing packages before running this script.
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(CheckEM)
library(ggtext)


# ---- User settings ----
# Change these settings here rather than editing the plotting functions below.

# Input files used in your existing workflow.
hab_data_file <- "app_data/hab_data.Rdata"
raw_count_file <- "raw_count.RDS"
functional_traits_file <- "data/lookups/SA-HAB-Functional Traits.csv"

# Output folder.
output_root <- "plots/20260723/top_occurrence_abundance_by_location_focus_period"

# Thresholds requested.
# Occurrence uses >= because the requirement is "at least 15%".
occurrence_threshold_percent <- 15

# Abundance uses > because the requirement was ">0.5 average abundance per drop".
abundance_threshold_per_drop <- 0.5

# If you want every species above the threshold, leave this as Inf.
# If you only want the top 10 species above the threshold, change to 10.
max_species_per_plot <- Inf

# Plot style options.
# show_error_bars adds standard errors.
# show_value_labels adds the numeric values at the end of bars.
# include_location_in_title is FALSE because you said you do not want plot titles.
show_error_bars <- TRUE
show_value_labels <- FALSE
include_location_in_title <- FALSE

# Colours.
# Pre-bloom is dark blue. Bloom periods are pink shades.
# pre_bloom_colour <- "#193b73"
# bloom_colour_start <- "#92bd83"
# bloom_colour_end <- "#8b95d9"
# 
# # "#e3c06d" 3rd colour
pre_bloom_colour <- "#193b73"

bloom_colour_start  <- "#92bd83"
bloom_colour_middle <- "#8b95d9"
bloom_colour_end    <- "#e3c06d"

# Output image sizing.
plot_width <- 9
minimum_plot_height <- 5
height_per_species <- 0.45
plot_dpi <- 300


# ---- Plot theme ----
# This is based on your existing plot theme.
# ggtext::element_markdown() lets the species names use italics.
plot_theme <- theme_bw(base_size = 16) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.text.x = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )


# ---- Helper functions ----

# Make reporting names safe to use as filenames.
safe_filename <- function(x) {
  x %>%
    gsub("[^A-Za-z0-9]+", "_", .) %>%
    gsub("^_+|_+$", "", .)
}

# Make colours for the periods in one plot.
# Pre-bloom always gets the dark blue.
# Any Bloom/Post-bloom periods get a gradient from pink to light pink.
# make_period_colours <- function(plot_periods) {
#   
#   bloom_periods <- setdiff(plot_periods, "Pre-bloom")
#   
#   if (length(bloom_periods) > 0) {
#     bloom_cols <- grDevices::colorRampPalette(
#       c(bloom_colour_start, bloom_colour_end)
#     )(length(bloom_periods))
#   } else {
#     bloom_cols <- character(0)
#   }
#   
#   c(
#     "Pre-bloom" = pre_bloom_colour,
#     setNames(bloom_cols, bloom_periods)
#   )
# }
make_period_colours <- function(plot_periods) {
  
  bloom_periods <- setdiff(plot_periods, "Pre-bloom")
  
  if (length(bloom_periods) > 0) {
    bloom_cols <- grDevices::colorRampPalette(
      c(
        bloom_colour_start,
        bloom_colour_middle,
        bloom_colour_end
      )
    )(length(bloom_periods))
  } else {
    bloom_cols <- character(0)
  }
  
  c(
    "Pre-bloom" = pre_bloom_colour,
    setNames(bloom_cols, bloom_periods)
  )
}

# ---- Read in data ----

# hab_data is loaded into the environment by this file.
load(hab_data_file)

# Raw counts.
# The script creates a scientific name field to match your existing workflow.
combined_count <- readRDS(raw_count_file) %>%
  mutate(scientific = paste(family, genus, species)) %>%
  distinct()

# Functional traits/common names lookup.
dew_species <- read_csv(functional_traits_file, show_col_types = FALSE)

# CheckEM species lookup for Australian common names.
species_list <- CheckEM::australia_life_history

# BRUVS metadata only.
combined_metadata <- hab_data$hab_combined_metadata %>%
  sf::st_drop_geometry() %>%
  filter(method %in% "BRUVs") %>%
  ungroup()

# One row per BRUVS deployment with the key metadata needed for grouping.
deployments <- combined_metadata %>%
  distinct(
    campaignid,
    sample,
    start_month,
    reporting_name,
    period
  ) %>%
  filter(!is.na(reporting_name)) %>%
  mutate(
    # Keep the original period for the focus-period grouping.
    period_original = as.character(period),
    
    # Keep separate Bloom time points for plotting.
    # For example, Bloom becomes Bloom 2025-05 if start_month is 2025-05.
    period = case_when(
      period_original %in% "Bloom" ~ paste(period_original, start_month),
      TRUE ~ period_original
    ),
    
    # Focus-period groups used for species selection.
    # Pre-bloom is kept by itself.
    # Everything else is grouped together as Post-bloom for selection only.
    period_group = case_when(
      period_original %in% "Pre-bloom" ~ "Pre-bloom",
      TRUE ~ "Post-bloom"
    ),
    
    # A unique deployment ID makes deployment counts clear and robust.
    deployment_id = paste(campaignid, sample, sep = "__")
  )

# Keep only counts from BRUVS deployments.
# If a species appears more than once in a deployment, keep the maximum count.
# This is usually the safest behaviour for BRUVS MaxN-style count data.
counts_bruv <- combined_count %>%
  semi_join(
    deployments %>% distinct(campaignid, sample),
    by = c("campaignid", "sample")
  ) %>%
  filter(!is.na(family), !is.na(genus), !is.na(species)) %>%
  filter(scientific != "NA NA NA") %>%
  group_by(campaignid, sample, scientific, family, genus, species) %>%
  summarise(
    count = if (all(is.na(count))) NA_real_ else max(count, na.rm = TRUE),
    .groups = "drop"
  )

# Species list observed in BRUVS data.
species_lookup <- counts_bruv %>%
  distinct(scientific, family, genus, species)

# Make a complete deployment-by-species table.
# This adds zeroes for species not seen in a deployment.
# This step is essential for correct occurrence percentages and averages.
complete_counts <- tidyr::crossing(
  deployments,
  species_lookup
) %>%
  left_join(
    counts_bruv,
    by = c(
      "campaignid",
      "sample",
      "scientific",
      "family",
      "genus",
      "species"
    )
  ) %>%
  mutate(
    count = tidyr::replace_na(count, 0)
  )


# ---- Add names for plotting ----

# The display name is the italic scientific name plus the common name.
# This follows your original approach, using the DEW common name first and
# falling back to the CheckEM Australian common name if needed.
complete_counts <- complete_counts %>%
  mutate(
    genus = if_else(genus %in% "Unknown", family, genus),
    genus_species = paste(genus, species)
  ) %>%
  left_join(
    dew_species %>% select(genus_species, common_name),
    by = "genus_species"
  ) %>%
  left_join(
    species_list,
    by = c("family", "genus", "species")
  ) %>%
  mutate(
    common_name = coalesce(common_name, australian_common_name, scientific),
    display_name = paste0(
      "<i>", genus, " ", species, "</i><br>",
      "(", common_name, ")"
    )
  )


# ---- Summaries for plotting and filtering ----

# Summary by the separate plotted periods.
# These are the values shown as bars in the final plots.
# Example periods: Pre-bloom, Bloom 2025-05, Bloom 2025-11.
period_summary <- complete_counts %>%
  group_by(
    reporting_name,
    period,
    scientific,
    family,
    genus,
    species,
    common_name,
    display_name
  ) %>%
  summarise(
    n_deployments = n_distinct(deployment_id),
    n_deployments_present = sum(count > 0, na.rm = TRUE),
    
    occurrence_percent = 100 * n_deployments_present / n_deployments,
    
    # Standard error for a percentage based on a binomial proportion.
    occurrence_se = {
      p <- n_deployments_present / n_deployments
      100 * sqrt(p * (1 - p) / n_deployments)
    },
    
    average_abundance = mean(count, na.rm = TRUE),
    abundance_se = sd(count, na.rm = TRUE) / sqrt(n_deployments),
    
    .groups = "drop"
  )

# Summary by the focus-period groups used only for species selection.
# Pre-bloom remains separate.
# All Bloom/Post-bloom deployments are combined.
selection_summary <- complete_counts %>%
  group_by(
    reporting_name,
    period_group,
    scientific,
    family,
    genus,
    species,
    common_name,
    display_name
  ) %>%
  summarise(
    n_deployments = n_distinct(deployment_id),
    n_deployments_present = sum(count > 0, na.rm = TRUE),
    
    occurrence_percent = 100 * n_deployments_present / n_deployments,
    
    occurrence_se = {
      p <- n_deployments_present / n_deployments
      100 * sqrt(p * (1 - p) / n_deployments)
    },
    
    average_abundance = mean(count, na.rm = TRUE),
    abundance_se = sd(count, na.rm = TRUE) / sqrt(n_deployments),
    
    .groups = "drop"
  )

# Save summary CSVs so you can check exactly which values were used.
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

write_csv(
  period_summary,
  file.path(output_root, "summary_by_separate_period.csv")
)

write_csv(
  selection_summary,
  file.path(output_root, "summary_used_for_focus_period_filters.csv")
)


# ---- Period order ----
# Always show Pre-bloom first, then the Bloom/Post-bloom periods after it.
period_levels <- c(
  "Pre-bloom",
  sort(unique(period_summary$period[period_summary$period != "Pre-bloom"]))
)

period_summary <- period_summary %>%
  mutate(period = factor(period, levels = period_levels))


# ---- Species selection helper ----

# This function chooses which species to include for one plot.
#
# It uses focus-period filtering:
#   - focus_group = "Pre-bloom" uses Pre-bloom values only.
#   - focus_group = "Post-bloom" uses combined Bloom/Post-bloom values only.
#
# It does not require a species to pass the threshold in both periods.
get_species_to_plot <- function(location_name, metric, focus_group) {
  
  if (!metric %in% c("occurrence", "abundance")) {
    stop("metric must be 'occurrence' or 'abundance'.")
  }
  
  if (!focus_group %in% c("Pre-bloom", "Post-bloom")) {
    stop("focus_group must be 'Pre-bloom' or 'Post-bloom'.")
  }
  
  if (metric == "occurrence") {
    
    # Occurrence plots:
    # Keep species that occur in at least 15% of deployments in the focus
    # period only.
    # species_to_plot <- selection_summary %>%
    #   filter(
    #     reporting_name == location_name,
    #     period_group == focus_group,
    #     occurrence_percent >= occurrence_threshold_percent
    #   ) %>%
    #   mutate(selection_value = occurrence_percent)
    
    species_to_plot <- selection_summary %>%
      filter(
        reporting_name == location_name,
        period_group == focus_group,
        occurrence_percent >= occurrence_threshold_percent,
        average_abundance > abundance_threshold_per_drop
      ) %>%
      mutate(selection_value = occurrence_percent)
    
  } else {
    
    # Abundance plots:
    # Keep species that are present in at least 15% of deployments in the
    # focus period AND have average abundance >0.5 in that same focus period.
    #
    # If you later decide abundance plots should ignore occurrence and only
    # use average_abundance >0.5, remove the occurrence_percent line below.
    species_to_plot <- selection_summary %>%
      filter(
        reporting_name == location_name,
        period_group == focus_group,
        occurrence_percent >= occurrence_threshold_percent,
        average_abundance > abundance_threshold_per_drop
      ) %>%
      mutate(selection_value = average_abundance)
  }
  
  # Largest focus-period values first.
  species_to_plot <- species_to_plot %>%
    arrange(desc(selection_value))
  
  # Optional: keep only the top N species above the threshold.
  if (is.finite(max_species_per_plot)) {
    species_to_plot <- species_to_plot %>%
      slice_head(n = max_species_per_plot)
  }
  
  species_to_plot
}


# ---- Plotting function ----

make_one_plot <- function(location_name, metric, focus_group) {
  
  # Choose the metric-specific columns and labels.
  if (metric == "occurrence") {
    
    value_col <- "occurrence_percent"
    se_col <- "occurrence_se"
    y_axis_label <- "Occurrence (% of BRUVS deployments)"
    metric_folder <- "occurrence"
    label_digits <- 1
    
  } else if (metric == "abundance") {
    
    value_col <- "average_abundance"
    se_col <- "abundance_se"
    y_axis_label <- "Average abundance per BRUVS deployment"
    metric_folder <- "abundance"
    label_digits <- 2
    
  } else {
    stop("metric must be 'occurrence' or 'abundance'.")
  }
  
  # Folder and filename labels for the focus period.
  if (focus_group == "Pre-bloom") {
    focus_folder <- "pre_bloom_focus"
  } else if (focus_group == "Post-bloom") {
    focus_folder <- "post_bloom_focus"
  } else {
    stop("focus_group must be 'Pre-bloom' or 'Post-bloom'.")
  }
  
  # Species selected using the focus-period threshold.
  species_to_plot <- get_species_to_plot(
    location_name = location_name,
    metric = metric,
    focus_group = focus_group
  )
  
  if (nrow(species_to_plot) == 0) {
    message(
      "Skipping ", location_name, " - ", metric_folder, " - ", focus_folder,
      ": no species passed the focus-period filter."
    )
    return(invisible(NULL))
  }
  
  # Order species by the focus-period value.
  # With coord_flip(), the last factor level appears at the top, so arranging
  # from smallest to largest puts the largest species at the top of the plot.
  species_order <- species_to_plot %>%
    arrange(selection_value) %>%
    pull(display_name)
  
  # Pull the plotted values from period_summary so Bloom time points stay
  # separated in the bars.
  plot_dat <- period_summary %>%
    semi_join(
      species_to_plot %>% select(reporting_name, scientific),
      by = c("reporting_name", "scientific")
    ) %>%
    filter(reporting_name == location_name) %>%
    mutate(
      display_name = factor(display_name, levels = species_order),
      period = factor(as.character(period), levels = period_levels),
      value = .data[[value_col]],
      se = .data[[se_col]],
      ymin = pmax(0, value - se),
      ymax = value + se,
      value_label = if_else(
        value > 0,
        if (label_digits == 1) sprintf("%.1f", value) else sprintf("%.2f", value),
        ""
      )
    )
  
  # Occurrence cannot be less than 0% or greater than 100%.
  if (metric == "occurrence") {
    plot_dat <- plot_dat %>%
      mutate(ymax = pmin(100, ymax))
  }
  
  # Keep only periods that exist for this location/plot.
  plot_periods <- period_levels[period_levels %in% as.character(plot_dat$period)]
  plot_fill_cols <- make_period_colours(plot_periods)
  
  # Dodge bars so each period appears beside the others for the same species.
  pd <- position_dodge(width = 0.8, reverse = TRUE)
  
  # Start the plot.
  p <- ggplot(
    plot_dat,
    aes(
      x = display_name,
      y = value,
      fill = period
    )
  ) +
    geom_col(
      position = pd,
      width = 0.7
    )
  
  # Add error bars if requested.
  if (isTRUE(show_error_bars)) {
    p <- p +
      geom_errorbar(
        aes(
          ymin = ymin,
          ymax = ymax
        ),
        position = pd,
        width = 0.2
      )
  }
  
  # Add numeric labels if requested.
  if (isTRUE(show_value_labels)) {
    p <- p +
      geom_text(
        aes(label = value_label),
        position = pd,
        hjust = -0.1,
        size = 3,
        show.legend = FALSE
      )
  }
  
  # Finish the plot.
  p <- p +
    coord_flip(clip = "off") +
    scale_fill_manual(
      values = plot_fill_cols,
      breaks = plot_periods,
      drop = TRUE
    ) +
    labs(
      title = if (isTRUE(include_location_in_title)) location_name else NULL,
      subtitle = NULL,
      x = NULL,
      y = y_axis_label,
      fill = NULL
    ) +
    plot_theme +
    theme(
      plot.margin = margin(t = 5.5, r = 25, b = 5.5, l = 5.5)
    )
  
  # Axis scaling.
  if (metric == "occurrence") {
    p <- p +
      scale_y_continuous(
        limits = c(0, 100),
        expand = expansion(mult = c(0, 0.05))
      )
  } else {
    p <- p +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.12))
      )
  }
  
  # Save the plot.
  output_dir <- file.path(output_root, metric_folder, focus_folder)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  output_file <- file.path(
    output_dir,
    paste0(
      safe_filename(location_name),
      "_",
      metric_folder,
      "_",
      focus_folder,
      ".png"
    )
  )
  
  # Increase height when there are lots of species.
  plot_height <- max(
    minimum_plot_height,
    length(species_order) * height_per_species
  )
  
  print(p)
  
  ggsave(
    filename = output_file,
    plot = p,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi
  )
  
  message("Saved: ", output_file)
  
  invisible(output_file)
}


# ---- Make all plots ----

# Unique reporting locations.
reporting_names <- period_summary %>%
  distinct(reporting_name) %>%
  arrange(reporting_name) %>%
  pull(reporting_name)

# Make four plots for every reporting location.
for (location_name in reporting_names) {
  
  # 1. Occurrence plot, Pre-bloom focus:
  #    Species selected using Pre-bloom occurrence only.
  make_one_plot(
    location_name = location_name,
    metric = "occurrence",
    focus_group = "Pre-bloom"
  )
  
  # 2. Occurrence plot, Post-bloom focus:
  #    Species selected using combined Bloom/Post-bloom occurrence only.
  make_one_plot(
    location_name = location_name,
    metric = "occurrence",
    focus_group = "Post-bloom"
  )
  
  # 3. Abundance plot, Pre-bloom focus:
  #    Species selected using Pre-bloom occurrence >=15% and
  #    Pre-bloom average abundance >0.5 only.
  make_one_plot(
    location_name = location_name,
    metric = "abundance",
    focus_group = "Pre-bloom"
  )
  
  # 4. Abundance plot, Post-bloom focus:
  #    Species selected using combined Bloom/Post-bloom occurrence >=15% and
  #    combined Bloom/Post-bloom average abundance >0.5 only.
  make_one_plot(
    location_name = location_name,
    metric = "abundance",
    focus_group = "Post-bloom"
  )
}

message("Finished making plots.")