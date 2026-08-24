# ============================================================
# Top taxon occurrence and abundance plots from RLS data
# ============================================================
#
# Datasets:
#   1. M1 fish
#   2. M2 fish
#   3. M2 invertebrates
#
# For every requested site, location and region, this script makes:
#   1. Occurrence, Pre-bloom focus
#   2. Occurrence, Post-bloom focus
#   3. Abundance, Pre-bloom focus
#   4. Abundance, Post-bloom focus
#
# Focus-period selection logic:
#   - Pre-bloom focus selects taxa using Pre-bloom transects only.
#   - Post-bloom focus selects taxa using all Bloom transects combined.
#   - Once taxa are selected, bars show Pre-bloom and each separate
#     Bloom period from `period_split`.
#
# RLS sampling unit:
#   - Counts are first summed for each taxon within each block.
#   - Taxon abundance for a transect is the mean across all surveyed
#     blocks in that transect, including zero-abundance blocks.
#   - Occurrence is the percentage of transects where abundance > 0.
#
# The code uses "taxon" rather than "species" because an unresolved
# Genus spp record is retained when no identified species from that
# genus occurs in the same block.
# ============================================================


# ---- Packages ----
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggtext)
library(sf)


# ============================================================
# 1. User settings
# ============================================================

# Input files.
dataset_settings <- tibble::tribble(
  ~dataset_id,   ~dataset_label,       ~count_path,                                      ~survey_path,
  "m1_fish",     "M1 fish",           "data/tidy/rls_m1_complete_count.rds",             "data/tidy/rls_m1_surveys_final.rds",
  "m2_fish",     "M2 fish",           "data/tidy/rls_m2_fish_complete_count.rds",        "data/tidy/rls_m2_fish_surveys_final.rds",
  "m2_inverts",  "M2 invertebrates",  "data/tidy/rls_m2_inverts_complete_count.rds",     "data/tidy/rls_m2_inverts_surveys_final.rds"
)

site_lookup_file <- "dev/Dive_sites_2026_07_14.shp"

# Output folder.
output_root <- file.path(
  "plots",
  "rls_top_occurrence_abundance"
)

# Create plots for any combination of these three levels.
spatial_levels_to_plot <- c(
  # "site",
  "location"#,
  # "region"
)

# Selection thresholds.
occurrence_threshold_percent <- 0#15
abundance_threshold_per_transect <- 0#0.5

# The active code in the attached BRUV script required both occurrence
# >= 15% and abundance > 0.5 for the occurrence plots. Keep TRUE to
# reproduce that behaviour. Change to FALSE for occurrence-only selection.
occurrence_also_requires_abundance <- FALSE

# Leave as Inf to plot every taxon passing the thresholds.
# Change to 10, for example, to keep only the top 10.
max_taxa_per_plot <- 5

# When TRUE, remove a Genus spp record if an identified species from the
# same genus occurs in the same block. This matches the taxonomic handling
# used in the species-richness workflow.
remove_spp_when_identified_present <- TRUE

# Plot style.
show_error_bars <- TRUE
show_value_labels <- FALSE
include_group_in_title <- FALSE
include_dataset_in_title <- FALSE

pre_bloom_colour <- "#193b73"
bloom_colour_start <- "#92bd83"
bloom_colour_middle <- "#8b95d9"
bloom_colour_end <- "#e3c06d"

plot_width <- 10
minimum_plot_height <- 5
height_per_taxon <- 0.45
plot_dpi <- 300


# ============================================================
# 2. Plot theme and small helpers
# ============================================================

plot_theme <- theme_bw(base_size = 16) +
  theme(
    axis.line.x = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    axis.line.y = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    axis.text.x = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "plain")
  )


safe_filename <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}


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
    stats::setNames(bloom_cols, bloom_periods)
  )
}


# Add a column when it is absent so the later code can be shared by
# fish and invertebrate datasets.
add_missing_character_column <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    data[[column_name]] <- NA_character_
  }

  data
}


first_non_missing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & stringr::str_trim(x) != ""]

  if (length(x) == 0) {
    NA_character_
  } else {
    x[[1]]
  }
}


# ============================================================
# 3. Read site, location and region lookup
# ============================================================

sa_sites <- sf::read_sf(site_lookup_file) %>%
  CheckEM::clean_names() %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    site_code = as.character(site_code),
    site_name_lookup = as.character(site_name),
    location = as.character(location_g),
    region = as.character(bruvsrepor)
  ) %>%
  dplyr::distinct(site_code, .keep_all = TRUE)


# Use CheckEM common names where available. Unmatched invertebrate taxa
# retain their scientific/taxon name only.
species_common_lookup <- CheckEM::australia_life_history %>%
  dplyr::transmute(
    family = as.character(family),
    genus = as.character(genus),
    species = as.character(species),
    australian_common_name = as.character(australian_common_name)
  ) %>%
  dplyr::distinct(
    family,
    genus,
    species,
    .keep_all = TRUE
  )


# ============================================================
# 4. Prepare one RLS dataset
# ============================================================

prepare_rls_dataset <- function(
    dataset_id,
    dataset_label,
    count_path,
    survey_path) {

  message("Preparing ", dataset_label)

  counts <- readr::read_rds(count_path)
  surveys_raw <- readr::read_rds(survey_path)

  required_count_columns <- c(
    "transect",
    "block",
    "family",
    "genus",
    "species",
    "total"
  )

  missing_count_columns <- setdiff(
    required_count_columns,
    names(counts)
  )

  if (length(missing_count_columns) > 0) {
    stop(
      dataset_label,
      " count data are missing: ",
      paste(missing_count_columns, collapse = ", ")
    )
  }

  required_survey_columns <- c(
    "transect",
    "block",
    "site_code",
    "site_name",
    "period",
    "period_split"
  )

  missing_survey_columns <- setdiff(
    required_survey_columns,
    names(surveys_raw)
  )

  if (length(missing_survey_columns) > 0) {
    stop(
      dataset_label,
      " survey list is missing: ",
      paste(missing_survey_columns, collapse = ", ")
    )
  }

  counts <- counts %>%
    add_missing_character_column("scientific") %>%
    add_missing_character_column("class") %>%
    add_missing_character_column("common_name") %>%
    dplyr::mutate(
      family = as.character(family),
      genus = as.character(genus),
      species = as.character(species),
      scientific = as.character(scientific),
      class = as.character(class),
      common_name = as.character(common_name),
      total = as.numeric(total)
    )

  # Match the species-richness handling of unresolved spp records.
  if (isTRUE(remove_spp_when_identified_present)) {
    counts <- counts %>%
      dplyr::group_by(
        transect,
        block,
        family,
        genus
      ) %>%
      dplyr::mutate(
        identified_species_present = any(
          !is.na(species) &
            species != "spp" &
            total > 0,
          na.rm = TRUE
        )
      ) %>%
      dplyr::filter(
        !dplyr::coalesce(
          species == "spp" &
            total > 0 &
            identified_species_present,
          FALSE
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-identified_species_present)
  }

  # Construct one stable taxon ID and one display label.
  counts <- counts %>%
    dplyr::mutate(
      genus_for_label = dplyr::case_when(
        !is.na(genus) &
          stringr::str_trim(genus) != "" &
          genus != "Unknown" ~ genus,
        !is.na(family) &
          stringr::str_trim(family) != "" ~ family,
        TRUE ~ "Unresolved"
      ),
      species_for_label = dplyr::case_when(
        !is.na(species) &
          stringr::str_trim(species) != "" ~ species,
        TRUE ~ "spp"
      ),
      scientific_clean = dplyr::case_when(
        !is.na(scientific) &
          stringr::str_trim(scientific) != "" &
          scientific != "NA" ~ scientific,
        TRUE ~ paste(genus_for_label, species_for_label)
      ),
      taxon_id = paste(
        family,
        genus_for_label,
        species_for_label,
        sep = "__"
      ),
      display_name = dplyr::if_else(
        !is.na(common_name) &
          stringr::str_trim(common_name) != "",
        paste0(
          "<i>",
          genus_for_label,
          " ",
          species_for_label,
          "</i><br>(",
          common_name,
          ")"
        ),
        paste0(
          "<i>",
          genus_for_label,
          " ",
          species_for_label,
          "</i>"
        )
      )
    )

  # Taxa with no positive count anywhere are not useful plot candidates.
  # Summarising to one row per taxon_id prevents optional naming fields from
  # accidentally duplicating the same taxon in the summary tables.
  taxon_lookup <- counts %>%
    dplyr::group_by(taxon_id) %>%
    dplyr::summarise(
      dataset_total = sum(total, na.rm = TRUE),
      scientific = first_non_missing(scientific_clean),
      class = first_non_missing(class),
      family = first_non_missing(family),
      genus = first_non_missing(genus_for_label),
      species = first_non_missing(species_for_label),
      raw_common_name = first_non_missing(common_name),
      .groups = "drop"
    ) %>%
    dplyr::filter(dataset_total > 0) %>%
    dplyr::select(-dataset_total) %>%
    dplyr::left_join(
      species_common_lookup,
      by = c("family", "genus", "species")
    ) %>%
    dplyr::mutate(
      common_name = dplyr::coalesce(
        raw_common_name,
        australian_common_name
      ),
      display_name = dplyr::if_else(
        !is.na(common_name) &
          stringr::str_trim(common_name) != "",
        paste0(
          "<i>",
          genus,
          " ",
          species,
          "</i><br>(",
          common_name,
          ")"
        ),
        paste0(
          "<i>",
          genus,
          " ",
          species,
          "</i>"
        )
      )
    ) %>%
    dplyr::select(
      taxon_id,
      scientific,
      class,
      family,
      genus,
      species,
      common_name,
      display_name
    )

  # Use the survey list to establish every surveyed block. This means
  # zero-abundance blocks are included in each transect mean.
  survey_block_keys <- surveys_raw %>%
    dplyr::filter(
      !is.na(transect),
      !is.na(block)
    ) %>%
    dplyr::distinct(transect, block)

  block_counts <- survey_block_keys %>%
    dplyr::count(
      transect,
      name = "n_blocks"
    )

  if (nrow(block_counts) == 0) {
    stop(
      "No transect x block combinations were found in ",
      dataset_label,
      " survey list."
    )
  }

  # Sum duplicate rows and all surveyed blocks for each taxon, then divide
  # by the number of surveyed blocks. A taxon missing from one or more blocks
  # therefore contributes zero to that transect's block mean.
  transect_taxa <- counts %>%
    dplyr::semi_join(
      survey_block_keys,
      by = c("transect", "block")
    ) %>%
    dplyr::filter(taxon_id %in% taxon_lookup$taxon_id) %>%
    dplyr::group_by(
      transect,
      taxon_id
    ) %>%
    dplyr::summarise(
      summed_count = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      block_counts,
      by = "transect"
    ) %>%
    dplyr::mutate(
      abundance = summed_count / n_blocks
    ) %>%
    dplyr::filter(
      !is.na(abundance),
      abundance > 0
    ) %>%
    dplyr::select(
      transect,
      taxon_id,
      abundance
    )

  # One metadata row per transect.
  sample_metadata <- surveys_raw %>%
    dplyr::select(-dplyr::any_of(c("block", "id"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      site_code = as.character(site_code),
      dataset_id = .env$dataset_id,
      dataset_label = .env$dataset_label
    ) %>%
    dplyr::select(
      -dplyr::any_of(c("location", "region"))
    ) %>%
    dplyr::left_join(
      sa_sites,
      by = "site_code"
    ) %>%
    dplyr::mutate(
      site_name = dplyr::coalesce(
        as.character(site_name),
        site_name_lookup,
        site_code
      ),
      period = as.character(period),
      period_split = as.character(period_split),
      focus_group = dplyr::case_when(
        period == "Pre-bloom" ~ "Pre-bloom",
        !is.na(period) ~ "Post-bloom",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-site_name_lookup)

  duplicate_transects <- sample_metadata %>%
    dplyr::count(transect) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicate_transects) > 0) {
    stop(
      dataset_label,
      " survey metadata has more than one row for ",
      nrow(duplicate_transects),
      " transects after block and id are removed."
    )
  }

  list(
    sample_metadata = sample_metadata,
    transect_taxa = transect_taxa,
    taxon_lookup = taxon_lookup
  )
}


# ============================================================
# 5. Expand sample metadata to site, location and region
# ============================================================

expand_spatial_levels <- function(sample_metadata) {
  dplyr::bind_rows(
    sample_metadata %>%
      dplyr::transmute(
        dataset_id,
        dataset_label,
        transect,
        spatial_level = "site",
        group_id = as.character(site_code),
        group_name = dplyr::coalesce(
          as.character(site_name),
          as.character(site_code)
        ),
        plot_period = dplyr::coalesce(period_split, period),
        focus_group
      ),
    sample_metadata %>%
      dplyr::transmute(
        dataset_id,
        dataset_label,
        transect,
        spatial_level = "location",
        group_id = as.character(location),
        group_name = as.character(location),
        plot_period = dplyr::coalesce(period_split, period),
        focus_group
      ),
    sample_metadata %>%
      dplyr::transmute(
        dataset_id,
        dataset_label,
        transect,
        spatial_level = "region",
        group_id = as.character(region),
        group_name = as.character(region),
        plot_period = dplyr::coalesce(period_split, period),
        focus_group
      )
  ) %>%
    dplyr::filter(
      spatial_level %in% spatial_levels_to_plot,
      !is.na(group_id),
      group_id != ""
    )
}


# ============================================================
# 6. Summarise occurrence and abundance including zeroes
# ============================================================

# Convert n, sum(x), and sum(x^2) into a mean and standard error while
# treating all unobserved transect x taxon combinations as zero.
add_summary_statistics <- function(data) {
  data %>%
    dplyr::mutate(
      n_transects_present = tidyr::replace_na(
        n_transects_present,
        0L
      ),
      sum_abundance = tidyr::replace_na(
        sum_abundance,
        0
      ),
      sum_squared_abundance = tidyr::replace_na(
        sum_squared_abundance,
        0
      ),
      occurrence_percent = dplyr::if_else(
        n_transects > 0,
        100 * n_transects_present / n_transects,
        NA_real_
      ),
      occurrence_se = dplyr::if_else(
        n_transects > 0,
        {
          p <- n_transects_present / n_transects
          100 * sqrt(p * (1 - p) / n_transects)
        },
        NA_real_
      ),
      average_abundance = dplyr::if_else(
        n_transects > 0,
        sum_abundance / n_transects,
        NA_real_
      ),
      abundance_variance = dplyr::if_else(
        n_transects > 1,
        pmax(
          (
            sum_squared_abundance -
              n_transects * average_abundance^2
          ) /
            (n_transects - 1),
          0
        ),
        NA_real_
      ),
      abundance_se = sqrt(abundance_variance) /
        sqrt(n_transects)
    ) %>%
    dplyr::select(-abundance_variance)
}


summarise_one_dataset <- function(prepared_data) {
  sample_groups <- expand_spatial_levels(
    prepared_data$sample_metadata
  )

  taxon_lookup <- prepared_data$taxon_lookup

  positive_values <- sample_groups %>%
    dplyr::inner_join(
      prepared_data$transect_taxa,
      by = "transect"
    )

  # Values displayed in the plots: Pre-bloom plus each separate Bloom
  # period from period_split.
  period_denominators <- sample_groups %>%
    dplyr::filter(!is.na(plot_period)) %>%
    dplyr::group_by(
      dataset_id,
      dataset_label,
      spatial_level,
      group_id,
      group_name,
      plot_period
    ) %>%
    dplyr::summarise(
      n_transects = dplyr::n_distinct(transect),
      .groups = "drop"
    )

  period_positive <- positive_values %>%
    dplyr::filter(!is.na(plot_period)) %>%
    dplyr::group_by(
      dataset_id,
      dataset_label,
      spatial_level,
      group_id,
      group_name,
      plot_period,
      taxon_id
    ) %>%
    dplyr::summarise(
      n_transects_present = dplyr::n_distinct(transect),
      sum_abundance = sum(abundance, na.rm = TRUE),
      sum_squared_abundance = sum(abundance^2, na.rm = TRUE),
      .groups = "drop"
    )

  period_summary <- tidyr::crossing(
    period_denominators,
    taxon_lookup
  ) %>%
    dplyr::left_join(
      period_positive,
      by = c(
        "dataset_id",
        "dataset_label",
        "spatial_level",
        "group_id",
        "group_name",
        "plot_period",
        "taxon_id"
      )
    ) %>%
    add_summary_statistics()

  # Values used only to select taxa for a Pre-bloom or Post-bloom focus.
  focus_denominators <- sample_groups %>%
    dplyr::filter(!is.na(focus_group)) %>%
    dplyr::group_by(
      dataset_id,
      dataset_label,
      spatial_level,
      group_id,
      group_name,
      focus_group
    ) %>%
    dplyr::summarise(
      n_transects = dplyr::n_distinct(transect),
      .groups = "drop"
    )

  focus_positive <- positive_values %>%
    dplyr::filter(!is.na(focus_group)) %>%
    dplyr::group_by(
      dataset_id,
      dataset_label,
      spatial_level,
      group_id,
      group_name,
      focus_group,
      taxon_id
    ) %>%
    dplyr::summarise(
      n_transects_present = dplyr::n_distinct(transect),
      sum_abundance = sum(abundance, na.rm = TRUE),
      sum_squared_abundance = sum(abundance^2, na.rm = TRUE),
      .groups = "drop"
    )

  selection_summary <- tidyr::crossing(
    focus_denominators,
    taxon_lookup
  ) %>%
    dplyr::left_join(
      focus_positive,
      by = c(
        "dataset_id",
        "dataset_label",
        "spatial_level",
        "group_id",
        "group_name",
        "focus_group",
        "taxon_id"
      )
    ) %>%
    add_summary_statistics()

  list(
    sample_groups = sample_groups,
    period_summary = period_summary,
    selection_summary = selection_summary
  )
}


# ============================================================
# 7. Prepare and summarise all three datasets
# ============================================================

prepared_datasets <- purrr::pmap(
  dataset_settings,
  prepare_rls_dataset
)

names(prepared_datasets) <- dataset_settings$dataset_id

summary_lists <- purrr::map(
  prepared_datasets,
  summarise_one_dataset
)

period_summary <- purrr::map_dfr(
  summary_lists,
  "period_summary"
)

selection_summary <- purrr::map_dfr(
  summary_lists,
  "selection_summary"
)


# Save checkable summary tables for every dataset and spatial level.
dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

summary_output_lookup <- period_summary %>%
  dplyr::distinct(
    dataset_id,
    spatial_level
  )

purrr::pwalk(
  summary_output_lookup,
  function(dataset_id, spatial_level) {
    summary_dir <- file.path(
      output_root,
      dataset_id,
      spatial_level,
      "summaries"
    )

    dir.create(
      summary_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    period_summary %>%
      dplyr::filter(
        .data$dataset_id == .env$dataset_id,
        .data$spatial_level == .env$spatial_level
      ) %>%
      readr::write_csv(
        file.path(
          summary_dir,
          "summary_by_separate_period.csv"
        )
      )

    selection_summary %>%
      dplyr::filter(
        .data$dataset_id == .env$dataset_id,
        .data$spatial_level == .env$spatial_level
      ) %>%
      readr::write_csv(
        file.path(
          summary_dir,
          "summary_used_for_focus_period_filters.csv"
        )
      )
  }
)


# ============================================================
# 8. Select taxa for one plot
# ============================================================

get_taxa_to_plot <- function(
    dataset_id_value,
    spatial_level_value,
    group_id_value,
    metric,
    focus_group_value) {

  if (!metric %in% c("occurrence", "abundance")) {
    stop("metric must be 'occurrence' or 'abundance'.")
  }

  if (!focus_group_value %in% c("Pre-bloom", "Post-bloom")) {
    stop(
      "focus_group_value must be 'Pre-bloom' or 'Post-bloom'."
    )
  }

  taxa_to_plot <- selection_summary %>%
    dplyr::filter(
      dataset_id == dataset_id_value,
      spatial_level == spatial_level_value,
      group_id == group_id_value,
      focus_group == focus_group_value
    )

  if (metric == "occurrence") {
    taxa_to_plot <- taxa_to_plot %>%
      dplyr::filter(
        occurrence_percent >= occurrence_threshold_percent
      )

    if (isTRUE(occurrence_also_requires_abundance)) {
      taxa_to_plot <- taxa_to_plot %>%
        dplyr::filter(
          average_abundance > abundance_threshold_per_transect
        )
    }

    taxa_to_plot <- taxa_to_plot %>%
      dplyr::mutate(
        selection_value = occurrence_percent
      )

  } else {
    taxa_to_plot <- taxa_to_plot %>%
      dplyr::filter(
        occurrence_percent >= occurrence_threshold_percent,
        average_abundance > abundance_threshold_per_transect
      ) %>%
      dplyr::mutate(
        selection_value = average_abundance
      )
  }

  taxa_to_plot <- taxa_to_plot %>%
    dplyr::arrange(
      dplyr::desc(selection_value),
      display_name
    )

  if (is.finite(max_taxa_per_plot)) {
    taxa_to_plot <- taxa_to_plot %>%
      dplyr::slice_head(
        n = max_taxa_per_plot
      )
  }

  taxa_to_plot
}


# ============================================================
# 9. Make one plot
# ============================================================

make_one_plot <- function(
    dataset_id_value,
    dataset_label_value,
    spatial_level_value,
    group_id_value,
    group_name_value,
    metric,
    focus_group_value) {

  if (metric == "occurrence") {
    value_col <- "occurrence_percent"
    se_col <- "occurrence_se"
    y_axis_label <- "Occurrence (% of transects)"
    metric_folder <- "occurrence"
    label_digits <- 1
  } else if (metric == "abundance") {
    value_col <- "average_abundance"
    se_col <- "abundance_se"
    y_axis_label <- "Average abundance per transect"
    metric_folder <- "abundance"
    label_digits <- 2
  } else {
    stop("metric must be 'occurrence' or 'abundance'.")
  }

  if (focus_group_value == "Pre-bloom") {
    focus_folder <- "pre_bloom_focus"
  } else if (focus_group_value == "Post-bloom") {
    focus_folder <- "post_bloom_focus"
  } else {
    stop(
      "focus_group_value must be 'Pre-bloom' or 'Post-bloom'."
    )
  }

  taxa_to_plot <- get_taxa_to_plot(
    dataset_id_value = dataset_id_value,
    spatial_level_value = spatial_level_value,
    group_id_value = group_id_value,
    metric = metric,
    focus_group_value = focus_group_value
  )

  if (nrow(taxa_to_plot) == 0) {
    message(
      "Skipping ",
      dataset_label_value,
      " - ",
      spatial_level_value,
      " - ",
      group_name_value,
      " - ",
      metric_folder,
      " - ",
      focus_folder,
      ": no taxa passed the focus-period filter."
    )

    return(NA_character_)
  }

  # With coord_flip(), the last factor level is displayed at the top.
  taxon_order <- taxa_to_plot %>%
    dplyr::arrange(selection_value) %>%
    dplyr::pull(taxon_id)

  taxon_labels <- taxa_to_plot %>%
    dplyr::distinct(taxon_id, display_name) %>%
    tibble::deframe()

  plot_dat <- period_summary %>%
    dplyr::filter(
      dataset_id == dataset_id_value,
      spatial_level == spatial_level_value,
      group_id == group_id_value,
      taxon_id %in% taxa_to_plot$taxon_id
    ) %>%
    dplyr::mutate(
      taxon_id = factor(
        taxon_id,
        levels = taxon_order
      ),
      value = .data[[value_col]],
      se = .data[[se_col]],
      ymin = pmax(0, value - se),
      ymax = value + se,
      value_label = dplyr::if_else(
        value > 0,
        if (label_digits == 1) {
          sprintf("%.1f", value)
        } else {
          sprintf("%.2f", value)
        },
        ""
      )
    )

  if (metric == "occurrence") {
    plot_dat <- plot_dat %>%
      dplyr::mutate(
        ymax = pmin(100, ymax)
      )
  }

  plot_periods <- c(
    "Pre-bloom",
    sort(
      setdiff(
        unique(plot_dat$plot_period),
        "Pre-bloom"
      )
    )
  )

  plot_periods <- plot_periods[
    !is.na(plot_periods) &
      plot_periods %in% plot_dat$plot_period
  ]

  plot_dat <- plot_dat %>%
    dplyr::mutate(
      plot_period = factor(
        plot_period,
        levels = plot_periods
      )
    )

  plot_fill_cols <- make_period_colours(
    plot_periods
  )

  pd <- position_dodge(
    width = 0.8,
    reverse = TRUE
  )

  plot_title <- NULL
  plot_subtitle <- NULL

  if (isTRUE(include_group_in_title)) {
    plot_title <- group_name_value
  }

  if (isTRUE(include_dataset_in_title)) {
    plot_subtitle <- dataset_label_value
  }

  p <- ggplot(
    plot_dat,
    aes(
      x = taxon_id,
      y = value,
      fill = plot_period
    )
  ) +
    geom_col(
      position = pd,
      width = 0.7
    )

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

  p <- p +
    coord_flip(clip = "off") +
    scale_x_discrete(
      labels = taxon_labels
    ) +
    scale_fill_manual(
      values = plot_fill_cols,
      breaks = plot_periods,
      drop = TRUE
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = y_axis_label,
      fill = NULL
    ) +
    plot_theme +
    theme(
      plot.margin = margin(
        t = 5.5,
        r = 25,
        b = 5.5,
        l = 5.5
      )
    )

  if (metric == "occurrence") {
    p <- p +
      scale_y_continuous(
        limits = c(0, 100),
        expand = expansion(
          mult = c(0, 0.05)
        )
      )
  } else {
    p <- p +
      scale_y_continuous(
        expand = expansion(
          mult = c(0, 0.12)
        )
      )
  }

  output_dir <- file.path(
    output_root,
    dataset_id_value,
    spatial_level_value,
    metric_folder,
    focus_folder
  )

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    output_dir,
    paste0(
      safe_filename(group_name_value),
      "_",
      safe_filename(group_id_value),
      "_",
      metric_folder,
      "_",
      focus_folder,
      ".png"
    )
  )

  plot_height <- max(
    minimum_plot_height,
    length(taxon_order) * height_per_taxon
  )

  ggsave(
    filename = output_file,
    plot = p,
    width = plot_width,
    height = plot_height,
    dpi = plot_dpi,
    bg = "white"
  )

  message("Saved: ", output_file)

  output_file
}


# ============================================================
# 10. Make all plots with one loop
# ============================================================

group_lookup <- period_summary %>%
  dplyr::distinct(
    dataset_id,
    dataset_label,
    spatial_level,
    group_id,
    group_name
  ) %>%
  dplyr::arrange(
    dataset_id,
    spatial_level,
    group_name
  )

plot_jobs <- tidyr::crossing(
  group_lookup,
  metric = c(
    "occurrence",
    "abundance"
  ),
  focus_group = c(
    "Pre-bloom",
    "Post-bloom"
  )
)

plot_log <- purrr::pmap_dfr(
  plot_jobs,
  function(
      dataset_id,
      dataset_label,
      spatial_level,
      group_id,
      group_name,
      metric,
      focus_group) {

    tryCatch(
      {
        output_file <- make_one_plot(
          dataset_id_value = dataset_id,
          dataset_label_value = dataset_label,
          spatial_level_value = spatial_level,
          group_id_value = group_id,
          group_name_value = group_name,
          metric = metric,
          focus_group_value = focus_group
        )

        tibble::tibble(
          dataset_id = dataset_id,
          dataset_label = dataset_label,
          spatial_level = spatial_level,
          group_id = group_id,
          group_name = group_name,
          metric = metric,
          focus_group = focus_group,
          status = dplyr::if_else(
            is.na(output_file),
            "Skipped - no taxa passed",
            "Saved"
          ),
          output_file = output_file,
          error = NA_character_
        )
      },
      error = function(e) {
        tibble::tibble(
          dataset_id = dataset_id,
          dataset_label = dataset_label,
          spatial_level = spatial_level,
          group_id = group_id,
          group_name = group_name,
          metric = metric,
          focus_group = focus_group,
          status = "Failed",
          output_file = NA_character_,
          error = conditionMessage(e)
        )
      }
    )
  }
)

readr::write_csv(
  plot_log,
  file.path(
    output_root,
    "plot_log.csv"
  )
)

plot_log %>%
  dplyr::count(
    dataset_label,
    spatial_level,
    status
  )

plot_log %>%
  dplyr::filter(status == "Failed")

message("Finished making RLS top occurrence and abundance plots.")
