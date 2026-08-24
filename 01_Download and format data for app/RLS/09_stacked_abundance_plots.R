#################################################################
# RLS stacked relative-abundance plots
#
# Creates TWO plots for every LOCATION and REGION:
#   1. period       = Pre-bloom vs Bloom
#   2. period_split = Pre-bloom vs individual Bloom periods
#
# Each plot has 3 columns:
#   - M1 fish
#   - M2 fish
#   - M2 invertebrates
#
# IMPORTANT:
# Abundance is first calculated at the RLS transect level:
#   taxon count within block -> mean across surveyed blocks in transect
#
# Group-period relative abundance is then based on mean abundance per
# surveyed transect, so transects with more blocks do not receive more weight.
#################################################################

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggtext)
library(purrr)
library(sf)
library(CheckEM)

# -----------------------------------------------------------------
# 1. Settings
# -----------------------------------------------------------------

top_n <- 5

# Site plots deliberately excluded.
spatial_levels_to_plot <- c("location", "region")

method_levels <- c(
  "M1 fish",
  "M2 fish",
  "M2 invertebrates"
)

period_levels <- c(
  "Pre-bloom",
  "Bloom"
)

master_colour_file <- "sasha example/master_species_colours.rds"

output_root <- file.path(
  "plots",
  "rls_stacked_relative_abundance"
)

plot_width <- 16
plot_height <- 7
plot_dpi <- 300

include_group_title <- FALSE

# -----------------------------------------------------------------
# 2. General helpers
# -----------------------------------------------------------------

make_safe_filename <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")
}


month_to_date <- function(x) {
  x <- as.character(x)
  as.Date(
    ifelse(
      is.na(x),
      NA_character_,
      paste0(x, "-01")
    )
  )
}


check_one_row_per_transect <- function(data, dataset_name) {
  
  duplicate_transects <- data %>%
    dplyr::count(transect, name = "n") %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_transects) > 0) {
    stop(
      dataset_name,
      " survey list does not have one row per transect after cleaning. ",
      "Found ",
      nrow(duplicate_transects),
      " duplicated transects."
    )
  }
  
  invisible(data)
}


# -----------------------------------------------------------------
# 3. Read spatial metadata and survey lists
# -----------------------------------------------------------------

sa_sites <- sf::read_sf(
  "dev/Dive_sites_2026_07_14.shp"
) %>%
  CheckEM::clean_names() %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    site_code = as.character(site_code),
    site_name_lookup = site_name,
    region = bruvsrepor,
    location = location_g
  ) %>%
  dplyr::distinct(
    site_code,
    .keep_all = TRUE
  )


sl_m1 <- readr::read_rds(
  "data/tidy/rls_m1_surveys_final.rds"
) %>%
  dplyr::select(-dplyr::any_of(c("block", "id"))) %>%
  dplyr::distinct()

sl_m2_fish <- readr::read_rds(
  "data/tidy/rls_m2_fish_surveys_final.rds"
) %>%
  dplyr::select(-dplyr::any_of(c("block", "id"))) %>%
  dplyr::distinct()

sl_m2_inverts <- readr::read_rds(
  "data/tidy/rls_m2_inverts_surveys_final.rds"
) %>%
  dplyr::select(-dplyr::any_of(c("block", "id"))) %>%
  dplyr::distinct()


check_one_row_per_transect(sl_m1, "M1 fish")
check_one_row_per_transect(sl_m2_fish, "M2 fish")
check_one_row_per_transect(sl_m2_inverts, "M2 invertebrates")


# -----------------------------------------------------------------
# 4. Common-name lookups
# -----------------------------------------------------------------

dew_species_raw <- readr::read_csv(
  "data/lookups/SA-HAB-Functional Traits.csv",
  show_col_types = FALSE
) %>%
  CheckEM::clean_names()


# Your older code used left_join(dew_species) without specifying a key.
# That is risky, particularly because your BRUV script already checked for
# duplicate genus_species rows. Make a one-row-per-taxon lookup explicitly.
dew_duplicate_names <- dew_species_raw %>%
  dplyr::filter(
    !is.na(genus_species),
    genus_species != ""
  ) %>%
  dplyr::count(
    genus_species,
    name = "n"
  ) %>%
  dplyr::filter(n > 1)

if (nrow(dew_duplicate_names) > 0) {
  message(
    "DEW lookup contains ",
    nrow(dew_duplicate_names),
    " duplicated genus_species values. ",
    "The first non-missing common name will be used for plotting."
  )
}


dew_species_lookup <- dew_species_raw %>%
  dplyr::filter(
    !is.na(genus_species),
    genus_species != ""
  ) %>%
  dplyr::arrange(
    genus_species,
    is.na(common_name),
    common_name
  ) %>%
  dplyr::distinct(
    genus_species,
    .keep_all = TRUE
  ) %>%
  dplyr::select(
    genus_species,
    common_name
  )


checkem_species_lookup <- CheckEM::australia_life_history %>%
  dplyr::select(
    family,
    genus,
    species,
    australian_common_name
  ) %>%
  dplyr::arrange(
    family,
    genus,
    species,
    is.na(australian_common_name),
    australian_common_name
  ) %>%
  dplyr::distinct(
    family,
    genus,
    species,
    .keep_all = TRUE
  )


# -----------------------------------------------------------------
# 5. Convert a count file into transect-level taxon abundance
# -----------------------------------------------------------------

prepare_rls_taxon_abundance <- function(
    count_path,
    survey_list,
    method_name) {
  
  count_data <- readr::read_rds(count_path)
  
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
    names(count_data)
  )
  
  if (length(missing_count_columns) > 0) {
    stop(
      method_name,
      " count file is missing: ",
      paste(missing_count_columns, collapse = ", ")
    )
  }
  
  if (any(count_data$total < 0, na.rm = TRUE)) {
    stop(
      method_name,
      " contains negative values in `total`."
    )
  }
  
  # Number of surveyed blocks is calculated from the ENTIRE count table,
  # not separately for each species. This is essential. Otherwise a species
  # absent from one block would incorrectly have a smaller denominator.
  block_effort <- count_data %>%
    dplyr::distinct(
      transect,
      block
    ) %>%
    dplyr::count(
      transect,
      name = "n_blocks"
    )
  
  if (any(block_effort$n_blocks < 1)) {
    stop(
      method_name,
      " contains a transect with zero surveyed blocks."
    )
  }
  
  # Keep spp records for abundance.
  # Unlike species richness, a Genus spp observation still represents
  # counted individuals and should not automatically be discarded.
  block_taxa <- count_data %>%
    dplyr::filter(
      !is.na(transect),
      !is.na(block),
      !is.na(family),
      !is.na(genus),
      !is.na(species),
      !is.na(total)
    ) %>%
    dplyr::mutate(
      genus_species = paste(
        genus,
        species
      ),
      genus_plot = dplyr::if_else(
        genus == "Unknown",
        family,
        genus
      ),
      taxon_key = paste(
        genus_plot,
        species
      )
    ) %>%
    dplyr::group_by(
      transect,
      block,
      taxon_key
    ) %>%
    dplyr::summarise(
      # Sum duplicated records of the same plotted taxon within a block.
      block_abundance = sum(
        total,
        na.rm = TRUE
      ),
      
      # Retain one taxonomic record for common-name lookup.
      family = dplyr::first(family),
      genus = dplyr::first(genus),
      species = dplyr::first(species),
      genus_species = dplyr::first(genus_species),
      
      .groups = "drop"
    )
  
  # Mean abundance across ALL blocks surveyed in a transect.
  #
  # We sum the observed block counts then divide by n_blocks from block_effort.
  # A taxon absent from a block therefore contributes zero to that mean,
  # even if there was no explicit zero row for that taxon in that block.
  transect_taxa <- block_taxa %>%
    dplyr::group_by(
      transect,
      taxon_key
    ) %>%
    dplyr::summarise(
      summed_block_abundance = sum(
        block_abundance,
        na.rm = TRUE
      ),
      family = dplyr::first(family),
      genus = dplyr::first(genus),
      species = dplyr::first(species),
      genus_species = dplyr::first(genus_species),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      block_effort,
      by = "transect"
    ) %>%
    dplyr::mutate(
      abundance = summed_block_abundance / n_blocks,
      method = method_name
    ) %>%
    dplyr::select(
      transect,
      method,
      family,
      genus,
      species,
      genus_species,
      taxon_key,
      abundance,
      n_blocks
    )
  
  # The survey list is the authoritative list of sampled transects.
  samples <- survey_list %>%
    dplyr::mutate(
      method = method_name
    )
  
  # Only retain counts belonging to valid surveyed transects.
  transect_taxa <- transect_taxa %>%
    dplyr::semi_join(
      samples %>%
        dplyr::select(transect),
      by = "transect"
    )
  
  list(
    taxa = transect_taxa,
    samples = samples,
    block_effort = block_effort
  )
}


# -----------------------------------------------------------------
# 6. Prepare M1, M2 fish and M2 invertebrate data
# -----------------------------------------------------------------

m1 <- prepare_rls_taxon_abundance(
  count_path = "data/tidy/rls_m1_complete_count.rds",
  survey_list = sl_m1,
  method_name = "M1 fish"
)

m2_fish <- prepare_rls_taxon_abundance(
  count_path = "data/tidy/rls_m2_fish_complete_count.rds",
  survey_list = sl_m2_fish,
  method_name = "M2 fish"
)

m2_inverts <- prepare_rls_taxon_abundance(
  count_path = "data/tidy/rls_m2_inverts_complete_count.rds",
  survey_list = sl_m2_inverts,
  method_name = "M2 invertebrates"
)


all_taxa <- dplyr::bind_rows(
  m1$taxa,
  m2_fish$taxa,
  m2_inverts$taxa
)

all_samples <- dplyr::bind_rows(
  m1$samples,
  m2_fish$samples,
  m2_inverts$samples
)


# Add site -> location/region lookup to BOTH count and effort tables.
all_samples <- all_samples %>%
  dplyr::mutate(
    site_code = as.character(site_code)
  ) %>%
  dplyr::select(
    -dplyr::any_of(
      c("location", "region")
    )
  ) %>%
  dplyr::left_join(
    sa_sites,
    by = "site_code"
  )


all_taxa <- all_taxa %>%
  dplyr::left_join(
    all_samples %>%
      dplyr::select(
        transect,
        method,
        site_code,
        period,
        period_split,
        location,
        region
      ),
    by = c(
      "transect",
      "method"
    )
  )


# -----------------------------------------------------------------
# 7. Build one taxon display-name lookup
# -----------------------------------------------------------------

taxon_display_lookup <- all_taxa %>%
  dplyr::distinct(
    taxon_key,
    family,
    genus,
    species,
    genus_species
  ) %>%
  dplyr::left_join(
    dew_species_lookup,
    by = "genus_species"
  ) %>%
  dplyr::left_join(
    checkem_species_lookup,
    by = c(
      "family",
      "genus",
      "species"
    )
  ) %>%
  dplyr::mutate(
    common_name = dplyr::coalesce(
      common_name,
      australian_common_name
    ),
    
    display_name = dplyr::if_else(
      is.na(common_name) |
        common_name == "",
      paste0(
        "<i>",
        taxon_key,
        "</i>"
      ),
      paste0(
        "<i>",
        taxon_key,
        "</i><br>(",
        common_name,
        ")"
      )
    )
  ) %>%
  dplyr::arrange(
    taxon_key,
    is.na(common_name)
  ) %>%
  dplyr::distinct(
    taxon_key,
    .keep_all = TRUE
  ) %>%
  dplyr::select(
    taxon_key,
    display_name
  )


# -----------------------------------------------------------------
# 8. Expand into LOCATION and REGION only
# -----------------------------------------------------------------

expand_taxa_spatial_levels <- function(data) {
  
  dplyr::bind_rows(
    
    data %>%
      dplyr::transmute(
        spatial_level = "location",
        group_name = as.character(location),
        transect,
        method,
        period = as.character(period),
        period_split = as.character(period_split),
        taxon_key,
        abundance
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "region",
        group_name = as.character(region),
        transect,
        method,
        period = as.character(period),
        period_split = as.character(period_split),
        taxon_key,
        abundance
      )
  ) %>%
    dplyr::filter(
      spatial_level %in% spatial_levels_to_plot,
      !is.na(group_name),
      group_name != ""
    )
}


expand_sample_spatial_levels <- function(data) {
  
  dplyr::bind_rows(
    
    data %>%
      dplyr::transmute(
        spatial_level = "location",
        group_name = as.character(location),
        transect,
        method,
        period = as.character(period),
        period_split = as.character(period_split)
      ),
    
    data %>%
      dplyr::transmute(
        spatial_level = "region",
        group_name = as.character(region),
        transect,
        method,
        period = as.character(period),
        period_split = as.character(period_split)
      )
  ) %>%
    dplyr::filter(
      spatial_level %in% spatial_levels_to_plot,
      !is.na(group_name),
      group_name != ""
    )
}


spatial_taxa <- expand_taxa_spatial_levels(
  all_taxa
)

spatial_samples <- expand_sample_spatial_levels(
  all_samples
)


# -----------------------------------------------------------------
# 9. Format stacked abundance data
# -----------------------------------------------------------------

format_stacked_species_data <- function(
    taxon_data,
    sample_data,
    period_var,
    top_n = 5) {
  
  period_var <- rlang::ensym(period_var)
  
  # ---------------------------------------------------------------
  # A. Number of surveyed transects for every group / method / period
  # ---------------------------------------------------------------
  
  effort <- sample_data %>%
    dplyr::transmute(
      spatial_level,
      group_name,
      method,
      period_name = as.character(
        !!period_var
      ),
      transect
    ) %>%
    dplyr::filter(
      !is.na(period_name),
      period_name != ""
    ) %>%
    dplyr::distinct(
      spatial_level,
      group_name,
      method,
      period_name,
      transect
    ) %>%
    dplyr::count(
      spatial_level,
      group_name,
      method,
      period_name,
      name = "n_transects"
    )
  
  
  # ---------------------------------------------------------------
  # B. Mean abundance per surveyed transect for every taxon
  #
  # Do NOT simply sum raw counts across a period. That gives extra
  # weight to transects with more blocks.
  # ---------------------------------------------------------------
  
  taxon_summary <- taxon_data %>%
    dplyr::transmute(
      spatial_level,
      group_name,
      method,
      period_name = as.character(
        !!period_var
      ),
      transect,
      taxon_key,
      abundance
    ) %>%
    dplyr::filter(
      !is.na(period_name),
      period_name != "",
      !is.na(taxon_key),
      taxon_key != ""
    ) %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name,
      taxon_key
    ) %>%
    dplyr::summarise(
      abundance_sum = sum(
        abundance,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      effort,
      by = c(
        "spatial_level",
        "group_name",
        "method",
        "period_name"
      )
    ) %>%
    dplyr::mutate(
      mean_abundance = abundance_sum / n_transects
    )
  
  
  if (any(is.na(taxon_summary$n_transects))) {
    stop(
      "At least one taxon summary could not be matched to survey effort."
    )
  }
  
  
  # ---------------------------------------------------------------
  # C. Find the top N taxa IN EACH PERIOD
  #
  # This matches the intention of your original code.
  # ---------------------------------------------------------------
  
  top_taxa_by_period <- taxon_summary %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name
    ) %>%
    dplyr::slice_max(
      order_by = mean_abundance,
      n = top_n,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()
  
  
  # IMPORTANT FIX:
  # Your original function joined the top-species table back using period.
  # That means a species could be coloured separately in one period but be
  # hidden inside "Other" in another period.
  #
  # Instead, take the UNION of the period-specific top-N taxa, then show each
  # selected taxon explicitly in EVERY period for that group/method.
  selected_taxa <- top_taxa_by_period %>%
    dplyr::distinct(
      spatial_level,
      group_name,
      method,
      taxon_key
    ) %>%
    dplyr::mutate(
      is_selected = TRUE
    )
  
  
  # ---------------------------------------------------------------
  # D. Collapse everything not selected into Other
  # ---------------------------------------------------------------
  
  plot_df <- taxon_summary %>%
    dplyr::left_join(
      selected_taxa,
      by = c(
        "spatial_level",
        "group_name",
        "method",
        "taxon_key"
      )
    ) %>%
    dplyr::mutate(
      taxon_plot = dplyr::if_else(
        !is.na(is_selected),
        taxon_key,
        "Other"
      )
    ) %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name,
      taxon_plot
    ) %>%
    dplyr::summarise(
      mean_abundance = sum(
        mean_abundance,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name
    ) %>%
    dplyr::mutate(
      total_mean_abundance = sum(
        mean_abundance,
        na.rm = TRUE
      ),
      percent = dplyr::if_else(
        total_mean_abundance > 0,
        100 * mean_abundance /
          total_mean_abundance,
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
  
  
  # ---------------------------------------------------------------
  # E. Number of observed taxa represented by Other
  # ---------------------------------------------------------------
  
  other_labels <- taxon_summary %>%
    dplyr::anti_join(
      selected_taxa,
      by = c(
        "spatial_level",
        "group_name",
        "method",
        "taxon_key"
      )
    ) %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name
    ) %>%
    dplyr::summarise(
      n_other = dplyr::n_distinct(
        taxon_key
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      label = paste0(
        n_other,
        " taxa"
      )
    )
  
  
  # ---------------------------------------------------------------
  # F. Sanity check: every non-empty stacked bar must sum to 100%
  # ---------------------------------------------------------------
  
  percent_check <- plot_df %>%
    dplyr::filter(
      !is.na(percent)
    ) %>%
    dplyr::group_by(
      spatial_level,
      group_name,
      method,
      period_name
    ) %>%
    dplyr::summarise(
      total_percent = sum(
        percent,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      abs(total_percent - 100) > 1e-8
    )
  
  if (nrow(percent_check) > 0) {
    warning(
      nrow(percent_check),
      " stacked bars did not sum to 100%."
    )
  }
  
  
  list(
    plot_df = plot_df,
    other_labels = other_labels,
    taxon_summary = taxon_summary,
    top_taxa_by_period = top_taxa_by_period,
    selected_taxa = selected_taxa,
    percent_check = percent_check
  )
}


stacked_period <- format_stacked_species_data(
  taxon_data = spatial_taxa,
  sample_data = spatial_samples,
  period_var = period,
  top_n = top_n
)


stacked_period_split <- format_stacked_species_data(
  taxon_data = spatial_taxa,
  sample_data = spatial_samples,
  period_var = period_split,
  top_n = top_n
)


# -----------------------------------------------------------------
# 10. Period ordering
# -----------------------------------------------------------------

stacked_period$plot_df <- stacked_period$plot_df %>%
  dplyr::mutate(
    period_name = factor(
      period_name,
      levels = period_levels
    )
  )


period_split_levels <- c(
  "Pre-bloom",
  stacked_period_split$plot_df$period_name %>%
    as.character() %>%
    unique() %>%
    stats::na.omit() %>%
    setdiff("Pre-bloom") %>%
    sort()
)


stacked_period_split$plot_df <- stacked_period_split$plot_df %>%
  dplyr::mutate(
    period_name = factor(
      period_name,
      levels = period_split_levels
    )
  )


# -----------------------------------------------------------------
# 11. Build ONE reproducible palette for both plot types
# -----------------------------------------------------------------

all_selected_taxa <- unique(
  c(
    as.character(
      stacked_period$plot_df$taxon_plot
    ),
    as.character(
      stacked_period_split$plot_df$taxon_plot
    )
  )
)

all_selected_taxa <- sort(
  setdiff(
    all_selected_taxa,
    "Other"
  )
)


if (file.exists(master_colour_file)) {
  
  colour_pool <- unname(
    readRDS(master_colour_file)
  )
  
} else {
  
  message(
    "Could not find ",
    master_colour_file,
    ". Using an automatically generated qualitative palette instead."
  )
  
  colour_pool <- grDevices::hcl.colors(
    max(
      20,
      length(all_selected_taxa)
    ),
    palette = "Dynamic"
  )
}


if (length(colour_pool) < length(all_selected_taxa)) {
  
  message(
    "Master colour file has fewer colours than selected taxa. ",
    "Generating a larger palette."
  )
  
  colour_pool <- grDevices::hcl.colors(
    length(all_selected_taxa),
    palette = "Dynamic"
  )
}


taxon_palette <- stats::setNames(
  colour_pool[
    seq_along(
      all_selected_taxa
    )
  ],
  all_selected_taxa
)

taxon_palette["Other"] <- "grey70"


# Display-name vector used only for legend labels.
legend_labels <- taxon_display_lookup$display_name
names(legend_labels) <- taxon_display_lookup$taxon_key
legend_labels["Other"] <- "Other"


# -----------------------------------------------------------------
# 12. Plot theme
# -----------------------------------------------------------------

plot_theme <- ggplot2::theme_bw(
  base_size = 14
) +
  ggplot2::theme(
    axis.line.x = element_line(
      color = "black",
      linewidth = 0.5
    ),
    axis.line.y = element_line(
      color = "black",
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    legend.position = "bottom",
    legend.text = ggtext::element_markdown(),
    plot.title = element_text(
      face = "bold"
    )
  )


# -----------------------------------------------------------------
# 13. Plot one location/region
# -----------------------------------------------------------------

plot_stacked_abundance <- function(
    stacked_data,
    spatial_level_value,
    group_name_value,
    period_type = c(
      "period",
      "period_split"
    )) {
  
  period_type <- match.arg(
    period_type
  )
  
  plot_dat <- stacked_data$plot_df %>%
    dplyr::filter(
      spatial_level == spatial_level_value,
      group_name == group_name_value
    ) %>%
    dplyr::mutate(
      method = factor(
        method,
        levels = method_levels
      )
    )
  
  
  if (nrow(plot_dat) == 0) {
    return(NULL)
  }
  
  
  # Use only taxa actually required by this plot, but keep colours consistent
  # with every other plot produced by this script.
  plot_taxa <- plot_dat$taxon_plot %>%
    as.character() %>%
    unique()
  
  plot_taxa <- c(
    sort(
      setdiff(
        plot_taxa,
        "Other"
      )
    ),
    intersect(
      "Other",
      plot_taxa
    )
  )
  
  
  plot_dat <- plot_dat %>%
    dplyr::mutate(
      taxon_plot = factor(
        taxon_plot,
        levels = plot_taxa
      )
    )
  
  
  if (period_type == "period") {
    
    x_labels <- function(x) x
    
  } else {
    
    # Match the style of your BRUV example:
    # Bloom 2025-05 -> Bloom
    #                  2025-05
    x_labels <- function(x) {
      stringr::str_replace(
        x,
        "^Bloom ",
        "Bloom\n"
      )
    }
  }
  
  
  p <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = period_name,
      y = percent,
      fill = taxon_plot
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(
        reverse = TRUE
      ),
      width = 0.75,
      colour = "black",
      linewidth = 0.25
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(method),
      nrow = 1,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = taxon_palette,
      breaks = plot_taxa,
      labels = legend_labels[
        plot_taxa
      ],
      drop = TRUE,
      na.translate = FALSE
    ) +
    ggplot2::scale_x_discrete(
      labels = x_labels,
      drop = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        100
      ),
      breaks = c(
        0,
        25,
        50,
        75,
        100
      ),
      labels = function(x) {
        paste0(
          x,
          "%"
        )
      },
      expand = ggplot2::expansion(
        mult = c(
          0,
          0
        )
      )
    ) +
    ggplot2::labs(
      title = if (
        isTRUE(
          include_group_title
        )
      ) {
        group_name_value
      } else {
        NULL
      },
      x = NULL,
      y = "Relative abundance (%)",
      fill = "Taxon"
    ) +
    plot_theme +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        ncol = 3,
        byrow = TRUE
      )
    )
  
  
  p
}


# -----------------------------------------------------------------
# 14. Save TWO plots for every location and region
# -----------------------------------------------------------------

group_lookup <- spatial_samples %>%
  dplyr::select(
    spatial_level,
    group_name
  ) %>%
  dplyr::filter(
    spatial_level %in% spatial_levels_to_plot
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(
    spatial_level,
    group_name
  )


save_one_group <- function(
    spatial_level,
    group_name) {
  
  safe_group <- make_safe_filename(
    group_name
  )
  
  output_dir <- file.path(
    output_root,
    spatial_level
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ---------------------------------------------------------------
  # Plot 1: Pre-bloom vs Bloom
  # ---------------------------------------------------------------
  
  period_plot <- plot_stacked_abundance(
    stacked_data = stacked_period,
    spatial_level_value = spatial_level,
    group_name_value = group_name,
    period_type = "period"
  )
  
  
  # ---------------------------------------------------------------
  # Plot 2: Pre-bloom vs separate Bloom periods
  # ---------------------------------------------------------------
  
  period_split_plot <- plot_stacked_abundance(
    stacked_data = stacked_period_split,
    spatial_level_value = spatial_level,
    group_name_value = group_name,
    period_type = "period_split"
  )
  
  
  saved <- character(0)
  
  
  if (!is.null(period_plot)) {
    
    period_file <- file.path(
      output_dir,
      paste0(
        safe_group,
        "_stacked_abundance_period.png"
      )
    )
    
    ggplot2::ggsave(
      filename = period_file,
      plot = period_plot,
      width = plot_width,
      height = plot_height,
      dpi = plot_dpi,
      bg = "white"
    )
    
    saved <- c(
      saved,
      "period"
    )
  }
  
  
  if (!is.null(period_split_plot)) {
    
    period_split_file <- file.path(
      output_dir,
      paste0(
        safe_group,
        "_stacked_abundance_period_split.png"
      )
    )
    
    ggplot2::ggsave(
      filename = period_split_file,
      plot = period_split_plot,
      width = plot_width,
      height = plot_height,
      dpi = plot_dpi,
      bg = "white"
    )
    
    saved <- c(
      saved,
      "period_split"
    )
  }
  
  
  tibble::tibble(
    spatial_level = spatial_level,
    group_name = group_name,
    plots_saved = paste(
      saved,
      collapse = ", "
    ),
    status = ifelse(
      length(saved) > 0,
      "Saved",
      "No data"
    )
  )
}


plot_log <- purrr::pmap_dfr(
  group_lookup,
  save_one_group
)


# -----------------------------------------------------------------
# 15. Save validation tables
# -----------------------------------------------------------------

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)


readr::write_csv(
  stacked_period$taxon_summary,
  file.path(
    output_root,
    "period_taxon_mean_abundance.csv"
  )
)

readr::write_csv(
  stacked_period$selected_taxa,
  file.path(
    output_root,
    "period_selected_taxa.csv"
  )
)

readr::write_csv(
  stacked_period$plot_df,
  file.path(
    output_root,
    "period_stacked_plot_data.csv"
  )
)


readr::write_csv(
  stacked_period_split$taxon_summary,
  file.path(
    output_root,
    "period_split_taxon_mean_abundance.csv"
  )
)

readr::write_csv(
  stacked_period_split$selected_taxa,
  file.path(
    output_root,
    "period_split_selected_taxa.csv"
  )
)

readr::write_csv(
  stacked_period_split$plot_df,
  file.path(
    output_root,
    "period_split_stacked_plot_data.csv"
  )
)


readr::write_csv(
  plot_log,
  file.path(
    output_root,
    "plot_log.csv"
  )
)


# -----------------------------------------------------------------
# 16. Final checks
# -----------------------------------------------------------------

plot_log %>%
  dplyr::count(
    spatial_level,
    status
  )

stacked_period$percent_check
stacked_period_split$percent_check

message(
  "Finished stacked RLS abundance plots."
)