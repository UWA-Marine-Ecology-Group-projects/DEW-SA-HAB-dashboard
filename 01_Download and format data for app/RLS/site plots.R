#################################################################
# Check site representation through time
#
# Outputs:
# 1. One CSV table per location
# 2. One site x year heatmap per location
#
# Heatmaps show:
# - Sites on y-axis
# - Years on x-axis
# - Number of transects in each cell
# - Fished / No-take sites in separate panels
#################################################################


# Load libraries ----

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)


# ============================================================
# 1. Create site status lookup
# ============================================================

status <- sa_sites %>%
  dplyr::select(
    site_code,
    status
  ) %>%
  dplyr::distinct(
    site_code,
    .keep_all = TRUE
  )


# ============================================================
# 2. Calculate site sampling effort by year
# ============================================================

site_year_effort <- sl_m1 %>%
  
  # Add Fished / No-take status
  dplyr::left_join(
    status,
    by = "site_code"
  ) %>%
  
  # Only records with a sampling event date
  dplyr::filter(
    !is.na(sampling_event_start_date)
  ) %>%
  
  # Extract year from the location-level sampling event
  dplyr::mutate(
    year = as.integer(
      format(
        sampling_event_start_date,
        "%Y"
      )
    )
  ) %>%
  
  dplyr::group_by(
    location,
    site_name,
    status,
    year
  ) %>%
  
  dplyr::summarise(
    
    # Number of separate sampling events involving this site
    n_events = dplyr::n_distinct(
      sampling_event
    ),
    
    # Number of actual transects
    # This does not count the duplicated M1 blocks separately
    n_transects = dplyr::n_distinct(
      transect
    ),
    
    .groups = "drop"
  )


# ============================================================
# 3. Add missing site x year combinations
# ============================================================
#
# This creates a zero where:
# - the LOCATION was sampled in that year
# - but that particular SITE was not sampled
#
# Importantly, it does NOT add years from other locations.
#
# nesting(site_name, status) keeps each site's real status and
# prevents a site being artificially created as both Fished
# and No-take.
# ============================================================

site_year_coverage <- site_year_effort %>%
  
  dplyr::group_by(
    location
  ) %>%
  
  tidyr::complete(
    
    tidyr::nesting(
      site_name,
      status
    ),
    
    year,
    
    fill = list(
      n_events = 0,
      n_transects = 0
    )
  ) %>%
  
  dplyr::ungroup()


# ============================================================
# 4. Summarise each site's contribution to the time series
# ============================================================

site_summary <- site_year_coverage %>%
  
  dplyr::group_by(
    location,
    site_name,
    status
  ) %>%
  
  dplyr::summarise(
    
    # Number of years this site was actually sampled
    n_years_sampled = sum(
      n_events > 0
    ),
    
    # Number of years the location was sampled
    n_location_years = dplyr::n_distinct(
      year
    ),
    
    # Proportion of the location's sampled years
    # in which this site was represented
    proportion_years_sampled =
      n_years_sampled / n_location_years,
    
    # Total sampling effort
    total_events = sum(
      n_events
    ),
    
    total_transects = sum(
      n_transects
    ),
    
    .groups = "drop"
  )


# ============================================================
# 5. Make one site x year table for each location
# ============================================================
#
# Example cell:
#
# 1 event / 4 transects
#
# "-" means:
# The location was sampled in this year,
# but this particular site was not.
#
# ============================================================

location_tables <- site_year_coverage %>%
  
  dplyr::mutate(
    
    sampling = dplyr::case_when(
      
      n_events == 0 ~ "-",
      
      TRUE ~ paste0(
        
        n_events,
        " event",
        dplyr::if_else(
          n_events == 1,
          "",
          "s"
        ),
        
        " / ",
        
        n_transects,
        " transect",
        dplyr::if_else(
          n_transects == 1,
          "",
          "s"
        )
      )
    )
  ) %>%
  
  # Split BEFORE pivot_wider()
  # so each location only gets its own years
  split(
    .$location
  ) %>%
  
  purrr::map(
    
    ~ .x %>%
      
      dplyr::left_join(
        
        site_summary,
        
        by = c(
          "location",
          "site_name",
          "status"
        )
      ) %>%
      
      dplyr::select(
        site_name,
        status,
        year,
        sampling,
        n_years_sampled,
        n_location_years,
        proportion_years_sampled,
        total_events,
        total_transects
      ) %>%
      
      tidyr::pivot_wider(
        names_from = year,
        values_from = sampling
      ) %>%
      
      # Order sites that contribute most heavily first
      dplyr::arrange(
        status,
        dplyr::desc(
          proportion_years_sampled
        ),
        dplyr::desc(
          total_transects
        ),
        site_name
      )
  )


# ============================================================
# 6. View an example table
# ============================================================

location_tables[["Eastern Spencer Gulf"]]


# ============================================================
# 7. Save tables
# ============================================================

dir.create(
  "outputs/sampling_coverage/tables",
  recursive = TRUE,
  showWarnings = FALSE
)


for (loc in names(location_tables)) {
  
  file_name <- loc %>%
    stringr::str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    stringr::str_to_lower()
  
  
  readr::write_csv(
    
    location_tables[[loc]],
    
    paste0(
      "outputs/sampling_coverage/tables/",
      file_name,
      "_site_sampling.csv"
    )
  )
}


# ============================================================
# 8. Create heatmaps
# ============================================================
#
# Cell values = number of transects
#
# Grey cell = site was not sampled in that year
#             although the location was sampled
#
# Fished and No-take sites are shown in separate panels.
# ============================================================

dir.create(
  "outputs/sampling_coverage/plots",
  recursive = TRUE,
  showWarnings = FALSE
)


for (loc in unique(site_year_coverage$location)) {
  
  
  # ----------------------------------------------------------
  # Data for this location
  # ----------------------------------------------------------
  
  plot_data <- site_year_coverage %>%
    
    dplyr::filter(
      location == loc
    )
  
  
  # ----------------------------------------------------------
  # Order sites within status according to their total
  # contribution to the dataset
  #
  # Least-sampled sites appear towards the bottom and
  # most-sampled sites towards the top.
  # ----------------------------------------------------------
  
  site_order <- plot_data %>%
    
    dplyr::group_by(
      status,
      site_name
    ) %>%
    
    dplyr::summarise(
      
      total_transects = sum(
        n_transects
      ),
      
      .groups = "drop"
    ) %>%
    
    dplyr::arrange(
      status,
      total_transects
    ) %>%
    
    dplyr::pull(
      site_name
    )
  
  
  # ----------------------------------------------------------
  # Prepare plotting variables
  # ----------------------------------------------------------
  
  plot_data <- plot_data %>%
    
    dplyr::mutate(
      
      site_name = factor(
        site_name,
        levels = unique(site_order)
      ),
      
      # Convert zero effort to NA for plotting.
      # These cells will appear grey.
      effort_plot = dplyr::if_else(
        n_transects == 0,
        NA_real_,
        as.numeric(n_transects)
      )
    )
  
  
  # ----------------------------------------------------------
  # Plot
  # ----------------------------------------------------------
  
  p <- ggplot(
    
    plot_data,
    
    aes(
      x = factor(year),
      y = site_name,
      fill = effort_plot
    )
  ) +
    
    
    # Heatmap cells
    geom_tile(
      colour = "white",
      linewidth = 0.5
    ) +
    
    
    # Number of transects within each sampled cell
    geom_text(
      
      aes(
        label = dplyr::if_else(
          n_transects == 0,
          "",
          as.character(
            n_transects
          )
        )
      ),
      
      size = 3.5
    ) +
    
    
    # Separate Fished and No-take sites
    facet_grid(
      status ~ .,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    
    
    # Sampling effort colour scale
    scale_fill_viridis_c(
      name = "Transects",
      na.value = "grey90"
    ) +
    
    
    # Labels
    labs(
      x = "Year",
      y = "Site"
    ) +
    
    
    # Theme
    theme_bw() +
    
    theme(
      
      panel.grid = element_blank(),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      
      legend.position = "right",
      
      strip.placement = "outside",
      
      strip.background = element_rect(
        fill = "grey95"
      ),
      
      strip.text.y.left = element_text(
        angle = 0
      )
    )
  
  
  # ----------------------------------------------------------
  # Save plot
  # ----------------------------------------------------------
  
  file_name <- loc %>%
    
    stringr::str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    
    stringr::str_to_lower()
  
  
  ggsave(
    
    paste0(
      "outputs/sampling_coverage/plots/",
      file_name,
      "_site_sampling_heatmap.png"
    ),
    
    p,
    
    width = 10,
    
    height = max(
      5,
      length(
        unique(
          plot_data$site_name
        )
      ) * 0.35
    ),
    
    dpi = 300
  )
}


# ============================================================
# 9. Summarise site coverage within each location x year
# ============================================================
#
# Useful for identifying years where only a small proportion
# of the available sites contributed to the analysis.
# ============================================================

year_coverage <- site_year_coverage %>%
  
  dplyr::group_by(
    location,
    year
  ) %>%
  
  dplyr::summarise(
    
    n_sites_sampled = sum(
      n_events > 0
    ),
    
    n_sites_possible = dplyr::n_distinct(
      site_name
    ),
    
    proportion_sites_sampled =
      n_sites_sampled /
      n_sites_possible,
    
    total_events = sum(
      n_events
    ),
    
    total_transects = sum(
      n_transects
    ),
    
    .groups = "drop"
  )


# View the year coverage
year_coverage


# ============================================================
# 10. Status-specific year coverage
# ============================================================
#
# This is particularly useful for your Period x Status models.
#
# It shows whether, for example:
# - 5 Fished sites but only 1 No-take site were sampled
# - No-take sites only appeared in later years
# - site representation differs substantially among years
# ============================================================

year_status_coverage <- site_year_coverage %>%
  
  dplyr::group_by(
    location,
    year,
    status
  ) %>%
  
  dplyr::summarise(
    
    n_sites_sampled = sum(
      n_events > 0
    ),
    
    n_sites_possible = dplyr::n_distinct(
      site_name
    ),
    
    proportion_sites_sampled =
      n_sites_sampled /
      n_sites_possible,
    
    total_events = sum(
      n_events
    ),
    
    total_transects = sum(
      n_transects
    ),
    
    .groups = "drop"
  )


# View the status-specific coverage
year_status_coverage


# ============================================================
# 11. Save the overall coverage summaries
# ============================================================

readr::write_csv(
  site_summary,
  "outputs/sampling_coverage/site_summary.csv"
)


readr::write_csv(
  year_coverage,
  "outputs/sampling_coverage/year_coverage.csv"
)


readr::write_csv(
  year_status_coverage,
  "outputs/sampling_coverage/year_status_coverage.csv"
)
