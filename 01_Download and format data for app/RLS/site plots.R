library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# ============================================================
# Site representation through time
# ============================================================

site_year_effort <- sl_m1 %>%
  
  filter(!is.na(sampling_event_start_date)) %>%
  
  mutate(
    year = as.integer(format(sampling_event_start_date, "%Y"))
  ) %>%
  
  group_by(
    location,
    site_name,
    year
  ) %>%
  
  summarise(
    
    # Number of separate sampling events involving this site
    n_events = n_distinct(sampling_event),
    
    # Number of actual transects
    # "transect" is already unique across your duplicated blocks
    n_transects = n_distinct(transect),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# Add the missing site x year combinations
# ------------------------------------------------------------
# Importantly, this only uses years in which THAT LOCATION
# was sampled - not every year in the entire dataset.

site_year_coverage <- site_year_effort %>%
  group_by(location) %>%
  complete(
    site_name,
    year,
    fill = list(
      n_events = 0,
      n_transects = 0
    )
  ) %>%
  ungroup()

# ============================================================
# Summary of each site's contribution
# ============================================================

site_summary <- site_year_coverage %>%
  group_by(location, site_name) %>%
  summarise(
    n_years_sampled = sum(n_events > 0),
    n_location_years = n_distinct(year),
    proportion_years_sampled = n_years_sampled / n_location_years,
    total_events = sum(n_events),
    total_transects = sum(n_transects),
    .groups = "drop"
  )


# ============================================================
# Pretty site x year table
# ============================================================

site_year_tables <- site_year_coverage %>%
  mutate(
    sampling = case_when(
      n_events == 0 ~ "-",
      TRUE ~ paste0(
        n_events, " event",
        if_else(n_events == 1, "", "s"),
        " / ",
        n_transects, " transect",
        if_else(n_transects == 1, "", "s")
      )
    )
  ) %>%
  select(
    location,
    site_name,
    year,
    sampling
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = sampling
  ) %>%
  left_join(
    site_summary,
    by = c("location", "site_name")
  ) %>%
  arrange(
    location,
    desc(proportion_years_sampled),
    desc(total_transects),
    site_name
  )


# Turn it into a list - one table per location
# ============================================================
# Make one site x year table PER LOCATION
# ============================================================

location_tables <- site_year_coverage %>%
  
  mutate(
    sampling = case_when(
      n_events == 0 ~ "-",
      TRUE ~ paste0(
        n_events, " event",
        if_else(n_events == 1, "", "s"),
        " / ",
        n_transects, " transect",
        if_else(n_transects == 1, "", "s")
      )
    )
  ) %>%
  
  split(.$location) %>%
  
  purrr::map(
    ~ .x %>%
      
      # Add site-level summaries
      left_join(
        site_summary %>%
          select(
            location,
            site_name,
            n_years_sampled,
            n_location_years,
            proportion_years_sampled,
            total_events,
            total_transects
          ),
        by = c("location", "site_name")
      ) %>%
      
      select(
        site_name,
        year,
        sampling,
        n_years_sampled,
        n_location_years,
        proportion_years_sampled,
        total_events,
        total_transects
      ) %>%
      
      pivot_wider(
        names_from = year,
        values_from = sampling
      ) %>%
      
      arrange(
        desc(proportion_years_sampled),
        desc(total_transects),
        site_name
      )
  )

location_tables[["Eastern Spencer Gulf"]]

dir.create(
  "outputs/sampling_coverage/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

for (loc in names(location_tables)) {
  
  file_name <- loc %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_to_lower()
  
  write_csv(
    location_tables[[loc]],
    paste0(
      "outputs/sampling_coverage/tables/",
      file_name,
      "_site_sampling.csv"
    )
  )
}

dir.create(
  "outputs/sampling_coverage/plots",
  recursive = TRUE,
  showWarnings = FALSE
)

for (loc in unique(site_year_coverage$location)) {
  
  plot_data <- site_year_coverage %>%
    filter(location == loc)
  
  # Put the sites contributing the most data at the top
  site_order <- plot_data %>%
    group_by(site_name) %>%
    summarise(
      total_transects = sum(n_transects),
      .groups = "drop"
    ) %>%
    arrange(total_transects) %>%
    pull(site_name)
  
  plot_data <- plot_data %>%
    mutate(
      site_name = factor(
        site_name,
        levels = site_order
      ),
      
      # Zero becomes NA so unsampled combinations are grey
      effort_plot = if_else(
        n_transects == 0,
        NA_real_,
        as.numeric(n_transects)
      )
    )
  
  
  p <- ggplot(
    plot_data,
    aes(
      x = factor(year),
      y = site_name,
      fill = effort_plot
    )
  ) +
    
    geom_tile(
      colour = "white",
      linewidth = 0.5
    ) +
    
    geom_text(
      aes(
        label = if_else(
          n_transects == 0,
          "",
          as.character(n_transects)
        )
      ),
      size = 3.5
    ) +
    
    scale_fill_viridis_c(
      name = "Transects",
      na.value = "grey90"
    ) +
    
    labs(
      x = "Year",
      y = "Site"
    ) +
    
    theme_bw() +
    
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      legend.position = "right"
    )
  
  
  file_name <- loc %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_to_lower()
  
  ggsave(
    paste0(
      "outputs/sampling_coverage/plots/",
      file_name,
      "_site_sampling_heatmap.png"
    ),
    p,
    width = 10,
    height = max(5, length(unique(plot_data$site_name)) * 0.35),
    dpi = 300
  )
}