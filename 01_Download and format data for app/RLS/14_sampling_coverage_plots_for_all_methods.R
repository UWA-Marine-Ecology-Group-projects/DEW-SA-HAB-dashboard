library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(patchwork)


sl_m1 <- readr::read_rds("data/tidy/rls_m1_surveys_final.rds") %>% dplyr::select(-c(block, id)) %>% dplyr::distinct() 
sl_m2_fish <- readr::read_rds("data/tidy/rls_m2_fish_surveys_final.rds") %>% dplyr::select(-c(block, id)) %>% dplyr::distinct() 
sl_m2_inverts <- readr::read_rds("data/tidy/rls_m2_inverts_surveys_final.rds") %>% dplyr::select(-c(block, id)) %>% dplyr::distinct()

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
# 2. Function to calculate site x year sampling coverage
# ============================================================

calculate_coverage_by_method <- function(data) {
  
  data %>%
    
    # Add Fished / No-take status
    dplyr::left_join(
      status,
      by = "site_code"
    ) %>%
    
    # Only records with a sampling event date
    dplyr::filter(
      !is.na(sampling_event_start_date)
    ) %>%
    
    # Extract sampling year
    dplyr::mutate(
      year = as.integer(
        format(
          sampling_event_start_date,
          "%Y"
        )
      )
    ) %>%
    
    # Calculate effort for each site x year
    dplyr::group_by(
      location,
      site_name,
      status,
      year
    ) %>%
    
    dplyr::summarise(
      
      n_events = dplyr::n_distinct(
        sampling_event
      ),
      
      n_transects = dplyr::n_distinct(
        transect
      ),
      
      .groups = "drop"
    ) %>%
    
    
    # Add site x year combinations where the LOCATION
    # was sampled but this particular SITE was not
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
}


# ============================================================
# 3. Calculate coverage for each survey method
# ============================================================

coverage_by_method <- list(
  
  "M1" =
    calculate_coverage_by_method(
      sl_m1
    ),
  
  "M2 fish" =
    calculate_coverage_by_method(
      sl_m2_fish
    ),
  
  "M2 inverts" =
    calculate_coverage_by_method(
      sl_m2_inverts
    )
)


# ============================================================
# 4. Function to make one heatmap
# ============================================================

make_sampling_heatmap <- function(
    data,
    location_name,
    method_name,
    all_years,
    max_effort,
    show_x_axis = TRUE
) {
  
  
  # ----------------------------------------------------------
  # Data for this location
  # ----------------------------------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      location == location_name
    )
  
  
  # Return nothing if this method was never sampled here
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Order sites within status
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
    
    # Least sampled at bottom;
    # most sampled at top
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
      
      # Zero effort becomes grey
      effort_plot = dplyr::if_else(
        n_transects == 0,
        NA_real_,
        as.numeric(n_transects)
      )
    )
  
  
  # ----------------------------------------------------------
  # Make heatmap
  # ----------------------------------------------------------
  
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
        label = dplyr::if_else(
          n_transects == 0,
          "",
          as.character(n_transects)
        )
      ),
      
      size = 3.5
    ) +
    
    
    # Fished / No-take
    facet_grid(
      status ~ .,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    
    
    # Same year axis for all three methods
    scale_x_discrete(
      limits = as.character(all_years),
      drop = FALSE
    ) +
    
    
    # Same colour scale for all three methods
    scale_fill_viridis_c(
      name = "Transects",
      na.value = "grey90",
      limits = c(
        0,
        max_effort
      )
    ) +
    
    
    labs(
      title = method_name,
      x = if (show_x_axis) "Year" else NULL,
      y = "Site"
    ) +
    
    
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
      ),
      
      plot.title = element_text(
        face = "bold"
      )
    )
  
  
  # Remove duplicated year labels from upper plots
  if (!show_x_axis) {
    
    p <- p +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }
  
  
  p
}


# ============================================================
# 5. Make combined M1 + M2 fish + M2 invert heatmap
#    for every location
# ============================================================

dir.create(
  "outputs/sampling_coverage/plots_combined",
  recursive = TRUE,
  showWarnings = FALSE
)


# All locations represented in any method
all_locations <- coverage_by_method %>%
  purrr::map(
    ~ unique(.x$location)
  ) %>%
  unlist(
    use.names = FALSE
  ) %>%
  unique() %>%
  sort()


for (loc in all_locations) {
  
  
  # ----------------------------------------------------------
  # Which methods exist at this location?
  # ----------------------------------------------------------
  
  methods_present <- names(coverage_by_method)[
    
    purrr::map_lgl(
      
      coverage_by_method,
      
      ~ any(
        .x$location == loc
      )
    )
  ]
  
  
  # ----------------------------------------------------------
  # Use the union of years from all three methods
  #
  # This makes the x axes line up between M1, M2 fish
  # and M2 inverts.
  # ----------------------------------------------------------
  
  location_years <- coverage_by_method[
    methods_present
  ] %>%
    
    purrr::map(
      
      ~ .x %>%
        dplyr::filter(
          location == loc
        ) %>%
        dplyr::pull(
          year
        )
    ) %>%
    
    unlist(
      use.names = FALSE
    ) %>%
    
    unique() %>%
    
    sort()
  
  
  # ----------------------------------------------------------
  # Common colour scale for the three heatmaps
  # ----------------------------------------------------------
  
  max_effort <- coverage_by_method[
    methods_present
  ] %>%
    
    purrr::map_dbl(
      
      ~ .x %>%
        dplyr::filter(
          location == loc
        ) %>%
        dplyr::pull(
          n_transects
        ) %>%
        max(
          na.rm = TRUE
        )
    ) %>%
    
    max(
      na.rm = TRUE
    )
  
  
  if (
    !is.finite(max_effort) ||
    max_effort == 0
  ) {
    max_effort <- 1
  }
  
  
  # ----------------------------------------------------------
  # Make each method's heatmap
  # ----------------------------------------------------------
  
  plots <- purrr::map2(
    
    methods_present,
    seq_along(methods_present),
    
    ~ make_sampling_heatmap(
      
      data =
        coverage_by_method[[.x]],
      
      location_name =
        loc,
      
      method_name =
        .x,
      
      all_years =
        location_years,
      
      max_effort =
        max_effort,
      
      # Only show year labels on bottom plot
      show_x_axis =
        .y == length(methods_present)
    )
  )
  
  
  # ----------------------------------------------------------
  # Stack heatmaps into one figure
  # ----------------------------------------------------------
  
  combined_plot <- patchwork::wrap_plots(
    
    plots,
    
    ncol = 1,
    
    guides = "collect"
    
  ) +
    
    patchwork::plot_annotation(
      title = loc
    ) &
    
    theme(
      legend.position = "right"
    )
  
  
  # ----------------------------------------------------------
  # Determine sensible plot height
  # ----------------------------------------------------------
  
  n_site_rows <- coverage_by_method[
    methods_present
  ] %>%
    
    purrr::map_int(
      
      ~ .x %>%
        dplyr::filter(
          location == loc
        ) %>%
        dplyr::distinct(
          site_name
        ) %>%
        nrow()
    ) %>%
    
    sum()
  
  
  plot_height <- max(
    10,
    n_site_rows * 0.30
  )
  
  
  # ----------------------------------------------------------
  # Save
  # ----------------------------------------------------------
  
  file_name <- loc %>%
    
    stringr::str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    
    stringr::str_to_lower()
  
  
  ggsave(
    
    filename = paste0(
      "outputs/sampling_coverage/plots_combined/",
      file_name,
      "_sampling_heatmaps.png"
    ),
    
    plot = combined_plot,
    
    width = 11,
    
    height = plot_height,
    
    dpi = 300
  )
}