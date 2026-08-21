library(dplyr)
library(tidyr)
library(vegan)
library(ecotraj)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tibble)
library(stringr)

# ============================================================
# 1. Read data ONCE
# ============================================================

sa_sites <- sf::read_sf(
  "dev/Dive_sites_2026_07_14.shp"
) %>%
  CheckEM::clean_names() %>%
  select(
    site_code,
    site_name,
    location_g,
    bruvsrepor
  )


m1_raw <- readRDS(
  "data/tidy/rls_m1_complete_count.rds"
) %>%
  left_join(sa_sites)


# ============================================================
# 2. Output folder
# ============================================================

output_dir <- "outputs/trajectory_plots"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. Function to produce trajectory plot for one location
# ============================================================

make_trajectory_plot <- function(location_name) {
  
  message("Running: ", location_name)
  
  
  # ==========================================================
  # CREATE SITE/EVENT SPECIES ABUNDANCE
  # ==========================================================
  
  m1_complete_count <- m1_raw %>%
    
    # --------------------------------------------------------
  # Filter current location
  # --------------------------------------------------------
  filter(
    location_g == location_name
  ) %>%
    
    # --------------------------------------------------------
  # STEP 1:
  # Average blocks within survey
  # --------------------------------------------------------
  group_by(
    period,
    sampling_event_start_date,
    survey_id,
    survey_date,
    site_name,
    sampling_event,
    depth,
    family,
    genus,
    species,
    scientific
  ) %>%
    
    summarise(
      total_block = mean(total),
      .groups = "drop"
    ) %>%
    
    # --------------------------------------------------------
  # STEP 2:
  # Average surveys/events within site
  # --------------------------------------------------------
  group_by(
    period,
    sampling_event_start_date,
    site_name,
    sampling_event,
    family,
    genus,
    species,
    scientific
  ) %>%
    
    summarise(
      total_site = mean(total_block),
      .groups = "drop"
    ) %>%
    
    filter(
      total_site > 1
    ) %>%
    
    mutate(
      id = paste(
        site_name,
        sampling_event,
        sep = "_"
      )
    ) %>%
    
    ungroup()
  
  
  # ==========================================================
  # Metadata
  # ==========================================================
  
  m1_meta <- m1_complete_count %>%
    
    distinct(
      id,
      site_name,
      sampling_event,
      sampling_event_start_date,
      period
    ) %>%
    
    mutate(
      survey_date = as.Date(
        sampling_event_start_date
      ),
      
      Year = lubridate::year(
        survey_date
      ),
      
      period = factor(period),
      
      year = factor(Year)
    ) %>%
    
    arrange(
      site_name,
      survey_date
    )
  
  
  # ----------------------------------------------------------
  # Need enough observations for a 2D ordination
  # ----------------------------------------------------------
  
  if (nrow(m1_meta) < 3) {
    
    message(
      "Skipping ",
      location_name,
      ": fewer than 3 sampling events."
    )
    
    return(NULL)
  }
  
  
  stopifnot(
    !anyDuplicated(m1_meta$id)
  )
  
  
  # ==========================================================
  # Sample x species matrix
  # ==========================================================
  
  m1_assemblage <- m1_complete_count %>%
    
    select(
      id,
      scientific,
      total_site
    ) %>%
    
    pivot_wider(
      names_from = scientific,
      values_from = total_site,
      values_fill = 0
    ) %>%
    
    arrange(
      match(
        id,
        m1_meta$id
      )
    )
  
  
  assemblage <- m1_assemblage %>%
    
    column_to_rownames(
      "id"
    ) %>%
    
    as.data.frame()
  
  
  stopifnot(
    identical(
      rownames(assemblage),
      m1_meta$id
    )
  )
  
  
  # ----------------------------------------------------------
  # Remove species with no variation within this location
  #
  # This makes envfit more robust when looping over locations.
  # ----------------------------------------------------------
  
  assemblage <- assemblage[
    ,
    vapply(
      assemblage,
      function(x) {
        length(unique(x)) > 1
      },
      logical(1)
    ),
    drop = FALSE
  ]
  
  
  if (ncol(assemblage) < 2) {
    
    message(
      "Skipping ",
      location_name,
      ": fewer than 2 variable species."
    )
    
    return(NULL)
  }
  
  
  # ==========================================================
  # log2 transformation
  # ==========================================================
  
  assemblage_log2 <- vegan::decostand(
    assemblage,
    method = "log",
    logbase = 2
  )
  
  
  # ==========================================================
  # Modified Gower distance
  # ==========================================================
  
  dist_modgower <- vegan::vegdist(
    assemblage_log2,
    method = "altGower"
  )
  
  
  # ==========================================================
  # Order observations for trajectories
  # ==========================================================
  
  ord <- order(
    m1_meta$site_name,
    m1_meta$survey_date
  )
  
  
  m1_meta_traj <- m1_meta[
    ord,
    ,
    drop = FALSE
  ]
  
  
  # ==========================================================
  # Reorder distance matrix
  # ==========================================================
  
  dmat <- as.matrix(
    dist_modgower
  )
  
  
  dmat <- dmat[
    m1_meta_traj$id,
    m1_meta_traj$id
  ]
  
  
  dist_traj <- as.dist(
    dmat
  )
  
  
  stopifnot(
    identical(
      attr(
        dist_traj,
        "Labels"
      ),
      m1_meta_traj$id
    )
  )
  
  
  # ==========================================================
  # PCoA
  # ==========================================================
  
  pcoa <- cmdscale(
    dist_traj,
    k = 2,
    eig = TRUE,
    add = TRUE
  )
  
  
  pcoa_scores <- as.data.frame(
    pcoa$points
  )
  
  
  names(
    pcoa_scores
  ) <- c(
    "PCoA1",
    "PCoA2"
  )
  
  
  pcoa_scores$id <- rownames(
    pcoa_scores
  )
  
  
  pcoa_scores <- pcoa_scores %>%
    
    left_join(
      m1_meta_traj,
      by = "id"
    ) %>%
    
    arrange(
      site_name,
      survey_date
    )
  
  
  # ==========================================================
  # Percentage variation explained
  # ==========================================================
  
  eig <- pcoa$eig
  
  eig_positive <- eig[
    eig > 0
  ]
  
  
  PCoA1_percent <- round(
    100 *
      eig_positive[1] /
      sum(eig_positive),
    1
  )
  
  
  PCoA2_percent <- round(
    100 *
      eig_positive[2] /
      sum(eig_positive),
    1
  )
  
  
  # ==========================================================
  # Base trajectory plot
  # ==========================================================
  
  trajectory_plot <- ggplot(
    pcoa_scores,
    aes(
      x = PCoA1,
      y = PCoA2,
      group = site_name,
      colour = site_name
    )
  ) +
    
    geom_path(
      arrow = arrow(
        length = unit(
          0.15,
          "cm"
        ),
        type = "closed"
      ),
      linewidth = 0.8
    ) +
    
    geom_point(
      size = 3
    ) +
    
    geom_text(
      aes(
        label = str_sub(
          sampling_event_start_date,
          1,
          7
        )
      ),
      vjust = -1,
      size = 3,
      show.legend = FALSE
    ) +
    
    theme_classic() +
    
    labs(
      title = location_name,
      
      x = paste0(
        "PCoA1 (",
        PCoA1_percent,
        "%)"
      ),
      
      y = paste0(
        "PCoA2 (",
        PCoA2_percent,
        "%)"
      ),
      
      colour = "Site"
    )
  
  
  # ==========================================================
  # Species vectors
  # ==========================================================
  
  assemblage_log2_traj <- assemblage_log2[
    m1_meta_traj$id,
    ,
    drop = FALSE
  ]
  
  
  stopifnot(
    identical(
      rownames(
        assemblage_log2_traj
      ),
      rownames(
        pcoa$points
      )
    )
  )
  
  
  # Make permutation results reproducible
  set.seed(123)
  
  
  species_fit <- vegan::envfit(
    pcoa$points ~ .,
    data = as.data.frame(
      assemblage_log2_traj
    ),
    permutations = 999
  )
  
  
  species_vec <- as.data.frame(
    scores(
      species_fit,
      display = "vectors"
    )
  )
  
  
  names(
    species_vec
  )[1:2] <- c(
    "PCoA1",
    "PCoA2"
  )
  
  
  species_vec$scientific <- rownames(
    species_vec
  )
  
  species_vec$r2 <- species_fit$vectors$r
  
  species_vec$pval <- species_fit$vectors$pvals
  
  
  # ----------------------------------------------------------
  # Top 10 species
  # ----------------------------------------------------------
  
  species_vec_top <- species_vec %>%
    
    # If you decide to require significance, uncomment:
    # filter(pval <= 0.05) %>%
    
    filter(
      is.finite(PCoA1),
      is.finite(PCoA2),
      is.finite(r2)
    ) %>%
    
    arrange(
      desc(r2)
    ) %>%
    
    slice_head(
      n = 10
    )
  
  
  if (nrow(species_vec_top) == 0) {
    
    message(
      "Skipping ",
      location_name,
      ": no usable species vectors."
    )
    
    return(NULL)
  }
  
  
  # ==========================================================
  # Scale vector lengths
  # ==========================================================
  
  x_species_range <- diff(
    range(
      species_vec_top$PCoA1,
      na.rm = TRUE
    )
  )
  
  y_species_range <- diff(
    range(
      species_vec_top$PCoA2,
      na.rm = TRUE
    )
  )
  
  
  # Protect against zero-range vectors
  if (x_species_range == 0) {
    x_species_range <- 1
  }
  
  if (y_species_range == 0) {
    y_species_range <- 1
  }
  
  
  arrow_mult <- min(
    
    diff(
      range(
        pcoa_scores$PCoA1,
        na.rm = TRUE
      )
    ) /
      x_species_range,
    
    diff(
      range(
        pcoa_scores$PCoA2,
        na.rm = TRUE
      )
    ) /
      y_species_range
    
  ) * 0.35
  
  
  species_vec_top <- species_vec_top %>%
    
    mutate(
      xend = PCoA1 * arrow_mult,
      
      yend = PCoA2 * arrow_mult,
      
      genus_species = str_extract(
        scientific,
        "[A-Z][a-z]+\\s+[a-z]+$"
      ),
      
      label = str_replace(
        genus_species,
        "^([A-Z])[a-z]+\\s+([a-z]+)$",
        "\\1. \\2"
      )
    )
  
  
  # ==========================================================
  # FINAL PLOT
  # ==========================================================
  
  trajectory_species_arrows <- trajectory_plot +
    
    geom_segment(
      data = species_vec_top,
      
      aes(
        x = 0,
        y = 0,
        xend = xend,
        yend = yend
      ),
      
      inherit.aes = FALSE,
      
      colour = "black",
      
      linewidth = 0.5,
      
      arrow = arrow(
        length = unit(
          0.12,
          "cm"
        )
      )
    ) +
    
    ggrepel::geom_text_repel(
      data = species_vec_top,
      
      aes(
        x = xend,
        y = yend,
        label = label
      ),
      
      inherit.aes = FALSE,
      
      colour = "black",
      
      size = 3,
      
      show.legend = FALSE
    )
  
  
  # ==========================================================
  # Save plot
  # ==========================================================
  
  safe_location_name <- location_name %>%
    
    str_replace_all(
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    
    str_remove(
      "_$"
    )
  
  
  output_file <- file.path(
    output_dir,
    paste0(
      "M1_trajectory_",
      safe_location_name,
      ".png"
    )
  )
  
  
  ggsave(
    filename = output_file,
    plot = trajectory_species_arrows,
    width = 12,
    height = 9,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  
  message(
    "Saved: ",
    output_file
  )
  
  
  # Return plot invisibly in case you want to inspect it
  invisible(
    trajectory_species_arrows
  )
}


# ============================================================
# 4. Get every location_g
# ============================================================

locations <- m1_raw %>%
  
  filter(
    !is.na(location_g)
  ) %>%
  
  distinct(
    location_g
  ) %>%
  
  arrange(
    location_g
  ) %>%
  
  pull(
    location_g
  )


locations


# ============================================================
# 5. LOOP OVER ALL LOCATIONS
#
# tryCatch means one problematic location will not stop all
# the remaining plots from being created.
# ============================================================

for (current_location in locations) {
  
  tryCatch(
    
    make_trajectory_plot(
      current_location
    ),
    
    error = function(e) {
      
      message(
        "ERROR for ",
        current_location,
        ": ",
        conditionMessage(e)
      )
    }
  )
}