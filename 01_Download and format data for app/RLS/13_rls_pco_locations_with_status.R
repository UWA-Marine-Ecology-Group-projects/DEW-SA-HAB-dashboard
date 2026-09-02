library(dplyr)
library(tidyr)
library(vegan)
library(ecotraj)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tibble)
library(stringr)
library(readr)

# ============================================================
# 1. Read data ONCE
# ============================================================

sa_sites <- read_rds("data/tidy/sa_sites.rds") # Made in script 2

m1_raw <- readRDS("data/tidy/rls_m1_complete_count.rds") %>%
  left_join(sa_sites)

# ============================================================
# 2. Output folder
# ============================================================

output_dir <- "outputs/pco_new"

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
    location == location_name
  ) %>%
    
    # --------------------------------------------------------
  # STEP 1:
  # Average blocks within survey
  # --------------------------------------------------------
  group_by(
    period,
    status,
    transect,
    sampling_event_start_date,
    # survey_id,
    # survey_date,
    site_name,
    sampling_event,
    # depth,
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
    status,
    # transect,
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
    
    # filter(
    #   total_site > 1
    # ) %>%
    
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
      # transect,
      site_name,
      sampling_event,
      sampling_event_start_date,
      period,
      status
    ) %>%
    
    mutate(
      survey_date = as.Date(
        sampling_event_start_date
      ),
      
      Year = lubridate::year(
        survey_date
      ),
      
      period = factor(period),
      
      status = factor(status),
      
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
  # Base trajectory plot: path shows site x colour, points show
  # Status (shape) and Period (fill) - same encoding as the
  # status PCO plot, so the two plot types read consistently.
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
      aes(shape = status, fill = period),
      size = 3, stroke = 1
    ) +
    
    # pch 21/24 are fillable (circle/triangle) so period can use the
    # interior. Confirm exact labels via levels(pcoa_scores$status)
    # and levels(pcoa_scores$period), then adjust below if they differ.
    scale_shape_manual(
      values = c(
        "Fished" = 21,
        "No-take" = 24
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "Pre-bloom" = "white",
        "Bloom" = "black"
      )
    ) +
    
    # Without this, the Status legend key inherits whatever fill colour
    # happens to be mapped first, which looks broken - force it neutral.
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
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
      
      colour = "Site",
      shape = "Status",
      fill = "Period"
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
  
  # ==========================================================
  # PCoA2 THROUGH TIME
  #
  # scale_y_reverse() places negative PCoA2 values at the top
  # while retaining the original PCoA2 values on the axis.
  # ==========================================================
  
  pcoa2_time_plot <- ggplot(
    pcoa_scores,
    aes(
      x = survey_date,
      y = PCoA2,
      group = site_name,
      colour = site_name
    )
  ) +
    
    # Connect sampling events within each site through time
    geom_line(
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    
    geom_point(
      aes(shape = status, fill = period),
      size = 3, stroke = 1,
      na.rm = TRUE
    ) +
    
    # pch 21/24 are fillable (circle/triangle) so period can use the
    # interior. Confirm exact labels via levels(pcoa_scores$status)
    # and levels(pcoa_scores$period), then adjust below if they differ.
    scale_shape_manual(
      values = c(
        "Fished" = 21,
        "No-take" = 24
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "Pre-bloom" = "white",
        "Bloom" = "black"
      )
    ) +
    
    # Without this, the Status legend key inherits whatever fill colour
    # happens to be mapped first, which looks broken - force it neutral.
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
    ) +
    
    # Reference line for zero
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey60"
    ) +
    
    # Reverse the PCoA2 axis so negative values are at the top
    scale_y_reverse() +
    
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(
        mult = c(0.02, 0.04)
      )
    ) +
    
    labs(
      title = location_name,
      
      x = "Sampling date",
      
      y = paste0(
        "PCoA2 (",
        PCoA2_percent,
        "%; reversed)"
      ),
      
      colour = "Site",
      shape = "Status",
      fill = "Period"
    ) +
    
    theme_classic() +
    
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  
  # ==========================================================
  # SAVE PCoA2 TIME PLOT
  # ==========================================================
  
  pcoa2_time_file <- file.path(
    output_dir,
    paste0(
      "M1_PCoA2_through_time_",
      safe_location_name,
      ".png"
    )
  )
  
  
  ggsave(
    filename = pcoa2_time_file,
    plot = pcoa2_time_plot,
    width = 12,
    height = 7,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  
  message(
    "Saved: ",
    pcoa2_time_file
  )
  
  
  # output_file <- file.path(
  #   output_dir,
  #   paste0(
  #     "M1_trajectory_",
  #     safe_location_name,
  #     ".png"
  #   )
  # )
  # 
  # 
  # ggsave(
  #   filename = output_file,
  #   plot = trajectory_species_arrows,
  #   width = 12,
  #   height = 9,
  #   units = "in",
  #   dpi = 300,
  #   bg = "white"
  # )
  # 
  
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
# Function: PCO with Status symbols + species vectors,
# for one location. Dots = site x sampling-event centroids.
# No trajectory paths - this is a single ordination snapshot,
# not a time series.
# ============================================================

make_status_pco_plot <- function(location_name) {
  
  message("Running status PCO: ", location_name)
  
  
  # ==========================================================
  # CREATE SITE/EVENT SPECIES ABUNDANCE (status carried through
  # as a grouping variable, exactly like site_name - it's a
  # per-site constant so it doesn't affect the mean() calcs)
  # ==========================================================
  
  m1_complete_count <- m1_raw %>%
    
    filter(
      location == location_name
    ) %>%
    
    group_by(
      period, sampling_event_start_date, survey_id, survey_date,
      site_name, status, sampling_event, depth,
      family, genus, species, scientific
    ) %>%
    
    summarise(
      total_block = mean(total),
      .groups = "drop"
    ) %>%
    
    group_by(
      period, sampling_event_start_date, site_name, status,
      sampling_event, family, genus, species, scientific
    ) %>%
    
    summarise(
      total_site = mean(total_block),
      .groups = "drop"
    ) %>%
    
    # filter(
    #   total_site > 1
    # ) %>%
    
    mutate(
      id = paste(site_name, sampling_event, sep = "_")
    ) %>%
    
    ungroup()
  
  
  # ==========================================================
  # Metadata
  # ==========================================================
  
  m1_meta <- m1_complete_count %>%
    
    distinct(
      id, site_name, status, sampling_event,
      sampling_event_start_date, period
    ) %>%
    
    mutate(
      survey_date = as.Date(sampling_event_start_date),
      Year = lubridate::year(survey_date),
      period = factor(period),
      year = factor(Year),
      status = factor(status)
    ) %>%
    
    arrange(
      site_name, survey_date
    )
  
  
  if (nrow(m1_meta) < 3) {
    message("Skipping ", location_name, ": fewer than 3 sampling events.")
    return(NULL)
  }
  
  if (any(is.na(m1_meta$status))) {
    message(
      "Note: ", sum(is.na(m1_meta$status)), " sampling event(s) in ",
      location_name, " have no matching Status - check the join to sa_sites."
    )
  }
  
  stopifnot(!anyDuplicated(m1_meta$id))
  
  
  # ==========================================================
  # Sample x species matrix
  # ==========================================================
  
  m1_assemblage <- m1_complete_count %>%
    
    select(
      id, scientific, total_site
    ) %>%
    
    pivot_wider(
      names_from = scientific,
      values_from = total_site,
      values_fill = 0
    ) %>%
    
    arrange(
      match(id, m1_meta$id)
    )
  
  
  assemblage <- m1_assemblage %>%
    column_to_rownames("id") %>%
    as.data.frame()
  
  
  stopifnot(
    identical(rownames(assemblage), m1_meta$id)
  )
  
  
  assemblage <- assemblage[
    ,
    vapply(assemblage, function(x) length(unique(x)) > 1, logical(1)),
    drop = FALSE
  ]
  
  
  if (ncol(assemblage) < 2) {
    message("Skipping ", location_name, ": fewer than 2 variable species.")
    return(NULL)
  }
  
  
  # ==========================================================
  # log2 transformation + modified Gower distance
  # (identical to the trajectory function, so the two analyses
  # are directly comparable)
  # ==========================================================
  
  assemblage_log2 <- vegan::decostand(
    assemblage, method = "log", logbase = 2
  )
  
  dist_modgower <- vegan::vegdist(
    assemblage_log2, method = "altGower"
  )
  
  
  # ==========================================================
  # Metric PCO
  # (no reordering needed here - we're not drawing paths,
  # so sample order doesn't matter for cmdscale/envfit)
  # ==========================================================
  
  pcoa <- cmdscale(
    dist_modgower, k = 2, eig = TRUE, add = TRUE
  )
  
  pcoa_scores <- as.data.frame(pcoa$points)
  names(pcoa_scores) <- c("PCoA1", "PCoA2")
  pcoa_scores$id <- rownames(pcoa_scores)
  
  pcoa_scores <- pcoa_scores %>%
    left_join(m1_meta, by = "id")
  
  
  eig <- pcoa$eig
  eig_positive <- eig[eig > 0]
  
  PCoA1_percent <- round(100 * eig_positive[1] / sum(eig_positive), 1)
  PCoA2_percent <- round(100 * eig_positive[2] / sum(eig_positive), 1)
  
  # ==========================================================
  # Build consecutive-point segments per site, tagged by Period
  # (same approach as make_trajectory_plot - a segment is styled
  # by its STARTING point's period; swap `period` for `lead(period)`
  # below if you'd rather style by the ending point instead)
  # ==========================================================
  
  trajectory_segments <- pcoa_scores %>%
    
    arrange(
      site_name, survey_date
    ) %>%
    
    group_by(
      site_name
    ) %>%
    
    mutate(
      PCoA1_end = lead(PCoA1),
      PCoA2_end = lead(PCoA2)
    ) %>%
    
    ungroup() %>%
    
    filter(
      !is.na(PCoA1_end)
    )
  
  
  # ==========================================================
  # Base PCO plot: trajectories through time (colour = Site),
  # points encode Status (shape) and Period (fill) independently
  # so a mixed-period segment is never ambiguous - each point
  # shows its own period regardless of which way the line runs.
  # ==========================================================
  
  status_pco_plot <- ggplot(
    pcoa_scores,
    aes(x = PCoA1, y = PCoA2, colour = site_name)
  ) +
    
    geom_segment(
      data = trajectory_segments,
      aes(
        x = PCoA1,
        y = PCoA2,
        xend = PCoA1_end,
        yend = PCoA2_end,
        colour = site_name
      ),
      inherit.aes = FALSE,
      arrow = arrow(
        length = unit(0.15, "cm"),
        type = "closed"
      ),
      linewidth = 0.8
    ) +
    
    geom_point(
      aes(shape = status, fill = period),
      size = 3, stroke = 1
    ) +
    
    # pch 21/24 are fillable (circle/triangle) so period can use the
    # interior. Confirm exact labels via levels(pcoa_scores$status)
    # and levels(pcoa_scores$period), then adjust below if they differ.
    scale_shape_manual(
      values = c(
        "Fished" = 21,
        "No-take" = 24
      )
    ) +
    
    scale_fill_manual(
      values = c(
        "Pre-bloom" = "white",
        "Bloom" = "black"
      )
    ) +
    
    # Without this, the Status legend key inherits whatever fill colour
    # happens to be mapped first, which looks broken - force it neutral.
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
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
      x = paste0("PCoA1 (", PCoA1_percent, "%)"),
      y = paste0("PCoA2 (", PCoA2_percent, "%)"),
      colour = "Site",
      shape = "Status",
      fill = "Period"
    )
  

  
  # ==========================================================
  # Species vectors
  # ==========================================================
  
  set.seed(123)
  
  species_fit <- vegan::envfit(
    pcoa$points ~ .,
    data = as.data.frame(assemblage_log2),
    permutations = 999
  )
  
  species_vec <- as.data.frame(
    scores(species_fit, display = "vectors")
  )
  
  names(species_vec)[1:2] <- c("PCoA1", "PCoA2")
  species_vec$scientific <- rownames(species_vec)
  species_vec$r2 <- species_fit$vectors$r
  species_vec$pval <- species_fit$vectors$pvals
  
  species_vec_top <- species_vec %>%
    filter(
      is.finite(PCoA1), is.finite(PCoA2), is.finite(r2)
    ) %>%
    arrange(desc(r2)) %>%
    slice_head(n = 10)
  
  if (nrow(species_vec_top) == 0) {
    message("Skipping ", location_name, ": no usable species vectors.")
    return(NULL)
  }
  
  
  x_species_range <- diff(range(species_vec_top$PCoA1, na.rm = TRUE))
  y_species_range <- diff(range(species_vec_top$PCoA2, na.rm = TRUE))
  
  if (x_species_range == 0) x_species_range <- 1
  if (y_species_range == 0) y_species_range <- 1
  
  arrow_mult <- min(
    diff(range(pcoa_scores$PCoA1, na.rm = TRUE)) / x_species_range,
    diff(range(pcoa_scores$PCoA2, na.rm = TRUE)) / y_species_range
  ) * 0.35
  
  species_vec_top <- species_vec_top %>%
    mutate(
      xend = PCoA1 * arrow_mult,
      yend = PCoA2 * arrow_mult,
      genus_species = str_extract(scientific, "[A-Z][a-z]+\\s+[a-z]+$"),
      label = str_replace(
        genus_species, "^([A-Z])[a-z]+\\s+([a-z]+)$", "\\1. \\2"
      )
    )
  
  
  # ==========================================================
  # Final plot: points + species vectors
  # ==========================================================
  
  status_pco_species <- status_pco_plot +
    
    geom_segment(
      data = species_vec_top,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.5,
      arrow = arrow(length = unit(0.12, "cm"))
    ) +
    
    ggrepel::geom_text_repel(
      data = species_vec_top,
      aes(x = xend, y = yend, label = label),
      inherit.aes = FALSE,
      colour = "black",
      size = 3,
      show.legend = FALSE
    )
  
  
  # ==========================================================
  # Save
  # ==========================================================
  
  safe_location_name <- location_name %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_remove("_$")
  
  output_file <- file.path(
    output_dir,
    paste0("M1_status_PCO_", safe_location_name, ".png")
  )
  
  ggsave(
    filename = output_file,
    plot = status_pco_species,
    width = 12, height = 9, units = "in", dpi = 300, bg = "white"
  )
  
  message("Saved: ", output_file)
  
  invisible(status_pco_species)
}


# ============================================================
# 4. Get every location
# ============================================================

locations <- m1_raw %>%
  
  filter(
    !is.na(location)
  ) %>%
  
  distinct(
    location
  ) %>%
  
  arrange(
    location
  ) %>%
  
  pull(
    location
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

# ============================================================
# LOOP OVER ALL LOCATIONS - Status PCO plots
# ============================================================

for (current_location in locations) {
  
  tryCatch(
    
    make_status_pco_plot(current_location),
    
    error = function(e) {
      message("ERROR for ", current_location, ": ", conditionMessage(e))
    }
  )
}

# ============================================================
# 6. Test for a Period effect (PERMANOVA), blocked by Site
# ============================================================
#
# site_name is included as a model term (removed first, via
# sequential/Type I sums of squares) so period is tested on the
# variance left AFTER accounting for baseline site differences -
# this mirrors what Condition(site_name) does in the CAP model
# below, keeping the two analyses conceptually aligned.
#
# `strata` additionally restricts permutations to within each
# site, since repeated sampling events at one site are not
# independent - without this, the test would overstate its power.
#
# betadisper checks a key PERMANOVA assumption: that within-group
# multivariate dispersion is similar between Bloom and Pre-bloom.
# A significant betadisper result means a significant PERMANOVA
# could partly reflect a dispersion difference, not just a
# location-shift between groups - worth reporting alongside the
# main test rather than ignoring.
# ============================================================

test_period_effect <- function(location_name) {
  
  message("Testing period effect: ", location_name)
  
  m1_complete_count <- m1_raw %>%
    
    filter(location == location_name) %>%
    
    group_by(
      period, status, transect, sampling_event_start_date,
      site_name, sampling_event, family, genus, species, scientific
    ) %>%
    
    summarise(total_block = mean(total), .groups = "drop") %>%
    
    group_by(
      period, status, sampling_event_start_date, site_name,
      sampling_event, family, genus, species, scientific
    ) %>%
    
    summarise(total_site = mean(total_block), .groups = "drop") %>%
    
    mutate(id = paste(site_name, sampling_event, sep = "_")) %>%
    
    ungroup()
  
  
  m1_meta <- m1_complete_count %>%
    
    distinct(id, site_name, status, sampling_event, sampling_event_start_date, period) %>%
    
    mutate(
      survey_date = as.Date(sampling_event_start_date),
      period = factor(period),
      status = factor(status)
    ) %>%
    
    arrange(site_name, survey_date)
  
  
  if (nrow(m1_meta) < 4) {
    message("Skipping ", location_name, ": too few sampling events for PERMANOVA.")
    return(NULL)
  }
  
  # A period effect can't be tested if only one period is present
  if (length(unique(m1_meta$period)) < 2) {
    message("Skipping ", location_name, ": only one Period present.")
    return(NULL)
  }
  
  # Warn (don't skip) if any site never saw both periods - the
  # strata-restricted permutation below still runs, but that
  # site contributes no information to the test.
  site_period_counts <- m1_meta %>%
    distinct(site_name, period) %>%
    count(site_name, name = "n_periods")
  
  if (any(site_period_counts$n_periods < 2)) {
    message(
      "Note: ", sum(site_period_counts$n_periods < 2),
      " site(s) in ", location_name,
      " only have one Period - they contribute less to this test."
    )
  }
  
  
  m1_assemblage <- m1_complete_count %>%
    select(id, scientific, total_site) %>%
    pivot_wider(names_from = scientific, values_from = total_site, values_fill = 0) %>%
    arrange(match(id, m1_meta$id))
  
  assemblage <- m1_assemblage %>%
    column_to_rownames("id") %>%
    as.data.frame()
  
  stopifnot(identical(rownames(assemblage), m1_meta$id))
  
  assemblage <- assemblage[
    , vapply(assemblage, function(x) length(unique(x)) > 1, logical(1)), drop = FALSE
  ]
  
  if (ncol(assemblage) < 2) {
    message("Skipping ", location_name, ": fewer than 2 variable species.")
    return(NULL)
  }
  
  assemblage_log2 <- vegan::decostand(assemblage, method = "log", logbase = 2)
  dist_modgower <- vegan::vegdist(assemblage_log2, method = "altGower")
  
  
  # ----------------------------------------------------------
  # Dispersion check
  # (grouped by period only - a full site-partialled dispersion
  # test isn't straightforward in vegan, so treat this as a
  # general sanity check rather than a perfectly matched test)
  # ----------------------------------------------------------
  
  disp <- vegan::betadisper(dist_modgower, m1_meta$period)
  disp_test <- vegan::permutest(disp, permutations = 999)
  disp_p <- disp_test$tab$`Pr(>F)`[1]
  
  
  # ----------------------------------------------------------
  # PERMANOVA: site_name partialled out first, period tested
  # on the remainder, permutations restricted within site
  # ----------------------------------------------------------
  
  set.seed(123)
  
  permanova <- vegan::adonis2(
    dist_modgower ~ site_name + period,
    data = m1_meta,
    permutations = 999,
    strata = m1_meta$site_name,
    by = "terms"
  )
  
  period_row <- permanova["period", ]
  
  tibble(
    location = location_name,
    n = nrow(m1_meta),
    n_sites = n_distinct(m1_meta$site_name),
    R2 = period_row$R2,
    F_value = period_row$F,
    p_value = period_row$`Pr(>F)`,
    dispersion_p = disp_p
  )
}

permanova_list <- list()

for (current_location in locations) {
  
  result <- tryCatch(
    test_period_effect(current_location),
    error = function(e) {
      message("ERROR for ", current_location, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(result)) {
    permanova_list[[current_location]] <- result
  }
}

permanova_results <- bind_rows(permanova_list)

permanova_results

write.csv(
  permanova_results,
  file = file.path(output_dir, "period_PERMANOVA_results.csv"),
  row.names = FALSE
)

# ============================================================
# 7. Constrained ordination (CAP) for locations with a
# significant Period effect
# ============================================================
#
# capscale() takes the species matrix directly (not a
# precomputed dist object) so species scores come for free via
# weighted averaging - consistent with how species vectors are
# shown in your other plots. distance = "altGower" reproduces
# the same distance used everywhere else in this script.
#
# Condition(site_name) partials out among-site variation before
# fitting/testing Period, matching the site_name-first term in
# the PERMANOVA above.
# ============================================================

make_cap_plot <- function(location_name) {
  
  message("Running CAP: ", location_name)
  
  m1_complete_count <- m1_raw %>%
    
    filter(location == location_name) %>%
    
    group_by(
      period, status, transect, sampling_event_start_date,
      site_name, sampling_event, family, genus, species, scientific
    ) %>%
    
    summarise(total_block = mean(total), .groups = "drop") %>%
    
    group_by(
      period, status, sampling_event_start_date, site_name,
      sampling_event, family, genus, species, scientific
    ) %>%
    
    summarise(total_site = mean(total_block), .groups = "drop") %>%
    
    mutate(id = paste(site_name, sampling_event, sep = "_")) %>%
    
    ungroup()
  
  
  m1_meta <- m1_complete_count %>%
    
    distinct(id, site_name, status, sampling_event, sampling_event_start_date, period) %>%
    
    mutate(
      survey_date = as.Date(sampling_event_start_date),
      period = factor(period),
      status = factor(status)
    ) %>%
    
    arrange(site_name, survey_date)
  
  
  # Need enough residual degrees of freedom for a sensible CAP:
  # roughly (n obs) > (n sites) + (period df) + (intercept)
  if (nrow(m1_meta) <= n_distinct(m1_meta$site_name) + 2) {
    message("Skipping CAP for ", location_name, ": not enough residual d.f.")
    return(NULL)
  }
  
  
  m1_assemblage <- m1_complete_count %>%
    select(id, scientific, total_site) %>%
    pivot_wider(names_from = scientific, values_from = total_site, values_fill = 0) %>%
    arrange(match(id, m1_meta$id))
  
  assemblage <- m1_assemblage %>%
    column_to_rownames("id") %>%
    as.data.frame()
  
  stopifnot(identical(rownames(assemblage), m1_meta$id))
  
  assemblage <- assemblage[
    , vapply(assemblage, function(x) length(unique(x)) > 1, logical(1)), drop = FALSE
  ]
  
  assemblage_log2 <- vegan::decostand(assemblage, method = "log", logbase = 2)
  
  
  # ----------------------------------------------------------
  # Constrained ordination + permutation test
  # (strata matches the PERMANOVA's within-site restriction)
  # ----------------------------------------------------------
  
  cap_model <- vegan::capscale(
    assemblage_log2 ~ period + Condition(site_name),
    data = m1_meta,
    distance = "altGower",
    add = TRUE
  )
  
  set.seed(123)
  
  cap_test <- anova(
    cap_model,
    permutations = 999,
    strata = m1_meta$site_name
  )
  
  cap_p <- cap_test$`Pr(>F)`[1]
  
  message("  CAP permutation test p = ", cap_p)
  
  
  # ----------------------------------------------------------
  # Axis scores: CAP1 (constrained, the Period axis) vs MDS1
  # (first residual axis) - see note above on why there's only
  # one constrained axis with a 2-level predictor.
  # ----------------------------------------------------------
  
  all_scores <- as.data.frame(vegan::scores(cap_model, display = "sites"))
  
  if (!all(c("CAP1", "MDS1") %in% names(all_scores))) {
    message(
      "Skipping CAP plot for ", location_name,
      ": expected CAP1/MDS1 axes not found - inspect cap_model manually."
    )
    return(NULL)
  }
  
  cap_scores <- all_scores %>%
    select(CAP1, MDS1) %>%
    mutate(id = rownames(all_scores)) %>%
    left_join(m1_meta, by = "id")
  
  
  # ----------------------------------------------------------
  # Species scores (weighted-average scores on the same axes)
  # ----------------------------------------------------------
  
  species_scores <- as.data.frame(vegan::scores(cap_model, display = "species"))
  
  species_vec_top <- NULL
  
  if (all(c("CAP1", "MDS1") %in% names(species_scores))) {
    species_vec_top <- species_scores %>%
      select(CAP1, MDS1) %>%
      mutate(
        scientific = rownames(species_scores),
        vector_length = sqrt(CAP1^2 + MDS1^2)
      ) %>%
      arrange(desc(vector_length)) %>%
      slice_head(n = 10)
  }
  
  
  # Percent variance is relative to TOTAL inertia (including the
  # portion already assigned to Condition(site_name)), so these
  # numbers will look smaller than the plain PCoA plots' - that's
  # expected, not an error.
  cap_percent <- round(100 * cap_model$CCA$eig[1] / cap_model$tot.chi, 1)
  mds_percent <- round(100 * cap_model$CA$eig[1] / cap_model$tot.chi, 1)
  
  safe_location_name <- location_name %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_remove("_$")
  
  # ==========================================================
  # CAP1 THROUGH TIME
  #
  # Same layout as pcoa2_time_plot, but the y-axis is now the
  # CONSTRAINED (Period) axis rather than an unconstrained PCoA
  # axis. Since CAP1 is literally the axis built to separate
  # Bloom from Pre-bloom, this shows how far along that
  # separation each site sits over time - if the PERMANOVA/CAP
  # test was significant, Bloom and Pre-bloom points should
  # cluster at different CAP1 values.
  # ==========================================================
  
  cap1_time_plot <- ggplot(
    cap_scores,
    aes(
      x = survey_date,
      y = CAP1,
      group = site_name,
      colour = site_name
    )
  ) +
    
    geom_line(
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    
    geom_point(
      aes(shape = status, fill = period),
      size = 3, stroke = 1,
      na.rm = TRUE
    ) +
    
    scale_shape_manual(values = c("Fished" = 21, "No-take" = 24)) +
    
    scale_fill_manual(values = c("Pre-bloom" = "white", "Bloom" = "black")) +
    
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
    ) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey60"
    ) +
    
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.02, 0.04))
    ) +
    
    labs(
      title = location_name,
      x = "Sampling date",
      y = paste0("CAP1 - Period axis (", cap_percent, "%)"),
      colour = "Site",
      shape = "Status",
      fill = "Period"
    ) +
    
    theme_classic() +
    
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  
  cap1_time_file <- file.path(
    output_dir,
    paste0("M1_CAP1_through_time_", safe_location_name, ".png")
  )
  
  ggsave(
    filename = cap1_time_file,
    plot = cap1_time_plot,
    width = 12, height = 7, units = "in", dpi = 300, bg = "white"
  )
  
  message("Saved: ", cap1_time_file)
  
  
  cap_plot <- ggplot(
    cap_scores,
    aes(x = CAP1, y = MDS1, colour = site_name)
  ) +
    
    geom_point(
      aes(shape = status, fill = period),
      size = 3, stroke = 1
    ) +
    
    scale_shape_manual(values = c("Fished" = 21, "No-take" = 24)) +
    
    scale_fill_manual(values = c("Pre-bloom" = "white", "Bloom" = "black")) +
    
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
    ) +
    
    theme_classic() +
    
    labs(
      title = paste0(location_name, " (CAP, p = ", signif(cap_p, 3), ")"),
      x = paste0("CAP1 - Period axis (", cap_percent, "%)"),
      y = paste0("MDS1 - residual axis (", mds_percent, "%)"),
      colour = "Site",
      shape = "Status",
      fill = "Period"
    )
  
  
  if (!is.null(species_vec_top) && nrow(species_vec_top) > 0) {
    
    x_range <- diff(range(species_vec_top$CAP1, na.rm = TRUE))
    y_range <- diff(range(species_vec_top$MDS1, na.rm = TRUE))
    if (x_range == 0) x_range <- 1
    if (y_range == 0) y_range <- 1
    
    arrow_mult <- min(
      diff(range(cap_scores$CAP1, na.rm = TRUE)) / x_range,
      diff(range(cap_scores$MDS1, na.rm = TRUE)) / y_range
    ) * 0.35
    
    species_vec_top <- species_vec_top %>%
      mutate(
        xend = CAP1 * arrow_mult,
        yend = MDS1 * arrow_mult,
        genus_species = str_extract(scientific, "[A-Z][a-z]+\\s+[a-z]+$"),
        label = str_replace(
          genus_species, "^([A-Z])[a-z]+\\s+([a-z]+)$", "\\1. \\2"
        )
      )
    
    cap_plot <- cap_plot +
      
      geom_segment(
        data = species_vec_top,
        aes(x = 0, y = 0, xend = xend, yend = yend),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.5,
        arrow = arrow(length = unit(0.12, "cm"))
      ) +
      
      ggrepel::geom_text_repel(
        data = species_vec_top,
        aes(x = xend, y = yend, label = label),
        inherit.aes = FALSE,
        colour = "black",
        size = 3,
        show.legend = FALSE
      )
  }

  
  output_file <- file.path(output_dir, paste0("M1_CAP_", safe_location_name, ".png"))
  
  ggsave(
    filename = output_file,
    plot = cap_plot,
    width = 12, height = 9, units = "in", dpi = 300, bg = "white"
  )
  
  message("Saved: ", output_file)
  
  invisible(cap_plot)
}

# ============================================================
# 8. CAP for significant locations only
# ============================================================

significant_locations <- permanova_results %>%
  filter(p_value <= 0.05) %>%
  pull(location)

significant_locations

for (current_location in significant_locations) {
  
  tryCatch(
    
    make_cap_plot(current_location),
    
    error = function(e) {
      message("ERROR for ", current_location, ": ", conditionMessage(e))
    }
  )
}
