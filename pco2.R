# ============================================================
# M1 ASSEMBLAGE TRAJECTORY ANALYSIS
#
# - Average blocks first
# - Average surveys/events within site
# - log2 transformation
# - Modified Gower distance (altGower)
# - PERMANOVA
# - PCoA trajectories
# - Site-coloured trajectories
# - Period labels
# - SIMPER: species contributing to period differences
# - Species vectors overlaid on trajectory PCoA
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

library(dplyr)
library(tidyr)
library(vegan)
library(ecotraj)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tibble)


# ============================================================
# 1. Read M1 data
# ============================================================

sl_m1 <- readRDS(
  "data/tidy/rls_m1_survey_list.rds"
) %>%
  mutate(
    id = paste(survey_id, block)
  ) %>%
  filter(
    location %in% "Encounter"
  )


names(sl_m1)


# ============================================================
# 2. Read site metadata
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


# ============================================================
# 3. CREATE SITE/EVENT SPECIES ABUNDANCE
#
# IMPORTANT:
# First average blocks
# THEN average surveys/events within site
#
# This preserves your calculation.
# ============================================================

m1_complete_count <- readRDS(
  "data/tidy/rls_m1_complete_count.rds"
) %>%
  
  # Add site information
  left_join(
    sa_sites
  ) %>%
  
  # ----------------------------------------------------------
# TESTING FILTER
# Remove/change this when running the full analysis
# ----------------------------------------------------------
filter(
  location_g %in% "Metro"
) %>%
  
  # ----------------------------------------------------------
# STEP 1:
# Average blocks within survey
# ----------------------------------------------------------
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
  
  # ----------------------------------------------------------
# STEP 2:
# Average surveys/events within site
# ----------------------------------------------------------
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
  
  # Remove species with essentially no abundance
  filter(
    total_site > 1
  ) %>%
  
  # One sample = site x sampling event
  mutate(
    id = paste(
      site_name,
      sampling_event,
      sep = "_"
    )
  ) %>%
  
  ungroup()


names(m1_complete_count)


# ============================================================
# 4. Metadata for each sample
# ============================================================

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
    )
  ) %>%
  
  arrange(
    site_name,
    survey_date
  )


# Check IDs are unique
stopifnot(
  !anyDuplicated(m1_meta$id)
)


# ============================================================
# 5. Make sample x species matrix
# ============================================================

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
    match(id, m1_meta$id)
  )


# Put IDs into row names
assemblage <- m1_assemblage %>%
  column_to_rownames("id") %>%
  as.data.frame()


# ============================================================
# 6. Check assemblage / metadata alignment
# ============================================================

stopifnot(
  identical(
    rownames(assemblage),
    m1_meta$id
  )
)

str(assemblage)

# Missing values
sum(is.na(assemblage))

# All abundance columns numeric
all(
  vapply(
    assemblage,
    is.numeric,
    logical(1)
  )
)

# Samples with zero total abundance
any(
  rowSums(assemblage) == 0
)


# ============================================================
# 7. log2 transformation
# ============================================================

assemblage_log2 <- vegan::decostand(
  assemblage,
  method = "log",
  logbase = 2
)


# ============================================================
# 8. Modified Gower distance
# ============================================================

dist_modgower <- vegan::vegdist(
  assemblage_log2,
  method = "altGower"
)

dist_modgower


# ============================================================
# 9. PERMANOVA
# ============================================================

m1_meta <- m1_meta %>%
  mutate(
    period = factor(period),
    year = factor(Year)
  )


# Check metadata
m1_meta %>%
  summarise(
    n = n(),
    n_period_NA = sum(is.na(period)),
    n_year_NA = sum(is.na(year))
  )


# PERMANOVA
permanova <- vegan::adonis2(
  dist_modgower ~ period * year,
  data = m1_meta,
  permutations = 999
)

permanova


# ============================================================
# 10. Check PERMANOVA dispersion
#
# This is important because PERMANOVA can detect differences
# in dispersion as well as differences in centroid/location.
# ============================================================

dispersion_period <- vegan::betadisper(
  dist_modgower,
  m1_meta$period
)

anova(dispersion_period)

permutest(
  dispersion_period,
  permutations = 999
)


# ============================================================
# 11. Order observations for trajectories
#
# Site first, then date
# ============================================================

ord <- order(
  m1_meta$site_name,
  m1_meta$survey_date
)

m1_meta_traj <- m1_meta[
  ord,
  ,
  drop = FALSE
]


# ============================================================
# 12. Reorder distance matrix to exactly match trajectory
# ============================================================

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


# Check
stopifnot(
  identical(
    attr(dist_traj, "Labels"),
    m1_meta_traj$id
  )
)


# ============================================================
# 13. ecotraj trajectory object
# ============================================================

x <- ecotraj::defineTrajectories(
  dist_traj,
  sites = m1_meta_traj$site_name,
  surveys = as.integer(
    m1_meta_traj$Year
  )
)


# ============================================================
# 14. PCoA
#
# We calculate the coordinates ourselves so that ggplot gives
# us complete control over the trajectory plot.
# ============================================================

pcoa <- cmdscale(
  dist_traj,
  k = 2,
  eig = TRUE,
  add = TRUE
)


pcoa_scores <- as.data.frame(
  pcoa$points
)

names(pcoa_scores) <- c(
  "PCoA1",
  "PCoA2"
)

pcoa_scores$id <- rownames(
  pcoa_scores
)


# Add metadata
pcoa_scores <- pcoa_scores %>%
  left_join(
    m1_meta_traj,
    by = "id"
  ) %>%
  arrange(
    site_name,
    survey_date
  )


# ============================================================
# 15. Calculate % variance explained
# ============================================================

eig <- pcoa$eig

eig_positive <- eig[
  eig > 0
]

PCoA1_percent <- round(
  100 * eig_positive[1] /
    sum(eig_positive),
  1
)

PCoA2_percent <- round(
  100 * eig_positive[2] /
    sum(eig_positive),
  1
)


# ============================================================
# 16. Calculate midpoint coordinates for period labels
#
# The label is placed halfway along each trajectory segment.
# ============================================================

pcoa_scores <- pcoa_scores %>%
  group_by(
    site_name
  ) %>%
  arrange(
    survey_date,
    .by_group = TRUE
  ) %>%
  mutate(
    next_PCoA1 = lead(PCoA1),
    next_PCoA2 = lead(PCoA2),
    next_period = lead(period),
    next_year = lead(Year),
    
    mid_PCoA1 =
      (PCoA1 + next_PCoA1) / 2,
    
    mid_PCoA2 =
      (PCoA2 + next_PCoA2) / 2
  ) %>%
  ungroup()


# ============================================================
# 17. TRAJECTORY PLOT
#
# Colour = site
# Point = sampling event
# Label = year
# Segment label = period
# ============================================================

trajectory_plot <- ggplot(
  pcoa_scores,
  aes(
    x = PCoA1,
    y = PCoA2,
    group = site_name,
    colour = site_name
  )
) +
  
  # ----------------------------------------------------------
# Trajectory
# ----------------------------------------------------------
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
  
  # ----------------------------------------------------------
# Sampling points
# ----------------------------------------------------------
geom_point(
  size = 3
) +
  
  # ----------------------------------------------------------
# Year labels
# ----------------------------------------------------------
geom_text(
  aes(
    label = str_sub(sampling_event_start_date, 1, 7)
  ),
  vjust = -1,
  size = 3,
  show.legend = FALSE
) +
  
  # ----------------------------------------------------------
# Period labels on trajectory segments
# ----------------------------------------------------------
# geom_text(
#   data = pcoa_scores %>%
#     filter(
#       !is.na(next_period)
#     ),
#   aes(
#     x = mid_PCoA1,
#     y = mid_PCoA2,
#     label = next_period
#   ),
#   inherit.aes = FALSE,
#   size = 3,
#   colour = "black"
# ) +
  
  theme_classic() +
  
  labs(
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


trajectory_plot


# ============================================================
# 18. SIMPER
#
# Identify species contributing to differences BETWEEN PERIODS
# ============================================================

# Make sure period is a factor
m1_meta_traj$period <- factor(
  m1_meta_traj$period
)


# Check periods
levels(
  m1_meta_traj$period
)

table(
  m1_meta_traj$period
)


# ------------------------------------------------------------
# Run SIMPER
#
# IMPORTANT:
# SIMPER is run on the same log2 abundance representation
# used for the community analysis.
# ------------------------------------------------------------

simper_period <- vegan::simper(
  assemblage_log2[
    m1_meta_traj$id,
    ,
    drop = FALSE
  ],
  
  group = m1_meta_traj$period,
  
  permutations = 999
)


# Look at the results
summary(
  simper_period
)


# ============================================================
# 19. Extract SIMPER results into a tidy table
# ============================================================

simper_table <- bind_rows(
  
  lapply(
    names(simper_period),
    
    function(comparison) {
      
      result <- simper_period[[comparison]]
      
      tibble(
        comparison = comparison,
        species = rownames(result),
        average = result$average,
        sd = result$sd,
        ratio = result$ratio,
        ava = result$ava,
        avb = result$avb,
        cumsum = result$cusum,
        p = result$p
      )
    }
  )
)


# ============================================================
# 20. Top species contributing to each period difference
# ============================================================

top_simper <- simper_table %>%
  
  group_by(
    comparison
  ) %>%
  
  arrange(
    desc(average),
    .by_group = TRUE
  ) %>%
  
  slice_head(
    n = 15
  ) %>%
  
  ungroup()


top_simper


# ============================================================
# 21. Species contributing strongly to each comparison
#
# A useful alternative is to select species accounting for the
# first 70% of cumulative dissimilarity.
# ============================================================

simper_70 <- simper_table %>%
  
  group_by(
    comparison
  ) %>%
  
  arrange(
    cumsum,
    .by_group = TRUE
  ) %>%
  
  filter(
    cumsum <= 0.70
  ) %>%
  
  ungroup()


simper_70


# ============================================================
# 22. Save SIMPER results
# ============================================================

write.csv(
  simper_table,
  "m1_simper_period_results.csv",
  row.names = FALSE
)

write.csv(
  top_simper,
  "m1_simper_top_species_by_period.csv",
  row.names = FALSE
)


# ============================================================
# 23. OPTIONAL:
# Identify species with strongest contribution to ALL
# period comparisons
# ============================================================

species_overall <- simper_table %>%
  
  group_by(
    species
  ) %>%
  
  summarise(
    mean_contribution = mean(
      average,
      na.rm = TRUE
    ),
    
    max_contribution = max(
      average,
      na.rm = TRUE
    ),
    
    n_comparisons = n(),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(mean_contribution)
  )


species_overall


# ============================================================
# 24. SPECIES VECTORS
#
# These show species associated with the PCoA directions.
#
# NOTE:
# These are NOT the SIMPER results.
# They show association with ordination direction.
# SIMPER above identifies species contributing to differences
# between periods.
# ============================================================

species_cor <- sapply(
  
  1:ncol(assemblage_log2),
  
  function(i) {
    
    c(
      PCoA1 = cor(
        assemblage_log2[, i],
        pcoa$points[, 1],
        method = "pearson"
      ),
      
      PCoA2 = cor(
        assemblage_log2[, i],
        pcoa$points[, 2],
        method = "pearson"
      )
    )
  }
)


species_cor <- as.data.frame(
  t(species_cor)
)


species_cor$scientific <- rownames(
  species_cor
)


# Strength of association with ordination
species_cor <- species_cor %>%
  mutate(
    strength = sqrt(
      PCoA1^2 +
        PCoA2^2
    )
  ) %>%
  arrange(
    desc(strength)
  )


# ============================================================
# 25. Select top species for plotting
# ============================================================

species_cor_top <- species_cor %>%
  slice_head(
    n = 15
  )


# Scale vectors to make them visible
arrow_scale <- 2

species_cor_top <- species_cor_top %>%
  mutate(
    xend = PCoA1 * arrow_scale,
    yend = PCoA2 * arrow_scale
  )


# ============================================================
# 26. PLOT TRAJECTORIES + SPECIES VECTORS
# ============================================================

trajectory_species_plot <- ggplot(
  
  pcoa_scores,
  
  aes(
    x = PCoA1,
    y = PCoA2,
    group = site_name,
    colour = site_name
  )
) +
  
  # ----------------------------------------------------------
# Site trajectories
# ----------------------------------------------------------
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
  
  # ----------------------------------------------------------
# Sampling points
# ----------------------------------------------------------
geom_point(
  size = 3
) +
  
  # ----------------------------------------------------------
# Year
# ----------------------------------------------------------
geom_text(
  aes(
    label = Year
  ),
  vjust = -1,
  size = 3,
  show.legend = FALSE
) +
  
  # ----------------------------------------------------------
# Period labels
# ----------------------------------------------------------
geom_text(
  data = pcoa_scores %>%
    filter(
      !is.na(next_period)
    ),
  
  aes(
    x = mid_PCoA1,
    y = mid_PCoA2,
    label = next_period
  ),
  
  inherit.aes = FALSE,
  
  colour = "black",
  size = 3
) +
  
  # ----------------------------------------------------------
# Species arrows
# ----------------------------------------------------------
geom_segment(
  
  data = species_cor_top,
  
  aes(
    x = 0,
    y = 0,
    xend = xend,
    yend = yend
  ),
  
  inherit.aes = FALSE,
  
  colour = "grey40",
  
  arrow = arrow(
    length = unit(
      0.1,
      "cm"
    )
  )
) +
  
  # ----------------------------------------------------------
# Species names
# ----------------------------------------------------------
ggrepel::geom_text_repel(
  
  data = species_cor_top,
  
  aes(
    x = xend,
    y = yend,
    label = scientific
  ),
  
  inherit.aes = FALSE,
  
  size = 3
) +
  
  theme_classic() +
  
  labs(
    
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


trajectory_species_plot


#### SPECIES -----
# ============================================================
# SPECIES DRIVING PRE-BLOOM -> BLOOM CHANGE WITHIN EACH SITE
#
# This decomposes the SAME altGower distance used in the
# trajectory analysis.
#
# For each site:
#   - compare every Pre-bloom sample with every Bloom sample
#   - calculate each species' contribution to altGower
#   - average those contributions across all comparisons
# ============================================================

# Check exact period names
levels(m1_meta_traj$period)

pre_level   <- "Pre-bloom"
bloom_level <- "Bloom"


# Make absolutely sure assemblage rows follow trajectory metadata
assemblage_log2_traj <- as.matrix(
  assemblage_log2[
    m1_meta_traj$id,
    ,
    drop = FALSE
  ]
)

stopifnot(
  identical(
    rownames(assemblage_log2_traj),
    m1_meta_traj$id
  )
)


# ============================================================
# Function to calculate altGower species contributions
# within one site
# ============================================================

calculate_site_drivers <- function(site) {
  
  # Samples belonging to this site
  site_rows <- which(
    m1_meta_traj$site_name == site
  )
  
  site_meta <- m1_meta_traj[
    site_rows,
    ,
    drop = FALSE
  ]
  
  site_comm <- assemblage_log2_traj[
    site_rows,
    ,
    drop = FALSE
  ]
  
  
  # Pre-bloom and Bloom samples
  pre_rows <- which(
    site_meta$period == pre_level
  )
  
  bloom_rows <- which(
    site_meta$period == bloom_level
  )
  
  
  # Can't calculate contrast if site lacks either period
  if (
    length(pre_rows) == 0 ||
    length(bloom_rows) == 0
  ) {
    return(NULL)
  }
  
  
  # Every Pre-bloom x Bloom comparison
  comparisons <- expand.grid(
    pre = pre_rows,
    bloom = bloom_rows
  )
  
  
  # Matrix:
  # rows = pairwise Pre vs Bloom comparisons
  # columns = species contributions
  contributions <- matrix(
    0,
    nrow = nrow(comparisons),
    ncol = ncol(site_comm),
    dimnames = list(
      NULL,
      colnames(site_comm)
    )
  )
  
  
  # ----------------------------------------------------------
  # altGower:
  #
  # d = sum(abs(x1 - x2)) / NZ
  #
  # NZ = number of species that are non-zero in at least
  #      one member of that pair
  # ----------------------------------------------------------
  
  for (i in seq_len(nrow(comparisons))) {
    
    x_pre <- site_comm[
      comparisons$pre[i],
      ,
      drop = TRUE
    ]
    
    x_bloom <- site_comm[
      comparisons$bloom[i],
      ,
      drop = TRUE
    ]
    
    
    # Exclude double zeros exactly as altGower does
    non_double_zero <- (
      x_pre != 0 |
        x_bloom != 0
    )
    
    NZ <- sum(non_double_zero)
    
    
    if (NZ > 0) {
      
      contributions[
        i,
        non_double_zero
      ] <-
        abs(
          x_pre[non_double_zero] -
            x_bloom[non_double_zero]
        ) / NZ
    }
  }
  
  
  # Average contribution over all Pre x Bloom comparisons
  mean_contribution <- colMeans(
    contributions,
    na.rm = TRUE
  )
  
  
  # Mean change in transformed abundance
  mean_pre <- colMeans(
    site_comm[
      pre_rows,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
  
  mean_bloom <- colMeans(
    site_comm[
      bloom_rows,
      ,
      drop = FALSE
    ],
    na.rm = TRUE
  )
  
  delta <- mean_bloom - mean_pre
  
  
  total_contribution <- sum(
    mean_contribution,
    na.rm = TRUE
  )
  
  
  tibble(
    site_name = site,
    
    species = colnames(site_comm),
    
    contribution = mean_contribution,
    
    percent_contribution =
      100 *
      mean_contribution /
      total_contribution,
    
    mean_pre = mean_pre,
    
    mean_bloom = mean_bloom,
    
    delta = delta,
    
    direction = case_when(
      delta > 0 ~ "Higher in Bloom",
      delta < 0 ~ "Lower in Bloom",
      TRUE ~ "No change"
    )
  )
}


# ============================================================
# Run for every site
# ============================================================

site_driver_table <- bind_rows(
  lapply(
    unique(m1_meta_traj$site_name),
    calculate_site_drivers
  )
) %>%
  
  arrange(
    site_name,
    desc(contribution)
  )


site_driver_table

top_site_drivers <- site_driver_table %>%
  
  group_by(
    site_name
  ) %>%
  
  arrange(
    desc(contribution),
    .by_group = TRUE
  ) %>%
  
  slice_head(
    n = 3
  ) %>%
  
  mutate(
    
    driver_text = paste0(
      
      case_when(
        delta > 0 ~ "\u2191 ",
        delta < 0 ~ "\u2193 ",
        TRUE ~ ""
      ),
      
      species,
      
      " (",
      round(
        percent_contribution,
        1
      ),
      "%)"
    )
  ) %>%
  
  summarise(
    
    driver_label = paste(
      driver_text,
      collapse = "\n"
    ),
    
    .groups = "drop"
  )


top_site_drivers

# ============================================================
# PERIOD CENTROIDS FOR EACH SITE
# ============================================================

site_period_centroids <- pcoa_scores %>%
  
  filter(
    period %in% c(
      pre_level,
      bloom_level
    )
  ) %>%
  
  group_by(
    site_name,
    period
  ) %>%
  
  summarise(
    PCoA1 = mean(
      PCoA1,
      na.rm = TRUE
    ),
    
    PCoA2 = mean(
      PCoA2,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Pre-bloom centroids
pre_centroids <- site_period_centroids %>%
  
  filter(
    period == pre_level
  ) %>%
  
  transmute(
    site_name,
    pre_x = PCoA1,
    pre_y = PCoA2
  )


# Bloom centroids
bloom_centroids <- site_period_centroids %>%
  
  filter(
    period == bloom_level
  ) %>%
  
  transmute(
    site_name,
    bloom_x = PCoA1,
    bloom_y = PCoA2
  )


# Join them
site_period_change <- pre_centroids %>%
  
  inner_join(
    bloom_centroids,
    by = "site_name"
  ) %>%
  
  mutate(
    
    # Midpoint where species label will go
    label_x = (
      pre_x +
        bloom_x
    ) / 2,
    
    label_y = (
      pre_y +
        bloom_y
    ) / 2
  ) %>%
  
  left_join(
    top_site_drivers,
    by = "site_name"
  )

trajectory_driver_plot <- trajectory_plot +
  
  # ----------------------------------------------------------
# Site-specific Pre-bloom -> Bloom centroid movement
# ----------------------------------------------------------
geom_segment(
  
  data = site_period_change,
  
  aes(
    x = pre_x,
    y = pre_y,
    xend = bloom_x,
    yend = bloom_y
  ),
  
  inherit.aes = FALSE,
  
  linewidth = 0.6,
  
  linetype = "dashed",
  
  colour = "grey35",
  
  arrow = arrow(
    length = unit(
      0.12,
      "cm"
    ),
    type = "closed"
  )
) +
  
  # ----------------------------------------------------------
# Top species driving that site's change
# ----------------------------------------------------------
ggrepel::geom_label_repel(
  
  data = site_period_change,
  
  aes(
    x = label_x,
    y = label_y,
    label = driver_label,
    colour = site_name
  ),
  
  inherit.aes = FALSE,
  
  size = 2.7,
  
  lineheight = 1.05,
  
  show.legend = FALSE,
  
  min.segment.length = 0,
  
  box.padding = 0.5,
  
  max.overlaps = Inf
)


trajectory_driver_plot


## FROM CENTRE ----
assemblage_log2_traj <- assemblage_log2[
  m1_meta_traj$id,
  ,
  drop = FALSE
]

stopifnot(
  identical(
    rownames(assemblage_log2_traj),
    rownames(pcoa$points)
  )
)

library(vegan)
library(dplyr)
library(ggrepel)

species_fit <- vegan::envfit(
  pcoa$points ~ .,
  data = as.data.frame(assemblage_log2_traj),
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
  # filter(pval <= 0.05) %>%
  arrange(desc(r2)) %>%
  slice_head(n = 10)

arrow_mult <- min(
  diff(range(pcoa_scores$PCoA1)) / diff(range(species_vec_top$PCoA1)),
  diff(range(pcoa_scores$PCoA2)) / diff(range(species_vec_top$PCoA2))
) * 0.35

species_vec_top <- species_vec_top %>%
  mutate(
    xend = PCoA1 * arrow_mult,
    yend = PCoA2 * arrow_mult
  )

species_vec_top <- species_vec_top %>%
  mutate(
    genus_species = str_extract(scientific, "[A-Z][a-z]+\\s+[a-z]+$"),
    label = str_replace(genus_species, "^([A-Z])[a-z]+\\s+([a-z]+)$", "\\1. \\2")
  )

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
    arrow = arrow(length = unit(0.12, "cm"))
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

trajectory_species_arrows
