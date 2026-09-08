library(dplyr)
library(tidyr)
library(vegan)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tibble)
library(stringr)
library(readr)

# ============================================================
# GENERIC HELPERS
# ============================================================

make_safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_remove("_$")
}

save_plot <- function(plot, filename, width = 12, height = 9) {
  ggsave(
    filename = filename, plot = plot,
    width = width, height = height,
    units = "in", dpi = 300, bg = "white"
  )
  message("Saved: ", filename)
}

# Shared Status (shape) / Period (fill) styling, used on every
# point layer across trajectory, status PCO, and CAP plots.
# pch 21/24 are fillable (circle/triangle) so period can use the
# interior. Confirm exact factor labels via levels(meta$status)
# and levels(meta$period) for each dataset and pass overrides in
# if they differ from Fished/No-take/Pre-bloom/Bloom.
status_period_layers <- function(status_values = c("Fished" = 21, "No-take" = 24),
                                 period_values = c("Pre-bloom" = "white", "Bloom" = "black")) {
  list(
    scale_shape_manual(values = status_values),
    scale_fill_manual(values = period_values),
    # Without this, the Status legend key inherits whatever fill
    # colour happens to be mapped first, which looks broken.
    guides(
      shape = guide_legend(override.aes = list(fill = "grey50")),
      fill = guide_legend(override.aes = list(shape = 21))
    )
  )
}

pcoa_percent_variance <- function(pcoa) {
  eig_positive <- pcoa$eig[pcoa$eig > 0]
  round(100 * eig_positive[1:2] / sum(eig_positive), 1)
}

# Fit species vectors via envfit and return the top_n by r2
# (unscaled - use scale_species_vectors() before plotting)
fit_species_envfit <- function(ordination_matrix, formula_data, x_col, y_col, top_n = 10, seed = 123) {
  
  set.seed(seed)
  
  fit <- vegan::envfit(
    ordination_matrix ~ .,
    data = as.data.frame(formula_data),
    permutations = 999
  )
  
  vec <- as.data.frame(vegan::scores(fit, display = "vectors"))
  names(vec)[1:2] <- c(x_col, y_col)
  vec$scientific <- rownames(vec)
  vec$r2 <- fit$vectors$r
  vec$pval <- fit$vectors$pvals
  
  vec %>%
    filter(is.finite(.data[[x_col]]), is.finite(.data[[y_col]]), is.finite(r2)) %>%
    arrange(desc(r2)) %>%
    slice_head(n = top_n)
}

# Scale already-selected top species vectors to a sensible arrow
# length relative to the sample scores, and build genus-species
# labels. Works for envfit vectors (r2-ranked) or capscale species
# scores (vector-length-ranked) - both just need x_col/y_col.
scale_species_vectors <- function(vec_top, sample_scores, x_col, y_col) {
  
  if (is.null(vec_top) || nrow(vec_top) == 0) return(NULL)
  
  x_range <- diff(range(vec_top[[x_col]], na.rm = TRUE)); if (x_range == 0) x_range <- 1
  y_range <- diff(range(vec_top[[y_col]], na.rm = TRUE)); if (y_range == 0) y_range <- 1
  
  arrow_mult <- min(
    diff(range(sample_scores[[x_col]], na.rm = TRUE)) / x_range,
    diff(range(sample_scores[[y_col]], na.rm = TRUE)) / y_range
  ) * 0.35
  
  vec_top %>%
    mutate(
      xend = .data[[x_col]] * arrow_mult,
      yend = .data[[y_col]] * arrow_mult,
      genus_species = str_extract(scientific, "[A-Z][a-z]+\\s+[a-z]+$"),
      label = str_replace(genus_species, "^([A-Z])[a-z]+\\s+([a-z]+)$", "\\1. \\2")
    )
}

species_vector_layers <- function(vec_top) {
  if (is.null(vec_top) || nrow(vec_top) == 0) return(list())
  
  list(
    geom_segment(
      data = vec_top,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      inherit.aes = FALSE, colour = "black", linewidth = 0.5,
      arrow = arrow(length = unit(0.12, "cm"))
    ),
    ggrepel::geom_text_repel(
      data = vec_top,
      aes(x = xend, y = yend, label = label),
      inherit.aes = FALSE, colour = "black", size = 3, show.legend = FALSE
    )
  )
}

# ============================================================
# CORE DATA PREP: one location's site x sampling-event
# centroids, ready for any downstream ordination or test.
#
# STEP 1 averages blocks (transects) within a survey.
# STEP 2 averages surveys/events within a site.
# This is now the ONLY place this logic lives - previously it
# was copy-pasted into 4 functions and had drifted out of sync
# in one of them (status PCO was grouping by survey_id/survey_
# date/depth instead of transect).
# ============================================================

build_assemblage_data <- function(raw_data, location_name, min_events = 3) {
  
  complete_count <- raw_data %>%
    
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
  
  
  meta <- complete_count %>%
    
    distinct(id, site_name, status, sampling_event, sampling_event_start_date, period) %>%
    
    mutate(
      survey_date = as.Date(sampling_event_start_date),
      Year = lubridate::year(survey_date),
      year = factor(Year),
      period = factor(period),
      status = factor(status)
    ) %>%
    
    arrange(site_name, survey_date)
  
  
  if (nrow(meta) < min_events) {
    message("Skipping ", location_name, ": fewer than ", min_events, " sampling events.")
    return(NULL)
  }
  
  if (any(is.na(meta$status))) {
    message(
      "Note: ", sum(is.na(meta$status)), " sampling event(s) in ", location_name,
      " have no matching Status - check the join to the metadata file."
    )
  }
  
  stopifnot(!anyDuplicated(meta$id))
  
  
  assemblage <- complete_count %>%
    
    select(id, scientific, total_site) %>%
    
    pivot_wider(names_from = scientific, values_from = total_site, values_fill = 0) %>%
    
    arrange(match(id, meta$id)) %>%
    
    column_to_rownames("id") %>%
    
    as.data.frame()
  
  
  stopifnot(identical(rownames(assemblage), meta$id))
  
  
  # Remove species with no variation within this location - makes
  # envfit/PERMANOVA/CAP more robust when looping over locations.
  assemblage <- assemblage[
    , vapply(assemblage, function(x) length(unique(x)) > 1, logical(1)), drop = FALSE
  ]
  
  if (ncol(assemblage) < 2) {
    message("Skipping ", location_name, ": fewer than 2 variable species.")
    return(NULL)
  }
  
  
  assemblage_log2 <- vegan::decostand(assemblage, method = "log", logbase = 2)
  dist_modgower <- vegan::vegdist(assemblage_log2, method = "altGower")
  
  list(
    complete_count  = complete_count,
    meta            = meta,
    assemblage      = assemblage,
    assemblage_log2 = assemblage_log2,
    dist            = dist_modgower
  )
}

# ============================================================
# TRAJECTORY PLOT + PCoA2 THROUGH TIME
# ============================================================

make_trajectory_plot <- function(raw_data, location_name, output_dir, dataset_prefix) {
  
  message("Running trajectory: ", location_name)
  
  built <- build_assemblage_data(raw_data, location_name)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  assemblage_log2 <- built$assemblage_log2
  
  
  # Order observations for the trajectory line, reorder distance matrix to match
  ord <- order(meta$site_name, meta$survey_date)
  meta_traj <- meta[ord, , drop = FALSE]
  
  dmat <- as.matrix(built$dist)
  dmat <- dmat[meta_traj$id, meta_traj$id]
  dist_traj <- as.dist(dmat)
  
  stopifnot(identical(attr(dist_traj, "Labels"), meta_traj$id))
  
  
  pcoa <- cmdscale(dist_traj, k = 2, eig = TRUE, add = TRUE)
  
  pcoa_scores <- as.data.frame(pcoa$points)
  names(pcoa_scores) <- c("PCoA1", "PCoA2")
  pcoa_scores$id <- rownames(pcoa_scores)
  
  pcoa_scores <- pcoa_scores %>%
    left_join(meta_traj, by = "id") %>%
    arrange(site_name, survey_date)
  
  pv <- pcoa_percent_variance(pcoa)
  PCoA1_percent <- pv[1]; PCoA2_percent <- pv[2]
  
  
  # Species vectors
  assemblage_log2_traj <- assemblage_log2[meta_traj$id, , drop = FALSE]
  stopifnot(identical(rownames(assemblage_log2_traj), rownames(pcoa$points)))
  
  species_vec_top <- fit_species_envfit(pcoa$points, assemblage_log2_traj, "PCoA1", "PCoA2")
  
  if (is.null(species_vec_top) || nrow(species_vec_top) == 0) {
    message("Skipping ", location_name, ": no usable species vectors.")
    return(NULL)
  }
  
  species_vec_top <- scale_species_vectors(species_vec_top, pcoa_scores, "PCoA1", "PCoA2")
  
  
  trajectory_plot <- ggplot(
    pcoa_scores,
    aes(x = PCoA1, y = PCoA2, group = site_name, colour = site_name)
  ) +
    
    geom_path(
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      linewidth = 0.8
    ) +
    
    geom_point(aes(shape = status, fill = period), size = 3, stroke = 1) +
    
    status_period_layers() +
    
    geom_text(
      aes(label = str_sub(sampling_event_start_date, 1, 7)),
      vjust = -1, size = 3, show.legend = FALSE
    ) +
    
    theme_classic() +
    
    labs(
      title = location_name,
      x = paste0("PCoA1 (", PCoA1_percent, "%)"),
      y = paste0("PCoA2 (", PCoA2_percent, "%)"),
      colour = "Site", shape = "Status", fill = "Period"
    )
  
  
  trajectory_species_arrows <- trajectory_plot + species_vector_layers(species_vec_top)
  
  
  pcoa2_time_plot <- ggplot(
    pcoa_scores,
    aes(x = survey_date, y = PCoA2, group = site_name, colour = site_name)
  ) +
    
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    
    geom_point(aes(shape = status, fill = period), size = 3, stroke = 1, na.rm = TRUE) +
    
    status_period_layers() +
    
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
    
    scale_y_reverse() +
    
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.02, 0.04))) +
    
    labs(
      title = location_name, x = "Sampling date",
      y = paste0("PCoA2 (", PCoA2_percent, "%; reversed)"),
      colour = "Site", shape = "Status", fill = "Period"
    ) +
    
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  safe_name <- make_safe_name(location_name)
  
  save_plot(
    pcoa2_time_plot,
    file.path(output_dir, paste0(dataset_prefix, "_PCoA2_through_time_", safe_name, ".png")),
    width = 12, height = 7
  )
  
  # Restoring this save - it was being skipped due to a dangling
  # reference to a commented-out `output_file` variable.
  save_plot(
    trajectory_species_arrows,
    file.path(output_dir, paste0(dataset_prefix, "_trajectory_", safe_name, ".png")),
    width = 12, height = 9
  )
  
  invisible(trajectory_species_arrows)
}


# ============================================================
# STATUS PCO PLOT
# ============================================================

make_status_pco_plot <- function(raw_data, location_name, output_dir, dataset_prefix) {
  
  message("Running status PCO: ", location_name)
  
  built <- build_assemblage_data(raw_data, location_name)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  assemblage_log2 <- built$assemblage_log2
  
  
  pcoa <- cmdscale(built$dist, k = 2, eig = TRUE, add = TRUE)
  
  pcoa_scores <- as.data.frame(pcoa$points)
  names(pcoa_scores) <- c("PCoA1", "PCoA2")
  pcoa_scores$id <- rownames(pcoa_scores)
  pcoa_scores <- pcoa_scores %>% left_join(meta, by = "id")
  
  pv <- pcoa_percent_variance(pcoa)
  PCoA1_percent <- pv[1]; PCoA2_percent <- pv[2]
  
  
  trajectory_segments <- pcoa_scores %>%
    arrange(site_name, survey_date) %>%
    group_by(site_name) %>%
    mutate(PCoA1_end = lead(PCoA1), PCoA2_end = lead(PCoA2)) %>%
    ungroup() %>%
    filter(!is.na(PCoA1_end))
  
  
  species_vec_top <- fit_species_envfit(pcoa$points, assemblage_log2, "PCoA1", "PCoA2")
  
  if (is.null(species_vec_top) || nrow(species_vec_top) == 0) {
    message("Skipping ", location_name, ": no usable species vectors.")
    return(NULL)
  }
  
  species_vec_top <- scale_species_vectors(species_vec_top, pcoa_scores, "PCoA1", "PCoA2")
  
  
  status_pco_plot <- ggplot(pcoa_scores, aes(x = PCoA1, y = PCoA2, colour = site_name)) +
    
    geom_segment(
      data = trajectory_segments,
      aes(x = PCoA1, y = PCoA2, xend = PCoA1_end, yend = PCoA2_end, colour = site_name),
      inherit.aes = FALSE,
      arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
      linewidth = 0.8
    ) +
    
    geom_point(aes(shape = status, fill = period), size = 3, stroke = 1) +
    
    status_period_layers() +
    
    geom_text(
      aes(label = str_sub(sampling_event_start_date, 1, 7)),
      vjust = -1, size = 3, show.legend = FALSE
    ) +
    
    theme_classic() +
    
    labs(
      title = location_name,
      x = paste0("PCoA1 (", PCoA1_percent, "%)"),
      y = paste0("PCoA2 (", PCoA2_percent, "%)"),
      colour = "Site", shape = "Status", fill = "Period"
    )
  
  
  status_pco_species <- status_pco_plot + species_vector_layers(species_vec_top)
  
  
  save_plot(
    status_pco_species,
    file.path(output_dir, paste0(dataset_prefix, "_status_PCO_", make_safe_name(location_name), ".png")),
    width = 12, height = 9
  )
  
  invisible(status_pco_species)
}

# ============================================================
# PERMANOVA: Period effect, blocked by Site
# ============================================================

test_period_effect <- function(raw_data, location_name, min_events = 4) {
  
  message("Testing period effect: ", location_name)
  
  built <- build_assemblage_data(raw_data, location_name, min_events = min_events)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  
  if (length(unique(meta$period)) < 2) {
    message("Skipping ", location_name, ": only one Period present.")
    return(NULL)
  }
  
  site_period_counts <- meta %>%
    distinct(site_name, period) %>%
    count(site_name, name = "n_periods")
  
  if (any(site_period_counts$n_periods < 2)) {
    message(
      "Note: ", sum(site_period_counts$n_periods < 2), " site(s) in ", location_name,
      " only have one Period - they contribute less to this test."
    )
  }
  
  
  disp <- vegan::betadisper(built$dist, meta$period)
  disp_test <- vegan::permutest(disp, permutations = 999)
  disp_p <- disp_test$tab$`Pr(>F)`[1]
  
  set.seed(123)
  
  permanova <- vegan::adonis2(
    built$dist ~ site_name + period,  #+ period:status,
    data = meta, permutations = 999,
    strata = meta$site_name, by = "terms"
  )
  
  period_row <- permanova["period", ]
  
  tibble(
    location = location_name,
    n = nrow(meta),
    n_sites = n_distinct(meta$site_name),
    R2 = period_row$R2,
    F_value = period_row$F,
    p_value = period_row$`Pr(>F)`,
    dispersion_p = disp_p
  )
}


# ============================================================
# CAP (constrained ordination)
# ============================================================

make_cap_plot <- function(raw_data, location_name, output_dir, dataset_prefix) {
  
  message("Running CAP: ", location_name)
  
  built <- build_assemblage_data(raw_data, location_name)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  assemblage_log2 <- built$assemblage_log2
  
  if (nrow(meta) <= n_distinct(meta$site_name) + 2) {
    message("Skipping CAP for ", location_name, ": not enough residual d.f.")
    return(NULL)
  }
  
  
  cap_model <- vegan::capscale(
    assemblage_log2 ~ period + Condition(site_name),
    data = meta, distance = "altGower", add = TRUE
  )
  
  set.seed(123)
  cap_test <- anova(cap_model, permutations = 999, strata = meta$site_name)
  cap_p <- cap_test$`Pr(>F)`[1]
  
  message("  CAP permutation test p = ", cap_p)
  
  
  all_scores <- as.data.frame(vegan::scores(cap_model, display = "sites"))
  
  if (!all(c("CAP1", "MDS1") %in% names(all_scores))) {
    message("Skipping CAP plot for ", location_name, ": expected CAP1/MDS1 axes not found.")
    return(NULL)
  }
  
  cap_scores <- all_scores %>%
    select(CAP1, MDS1) %>%
    mutate(id = rownames(all_scores)) %>%
    left_join(meta, by = "id")
  
  
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
      slice_head(n = 10) %>%
      scale_species_vectors(cap_scores, "CAP1", "MDS1")
  }
  
  
  # Relative to TOTAL inertia (including Condition(site_name)'s
  # share) - these numbers will look smaller than the PCoA plots'.
  cap_percent <- round(100 * cap_model$CCA$eig[1] / cap_model$tot.chi, 1)
  mds_percent <- round(100 * cap_model$CA$eig[1] / cap_model$tot.chi, 1)
  
  safe_name <- make_safe_name(location_name)
  
  
  cap1_time_plot <- ggplot(
    cap_scores,
    aes(x = survey_date, y = CAP1, group = site_name, colour = site_name)
  ) +
    
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    
    geom_point(aes(shape = status, fill = period), size = 3, stroke = 1, na.rm = TRUE) +
    
    status_period_layers() +
    
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, colour = "grey60") +
    
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.02, 0.04))) +
    
    labs(
      title = location_name, x = "Sampling date",
      y = paste0("CAP1 - Period axis (", cap_percent, "%)"),
      colour = "Site", shape = "Status", fill = "Period"
    ) +
    
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  save_plot(
    cap1_time_plot,
    file.path(output_dir, paste0(dataset_prefix, "_CAP1_through_time_", safe_name, ".png")),
    width = 12, height = 7
  )
  
  
  cap_plot <- ggplot(cap_scores, aes(x = CAP1, y = MDS1, colour = site_name)) +
    
    geom_point(aes(shape = status, fill = period), size = 3, stroke = 1) +
    
    status_period_layers() +
    
    theme_classic() +
    
    labs(
      title = paste0(location_name, " (CAP, p = ", signif(cap_p, 3), ")"),
      x = paste0("CAP1 - Period axis (", cap_percent, "%)"),
      y = paste0("MDS1 - residual axis (", mds_percent, "%)"),
      colour = "Site", shape = "Status", fill = "Period"
    ) +
    
    species_vector_layers(species_vec_top)
  
  
  save_plot(
    cap_plot,
    file.path(output_dir, paste0(dataset_prefix, "_CAP_", safe_name, ".png")),
    width = 12, height = 9
  )
  
  invisible(cap_plot)
}

# ============================================================
# FULL PIPELINE FOR ONE DATASET
#
# Runs trajectory + status PCO for every location, tests for a
# Period effect everywhere, then runs CAP only where that test
# came back significant. Everything is parameterised, so this
# is the only thing you need to call per dataset.
# ============================================================

run_dataset_pipeline <- function(count_rds_path, meta_rds_path, output_dir, dataset_prefix) {
  
  site_meta <- read_rds(meta_rds_path)
  raw_data <- read_rds(count_rds_path) %>% left_join(site_meta)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  locations <- raw_data %>%
    filter(!is.na(location)) %>%
    distinct(location) %>%
    arrange(location) %>%
    pull(location)
  
  message("=== ", dataset_prefix, ": ", length(locations), " location(s) found ===")
  
  
  for (loc in locations) {
    tryCatch(
      make_trajectory_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (trajectory) for ", loc, ": ", conditionMessage(e))
    )
  }
  
  for (loc in locations) {
    tryCatch(
      make_status_pco_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (status PCO) for ", loc, ": ", conditionMessage(e))
    )
  }
  
  
  permanova_list <- list()
  
  for (loc in locations) {
    result <- tryCatch(
      test_period_effect(raw_data, loc),
      error = function(e) {
        message("ERROR (PERMANOVA) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) permanova_list[[loc]] <- result
  }
  
  permanova_results <- bind_rows(permanova_list)
  
  write.csv(
    permanova_results,
    file = file.path(output_dir, paste0(dataset_prefix, "_period_PERMANOVA_results.csv")),
    row.names = FALSE
  )
  
  
  significant_locations <- permanova_results %>%
    filter(p_value <= 0.05) %>%
    pull(location)
  
  for (loc in significant_locations) {
    tryCatch(
      make_cap_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (CAP) for ", loc, ": ", conditionMessage(e))
    )
  }
  
  invisible(permanova_results)
}


# ============================================================
# RUN FOR ALL THREE DATASETS
# Same output_dir is fine for all three - every filename is
# already prefixed, so nothing will collide. Split into per-
# dataset subfolders instead if you'd rather keep them apart.
# ============================================================

m1_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m1_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M1",
  dataset_prefix = "M1"
)

m2_inverts_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m2_inverts_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M2_inverts",
  dataset_prefix = "M2_inverts"
)

m2_fish_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m2_fish_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M2_cryptic",
  dataset_prefix = "M2_cryptic"
)
