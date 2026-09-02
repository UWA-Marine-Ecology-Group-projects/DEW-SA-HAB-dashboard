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
# GENERIC HELPERS (unchanged)
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

status_period_layers <- function(status_values = c("Fished" = 21, "No-take" = 24),
                                 period_values = c("Pre-bloom" = "white", "Bloom" = "black")) {
  list(
    scale_shape_manual(values = status_values),
    scale_fill_manual(values = period_values),
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

# Interaction term labels are ordered by each variable's FIRST
# appearance across the WHOLE formula, not the order written
# inside that specific term. Since `period` already appears as
# its own main-effect term earlier in the formula, R names this
# row "period:status" even though it was written `status:period`.
# Searching by content (rather than assuming one label) makes
# this robust to that reordering, and to future formula edits.
find_interaction_row <- function(permanova, term_a, term_b) {
  rn <- rownames(permanova)
  match_idx <- which(
    grepl(term_a, rn, fixed = TRUE) &
      grepl(term_b, rn, fixed = TRUE) &
      grepl(":", rn, fixed = TRUE)
  )
  if (length(match_idx) != 1) return(NULL)
  rn[match_idx]
}

# ============================================================
# CORE DATA PREP: site x sampling-event level (unchanged)
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
# NEW: CORE DATA PREP AT SITE LEVEL (one row per site,
# collapsed across sampling events/periods)
#
# Status doesn't vary within a site, so this is the correct
# unit of replication for testing it - averaging up to site
# level avoids treating repeated visits as independent evidence.
# A species that's constant across every event-level row is
# necessarily constant after averaging too, so reusing
# build_assemblage_data()'s event-level filtering first is safe
# and loses no site-level discriminating power.
# ============================================================

build_site_level_data <- function(raw_data, location_name, min_sites = 3) {
  
  built <- build_assemblage_data(raw_data, location_name, min_events = 1)
  if (is.null(built)) return(NULL)
  
  site_level_count <- built$complete_count %>%
    group_by(site_name, status, family, genus, species, scientific) %>%
    summarise(total_site_avg = mean(total_site), .groups = "drop")
  
  site_meta <- site_level_count %>%
    distinct(site_name, status) %>%
    mutate(status = factor(status))
  
  if (nrow(site_meta) < min_sites) {
    message("Skipping ", location_name, ": fewer than ", min_sites, " sites for a Status test.")
    return(NULL)
  }
  
  if (length(unique(site_meta$status)) < 2) {
    message("Skipping ", location_name, ": only one Status present.")
    return(NULL)
  }
  
  site_assemblage <- site_level_count %>%
    select(site_name, scientific, total_site_avg) %>%
    pivot_wider(names_from = scientific, values_from = total_site_avg, values_fill = 0) %>%
    arrange(match(site_name, site_meta$site_name)) %>%
    column_to_rownames("site_name") %>%
    as.data.frame()
  
  stopifnot(identical(rownames(site_assemblage), site_meta$site_name))
  
  site_assemblage <- site_assemblage[
    , vapply(site_assemblage, function(x) length(unique(x)) > 1, logical(1)), drop = FALSE
  ]
  
  if (ncol(site_assemblage) < 2) {
    message("Skipping ", location_name, ": fewer than 2 variable species at site level.")
    return(NULL)
  }
  
  site_assemblage_log2 <- vegan::decostand(site_assemblage, method = "log", logbase = 2)
  site_dist <- vegan::vegdist(site_assemblage_log2, method = "altGower")
  
  list(
    meta            = site_meta,
    assemblage      = site_assemblage,
    assemblage_log2 = site_assemblage_log2,
    dist            = site_dist
  )
}


# ============================================================
# TRAJECTORY PLOT + PCoA2 THROUGH TIME (unchanged)
# ============================================================

make_trajectory_plot <- function(raw_data, location_name, output_dir, dataset_prefix) {
  
  message("Running trajectory: ", location_name)
  
  built <- build_assemblage_data(raw_data, location_name)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  assemblage_log2 <- built$assemblage_log2
  
  
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
  
  save_plot(
    trajectory_species_arrows,
    file.path(output_dir, paste0(dataset_prefix, "_trajectory_", safe_name, ".png")),
    width = 12, height = 9
  )
  
  invisible(trajectory_species_arrows)
}


# ============================================================
# STATUS PCO PLOT (unchanged)
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
# CHANGED: PERMANOVA for Period, now also reporting the
# status:period interaction (sub-plot level, tested against the
# same within-site residual as period itself - this is the
# valid error structure for a split-plot-style interaction).
#
# `status` main effect is deliberately NOT added here alongside
# site_name - see test_status_effect() below for why.
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
  
  
  # status:period can only be estimated where this location
  # actually has more than one Status - if every site here
  # shares one Status, drop the interaction rather than risk a
  # rank-deficient design.
  has_two_statuses <- length(unique(meta$status)) >= 2
  
  model_formula <- if (has_two_statuses) {
    built$dist ~ site_name + period + status:period
  } else {
    message(
      "Note: only one Status present in ", location_name,
      " - fitting without the status:period interaction."
    )
    built$dist ~ site_name + period
  }
  
  set.seed(123)
  
  permanova <- vegan::adonis2(
    model_formula,
    data = meta, permutations = 999,
    strata = meta$site_name, by = "terms"
  )
  
  period_stats <- permanova["period", ]
  
  period_row <- tibble(
    location = location_name,
    term = "period",
    n = nrow(meta),
    n_sites = n_distinct(meta$site_name),
    R2 = period_stats$R2,
    F_value = period_stats$F,
    p_value = period_stats$`Pr(>F)`,
    dispersion_p = disp_p
  )
  
  if (!has_two_statuses) return(period_row)
  
  interaction_term_name <- find_interaction_row(permanova, "status", "period")
  
  if (is.null(interaction_term_name)) {
    message(
      "Note: could not identify a status:period interaction row for ", location_name,
      " - it may have been dropped as aliased. Rows present: ",
      paste(rownames(permanova), collapse = ", ")
    )
    return(period_row)
  }
  
  interaction_stats <- permanova[interaction_term_name, ]
  
  interaction_row <- tibble(
    location = location_name,
    term = interaction_term_name,   # records whatever label R actually used
    n = nrow(meta),
    n_sites = n_distinct(meta$site_name),
    R2 = interaction_stats$R2,
    F_value = interaction_stats$F,
    p_value = interaction_stats$`Pr(>F)`,
    dispersion_p = NA_real_
  )
  
  bind_rows(period_row, interaction_row)
}


# ============================================================
# NEW: PERMANOVA for Status (whole-plot level, site-level
# replication, UNRESTRICTED permutation - sites are genuinely
# independent replicates at this level, so no strata needed).
# ============================================================

test_status_effect <- function(raw_data, location_name, min_sites = 3) {
  
  message("Testing status effect: ", location_name)
  
  built <- build_site_level_data(raw_data, location_name, min_sites = min_sites)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  
  disp <- vegan::betadisper(built$dist, meta$status)
  disp_test <- vegan::permutest(disp, permutations = 999)
  disp_p <- disp_test$tab$`Pr(>F)`[1]
  
  set.seed(123)
  
  permanova <- vegan::adonis2(
    built$dist ~ status,
    data = meta,
    permutations = 999,
    by = "terms"     # <- always name this - defaults have changed across vegan versions
  )
  
  status_stats <- permanova["status", ]
  
  tibble(
    location = location_name,
    n_sites = nrow(meta),
    R2 = status_stats$R2,
    F_value = status_stats$F,
    p_value = status_stats$`Pr(>F)`,
    dispersion_p = disp_p
  )
}


# ============================================================
# CAP for Period (unchanged)
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
# NEW: CAP for Status (site-level; one point per SITE, matching
# the same aggregation used in test_status_effect() so the plot
# visualises exactly what was tested). No Condition(site_name)
# here - site IS the unit of replication, not something to
# control for.
# ============================================================

make_status_cap_plot <- function(raw_data, location_name, output_dir, dataset_prefix) {
  
  message("Running Status CAP: ", location_name)
  
  built <- build_site_level_data(raw_data, location_name)
  if (is.null(built)) return(NULL)
  
  meta <- built$meta
  assemblage_log2 <- built$assemblage_log2
  
  if (nrow(meta) <= 3) {
    message("Skipping Status CAP for ", location_name, ": not enough residual d.f.")
    return(NULL)
  }
  
  
  cap_model <- vegan::capscale(
    assemblage_log2 ~ status,
    data = meta, distance = "altGower", add = TRUE
  )
  
  set.seed(123)
  cap_test <- anova(cap_model, permutations = 999)
  cap_p <- cap_test$`Pr(>F)`[1]
  
  message("  Status CAP permutation test p = ", cap_p)
  
  
  all_scores <- as.data.frame(vegan::scores(cap_model, display = "sites"))
  
  if (!all(c("CAP1", "MDS1") %in% names(all_scores))) {
    message("Skipping Status CAP plot for ", location_name, ": expected CAP1/MDS1 axes not found.")
    return(NULL)
  }
  
  cap_scores <- all_scores %>%
    select(CAP1, MDS1) %>%
    mutate(site_name = rownames(all_scores)) %>%
    left_join(meta, by = "site_name")
  
  
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
  
  
  cap_percent <- round(100 * cap_model$CCA$eig[1] / cap_model$tot.chi, 1)
  mds_percent <- round(100 * cap_model$CA$eig[1] / cap_model$tot.chi, 1)
  
  safe_name <- make_safe_name(location_name)
  
  
  status_cap_plot <- ggplot(cap_scores, aes(x = CAP1, y = MDS1, colour = status, shape = status)) +
    
    geom_point(size = 3, stroke = 1) +
    
    ggrepel::geom_text_repel(
      aes(label = site_name),
      size = 3, show.legend = FALSE
    ) +
    
    theme_classic() +
    
    labs(
      title = paste0(location_name, " (Status CAP, p = ", signif(cap_p, 3), ")"),
      x = paste0("CAP1 - Status axis (", cap_percent, "%)"),
      y = paste0("MDS1 - residual axis (", mds_percent, "%)"),
      colour = "Status", shape = "Status"
    ) +
    
    species_vector_layers(species_vec_top)
  
  
  save_plot(
    status_cap_plot,
    file.path(output_dir, paste0(dataset_prefix, "_status_CAP_", safe_name, ".png")),
    width = 12, height = 9
  )
  
  invisible(status_cap_plot)
}


# ============================================================
# CHANGED: FULL PIPELINE - now runs both PERMANOVAs and gates
# each CAP type on its own test
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
  
  
  # Period (+ status:period interaction) - within-site test
  period_list <- list()
  
  for (loc in locations) {
    result <- tryCatch(
      test_period_effect(raw_data, loc),
      error = function(e) {
        message("ERROR (period PERMANOVA) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) period_list[[loc]] <- result
  }
  
  period_results <- bind_rows(period_list)
  
  write.csv(
    period_results,
    file = file.path(output_dir, paste0(dataset_prefix, "_period_PERMANOVA_results.csv")),
    row.names = FALSE
  )
  
  
  # Status - between-site test
  status_list <- list()
  
  for (loc in locations) {
    result <- tryCatch(
      test_status_effect(raw_data, loc),
      error = function(e) {
        message("ERROR (status PERMANOVA) for ", loc, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) status_list[[loc]] <- result
  }
  
  status_results <- bind_rows(status_list)
  
  write.csv(
    status_results,
    file = file.path(output_dir, paste0(dataset_prefix, "_status_PERMANOVA_results.csv")),
    row.names = FALSE
  )
  
  
  # CAP plots, each gated on its own test
  significant_period_locations <- period_results %>%
    filter(term == "period", p_value <= 0.05) %>%
    pull(location)
  
  for (loc in significant_period_locations) {
    tryCatch(
      make_cap_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (period CAP) for ", loc, ": ", conditionMessage(e))
    )
  }
  
  significant_status_locations <- status_results %>%
    filter(p_value <= 0.05) %>%
    pull(location)
  
  for (loc in significant_status_locations) {
    tryCatch(
      make_status_cap_plot(raw_data, loc, output_dir, dataset_prefix),
      error = function(e) message("ERROR (status CAP) for ", loc, ": ", conditionMessage(e))
    )
  }
  
  invisible(list(period = period_results, status = status_results))
}


# ============================================================
# RUN FOR ALL THREE DATASETS (unchanged)
# ============================================================

m1_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m1_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M1_with_status",
  dataset_prefix = "M1"
)

m2_inverts_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m2_inverts_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M2_inverts_with_status",
  dataset_prefix = "M2_inverts"
)

m2_fish_results <- run_dataset_pipeline(
  count_rds_path = "data/tidy/rls_m2_fish_complete_count.rds",
  meta_rds_path  = "data/tidy/sa_sites.rds",
  output_dir     = "outputs/multivariate/M2_cryptic_with_status",
  dataset_prefix = "M2_cryptic"
)