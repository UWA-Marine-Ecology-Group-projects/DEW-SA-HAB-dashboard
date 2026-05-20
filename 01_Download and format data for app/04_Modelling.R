# Load libraries needed -----
library(dplyr)
library(purrr)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(tibble)
library(progressr)
library(sf)

handlers(global = TRUE)
handlers("progress")

sf_use_s2(FALSE)

# Read in metadata -----
load("app_data/hab_data.Rdata")

metadata <- hab_data$hab_combined_metadata %>%
  dplyr::filter(method %in% "BRUVs")

unique(metadata$method)

glimpse(hab_data)
unique(metadata$reporting_name)

# -----------------------------
# 1. Prepare data
# -----------------------------

prep_metric_data <- function(df, response_col) {
  df %>%
    filter(
      !is.na(.data[[response_col]]),
      !is.na(period),
      !is.na(status),
      !is.na(reporting_name),
      !is.na(year),
      !is.na(sample)
    ) %>%
    mutate(
      Period = factor(period, levels = c("Pre-bloom", "Bloom")),
      Status = factor(status, levels = c("Fished", "No-take")),
      year = factor(year),
      site = factor(sample)
    )
}

unique((hab_data$total_abundance_samples)$reporting_name)

abund_dat <- prep_metric_data(
  hab_data$total_abundance_samples,
  "total_abundance_sample") %>%
  # filter(reporting_name %in% c("Aldinga - Aldinga Reef Sanctuary Zone",
  #                              "Port Noarlunga - Port Noarlunga Reef Sanctuary Zone",
  #                              "O'Sullivan",
  #                              "Sponge Gardens - Sponge Gardens Sanctuary Zone",
  #                              "Carrickalinga - Carrickalinga Cliffs Sanctuary Zone" 
  #                              # "Glenelg",
  #                              # "Boston Bay"
  #                              )) %>%
  
  dplyr::filter(!reporting_name %in% c(NA, "Glenelg", "Boston Bay")) %>%
  glimpse()

unique(abund_dat$reporting_name)

rich_dat <- prep_metric_data(
  hab_data$species_richness_samples,
  "n_species_sample"
)  %>%
  filter(reporting_name %in% c("Aldinga - Aldinga Reef Sanctuary Zone",
                               "Port Noarlunga - Port Noarlunga Reef Sanctuary Zone",
                               "O'Sullivan",
                               "Sponge Gardens - Sponge Gardens Sanctuary Zone",
                               "Glenelg",
                               "Boston Bay"))%>%
  glimpse()

shark_dat <- prep_metric_data(
  hab_data$shark_ray_richness_samples %>% left_join(metadata),
  "n_species_sample"
)  %>%
  filter(reporting_name %in% c("Aldinga - Aldinga Reef Sanctuary Zone",
                               "Port Noarlunga - Port Noarlunga Reef Sanctuary Zone",
                               "O'Sullivan",
                               "Sponge Gardens - Sponge Gardens Sanctuary Zone",
                               # "Glenelg",
                               # "Boston Bay"))%>%
  glimpse()

reef_dat <- prep_metric_data(
  hab_data$reef_associated_richness_samples %>% left_join(metadata),
  "n_species_sample"
)  %>%
  filter(reporting_name %in% c("Aldinga - Aldinga Reef Sanctuary Zone",
                               "Port Noarlunga - Port Noarlunga Reef Sanctuary Zone",
                               "O'Sullivan",
                               "Sponge Gardens - Sponge Gardens Sanctuary Zone",
                               "Glenelg",
                               "Boston Bay"))%>%
  glimpse()

unique(abund_dat$site)
unique(abund_dat$uwa_site_code)

# -----------------------------
# 2. Fit one reporting_name model
# -----------------------------

fit_one_reporting_name <- function(df,
                                   response_col,
                                   metric_name,
                                   use_site = FALSE) {
  
  # Skip areas without enough data
  if (
    nrow(df) < 20 ||
    n_distinct(df$Period) < 1 ||
    n_distinct(df$Status) < 1 ||
    n_distinct(df$year) < 1
  ) {
    stop("Not enough data to fit model")
  }
  
  area_reporting_name <- df %>%
    distinct(reporting_name) %>%
    pull(reporting_name) %>%
    first()
  
  rand_effects <- if (use_site) {
    "(1 | uwa_site_code) + (1 | year)"
  } else {
    "(1 | year)"
  }
  
  model_used <- if (use_site) {
    "Period * Status + site random effect + year random effect"
  } else {
    "Period * Status + year random effect"
  }
  
  form <- as.formula(
    paste0(response_col, " ~ Period * Status + ", rand_effects)
  )
  
  family_to_use <- if (response_col == "n_species_sample") {
    nbinom2(link = "log")
  } else {
    tweedie(link = "log")
  }
  
  model <- glmmTMB(
    form,
    data = df,
    family = family_to_use,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS")
    )
  )
  
  model_summary <- summary(model)
  
  period_means <- emmeans(
    model,
    ~ Period,
    type = "response",
    weights = "proportional"
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = "Period averaged over Status"
    )
  
  period_status_means <- emmeans(
    model,
    ~ Period * Status,
    type = "response"
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(
      reporting_name = area_reporting_name,
      metric = metric_name,
      model_used = model_used,
      summary_type = "Period by Status"
    )
  
  list(
    model = model,
    model_summary = model_summary,
    period_means = period_means,
    period_status_means = period_status_means
  )
}

# -----------------------------
# 3. Run models across all areas
# -----------------------------

run_metric_models <- function(df, response_col, metric_name, use_site = FALSE) {
  
  split_dat <- df %>%
    group_by(reporting_name) %>%
    group_split()
  
  names(split_dat) <- map_chr(split_dat, ~ unique(.x$reporting_name))
  
  # with_progress({
    
    # p <- progressor(
    #   steps = length(split_dat)
    # )
    
    model_outputs <- map(
      split_dat,
      ~ {
        area_name <- unique(.x$reporting_name)
        
        # p(message = paste("Fitting", metric_name, "for", area_name))
        
        safely(
          fit_one_reporting_name
        )(
          .x,
          response_col,
          metric_name,
          use_site = use_site
        )
      }
    )
  # })
  
  period_means <- map_dfr(
    model_outputs,
    ~ if (is.null(.x$error)) .x$result$period_means else NULL
  )
  
  period_status_means <- map_dfr(
    model_outputs,
    ~ if (is.null(.x$error)) .x$result$period_status_means else NULL
  )
  
  model_errors <- imap_dfr(
    model_outputs,
    ~ {
      if (!is.null(.x$error)) {
        tibble(
          reporting_name = .y,
          metric = metric_name,
          error = .x$error$message
        )
      } else {
        NULL
      }
    }
  )
  
  list(
    model_outputs = model_outputs,
    period_means = period_means,
    period_status_means = period_status_means,
    model_errors = model_errors
  )
}

# -----------------------------
# 4. Run metrics
# -----------------------------

abund_models <- run_metric_models(
  abund_dat,
  response_col = "total_abundance_sample",
  metric_name = "Total abundance",
  use_site = TRUE
)

rich_models <- run_metric_models(
  rich_dat,
  response_col = "n_species_sample",
  metric_name = "Species richness",
  use_site = FALSE
)

shark_models <- run_metric_models(
  shark_dat,
  response_col = "n_species_sample",
  metric_name = "Shark and ray richness",
  use_site = FALSE
)

reef_models <- run_metric_models(
  reef_dat,
  response_col = "n_species_sample",
  metric_name = "Reef associated species richness",
  use_site = FALSE
)

# -----------------------------
# 5. Combine dashboard-ready outputs
# -----------------------------

period_results <- bind_rows(
  abund_models$period_means,
  # rich_models$period_means,
  # shark_models$period_means,
  # reef_models$period_means
  
)

period_status_results <- bind_rows(
  abund_models$period_status_means,
  # rich_models$period_status_means,
  # shark_models$period_status_means,
  # reef_models$period_status_means
)

model_errors <- bind_rows(
  abund_models$model_errors,
  # rich_models$model_errors,
  # shark_models$model_errors,
  # reef_models$model_errors
)

# Check any models that failed
model_errors

# Plot 1: Pre-bloom vs Bloom for all areas
# shared colours for pre/post everywhere
metric_period_cols <- c(
  "Pre-bloom"  = "#072759",  # same blue
  "Bloom" = "#e88e98"   # same orange
)

ggplot(period_results, aes(x = Period, y = response)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = response - SE, ymax = response + SE),
    width = 0.15
  ) +
  facet_wrap(metric ~ reporting_name, scales = "free_y") +
  theme_bw()

# Trying to make it look like the dashboard ----
ggplot(period_results, aes(x = Period, y = response, fill = Period)) +
  geom_col(width = 0.6, colour = "black", alpha = 0.85) +
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = metric_period_cols) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(metric ~ reporting_name, scales = "free_y")

# Plot 2: Pre-bloom vs Bloom by Fished vs No-take
ggplot(period_status_results, aes(x = Period, y = response, colour = Status)) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 2
  ) +
  geom_errorbar(
    aes(ymin = response - SE, ymax = response + SE),
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  facet_wrap(metric ~ reporting_name, scales = "free_y") +
  theme_bw()
