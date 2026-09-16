###Pre-bloom vs bloom plots, per site - QUICK MOCK-UP
##Written 16/09/2026
##
##Reads the tables written by script 17 and draws three plots:
##   1. composition - stacked % cover by level_2, Pre-bloom and Bloom, one panel per site
##   2. change      - % cover Pre-bloom -> Bloom, one line per site, one panel per level_2
##   3. richness    - morphospecies per image Pre-bloom -> Bloom, one line per site
##
##THE SPLIT. There is a clean 692-day gap in the survey dates: nothing between
##2024-02-20 and 2026-01-12, and the bloom sits in it. So "Pre-bloom" is everything up to
##2024 and "Bloom" is the 2026 surveys, with no judgement call needed at the boundary.
##
##WHAT THESE CAN AND CANNOT SHOW. 32 of the 53 sites were surveyed in both periods and
##only those appear - 13 sites have baseline data only and 8 are post-bloom only, so
##nothing can be said about change at those 21. "Pre-bloom" pools up to six sampling events
##spread over 2018-2024 at some sites, so a site's Pre-bloom value is a multi-year average
##rather than a single baseline. These are mock-ups for looking, not results - the
##models in script 11 are where a difference gets tested.

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

dir.create("plots/hab", recursive = TRUE, showWarnings = FALSE)

# `period` comes through from add_sampling_event() in script 17 - no date cut is made
# here, so this and the fish/invertebrate scripts split the timeline the same way.
cover_site  <- read_csv("data/tidy/cover_level_2_per_sampling_event.csv", show_col_types = FALSE) #%>%
  # dplyr::filter(n_images > 50)

test <- cover_site %>% dplyr::filter(site_code %in% "GSV1")

summary(cover_site)

richness_site <- read_csv("data/tidy/richness_per_sampling_event.csv", show_col_types = FALSE)

set_period <- function(x) {
  x %>% dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))
}

# ================================================================
# Average the sampling events within each period
# ================================================================
# Averaging the EVENTS, not the transects - script 17 already averaged the transects
# into each sampling event, so a field trip counts once however many transects it ran.

cover_period <- cover_site %>%
  set_period() %>%
  dplyr::group_by(site_code, period, level_1, level_2) %>%
  dplyr::summarise(n_events      = dplyr::n(),
                   percent_cover = mean(percent_cover),
                   .groups = "drop")

richness_period <- richness_site %>%
  set_period() %>%
  dplyr::group_by(site_code, period) %>%
  dplyr::summarise(n_events                = dplyr::n(),
                   mean_richness_per_image = mean(mean_richness_per_image),
                   .groups = "drop")

# only sites with data on both sides of the bloom can show a change
paired_sites <- cover_period %>%
  dplyr::distinct(site_code, period) %>%
  dplyr::count(site_code) %>%
  dplyr::filter(n == 2) %>%
  dplyr::pull(site_code)

message(length(paired_sites), " of ", dplyr::n_distinct(cover_site$site_code),
        " sites have surveys in both periods")

cover_paired    <- cover_period    %>% dplyr::filter(site_code %in% paired_sites)
richness_paired <- richness_period %>% dplyr::filter(site_code %in% paired_sites)

# ================================================================
# Colours
# ================================================================
# Seven habitat classes: the six that make up ~99.5% of points, plus everything else
# folded into "Other invertebrates" - past seven, a stacked bar stops being readable
# and the hues stop being tellable apart.
habitat_levels <- c("Macroalgae", "Substrate", "Seagrasses", "Matrix",
                    "Sponges", "Bryozoa", "Other invertebrates")

habitat_cols <- c("Macroalgae"          = "#1baf7a",
                  "Substrate"           = "#eda100",
                  "Seagrasses"          = "#008300",
                  "Matrix"              = "#eb6834",
                  "Sponges"             = "#e87ba4",
                  "Bryozoa"             = "#4a3aa7",
                  "Other invertebrates" = "#2a78d6")

# direction of change - two opposed hues, nothing else encodes it
direction_cols <- c("Decrease" = "#d03b3b", "Increase" = "#2a78d6", "No change" = "#9a9a94")

cover_plot_data <- cover_paired %>%
  dplyr::mutate(habitat = ifelse(level_2 %in% habitat_levels,
                                 level_2, "Other invertebrates"),
                habitat = factor(habitat, levels = habitat_levels)) %>%
  dplyr::group_by(site_code, period, habitat) %>%
  dplyr::summarise(percent_cover = sum(percent_cover), .groups = "drop")

# ================================================================
# Plot 1 - composition Pre-bloom and Bloom, one panel per site
# ================================================================

p_composition <- cover_plot_data %>%
  ggplot(aes(x = period, y = percent_cover, fill = habitat)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4) +   # white gap between segments
  facet_wrap(~ site_code, ncol = 8) +
  scale_fill_manual(values = habitat_cols, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(#title = "Seafloor composition before and during the bloom",
       #subtitle = "% cover by habitat class, mean of sampling events in each period",
       x = NULL, y = "% cover") +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey95", colour = NA))

p_composition

ggsave("plots/hab/01_composition_by_site.png", p_composition,
       width = 14, height = 10, dpi = 200)

# ================================================================
# Plot 2 - change in each habitat class, one line per site
# ================================================================
# A slope chart, so each site's own Pre-bloom -> Bloom is one line. Reading a difference
# off two separate bars is much harder than reading the slope of a line, and the line
# also shows whether sites moved together or in opposite directions.

slope_data <- cover_plot_data %>%
  tidyr::pivot_wider(names_from = period, values_from = percent_cover,
                     values_fill = 0) %>%
  dplyr::mutate(change = Bloom - `Pre-bloom`,
                direction = dplyr::case_when(change < -1 ~ "Decrease",
                                             change >  1 ~ "Increase",
                                             TRUE        ~ "No change")) %>%
  tidyr::pivot_longer(c(`Pre-bloom`, Bloom), names_to = "period",
                      values_to = "percent_cover") %>%
  dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))

p_change <- slope_data %>%
  ggplot(aes(x = period, y = percent_cover, group = site_code, colour = direction)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~ habitat, scales = "free_y", nrow = 2) +
  scale_colour_manual(values = direction_cols, name = NULL) +
  labs(#title = "Change in % cover at each site",
       #subtitle = "one line per site; colour shows the direction of change",
       x = NULL, y = "% cover") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey95", colour = NA))

ggsave("plots/hab/02_cover_change_by_class.png", p_change,
       width = 13, height = 7, dpi = 200)

# ================================================================
# Plot 3 - morphospecies richness
# ================================================================
# Sites are labelled directly rather than given a legend of 32 colours.

richness_slope <- richness_paired %>%
  tidyr::pivot_wider(names_from = period, values_from = mean_richness_per_image) %>%
  dplyr::mutate(change = Bloom - `Pre-bloom`,
                direction = dplyr::case_when(change < -0.25 ~ "Decrease",
                                             change >  0.25 ~ "Increase",
                                             TRUE           ~ "No change")) %>%
  tidyr::pivot_longer(c(`Pre-bloom`, Bloom), names_to = "period", values_to = "richness") %>%
  dplyr::mutate(period = factor(period, levels = c("Pre-bloom", "Bloom")))

p_richness <- richness_slope %>%
  ggplot(aes(x = period, y = richness, group = site_code, colour = direction)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 2.2) +
  geom_text(
    data = dplyr::filter(richness_slope, period == "Bloom"),
    aes(label = site_code), size = 2.6, hjust = 0, nudge_x = 0.06,
    check_overlap = TRUE, show.legend = FALSE) +
  scale_colour_manual(values = direction_cols, name = NULL) +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.28))) +
  labs(#title = "Morphospecies richness before and during the bloom",
       #subtitle = "mean living level_3 classes per 20-point image; one line per site",
       x = NULL, y = "morphospecies per image") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave("plots/hab/03_richness_change.png", p_richness,
       width = 7, height = 8, dpi = 200)

# ================================================================
# The same numbers as a table - what the plots are drawn from
# ================================================================

change_table <- slope_data %>%
  dplyr::filter(period == "Bloom") %>%
  dplyr::select(site_code, habitat, change) %>%
  tidyr::pivot_wider(names_from = habitat, values_from = change) %>%
  dplyr::left_join(richness_slope %>%
                     dplyr::filter(period == "Bloom") %>%
                     dplyr::select(site_code, richness_change = change),
                   by = "site_code") %>%
  dplyr::arrange(site_code)

write_csv(change_table, "data/tidy/change_pre_bloom_to_bloom_by_site.csv")

# headline direction, across the paired sites
slope_data %>%
  dplyr::filter(period == "Bloom") %>%
  dplyr::group_by(habitat) %>%
  dplyr::summarise(sites_down   = sum(change < -1),
                   sites_up     = sum(change > 1),
                   median_change = round(stats::median(change), 1),
                   .groups = "drop") %>%
  dplyr::arrange(median_change) %>%
  print(n = Inf)

richness_slope %>%
  dplyr::filter(period == "Bloom") %>%
  dplyr::summarise(sites_down    = sum(change < -0.25),
                   sites_up      = sum(change > 0.25),
                   median_change = round(stats::median(change), 2)) %>%
  print()

