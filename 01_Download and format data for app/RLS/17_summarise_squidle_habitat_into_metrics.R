###Summarise Squidle+ photo-quadrat annotations into % cover metrics
##Written 16/09/2026
##
##Rolls the point annotations up through the sampling hierarchy:
##      point  ->  image (photo quadrat)  ->  survey_id (transect)  ->  site x date
##
##Two classifications are produced in parallel, from the same function:
##   level_1 x level_2            e.g. Biota > Macroalgae        (12 classes)
##   level_1 x level_2 x level_3  e.g. Biota > Macroalgae > Erect coarse branching
##
##Then morphospecies richness - the mean number of living level_3 classes per image -
##up the same hierarchy.
##
##THREE THINGS WORTH KNOWING BEFORE USING THE OUTPUT
##
## 1. Absent classes are filled in as 0% cover, not left out. A plain
##    group_by/summarise only makes rows for classes that were actually scored, so
##    averaging those rows answers "how much cover when it was present", which is
##    always higher than "how much cover". tidyr::complete() below fills the zeros.
##
## 2. Site means are per site x DATE, never per site alone. 33 sites were surveyed on
##    2 dates, 6 on 3, 2 on 4 and 1 on 6 - averaging to site_code by itself would
##    blend the historical baseline and the post-bloom surveys into one number.
##
## 3. Two ways of getting from images to a survey are reported side by side:
##      percent_cover        - the mean of the image percentages (every image counts
##                             equally, which is what the sampling design intends)
##      percent_cover_pooled - all the survey's points pooled (every POINT counts
##                             equally, so sparse images carry less weight)
##    They are identical when every image has the same number of points. They don't,
##    because label filtering removed points from some images - 5% of images are down
##    to fewer than 10 points and 34 are down to 1. The two agree to within 0.03
##    percentage points for 90% of survey x class cells, but 12 cells differ by more
##    than 5 points and the worst by 19.5. Pick one deliberately and say which in the
##    methods; `percent_cover` is the default here.

##Loading libraries
library(CheckEM)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)

# Read in data
benthos <- read_rds("data/raw/rls_benthos_summarised.rds")

dir.create("data/tidy", recursive = TRUE, showWarnings = FALSE)

# ================================================================
# Sampling structure - what is being averaged over
# ================================================================

# Quick look at the number of images per survey ID and Site
check <- benthos %>%
  distinct(site_code, survey_id, point_media_key) %>%
  group_by(site_code, survey_id) %>%
  dplyr::summarise(n = n(), .groups = "drop")

check %>% dplyr::count(n, name = "n_surveys") %>% print(n = Inf)

# Points per image. Nominally 20; anything well under that is an image where most
# points were dropped by the label filtering in script 16, and anything over 20 is an
# image scored in two annotation sets (query A3) that has not been resolved yet.
points_per_image <- benthos %>%
  dplyr::count(site_code, date, survey_id, point_media_key, name = "n_points")

points_per_image %>% dplyr::count(n_points) %>% print(n = Inf)

message("Images: ", nrow(points_per_image),
        " | with fewer than 10 points: ", sum(points_per_image$n_points < 10),
        " | with more than 20 (scored twice): ", sum(points_per_image$n_points > 20))

# ================================================================
# The roll-up, written once and used for both classifications
# ================================================================
# `class_cols` is a CHARACTER VECTOR of the columns defining a class, e.g.
# c("level_1", "level_2"). Returns a list of the three tables so nothing downstream
# has to re-derive them.

summarise_cover <- function(data, class_cols) {

  image_cols <- c("site_code", "date", "survey_id", "point_media_key")

  # every class in the dataset, and every image in the dataset
  classes <- data %>% dplyr::distinct(dplyr::across(dplyr::all_of(class_cols)))
  images  <- data %>% dplyr::distinct(dplyr::across(dplyr::all_of(image_cols)))

  # --- 1. % cover within each image ----
  # Build every image x class combination FIRST, then join the counts on. That is what
  # puts a real 0 against a class an image didn't record - counting straight off the
  # data only makes rows where a class was scored, and averaging those answers "how
  # much cover when present", which is always higher than "how much cover".
  per_image <- images %>%
    tidyr::crossing(classes) %>%
    dplyr::left_join(
      data %>% dplyr::count(dplyr::across(dplyr::all_of(c(image_cols, class_cols))),
                            name = "n_points_class"),
      by = c(image_cols, class_cols)) %>%
    dplyr::mutate(n_points_class = tidyr::replace_na(n_points_class, 0)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(image_cols))) %>%
    dplyr::mutate(n_points_image = sum(n_points_class)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent_cover = 100 * n_points_class / n_points_image)

  # --- 2. average across the images in a survey (transect) ----
  # sd/se describe how variable the images were within the transect. They come out NA
  # where a survey has only one image (18 of them do) - that is correct, not a bug.
  per_survey <- per_image %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("site_code", "date", "survey_id",
                                                  class_cols)))) %>%
    dplyr::summarise(
      n_images             = dplyr::n(),
      n_points             = sum(n_points_image),
      sd_cover             = stats::sd(percent_cover),
      percent_cover        = mean(percent_cover),                              # mean of images
      percent_cover_pooled = 100 * sum(n_points_class) / sum(n_points_image),  # pooled points
      .groups = "drop") %>%
    dplyr::mutate(se_cover = sd_cover / sqrt(n_images))

  # --- 3. average across the surveys at a site on that date ----
  # site x date, NOT site alone - see note 2 in the header.
  per_site_date <- per_survey %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("site_code", "date", class_cols)))) %>%
    dplyr::summarise(
      n_surveys     = dplyr::n(),
      n_images      = sum(n_images),
      sd_cover      = stats::sd(percent_cover),
      percent_cover = mean(percent_cover),
      .groups = "drop") %>%
    dplyr::mutate(se_cover = sd_cover / sqrt(n_surveys))

  list(image = per_image, survey = per_survey, site = per_site_date)
}

# ================================================================
# Run it for both classifications
# ================================================================
# level_3 is NA for 393 points (mostly sponges scored only to level_2). Give them a
# name so they stay in the table and the percentages still add to 100.
benthos <- benthos %>%
  dplyr::mutate(level_3 = tidyr::replace_na(level_3, "Not identified further"))

cover_l2 <- summarise_cover(benthos, c("level_1", "level_2"))
cover_l3 <- summarise_cover(benthos, c("level_1", "level_2", "level_3"))

cover_l2_survey <- cover_l2$survey
# ================================================================
# Checks - do these before trusting the numbers
# ================================================================

# Every image must add to 100%, in both classifications
check_sums <- dplyr::bind_rows(
  cover_l2$image %>% dplyr::mutate(classification = "level_2"),
  cover_l3$image %>% dplyr::mutate(classification = "level_2 x level_3")) %>%
  dplyr::group_by(classification, survey_id, point_media_key) %>%
  dplyr::summarise(total = sum(percent_cover), .groups = "drop") %>%
  dplyr::filter(abs(total - 100) > 0.01)

if (nrow(check_sums) > 0) {
  warning(nrow(check_sums), " images do not add to 100% - inspect `check_sums`")
} else {
  message("All images add to 100% in both classifications")
}

# The two ways of averaging images into a survey - how much do they disagree?
cover_l2$survey %>%
  dplyr::mutate(difference = abs(percent_cover - percent_cover_pooled)) %>%
  dplyr::slice_max(difference, n = 10) %>%
  dplyr::select(site_code, date, survey_id, level_2, n_images,
                percent_cover, percent_cover_pooled, difference) %>%
  print()

# ================================================================
# Morphospecies richness - how many living level_3 classes are present
# ================================================================
# Counted per IMAGE, then averaged up, exactly like % cover. Every image is a
# 20-point sample, so a mean per image is comparable between surveys regardless of
# how many images each one had annotated. (A count pooled over a whole survey would
# not be - richness climbs with the number of images, from a median of 1 class in
# 1-image surveys to 12 in 23-image surveys - which is why it isn't used here.)
#
# What counts as living: everything under level_1 "Biota". Only level_1 "Physical"
# (Substrate) is excluded, as not alive.
# Matrix (turf/sediment/silt and the algal/bryozoan matrices) IS counted - the
# decision, 16/09/2026, is that those categories all contain living organisms.
# `non_living_level_2` is left in place as the one spot to change if that is revisited.
# Classes are counted as level_2 + level_3 together, so two identically named growth
# forms under different groups can never be merged by accident.
#
# Entries scored only to level_2 ("Not identified further") are handled per image:
# they count as their own morphospecies where nothing else in the same level_2 group
# was identified in that image, and are dropped where something was. So a lone
# "Sponges > Not identified further" is one sponge morphospecies, but the same entry
# alongside "Sponges > Crusts" adds no information and is not counted again. This
# affects 199 of 6,033 images, nearly all sponges, and lifts mean richness per image
# from 2.84 to 2.87.

non_living_level_2 <- character(0)   # Matrix kept - see note above
image_cols         <- c("site_code", "date", "survey_id", "point_media_key")

living <- benthos %>%
  dplyr::filter(level_1 == "Biota",
                !level_2 %in% non_living_level_2) %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(c(image_cols, "level_2")))) %>%
  dplyr::filter(level_3 != "Not identified further" |
                !any(level_3 != "Not identified further")) %>%
  dplyr::ungroup()

# Check what is being counted, and what is being left out, before trusting the numbers
living %>% dplyr::count(level_2, level_3, name = "points") %>% print(n = Inf)

benthos %>%
  dplyr::filter(level_1 != "Biota" | level_2 %in% non_living_level_2) %>%
  dplyr::count(level_1, level_2, level_3, name = "points_excluded") %>%
  print(n = Inf)

message("Entries scored only to level_2: ",
        sum(living$level_3 == "Not identified further"),
        " counted as their own morphospecies, ",
        sum(benthos$level_1 == "Biota" &
            !benthos$level_2 %in% non_living_level_2 &
            benthos$level_3 == "Not identified further") -
          sum(living$level_3 == "Not identified further"),
        " dropped because the same group was identified in that image")

# --- 1. richness within each image ----
# Left join from every image, so an image with no living points scores 0 rather than
# dropping out of the averages (36 images are in that position).
richness_image <- benthos %>%
  dplyr::distinct(dplyr::across(dplyr::all_of(image_cols))) %>%
  dplyr::left_join(
    living %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(image_cols))) %>%
      dplyr::summarise(n_morphospecies = dplyr::n_distinct(paste(level_2, level_3)),
                       n_living_points = dplyr::n(),
                       .groups = "drop"),
    by = image_cols) %>%
  dplyr::mutate(dplyr::across(c(n_morphospecies, n_living_points),
                              ~ tidyr::replace_na(.x, 0)))

# --- 2. average across the images in a survey (transect) ----
richness_survey <- richness_image %>%
  dplyr::group_by(site_code, date, survey_id) %>%
  dplyr::summarise(
    n_images                = dplyr::n(),
    n_living_points         = sum(n_living_points),
    sd_richness             = stats::sd(n_morphospecies),
    mean_richness_per_image = mean(n_morphospecies),
    .groups = "drop") %>%
  dplyr::mutate(se_richness = sd_richness / sqrt(n_images))

# --- 3. average across the surveys at a site on that date ----
# Again site x date, not site alone.
richness_site_date <- richness_survey %>%
  dplyr::group_by(site_code, date) %>%
  dplyr::summarise(
    n_surveys               = dplyr::n(),
    n_images                = sum(n_images),
    sd_richness             = stats::sd(mean_richness_per_image),
    mean_richness_per_image = mean(mean_richness_per_image),
    .groups = "drop") %>%
  dplyr::mutate(se_richness = sd_richness / sqrt(n_surveys))

# Sanity check - this should be near zero. If it drifts up, the metric has started
# tracking how much was annotated rather than what is on the reef.
message("Correlation between images annotated and mean_richness_per_image: ",
        round(stats::cor(richness_survey$n_images,
                         richness_survey$mean_richness_per_image), 2))

richness_site_date %>%
  ggplot(aes(x = date, y = mean_richness_per_image)) +
  geom_pointrange(aes(ymin = mean_richness_per_image - se_richness,
                      ymax = mean_richness_per_image + se_richness), alpha = 0.6) +
  labs(x = NULL, y = "morphospecies per image") +
  theme_bw()

# ================================================================
# Write out
# ================================================================

write_csv(richness_image,     "data/tidy/richness_per_image.csv")
write_csv(richness_survey,    "data/tidy/richness_per_survey.csv")
write_csv(richness_site_date, "data/tidy/richness_per_site_date.csv")

write_csv(cover_l2$image,  "data/tidy/cover_level_2_per_image.csv")
write_csv(cover_l2$survey, "data/tidy/cover_level_2_per_survey.csv")
write_csv(cover_l2$site,   "data/tidy/cover_level_2_per_site_date.csv")

write_csv(cover_l3$image,  "data/tidy/cover_level_3_per_image.csv")
write_csv(cover_l3$survey, "data/tidy/cover_level_3_per_survey.csv")
write_csv(cover_l3$site,   "data/tidy/cover_level_3_per_site_date.csv")

# Wide, one row per survey - for the multivariate work in script 13 and for modelling
cover_l3_wide <- cover_l3$survey %>%
  tidyr::unite("class", level_1, level_2, level_3, sep = " > ") %>%
  dplyr::select(site_code, date, survey_id, class, percent_cover) %>%
  tidyr::pivot_wider(names_from = class, values_from = percent_cover, values_fill = 0)

write_csv(cover_l3_wide, "data/tidy/cover_level_3_per_survey_wide.csv")

# ================================================================
# A look at the result
# ================================================================

cover_l2$site %>%
  dplyr::filter(percent_cover > 0) %>%
  ggplot(aes(x = date, y = percent_cover, colour = level_2)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ level_2, scales = "free_y") +
  labs(x = NULL, y = "% cover (mean of surveys at a site)") +
  theme_bw() +
  theme(legend.position = "none")

# ================================================================
# NOTE for the modelling in script 11
# ================================================================
# Use `cover_*$survey` there, not `$site`. The site x date table is for maps, summary
# tables and plots. Feeding site means into the model throws away the transect-level
# variation and treats a site with one transect as being as well measured as a site
# with four. Keep the survey rows and let site (and date) be random effects, so the
# model sees the nesting and weights the sites accordingly.
