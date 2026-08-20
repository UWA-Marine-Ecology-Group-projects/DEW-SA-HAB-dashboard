library(dplyr)
library(tidyr)
library(vegan)
library(ecotraj)
library(lubridate)

# ============================================================
# 1. Start with your complete M1 count dataframe
# ============================================================
sl_m1 <- readRDS("data/tidy/rls_m1_survey_list.rds") %>% 
  dplyr::mutate(id = paste(survey_id, block)) %>%
  dplyr::filter(location %in% "Encounter")

sa_sites <- sf::read_sf("dev/Dive_sites_2026_07_14.shp") %>%
  CheckEM::clean_names() %>%
  select(site_code, site_name, location_g, bruvsrepor)

unique(sa_sites$location_g)

dates <- sl_m1 %>%
  distinct(period, site_name, sampling_event, sampling_event_start_date)

m1_complete_count <- readRDS("data/tidy/rls_m1_complete_count.rds") %>%
  left_join(sa_sites) %>%
  left_join(sl_m1) %>%
  dplyr::filter(location_g %in% "Metro")  %>% # FOR TESTING
  dplyr::group_by(period, survey_id, survey_date, site_name, sampling_event, sampling_event_start_date, depth, family, genus, species, scientific) %>% # average blocks
  dplyr::summarise(total_block = mean(total)) %>%
  ungroup() %>%
  dplyr::group_by(period, site_name, sampling_event, sampling_event_start_date, family, genus, species, scientific) %>% # average site x year
  dplyr::summarise(total_site = mean(total_block)) %>%
  dplyr::filter(total_site > 1)  %>%
  mutate(
    id = paste(site_name, sampling_event, sep = "_")
  ) %>%
  ungroup() %>%
  left_join(dates)

# This is already:
# one row = survey/block x species
#
# total = abundance
# id    = paste(survey_id, block)
# scientific = species ID


# ============================================================
# 2. Create metadata for each sample/block
# ============================================================
event_dates <- sl_m1 %>%
  distinct(
    site_name,
    sampling_event,
    sampling_event_start_date
  )

m1_meta <- m1_complete_count %>%
  distinct(
    id,
    site_name,
    sampling_event,
    period
  ) %>%
  left_join(
    event_dates#,
    # by = "sampling_event"
  ) %>%
  mutate(
    survey_date = as.Date(sampling_event_start_date),
    Year = lubridate::year(survey_date)
  ) %>%
  arrange(
    site_name,
    survey_date
  )


# Check that ID is genuinely unique
stopifnot(!anyDuplicated(m1_meta$id))


# ============================================================
# 3. Make sample x species abundance matrix
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
  )

m1_assemblage <- m1_assemblage %>%
  arrange(match(id, m1_meta$id))


# Put IDs into row names
assemblage <- m1_assemblage %>%
  tibble::column_to_rownames("id") %>%
  as.data.frame()

stopifnot(
  identical(
    rownames(assemblage),
    m1_meta$id
  )
)

# ============================================================
# 4. VERY IMPORTANT: make sure assemblage and metadata align
# ============================================================


# Basic checks
str(assemblage)

sum(is.na(assemblage))
# should = 0

all(vapply(assemblage, is.numeric, logical(1)))
# should = TRUE

any(rowSums(assemblage) == 0)
# TRUE is OK here - these are your genuine zero-fish blocks


# ============================================================
# 5. log2 transformation
# ============================================================

assemblage_log2 <- vegan::decostand(
  assemblage,
  method = "log",
  logbase = 2
)

# Equivalent idea to log2(x + 1), but using vegan's
# transformation.


# ============================================================
# 6. Modified Gower distance
# ============================================================

dist_modgower <- vegan::vegdist(
  assemblage_log2,
  method = "altGower"
)

dist_modgower


# ============================================================
# 7. PERMANOVA
# ============================================================

# Your example uses:
#
# Treatment * Year
#
# But I can't see a variable literally called "Treatment"
# being retained in m1_complete_count.
#
# If "program" is the grouping/treatment variable you want:
m1_meta <- m1_meta %>%
  mutate(
    period = factor(period),
    year = factor(Year)
  )

m1_meta %>%
  summarise(
    n = n(),
    n_period_NA = sum(is.na(period)),
    n_Year_NA = sum(is.na(year))
  )

permanova <- vegan::adonis2(
  dist_modgower ~ period * year,
  data = m1_meta,
  permutations = 9 # 999
)

permanova

# pcoa_result <- trajectoryPCoA(dist_modgower, lwd = 2) # draws trajectory arrows, returns cmdscale object
library(ecotraj)

# Make sure metadata is in exactly the same order as the distance matrix
stopifnot(
  identical(
    attr(dist_modgower, "Labels"),
    m1_meta$id
  )
)

# Important: order observations within site through time
ord <- order(
  m1_meta$site_name,
  m1_meta$survey_date
)

m1_meta_traj <- m1_meta[ord, ]

# Reorder the distance matrix to match
dmat <- as.matrix(dist_modgower)

dmat <- dmat[
  m1_meta_traj$id,
  m1_meta_traj$id
]

dist_traj <- as.dist(dmat)

x <- ecotraj::defineTrajectories(
  dist_traj,
  sites = m1_meta_traj$site_name,
  surveys = as.integer(m1_meta_traj$Year)
)

surveys = as.integer(as.character(m1_meta_traj$Year))

pcoa_result <- ecotraj::trajectoryPCoA(
  x,
  lwd = 2
)
