### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Workgroup 
# 3. combine multiple Campaigns into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

# rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
# library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE) # to check for updates
library(GlobalArchive)
library(httr)
library(jsonlite)
library(R.utils)
# To connect to GitHub
library(RCurl)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)

ga.read.files_csv <- function(flnm) {
  read_csv(flnm,col_types = cols(.default = "c"))%>%
    ga.clean.names() %>%
    dplyr::select(-c(any_of(c("campaignid")))) %>%
    dplyr::mutate(campaign.naming=str_replace_all(flnm,paste(download.dir,"/",sep=""),""))%>%
    tidyr::separate(campaign.naming,into=c("project","campaignid"),sep="/", extra = "drop", fill = "right")%>%
    plyr::rename(., replace = c(opcode="sample"),warn_missing = FALSE)
}

## Set your working directory ----
working.dir <- "data/GlobalArchive" # to directory of current file - or type your own

dir.create(working.dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(working.dir, "Downloads"), recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

## Save these directory names to use later----
download.dir <- paste(working.dir, "Downloads", sep = "/")

## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")

# An API token allows R to communicate with GlobalArchive
# Add your personal API user token ----
API_USER_TOKEN <- Sys.getenv("API_USER_TOKEN")
if (API_USER_TOKEN == "") stop("API_USER_TOKEN is missing (set as GitHub repo secret).")

## Download data ----
# takes 6 minutes to run - turn on again to refresh the data
ga.get.campaign.list(API_USER_TOKEN, process_campaign_object,
                     q = ga.query.workgroup("SA+Dashboard"))

# Combine all downloaded data----
# Your data is now downloaded into many folders within the 'Downloads' folder. (You can open File Explorer or use the Files Pane to check)
# The below code will go into each of these folders and find all files that have the same ending (e.g. "_Metadata.csv") and bind them together.
# The end product is three data frames; metadata, maxn and length.

metadata <- ga.list.files("_Metadata.csv") %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_csv(.)) %>% # combine into dataframe
  dplyr::select(project, campaignid, sample, latitude, longitude, date, time, location, status, site, depth, observer, successful.count, successful.length, successful.count, successful.length) %>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
  # dplyr::mutate(successful.count = if_else(is.na(successful.count), successful_count, successful.count)) %>%
  # dplyr::mutate(successful.length = if_else(is.na(successful.length), successful_length, successful.length)) %>%
  # dplyr::select(!c(successful_count, successful_length)) %>%
  glimpse()

unique(metadata$project) %>% sort() # 4 projects
unique(metadata$campaignid)  %>% sort() # 26 campaigns 
unique(metadata$location)
unique(metadata$sample)

write.csv(metadata, "data/raw/sa_metadata_bruv.csv", row.names = FALSE)
saveRDS(metadata, "data/raw/sa_metadata_bruv.RDS")

## Combine Points and Count files into maxn ----
# Combine all points files into one ----
points <- ga.list.files("_Points.txt") %>% 
  purrr::map_df(~ga.read.files_txt(.)) %>%
  dplyr::select(project, campaignid, sample, family, genus, species, number, stage, frame) %>%
 dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", 
                                                "NANA", 
                                                NA, 
                                                "unknown", 
                                                "", NULL, 
                                                " ", 
                                                NA_character_, 
                                                "sp", 
                                                "spp."), "spp", as.character(species)))  %>%
  glimpse()

names(points)

# If there are points then turn the next chunk 

# # Turn points into MaxN
points_maxn <- points %>%
  dplyr::group_by(project, campaignid, sample, family, genus, species, frame) %>% # TODO have removed stage, but will need to go back and fix this for the campaigns that have MaxN'd by stage
  dplyr::summarise(count = sum(number)) %>%
  dplyr::slice(which.max(count))

# Read in count data ----
counts <- ga.list.files("_Count.csv") %>% 
  purrr::map_df(~ga.read.files_csv(.)) %>%
  dplyr::select(project, campaignid, sample, family, genus, species, count) %>% # , stage
  dplyr::mutate(count = as.numeric(count)) %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", 
                                                "NANA", 
                                                NA, 
                                                "unknown", 
                                                "", NULL, 
                                                " ", 
                                                NA_character_, 
                                                "sp", 
                                                "spp."), "spp", as.character(species))) 

# TODO check with the 

unique(counts$species) %>% sort()

names(counts)

counts_single <- counts %>%
  dplyr::group_by(project, campaignid, sample, family, genus, species) %>%
  dplyr::slice(which.max(count))

count <- bind_rows(points_maxn, counts_single) %>%
  ungroup() %>%
  dplyr::mutate(genus = case_when(
    genus %in% "Pagrus" ~ "Chrysophrys",
    genus %in% "Unidentified" ~ "Unknown",
    genus %in% "Cheilodactylus" ~ "Pseudogoniistius",
    genus %in% "Dasyatis" ~ "Bathytoshia",
    genus %in% "Pelates" & species %in% "octolineatus" ~ "Helotes",
    genus %in% "Unknown" ~ family,
    .default = genus
  )) %>%
  
  dplyr::mutate(species = case_when(
    species %in% c("sp.", "fish sp.", "fish sp. 1") ~ "spp",
    genus %in% "Portunus" & species %in% "pelagicus" ~ "armatus",    
    .default = species
  )) %>%
  
  dplyr::mutate(genus_species = paste(genus, species)) %>%
  glimpse()

# Test which species are missing ----
# Read in DEWs sheet ----
# dew_species <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing")
dew_species <- read_csv("data/lookups/SA-HAB-Functional Traits.csv")

# Find which ones are missing ----
unique_species <- count %>%

  distinct(family, genus, species, genus_species) %>%
  glimpse()

missing_from_list <- anti_join(unique_species, dew_species)

# # Add to google sheet
to_add <- missing_from_list %>%
  select(genus_species)

# sheet_append("https://docs.google.com/spreadsheets/d/1UN03pLMRCRsfRfZXnhY6G4UqWznkWibBXEmi5SBaobE/edit?usp=sharing", to_add)
# 
# 

# Synoynms -----
# TODO - brooke to figure out if she needs to do this or not - not sure why in 3rd person
count_with_syn <- dplyr::left_join(count, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  mutate(species = case_when(
    genus == "Portunus" & species == "pelagicus" ~ "armatus",    
    .default =  species)) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) 

# Test which metadata files do not have a match in count
missing_metadata <- anti_join(count, metadata) # TODO chase these up with Sasha if they are being used

# Test which count files do not have a match in metadata
missing_count <- anti_join(metadata, count)

# Save count file ----
write.csv(count_with_syn, "data/raw/sa_count_bruv.csv", row.names = FALSE)
saveRDS(count_with_syn, "data/raw/sa_count_bruv.RDS")

unique(count$project) %>% sort() # 4 projects
unique(count$campaignid) # 26 campaigns

## Combine Length, Lengths and 3D point files into length3dpoints----
gen_length <- ga.list.files("_Length.csv") %>% 
  purrr::map_df(~ga.read.files_csv(.)) %>%
  dplyr::select(project, campaignid, sample, family, genus, species, count, length) %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_, "sp", "spp."), "spp", as.character(species))) 

em_length <- ga.list.files("_Lengths.txt") %>%
  purrr::map_df(~ga.read.files_txt(.)) %>%
  dplyr::rename(count = number) %>%
  dplyr::select(project, campaignid, sample, family, genus, species, count, length, range, precision, rms) %>%
  dplyr::mutate(family = ifelse(family %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(family))) %>%
  dplyr::mutate(genus = ifelse(genus %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "Unknown", as.character(genus))) %>%
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) 
# 
# em_threedpoints <- ga.list.files("_3DPoints.txt") %>% 
#   purrr::map_df(~ga.read.files_txt(.)) %>%
#   dplyr::rename(count = number)%>%
#   dplyr::select(project, campaignid, sample, family, genus, species, count, range, rms)
# 
# length3dpoints <- bind_rows(em_length, em_threedpoints, gen_length) %>%
#   # dplyr::inner_join(metadata) %>%
#   # dplyr::filter(successful.length %in% "Yes") %>%
#   glimpse()
# 
# unique(length3dpoints$project) %>% sort() # 49 projects
# unique(length3dpoints$campaignid) # 91 campaigns with length

length_with_syn <- dplyr::left_join(gen_length, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  dplyr::mutate(scientific = paste(family, genus, species))

## Save length files ----
write.csv(length_with_syn, "data/raw/sa_length_bruv.csv", row.names = FALSE)
saveRDS(length_with_syn, "data/raw/sa_length_bruv.RDS")

