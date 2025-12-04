### Secure access to EventMeasure or generic stereo-video annotations from Campaigns, Projects and Collaborations within GlobalArchive

### OBJECTIVES ###
# 1. use an API token to access Projects and Collaborations shared with you.
# 2. securely download any number of Campaigns within a Workgroup 
# 3. combine multiple Campaigns into single Metadata, MaxN and Length files for subsequent validation and data analysis.

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository

rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive", dependencies = TRUE) # to check for updates
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
remotes::install_github("GlobalArchiveManual/CheckEM")
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

## Save these directory names to use later----
download.dir <- paste(working.dir, "Downloads", sep = "/")

## Query from GlobalArchive----
# Load default values from GlobalArchive ----
source("https://raw.githubusercontent.com/UWAMEGFisheries/GlobalArchive/master/values.R")

# An API token allows R to communicate with GlobalArchive
# Add your personal API user token ----
API_USER_TOKEN <- "993ba5c4267b9f8cd21de73b0434c95bc72f518a4f6e725226986022"

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


write.csv(metadata, "data/raw/sa_metadata_bruv.csv", row.names = FALSE)
saveRDS(metadata, "data/raw/sa_metadata_bruv.RDS")

## Combine Points and Count files into maxn ----
# Combine all points files into one ----
points <- ga.list.files("_Points.txt") %>% 
  purrr::map_df(~ga.read.files_txt(.)) %>%
  dplyr::select(project, campaignid, sample, family, genus, species, number, stage, frame) %>%
 dplyr::mutate(number = as.numeric(number)) %>%
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
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) 

names(counts)

counts_single <- counts %>%
  dplyr::group_by(project, campaignid, sample, family, genus, species) %>%
  dplyr::slice(which.max(count))

count <- bind_rows(points_maxn, counts_single)

count_with_syn <- dplyr::left_join(count, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
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
  dplyr::mutate(species = ifelse(species %in% c("NA", "NANA", NA, "unknown", "", NULL, " ", NA_character_), "spp", as.character(species))) 

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

