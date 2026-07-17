rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(dplyr)
library(fy)
library(stringr)
library(vegan)
library(reshape)

# Set seed so random numbers remain constant
set.seed(10)

#### INPUT PARAMETERS ######################################################################

# Set the project name:
ProjectName <- "SA_MP_RLS_ALL"

############################################################################################

# Import winter site summaries

working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(working_directory)
setwd('..')
working_directory <- getwd()

#import data
setwd(paste(working_directory, "/data/RLS", sep = ""))

fish <- read.csv("ep_m1_sa.csv", header = TRUE, stringsAsFactors = FALSE)
metadata <- read.csv("SA MPA Dive Data Metadata.csv", header = TRUE, stringsAsFactors = FALSE)
crypticfish <- read.csv("ep_m2_cryptic_fish_sa.csv", header = TRUE, stringsAsFactors = FALSE)
inverts <- read.csv("ep_m2_inverts_sa.csv", header = TRUE, stringsAsFactors = FALSE)

#Add financial year
fish$Finyear <- date2fy(fish$survey_date)
crypticfish$Finyear <- date2fy(crypticfish$survey_date)
inverts$Finyear <- date2fy(inverts$survey_date)

#Add numeric year column
fish$year <- paste(fish$survey_date)
fish$year <- format(as.Date(fish$year), "%Y")                   
fish$year <- as.numeric(fish$year)

crypticfish$year <- paste(crypticfish$survey_date)
crypticfish$year <- format(as.Date(crypticfish$year), "%Y")                   
crypticfish$year <- as.numeric(crypticfish$year)

inverts$year <- paste(inverts$survey_date)
inverts$year <- format(as.Date(inverts$year), "%Y")                   
inverts$year <- as.numeric(inverts$year)

#combine metadata
metadata <- select(metadata, 1:6)

fish.metadata <- merge(fish, metadata, by = "site_code", all.x = TRUE)
crypticfish.metadata <- merge(crypticfish, metadata, by = "site_code", all.x = TRUE)
inverts.metadata <- merge(inverts, metadata, by = "site_code", all.x = TRUE)

#Write csv

write.csv(fish.metadata, file = "SA_MP_RLS_fish_metadata.csv", row.names = FALSE)
write.csv(crypticfish.metadata, file = "SA_MP_RLS_crypticfish_metadata.csv", row.names = FALSE)
write.csv(inverts.metadata, file = "SA_MP_RLS_inverts_metadata.csv", row.names = FALSE)

#update species name
fish.metadata$taxon [fish.metadata$taxon == "Pagrus auratus"] <- "Chrysophrys auratus"
fish.metadata$taxon [fish.metadata$taxon == "Pelates octolineatus"] <- "Helotes octolineatus"
fish.metadata$taxon [fish.metadata$taxon == "Ascarosepion apama"] <- "Sepia apama"
fish.metadata$taxon [fish.metadata$taxon == ""] <- "Nil"
fish.metadata$family [fish.metadata$family == "Cheilodactylidae"] <- "Latridae"
fish.metadata$family [fish.metadata$family == "Ostraciidae"] <- "Aracanidae"
fish.metadata$family [fish.metadata$family == "Odacidae"] <- "Labridae"
fish.metadata$taxon [fish.metadata$taxon == "Myliobatis australis"] <- "Myliobatis tenuicaudatus"
fish.metadata$family [fish.metadata$family == "Kyphosidae" & fish.metadata$taxon == "Tilodon sexfasciatus"] <- "Microcanthidae"
fish.metadata$family [fish.metadata$family == "Kyphosidae" & fish.metadata$taxon == "Neatypus obliquus"] <- "Microcanthidae"
fish.metadata$family [fish.metadata$family == "Kyphosidae" & fish.metadata$taxon == "Scorpis aequipinnis"] <- "Scorpididae"
fish.metadata$family [fish.metadata$family == "Kyphosidae" & fish.metadata$taxon == "Scorpis georgiana"] <- "Scorpididae"
fish.metadata$family [fish.metadata$family == "Kyphosidae" & fish.metadata$taxon == "Girella zebra"] <- "Girellidae"
fish.metadata$taxon [fish.metadata$taxon == "Apogon victoriae"] <- "Ostorhinchus victoriae"


crypticfish.metadata$taxon [crypticfish.metadata$taxon == ""] <- "Nil"
crypticfish.metadata$family [crypticfish.metadata$family == "Scorpaenidae" & crypticfish.metadata$taxon == "Glyptauchen panduratus"] <- "Tetrarogidae"
crypticfish.metadata$family [crypticfish.metadata$family == "Scorpaenidae" & crypticfish.metadata$taxon == "Helicolenus percoides"] <- "Sebastidae"
crypticfish.metadata$family [crypticfish.metadata$family == "Scorpaenidae" & crypticfish.metadata$taxon == "Neosebastes scorpaenoides"] <- "Neosebastidae"

inverts.metadata$taxon [inverts.metadata$taxon == ""] <- "Nil"
inverts.metadata$taxon [inverts.metadata$taxon == "Ascarosepion apama"] <- "Sepia apama"

#remove taxon
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Heteroclinus tristis"),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Ophiclinus spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Labrid spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Monacanthid spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Siphonognathus spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Pempheris spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Sillaginid spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Tripterygiid spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Actinopterygii spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Clupeiformes spp."),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Meridiastra gunnii"),]
fish.metadata <- fish.metadata[-which(fish.metadata$taxon == "Pleuroploca australasia"),]

crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Blenniid spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Clinid spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Cristiceps spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Gobiid spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Heterodontus portusjacksoni egg"),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Parascyllium spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Pempheris spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Scorpaenid spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Norfolkia spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Tripterygiid spp."),]
crypticfish.metadata <- crypticfish.metadata[-which(crypticfish.metadata$taxon == "Actinopterygii spp."),]

inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Aeolidiid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Meridiastra spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Cypraeid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Facelinid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Holothuriid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Naxia spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Mitrid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Muricid spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Pterynotus spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Decapoda spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Nectocarcinus spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Cabestana spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Crinoidea spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Gastropoda spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Malacostraca spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Nudibranchia spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Platyhelminthes spp."),]
inverts.metadata <- inverts.metadata[-which(inverts.metadata$taxon == "Pycnogonida spp."),]

#Aggregate similar species

fish.metadata$taxon [fish.metadata$taxon == "Pseudocaranx georgianus"] <- "Pseudocaranx spp."
fish.metadata$taxon [fish.metadata$taxon == "Pseudocaranx wrighti"] <- "Pseudocaranx spp."
fish.metadata$taxon [fish.metadata$taxon == "Clinid spp."] <- "Heteroclinus spp."
fish.metadata$taxon [fish.metadata$taxon == "Heteroclinus johnstoni"] <- "Heteroclinus spp."
fish.metadata$taxon [fish.metadata$taxon == "Cochleoceps bicolor"] <- "Gobiesocidae spp."
fish.metadata$taxon [fish.metadata$taxon == "Gobiesocid spp."] <- "Gobiesocidae spp."
fish.metadata$taxon [fish.metadata$taxon == "Nesogobius spp."] <- "Gobiidae spp."
fish.metadata$taxon [fish.metadata$taxon == "Gobiid spp."] <- "Gobiidae spp."
fish.metadata$taxon [fish.metadata$taxon == "Clupeid spp."] <- "Clupeidae spp."
fish.metadata$taxon [fish.metadata$taxon == "Urolophus gigas"] <- "Urolophus spp."
fish.metadata$taxon [fish.metadata$taxon == "Atherinid spp."] <- "Atherinidae spp."

crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus adelaidae"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus johnstoni"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus kuiteri"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus perspicillatus"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus roseus"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Heteroclinus tristis"] <- "Heteroclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Ophiclinus gracilis"] <- "Ophiclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Ophiclinus ningulus"] <- "Ophiclinus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Aspasmogaster tasmaniensis"] <- "Aspasmogaster spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Nesogobius sp. 4 [groovedcheek]"] <- "Nesogobius spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Platycephalid spp."] <- "Platycephalus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Platycephalus laevigatus"] <- "Platycephalus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Scorpaena papillosa [cf]"] <- "Scorpaena papillosa"
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Urolophus gigas"] <- "Urolophus spp."
crypticfish.metadata$taxon [crypticfish.metadata$taxon == "Ophichthid spp."] <- "Ophichthidae spp."

inverts.metadata$taxon [inverts.metadata$taxon == "Comanthus tasmaniae"] <- "Comanthus spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Coryphellina rubrolineata"] <- "Flabellinidae spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Flabellina spp."] <- "Flabellinidae spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Pagurus sinuatus"] <- "Paguridae spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Pagurid spp."] <- "Paguridae spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Portunus pelagicus"] <- "Portunus armatus"
inverts.metadata$taxon [inverts.metadata$taxon == "Pseudoceros lividus"] <- "Pseudobiceros spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Holopneustes porosissimus"] <- "Holopneustes spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Amblypneustes elevatus"] <- "Amblypneustes spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Amblypneustes ovum"] <- "Amblypneustes spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Amblypneustes pallidus"] <- "Amblypneustes spp."
inverts.metadata$taxon [inverts.metadata$taxon == "Pyura australis"] <- "Pyura spp."


#remove 0s

fish.metadata <- fish.metadata[-which(fish.metadata$size_class == "0" & fish.metadata$taxon != "Nil"),]
fish.metadata$total [fish.metadata$total == "1" & fish.metadata$taxon == "Nil"] <- "0"


crypticfish.metadata$total [crypticfish.metadata$total == "1" & crypticfish.metadata$taxon == "Nil"] <- "0"

inverts.metadata <- inverts.metadata[-which(inverts.metadata$total == "0" & inverts.metadata$taxon != "Nil"),]
inverts.metadata$total [inverts.metadata$total == "1" & inverts.metadata$taxon == "Nil"] <- "0"


#amend cryptic fish 0's for small species
crypticfish.metadata$size_class [crypticfish.metadata$size_class == "0" & crypticfish.metadata$taxon == "Cochleoceps bicolor"] <- "2.5"

crypticfish.metadata$size_class [crypticfish.metadata$size_class == "0" & crypticfish.metadata$taxon == "Helcogramma decurrens"] <- "2.5"
crypticfish.metadata$size_class [crypticfish.metadata$size_class == "0" & crypticfish.metadata$taxon == "Siphamia cephalotes"] <- "2.5"
crypticfish.metadata$size_class [crypticfish.metadata$size_class == "0" & crypticfish.metadata$taxon == "Trinorfolkia cristata"] <- "2.5"

#Amend lobster sizes
inverts.metadata$size_class [inverts.metadata$size_class == "87.5" & inverts.metadata$taxon == "Jasus edwardsii"] <- "7.5"
inverts.metadata$size_class [inverts.metadata$size_class == "100" & inverts.metadata$taxon == "Jasus edwardsii"] <- "8"
inverts.metadata$size_class [inverts.metadata$size_class == "125" & inverts.metadata$taxon == "Jasus edwardsii"] <- "9"
inverts.metadata$size_class [inverts.metadata$size_class == "150" & inverts.metadata$taxon == "Jasus edwardsii"] <- "10"
inverts.metadata$size_class [inverts.metadata$size_class == "175" & inverts.metadata$taxon == "Jasus edwardsii"] <- "11"


#reset total to numeric
fish.metadata$total <- as.numeric(fish.metadata$total)
crypticfish.metadata$total <- as.numeric(crypticfish.metadata$total)
inverts.metadata$total <- as.numeric(inverts.metadata$total)

inverts.metadata$size_class <- as.numeric(inverts.metadata$size_class)

#Calculate biomass for 0 size class cryptic
# Define biomass values for the species that require calculations
taxon_b <- list("Helcogramma decurrens" = 0.162006904383042, 
                "Trinorfolkia cristata" = 0.162006904383042,
                "Cochleoceps bicolor" = 0.119844500824995, 
                "Siphamia cephalotes" = 0.451609599828783)

# Function to calculate biomass
calc_biomass <- function(total, taxon) {
  biomass_g <- taxon_b[[taxon]]  # Get the biomass multiplier for the species
  return(total * biomass_g)  # Calculate the biomass
}

# Apply calculation to update NA biomass values for the species Cochleoceps bicolor
#crypticfish.metadata$biomass[is.na(crypticfish.metadata$biomass) & 
                               #crypticfish.metadata$size_class == "2.5" & 
                               #crypticfish.metadata$taxon == "Cochleoceps bicolor"] <- 
 # calc_biomass(crypticfish.metadata$total, 
              # "Cochleoceps bicolor")

# Loop through each species in the taxon_b group and update the biomass
for (taxon in names(taxon_b)) {
  
  # Identify the rows where biomass is NA and the species matches
  idx <- which(is.na(crypticfish.metadata$biomass) & 
                 crypticfish.metadata$size_class == "2.5" & 
                 crypticfish.metadata$taxon == taxon)
  
  # Check the selected indices for debugging (optional)
  print(paste("Processing species:", taxon, "Rows to update:", length(idx)))
  
  # If there are rows that meet the conditions, update them
  if (length(idx) > 0) {
    crypticfish.metadata$biomass[idx] <- calc_biomass(crypticfish.metadata$total[idx], taxon)
  }
}


#filter sites (MPA only) and years (>2012/2013)

fish.metadata_Filtered <- filter(fish.metadata, Finyear >= "2012-13")
fish.metadata_Filtered <- filter(fish.metadata_Filtered, Data.Type != "")
fish.metadata_Filtered <- filter(fish.metadata_Filtered, Data.Type != "Other")

crypticfish.metadata_Filtered <- filter(crypticfish.metadata, Finyear >= "2012-13")
crypticfish.metadata_Filtered <- filter(crypticfish.metadata_Filtered, Data.Type != "")
crypticfish.metadata_Filtered <- filter(crypticfish.metadata_Filtered, Data.Type != "Other")

inverts.metadata_Filtered <- filter(inverts.metadata, Finyear >= "2012-13")
inverts.metadata_Filtered <- filter(inverts.metadata_Filtered, Data.Type != "")
inverts.metadata_Filtered <- filter(inverts.metadata_Filtered, Data.Type != "Other")

#remove sites with less than 4 transects
setwd(paste(working_directory, "/data/RLS", sep = ""))
surveystoremove <- read.csv("surveystoremove.csv", header = TRUE, stringsAsFactors = FALSE)

fish.metadata_Filtered <- fish.metadata_Filtered %>% filter(!survey_id %in% surveystoremove$survey_id)
crypticfish.metadata_Filtered <- crypticfish.metadata_Filtered %>% filter(!survey_id %in% surveystoremove$survey_id)
inverts.metadata_Filtered <- inverts.metadata_Filtered %>% filter(!survey_id %in% surveystoremove$survey_id)

#Write csv

write.csv(fish.metadata_Filtered, file = "SA_MP_RLS_fish_metadata_filtered.csv", row.names = FALSE)
write.csv(crypticfish.metadata_Filtered, file = "SA_MP_RLS_crypticfish_metadata_filtered.csv", row.names = FALSE)
write.csv(inverts.metadata_Filtered, file = "SA_MP_RLS_inverts_metadata_filtered.csv", row.names = FALSE)

#abalone converts

inverts.metadata_Filtered_Allabspp <- inverts.metadata_Filtered
inverts.metadata_Filtered_Oneab <- inverts.metadata_Filtered

inverts.metadata_Filtered_Allabspp <- inverts.metadata_Filtered_Allabspp[-which(inverts.metadata_Filtered_Allabspp$taxon == "Haliotis spp."),]

inverts.metadata_Filtered_Oneab$taxon [inverts.metadata_Filtered_Oneab$taxon == "Haliotis cyclobates"] <- "Haliotis spp."
inverts.metadata_Filtered_Oneab$taxon [inverts.metadata_Filtered_Oneab$taxon == "Haliotis laevigata"] <- "Haliotis spp."
inverts.metadata_Filtered_Oneab$taxon [inverts.metadata_Filtered_Oneab$taxon == "Haliotis roei"] <- "Haliotis spp."
inverts.metadata_Filtered_Oneab$taxon [inverts.metadata_Filtered_Oneab$taxon == "Haliotis rubra"] <- "Haliotis spp."
inverts.metadata_Filtered_Oneab$taxon [inverts.metadata_Filtered_Oneab$taxon == "Haliotis scalaris"] <- "Haliotis spp."


#calculate biomass for abs and lobster

abalone_biomass <- function(total, size_class) {
  # Check for NA values and filter them out
  valid_indices <- !is.na(total) & !is.na(size_class) & total >= 0 & size_class >= 0
  total <- total[valid_indices]
  size_class <- size_class[valid_indices]
  
  # Constants for the biomass equation
  a <- 0.1526
  b <- 2.97
  biomass_g <- a * (size_class^b)
  
  # Calculate total biomass
  total_biomass <- total * biomass_g
  return(total_biomass)
}

# Apply the function to your dataset
inverts.metadata_Filtered_Oneab$biomass[inverts.metadata_Filtered_Oneab$taxon == "Haliotis spp."] <- 
  abalone_biomass(inverts.metadata_Filtered_Oneab$total[inverts.metadata_Filtered_Oneab$taxon == "Haliotis spp."], 
                  inverts.metadata_Filtered_Oneab$size_class[inverts.metadata_Filtered_Oneab$taxon == "Haliotis spp."])


# Function to calculate biomass for Southern Rock Lobster
lobster_biomass <- function(total, size_class) {
  valid_indices <- !is.na(total) & !is.na(size_class) & total >= 0 & size_class >= 0
  total <- total[valid_indices]
  size_class <- size_class[valid_indices]
  
  # Constants for the biomass equation (adjust if necessary)
  a <- 0.000271
  b <- 3.135
  biomass_g <- a * ((size_class*10)^b)
  
  # Calculate total biomass
  total_biomass <- total * biomass_g
  return(total_biomass)
}
inverts.metadata_Filtered_Oneab$biomass[inverts.metadata_Filtered_Oneab$taxon == "Jasus edwardsii"] <- 
  lobster_biomass(inverts.metadata_Filtered_Oneab$total[inverts.metadata_Filtered_Oneab$taxon == "Jasus edwardsii"], 
                  inverts.metadata_Filtered_Oneab$size_class[inverts.metadata_Filtered_Oneab$taxon == "Jasus edwardsii"])


write.csv(inverts.metadata_Filtered_Allabspp, file = "SA_MP_RLS_inverts_metadata_filtered_Allabspp.csv", row.names = FALSE)
write.csv(inverts.metadata_Filtered_Oneab, file = "SA_MP_RLS_inverts_metadata_filtered_Oneab.csv", row.names = FALSE)


##compare species lists

# Select only the taxon (species) and total (sightings) columns
fish.metadata_short <- fish.metadata_Filtered[, c("taxon", "total", "survey_date")]
crypticfish.metadata_short <- crypticfish.metadata_Filtered[, c("taxon", "total", "survey_date")]
inverts.metadata_short <- inverts.metadata_Filtered[, c("taxon", "total", "survey_date")]

# Sum sightings per species for each dataset
summed_datasetfish <- aggregate(total ~ taxon, data = fish.metadata_short, sum)
summed_datasetcryptic <- aggregate(total ~ taxon, data = crypticfish.metadata_short, sum)
summed_datasetinverts <- aggregate(total ~ taxon, data = inverts.metadata_short, sum)

# Merge datasets on Species to compare sightings
merged_data <- merge(summed_datasetfish, summed_datasetcryptic, by = "taxon", all = TRUE, suffixes = c("_fish", "_crypticfish"))
merged_dataInvert <- merge(summed_datasetfish, summed_datasetinverts, by = "taxon", all = TRUE, suffixes = c("_fish", "_inverts"))

# Replace NA with 0 for missing sightings
merged_data[is.na(merged_data)] <- 0
merged_dataInvert[is.na(merged_dataInvert)] <- 0

# Check for common species with sightings on the same date
common_dates <- merge(fish.metadata_short, crypticfish.metadata_short, by = c("taxon", "survey_date"), suffixes = c("_fish", "_crypticfish"))

# Sum the sightings on the common dates
common_dates_summed <- aggregate(cbind(total_fish, total_crypticfish) ~ taxon + survey_date, data = common_dates, sum)


# Identify common and unique species
common_species <- merged_data[merged_data$total_fish > 0 & merged_data$total_crypticfish > 0, ]
unique_to_fish <- merged_data[merged_data$total_fish > 0 & merged_data$total_crypticfish == 0, ]
unique_to_crypticfish <- merged_data[merged_data$total_fish == 0 & merged_data$total_crypticfish > 0, ]

common_speciesI <- merged_dataInvert[merged_dataInvert$total_fish > 0 & merged_dataInvert$total_inverts > 0, ]
#unique_to_fish <- merged_data[merged_data$total_fish > 0 & merged_data$total_crypticfish == 0, ]
#unique_to_crypticfish <- merged_data[merged_data$total_fish == 0 & merged_data$total_crypticfish > 0, ]


# If you want to save these results to a file, you can use write.csv
write.csv(data.frame(Common_Species = common_species), "common_species.csv", row.names = FALSE)
write.csv(data.frame(Unique_to_fish = unique_to_fish), "unique_to_fish.csv", row.names = FALSE)
write.csv(data.frame(Unique_to_crypticfish = unique_to_crypticfish), "unique_to_crypticfish.csv", row.names = FALSE)
write.csv(data.frame(common_dates_summed = common_dates_summed), "common_dates_summed.csv", row.names = FALSE)
write.csv(data.frame(Common_SpeciesI = common_speciesI), "common_speciesInverts.csv", row.names = FALSE)



# Convert to character vectors
species_listfish <- as.character(fish.metadata_Filtered$taxon)
species_listcrypticfish <- as.character(crypticfish.metadata_Filtered$taxon)

# Find common species
common_species <- intersect(species_listfish, species_listcrypticfish)

# Find species unique to each dataset
unique_to_fish <- setdiff(species_listfish, species_listcrypticfish)
unique_to_crypticfish <- setdiff(species_listcrypticfish, species_listfish)          
          
