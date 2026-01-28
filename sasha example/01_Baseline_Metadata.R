rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)

##########################################################################################################

project.id <- "Windara_baseline_BRUVS"

##########################################################################################################

# Only works if using R studio
#setwd(getSrcDirectory()[1])
working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(working_directory)
setwd('..')
working_directory <- getwd()

# Load EventMeasure Output Files
setwd(paste(working_directory, "/data/Baseline", sep = ""))

counts <- read.csv("Windara_baseline_BRUVS_Count.csv", header = TRUE, stringsAsFactors = FALSE)
#counts$report.ID <- paste(counts$Year, counts$Sample, sep = "")
#counts <- counts %>%
# dplyr::select(-c(Sample, Year, Others))

lengths <- read.csv("Windara_baseline_BRUVS_Length.csv", header = TRUE, stringsAsFactors = FALSE)
#lengths$report.ID <- paste(lengths$Year, lengths$Sample, sep = "")
#lengths <- lengths %>%
#dplyr::select(-c(Sample, Year, Others))

metadata <- read.csv("Windara_baseline_BRUVS_Metadata.csv", header = TRUE, stringsAsFactors = FALSE)
metadata$Fish.present <- paste("Yes")

# Remove failed drops from metadata

#metadata.failsremoved <- metadata[metadata$Successful.count != "No", ]

##fix species names

#counts$Taxon <- as.character(counts$Taxon)

counts <- counts %>% mutate(Species = ifelse(Species == "sp", "spp", Species))
counts <- counts %>% mutate(Species = ifelse(Species == "", "spp", Species))
counts <- counts %>% mutate(Species = ifelse(Species == "spp.", "spp", Species))

counts <- counts %>%
  mutate(
    Genus = case_when(
      Genus == "Cheilodactylus" & Species == "nigripes" ~ "Pseudogoniistius",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

counts <- counts %>%
  mutate(
    Genus = case_when(
      Genus == "Dasyatis" & Species == "brevicaudata" ~ "Bathytoshia",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

counts <- counts %>%
  mutate(
    Genus = case_when(
      Genus == "Pagrus" & Species == "auratus" ~ "Chrysophrys",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

counts <- counts %>%
  mutate(
    Genus = case_when(
      Genus == "Pelates" & Species == "octolineatus" ~ "Helotes",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )


counts <- counts %>%
  mutate(Species = case_when(
    Genus == "Portunus" & Species == "pelagicus" ~ "armatus",    
    TRUE ~ Species),
    # Create Genus_species
    Genus_species = case_when(
      is.na(Genus) | Genus == "" ~ paste(Family, Species),
      TRUE ~ paste(Genus, Species)
    )
  )



lengths <- lengths %>% mutate(Species = ifelse(Species == "sp", "spp", Species))
lengths <- lengths %>% mutate(Species = ifelse(Species == "", "spp", Species))
lengths <- lengths %>% mutate(Species = ifelse(Species == "spp.", "spp", Species))

lengths <- lengths %>%
  mutate(
    Genus = case_when(
      Genus == "Cheilodactylus" & Species == "nigripes" ~ "Pseudogoniistius",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

lengths <- lengths %>%
  mutate(
    Genus = case_when(
      Genus == "Dasyatis" & Species == "brevicaudata" ~ "Bathytoshia",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

lengths <- lengths %>%
  mutate(
    Genus = case_when(
      Genus == "Pagrus" & Species == "auratus" ~ "Chrysophrys",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )

lengths <- lengths %>%
  mutate(
    Genus = case_when(
      Genus == "Pelates" & Species == "octolineatus" ~ "Helotes",
      TRUE ~ Genus
    ),
    Genus_species = paste(Genus, Species)
  )


lengths <- lengths %>%
  mutate(Species = case_when(
    Genus == "Portunus" & Species == "pelagicus" ~ "armatus",    
    TRUE ~ Species),
    # Create Genus_species
    Genus_species = case_when(
      is.na(Genus) | Genus == "" ~ paste(Family, Species),
      TRUE ~ paste(Genus, Species)
    )
  )




# Merge metadata with count and length data
count.metadata <- merge(counts, metadata, by = "Sample", all.x = TRUE)
length.metadata <- merge(lengths, metadata, by = "Sample", all.x = TRUE)

# Write output files
write.csv(count.metadata, file = paste(project.id, "_Count_Metadata.csv", sep = ""), row.names = FALSE, na = "")
write.csv(length.metadata, file = paste(project.id, "_Length_Metadata.csv", sep = ""), row.names = FALSE, na = "")






