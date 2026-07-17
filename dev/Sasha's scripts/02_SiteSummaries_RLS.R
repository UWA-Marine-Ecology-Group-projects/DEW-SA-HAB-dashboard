rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(vegan)
library(reshape)
library(dplyr)
library(plyr)

# Set seed so random numbers remain constant
set.seed(10)

#### INPUT PARAMETERS ######################################################################

# Set the project name:
ProjectName <- "SA_MP_RLS_ALL"

############################################################################################

# Only works if using R studio
#setwd(getSrcDirectory()[1])
working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(working_directory)
setwd('..')
working_directory <- getwd()

# Import Data
setwd(paste(working_directory, "/data/RLS", sep = ""))
counts_orig <- read.csv("SA_MP_RLS_fish_metadata_filtered.csv", header = TRUE, stringsAsFactors = FALSE)
counts_orig$report.ID <- paste(counts_orig$year, counts_orig$site_code, sep = "_")

counts_orig$month <- paste(counts_orig$survey_date)
counts_orig$month <- format(as.Date(counts_orig$month), "%m")

counts_orig$report.ID_m <- paste(counts_orig$year, counts_orig$site_code, counts_orig$month, sep = "_")


counts <- counts_orig
#setwd(paste(working_directory, "/analysis/biomass", sep = ""))
#biomass <- read.csv("SA_MP_BRUVS_ALL_TotalBiomass.csv", header = TRUE, stringsAsFactors = FALSE)
#biomass.w.pseudolengths <- read.csv("SA_MP_BRUVS_ALL_Length_Biomass_IncPseudoBiomass.csv", header = TRUE, stringsAsFactors = FALSE)

#setwd(paste(working_directory, "/data", sep = ""))
#metadata <- read.csv("combined.metadata.csv", header = TRUE, stringsAsFactors = FALSE)
#metadata$report.ID <- paste(metadata$Year, metadata$Sample, sep = "_")

#filter year

#counts <- filter(counts, Finyear >= "2015-16")

#Set-up metadata

metadata <- counts_orig %>%
  select(1:18, 20, 22:25, 28, 30, 33:42) %>%
  unique(, keep_all = FALSE)

metadata_simple <- metadata %>%
  select(!20:25) %>%
  unique(, keep_all = FALSE)

metadata_tran <- counts_orig %>%
  select(1, 3:10, 13:14, 20, 33:42) %>%
  unique(, keep_all = FALSE, na.rm = FALSE)

metadata_tran <- metadata_tran %>%
  drop_na(13)
  #filter(, Data.Type != "Other")

n_distinct(metadata_tran$report.ID_m)

n_occur <- data.frame(table(metadata_tran$report.ID_m))

#combine block for each transect

combineblock <- counts %>%
  group_by(survey_id, taxon, size_class) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    biomass = sum(biomass, na.rm = TRUE),
)

counts <- merge(combineblock, metadata, by = c('survey_id','taxon','size_class'), all.y = FALSE)


#### Calculate species richness of the entire assemblage ####

SpeciesRichness <- counts %>%
  dplyr::select(c(survey_id, taxon)) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(SpeciesRichness = n_distinct(taxon))
head(SpeciesRichness,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness <- counts %>%
  dplyr::select(c(survey_id, family))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(FamilyRichness = n_distinct(family))
head(FamilyRichness,2)


#### Calculate the total abundance at each site ####

TotalAbundance <- counts %>%
  dplyr::select(c(survey_id, total))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalAbundance = sum(total))
head(TotalAbundance,2)


#### Calculate the total biomass at each site ####

TotalBiomass <- counts %>%
  dplyr::select(c(survey_id, biomass))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalBiomass = sum(biomass))
head(TotalBiomass,2)


#### Calculate the Shannon-Wiener Index (H') of each site ####

Shannon.Wiener.Diversity.Index <- counts %>%
  dplyr::select(c(survey_id,taxon,total))

Shannon.Wiener.Diversity.Index.Pivot<- cast(Shannon.Wiener.Diversity.Index, survey_id ~ taxon, sum, value = "total")

Shannon.Wiener.Diversity.Index.Pivot$Shannon.Wiener.Diversity.Index <- diversity(Shannon.Wiener.Diversity.Index.Pivot, index="shannon")

Shannon.Wiener.Diversity.Index.Output <- Shannon.Wiener.Diversity.Index.Pivot %>%
  dplyr::select(c(survey_id, Shannon.Wiener.Diversity.Index))


#### Calculate abundance of individual species for each site ####

Abundance.Species <- counts %>%
  dplyr::select(c(survey_id,taxon,total))

Abundance.Species.Pivot <- cast(Abundance.Species, survey_id ~ taxon, sum, value = "total")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
  #Abundance.Species.Pivot$`Platycephalus sp2` +
  #Abundance.Species.Pivot$`Platycephalus sp3` +
  #Abundance.Species.Pivot$`Platycephalus sp4`


#### Calculate biomass of individual species for each site ####

Biomass.Species <- counts %>%
  dplyr::select(c(survey_id,taxon,biomass))

Biomass.Species.Pivot <- cast(Biomass.Species, survey_id ~ taxon, sum, value = "biomass")

#Biomass.Species.Pivot$`All Platycephalus sp.` <- Biomass.Species.Pivot$`Platycephalus sp1` +
  #Biomass.Species.Pivot$`Platycephalus sp2` +
  #Biomass.Species.Pivot$`Platycephalus sp3` +
  #Biomass.Species.Pivot$`Platycephalus sp4`

#### Make presence absence summary ####
Common.Species <- counts %>%
  dplyr::select(c(survey_id,taxon,total)) %>%
  group_by(taxon) %>%
  tally()


#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, Abundance.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master6 <- (merge(Abundance.temp.Master5, metadata_simple, by = "survey_id", all.x = TRUE, sort = TRUE))


#### Merge all summaries together to make output site summaries table ####

Biomass.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master2 <- (merge(Biomass.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master3 <- (merge(Biomass.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master4 <- (merge(Biomass.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master5 <- (merge(Biomass.temp.Master4, Biomass.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master6 <- (merge(Biomass.temp.Master5, metadata_simple, by = "survey_id", all.x = TRUE, sort = TRUE))



# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))

write.csv(Abundance.temp.Master6, file = paste(ProjectName, "_AbundanceSiteSummaries_full.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Biomass.temp.Master6, file = paste(ProjectName, "_BiomassSiteSummaries_full.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Common.Species, file = paste(ProjectName, "_MostCommonSp_full.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
#Abundance.temp.Master6e <- filter(Abundance.temp.Master6, mpa == "Encounter Marine Park")
#Biomass.temp.Master6e <- filter(Biomass.temp.Master6, mpa == "Encounter Marine Park")

#write.csv(Abundance.temp.Master6e, file = paste(ProjectName, "_AbundanceSiteSummaries_encounter.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Master6e, file = paste(ProjectName, "_BiomassSiteSummaries_encounter.csv", sep = ""), row.names = FALSE, na = "")

#create average values per site across the four transects


Abundance.temp.Master.7 <- Abundance.temp.Master6 

combinedepth <- Abundance.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:105, mean, na.rm = TRUE))

Abundance.temp.Master.8 <- (merge(combinedepth, metadata_tran, by = "report.ID_m", all.x = TRUE, sort = TRUE))


if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
write.csv(Abundance.temp.Master.8, file = paste(ProjectName, "_AbundanceSiteSummaries_Average_full.csv", sep = ""), row.names = FALSE, na = "")

Biomass.temp.Master.7 <- Biomass.temp.Master6 

combinedepthB <- Biomass.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:105, mean, na.rm = TRUE))

Biomass.temp.Master.8 <- (merge(combinedepthB, metadata_tran, by = "report.ID_m", all.x = TRUE, sort = TRUE))

write.csv(Biomass.temp.Master.8, file = paste(ProjectName, "_BiomassSiteSummaries_Average_full.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
#Abundance.temp.Master8e <- filter(Abundance.temp.Master.8, mpa == "Encounter Marine Park")
#Biomass.temp.Master8e <- filter(Biomass.temp.Master.8, mpa == "Encounter Marine Park")

#write.csv(Abundance.temp.Master8e, file = paste(ProjectName, "_AbundanceSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Master8e, file = paste(ProjectName, "_BiomassSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")


##Big Fish filter

lengths20 <- filter(counts, size_class >= 20)


SpeciesRichness20 <- lengths20 %>%
  dplyr::select(c(survey_id, taxon)) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(SpeciesRichness20 = n_distinct(taxon))
head(SpeciesRichness20,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness20 <- lengths20 %>%
  dplyr::select(c(survey_id, family))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(FamilyRichness20 = n_distinct(family))
head(FamilyRichness20,2)


#### Calculate the total abundance at each site ####

TotalAbundance20 <- lengths20 %>%
  dplyr::select(c(survey_id, total))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalAbundance20 = sum(total))
head(TotalAbundance20,2)


#### Calculate the total biomass at each site ####

TotalBiomass20 <- lengths20 %>%
  dplyr::select(c(survey_id, biomass))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalBiomass20 = sum(biomass))
head(TotalBiomass20,2)

#### Calculate abundance of individual species for each site ####

Abundance.Species20 <- lengths20 %>%
  dplyr::select(c(survey_id,taxon,total))

Abundance.Species.Pivot20 <- cast(Abundance.Species20, survey_id ~ taxon, sum, value = "total")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
#Abundance.Species.Pivot$`Platycephalus sp2` +
#Abundance.Species.Pivot$`Platycephalus sp3` +
#Abundance.Species.Pivot$`Platycephalus sp4`


#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness20, FamilyRichness20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Abundance.Species.Pivot20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, metadata_simple, by = "survey_id", all.x = TRUE, sort = TRUE))


# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))

write.csv(Abundance.temp.Master5, file = paste(ProjectName, "_AbundanceSiteSummariesB20_full.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(lengths20, file = paste(ProjectName, "SA_MP_BRUVS_ALL_Length_Biomass_Metadata_fuctional_20.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
#Abundance.temp.Master5e <- filter(Abundance.temp.Master5, mpa == "Encounter Marine Park")
#Biomass.temp.Master8e <- filter(Biomass.temp.Master8, mpa == "Encounter Marine Park")

#write.csv(Abundance.temp.Master5e, file = paste(ProjectName, "_AbundanceSiteSummariesB20_encounter.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Master8e, file = paste(ProjectName, "_BiomassSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")


#create average values per site across the four transects


Abundance.temp.Master.7 <- Abundance.temp.Master5 

combinedepth <- Abundance.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:72, mean, na.rm = TRUE))

Abundance.temp.Master.8 <- (merge(combinedepth, metadata_tran, by = "report.ID_m", all.x = TRUE, sort = TRUE))


if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
write.csv(Abundance.temp.Master.8, file = paste(ProjectName, "_AbundanceSiteSummaries_AverageB20_full.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
#Abundance.temp.Master8e <- filter(Abundance.temp.Master.8, mpa == "Encounter Marine Park")
#Biomass.temp.Master8e <- filter(Biomass.temp.Master8, mpa == "Encounter Marine Park")

#write.csv(Abundance.temp.Master8e, file = paste(ProjectName, "_AbundanceSiteSummaries_AverageB20_encounter.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Master8e, file = paste(ProjectName, "_BiomassSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")

##Big Fish filter - No drummer, salmon, zebra

lengths20 <- filter(counts, !taxon %in% c("Kyphosus sydneyanus", "Girella zebra", "Arripis truttaceus"))


SpeciesRichness20 <- lengths20 %>%
  dplyr::select(c(survey_id, taxon)) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(SpeciesRichness20 = n_distinct(taxon))
head(SpeciesRichness20,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness20 <- lengths20 %>%
  dplyr::select(c(survey_id, family))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(FamilyRichness20 = n_distinct(family))
head(FamilyRichness20,2)


#### Calculate the total abundance at each site ####

TotalAbundance20 <- lengths20 %>%
  dplyr::select(c(survey_id, total))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalAbundance20 = sum(total))
head(TotalAbundance20,2)


#### Calculate the total biomass at each site ####

TotalBiomass20 <- lengths20 %>%
  dplyr::select(c(survey_id, biomass))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalBiomass20 = sum(biomass))
head(TotalBiomass20,2)

#### Calculate abundance of individual species for each site ####

Abundance.Species20 <- lengths20 %>%
  dplyr::select(c(survey_id,taxon,total))

Abundance.Species.Pivot20 <- cast(Abundance.Species20, survey_id ~ taxon, sum, value = "total")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
#Abundance.Species.Pivot$`Platycephalus sp2` +
#Abundance.Species.Pivot$`Platycephalus sp3` +
#Abundance.Species.Pivot$`Platycephalus sp4`


#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness20, FamilyRichness20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Abundance.Species.Pivot20, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, metadata_simple, by = "survey_id", all.x = TRUE, sort = TRUE))


# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))

write.csv(Abundance.temp.Master5, file = paste(ProjectName, "_AbundanceSiteSummariesB20_Noschool.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(lengths20, file = paste(ProjectName, "SA_MP_BRUVS_ALL_Length_Biomass_Metadata_fuctional_20.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
Abundance.temp.Master5e <- filter(Abundance.temp.Master5, mpa == "Encounter Marine Park")
#Biomass.temp.Master8e <- filter(Biomass.temp.Master8, mpa == "Encounter Marine Park")

write.csv(Abundance.temp.Master5e, file = paste(ProjectName, "_AbundanceSiteSummariesB20_encounter_Noschool.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Master8e, file = paste(ProjectName, "_BiomassSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")


#create average values per site across the four transects


Abundance.temp.Master.7 <- Abundance.temp.Master5 

combinedepth <- Abundance.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:87, mean, na.rm = TRUE))

Abundance.temp.Master.8 <- (merge(combinedepth, metadata_tran, by = "report.ID_m", all.x = TRUE, sort = TRUE))


if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
write.csv(Abundance.temp.Master.8, file = paste(ProjectName, "_AbundanceSiteSummaries_AverageB20.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
Abundance.temp.Master8e <- filter(Abundance.temp.Master8, mpa == "Encounter Marine Park")
#Biomass.temp.Master8e <- filter(Biomass.temp.Master8, mpa == "Encounter Marine Park")

write.csv(Abundance.temp.Master8e, file = paste(ProjectName, "_AbundanceSiteSummaries_AverageB20_encounter.csv", sep = ""), row.names = FALSE, na = "")
#write.csv(Biomass.temp.Mast

####### Cryptic fish  #######





# Import Data
setwd(paste(working_directory, "/data/RLS", sep = ""))
counts_origcryptic <- read.csv("SA_MP_RLS_crypticfish_metadata_filtered.csv", header = TRUE, stringsAsFactors = FALSE)
counts_origcryptic$report.ID <- paste(counts_origcryptic$year, counts_origcryptic$site_code, sep = "_")

counts_origcryptic$month <- paste(counts_origcryptic$survey_date)
counts_origcryptic$month <- format(as.Date(counts_origcryptic$month), "%m")

counts_origcryptic$report.ID_m <- paste(counts_origcryptic$year, counts_origcryptic$site_code, counts_origcryptic$month, sep = "_")


countscryptic <- counts_origcryptic
#setwd(paste(working_directory, "/analysis/biomass", sep = ""))
#biomass <- read.csv("SA_MP_BRUVS_ALL_TotalBiomass.csv", header = TRUE, stringsAsFactors = FALSE)
#biomass.w.pseudolengths <- read.csv("SA_MP_BRUVS_ALL_Length_Biomass_IncPseudoBiomass.csv", header = TRUE, stringsAsFactors = FALSE)

#setwd(paste(working_directory, "/data", sep = ""))
#metadata <- read.csv("combined.metadata.csv", header = TRUE, stringsAsFactors = FALSE)
#metadata$report.ID <- paste(metadata$Year, metadata$Sample, sep = "_")




#Set-up metadata

metadatacryptic <- counts_origcryptic %>%
  select(1:18, 20, 22:25, 28, 30, 33:42) %>%
  unique(, keep_all = FALSE)

metadata_simplecryptic <- metadatacryptic %>%
  select(!20:25) %>%
  unique(, keep_all = FALSE)

metadata_trancryptic <- counts_origcryptic %>%
  select(1, 3:10, 13:14, 20, 33:42) %>%
  unique(, keep_all = FALSE, na.rm = FALSE)

metadata_trancryptic <- metadata_trancryptic %>%
  drop_na(13)
#filter(, Data.Type != "Other")

n_distinct(metadata_trancryptic$report.ID_m)

n_occur <- data.frame(table(metadata_trancryptic$report.ID_m))

#combine block for each transect

combineblockcryptic <- countscryptic %>%
  group_by(survey_id, taxon, size_class) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    biomass = sum(biomass, na.rm = TRUE),
  )

countscryptic <- merge(combineblockcryptic, metadatacryptic, by = c('survey_id','taxon','size_class'), all.y = FALSE)


#### Calculate species richness of the entire assemblage ####

SpeciesRichness <- countscryptic %>%
  dplyr::select(c(survey_id, taxon)) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(SpeciesRichness = n_distinct(taxon))
head(SpeciesRichness,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness <- countscryptic %>%
  dplyr::select(c(survey_id, family))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(FamilyRichness = n_distinct(family))
head(FamilyRichness,2)


#### Calculate the total abundance at each site ####

TotalAbundance <- countscryptic %>%
  dplyr::select(c(survey_id, total))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalAbundance = sum(total))
head(TotalAbundance,2)


#### Calculate the total biomass at each site ####

TotalBiomass <- countscryptic %>%
  dplyr::select(c(survey_id, biomass))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalBiomass = sum(biomass))
head(TotalBiomass,2)


#### Calculate the Shannon-Wiener Index (H') of each site ####

Shannon.Wiener.Diversity.Index <- countscryptic %>%
  dplyr::select(c(survey_id,taxon,total))

Shannon.Wiener.Diversity.Index.Pivot<- cast(Shannon.Wiener.Diversity.Index, survey_id ~ taxon, sum, value = "total")

Shannon.Wiener.Diversity.Index.Pivot$Shannon.Wiener.Diversity.Index <- diversity(Shannon.Wiener.Diversity.Index.Pivot, index="shannon")

Shannon.Wiener.Diversity.Index.Output <- Shannon.Wiener.Diversity.Index.Pivot %>%
  dplyr::select(c(survey_id, Shannon.Wiener.Diversity.Index))


#### Calculate abundance of individual species for each site ####

Abundance.Species <- countscryptic %>%
  dplyr::select(c(survey_id,taxon,total))

Abundance.Species.Pivot <- cast(Abundance.Species, survey_id ~ taxon, sum, value = "total")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
#Abundance.Species.Pivot$`Platycephalus sp2` +
#Abundance.Species.Pivot$`Platycephalus sp3` +
#Abundance.Species.Pivot$`Platycephalus sp4`


#### Calculate biomass of individual species for each site ####

Biomass.Species <- countscryptic %>%
  filter(, biomass != "0") %>%
dplyr::select(c(survey_id,taxon,biomass)) 
  


Biomass.Species.Pivot <- cast(Biomass.Species, survey_id ~ taxon, sum, value = "biomass")

#Biomass.Species.Pivot$`All Platycephalus sp.` <- Biomass.Species.Pivot$`Platycephalus sp1` +
#Biomass.Species.Pivot$`Platycephalus sp2` +
#Biomass.Species.Pivot$`Platycephalus sp3` +
#Biomass.Species.Pivot$`Platycephalus sp4`

#### Make presence absence summary ####
Common.Species <- countscryptic %>%
  dplyr::select(c(survey_id,taxon,total)) %>%
  group_by(taxon) %>%
  tally()


#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, Abundance.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master6 <- (merge(Abundance.temp.Master5, metadata_simplecryptic, by = "survey_id", all.x = TRUE, sort = TRUE))


#### Merge all summaries together to make output site summaries table ####

Biomass.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master2 <- (merge(Biomass.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master3 <- (merge(Biomass.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master4 <- (merge(Biomass.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master5 <- (merge(Biomass.temp.Master4, Biomass.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master6 <- (merge(Biomass.temp.Master5, metadatacryptic, by = "survey_id", all.x = TRUE, sort = TRUE))



# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))

write.csv(Abundance.temp.Master6, file = paste(ProjectName, "cryptic", "_AbundanceSiteSummaries.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Biomass.temp.Master6, file = paste(ProjectName,"cryptic","_BiomassSiteSummaries.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Common.Species, file = paste(ProjectName,"cryptic","_MostCommonSp.csv", sep = ""), row.names = FALSE, na = "")


#create average values per site across the four transects


Abundance.temp.Master.7 <- Abundance.temp.Master6 

combinedepthcryptic <- Abundance.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:94, mean, na.rm = TRUE))

Abundance.temp.Master.8 <- (merge(combinedepthcryptic, metadata_trancryptic, by = "report.ID_m", all.x = TRUE, sort = TRUE))


if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
write.csv(Abundance.temp.Master.8, file = paste(ProjectName,"cryptic", "_AbundanceSiteSummaries_Average.csv", sep = ""), row.names = FALSE, na = "")

Biomass.temp.Master.7 <- Biomass.temp.Master6 

combinedepthBcryptic <- Biomass.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:95, mean, na.rm = TRUE))

Biomass.temp.Master.8 <- (merge(combinedepthBcryptic, metadata_trancryptic, by = "report.ID_m", all.x = TRUE, sort = TRUE))

write.csv(Biomass.temp.Master.8, file = paste(ProjectName,"cryptic", "_BiomassSiteSummaries_Average.csv", sep = ""), row.names = FALSE, na = "")


####### Inverts  #######





# Import Data
setwd(paste(working_directory, "/data/RLS", sep = ""))
counts_originverts <- read.csv("SA_MP_RLS_inverts_metadata_filtered_Oneab.csv", header = TRUE, stringsAsFactors = FALSE)
counts_originverts$report.ID <- paste(counts_originverts$year, counts_originverts$site_code, sep = "_")

counts_originverts$month <- paste(counts_originverts$survey_date)
counts_originverts$month <- format(as.Date(counts_originverts$month), "%m")

counts_originverts$report.ID_m <- paste(counts_originverts$year, counts_originverts$site_code, counts_originverts$month, sep = "_")


countsinverts <- counts_originverts
#setwd(paste(working_directory, "/analysis/biomass", sep = ""))
#biomass <- read.csv("SA_MP_BRUVS_ALL_TotalBiomass.csv", header = TRUE, stringsAsFactors = FALSE)
#biomass.w.pseudolengths <- read.csv("SA_MP_BRUVS_ALL_Length_Biomass_IncPseudoBiomass.csv", header = TRUE, stringsAsFactors = FALSE)

#setwd(paste(working_directory, "/data", sep = ""))
#metadata <- read.csv("combined.metadata.csv", header = TRUE, stringsAsFactors = FALSE)
#metadata$report.ID <- paste(metadata$Year, metadata$Sample, sep = "_")

#filter year

countsinverts <- filter(countsinverts, Finyear >= "2015-16")


#Set-up metadata

metadatainverts <- counts_originverts %>%
  select(1:18, 20, 22:25, 28, 30, 33:42) %>%
  unique(, keep_all = FALSE)

metadata_simpleinverts <- metadatainverts %>%
  select(!20:25) %>%
  unique(, keep_all = FALSE)

metadata_traninverts <- counts_originverts %>%
  select(1, 3:10, 13:14, 20, 33:42) %>%
  unique(, keep_all = FALSE, na.rm = FALSE)

metadata_traninverts <- metadata_traninverts %>%
  drop_na(13)
#filter(, Data.Type != "Other")

n_distinct(metadata_traninverts$report.ID_m)

n_occur <- data.frame(table(metadata_traninverts$report.ID_m))

#combine block for each transect

combineblockinverts <- countsinverts %>%
  group_by(survey_id, taxon, size_class) %>%
  dplyr::summarise(
    total = sum(total, na.rm = TRUE),
    biomass = sum(biomass, na.rm = TRUE),
  )

countsinverts <- merge(combineblockinverts, metadatainverts, by = c('survey_id','taxon','size_class'), all.y = FALSE)


#### Calculate species richness of the entire assemblage ####

SpeciesRichness <- countsinverts %>%
  dplyr::select(c(survey_id, taxon)) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(SpeciesRichness = n_distinct(taxon))
head(SpeciesRichness,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness <- countsinverts %>%
  dplyr::select(c(survey_id, family))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(FamilyRichness = n_distinct(family))
head(FamilyRichness,2)


#### Calculate the total abundance at each site ####

TotalAbundance <- countsinverts %>%
  dplyr::select(c(survey_id, total))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalAbundance = sum(total))
head(TotalAbundance,2)


#### Calculate the total biomass at each site ####

TotalBiomass <- countsinverts %>%
  dplyr::select(c(survey_id, biomass))%>%
  dplyr::group_by(survey_id)%>%
  dplyr::summarise(TotalBiomass = sum(biomass))
head(TotalBiomass,2)


#### Calculate the Shannon-Wiener Index (H') of each site ####

Shannon.Wiener.Diversity.Index <- countsinverts %>%
  dplyr::select(c(survey_id,taxon,total))

Shannon.Wiener.Diversity.Index.Pivot<- cast(Shannon.Wiener.Diversity.Index, survey_id ~ taxon, sum, value = "total")

Shannon.Wiener.Diversity.Index.Pivot$Shannon.Wiener.Diversity.Index <- diversity(Shannon.Wiener.Diversity.Index.Pivot, index="shannon")

Shannon.Wiener.Diversity.Index.Output <- Shannon.Wiener.Diversity.Index.Pivot %>%
  dplyr::select(c(survey_id, Shannon.Wiener.Diversity.Index))


#### Calculate abundance of individual species for each site ####

Abundance.Species <- countsinverts %>%
  dplyr::select(c(survey_id,taxon,total))

Abundance.Species.Pivot <- cast(Abundance.Species, survey_id ~ taxon, sum, value = "total")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
#Abundance.Species.Pivot$`Platycephalus sp2` +
#Abundance.Species.Pivot$`Platycephalus sp3` +
#Abundance.Species.Pivot$`Platycephalus sp4`


#### Calculate biomass of individual species for each site ####

Biomass.Species <- countsinverts %>%
  filter(, biomass != "0") %>%
  dplyr::select(c(survey_id,taxon,biomass)) 



Biomass.Species.Pivot <- cast(Biomass.Species, survey_id ~ taxon, sum, value = "biomass")

#Biomass.Species.Pivot$`All Platycephalus sp.` <- Biomass.Species.Pivot$`Platycephalus sp1` +
#Biomass.Species.Pivot$`Platycephalus sp2` +
#Biomass.Species.Pivot$`Platycephalus sp3` +
#Biomass.Species.Pivot$`Platycephalus sp4`

#### Make presence absence summary ####
Common.Species <- countsinverts %>%
  dplyr::select(c(survey_id,taxon,total)) %>%
  group_by(taxon) %>%
  tally()


#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, Abundance.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Abundance.temp.Master6 <- (merge(Abundance.temp.Master5, metadata_simpleinverts, by = "survey_id", all.x = TRUE, sort = TRUE))


#### Merge all summaries together to make output site summaries table ####

Biomass.temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master2 <- (merge(Biomass.temp.Master1, TotalAbundance, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master3 <- (merge(Biomass.temp.Master2, TotalBiomass, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master4 <- (merge(Biomass.temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master5 <- (merge(Biomass.temp.Master4, Biomass.Species.Pivot, by = "survey_id", all.x = TRUE, sort = TRUE))
Biomass.temp.Master6 <- (merge(Biomass.temp.Master5, metadatainverts, by = "survey_id", all.x = TRUE, sort = TRUE))



# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))

write.csv(Abundance.temp.Master6, file = paste(ProjectName, "inverts", "_AbundanceSiteSummaries.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Biomass.temp.Master6, file = paste(ProjectName,"inverts","_BiomassSiteSummaries.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Common.Species, file = paste(ProjectName,"inverts","_MostCommonSp.csv", sep = ""), row.names = FALSE, na = "")


#Encounter only
Abundance.temp.Master6e <- filter(Abundance.temp.Master6, mpa == "Encounter Marine Park")
Biomass.temp.Master6e <- filter(Biomass.temp.Master6, mpa == "Encounter Marine Park")

write.csv(Abundance.temp.Master6e, file = paste(ProjectName, "inverts","_AbundanceSiteSummaries_encounter.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Biomass.temp.Master6e, file = paste(ProjectName, "inverts","_BiomassSiteSummaries_encounter.csv", sep = ""), row.names = FALSE, na = "")


#create average values per site across the four transects


Abundance.temp.Master.7 <- Abundance.temp.Master6 

combinedepthinverts <- Abundance.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:108, mean, na.rm = TRUE))

Abundance.temp.Master.8 <- (merge(combinedepthinverts, metadata_traninverts, by = "report.ID_m", all.x = TRUE, sort = TRUE))


if(dir.exists(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
write.csv(Abundance.temp.Master.8, file = paste(ProjectName,"inverts", "_AbundanceSiteSummaries_Average.csv", sep = ""), row.names = FALSE, na = "")

Biomass.temp.Master.7 <- Biomass.temp.Master6 

combinedepthBinverts <- Biomass.temp.Master.7 %>%
  dplyr::group_by(report.ID_m) %>%
  dplyr::summarise(across(2:8, mean, na.rm = TRUE))

Biomass.temp.Master.8 <- (merge(combinedepthBinverts, metadata_traninverts, by = "report.ID_m", all.x = TRUE, sort = TRUE))

write.csv(Biomass.temp.Master.8, file = paste(ProjectName,"inverts", "_BiomassSiteSummaries_Average.csv", sep = ""), row.names = FALSE, na = "")

#Encounter only
Abundance.temp.Master8e <- filter(Abundance.temp.Master.8, mpa == "Encounter Marine Park")
Biomass.temp.Master8e <- filter(Biomass.temp.Master.8, mpa == "Encounter Marine Park")

write.csv(Abundance.temp.Master8e, file = paste(ProjectName, "inverts","_AbundanceSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")
write.csv(Biomass.temp.Master8e, file = paste(ProjectName, "inverts","_BiomassSiteSummaries_Average_encounter.csv", sep = ""), row.names = FALSE, na = "")

