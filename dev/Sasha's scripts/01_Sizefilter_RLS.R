rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(dplyr)
library(fy)
library(stringr)
library(vegan)
library(reshape)
library(ggplot2)

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

lengths <- read.csv("SA_MP_RLS_fish_metadata_filtered.csv", header = TRUE, stringsAsFactors = FALSE)

#setwd(paste(working_directory, "/data", sep = ""))
#metadata <- read.csv("combined.metadata.csv", header = TRUE, stringsAsFactors = FALSE)
#metadata$report.ID <- paste(metadata$Year, metadata$Sample, sep = "_")

#filter lengths to only over 200 mm

lengths20 <- filter(lengths, size_class >= 20)
lengths20 <- filter(lengths20, MER.SZ != "")
lengths20 <- filter(lengths20, total != "0")
lengths20 <- filter(lengths20, year >= 2016)

#average per block
lengths_grouped <- lengths20 %>%
  group_by(year, site_name.x, MER.SZ, Inside.Outside.SZ) %>%
  summarize(mean(total))
  
#lengths_grouped  <- lengths_grouped%>%
 # group_by(year, site_name.x, MER.SZ, Inside.Outside.SZ) %>%
  #summarize(mean(total))


names(lengths_grouped)[names(lengths_grouped) == 'mean(total)'] <- 'total_m'
#lengths_grouped$survey_date <- format(as.Date(lengths_grouped$survey_date), "%Y")
#lengths_grouped$survey_date <- as.numeric(lengths_grouped$survey_date) 

#Carri_lengths20 <- filter(lengths20, MER.SZ == "Carrickalinga Cliffs SZ")
#RH_lengths20 <- filter(lengths20, MER.SZ == "Rapid Head SZ")
#SG_lengths20 <- filter(lengths20, MER.SZ == "Sponge Gardens SZ")


# Get unique sanctuaries
sanctuaries <- unique(lengths_grouped$MER.SZ)

# Loop over each sanctuary and create a plot
for (sanctuary in sanctuaries) {
  
  # Filter data for the current sanctuary
  sanctuary_data <- lengths_grouped %>% filter(MER.SZ == sanctuary)
  
  # Create the plot
  p <- ggplot(sanctuary_data, aes(x = year, y = total_m, color = Inside.Outside.SZ)) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +  # Scatter plot with jitter
    geom_smooth(method = "lm", se = TRUE, aes(fill = Inside.Outside.SZ), alpha = 0.1) +  # Trend line with confidence interval
    #facet_wrap(~site_name.x, scales = "free_y") +  # Create separate plots for each Location
    scale_y_continuous(limits = c(0, 5)) +
    scale_x_continuous(limits = c(2016, 2023)) +
    scale_color_manual(values = c("green", "blue", "purple")) +  # Custom colors
    scale_fill_manual(values = c("green", "blue", "purple")) +  # Custom fill for confidence interval
    labs(title = paste("Sanctuary:", sanctuary), x = "Year", y = "Mean abundance") +  # Axis labels with title
    theme_classic() +  # Clean theme
    theme(
      #strip.text = element_text(size = 15, face = "bold"),  # Customize facet labels
      legend.title = element_blank(),  # Remove legend title
      legend.position = "right"  # Position legend on the right
    )
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("Sanctuary_", sanctuary, ".png"), plot = p, width = 10, height = 8)
  
  # Print the plot (optional)
  print(p)
}

#reduce species list for Danny
Removespecies <- c('Arripis truttaceus', 'Kyphosus sydneyanus', 'Girella zebra')

lengths_red <- lengths20 %>%
  filter(, !lengths20$taxon %in% Removespecies)

lengths_grouped_red <- lengths_red %>%
  group_by(year, site_name.x, MER.SZ, Inside.Outside.SZ) %>%
  summarize(mean(total))

names(lengths_grouped_red)[names(lengths_grouped_red) == 'mean(total)'] <- 'total_m'


# Loop over each sanctuary and create a plot reduced species
for (sanctuary in sanctuaries) {
  
  # Filter data for the current sanctuary
  sanctuary_data <- lengths_grouped_red %>% filter(MER.SZ == sanctuary)
  
  # Create the plot
  p <- ggplot(sanctuary_data, aes(x = year, y = total_m, color = Inside.Outside.SZ)) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +  # Scatter plot with jitter
    geom_smooth(method = "lm", se = TRUE, aes(fill = Inside.Outside.SZ), alpha = 0.1) +  # Trend line with confidence interval
    #facet_wrap(~site_name.x, scales = "free_y") +  # Create separate plots for each Location
    scale_y_continuous(limits = c(0, 5)) +
    scale_x_continuous(limits = c(2016, 2023)) +
    scale_color_manual(values = c("green", "blue", "purple")) +  # Custom colors
    scale_fill_manual(values = c("green", "blue", "purple")) +  # Custom fill for confidence interval
    labs(title = paste("Sanctuary:", sanctuary), x = "Year", y = "Mean abundance") +  # Axis labels with title
    theme_classic() +  # Clean theme
    theme(
      #strip.text = element_text(size = 15, face = "bold"),  # Customize facet labels
      legend.title = element_blank(),  # Remove legend title
      legend.position = "right"  # Position legend on the right
    )
  
  # Save the plot as a PNG file
  ggsave(filename = paste0("Sanctuary_RED_", sanctuary, ".png"), plot = p, width = 10, height = 8)
  
  # Print the plot (optional)
  print(p)
}



SpeciesRichness20 <- lengths20 %>%
  dplyr::select(c(Sample, Taxon)) %>%
  dplyr::group_by(Sample) %>%
  dplyr::summarise(SpeciesRichness20 = n_distinct(Taxon))
head(SpeciesRichness20,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness20 <- lengths20 %>%
  dplyr::select(c(Sample, Family))%>%
  dplyr::group_by(Sample)%>%
  dplyr::summarise(FamilyRichness20 = n_distinct(Family))
head(FamilyRichness20,2)


#### Calculate the total abundance at each site ####

TotalAbundance20 <- lengths20 %>%
  dplyr::select(c(Sample, Count))%>%
  dplyr::group_by(Sample)%>%
  dplyr::summarise(TotalAbundance20 = sum(Count))
head(TotalAbundance20,2)


#### Calculate the total biomass at each site ####

TotalBiomass20 <- lengths20 %>%
  dplyr::select(c(Sample, Biomass..g.))%>%
  dplyr::group_by(Sample)%>%
  dplyr::summarise(TotalBiomass20 = sum(Biomass..g.))
head(TotalBiomass20,2)

#### Calculate abundance of individual species for each site ####

Abundance.Species20 <- lengths20 %>%
  dplyr::select(c(Sample,Taxon,Count))

Abundance.Species.Pivot20 <- cast(Abundance.Species20, Sample ~ Taxon, sum, value = "Count")

#Abundance.Species.Pivot$`All Platycephalus sp.` <- Abundance.Species.Pivot$`Platycephalus sp1` +
#Abundance.Species.Pivot$`Platycephalus sp2` +
#Abundance.Species.Pivot$`Platycephalus sp3` +
#Abundance.Species.Pivot$`Platycephalus sp4`

ratios_Spp <- lengths %>%
  group_by(Sample, Taxon) %>%
  summarize(
    Total_Fish = n(),
    Fish_Over_20cm = sum(Length > 200),
    Ratio_Over_20cm = Fish_Over_20cm / Total_Fish
  )

ratios_bruv <- lengths %>%
  group_by(Sample) %>%
  summarize(
    Total_Fish = n(),
    Fish_Over_20cm = sum(Length > 200),
    Ratio_Over_20cm = Fish_Over_20cm / Total_Fish
  )

#### Merge all summaries together to make output site summaries table ####

Abundance.temp.Master1 <- (merge(SpeciesRichness20, FamilyRichness20, by = "Sample", all.x = TRUE, sort = TRUE))
Abundance.temp.Master2 <- (merge(Abundance.temp.Master1, TotalAbundance20, by = "Sample", all.x = TRUE, sort = TRUE))
Abundance.temp.Master3 <- (merge(Abundance.temp.Master2, TotalBiomass20, by = "Sample", all.x = TRUE, sort = TRUE))
Abundance.temp.Master4 <- (merge(Abundance.temp.Master3, Abundance.Species.Pivot20, by = "Sample", all.x = TRUE, sort = TRUE))
Abundance.temp.Master5 <- (merge(Abundance.temp.Master4, metadata, by = "Sample", all.x = TRUE, sort = TRUE))


# Export products
if(dir.exists(paste(working_directory, "/analysis/SiteSummaries", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/analysis/SiteSummaries", sep = ""))
}
setwd(paste(working_directory, "/analysis/SiteSummaries", sep = ""))

write.csv(Abundance.temp.Master5, file = paste(ProjectName, "_AbundanceSiteSummaries20.csv", sep = ""), row.names = FALSE, na = "")
write.csv(lengths20, file = paste(ProjectName, "SA_MP_BRUVS_ALL_Length_Biomass_Metadata_fuctional.csv", sep = ""), row.names = FALSE, na = "")







