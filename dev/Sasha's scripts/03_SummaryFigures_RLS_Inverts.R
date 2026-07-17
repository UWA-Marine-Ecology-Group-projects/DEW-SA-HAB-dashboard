rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(reshape)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(scales)

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
setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
site.summaries <- read.csv("SA_MP_RLS_ALLinverts_AbundanceSiteSummaries_Average.csv", header = TRUE, stringsAsFactors = FALSE)

setwd(paste(working_directory, "/data/RLS", sep = ""))
counts <- read.csv("SA_MP_RLS_inverts_metadata_filtered_Oneab.csv", header = TRUE, stringsAsFactors = FALSE)


setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
biomass <- read.csv("SA_MP_RLS_ALLinverts_BiomassSiteSummaries_Average.csv", header = TRUE, stringsAsFactors = FALSE)


# Set Up Data
Plot.Data.Setup <- function(input.dataframe, group, metric){
  temp <- input.dataframe
  group.dplyr <- enquo(group)                         # Create quosure
  metric.dplyr  <- enquo(metric)                      # Create quosure
  group <- deparse(substitute(group))
  metric <- deparse(substitute(metric))
  temp <- temp %>%
    dplyr::select(!!group.dplyr, !!metric.dplyr)%>%
    group_by(!!group.dplyr)%>%
    summarise_each(funs(sum(!!metric.dplyr)))
  
  TotalCount <- sum(temp[[metric]], na.rm = TRUE)
  temp$SpeciesPercentage <- (((temp[[metric]]) / TotalCount)*100)
  
  temp[[group]] <- factor(temp[[group]], levels = temp[[group]])
  
  temp <- temp[order(-temp$SpeciesPercentage),] 
  
  working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(working_directory)
  setwd('..')
  working_directory <- getwd()
  
  setwd(paste(working_directory, "/results/SummaryStatistics", sep = ""))
  write.csv(temp, file = paste("SAMP_", group, "_", metric, ".csv", sep = ""), row.names = FALSE)
  
  temp.top10 <- head(temp, 10)
  
  temp.remainder <- temp %>%
    filter(!(!!group.dplyr) %in% temp.top10[[group]]) %>%
    drop_na(SpeciesPercentage)
  
  temp.remainder <- sum(temp.remainder$SpeciesPercentage)
  
  temp.remainder.2 <- data.frame("Other", NA, temp.remainder)
  
  colnames(temp.top10) <- c(group, metric, "Percentage")
  colnames(temp.remainder.2) <- c(group, metric, "Percentage")
  
  temp <- rbind(temp.top10, temp.remainder.2)
  
  return(temp)
}

# Example usage
count.species.plotdata <- Plot.Data.Setup(counts, taxon, total)
count.family.plotdata <- Plot.Data.Setup(counts, family, total)
biomass.species.plotdata <- Plot.Data.Setup(counts, taxon, biomass)
biomass.family.plotdata <- Plot.Data.Setup(counts, family, biomass)

colnames(count.species.plotdata) <- c("Taxon", "total", "Percentage")
colnames(count.family.plotdata) <- c("Family", "total", "Percentage")
colnames(biomass.species.plotdata) <- c("Taxon", "biomass", "Percentage")
colnames(biomass.family.plotdata) <- c("Family", "biomass", "Percentage")

# PLOTS ####

# Create directory
if(dir.exists(paste(working_directory, "/results/SummaryStatistics/RLS", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/results/SummaryStatistics/RLS", sep = ""))
}
setwd(paste(working_directory, "/results/SummaryStatistics/RLS", sep = ""))

#### SPECIES COUNT PLOT ####

count.species.plotdata$Taxon <- factor(count.species.plotdata$Taxon, levels = count.species.plotdata$Taxon)
unique(count.species.plotdata$Taxon)

labels = c(expression(italic("Heliocidaris erythrogramma")),
           expression(italic("Cenolia trichoptera")),
           expression(italic("Haliotis spp.")),
           expression(italic("Lunella undulata")),
           expression(italic("Pinna bicolor")),
           expression(italic("Pleuroploca australasia")),
           expression(italic("Paguristes frontalis")),
           expression(italic("Lunella torquata")),
           expression(italic("Pentagonaster duebeni")),
           expression(italic("Petricia vernicina")),
           "Other")

species_plot <- ggplot(data = count.species.plotdata) +
  geom_bar(stat = "identity", width = 100, aes(1, Percentage, fill=Taxon), color="black") +
  ylab("Percentage of observations") +
  scale_fill_brewer("Species", palette="Paired", direction = -1, labels = labels) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.text.align = 0,
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(species_plot)

ggsave("BarPlot_TotalAbundance_Species_01_inverts.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)


horizontal_species_plot <- ggplot(data = count.species.plotdata, aes(x=Taxon, y=Percentage)) +
  geom_bar(stat = "identity", color="#0B3041", fill = "#0B3041") +
  ylab("Percentage of observations") +
  xlab("Species") +
  scale_fill_brewer("Species", palette="Set3") +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""), expand = expand_scale(mult = c(0, .05)), limits = c(0,50)) +
  theme_classic() +
  theme(#axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        #axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.text.align = 0,
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(horizontal_species_plot)

ggsave("BarPlot_Horizontal_TotalAbundance_Species_01_inverts.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 150, units = "mm" ,limitsize = TRUE)


#### FAMILIES COUNT PLOT ####

count.family.plotdata$Family <- factor(count.family.plotdata$Family, levels = count.family.plotdata$Family)

species_plot <- ggplot(data = count.family.plotdata) +
  geom_bar(stat = "identity", width = 100, aes(x = 1, y = Percentage, fill=Family), color="black") + #, position="fill") +
  ylab("Percentage of observations") +
  scale_fill_brewer("Family", palette="Paired", direction = -1) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(species_plot)

ggsave("BarPlot_TotalAbundance_Family_01_inverts.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)


horizontal_family_plot <- ggplot(data = count.family.plotdata, aes(x=Family, y=Percentage)) +
  geom_bar(stat = "identity", color="#0B3041", fill = "#0B3041") +
  ylab("Percentage of observations") +
  xlab("Family") +
  scale_fill_brewer("Species", palette="Set3")+#, labels = labels) +
  #scale_x_discrete(breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""), expand = expand_scale(mult = c(0, .05))) +
  theme_classic() +
  theme(#axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    #axis.ticks.x=element_blank(),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text.align = 0,
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    text = element_text(size = 15),
    axis.title = element_text(face = "bold")
  )
plot(horizontal_family_plot)

ggsave("BarPlot_Horizontal_TotalAbundance_Family_01_inverts.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 129, units = "mm" ,limitsize = TRUE)






#### Compile Summary Tables ####

Table.Data.Setup <- function(input.dataframe, group, metric){
  temp <- input.dataframe
  group.dplyr <- enquo(group)                         # Create quosure
  metric.dplyr  <- enquo(metric)                      # Create quosure
  group <- deparse(substitute(group))
  metric <- deparse(substitute(metric))
  
  temp <- temp %>%
    dplyr::select(!!group.dplyr, !!metric.dplyr)%>%
    group_by(!!group.dplyr)%>%
    summarise_each(funs(sum(!!metric.dplyr)))
  
  TotalCount <- sum(temp[[metric]], na.rm = TRUE)
  temp$SpeciesPercentage <- (((temp[[metric]]) / TotalCount)*100)
  
  temp[[group]] <- factor(temp[[group]], levels = temp[[group]])
  
  temp <- temp[order(-temp$SpeciesPercentage),] 
  
  return(temp)
}

count.species.tabledata <- Table.Data.Setup(counts, taxon, total)
count.family.tabledata <- Table.Data.Setup(counts, family, total)
biomass.species.tabledata <- Table.Data.Setup(counts, taxon, biomass)
biomass.family.tabledata <- Table.Data.Setup(counts, family, biomass)

write.csv(count.species.tabledata, file = paste(ProjectName, "_Count_Species_Percent_Table_inverts.csv", sep = ""), row.names = FALSE, na = "")
write.csv(count.family.tabledata, file = paste(ProjectName, "_Count_Family_Percent_Table_inverts.csv", sep = ""), row.names = FALSE, na = "")
write.csv(biomass.species.tabledata, file = paste(ProjectName, "_Biomass_Species_Percent_Table_inverts.csv", sep = ""), row.names = FALSE, na = "")
write.csv(biomass.family.tabledata, file = paste(ProjectName, "_Biomass_Family_Percent_Table_inverts.csv", sep = ""), row.names = FALSE, na = "")





