rm(list=ls())

#.libPaths("D:/R/Libraries")

# Load libraries
library(tidyverse)
library(reshape)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(scales)
library(dplyr)

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

setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
site.summaries <- read.csv("SA_MP_RLS_ALL_AbundanceSiteSummaries_Average.csv", header = TRUE, stringsAsFactors = FALSE)

setwd(paste(working_directory, "/data/RLS", sep = ""))
counts <- read.csv("SA_MP_RLS_fish_metadata_filtered.csv", header = TRUE, stringsAsFactors = FALSE)

setwd(paste(working_directory, "/analysis/SiteSummaries/RLS", sep = ""))
biomass <- read.csv("SA_MP_RLS_ALL_BiomassSiteSummaries_Average.csv", header = TRUE, stringsAsFactors = FALSE)

# Make mean se tables
se <- function(x) sqrt(var(x)/length(x))

# filter out Encounter

EncounterMP <- site.summaries %>%
  filter(mpa == "Encounter Marine Park")

Biotope.MeanSE <- EncounterMP %>%
  dplyr::group_by(MER.SZ, Inside.Outside.SZ, Finyear) %>%
  dplyr::summarise(n = n(), 
                   Mean.SpeciesRichness = mean(SpeciesRichness), 
                   SE.SpeciesRichness = se(SpeciesRichness),
                   Mean.FamilyRichness = mean(FamilyRichness), 
                   SE.FamilyRichness = se(FamilyRichness),
                   Mean.TotalAbundance = mean(TotalAbundance), 
                   SE.TotalAbundance = se(TotalAbundance),
                   Mean.TotalBiomass = mean(TotalBiomass)/1000, 
                   SE.TotalBiomass = se(TotalBiomass)/1000)

Biotope.MeanSE <- rename(Biotope.MeanSE, c("MER.SZ" = "Location", "Inside.Outside.SZ" = "Status", "Finyear" = "Fin.Year"))

Biotope.MeanSE$Mean.SpeciesRichness <- round(Biotope.MeanSE$Mean.SpeciesRichness, digits = 2)
Biotope.MeanSE$SE.SpeciesRichness <- round(Biotope.MeanSE$SE.SpeciesRichness, digits = 2)
Biotope.MeanSE$SpeciesRichness.Mean.SE <- paste(as.character(Biotope.MeanSE$Mean.SpeciesRichness), "±", as.character(Biotope.MeanSE$SE.SpeciesRichness)) #"±"

Biotope.MeanSE$Mean.FamilyRichness <- round(Biotope.MeanSE$Mean.FamilyRichness, digits = 2)
Biotope.MeanSE$SE.FamilyRichness <- round(Biotope.MeanSE$SE.FamilyRichness, digits = 2)
Biotope.MeanSE$FamilyRichness.Mean.SE <- paste(as.character(Biotope.MeanSE$Mean.FamilyRichness), "±", as.character(Biotope.MeanSE$SE.FamilyRichness)) #"±"

Biotope.MeanSE$Mean.TotalAbundance <- round(Biotope.MeanSE$Mean.TotalAbundance, digits = 2)
Biotope.MeanSE$SE.TotalAbundance <- round(Biotope.MeanSE$SE.TotalAbundance, digits = 2)
Biotope.MeanSE$TotalAbundance.Mean.SE <- paste(as.character(Biotope.MeanSE$Mean.TotalAbundance), "±", as.character(Biotope.MeanSE$SE.TotalAbundance)) #"±"

Biotope.MeanSE$Mean.TotalBiomass <- round(Biotope.MeanSE$Mean.TotalBiomass, digits = 2)
Biotope.MeanSE$SE.TotalBiomass <- round(Biotope.MeanSE$SE.TotalBiomass, digits = 2)
Biotope.MeanSE$TotalBiomass.Mean.SE <- paste(as.character(Biotope.MeanSE$Mean.TotalBiomass), "±", as.character(Biotope.MeanSE$SE.TotalBiomass)) #"±"

Biotope.MeanSE.export <- Biotope.MeanSE %>% 
  dplyr::select(-c(Mean.SpeciesRichness,SE.SpeciesRichness, Mean.FamilyRichness, SE.FamilyRichness, Mean.TotalAbundance, SE.TotalAbundance, Mean.TotalBiomass, SE.TotalBiomass)) 

write.csv(Biotope.MeanSE.export, file = paste(ProjectName, "_Biotope_SpeciesRichness_MeanSE.csv", sep = ""), row.names = FALSE, na = "")

colour.schema <- c(#"Mixed sediments" = "#180038", 
                   #"Sand" = "#532961", 
                   #"Reef" = "#573c24", 
                   #"Pavement with veneer" = "#7d83db", 
                   #"Sand with patchy epibiota" = "#f28fc7", 
                   #"Deep" = "#4e94f5", 
                   #"Middle" = "#0c632d", 
                   ##"Shallow" = "#6e6e6e",
                   "Fished" = "#072759",
                   "No-take" = "#e88e98")

Location.unique <- unique(Biotope.MeanSE$Location)
groups <- data.frame(Name = c("SpeciesRichness", "FamilyRichness", "TotalAbundance", "TotalBiomass"),
                     Title = c("Species Richness", "Family Richness", "Total Abundance", "Total Biomass (kg)"),
                     X.Label = "Fin.Year")



#BayofShoals

Bay.of.Shoals <- Biotope.MeanSE %>%
  filter(Location == "Bay of Shoals")

for (i in 1:nrow(groups)) {
  
  temp.group <- groups$Name[[i]]
  temp.mean.column <- as.name(paste("Mean.", temp.group, sep = ""))
  temp.SE.column <- as.name(paste("SE.", temp.group, sep = ""))
  temp.signif.column <- as.name(paste("significant.", temp.group, sep = ""))
  temp.title <- groups$Title[[i]]
  temp.xlabel <- groups$X.Label[[i]]
  
  Plot.Mean.SE <- ggplot(data = Bay.of.Shoals, aes(x=Fin.Year, y=!!temp.mean.column, group = Status, shape= Status, colour= Status)) +
    geom_line(stat = "identity") +
    geom_point()+
    geom_errorbar(aes(ymin=!!temp.mean.column-!!temp.SE.column, ymax=!!temp.mean.column+!!temp.SE.column), width=.1,
                  position=position_dodge(.05)) + 
    #geom_text(aes(label = !!temp.signif.column, x = Fin.Year, y = (!!temp.mean.column+!!temp.SE.column)), vjust = -1, size = 4, fontface = "bold") +
    scale_fill_manual(values = colour.schema) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .15)), limits = c(0, NA)) +
    xlab("Year") +
    ylab(temp.title) +
    theme_classic() +
    theme(#axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      #axis.ticks.x=element_blank(),
      legend.title = element_text("Status"),
      legend.text.align = 0,
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      plot.margin=unit(c(5,5,5,5),"mm")
    )
  plot(Plot.Mean.SE)
  
  ggsave(paste("LinePlot_BayofShoals_", temp.group, "_01.png", sep = ""), plot = last_plot(), device = "png",
         scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)
  
}

#CarrickalingaCliffs

Carrickalinga.Cliffs <- Biotope.MeanSE %>%
  filter(Location == "Carrickalinga Cliffs SZ")

for (i in 1:nrow(groups)) {
  
  temp.group <- groups$Name[[i]]
  temp.mean.column <- as.name(paste("Mean.", temp.group, sep = ""))
  temp.SE.column <- as.name(paste("SE.", temp.group, sep = ""))
  temp.signif.column <- as.name(paste("significant.", temp.group, sep = ""))
  temp.title <- groups$Title[[i]]
  temp.xlabel <- groups$X.Label[[i]]
  
  Plot.Mean.SE <- ggplot(data = Carrickalinga.Cliffs, aes(x=Fin.Year, y=!!temp.mean.column, group = Status, shape= Status, colour= Status)) +
    geom_line(stat = "identity") +
    geom_point()+
    geom_errorbar(aes(ymin=!!temp.mean.column-!!temp.SE.column, ymax=!!temp.mean.column+!!temp.SE.column), width=.1,
                  position=position_dodge(.05)) + 
    #geom_text(aes(label = !!temp.signif.column, x = Fin.Year, y = (!!temp.mean.column+!!temp.SE.column)), vjust = -1, size = 4, fontface = "bold") +
    scale_fill_manual(values = colour.schema) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .15)), limits = c(0, NA)) +
    xlab("Year") +
    ylab(temp.title) +
    theme_classic() +
    theme(#axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      #axis.ticks.x=element_blank(),
      legend.title = element_text("Status"),
      legend.text.align = 0,
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      plot.margin=unit(c(5,5,5,5),"mm")
    )
  plot(Plot.Mean.SE)
  
  ggsave(paste("LinePlot_CarrickalingaCliffs_", temp.group, "_01.png", sep = ""), plot = last_plot(), device = "png",
         scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)
  
}

#RapidHead

Rapid.Head <- Biotope.MeanSE %>%
  filter(Location == "Rapid Head SZ")

for (i in 1:nrow(groups)) {
  
  temp.group <- groups$Name[[i]]
  temp.mean.column <- as.name(paste("Mean.", temp.group, sep = ""))
  temp.SE.column <- as.name(paste("SE.", temp.group, sep = ""))
  temp.signif.column <- as.name(paste("significant.", temp.group, sep = ""))
  temp.title <- groups$Title[[i]]
  temp.xlabel <- groups$X.Label[[i]]
  
  Plot.Mean.SE <- ggplot(data = Rapid.Head, aes(x=Fin.Year, y=!!temp.mean.column, group = Status, shape= Status, colour= Status)) +
    geom_line(stat = "identity") +
    geom_point()+
    geom_errorbar(aes(ymin=!!temp.mean.column-!!temp.SE.column, ymax=!!temp.mean.column+!!temp.SE.column), width=.1,
                  position=position_dodge(.05)) + 
    #geom_text(aes(label = !!temp.signif.column, x = Fin.Year, y = (!!temp.mean.column+!!temp.SE.column)), vjust = -1, size = 4, fontface = "bold") +
    scale_fill_manual(values = colour.schema) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .15)), limits = c(0, NA)) +
    xlab("Year") +
    ylab(temp.title) +
    theme_classic() +
    theme(#axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      #axis.ticks.x=element_blank(),
      legend.title = element_text("Status"),
      legend.text.align = 0,
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      plot.margin=unit(c(5,5,5,5),"mm")
    )
  plot(Plot.Mean.SE)
  
  ggsave(paste("LinePlot_RapidHead_", temp.group, "_01.png", sep = ""), plot = last_plot(), device = "png",
         scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)
  
}

#SpongeGardens
Sponge.Gardens <- Biotope.MeanSE %>%
  filter(Location == "Sponge Gardens SZ")

for (i in 1:nrow(groups)) {
  
  temp.group <- groups$Name[[i]]
  temp.mean.column <- as.name(paste("Mean.", temp.group, sep = ""))
  temp.SE.column <- as.name(paste("SE.", temp.group, sep = ""))
  temp.signif.column <- as.name(paste("significant.", temp.group, sep = ""))
  temp.title <- groups$Title[[i]]
  temp.xlabel <- groups$X.Label[[i]]
  
  Plot.Mean.SE <- ggplot(data = Sponge.Gardens, aes(x=Fin.Year, y=!!temp.mean.column, group = Status, shape= Status, colour= Status)) +
    geom_line(stat = "identity") +
    geom_point()+
    geom_errorbar(aes(ymin=!!temp.mean.column-!!temp.SE.column, ymax=!!temp.mean.column+!!temp.SE.column), width=.1,
                  position=position_dodge(.05)) + 
    #geom_text(aes(label = !!temp.signif.column, x = Fin.Year, y = (!!temp.mean.column+!!temp.SE.column)), vjust = -1, size = 4, fontface = "bold") +
    scale_fill_manual(values = colour.schema) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .15)), limits = c(0, NA)) +
    xlab("Year") +
    ylab(temp.title) +
    theme_classic() +
    theme(#axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      #axis.ticks.x=element_blank(),
      legend.title = element_text("Status"),
      legend.text.align = 0,
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      plot.margin=unit(c(5,5,5,5),"mm")
    )
  plot(Plot.Mean.SE)
  
  ggsave(paste("LinePlot_SpongeGardens_", temp.group, "_01.png", sep = ""), plot = last_plot(), device = "png",
         scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)
  
}

#ThePages

The.Pages <- Biotope.MeanSE %>%
  filter(Location == "The Pages")

for (i in 1:nrow(groups)) {
  
  temp.group <- groups$Name[[i]]
  temp.mean.column <- as.name(paste("Mean.", temp.group, sep = ""))
  temp.SE.column <- as.name(paste("SE.", temp.group, sep = ""))
  temp.signif.column <- as.name(paste("significant.", temp.group, sep = ""))
  temp.title <- groups$Title[[i]]
  temp.xlabel <- groups$X.Label[[i]]
  
  Plot.Mean.SE <- ggplot(data = The.Pages, aes(x=Fin.Year, y=!!temp.mean.column, group = Status, shape= Status, colour= Status)) +
    geom_line(stat = "identity") +
    geom_point()+
    geom_errorbar(aes(ymin=!!temp.mean.column-!!temp.SE.column, ymax=!!temp.mean.column+!!temp.SE.column), width=.1,
                  position=position_dodge(.05)) + 
    #geom_text(aes(label = !!temp.signif.column, x = Fin.Year, y = (!!temp.mean.column+!!temp.SE.column)), vjust = -1, size = 4, fontface = "bold") +
    scale_fill_manual(values = colour.schema) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .15)), limits = c(0, NA)) +
    xlab("Year") +
    ylab(temp.title) +
    theme_classic() +
    theme(#axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "right",
      #axis.ticks.x=element_blank(),
      legend.title = element_text("Status"),
      legend.text.align = 0,
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      text = element_text(size = 15),
      axis.title = element_text(face = "bold"),
      plot.margin=unit(c(5,5,5,5),"mm")
    )
  plot(Plot.Mean.SE)
  
  ggsave(paste("LinePlot_ThePages_", temp.group, "_01.png", sep = ""), plot = last_plot(), device = "png",
         scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)
  
}

# Simple line plot
# Change point shapes and line types by groups
ggplot(df3, aes(x=dose, y=len, group = supp, shape=supp, linetype=supp))+ 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() +
  geom_point()+
  labs(title="Plot of lengthby dose",x="Dose (mg)", y = "Length")+
  theme_classic()
# Change color by groups
# Add error bars
p <- ggplot(df3, aes(x=dose, y=len, group = supp, color=supp))+ 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line(aes(linetype=supp)) + 
  geom_point(aes(shape=supp))+
  labs(title="Plot of lengthby dose",x="Dose (mg)", y = "Length")+
  theme_classic()
p + theme_classic() + scale_color_manual(values=c('#999999','#E69F00'))