rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(grid)
library(cowplot)

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
biomass <- read.csv("SA_MP_RLS_fish_metadata_filtered.csv", header = TRUE, stringsAsFactors = FALSE)

# Import Maturity Length
setwd(paste(working_directory, "/data", sep = ""))
maturity <- read.csv("Fishbase_Maturity.csv", header = TRUE, stringsAsFactors = FALSE)

colour.schema <- c("Mixed sediments" = "#180038", 
                   "Sand" = "#532961", 
                   "Reef" = "#573c24", 
                   "Pavement with veneer" = "#7d83db", 
                   "Sand with patchy epibiota" = "#f28fc7", 
                   "Deep" = "#4e94f5", 
                   "Middle" = "#0c632d", 
                   "Shallow" = "#6e6e6e",
                   "Fished" = "#072759",
                   "No-take" = "#e88e98")


#problem.calls <- subset(calls, grepl(' sp', calls) == TRUE)
#calls <- subset(calls, grepl(' sp', calls) == FALSE)


colnames(biomass)[colnames (biomass) == "Inside.Outside.SZ"] <- "Status"
colnames(biomass)[colnames (biomass) == "taxon"]<- "Taxon"

# Prepare calls for loops
calls <- unique(biomass$Taxon)

biomass$Status[biomass$Status == "out"] <- "Fished"
biomass$Status[biomass$Status == "in"] <- "No-take"

# Plotting Function #####


#Filter by Encounter only

biomass_encounter <- dplyr::filter(biomass, location == "Encounter")



# Calculate x limits for each taxon across all financial years
calculate_limits <- function(data, groupname = "Taxon") {
  unique_taxa <- unique(data[[groupname]])
  limits <- list()
  
  for (taxon in unique_taxa) {
    # Filter data for each taxon across all years
    taxon_data <- data %>% dplyr::filter((.)[groupname] == taxon)
    
    # x-axis limits: min and max of size_class across all years
    x_limits <- range(taxon_data$size_class, na.rm = TRUE)
    
    limits[[taxon]] <- list(x_limits = x_limits)
  }
  
  return(limits)
}

# Store limits for each taxon across all years
taxon_limits <- calculate_limits(biomass_encounter)

# Function to plot density using size_class and total columns
function.plots.inout <- function(groupname = "Taxon", group, year, title) {
  # Subset by group and financial year
  temp <- biomass_encounter %>%
    dplyr::filter((.)[groupname] == group & Finyear == year)
  n.inside <- sum(temp %>% dplyr::filter(Status == "No-take") %>% dplyr::pull(total), na.rm = TRUE)
  n.outside <- sum(temp %>% dplyr::filter(Status == "Fished") %>% dplyr::pull(total), na.rm = TRUE)
  
  if(n.inside > 1 & n.outside > 1) {
    
    # Retrieve pre-calculated limits for the current taxon
    x_limits <- taxon_limits[[group]]$x_limits
    
    # Check if maturity length is available
    group_name <- maturity[[groupname]]
    if(group %in% group_name) {
      maturity_length <- maturity %>%
        dplyr::filter((.)[groupname] == group) %>%
        dplyr::pull(Maturity)
      maturity_length <- as.numeric(maturity_length[1])
    }
    
    # Start plot of size frequency
    temp$Status <- as.factor(temp$Status)
    grob <- grobTree(textGrob(paste("Inside = ", n.inside, "\nOutside = ", n.outside, sep = ""), 
                              x = 0.85, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 13)))
    
    plot1 <- ggplot(temp, aes(x = size_class, weight = 'total', fill = Status)) +
      geom_density(alpha = 0.4) +
      annotate("text", x = -Inf, y = Inf, label = paste("Year:", year), hjust = -0.1, vjust = 2.1, 
               size = 5, color = "black")
    
    if(exists("maturity_length") && !is.na(maturity_length)) {
      plot1 <- plot1 + geom_vline(xintercept = maturity_length, linetype = "dashed")
    }
    
    plot1 <- plot1 + 
      scale_fill_manual(values = colour.schema, name = "Status") +
      scale_x_continuous(name = "Size Class", limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(name = "Count", expand = c(0, 0)) +
      annotation_custom(grob) +
      theme_classic() + 
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    # Save directory organized by taxon
    save_dir <- paste0(working_directory, "/Results/SizeFrequency/RLS/", group)
    if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    # Check if save directory exists and if we can write to it
    if(!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
      print(paste("Directory created:", save_dir))
    } else {
      print(paste("Directory already exists:", save_dir))
    }
    
    # Check write permissions
    if (file.access(save_dir, 2) == 0) {
      print("Directory is writable.")
    } else {
      print("Directory is not writable. Check permissions.")
    }
    setwd(save_dir)
    
    ggsave(paste0("SizeDistribution_RLS_", group, "_", year, ".png"), plot = plot1, device = "png",
           scale = 1, width = 250, height = 100, units = "mm", limitsize = TRUE)
    
    return(plot1)
  }
}

# Loop through each taxon and financial year
for (taxon in unique(biomass_encounter$Taxon)) {
  for (year in unique(biomass_encounter$Finyear)) {
    function.plots.inout("Taxon", taxon, year, bquote(paste(italic(.(taxon)), " ", year, sep = "")))
  }
}
# Deal with problem calls

#problem.calls
#problem.calls.solutions <- data.frame(OriginalName = as.character(c("Aracana spp.", "Platycephalus sp1", "Caesioperca spp.", "Pseudophycis spp.", "Platycephalus sp4", "Platycephalus sp3", "Urolophidae spp.", "Platycephalus sp2", "Pseudocaranx spp.", "Platycephalus spp.", "Sillago spp.")),
#                                      Genus = as.character(c("Aracana", "Platycephalus", "Caesioperca", "Pseudophycis", "Platycephalus", "Platycephalus", "Urolophidae", "Platycephalus", "Pseudocaranx", "Platycephalus", "Sillago")),
#                                      Sp = as.character(c(" spp.", " sp1", " spp.", " spp.", " sp4", " sp3", " spp.", " sp2", " spp.", " spp.", " spp."))
#)

#for (i in 1:length(problem.calls.solutions)) {
#  function.plots.overall("Taxon", problem.calls.solutions$OriginalName[i], bquote(paste(problem.calls.solutions$Genus[i], problem.calls.solutions$Sp, sep = "")))
  #function.plots.inout("Taxon", problem.calls.solutions$OriginalName[i], problem.calls.solutions[i])
  #function.plots.depth("Taxon", problem.calls.solutions$OriginalName[i], problem.calls.solutions[i])
#}


# Calculate x limits for each taxon across all financial years
calculate_limits <- function(data, groupname = "Taxon") {
  unique_taxa <- unique(data[[groupname]])
  limits <- list()
  
  for (taxon in unique_taxa) {
    # Filter data for each taxon across all years
    taxon_data <- data %>% dplyr::filter((.)[groupname] == taxon)
    
    # x-axis limits: min and max of size_class across all years
    x_limits <- range(taxon_data$size_class, na.rm = TRUE)
    
    limits[[taxon]] <- list(x_limits = x_limits)
  }
  
  return(limits)
}

# Store limits for each taxon across all years
taxon_limits <- calculate_limits(biomass_encounter)

# Function to plot density using size_class and total columns
function.plots.inout <- function(groupname = "Taxon", group, year, title) {
  # Subset by group and financial year
  temp <- biomass_encounter %>%
    dplyr::filter((.)[groupname] == group & Finyear == year)
  n.inside <- sum(temp %>% dplyr::filter(Status == "No-take") %>% dplyr::pull(total), na.rm = TRUE)
  n.outside <- sum(temp %>% dplyr::filter(Status == "Fished") %>% dplyr::pull(total), na.rm = TRUE)
  
  if(n.inside > 1 & n.outside > 1) {
    
    # Retrieve pre-calculated limits for the current taxon
    x_limits <- taxon_limits[[group]]$x_limits
    
    # Check if maturity length is available
    group_name <- maturity[[groupname]]
    if(group %in% group_name) {
      maturity_length <- maturity %>%
        dplyr::filter((.)[groupname] == group) %>%
        dplyr::pull(Maturity)
      maturity_length <- as.numeric(maturity_length[1])
    }
    
    # Start plot of size frequency
    temp$Status <- as.factor(temp$Status)
    grob <- grobTree(textGrob(paste("Inside = ", n.inside, "\nOutside = ", n.outside, sep = ""), 
                              x = 0.85, y = 0.9, hjust = 0, gp = gpar(col = "black", fontsize = 13)))
    
    plot1 <- ggplot(temp, aes(x = size_class, weight = total, fill = Status)) +
      geom_density(alpha = 0.4) +
      annotate("text", x = -Inf, y = Inf, label = paste("Year:", year), hjust = -0.1, vjust = 2.1, 
               size = 5, color = "black")
    
    if(exists("maturity_length") && !is.na(maturity_length)) {
      plot1 <- plot1 + geom_vline(xintercept = maturity_length, linetype = "dashed")
    }
    
    plot1 <- plot1 + 
      scale_fill_manual(values = colour.schema, name = "Status") +
      scale_x_continuous(name = "Size Class", limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(name = "Count", expand = c(0, 0)) +
      annotation_custom(grob) +
      theme_classic() + 
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
    # Save directory organized by taxon
    save_dir <- paste0(working_directory, "/results/SizeFrequency/Status/", group)
    if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    setwd(save_dir)
    
    ggsave(paste0("SizeDistribution_Status_", group, "_", year, ".png"), plot = plot1, device = "png",
           scale = 1, width = 250, height = 100, units = "mm", limitsize = TRUE)
    
    return(plot1)
  }
}

# Loop through each taxon and financial year
for (taxon in unique(biomass_encounter$Taxon)) {
  for (year in unique(biomass_encounter$Finyear)) {
    function.plots.inout("Taxon", taxon, year, bquote(paste(italic(.(taxon)), " ", year, sep = "")))
  }
}