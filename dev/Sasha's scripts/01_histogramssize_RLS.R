rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(grid)
library(cowplot)
library(ggplot2)
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
                   "Outside SZ" = "#0ec930",
                   "Inside SZ" = "#02ccf5")

#problem.calls <- subset(calls, grepl(' sp', calls) == TRUE)
#calls <- subset(calls, grepl(' sp', calls) == FALSE)


colnames(biomass)[colnames (biomass) == "Inside.Outside.SZ"] <- "Status"
colnames(biomass)[colnames (biomass) == "Taxon"]<- "taxon"

# Prepare calls for loops
calls <- unique(biomass$taxon)

biomass$Status[biomass$Status == "out"] <- "Outside SZ"
biomass$Status[biomass$Status == "in"] <- "Inside SZ"

# Plotting Function #####


#Filter by Encounter only

dataB <- dplyr::filter(biomass, location == "Encounter")
dataB <- dplyr::filter(dataB, Finyear >= "2015-16")
# Convert necessary columns to factors
dataB$Finyear <- as.factor(dataB$Finyear)
dataB$taxon <- as.factor(dataB$taxon)
dataB$Status <- as.factor(dataB$Status)

calculate_limits <- function(data, groupname = "taxon") {
  unique_taxa <- unique(data[[groupname]])  # Get unique taxa names
  limits <- list()  # Initialize a list to store the limits
  
  for (taxon in unique_taxa) {
    # Filter data for each taxon across all years using the dynamic groupname column
    taxon_data <- data %>% dplyr::filter(!!rlang::sym(groupname) == taxon)
    
    # Calculate the x-axis limits (min and max of size_class)
    x_limits <- range(taxon_data$size_class, na.rm = TRUE)
    
    # Store the limits for the current taxon
    limits[[taxon]] <- list(x_limits = x_limits)
  }
  
  return(limits)
}

# Assuming dataB is your data frame
taxon_limits <- calculate_limits(dataB)

summary_df <- dataB %>%
  group_by(taxon, Finyear, Status) %>%
  summarise(total_individuals = sum(total, na.rm = TRUE)) %>%
  pivot_wider(names_from = Status, values_from = total_individuals, values_fill = 0) %>%
  rename(n.inside = 'Inside SZ', n.outside = `Outside SZ`)

# Subset summary_df to exclude rows where both Inside and Outside are 0,
# or where one is 0 and the other is 1
filtered_summary_df <- summary_df %>%
  filter(!(n.inside == 0 & n.outside == 0) & # Exclude rows where both are 0
           !(n.inside == 0 & n.outside == 1) & # Exclude rows where Inside is 0 and Outside is 1
           !(n.inside == 1 & n.outside == 0))  # Exclude rows where Inside is 1 and Outside is 0

# Filter the original data based on the filtered summary data frame
filtered_data <- dataB %>%
  semi_join(filtered_summary_df, by = c("taxon", "Finyear"))

# Loop through each unique taxon to create a histogram for each
for (taxon_name in unique(filtered_data$taxon)) {
  
  # Filter the data for the current taxon
  taxon_data <- filtered_data %>% filter(taxon == taxon_name)
  x_limits <- taxon_limits[taxon_name]$x_limits
  
  # Calculate the count of individuals for each Status and Finyear
  count_data <- taxon_data %>%
    group_by(Finyear, Status) %>%
    summarize(count = sum(total), .groups = "drop") %>%
    mutate(label = paste0("n.", Status, " = ", count),
           y_position = ifelse(Status == "Inside SZ", 0.1, 0.08)) # Offset position for each status)
  
  # Create the histogram
  p <- ggplot(taxon_data, aes(x = size_class, weight = total, fill = Status)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Finyear, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = colour.schema, name = "Status") +
    scale_x_continuous(name = "Size Class", limits =x_limits, expand = c(0, 0)) +
    labs(y = "Count") +
    #annotation_custom(grob) +
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), 
          axis.title = element_text(size = 15),
          strip.text = element_text(size=15)) + 
    geom_text(data = count_data, aes(x = Inf, y = y_position, label = label, hjust = 1.1, vjust = 1.1), 
              color = "black", size = 3, inherit.aes = FALSE)
  
  # Print or save the plot (you can uncomment the ggsave line to save each plot as a file)
  print(p) 
  save_dir <- paste0(working_directory, "/Results/SizeFrequency/RLS/", taxon_name)
  if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
   setwd(save_dir)
  
  ggsave(paste0("density_RLS_", taxon_name,".png"), plot = p, device = "png",
  scale = 1, width = 250, height = 200, units = "mm", limitsize = TRUE)
  # ggsave(paste("histogram_", taxon_name, ".png", sep=""), plot = p, width = 10, height = 6)
}


# Loop through each unique taxon to create a histogram for each
for (taxon_name in unique(data$taxon)) {
  
  # Filter the data for the current taxon
  taxon_data <- data %>% filter(taxon == taxon_name)
  x_limits <- taxon_limits[taxon_name]$x_limits
  
  # Create the histogram
  p <- ggplot(taxon_data, aes(x = size_class, weight = total, fill = Status)) +
    geom_histogram(binwidth = 1, position = "dodge") +
    facet_wrap(~ Finyear) +
    scale_fill_manual(values = colour.schema, name = "Status") +
    scale_x_continuous(name = "Size Class", limits =x_limits, expand = c(0, 0)) +
    scale_y_continuous(name = "Count", expand = c(0, 0)) +
    #annotation_custom(grob) +
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  # Print or save the plot (you can uncomment the ggsave line to save each plot as a file)
  print(p) 
  save_dir <- paste0(working_directory, "/Results/SizeFrequency/RLS/", taxon_name)
  if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  setwd(save_dir)
  
  ggsave(paste0("histogram_RLS_", taxon_name,".png"), plot = p, device = "png",
         scale = 1, width = 300, height = 100, units = "mm", limitsize = TRUE)
  # ggsave(paste("histogram_", taxon_name, ".png", sep=""), plot = p, width = 10, height = 6)
}




# Filter the original data based on the filtered summary data frame
filtered_data <- dataB %>%
  semi_join(filtered_summary_df, by = c("taxon", "Finyear"))

# Loop through each row in the filtered summary data frame
for (i in 1:nrow(filtered_summary_df)) {
  
  # Extract the current row values
  taxon_name <- filtered_summary_df$taxon[i]
  finyear <- filtered_summary_df$Finyear[i]
  
  # Filter the data for the current taxon and financial year
  taxon_data <- filtered_data %>%
    filter(taxon == taxon_name, Finyear == finyear)
  
  # Set x-axis limits if you have predefined limits (else set to NULL or define taxon_limits)
  x_limits <- taxon_limits[taxon_name]$x_limits # Update or define taxon_limits as needed
  
  # Create the density plot
  p <- ggplot(taxon_data, aes(x = size_class, weight = total, fill = Status)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = colour.schema, name = "Status") +
    scale_x_continuous(name = "Size Class", limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(name = "Count", expand = c(0, 0)) +
    facet_wrap(~ Finyear) +
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  # Print or save the plot (uncomment the ggsave line to save each plot as a file)
  print(p)
  
  # Define the save directory
  save_dir <- paste0(working_directory, "/Results/SizeFrequency/RLS/", taxon_name)
  if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  # Save the plot
  ggsave(paste0(save_dir, "/density_RLS_", taxon_name, "_", finyear, ".png"), 
         plot = p, device = "png", scale = 1, width = 300, height = 100, units = "mm", limitsize = TRUE)
}
