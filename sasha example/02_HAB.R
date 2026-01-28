rm(list=ls())

#.libPaths("C:/Users/Sasha/Documents/R/win-library/4.1")

# Load libraries
library(tidyverse)
library(vegan)
library(reshape)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(viridis)
library(scales)

# Set seed so random numbers remain constant
set.seed(10)

#### INPUT PARAMETERS ######################################################################

# Set the project name:
ProjectName <- "ALL_Windara_BRUVS"

############################################################################################

# Only works if using R studio
#setwd(getSrcDirectory()[1])
working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(working_directory)
setwd('..')
working_directory <- getwd()

# Import Data
setwd(paste(working_directory, "/data", sep = ""))

counts_og <- read.csv("ALL_Windara_BRUVS_Count_Metadata.csv", header = TRUE, stringsAsFactors = FALSE)
counts <- read.csv("ALL_Windara_BRUVS_Count_Metadata.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  dplyr::filter(!Location %in% "Windara")

lengths <- read.csv("ALL_Windara_BRUVS_Length_Metadata.csv", header = TRUE, stringsAsFactors = FALSE)
traits <- read.csv("SA_MP_BRUVS_ALL_traits_R.csv", header = TRUE, stringsAsFactors = FALSE)

samples_in_count <- counts %>%
  distinct(Sample, Successful.count, Successful.length, AB) %>%
  left_join(TotalAbundance)

test <- samples_in_count %>%
  dplyr::group_by(AB) %>%
  dplyr::summarise(Mean = mean(TotalAbundance, na.rm = TRUE), SE   = se(TotalAbundance))

num_samples <- samples_in_count %>%
  group_by(AB) %>%
  dplyr::summarise(n=n())


write_csv(samples_in_count, "windara-samples.csv")

#counts$Taxon <- as.character(counts$Taxon)


missing_species <- counts %>%
  filter(!Genus_species %in% traits$Taxon) %>%
  distinct(Genus_species)

missing_species

counts_traits <- merge(counts, traits, by.x = "Genus_species", by.y = "Taxon", all.x = TRUE) 


counts <- counts %>%
  mutate(
    Site = case_when(
      str_detect(Sample, "GLCC") ~ "Reef control",
      str_detect(Sample, "GLSC") ~ "Seagrass control",
      str_detect(Sample, "GLNC") ~ "Sand control",
      str_detect(Sample, "GL_T") ~ "Oyster Reef",
      str_detect(Sample, "GLT") ~ "Oyster Reef",
      str_detect(Sample, "WISC") ~ "South control",
      str_detect(Sample, "WINC") ~ "North control",
      str_detect(Sample, "WI_T") ~ "Oyster Reef",
      str_detect(Sample, "WIT") ~ "Oyster Reef", 
      TRUE ~ NA_character_
    )
  )

# Set output directory
if(dir.exists(paste(working_directory, "/results", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/results", sep = ""))
}
#if(dir.exists(paste(working_directory, "/results/HabitatFigures/2015", sep = "")) == FALSE){
# dir.create(paste(working_directory, "/results/HabitatFigures/2015", sep = ""))
#}
setwd(paste(working_directory, "/results", sep = ""))

counts$Genus_species <- as.character(counts$Genus_species)

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
  mutate(Species = case_when(
    Genus == "Portunus" & Species == "pelagicus" ~ "armatus",    
    TRUE ~ Species),
    # Create Genus_species
    Genus_species = case_when(
      is.na(Genus) | Genus == "" ~ paste(Family, Species),
      TRUE ~ paste(Genus, Species)
    )
  )


#### Calculate species richness of the entire assemblage ####

SpeciesRichness <- counts %>%
  dplyr::select(c(Sample, Genus_species)) %>%
  dplyr::group_by(Sample) %>%
  dplyr::summarise(SpeciesRichness = n_distinct(Genus_species))
head(SpeciesRichness,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness <- counts %>%
  dplyr::select(c(Sample, Family))%>%
  dplyr::group_by(Sample)%>%
  dplyr::summarise(FamilyRichness = n_distinct(Family))
head(FamilyRichness,2)


#### Calculate the total abundance at each site ####

TotalAbundance <- counts %>%
  dplyr::select(c(Sample, Count))%>%
  dplyr::group_by(Sample)%>%
  dplyr::summarise(TotalAbundance = sum(Count))
head(TotalAbundance,2)

#### Calculate functional groups####

Functionalgroup <- counts_traits %>%
  dplyr::select(Sample, Genus_species, Functionalgroup) %>%
  dplyr::group_by(Sample, Functionalgroup) %>%
  dplyr::summarise(n_species = n_distinct(Genus_species), .groups = "drop") %>%
  pivot_wider(
    names_from = Functionalgroup,
    values_from = n_species,
    values_fill = 0  # fill missing combos with 0
  )

head(Functionalgroup, 2)

#### Calculate diet####

Diet <- counts_traits %>%
  dplyr::select(Sample, Genus_species, Diet) %>%
  dplyr::group_by(Sample, Diet) %>%
  dplyr::summarise(n_species = n_distinct(Genus_species), .groups = "drop") %>%
  pivot_wider(
    names_from = Diet,
    values_from = n_species,
    values_fill = 0  # fill missing combos with 0
  )

head(Diet, 2)


#### Calculate Type####

Type <- counts_traits %>%
  dplyr::select(Sample, Genus_species, Type) %>%
  dplyr::group_by(Sample, Type) %>%
  dplyr::summarise(n_species = n_distinct(Genus_species), .groups = "drop") %>%
  pivot_wider(
    names_from = Type,
    values_from = n_species,
    values_fill = 0  # fill missing combos with 0
  )

head(Type, 2)

summary_table <- counts %>%
  select("Sample", "Location", "Site", "Year", "AB") %>%
  unique()

summary_table <- left_join(summary_table, SpeciesRichness, by = "Sample")
summary_table <- left_join(summary_table, FamilyRichness, by = "Sample")
summary_table <- left_join(summary_table, TotalAbundance, by = "Sample")
summary_table <- left_join(summary_table, Functionalgroup, by = "Sample")
summary_table <- left_join(summary_table, Diet, by = "Sample")
summary_table <- left_join(summary_table, Type, by = "Sample")

##add in reps with no fish observed

#nofish <- read.csv("No_fish_reps.csv", header = TRUE, stringsAsFactors = FALSE)

#summary_table <- bind_rows(summary_table, nofish)
#numeric_cols <- summary_table %>%
 # select(where(is.numeric)) %>%
 # names()

#summary_table <- summary_table %>%
 # mutate(across(all_of(numeric_cols), ~replace_na(.x, 0)))


# Set output directory
if(dir.exists(paste(working_directory, "/results", sep = "")) == FALSE){
  dir.create(paste(working_directory, "/results", sep = ""))
}
#if(dir.exists(paste(working_directory, "/results/HabitatFigures/2015", sep = "")) == FALSE){
# dir.create(paste(working_directory, "/results/HabitatFigures/2015", sep = ""))
#}
setwd(paste(working_directory, "/results", sep = ""))


write.csv(summary_table, file = paste(ProjectName, "_SummaryTable.csv", sep = ""), row.names = FALSE, na = "")



##### mean DATA ##########################
# Define a standard error helper
se <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# Specify column range (numeric indices) in summary_table
col_range <- 4:21  # adjust as needed

# Step 1: compute mean and SE
Summary.MeanSE <- summary_table %>%
  dplyr::group_by(AB, Site) %>%
  dplyr::summarise(
    n = n(),
    across(
      .cols = col_range,
      .fns = list(Mean = ~mean(.x, na.rm = TRUE),
                  SE   = ~se(.x)),
      .names = "{.col}.{.fn}"
    ),
    .groups = "drop"
  )

# Step 2: round numeric columns
Summary.MeanSE <- Summary.MeanSE %>%
  mutate(across(ends_with(".Mean"), round, 2),
         across(ends_with(".SE"), round, 2))

col_range <- 5:21

Overall.MeanSE <- summary_table %>%
  dplyr::group_by(AB) %>%         # still compute separately for Pre-AB and AB
  dplyr::summarise(
    n = n(),
    across(
      .cols = col_range,
      .fns = list(Mean = ~mean(.x, na.rm = TRUE),
                  SE   = ~se(.x)),
      .names = "{.col}.{.fn}"
    ),
    Site = "Overall",
    .groups = "drop"
  )

Summary.MeanSE.withOverall <- bind_rows(Summary.MeanSE, Overall.MeanSE)


write.csv(Summary.MeanSE.withOverall, file = paste(ProjectName, "_Summary_MeanSE.csv", sep = ""), row.names = FALSE, na = "")


###### Report table
# Step 1: select only the AB, Site, and numeric mean columns
mean_cols <- grep(".Mean$", colnames(Summary.MeanSE.withOverall), value = TRUE)

df_long <- Summary.MeanSE.withOverall %>%
  dplyr::select(AB, Site, all_of(mean_cols)) %>%
  pivot_longer(
    cols = all_of(mean_cols),
    names_to = "Metric",
    values_to = "Value"
  )

# Step 2: spread AB into columns
df_wide <- df_long %>%
  pivot_wider(
    names_from = AB,
    values_from = Value
  )

# Step 4: calculate percent change
df_wide <- df_wide %>%
  mutate(PercentChange = (`AB` - `Pre-AB`) / `Pre-AB` * 100)

# Step 5: pivot wider so each Site is a column
PercentChange_table <- df_wide %>%
  select(Site, Metric, PercentChange) %>%
  pivot_wider(
    names_from = Site,
    values_from = PercentChange
  )

PercentChange_table


write.csv(PercentChange_table, file = paste(ProjectName, "_Percent_change.csv", sep = ""), row.names = FALSE, na = "")




colour.schema <- c("Oyster reef" = "#785941", 
                   "Sand control" = "#ebc365",
                   "Reef control" = "#a9896b", 
                   "Seagrass control" = "#5dd91a", 
                   #"Sand w/ epi" = "#f28fc7", 
                   #"Deep" = "#4e94f5", 
                   #"Middle" = "#0c632d", 
                   #"Shallow" = "#6e6e6e",
                   "Pre-AB" = "#072759",
                   "AB" = "#e88e98")

# Plot mean se as bar charts


# ─────────────────────────────────────────────
# 1. Set up readable labels for each metric
# ─────────────────────────────────────────────

metric_labels <- c(
  "SpeciesRichness" = "Average species richness (± SE)",
  "FamilyRichness" = "Average Family richness (± SE)",
  "TotalAbundance" = "Average Total abundance (± SE)",
  "demersal" = "Average no. of demersal taxa (± SE)",
  "reef-associated" = "Average no. of reef-associated taxa (± SE)",
  "benthopelagic" = "Average no. of benthopelagic taxa (± SE)",
  "pelagic-neritic" = "Average no. of neritic pelagic taxa (± SE)",
  "pelagic-oceanic" = "Average no. of oceanic pelagic taxa (± SE)",
  "Carnivore" = "Average no. of carnivore taxa (± SE)",
  "Omnivore" = "Average no. of omnivore taxa (± SE)",
  "Herbivore" = "Average no. of herbivore taxa (± SE)",
  "Planktivore" = "Average no. of planktivore taxa (± SE)",
  "Chondrichthyes" = "Average no. of sharks and rays (± SE)",
  "Invertebrate" = "Average no. of invertebrate taxa (± SE)",
  "Teleost" = "Average no. of bony fish taxa (± SE)"
)

site_labels <- c(
  "Oyster Reef" = "Shellfish reef",
  "Reef control" = "Natural Reef",
  "Sand control" = "Sand",
  "Seagrass control" = "Seagrass"
)

# Detect `.Mean` columns
mean_cols <- Summary.MeanSE.withOverall %>%
  select(ends_with(".Mean")) %>%
  names()

# Extract metric root names
metrics <- gsub("\\.Mean$", "", mean_cols)


#Summary.MeanSE.withOverall$Site <- factor(Summary.MeanSE.withOverall$Site,
#levels = c("Pre-AB", "AB"))
Summary.MeanSE.withOverall$AB <- factor(Summary.MeanSE.withOverall$AB, levels = c("Pre-AB", "AB"))

# ─────────────────────────────────────────────
# 2. Loop through each metric to generate plots
# ─────────────────────────────────────────────

for (m in metrics) {
  
  mean_col <- sym(paste0(m, ".Mean"))
  se_col   <- sym(paste0(m, ".SE"))
  
  pretty_name <- metric_labels[m]
  
  df <- Summary.MeanSE.withOverall %>% 
    filter(Site != "Overall")   # <-- exclude Overall for now
  
  p <- ggplot(df, aes(x = Site, y = !!mean_col, fill = AB)) +
    geom_col(position = position_dodge(width = 0.9)) +
    scale_x_discrete(labels = site_labels) +
    geom_errorbar(aes(
      ymin = !!mean_col - !!se_col,
      ymax = !!mean_col + !!se_col,
      group = AB
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = colour.schema) +
    labs(
      title = pretty_name,
      x = "",
      y = pretty_name
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.title = element_blank(),
      text = element_text(size = 14)
    )
  
  print(p)
  
  ggsave(
    paste0("Plot_", m, "_bySite.png"),
    plot = p,
    width = 160, height = 100, units = "mm", dpi = 300
  )
}

# ─────────────────────────────────────────────
# 3. Generate SEPARATE OVERALL plot (Pre-AB vs AB)
# ─────────────────────────────────────────────

overall_df <- Summary.MeanSE.withOverall %>% 
  filter(Site == "Overall")

for (m in metrics) {
  
  mean_col <- sym(paste0(m, ".Mean"))
  se_col   <- sym(paste0(m, ".SE"))
  
  pretty_name <- metric_labels[m]
  
  p_overall <- ggplot(overall_df, aes(x = AB, y = !!mean_col, fill = AB)) +
    geom_col(width = 0.6) +
    geom_errorbar(aes(
      ymin = !!mean_col - !!se_col,
      ymax = !!mean_col + !!se_col),
      width = 0.2
    ) +
    scale_fill_manual(values = colour.schema) +
    labs(
      title = paste(pretty_name, "- Overall"),
      x = "",
      y = pretty_name
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      text = element_text(size = 14)
    )
  
  print(p_overall)
  
  ggsave(
    paste0("Plot_", m, "_Overall.png"),
    plot = p_overall,
    width = 120, height = 90, units = "mm", dpi = 300
  )
}


####stacked bar charts

diet_cols <-  c("Carnivore", "Omnivore", "Herbivore", "Planktivore", "NA.y")
functional_cols <-c("demersal", "reef-associated", "benthopelagic", "pelagic-neritic", "pelagic-oceanic", "NA.x")





# Rename NA groups
rename_groups <- function(x, type) {
  if (type == "functional") {
    x <- gsub("NA.x", "Inverts/Unknown (Functional)", x)
  } else if (type == "diet") {
    x <- gsub("NA.y", "Inverts/Unknown (Diet)", x)
  }
  
  x <- gsub("-", " ", x)
  x <- tools::toTitleCase(x)
  return(x)
}

### ---- DATA CLEANING ----

summary_table <- summary_table %>%
  mutate(
    Site = recode(Site, !!!site_labels),
    AB   = factor(AB, levels = c("Pre-AB", "AB"))
  )

### ---- STACKED CHART HELPER FUNCTION ----

make_stacked_plot <- function(data, group_cols, group_name, file_out, type) {
  
  long_df <- data %>%
    select(Sample, Site, AB, all_of(group_cols)) %>%
    pivot_longer(
      cols = all_of(group_cols),
      names_to = group_name,
      values_to = "Count"
    ) %>%
    mutate(
      !!group_name := rename_groups(.data[[group_name]], type)
    )
  
  summary_df <- long_df %>%
    group_by(Site, AB, .data[[group_name]]) %>%
    summarise(Total = mean(Count, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(summary_df,
              aes(x = AB, y = Total, fill = .data[[group_name]])) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~ Site) +
    scale_fill_brewer(palette = "Set3") +
    theme_classic() +
    labs(
      x = "Treatment",
      y = "Average number of taxa",
      fill = group_name
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save
  ggsave(
    paste0(file_out, ".png"),
    plot = p,
    width = 120, height = 90, units = "mm", dpi = 300
  )
  
  return(p)
}

### ---- GENERATE PLOTS ----

stacked_plot_fun <- make_stacked_plot(
  summary_table, functional_cols, "FunctionalGroup",
  "Plot_functional", type = "functional"
)

stacked_plot_diet <- make_stacked_plot(
  summary_table, diet_cols, "DietGroup",
  "Plot_diet", type = "diet"
)

stacked_plot_fun
stacked_plot_diet




####Top five


# Filter Pre-AB and sum abundance by species
top5_PreAB <- counts_traits %>%
  filter(AB == "Pre-AB") %>%
  group_by(Genus_species) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 5)

# Get their abundance in AB
top5_PreAB_compare <- counts_traits %>%
  filter(Genus_species %in% top5_PreAB$Genus_species,
         AB %in% c("Pre-AB", "AB")) %>%
  group_by(Genus_species, AB) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  complete(Genus_species, AB, fill = list(Count = 0))  # <-- fills missing with 0

top5_PreAB_compare$AB <- factor(top5_PreAB_compare$AB, levels = c("Pre-AB", "AB"))

PreAB5 <- ggplot(top5_PreAB_compare, aes(x = Genus_species, y = Count, fill = AB)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Top 5 species pre-bloom",
    x = "Species",
    y = "Count"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  paste0("Plot_Top5Pre.png"),
  plot = PreAB5,
  width = 120, height = 90, units = "mm", dpi = 300
)

top5_AB <- counts_traits %>%
  filter(AB == "AB") %>%
  group_by(Genus_species) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 5)

# Get their abundance in Pre-AB
top5_AB_compare <- counts_traits %>%
  filter(Genus_species %in% top5_AB$Genus_species,
         AB %in% c("Pre-AB", "AB")) %>%
  group_by(Genus_species, AB) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  complete(Genus_species, AB, fill = list(Count = 0))  # <-- fills missing with 0

top5_AB_compare$AB <- factor(top5_AB_compare$AB, levels = c("Pre-AB", "AB"))

top5AB <- ggplot(top5_AB_compare, aes(x = Genus_species, y = Count, fill = AB)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Top 5 species in bloom",
    x = "Species",
    y = "Count"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  paste0("Plot_top5AB.png"),
  plot = top5AB,
  width = 120, height = 90, units = "mm", dpi = 300
)



####Stacked bar charts    #####

counts$Taxon <- paste(counts$Genus_species)
counts_traits$Taxon <- paste(counts_traits$Genus_species)

#### FUNCTIONS ####

#--- Automatically format species labels into italics ---#
species_lookup <- counts_traits %>%
  distinct(Taxon, Commonname)


make_italic_labels <- function(taxa, lookup = NULL) {
  
  sapply(taxa, function(x) {
    
    # ---- Leave Other alone ----
    if (x == "Other") return("Other")
    
    # ---- Look up common name if available ----
    common <- NULL
    if (!is.null(lookup) && x %in% lookup$Taxon) {
      common <- lookup$Commonname[match(x, lookup$Taxon)]
      common <- gsub("'", "\\\\'", common)  # escape apostrophes
    }
    
    # ---- Build scientific name expression ----
    sci_expr <- if (grepl(" spp\\.", x)) {
      paste0("italic('", sub(" spp\\.", "", x), "')~' spp.'")
    } else {
      paste0("italic('", x, "')")
    }
    
    # ---- Stack common name underneath using atop ----
    if (!is.null(common) && !is.na(common)) {
      parse(text = paste0("atop(", sci_expr, ", '", common, "')"))
    } else {
      parse(text = sci_expr)
    }
  })
}




top_n <- 10



top_species <- counts %>%
  group_by(Taxon) %>%
  summarise(total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = top_n) %>%
  pull(Taxon)

other_counts_byAB <- counts %>%
  filter(!Taxon %in% top_species) %>%   # species that became "Other"
  distinct(AB, Taxon) %>%
  count(AB, name = "n_other")

counts_plot <- counts %>%
  mutate(
    Taxon_plot = if_else(Taxon %in% top_species, Taxon, "Other")
  )


Plot.Data.Setup <- function(df, group_var, value_var, filter_var = NULL) {
  
  group_var <- rlang::ensym(group_var)
  value_var <- rlang::ensym(value_var)
  
  if (!is.null(filter_var)) {
    
    out <- df %>%
      filter(!is.na(.data[[filter_var]])) %>%
      group_by(.data[[filter_var]], !!group_var) %>%
      summarise(
        Count = sum(!!value_var, na.rm = TRUE),
        .groups = "drop_last"
      ) %>%
      mutate(SpeciesPercentage = Count / sum(Count) * 100) %>%
      ungroup()
    
  } else {
    
    out <- df %>%
      group_by(!!group_var) %>%
      summarise(
        Count = sum(!!value_var, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(SpeciesPercentage = Count / sum(Count) * 100)
  }
  
  out
}


count.species.overall <- Plot.Data.Setup(
  counts_plot,
  Taxon_plot,
  Count
)

count.species.byAB <- Plot.Data.Setup(
  counts_plot,
  Taxon_plot,
  Count,
  filter_var = "AB"
)

other_labels <- count.species.byAB %>%
  filter(Taxon_plot == "Other") %>%
  left_join(other_counts_byAB, by = "AB") %>%
  mutate(
    label = paste0(n_other, " spp."),
    ypos  = SpeciesPercentage / 2
  )

species_levels <- c(top_species, "Other")

palette_colors <- viridis::viridis(length(species_levels))
names(palette_colors) <- species_levels
palette_colors["Other"] <- "grey70"

labels <- make_italic_labels(
  species_levels,
  lookup = species_lookup)



count.species.byAB$AB <- factor(
  count.species.byAB$AB,
  levels = c("Pre-AB", "AB")
)

other_labels <- other_labels %>%
  mutate(
    AB = factor(AB, levels = c("Pre-AB", "AB"))
  )

scale_fill_manual(
  values = palette_colors,
  labels = make_italic_labels(species_levels)
)

#### PLOTS ####
#### PLOTS ####
stacked_bar <- function(df, filename, label_df = NULL) {
  
  message("📊 Plotting file: ", filename)
  
  df$Taxon_plot <- factor(df$Taxon_plot, levels = species_levels)
  
  p <- ggplot(df, aes(x = 1, y = SpeciesPercentage, fill = Taxon_plot)) +
    geom_bar(stat = "identity", width = 0.8, colour = "black") +
    facet_wrap(~ AB) +
    scale_fill_manual(
      values = palette_colors,
      labels = make_italic_labels(species_levels, lookup = species_lookup),
      name = "Species"
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      y = "Percentage of observations",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      text = element_text(size = 15),
      legend.key.height = unit(0.8, "lines"),  # height of colour boxes
      legend.key.width  = unit(1, "lines"),    # width of colour boxes
      legend.text = element_text(size = 9)
    )
  
  # ---- Overlay "Other (x spp.)" text ----
  if (!is.null(label_df)) {
    p <- p +
      geom_text(
        data = label_df,
        aes(x = 1, y = ypos, label = label),
        inherit.aes = FALSE,
        colour = "black",
        size = 4,
        fontface = "bold"
      )
  }
  
  print(p)
  ggsave(filename, plot = p, width = 160, height = 120, units = "mm")
}

stacked_bar(
  count.species.byAB,
  "StackedBar_TotalAbundance_Species_ByAB.png",
  label_df = other_labels
)


levels(count.species.byAB$AB)
levels(other_labels$AB)


stacked_bar_common <- function(df, filename, species_lookup, top_species, other_labels = NULL) {
  
  # Ensure factor order
  species_levels <- c(top_species, "Other")
  df$Taxon_plot <- factor(df$Taxon_plot, levels = species_levels)
  
  # Palette: top species + grey for Other
  palette_colors <- viridis::viridis(length(species_levels))
  names(palette_colors) <- species_levels
  palette_colors["Other"] <- "grey70"
  
  # Create legend labels: just common names
  labels <- sapply(species_levels, function(x) {
    if (x == "Other") return("Other")
    common <- species_lookup$Commonname[match(x, species_lookup$Taxon)]
    common
  })
  names(labels) <- species_levels
  
  # Ensure AB order (left = Pre-AB, right = AB)
  if ("AB" %in% colnames(df)) {
    df$AB <- factor(df$AB, levels = c("Pre-AB", "AB"))
  }
  
  # Build plot
  p <- ggplot(df, aes(x = 1, y = SpeciesPercentage, fill = Taxon_plot)) +
    geom_bar(stat = "identity", width = 0.8, colour = "black") +
    facet_wrap(~ AB) +
    scale_fill_manual(
      values = palette_colors,
      labels = labels,
      name = "Species"
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(y = "Percentage of observations", x = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_text(face = "bold"),
      text = element_text(size = 15)
    )
  
  # Overlay "Other (x spp.)" if provided
  if (!is.null(other_labels)) {
    p <- p +
      geom_text(
        data = other_labels,
        aes(x = 1, y = ypos, label = label),
        inherit.aes = FALSE,
        colour = "black",
        size = 4,
        fontface = "bold"
      )
  }
  
  print(p)
  ggsave(filename, plot = p, width = 160, height = 120, units = "mm")
}

stacked_bar_common(
  df = count.species.byAB,
  filename = "StackedBar_CommonNames_ByAB.png",
  species_lookup = species_lookup,
  top_species = top_species,
  other_labels = other_labels
)





make_italic_labels <- function(x) {
  sapply(x, function(i) {
    if (i == "Other") return("Other")
    if (grepl(" spp\\.", i)) {
      parse(text = gsub("^(.*) spp\\.", "paste(italic('\\1'), ' spp.')", i))
    } else if (grepl(" ", i)) {
      parse(text = gsub("^(.*) (.*)$", "italic('\\1 \\2')", i))
    } else {
      parse(text = paste0("italic('", i, "')"))
    }
  })
}


stacked_bar <- function(df, filename, label_df = NULL) {
  
  df$Taxon_plot <- factor(df$Taxon_plot, levels = species_levels)
  
  p <- ggplot(df, aes(x = 1, y = SpeciesPercentage, fill = Taxon_plot)) +
    geom_bar(stat = "identity", width = 0.8, colour = "black") +
    scale_fill_manual(values = palette_colors, labels = make_italic_labels(species_levels)) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(y = "Percentage of observations", x = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      text = element_text(size = 15)
    )
  
  # ---- Facet only if AB exists ----
  if ("AB" %in% colnames(df)) {
    p <- p + facet_wrap(~ AB)
  }
  
  # ---- Overlay Other spp. labels ----
  if (!is.null(label_df)) {
    p <- p +
      geom_text(
        data = label_df,
        aes(x = 1, y = ypos, label = label),
        inherit.aes = FALSE,
        colour = "black",
        size = 4,
        fontface = "bold"
      )
  }
  
  print(p)
  ggsave(filename, plot = p, width = 160, height = 120, units = "mm")
}


stacked_bar(
  count.species.byAB,
  "StackedBar_TotalAbundance_Species_ByAB.png",
  label_df = other_labels
)

