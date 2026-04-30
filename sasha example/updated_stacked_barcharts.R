
####Stacked bar charts    ##### top ten abundance

counts$Taxon <- paste(counts$Genus_species)
counts_traits$Taxon <- paste(counts_traits$Genus_species)


traits <- traits %>%
  mutate(Colour = as.character(Colour))

MASTER_COLOURS <- readRDS("C:/Files/DEW HAB Analysis/master_species_colours.rds")

update_species_colours <- function(traits_df, species, colour_pool) {
  
  traits_df$Colour <- as.character(traits_df$Colour)
  
  # Existing colours
  used_colours <- traits_df$Colour[!is.na(traits_df$Colour)]
  
  # Species needing colours
  new_species <- species[!species %in%
                           traits_df$Taxon[!is.na(traits_df$Colour)]]
  
  if (length(new_species) > 0) {
    
    available_colours <- setdiff(colour_pool, used_colours)
    
    if (length(available_colours) < length(new_species)) {
      stop("❌ Not enough colours left in the master palette")
    }
    
    # Shuffle available colours for more contrast
    available_colours <- sample(available_colours, length(available_colours))
    
    # Assign one-to-one using named vector
    new_assign <- setNames(available_colours[1:length(new_species)], new_species)
    
    # Apply colours
    traits_df <- traits_df %>%
      mutate(
        Colour = if_else(
          Taxon %in% new_species,
          new_assign[Taxon],
          Colour
        )
      )
  }
  
  traits_df
}

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
      common <- gsub("'", "\\\\'", common)
    }
    
    # ---- Scientific name logic ----
    if (grepl(" spp\\.$", x)) {
      
      genus <- sub(" spp\\.$", "", x)
      sci_expr <- paste0("italic('", genus, "')~'spp.'")
      
    } else {
      
      sci_expr <- paste0("italic('", x, "')")
    }
    
    # ---- Stack common name underneath ----
    if (!is.null(common) && !is.na(common)) {
      parse(text = paste0("atop(", sci_expr, ", '", common, "')"))
    } else {
      parse(text = sci_expr)
    }
  })
}




top_n <- 10


top_species <- counts %>%
  group_by(AB, Taxon) %>%
  summarise(total = sum(Count), .groups = "drop") %>%
  group_by(AB) %>%
  arrange(desc(total), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  distinct(Taxon) %>%
  pull(Taxon)

counts %>%
  group_by(AB, Taxon) %>%
  summarise(total = sum(Count), .groups = "drop") %>%
  group_by(AB) %>%
  arrange(desc(total), .by_group = TRUE) %>%
  slice_head(n = 5)

traits <- update_species_colours(
  traits_df   = traits,
  species     = top_species,
  colour_pool = MASTER_COLOURS
)

write.csv(traits, "C:/Files/DEW HAB Analysis/species_traits_master.csv", row.names = FALSE)

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

palette_colors <- traits %>%
  filter(Taxon %in% top_species) %>%
  select(Taxon, Colour) %>%
  deframe()

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


#### PLOTS ####
#### PLOTS ####


levels(count.species.byAB$AB)
levels(other_labels$AB)


stacked_bar_common <- function(df, filename, species_lookup, top_species, other_labels = NULL) {
  
  # Ensure factor order
  species_levels <- c(top_species, "Other")
  df$Taxon_plot <- factor(df$Taxon_plot, levels = species_levels)
  
  # Palette: top species + grey for Other
  palette_colors <- traits %>%
    filter(Taxon %in% top_species) %>%
    select(Taxon, Colour) %>%
    deframe()
  
  palette_colors["Other"] <- "grey70"
  
  # Create legend labels: just common names
  labels <- sapply(names(palette_colors), function(x) {
    if (x == "Other") return("Other")
    
    cn <- species_lookup$Commonname[
      match(x, species_lookup$Taxon)
    ]
    
    if (is.na(cn) || cn == "") {
      return(x)  # fallback to scientific name
    }
    
    cn
  })
  
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
      breaks = species_levels,
      name   = "Species"
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
  filename = "StackedBar_CommonNames_ByAB_2.png",
  species_lookup = species_lookup,
  top_species = top_species,
  other_labels = other_labels
)


stacked_bar <- function(df, filename, label_df = NULL) {
  
  df$Taxon_plot <- factor(df$Taxon_plot, levels = species_levels)
  
  p <- ggplot(df, aes(x = 1, y = SpeciesPercentage, fill = Taxon_plot)) +
    geom_bar(stat = "identity", width = 0.8, colour = "black") +
    scale_fill_manual(
      values = palette_colors,
      labels = make_italic_labels(species_levels),
      breaks = species_levels,
      name   = "Species"
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
  "StackedBar_TotalAbundance_Species_scientific.png",
  label_df = other_labels
)

