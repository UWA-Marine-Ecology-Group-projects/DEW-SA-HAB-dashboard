

library(farver)
# Set seed so random numbers remain constant
set.seed(10)

# Only works if using R studio
#setwd(getSrcDirectory()[1])
working_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(working_directory)


# Input: vector of colours
# Output: same colours reordered to maximize contrast
shuffle_palette_evenly <- function(cols) {
  
  lch <- farver::decode_colour(cols, to = "lch")  # convert to LCh
  order <- integer(0)
  remaining <- seq_along(cols)
  
  # Start with first colour
  order <- c(order, remaining[1])
  remaining <- remaining[-1]
  
  while (length(remaining) > 0) {
    last <- order[length(order)]
    
    # Compute distance from last chosen colour
    dist <- farver::compare_colour(
      lch[last, , drop = FALSE],
      lch[remaining, , drop = FALSE],
      from_space = "lch"
    )
    
    # Pick the farthest
    next_idx <- remaining[which.max(dist)]
    order <- c(order, next_idx)
    remaining <- setdiff(remaining, next_idx)
  }
  
  cols[order]
}

MASTER_COLOURS <- qualitative_hcl(n = 50, palette = "Dynamic")

# reorder to maximize contrast
MASTER_COLOURS <- shuffle_palette_evenly(MASTER_COLOURS)

saveRDS(MASTER_COLOURS, "master_species_colours.rds")



library(Polychrome)
set.seed(123)

MASTER_COLOURS <- createPalette(50, c("#000000", "#FFFFFF"))  # 50 visually distinct colours
MASTER_COLOURS <- shuffle_palette_evenly(MASTER_COLOURS)
saveRDS(MASTER_COLOURS, "master_species_colours.rds")