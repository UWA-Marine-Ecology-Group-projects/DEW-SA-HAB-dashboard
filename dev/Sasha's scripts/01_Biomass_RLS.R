# Function to calculate biomass for Abalone
abalone_biomass <- function(length_mm) {
  # Constants for the biomass equation (adjust if necessary)
  a <- 1.012
  b <- 2.73
  biomass_g <- a * (length_mm ^ b)
  return(biomass_g)
}

# Function to calculate biomass for Southern Rock Lobster
lobster_biomass <- function(length_mm) {
  # Constants for the biomass equation (adjust if necessary)
  a <- 0.00036
  b <- 2.85
  biomass_g <- a * (length_mm ^ b)
  return(biomass_g)
}

# Example usage:
abalone_length <- 150  # in millimeters
lobster_length <- 100  # in millimeters

# Calculate biomass
abalone_bm <- abalone_biomass(abalone_length)
lobster_bm <- lobster_biomass(lobster_length)

# Output results
cat("Abalone biomass (g):", abalone_bm, "\n")
cat("Southern Rock Lobster biomass (g):", lobster_bm, "\n")