# Load necessary libraries
library(tidyverse)
library(janitor) # For clean_names()
library(pacman)  # For cleaning workspace

###############################################################################################################
# Load and preprocess data
all_lf <- read.csv("Data/Bio_data_2024.06.03_long.csv", header = TRUE, sep = ",", 
                   stringsAsFactors = FALSE, check.names = FALSE) |> 
  as_tibble() |> 
  filter(!grepl("delete", notes)) |> # Remove rows marked for deletion
  select(sample_id, site_id, site_code, day:longitude, waterbody_name:river_type, 
         rank:order, family:taxon_name, abundance) |> 
  arrange(desc(waterbody_type), site_id, year)

# Create a dataframe with all unique site_code combinations
TD <- all_lf |> 
  distinct(site_code)

###############################################################################################################
# Calculate Dipteran species richness
DiptSppRich <- all_lf |>
  filter(order == "Diptera") |> # Filter for Diptera
  group_by(site_code) |>
  summarise(DiptSppRich = n_distinct(taxon_name), .groups = "drop") # Species richness

# Add species richness to TD and handle missing values
TD <- TD |>
  left_join(DiptSppRich, by = "site_code") |>
  mutate(DiptSppRich = replace_na(DiptSppRich, 0))

###############################################################################################################
# Calculate Dipteran family richness and abundance
DiptFamRich <- all_lf |>
  filter(order == "Diptera") |> # Filter for Diptera
  group_by(site_id, site_code, sample_id, day, month, year, waterbody_type, family) |>
  summarise(
    abundance = sum(abundance, na.rm = TRUE), # Sum abundance by family
    .groups = "drop"
  ) |> 
  arrange(desc(waterbody_type), site_id, year)

# Check for duplicate family entries (optional)
if (any(duplicated(DiptFamRich[c("site_code", "family")]))) {
  message("Duplicate families detected in site_code combinations.")
}

# Summarize family richness and abundance for each unique site_code
DiptFamRich <- DiptFamRich |>
  group_by(site_code) |>
  summarise(
    DiptFamRich = n_distinct(family),   # Family richness
    DiptAbund = sum(abundance, na.rm = TRUE), # Total abundance
    .groups = "drop"
  )

# Add family richness and abundance to TD and handle missing values
TD <- TD |>
  left_join(DiptFamRich, by = "site_code") |>
  mutate(
    DiptFamRich = replace_na(DiptFamRich, 0),
    DiptAbund = replace_na(DiptAbund, 0)
  )

###############################################################################################################
# Calculate vector species richness (VecRich)
VecRich <- all_lf |>
  filter(family %in% c("Simuliidae", "Ceratopogonidae", "Culicidae")) |> # Filter for vector families
  group_by(site_code) |> 
  summarise(
    VecRich = n_distinct(taxon_name),   # Calculate number of distinct vector species
    .groups = "drop"
  )

# Add VecRich to the TD dataset, filling missing values with 0
TD <- TD |>
  left_join(VecRich, by = "site_code") |>
  mutate(VecRich = replace_na(VecRich, 0))

###############################################################################################################
# Calculate indices for Dipteran parasite vector families
VecFamRich <- all_lf |>
  filter(family %in% c("Simuliidae", "Ceratopogonidae", "Culicidae")) |> # Filter for vector families
  arrange(desc(waterbody_type), site_id, year)

# Check for duplicate vector families (optional)
if (any(duplicated(VecFamRich[c("site_code", "family")]))) {
  message("Duplicate vector families detected in site_code combinations.")
}

VecFamRich <- VecFamRich |> 
  group_by(site_code, family) |> 
  summarise(
    abundance = sum(abundance, na.rm = TRUE), # Sum abundances for duplicates
    .groups = "drop"
  )

# Summarize vector family richness and abundance
VecFamRich <- VecFamRich |>
  group_by(site_code) |>
  summarise(
    VecFamRich = n_distinct(family),   # Vector family richness
    VecAbund = sum(abundance, na.rm = TRUE), # Vector abundance
    .groups = "drop"
  )

# Add vector indices to TD and handle missing values
TD <- TD |>
  left_join(VecFamRich, by = "site_code") |>
  mutate(
    VecFamRich = replace_na(VecFamRich, 0),
    VecAbund = replace_na(VecAbund, 0)
  )

###############################################################################################################
# Clean up column names for consistency
TD <- TD |> 
  clean_names()

###############################################################################################################
# Add site information to the final dataframe
site_factors <- all_lf |>
  select(site_id, sample_id, site_code:river_type) |> # Select site-level data
  distinct() |> # Ensure uniqueness
  mutate(across(everything(), ~ if_else(.x == "" | is.na(.x), NA_character_, as.character(.x)))) # Handle empty strings and NAs

# Merge site information with TD
TD <- site_factors |>
  left_join(TD, by = "site_code") |> 
  arrange(desc(waterbody_type), site_id, year) |> 
  clean_names()

# Check for duplicate sample IDs (optional)
if (any(duplicated(TD[c("sample_id")]))) {
  message("Duplicate sample IDs detected in the final TD dataset.")
}

###############################################################################################################
# Save the final dataset
write.csv(TD, "Outputs/Diptera_indices.csv", row.names = FALSE)

###############################################################################################################
# CLEAN UP WORKSPACE
rm(list = ls())       # Remove all objects from environment
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
message("Workspace cleaned. Ready for the next task!")
