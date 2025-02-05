# Load the required packages
library(tidyverse)
library(geosphere)
library(vegan)
library(codyn)
library(mobr)
library(reshape2)

# Load data
LT_data <- as_tibble(read.csv("Data/Bio_data_2024.02.26_long.csv", sep = ",", header = TRUE))
LT_site_factors <- as_tibble(read.csv("Data/LT_river_lake_site_factors.csv", sep = ",", header = TRUE))
LT_env_yr <- as_tibble(read.csv("Data/Bio_Env_Linked_YrlyMeans_26.02.2024.csv", header =  TRUE)) %>%
  select(site_code, starts_with("Yr_")) # only keep the columns we are interested in
LT_env_gs <- as_tibble(read.csv("Data/Bio_Env_Linked_GrowSeasonMeans_26.02.2024.csv", header =  TRUE)) %>%
  select(site_code, starts_with("GS_")) # only keep the columns we are interested in

# Create diptera only dataset - short
Diptera <- LT_data %>%
  filter(order == "Diptera") %>%
  select(-c(life_stage, ephemeroptera:eptc)) # Adjust this with the actual column names you wish to remove

# Clean dataset by filtering our sites that do not meet our criteria (at least 10-year observation period with at least 5 sampling events)
# Step 1: Calculate the observation period for each site and filter for a period of at least 10 years
filtered_sites <- Diptera %>%
  group_by(site_id) %>%
  summarize(
    observation_period = max(year) - min(year) + 1,
    sampling_events = n_distinct(year)
  ) %>%
  filter(observation_period >= 10, sampling_events >= 5) %>%
  ungroup()
# Step 2: Filter the original dataset to keep only the sites that meet the criteria
Diptera_clean <- Diptera %>%
  semi_join(filtered_sites, by = "site_id")

# Joining additional site information based on site_code
Diptera_full <- Diptera_clean %>%
  left_join(LT_site_factors %>% 
              select(-sample_id, -site_id, -date, -day, -month, -year), 
            by = "site_code") %>%
  left_join(LT_env_yr, by = "site_code") %>%
  left_join(LT_env_gs, by = "site_code") 

# Calculate distances of each site from the Rybachy ornithological station
Rybachy_latitude <- 55.15373
Rybachy_longitude <- 20.85778
# Calculate distances
Diptera_full <- Diptera_full %>%
  mutate(dist_rybachy_km = distHaversine(cbind(longitude, latitude), c(Rybachy_longitude, Rybachy_latitude)) / 1000) # %>%  
  # filter(dist_rybachy_km <= 100) # we could filter the sites here, only keep those with a distance of e.g., 100km from the Rybachy ornithological station

# write.csv(Diptera_full, "Data/LT_dipteran_dataset_filtered.csv", row.names = F)

# summarise data to the family level
Diptera_families <- Diptera_full %>%
  select(-taxon_code, -taxon_name, -taxon_id, -genus, -species, -class, -order) %>% # Remove specified columns
  group_by(sample_id, site_code, site_id, date, day, month, year, family, bmwp, river_basin_district, latitude, longitude, wb_type, river_type, dist_rybachy_km) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>% # Sum abundance for each family and remove duplicates
  arrange(site_id, year)

# Joining additional site information based on site_code
Diptera_families <- Diptera_families %>%
  left_join(LT_site_factors %>% 
              select(-sample_id, -site_id, -date, -day, -month, -year, -river_basin_district, -latitude, -longitude, -wb_type, -river_type), 
            by = "site_code") %>%
  left_join(LT_env_yr, by = "site_code") %>%
  left_join(LT_env_gs, by = "site_code")

# # Filter sites, only keep those sampled in both 2010 and 2022
# Diptera_families_filtered <- Diptera_families %>%
#   semi_join(filtered_sites %>% filter(observation_period == 13), by = "site_id")

# initial data exploration
# Load necessary libraries
library(sf)
library(rnaturalearth)

# How has dipteran abundance chnages through time
# Aggregate the abundances for each site for each year
Diptera_aggregated <- Diptera_families %>%
  group_by(year, site_id, latitude, longitude) %>%
  summarize(total_abundance = sum(abundance), .groups = 'drop')

# Convert the aggregated data to an sf object with CRS definition
data_sf <- st_as_sf(Diptera_aggregated, coords = c("longitude", "latitude"), crs = 4326)

# Get country borders, ensuring valid geometries
countries <- ne_countries(scale = "medium", returnclass = "sf") %>% st_make_valid()

# Extract Lithuania's boundary for highlighting
lithuania_boundary <- countries %>% 
  filter(admin == "Lithuania")

# Identify Lithuania and its surrounding countries for labeling
countries_to_label <- c("Lithuania", "Latvia", "Belarus", "Poland", "Russia", "Sweden", "Germany")

# Calculate centroids for labeling, ensuring to retain 'admin' for country names.
label_countries <- countries %>%
  filter(admin %in% countries_to_label) %>%
  st_centroid()

# Extract coordinates and country names for labels
label_countries_df <- data.frame(
  admin = label_countries$admin,
  longitude = st_coordinates(label_countries)[,1],
  latitude = st_coordinates(label_countries)[,2]
)

# Creating faceted maps with highlighted Lithuania and accurate labels
p1 <- ggplot() +
  geom_sf(data = countries, fill = "light gray", color = "gray") + # All countries in light gray
  geom_sf(data = lithuania_boundary, fill = "antiquewhite", color = "gray") + # Lithuania highlighted
  geom_sf(data = data_sf, aes(size = total_abundance), color = "red", alpha = 0.6) +
  geom_text(data = label_countries_df, aes(x = longitude, y = latitude, label = admin), size = 3, check_overlap = TRUE) +
  scale_size(range = c(2, 10), name = "Total Abundance") +
  facet_wrap(~ year, ncol = 3) + # Adjust the number of columns as needed
  labs(title = "Dipteran Abundance at Riverine Sites in Lithuania, by Year",
       caption = "Data aggregated by site and year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(20.83, 26.93), ylim = c(53.75, 56.55), expand = FALSE) # Slightly zoomed-out view
p1

# tiff("Plots/Total dipteran abundance by year.tiff", width = 10, height = 15, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p1
# dev.off()

# How has Dipteran family richness changed through time?
# Aggregate to calculate richness for each site for each year
Diptera_richness <- Diptera_families %>%
  group_by(year, site_id, latitude, longitude) %>%
  summarize(richness = n_distinct(family), .groups = 'drop')

# Convert the aggregated data to an sf object with CRS definition
data_sf <- st_as_sf(Diptera_richness, coords = c("longitude", "latitude"), crs = 4326)

# Adjust the plot to represent richness
p2 <- ggplot() +
  geom_sf(data = countries, fill = "light gray", color = "gray") +
  geom_sf(data = lithuania_boundary, fill = "antiquewhite", color = "gray") +
  geom_sf(data = data_sf, aes(size = richness), color = "blue", alpha = 0.6) +
  geom_text(data = label_countries_df, aes(x = longitude, y = latitude, label = admin), size = 3, check_overlap = TRUE) +
  scale_size(range = c(2, 10), name = "Family Richness") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Dipteran Family Richness at Riverine Sites in Lithuania, by Year",
       caption = "Data aggregated by site and year") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(20.83, 26.93), ylim = c(53.75, 56.55), expand = FALSE)
p2

# tiff("Plots/Total dipteran richness by year.tiff", width = 10, height = 15, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p2
# dev.off()

# How have the abundances of dipteran families changed through time?
# Aggregate the abundance for each dipteran family for each year
family_abundance_yearly <- Diptera_families %>%
  group_by(year, family) %>%
  summarize(total_abundance = sum(abundance), .groups = 'drop')

# Plotting the change in dipteran family abundance across all sites through time
p3 <- ggplot(family_abundance_yearly, aes(x = year, y = log(total_abundance + 1), color = family)) +
  geom_line(alpha = 0.7, linewidth = 1) + # Reduced opacity for better visibility of overlaps
  theme_minimal(base_size = 14) + # Cleaner theme with larger base text size for readability
  labs(title = "Trends in dipteran family abundance between 2010 and 2020",
       subtitle = "Log-transformed abundance across all sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year.") +
  theme(legend.position = "right", # Adjust legend position
        legend.title = element_blank(), # Remove legend title
        plot.title = element_text(face = "bold", size = 16), # Bold and larger title
        plot.subtitle = element_text(size = 14), # Slightly smaller subtitle
        plot.caption = element_text(size = 10), # Smaller caption
        axis.text = element_text(size = 12), # Larger axis texts
        axis.title = element_text(size = 14), # Larger axis titles
        legend.text = element_text(size = 10)) + # Smaller legend texts
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) # Ensure x-axis is not cluttered
p3

# tiff("Plots/Change in dipteran family abundance by year - different colours.tiff", width = 10, height = 10, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p3
# dev.off()

p4 <- ggplot(family_abundance_yearly, aes(x = year, y = log(total_abundance + 1), color = family)) +
  geom_line(aes(linewidth = ifelse(family %in% c("Ceratopogonidae", "Culicidae", "Simuliidae"), 2, 0.5)), alpha = 0.7) + # Conditional line thickness
  scale_size_identity() + # Apply the size mapping directly as given
  theme_minimal(base_size = 14) + # Cleaner theme with larger base text size for readability
  labs(title = "Trends in dipteran family abundance between 2010 and 2020",
       subtitle = "Log-transformed abundance across all sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year.") +
  theme(legend.position = "right", # Adjust legend position
        legend.title = element_blank(), # Remove legend title
        plot.title = element_text(face = "bold", size = 16), # Bold and larger title
        plot.subtitle = element_text(size = 14), # Slightly smaller subtitle
        plot.caption = element_text(size = 10), # Smaller caption
        axis.text = element_text(size = 12), # Larger axis texts
        axis.title = element_text(size = 14), # Larger axis titles
        legend.text = element_text(size = 10)) + # Smaller legend texts
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) # Ensure x-axis is not cluttered
p4

# tiff("Plots/Change in dipteran family abundance by year - different colours and lines.tiff", width = 10, height = 10, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p4
# dev.off()

# How have the relative abundances of dipteran families changed through time?
# Calculate the total annual abundance across all families
annual_abundance <- family_abundance_yearly %>%
  group_by(year) %>%
  summarize(total_annual_abundance = sum(total_abundance))

# Join the total annual abundance back to the original dataset and calculate percentages
family_abundance_percentage <- family_abundance_yearly %>%
  left_join(annual_abundance, by = "year") %>%
  mutate(percentage = (total_abundance / total_annual_abundance) * 100) %>%
  select(year, family, percentage)

# Plot the proportional changes for each family over time
p5 <- ggplot(family_abundance_percentage, aes(x = year, y = percentage, color = family)) +
  geom_line(alpha = 0.7, size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Trends in dipteran family relative abundance between 2010 and 2022",
       subtitle = "Percentage of annual abundance across all sites in Lithuania",
       x = "Year",
       y = "Percentage of total annual abundance",
       caption = "Data aggregated by dipteran family and year.") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
p5

# tiff("Plots/Change in dipteran family relative abundance by year - different colours.tiff", width = 10, height = 10, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p5
# dev.off()

p6 <- ggplot(family_abundance_percentage, aes(x = year, y = percentage, color = family)) +
  geom_line(aes(linewidth = ifelse(family %in% c("Ceratopogonidae", "Culicidae", "Simuliidae"), 2, 0.5)), alpha = 0.7) + # Conditional line thickness
  scale_size_identity() + # Ensures that specified sizes are used as given
  theme_minimal(base_size = 14) +
  labs(title = "Trends in dipteran family relative abundance between 2010 and 2022",
       subtitle = "Percentage of annual abundance across all sites in Lithuania",
       x = "Year",
       y = "Percentage of total annual abundance",
       caption = "Data aggregated by dipteran family and year.") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
p6

# tiff("Plots/Change in dipteran family relative abundance by year - different colours and lines.tiff", width = 10, height = 10, units = "in", pointsize = 12, bg = "white", res = 300, compression = "lzw")
# p6
# dev.off()

# CLEAN UP 
library(pacman)
# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear packages
p_unload(all)  # Remove all contributed packages
# Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear mind :)