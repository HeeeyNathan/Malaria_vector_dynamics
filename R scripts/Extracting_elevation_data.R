#' Load packages
library(googleway)
library(httr)
library(tidyverse)
library(rgeoboundaries)
library(sf)

#' Load the dipteran vector data.
# Import the data
df <- read.csv("Data/Bio_Env_data_14.1.2025.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE) |>
  clean_names() |>
  arrange(desc(waterbody_type), site_id, year) |>  # order the data.frame
  as.data.frame() # Convert tibble to dataframe because some older code does not recognize tibble

# create dataframe with unique sites and coordinates
xy <- df |>
  dplyr::select(site_id, latitude, longitude) |>
  distinct(site_id, .keep_all = TRUE) # Keeps the first occurrence of each site_id

# Split the dataset into batches (e.g., 50 coordinates per batch)
batch_size <- 50
batches <- split(xy, ceiling(seq_along(xy$latitude) / batch_size))

# Function to fetch elevation for a batch
get_elevation_batch <- function(batch) {
  google_elevation(data.frame(lat = batch$latitude, lon = batch$longitude), key = api_key)
}

# Apply the function to each batch
elevation_results <- lapply(batches, get_elevation_batch)

# Combine all results into a single dataframe
# Extract the 'results' and reset row names for each batch
elevation_data <- bind_rows(
  lapply(seq_along(elevation_results), function(i) {
    # Extract 'results' and add corresponding site_id
    batch <- elevation_results[[i]]$results
    batch$site_id <- xy$site_id[(i - 1) * length(batch$elevation) + seq_along(batch$elevation)]
    as.data.frame(batch)
  })
)

# Merge elevation data back with the original dataframe
xy$elevation <- elevation_data$elevation

# rename columns for ease
xy <- xy |>
  rename(
    x = longitude,
    y = latitude,
  )

# Get Boundry for Lithuania
lithuania.shp <- geoboundaries("Lithuania")
lithuania <- st_transform(Lithuania.shp, crs = 4326)

# Plot the map
plot(st_geometry(lithuania),
     col = "white",
     border = "black",
     xlim = c(21, 27),
     ylim = c(53, 57),
     main = "Lithuania Map with Points")
# Overlay points on the map
points(x = df$longitude, y = df$latitude, col = "blue", pch = 1, cex = 1)

# create sf object
xy_sf <- st_as_sf(xy, coords = c("x", "y"), crs = 4326)

# Set finer resolution for interpolation
resolution <- 0.05  # Adjust this value for finer grids (smaller = finer)

# Generate a grid covering Lithuania's bounding box
lith_bbox <- st_bbox(lithuania)
grid <- expand.grid(
  x = seq(lith_bbox["xmin"], lith_bbox["xmax"], by = resolution),
  y = seq(lith_bbox["ymin"], lith_bbox["ymax"], by = resolution)
)

# Interpolate elevation data
interp <- with(xy, akima::interp(
  x = x, y = y, z = elevation,
  xo = unique(grid$x),
  yo = unique(grid$y),
  duplicate = "mean"
))

# Convert interpolation to a data frame for plotting
interp_df <- as.data.frame(expand.grid(x = interp$x, y = interp$y))
interp_df$elevation <- as.vector(interp$z)

# Convert the interpolated data to an sf object
interp_sf <- st_as_sf(interp_df, coords = c("x", "y"), crs = st_crs(lithuania))

# Clip the interpolated raster to Lithuania's boundary
interp_clipped <- st_intersection(st_as_sf(interp_df, coords = c("x", "y"), crs = st_crs(lithuania)), lithuania)

# Extract coordinates and elevation for raster plotting
interp_clipped_df <- interp_clipped |>
  st_as_sf() |>
  st_coordinates() |>
  as.data.frame() |>
  cbind(elevation = interp_clipped$elevation)

ggplot() +
  geom_sf(data = lithuania, fill = "lightgrey", color = "black") + # Lithuania map
  geom_raster(data = interp_clipped_df, aes(x = X, y = Y, fill = elevation)) + # Shaded elevation
  scale_fill_viridis_c(option = "viridis") + # Elevation colour scale
  geom_sf(data = xy_sf, aes(), color = "red", size = 1) + # Overlay points
  labs(
    title = "Elevation Data in Lithuania",
    x = "Longitude",
    y = "Latitude",
    fill = "Elevation"
  ) +
  theme_minimal()

# Join elevation data to main dataset
df <- df |>
  left_join(xy, by = "site_id") |>
  dplyr::select(-x, -y)

# save output
write.csv(df, "Data/Bio_Env_data_15.1.2025.csv", row.names = F)
