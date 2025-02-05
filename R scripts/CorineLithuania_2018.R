# Extract Corine land cover data from buffers around points
library(sp)  
library(sf)
library(raster)  
library(rasterVis)  
library(reshape2)
library(dplyr)
# library(terra)

# Read data
# Coorindate data
Data <- as_tibble(read.csv("Outputs/Env_Linked_YrlyMeans_02.12.2024.csv", sep = ",", h = T))

# Seleted data
Data_filterd <- Data %>%
  dplyr::select(sample_id, site_code, site_id, date, day, month, year, waterbody_type, latitude, longitude, waterbody_name, state, river_type)

# Remove duplicated rows 
Sites <- Data_filterd %>%
  dplyr::select(sample_id, latitude, longitude) %>%
  distinct()

# Corine landcover data
# install.packages("raster")
# Check if the file exists
Corine_2018 <- raster("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
plot(Corine_2018)

# Legend
LegendCorine_2012 <- as_tibble(read.table("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2012/clc_legend.csv",h=T, sep=";")) %>% 
  select(CLC_CODE, LABEL1, LABEL2)
LegendCorine_2018 <- as_tibble(read.csv("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/Legend/CLC2018_CLC2018_V2018_20_QGIS.txt", h = FALSE, sep=",")) %>%
  rename(CLC_CODE = V1, RED = V2, GREEN = V3, BLUE = V4, ALPHA = V5, LABEL3 = V6) %>%
  mutate(GRID_CODE = row_number()) %>% 
  left_join(LegendCorine_2012, by = "CLC_CODE") %>% 
  select(GRID_CODE, CLC_CODE, RED, GREEN, BLUE, ALPHA, LABEL1, LABEL2, LABEL3)

# Extracting the landuse at a given point
lat <- Sites$latitude
lon <- Sites$longitude
xy <- cbind(lon, lat) 
v <- terra::vect(xy, crs="+proj=longlat")
plot(v)
# using 2018 data
Corine_2018_terra <- terra::rast("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif") #2018 from https://land.copernicus.eu/pan-european/corine-land-cover
vv_2018 <- terra::project(v, crs(Corine_2018_terra))
plot(vv_2018)
terra::extract(Corine_2018_terra, vv_2018)

# Define map projection
coordinates(Sites) <- c("longitude", "latitude")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(Sites) <- crs.geo 
plot(Sites)

# reproject Sites into LEA projection (=same projection as corine)
crs.laea <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # Lambert Azimuthal Equal Area
Sites.laea <- spTransform(Sites, crs.laea)  # spTransform makes the projection
plot(Sites.laea)

#define buffer
# Convert SpatialPointsDataFrame to sf object
points_data_sf <- st_as_sf(Sites.laea)

# Create a buffer around the points
bufferSites <- st_buffer(points_data_sf, dist = 1000)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# # Convert to SpatialPolygonsDataFrame
# buffer_polygons_sp <- as(buffer_polygons, "Spatial")
plot(buffer_polygons)

# are they in the same projection?
st_crs(Corine_2018) == st_crs(buffer_polygons)

# Extract corine data 
extractedLandCover <- extract(Corine_2018, buffer_polygons)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))


# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine_2018, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover1000m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover1000m)
# colnames(Cover1000m) <- paste0(colnames(Cover1000m), "_2018_1000m")
names(Cover1000m)[1] <- "sample_id"

# Join 100m buffer data to main dataset
Data <- Data %>%
  left_join(Cover1000m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# Create a buffer around the points
bufferSites <- st_buffer(points_data_sf, dist = 200)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# Convert to SpatialPolygonsDataFrame
buffer_polygons_sp <- as(buffer_polygons, "Spatial")

# Extract corine data 
extractedLandCover <- extract(Corine_2018, bufferSites)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){
  prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))

# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine_2018, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover200m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover200m)
colnames(Cover200m) <- paste0(colnames(Cover200m), "_2018_200m")
names(Cover200m)[1] <- "sample_id"

# Join 200m buffer data to main dataset
Data <- Data %>%
  left_join(Cover200m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# Create a buffer around the points
bufferSites <- st_buffer(points_data_sf, dist = 300)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# Convert to SpatialPolygonsDataFrame
buffer_polygons_sp <- as(buffer_polygons, "Spatial")

# Extract corine data 
extractedLandCover <- extract(Corine_2018, bufferSites)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){
  prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))

# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine_2018, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover300m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover300m)
colnames(Cover300m) <- paste0(colnames(Cover300m), "_2018_300m")
names(Cover300m)[1] <- "sample_id"

# Join 300m buffer data to main dataset
Data <- Data %>%
  left_join(Cover300m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# export results
write.csv(data.frame(Data), "Outputs/Diptera_indices_wCorine2018_14.1.2025.csv", row.names = F)

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
