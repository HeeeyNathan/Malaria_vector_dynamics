# Extract Corine land cover data from buffers around points
library(sp)  
library(sf)
library(raster)  
library(rasterVis)  
library(reshape2)
library(dplyr)

# Read data
Data <- as_tibble(read.csv("Outputs/Diptera_indices.csv", sep = ",", h = T)) 
LegendCorine <- read.table("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2012/clc_legend.csv", h = T, sep = ";") 
Corine <- raster("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2012/g100_clc12_V18_5.tif") #2012 from https://land.copernicus.eu/pan-european/corine-land-cover
plot(Corine)

# Seleted data
Data_filterd <- Data %>%
  dplyr::select(sample_id, site_code, site_id, date, day, month, year, waterbody_type, latitude, longitude, state, river_type)

# Remove duplicated rows 
Sites <- Data_filterd %>%
  dplyr::select(sample_id, latitude, longitude) %>%
  distinct()

# Define map projection
coordinates(Sites) <- c("longitude", "latitude") 
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(Sites) <- crs.geo 
plot(Sites)

# reproject Sites into LEA projection (=same projection as corine)
crs.laea <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  # Lambert Azimuthal Equal Area
Sites.laea <- spTransform(Sites, crs.laea)  # spTransform makes the projection
plot(Sites.laea)

# Define buffer of 100m:
# Convert SpatialPointsDataFrame to sf object
points_data_sf <- st_as_sf(Sites.laea)

# Create a buffer of 100 m around the points
bufferSites <- st_buffer(points_data_sf, dist = 100)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# Convert to SpatialPolygonsDataFrame
buffer_polygons_sp <- as(buffer_polygons, "Spatial")

# Extract corine data 
extractedLandCover <- extract(Corine, bufferSites)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){
  prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))

# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover100m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover100m)
colnames(Cover100m) <- paste0(colnames(Cover100m), "_2012_100m")
names(Cover100m)[1] <- "sample_id"

# Join 100m buffer data to main dataset
Data <- Data %>%
  left_join(Cover100m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# Define buffer of 200m:
# Create a buffer of 200 m around the points
bufferSites <- st_buffer(points_data_sf, dist = 200)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# Convert to SpatialPolygonsDataFrame
buffer_polygons_sp <- as(buffer_polygons, "Spatial")

# Extract corine data 
extractedLandCover <- extract(Corine, bufferSites)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){
  prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))

# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover200m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover200m)
colnames(Cover200m) <- paste0(colnames(Cover200m), "_2012_200m")
names(Cover200m)[1] <- "sample_id"

# Join 200m buffer data to main dataset
Data <- Data %>%
  left_join(Cover200m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# Define buffer of 300m:
# Create a buffer of 300 m around the points
bufferSites <- st_buffer(points_data_sf, dist = 300)
row.names(bufferSites) <- bufferSites$sample_id
# Convert to polygons
buffer_polygons <- st_as_sf(st_geometry(bufferSites))
# Convert to SpatialPolygonsDataFrame
buffer_polygons_sp <- as(buffer_polygons, "Spatial")

# Extract corine data 
extractedLandCover <- extract(Corine, bufferSites)
extractedLandCover.table <- lapply(extractedLandCover, table)
extractedLandCover.table[1]
ProportionLandCover <- lapply(extractedLandCover, function(x){
  prop.table(table(x))})

# convert to data frame
mydf <- data.frame(id = rep(row.names(bufferSites), lapply(ProportionLandCover, length)),
                   cover = names(unlist(ProportionLandCover)),
                   percent = unlist(ProportionLandCover))

# merge with corine legend
Land_use_Sites <- merge(x = mydf, y = LegendCorine, by.x= "cover", by.y = "GRID_CODE")

# cross tab using LABEL1, more detailed land cover can be obtained using LABEL2 or LABEL3 
Cover300m <- dcast(Land_use_Sites, id ~ LABEL1, fun.aggregate = sum, value.var="percent")
data.frame(Cover300m)
colnames(Cover300m) <- paste0(colnames(Cover300m), "_2012_300m")
names(Cover300m)[1] <- "sample_id"

# Join 300m buffer data to main dataset
Data <- Data %>%
  left_join(Cover300m, by = "sample_id") %>% 
  arrange(desc(waterbody_type), site_id, year)

# export results
write.csv(data.frame(Data), "Outputs/Diptera_indices_wCorine2012.csv", row.names = F)

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

