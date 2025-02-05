# Extract Corine land cover data from buffers around points
library(sp)  
library(sf)
library(raster)  
library(rasterVis)  
library(reshape2)
library(dplyr)

# Read data
# Coorindate data
Data <- as_tibble(read.csv("LT_river_lake_geo_data.csv", sep = ",", header = TRUE)) %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>% 
  mutate(waterbody_type = as.factor(waterbody_type)) %>% 
  mutate(latitude = as.double(latitude)) %>% 
  mutate(longitude = as.double(longitude)) %>% 
  arrange(desc(waterbody_type), site_id, year) %>%
  filter(year <= 2022) # only keep data up to 2022 (No environmental data available for 2023 yet in the TerraClimate)
# Remove duplicated rows 
Sites <- Data %>%
  select(sample_id, site_code, site_id, date, day, month, year, waterbody_type, latitude, longitude) %>%
  distinct()

# Corine landcover data
Corine_2018 <- raster("C:/Users/natha/OneDrive/University of Johannesburg/Data/Lithuanian data/Landcover data/LandUse/Corine2018/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif") #2018 from https://land.copernicus.eu/pan-european/corine-land-cover
plot(Corine_2018)

# Legend
LegendCorine_2012 <- as_tibble(read.table("clc_legend.csv",h=T, sep=";")) %>% 
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
# using 2012 data
Corine_2012_terra <- terra::rast("Corine2012/g100_clc12_V18_5.tif") #2012 from https://land.copernicus.eu/pan-european/corine-land-cover
vv_2012 <- terra::project(v, crs(Corine_2012_terra))
plot(vv_2012)
terra::extract(Corine_2012_terra, vv_2012)
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

#define buffer of 200m:
# Convert SpatialPointsDataFrame to sf object
points_data_sf <- st_as_sf(Sites.laea)

# Create a buffer of 200 m around the points
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
names(Cover200m)[1] <- "sample_id"

# export results
write.table(data.frame(Cover200m), "LandUse200mBuffer_2018.csv",sep=",", row.names = F)

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
