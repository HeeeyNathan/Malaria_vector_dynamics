#==================== Administrative Boundries =================== 
#====== Load packages ======
library(rgeoboundaries)
library(ggplot2)
library(leaflet)

#====== Extract country boundaries ======
Lithuania_boundary <- geoboundaries("Lithuania")
Baltic_boundaries <- geoboundaries(c("Lithuania", "Latvia", "Estonia"))

#====== Extract admin boundaries ======
# Level 1 boundaries
Lithuania_boundary_lvl1 <- geoboundaries("Lithuania", "adm1")

# Level 2 boundaries
Lithuania_boundary_lvl2 <- geoboundaries("Lithuania", "adm2")

#====== Simple plots ======
ggplot(data = Lithuania_boundary) +
  geom_sf() +
  geom_sf_label(aes(label = shapeName)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithuania")

ggplot(data = Baltic_boundaries) +
  geom_sf() +
  geom_sf_label(aes(label = shapeName)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("The Baltic states")

ggplot(data = Lithuania_boundary_lvl1) +
  geom_sf() +
  # geom_sf_label(aes(label = shapeName)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithuanian administrative Level 1 boundaries")

ggplot(data = Lithuania_boundary_lvl2) +
  geom_sf() +
  # geom_sf_label(aes(label = shapeName)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithuanian administrative Level 2 boundaries")

#====== Interactive plots ======
Lithuania_boundary |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = Lithuania_boundary$shapeName)

Baltic_boundaries |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = Baltic_boundaries$shapeName)

Lithuania_boundary_lvl1 |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = Lithuania_boundary_lvl1$shapeName)

Lithuania_boundary_lvl2 |>
  leaflet() |>
  addTiles() |>
  addPolygons(label = Lithuania_boundary_lvl2$shapeName)

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)


#==================== Open street map =================== 
#====== Load packages ======
library(osmdata)
library(ggplot2)
library(ggmap)
library(leaflet)

register_google(key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc")

#====== Available features ======
available_features()
available_tags("amenity")

#====== Extract country boundaries ======
Vilnius_boundary <- getbb("Vilnius")
Vilnius_boundary

#====== Create overpass query ======
Vilnius_boundary |> 
  opq()

#====== Retrieve osmdata object ======
Vilnius_hospitals <- Vilnius_boundary |>
  opq() |>
  add_osm_feature(key = "amenity", value = "hospital") |>
  osmdata_sf()

# print data
Vilnius_hospitals

# bounding box used in query
Vilnius_hospitals$bbox

# metadata
Vilnius_hospitals$meta

# osm_points
Vilnius_hospitals$osm_points

# osm_polyogns
Vilnius_hospitals$osm_polygons

#====== Plot ======
ggplot() +
  geom_sf(data = Vilnius_hospitals$osm_polygons)

Vilnius_map <- get_map(Vilnius_boundary, maptype = "roadmap")
ggmap(Vilnius_map) +
  geom_sf(
    data = Vilnius_hospitals$osm_polygons,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(x = "", y = "")

#====== More complex example ======

# retrieving data of streets
Vilnius_streets <- Vilnius_boundary |>
  opq() |>
  add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary")) |>
  osmdata_sf()

# retrieving data of small streets in Lagos
Vilnius_small_streets <- Vilnius_boundary |>
  opq() |>
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) |>
  osmdata_sf()

# retrieving data of rivers in Lagos
Vilnius_rivers <- Vilnius_boundary |>
  opq() |>
  add_osm_feature(key = "waterway", value = "river") |>
  osmdata_sf()

# retrieving data of hospitals in Lagos
Vilnius_hospitals <- Vilnius_boundary |>
  opq() |>
  add_osm_feature("amenity", "hospital") |>
  osmdata_sf()

ggplot() +
  geom_sf(data = Vilnius_streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = .4, alpha = .8) +
  geom_sf(data = Vilnius_small_streets$osm_lines, inherit.aes = FALSE, color = "#a6a6a6", size = .2, alpha = .8) +
  geom_sf(data = Vilnius_rivers$osm_lines, inherit.aes = FALSE, color = "#7fc0ff", size = .8, alpha = .5) +
  geom_sf(data = Vilnius_hospitals$osm_polygons, inherit.aes = FALSE, colour = "#08519c", fill = "#08306b", alpha = .5, size = 1) +
  coord_sf(xlim = c(25.20, 25.35), ylim = c(54.65, 54.80), expand = FALSE) +
  theme_bw() +
  labs(
    title = "Hospitals in Vilnius (Lithuania)",
    x = "Latitude",
    y = "Longitude"
  )

leaflet() |>
  addTiles() |>
  addPolygons(
    data = Vilnius_hospitals$osm_polygons,
    label = Vilnius_hospitals$osm_polygons$name
  )

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)

#==================== Temperature =================== 
#====== Load packages ======
library(raster)
library(geodata)
library(tidyverse)
library(magrittr)
library(rgeoboundaries)
library(sf)
library(terra)

#====== Load data ======
tmax_data <- worldclim_global(var = "tmax", res = 10, path = "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/WorldClimData")

#====== Convert raster into df ======
tmax_data_df <- as.data.frame(tmax_data, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_df) <- c()

#====== Plot specific month ======
tmax_data_df_may <- as.data.frame(tmax_data$wc2.1_10m_tmax_05, xy = TRUE, na.rm = TRUE)

ggplot(
  data = tmax_data_df_may,
  aes(x = x, y = y)
) +
  geom_tile(aes(fill = wc2.1_10m_tmax_05)) +
  labs(
    title = "Maximum temperature in May",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )

# Rename columns, tranform to long-form, make month a factor
tmax_data_df_long <- tmax_data_df |>
  rename(
    "January" = "wc2.1_2.5m_tmax_01", "February" = "wc2.1_2.5m_tmax_02", "March" = "wc2.1_2.5m_tmax_03", "April" = "wc2.1_2.5m_tmax_04",
    "May" = "wc2.1_2.5m_tmax_05", "June" = "wc2.1_2.5m_tmax_06", "July" = "wc2.1_2.5m_tmax_07", "August" = "wc2.1_2.5m_tmax_08",
    "September" = "wc2.1_2.5m_tmax_09", "October" = "wc2.1_2.5m_tmax_10", "November" = "wc2.1_2.5m_tmax_11", "December" = "wc2.1_2.5m_tmax_12"
  ) |>
  pivot_longer(c(-x, -y), names_to = "month", values_to = "temp")

tmax_data_df_long$month <- factor(tmax_data_df_long$month, levels = month.name)

#====== Plot all months ======
tmax_data_df_long |>
  ggplot(aes(x = x, y = y)) +
  geom_tile(aes(fill = temp)) +
  facet_wrap(~month) +
  labs(
    title = "Monthly maximum temperatures",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#006D9B", "#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-40, -20, 0, 20, 40)
  )

#====== Plot monthly means ======
# Calculating mean of the monthly maximum temperatures
tmax_mean <- mean(tmax_data)

# Converting the raster object into a dataframe
tmax_mean_df <- as.data.frame(tmax_mean, xy = TRUE, na.rm = TRUE)

tmax_mean_df |>
  ggplot(aes(x = x, y = y)) +
  geom_tile(aes(fill = mean)) +
  labs(
    title = "Mean monthly maximum temperatures",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )

#====== Plot monthly means for Lithuania ======
tmax_data_LT <- worldclim_country("Lithuania", var = "tmax", res = 10, path = "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/WorldClimData")
# tmax_data <- worldclim_global(var = "tmax", res = 0.5, path = "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/WorldClimData")
tmax_mean_LT <- mean(tmax_data_LT)

# Downloading the boundary of Lithuania
lithuania_sf <- geoboundaries("Lithuania")
lithuania_spat <- terra::vect(lithuania_sf)

# Extracting temperature data of Lithuania
tmax_mean_Lithuania <- raster::mask(tmax_mean_LT, lithuania_spat)

# Converting the raster object into a dataframe
tmax_mean_LT_df <- as.data.frame(tmax_mean_LT, xy = TRUE, na.rm = TRUE)

tmax_mean_LT_df |>
  ggplot(aes(x = x, y = y)) +
  geom_tile(aes(fill = mean)) +
  geom_sf(data = lithuania_sf, inherit.aes = FALSE, fill = NA) +
  labs(
    title = "Mean monthly maximum temperatures in Lithuania",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradient(
    name = "Temperature (°C)",
    low = "#FEED99",
    high = "#AF3301"
  )

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
gc()
# Clear mind :)


#==================== Elevation =================== 
#====== Load packages ======
library(elevatr)
library(kableExtra)
library(rgeoboundaries)
library(sf)
library(raster)
library(ggplot2)
library(viridis)

#====== Extract elevation data ======
ll_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
elev <- elevatr::get_elev_point(pt_df, prj = ll_proj)
elev |>
  kable() |>
  kable_styling(bootstrap_options = c("striped", "hover"))

#====== Extract boundaries ======
Lithuania_boundary <- rgeoboundaries::geoboundaries("Lithuania", adm_lvl = 0)
elevation_data <- elevatr::get_elev_raster(locations = Lithuania_boundary, z = 9, clip = "locations")

#====== Link elevation data ======
elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"
# remove rows of data frame with one or more NA's,using complete.cases
elevation_data <- elevation_data[complete.cases(elevation_data), ]

#====== Plot ======
ggplot() +
  geom_tile(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = Lithuania_boundary, color = "white", fill = NA) +
  coord_sf() +
  # coord_sf(xlim = c(23.5, 24.5), ylim = c(54.5, 55.0)) +
  scale_fill_viridis_c() +
  labs(title = "Elevation in Lithuania", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)
#==================== Rainfall =================== 
#====== Load packages ======
library(nasapower)
library(dplyr)
library(DT)
library(ggplot2)
library(rnaturalearth)
library(raster)
library(viridis)
library(terra)

#====== Load data ======
# Fetching climatology data
flag <- 1
for (i in seq(20.5, 25, 0.5)) {
  for (j in seq(55.5, 57, 0.5)) {
    climate_avg_temp <- get_power(community = "AG",
                                  pars = "PRECTOTCORR",
                                  lonlat = c(i, (j - 2), (i + 2), j),
                                  temporal_api = "CLIMATOLOGY")
    if (flag == 1) {
      climate_avg <- climate_avg_temp
      flag <- 0
    } else{
      climate_avg <- rbind(climate_avg, climate_avg_temp)
    }
  }
}

climate_avg |> datatable(extensions = c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))

#====== Plot ======
# Creating a map of annual rainfall using all data retrieved
# Getting world map
map <- ne_countries(country = 'Lithuania', returnclass = "sf")

# Converting data to raster
r <- rasterFromXYZ(data.frame(climate_avg[, c("LON", "LAT", "OCT")]))

# Converting the raster into a data.frame
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)

# Plot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = OCT)) +
  geom_sf(data = map, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis() +
  labs(
    title = "Rainfall in inches",
    fill = "Annual Rainfall",
    subtitle = "Annual rainfall in Lithuania"
  ) + 
  labs(x = "Longitude", y = "Latitude")

# Creating maps of monthly rainfall
r <- list()
for (k in colnames(climate_avg)[-c(1:3, 16)]) {
  r[[k]] <- rasterFromXYZ(climate_avg[, c("LON", "LAT", k)])
}
r <- stack(r)
plot(r, asp = 1)

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)

#==================== Humidity =================== 
#====== Load packages ======
library(nasapower)
library(dplyr)
library(DT)
library(ggplot2)
library(rnaturalearth)
library(raster)
library(viridis)
library(terra)

#====== Load data ======
# Fetching climatology data
flag <- 1
for (i in seq(20.5, 25, 0.5)) {
  for (j in seq(55.5, 57, 0.5)) {
    climate_avg_RH_temp <- get_power(community = "AG",
                                  pars = "RH2M",
                                  lonlat = c(i, (j - 2), (i + 2), j),
                                  temporal_api = "CLIMATOLOGY")
    if (flag == 1) {
      climate_avg_RH <- climate_avg_RH_temp
      flag <- 0
    } else{
      climate_avg_RH <- rbind(climate_avg_RH, climate_avg_RH_temp)
    }
  }
}

climate_avg_RH |> datatable(extensions = c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))

#====== Plot ======
# Creating a map of annual rainfall using all data retrieved
# Getting world map
map <- ne_countries(country = 'Lithuania', returnclass = "sf")

# Converting data to raster
r <- rasterFromXYZ(data.frame(climate_avg_RH[, c("LON", "LAT", "ANN")]))

# Converting the raster into a data.frame
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)

# Plot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = ANN)) +
  geom_sf(data = map, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis() +
  labs(
    title = "Relative humidity",
    fill = "Humidity",
    subtitle = "Annual relative humidity in Lithuania"
  ) + 
  labs(x = "Longitude", y = "Latitude")

# Getting map for Lithunia
LT <- ne_states(country = "Lithuania", returnclass = "sf")

# Getting administrative boundaries for regions
VIL <- LT[LT$name == "Vilniaus", ]
KAU <- LT[LT$name == "Kauno", ]

# Converting data to raster
r <- rasterFromXYZ(climate_avg_RH[, c("LON", "LAT", "ANN")])

# Subset values for the region and converting the raster into a data.frame
rr <- mask(crop(r, VIL), VIL)
r_df <- as.data.frame(rr, xy = TRUE, na.rm = TRUE)

ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = ANN)) +
  geom_sf(data = LT, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis() +
  theme_minimal() +
  labs(title = "Relative Humidity in Vilnius, Lithuania", fill = "Humidity")

# Subset values for the region and converting the raster into a data.frame
rr <- mask(crop(r, KAU), KAU)
r_df <- as.data.frame(rr, xy = TRUE, na.rm = TRUE)

ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = ANN)) +
  geom_sf(data = LT, inherit.aes = FALSE, fill = NA) +
  scale_fill_viridis() +
  theme_minimal() +
  labs(title = "Relative Humidity in Kaunas, Lithunia", fill = "Humidity")

# Creating maps of monthly humidity
r <- list()
for (k in colnames(climate_avg_RH)[-c(1:3, 16)]) {
  r[[k]] <- rasterFromXYZ(climate_avg_RH[, c("LON", "LAT", k)])
}
r <- stack(r)
plot(r, asp = 1)

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)

#==================== Malaria =================== 
#====== Load packages ======
library(malariaAtlas)
library(ggplot2)
library(ggthemes)

#====== Check data availability ======
listPRPointVersions()
listVecOccPointVersions()
listShpVersions()

listPRPointCountries(version = "202406")
listVecOccPointCountries(version = "201201")
listSpecies(version = "201201")
listShp(version = "202403")
listRaster()

isAvailable_pr(country = "South Africa")
isAvailable_pr(ISO = "ZAF")
isAvailable_pr(continent = "Africa")
isAvailable_pr(country = "South Africa", version = "202406")

isAvailable_vec(country = "South Africa")
isAvailable_vec(country = c("Lithuania", "Latvia", "Estonia"))

#====== Download and plot data ======
ZAF_pr_data <- getPR(country = "South Africa", species = "both")
Africa_pvpr_data <- getPR(continent = "Africa", species = "Pv")
Extent_pfpr_data <- getPR(extent = rbind(c(-2.460181, 13.581921), c(-3.867188, 34.277344)), species = "Pf")
autoplot(ZAF_pr_data)
autoplot(Africa_pvpr_data)
autoplot(Extent_pfpr_data)

LT_vec_data <- getVecOcc(country = "Lithuania")
autoplot(LT_vec_data)
autoplot(LT_vec_data,
         facet = TRUE)

LT_shp <- getShp(ISO = "LTU", admin_level = "admin0")
autoplot(LT_shp)

LT_An_atroparvus <- getRaster(dataset_id = "Explorer__2010_Anopheles_atroparvus", shp = LT_shp)
LT_vec_An_atroparvus <- getVecOcc(country = "Lithuania", species = "Anopheles atroparvus")

p <- autoplot(LT_An_atroparvus, shp_df = LT_shp, printed = FALSE)
p[[1]] + geom_point(data = LT_vec_An_atroparvus, aes(longitude, latitude, colour = species))+
         scale_colour_manual(values = "yellow", name = "Vector survey locations")+
         scale_fill_distiller(name = "Predicted distribution of An. atroparvus", palette = "PuBuGn", direction = 1)+
         ggtitle("Vector Survey points\n + The predicted distribution of An. atroparvus")

LT_An_messeae <- getRaster(dataset_id = "Explorer__2010_Anopheles_messeae", shp = LT_shp)
LT_vec_An_messeae <- getVecOcc(country = "Lithuania", species = "Anopheles messeae")

p <- autoplot(LT_An_messeae, shp_df = LT_shp, printed = FALSE)
p[[1]] + geom_point(data = LT_vec_An_messeae, aes(longitude, latitude, colour = species))+
         scale_colour_manual(values = "yellow", name = "Vector survey locations")+
         scale_fill_distiller(name = "Predicted distribution of An. messeae", palette = "PuBuGn", direction = 1)+
         ggtitle("Vector Survey points\n + The predicted distribution of An. messeae")

pr <- getPR(country = "ALL", species = "BOTH")
autoplot(pr,
  facet = FALSE)

pr1 <- getVecOcc(country = "ALL")
autoplot(pr1,
  facet = FALSE) +
  theme(legend.position = "bottom")

#====== Clean up ======
library(pacman)
rm(list = ls())
p_unload(all)
graphics.off()
cat("\014")
# Clear mind :)