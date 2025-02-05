#==================== RScript for analysis of macroinvertebrate biomonitoring data from Lithuania
#==================== Start ===================
#==================== Load packages ===================

# Load packages
library(arm)
library(car)
library(ggplot2)
library(GGally)
library(lattice)
library(lawstat)
library(outliers)
library(tidyverse)
library(rlang)
library(gridExtra)
library(glmmTMB)
library(DHARMa)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(janitor)
library(gstat)
library(sf)
library(sp)
library(effects)
library(AICcmodavg)
library(performance)
library(ggpubr)
library(ggeffects)
library(ggmap)
library(httr)
library(googleway)
library(rnaturalearth)
library(rnaturalearthdata)

source("Additional functions/HighstatLibV11.R")

#==================== Import data ===================

# Import the data
df <- as_tibble(read.csv("Data/Bio_Env_data_03.12.2024.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)) |>
  clean_names() |>
  filter(year >= 2013 & year <= 2022) |> # Remove years less than 2013 or greater than 2022 (when looking at lakes and rivers combined)
  # filter(waterbody_type == "lake") |> # keep data from rivers only
  # filter(waterbody_type == "lake") |> # keep data from lakes only
  filter(!is.na(eqr)) |>  # Remove rows where EQR is NA
  mutate(dipt_abund_pa    = ifelse(dipt_abund == 0, 0, 1), # create vector with presence-absence data
         dipt_abund_pos   = ifelse(dipt_abund > 0, dipt_abund, NA), # create vector with only positive data (i.e., not zero)
         vec_abund_pa     = ifelse(vec_abund == 0, 0, 1), # create vector with presence-absence data
         vec_abund_pos    = ifelse(vec_abund > 0, vec_abund, NA), # create vector with only positive data (i.e., not zero)
         fyear            = factor(year, levels = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)), # make year a factor
         cyear            = year - median(year), # centered year - helps model convergence to center variables for the model
         iyear            = year - min(year) + 1, # year as an index starting from 1
         year             = as.numeric(year), # year as numeric
         month            = factor(month, levels = c(4, 5, 9, 10, 11),
                                          labels = c("April", "May", "September", "October", "November"),
                                          ordered = F), # make month a factor
         site_id          = factor(site_id), # make site_id a factor
         state            = factor(state, levels = c("A", "HM", "N"),
                                          labels = c("A", "HM", "N"),
                                          ordered = F), # make state a factor
         eqc              = factor(eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High"),
                                        labels = c("Bad", "Poor", "Moderate", "Good", "High"),
                                        ordered = F), # make EQC a factor
         waterbody_name   = factor(waterbody_name), # make waterbody_name a factor
         waterbody_type   = factor(waterbody_type, levels = c("lake", "river"),
                                                   labels = c("Lake", "River"),
                                                   ordered = T), # make waterbody_type a factor
         date             = as.Date(date, format = "%Y-%m-%d"), # make the dates dates
         doy              = yday(date)) |> # calculate sampling day of year (doy)
  arrange(site_id, year) # order the data.frame
  # arrange(desc(waterbody_type), site_id, year) # order the data.frame


# Convert tibble to dataframe because some older code does not recocgnise tibble
df <- as.data.frame(df)

glimpse(df)

# We have 2089 observations, each a unique site-year sampling survey of
# macroinvertebrates in Lithuanian freshwaters (lakes and riverS).
# 'Ecological quality' is continuous (ratio) variable of ecological quality (0 to 1)

# Any missing values
colSums(is.na(df[, c("vec_abund", "eqr", "ppt", "q", "tmax", "tmin", "doy", "year")]))
# no missing values

#==================== Coding issues ===================

# Data coding issues
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

xy <- LongLatToUTM(x = df$longitude, y = df$latitude, zone = 34)
df$Xutm <- xy[,2]
df$Yutm <- xy[,3]

df$Xkm <- df$Xutm / 1000 # covert metres to KM
df$Ykm <- df$Yutm / 1000 # covert metres to KM


#==================== Get elevation data ===================
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

lithuania <- ne_countries(scale = "large", country = "Lithuania", returnclass = "sf")
plot(lithuania$geometry)

# create sf object
xy_sf <- st_as_sf(xy, coords = c("x", "y"), crs = 4326)

# Set finer resolution for interpolation
resolution <- 0.01  # Adjust this value for finer grids (smaller = finer)

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


#==================== House keeping ===================

# How many observations do we have per year?
table(df$fyear)
# slightly less observations in 2017, 2018, 2019

# How many observations do we have per location x year?
obvs <- table(df$site_id, df$fyear)
print(obvs <- cbind(obvs, total = rowSums(obvs)))

# how many sites do we have in total?
NROW(unique(df$site_id))

#==================== Spatial distribution of sites ===================

# Spatial position of the sites
xyplot(latitude ~ longitude,
       aspect = "fill",
       data = df)

range(df$longitude, df$latitude)
MyCex <- 2 * sqrt(df$vec_abund + 1) / 10
glgmap <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57),
                  maptype = "terrain")
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude,
                        latitude),
                    pch = 19,
                    size = MyCex,
                    col = "red",
                    data = df)
p <- p + xlab("Longitude") + ylab("Latitude")
p <- p + theme(text = element_text(size=15))
p

# And by year
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude,
                        latitude),
                    pch = 19,
                    size = MyCex,
                    col = "red",
                    data = df)
p <- p + xlab("Longitude") + ylab("Latitude")
p <- p + theme(text = element_text(size=15))
p <- p + facet_wrap( ~ fyear)
p # Some 2018 misery?

# ggsave("Plots/Vector_abundance.png", plot = p, width = 8, height = 8, units = "in", dpi = 900, device = "png", bg = NA)


#==================== Game plan ===================

# The 7 steps to fitting a GLM are:

# 1. State the question
# 2. Perform a data exploration
# 3. Select a statistical model
# 4. Fit the model
# 5. Conduct model checks
# 6. Interpret and present model output
# 7. Visualise the results

#==================== State the question ===================

# The aim of this study is to determine whether vector abundance
# is positively correlated with lower ecological quality / water quality:

#==================== Perform exploration ===================

# Outliers
df |>
  select(vec_abund, eqr, ppt, q, tmax, tmin, doy, year) |>
  Mydotplot()

# Check individual plots
# Start by defining a preferred figure format, called 'My_theme'
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, linewidth = 1),
                  strip.background = element_rect(fill = "white",
                                                  color = "white", linewidth = 1),
                  text = element_text(size = 14),
                  panel.grid.major = element_line(colour = "white", linewidth = 0.1),
                  panel.grid.minor = element_line(colour = "white", linewidth = 0.1),
                  legend.position = "none")

# Then plot
df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = eqr)) +
  labs(y = "Vector abundance",
       x = "Ecological quality") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$eqr), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = ppt)) +
  labs(y = "Vector abundance",
       x = "Precipitation") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$ppt), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = q)) +
  labs(y = "Vector abundance",
       x = "Discharge") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$q), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = tmax)) +
  labs(y = "Vector abundance",
       x = "Maximum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$tmax), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = tmin)) +
  labs(y = "Vector abundance",
       x = "Minimum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$tmin), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = ws)) +
  labs(y = "Vector abundance",
       x = "Wind speed") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$ws), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = year)) +
  labs(y = "Vector abundance",
       x = "Time") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(y = vec_abund, x = doy)) +
  labs(y = "Vector abundance",
       x = "Day of year") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$doy), 0)) +
  ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

# NORMALITY AND HOMOGENEITY OF DEPENDENT VARIABLE
# Frequency polygon plot
df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(vec_abund)) +
  geom_freqpoly(bins = 15) +
  labs(x = "Vector abundance",
       y = "Frequency") +
  My_theme
# High number of zeros and count positively skewed
# ZEROS IN THE RESPONSE VARIABLE

df |>
  filter(vec_abund <= 300) |>
  summarise(percentage_zero = sum(vec_abund == 0) / n() * 100)
# 64% zeros - too many?
# Need to fit a model then simulate from it to be sure


# COLLINEARITY

df |>
  filter(vec_abund <= 300) |>
  ggpairs(columns = c("eqr", "ppt", "q", "tmax", "tmin", "ws", "doy", "year"),
          aes(alpha = 0.8), lower = list(continuous = "smooth_loess",
          combo = wrap("facethist", binwidth = 5))) + My_theme

df |>
  filter(vec_abund <= 300) |>
  select(eqr, ppt, q, tmax, tmin, ws, year, doy) |>
  corvif()

# Perhaps tmax and q can cause some trouble

df |>
  filter(vec_abund <= 300) |>
  select(eqr, ppt, tmin, ws, year, doy) |>
  corvif()

# RELATIONSHIPS

# Plot figure
grid.arrange(
df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = eqr, y = vec_abund_pa), alpha = 0.5) +
  geom_smooth(aes(x = eqr, y = vec_abund_pa), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = eqr, y = vec_abund_pa), method = 'gam', se = T, col = "blue")+
  labs(x = "Ecological quality", y = "Vector presence/absence") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = eqr, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = eqr, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = eqr, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Ecological quality", y = "Vector abundance") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = ppt, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = ppt, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = ppt, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Precipitation", y = "Vector abundance") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = tmin, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = tmin, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = tmin, y = vec_abund), method = 'gam', se = T, col = "blue") +
  labs(x = "Minimum temperature", y = "Vector abundance") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = ws, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = ws, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = ws, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Wind speed", y = "Vector abundance") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = year, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = year, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = year, y = vec_abund), method = 'gam', se = T, col = "blue")+
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  labs(x = "Time", y = "Vector abundance") +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot(aes(x = fyear, y = vec_abund, fill = fyear)) +
  geom_point(alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Time", y = "Vector abundance") +
  scale_x_discrete(breaks = 2013:2022, limits = as.character(2013:2022)) +
  My_theme,

df |>
  filter(vec_abund <= 300) |>
  ggplot() +
  geom_point(aes(x = doy, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = doy, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = doy, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Day of Year", y = "Vector abundance") +
  My_theme,

nrow = 4)

# Potentially a weakly positive effect of ecological quality on vector abundance, but also a lot of zeros


# What is the source of these zeros?
# Real zeros or should some be positive counts?

#==================== Standardise data ===================

df <- df |>
  mutate(
    eqr.std    = MyStd(eqr),
    ppt.std    = MyStd(ppt),
    q.std      = MyStd(q),
    tmin.std   = MyStd(tmin),
    tmax.std   = MyStd(tmax),
    ws.std     = MyStd(ws),
    year.std   = MyStd(year),
    doy.std    = MyStd(doy)
)

#==================== Create formula ===================

# Define the dependent variable and independent variables
dependent_var <- "vec_abund"
independent_vars <- c("eqr.std", "ppt.std", "tmin", "doy.std", "waterbody_type")
random_effect <- "(1 | site_id)"

# Construct the formula as a string
formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "), "+", random_effect)
formula_str_nospace <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))

# Convert the string to a formula object
formula_str <- as.formula(formula_str)
formula_str_nospace <- as.formula(formula_str_nospace)

#==================== Select & fit statistical model ===================
#========== Poisson GLM ==========

# Vector abundance is a count, so a Poisson distribution is an appropriate starting point

# The model will be fitted using the glmmTMB package
Pois1 <- glmmTMB(formula = formula_str,
  family = poisson(link = "log"),
  data = df)

Pois1.1 <- glmmTMB(formula = formula_str_nospace,
  family = poisson(link = "log"),
  data = df)

summary(Pois1)
summary(Pois1.1)

AIC(Pois1, Pois1.1)

# Test overdispersion
check_overdispersion(Pois1)
check_overdispersion(Pois1.1)
# Overdispersion detected

#===== Check Poisson model ==========

# Plot residuals against fitted values
# Simulate standardised residuals using model parameters
Res1 <- simulateResiduals(fittedModel = Pois1, plot = F)

# Plot standardised residuals
plotResiduals(Res1)
# Misery

# Plot model residuals against each covariate in the model:
plotResiduals(Res1, form = df$eqr)
plotResiduals(Res1, form = df$ppt)
plotResiduals(Res1, form = df$tmin)
plotResiduals(Res1, form = df$doy)
plotResiduals(Res1, form = df$waterbody_type)
# Misery

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res1, testOutliers = TRUE, testDispersion = TRUE)
# Misery

# Examine zero inflation
testZeroInflation(Pois1)
# Here is the problem - way too many zeros for a Poisson distribution to handle

# test spatial autocorrelation
groupLocations = aggregate(df[, c("Xkm", "Ykm")], list(df$site_id), mean)
res_space = recalculateResiduals(Res1, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
# Sample-variogram with distances up to 150 km
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
mydata <- data.frame(Res1$scaledResiduals, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
Vario <- variogram(Res1$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)

# test temporal autocorrelation
res_time = recalculateResiduals(Res1, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))


#========== Negative-binomial (NB) GLM ==========
NB1 <- glmmTMB(formula = formula_str,
               family = nbinom1(link = "log"),
               data = df)

NB1.1 <- glmmTMB(formula = formula_str_nospace,
               family = nbinom1(link = "log"),
               data = df)

summary(NB1)
summary(NB1.1)

AIC(NB1, NB1.1)

# Test overdispersion
check_overdispersion(NB1)
check_overdispersion(NB1.1)
# Overdispersion detected.

#===== Check NB model ==========
# Plot residuals against fitted values
Res2 <- simulateResiduals(fittedModel = NB1, plot = F)

# Plot standardised residuals
plotResiduals(Res2)
# Misery

# Plot model residuals against each covariate in the model:
plotResiduals(Res2, form = df$eqr)
plotResiduals(Res2, form = df$ppt)
plotResiduals(Res2, form = df$tmin)
plotResiduals(Res2, form = df$doy)
plotResiduals(Res2, form = df$waterbody_type)
# Generally fine

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res2, testOutliers = TRUE, testDispersion = TRUE)
# Slight misery

# Zero inflation
testZeroInflation(Res2)
# Negative binomial does a good job of handling zeros, but it is just a 'black box' approach...

# test spatial autocorrelation
res_space = recalculateResiduals(Res2, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
# Sample-variogram with distances up to 150 km
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
mydata <- data.frame(Res2$scaledResiduals, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
Vario <- variogram(Res2$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# Fine

# test temporal autocorrelation
res_time = recalculateResiduals(Res2, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# Fine

#========== Zero-adjusted Poisson (ZAP) hurdle model ==========
#========== Bernoulli model ==========

# Create a binomial variable with 'ifelse'
# Zeros remain as zero and all positive values = 1
df$vec_abund_pa

# Define the dependent variable and independent variables
dependent_var <- "vec_abund_pa"
independent_vars <- c("eqr.std", "ppt.std", "tmax.std", "doy.std", "waterbody_type")
random_effect <- "(1 | site_id)"

# Construct the formula as a string
formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "), "+", random_effect)
formula_str_nospace <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))

# Convert the string to a formula object
formula_str <- as.formula(formula_str)
formula_str_nospace <- as.formula(formula_str_nospace)

# Fit a Bernoulli model with logit link function
Bern1 <- glmmTMB(formula = formula_str,
  family = binomial(link = logit),
  data = df)

Bern1.1 <- glmmTMB(formula = formula_str_nospace,
  family = binomial(link = logit),
  data = df)

summary(Bern1)
summary(Bern1.1)

AIC(Bern1, Bern1.1)

# Test overdispersion
check_overdispersion(Bern1)
check_overdispersion(Bern1.1)
# No overdispersion detected.
# Note that a Bernoulli model cannot be overdispersed

#===== Check Bernoulli model ==========
Res4 <- simulateResiduals(fittedModel = Bern1, plot = T)

# Plot residuals
plotResiduals(Res4)
# No problem here

# Plot model residuals against each covariate in the model:
plotResiduals(Res4, form = df$eqr)
plotResiduals(Res4, form = df$ppt)
plotResiduals(Res4, form = df$tmin)
plotResiduals(Res4, form = df$doy)
plotResiduals(Res4, form = df$waterbody_type)
# Fine

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res4, testOutliers = TRUE, testDispersion = TRUE)
# No problem with normality of residuals or outliers

# Use simulated data to test zero-inflation
testZeroInflation(Res4)
# The model predicts the correct number of zeros

# Test spatial autocorrelation
res_space = recalculateResiduals(Res4, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
# Sample-variogram with distances up to 150 km
mydata <- data.frame(Res4$scaledResiduals, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
Vario <- variogram(Res4$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# This is fine

# test temporal autocorrelation
res_time = recalculateResiduals(Res4, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# this is fine


#========== Zero-truncated model ==========

# Create a zero-truncated variable by replacing zeros with NA
df$vec_abund_pos
df_pos <- df |>
  filter(!is.na(vec_abund_pos))

# Define the dependent variable and independent variables
dependent_var <- "vec_abund_pos"
independent_vars <- c("eqr.std", "ppt.std", "tmin.std", "doy.std", "waterbody_type")
random_effect <- "(1 | site_id)"

# Construct the formula as a string
formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "), "+", random_effect)
formula_str_nospace <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))

# Convert the string to a formula object
formula_str <- as.formula(formula_str)
formula_str_nospace <- as.formula(formula_str_nospace)

# Fit a truncated Poisson model
TP1 <- glmmTMB(formula = formula_str,
               family = truncated_poisson(link = "log"),
               data = df_pos)

TP1.1 <- glmmTMB(formula = formula_str_nospace,
                 family = truncated_poisson(link = "log"),
                 data = df_pos)

summary(TP1)
summary(TP1.1)

AIC(TP1, TP1.1)

# Test overdispersion
check_overdispersion(TP1)
check_overdispersion(TP1.1)
# No overdispersion detected.

#===== Check Zero-truncated model ==========
Res5 <- simulateResiduals(fittedModel = TP1, plot = T)

# Plot residuals for Pois9
plotResiduals(Res5)
# Misery

# Plot model residuals against each covariate in the model:
plotResiduals(Res5, form = df_pos$eqr)
plotResiduals(Res5, form = df_pos$ppt)
plotResiduals(Res5, form = df_pos$tmin)
plotResiduals(Res5, form = df_pos$doy)
# Misery

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res5, testOutliers = TRUE, testDispersion = TRUE)
# Problem with uniformity

# Use simulated data to test zero-inflation
testZeroInflation(Res5$fittedModel)
# The model predicts the correct number of zeros

# Test spatial autocorrelation
groupLocations1 = aggregate(df_pos[, c("Xkm", "Ykm")], list(df_pos$site_id), mean)
res_space = recalculateResiduals(Res5, group = df_pos$site_id)
testSpatialAutocorrelation(res_space, groupLocations1$Xkm, groupLocations1$Ykm)
# Sample-variogram with distances up to 150 km
mydata <- data.frame(Res5$scaledResiduals, df_pos$Ykm, df_pos$Xkm)
coordinates(mydata)    <- c("df_pos.Ykm", "df_pos.Xkm")
Vario <- variogram(Res5$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# this is not a horizontal line

# test temporal autocorrelation
res_time = recalculateResiduals(Res5, group = df_pos$year)
testTemporalAutocorrelation(res_time, time = unique(df_pos$year))
# Fine

#========== ZAP model with truncated generalised poisson distributon ==========
# Need an alternative distribution - try a generalised Poisson
# (an alternative is a Conway-Maxwell Poisson)
GP1 <- glmmTMB(formula = formula_str,
  family = truncated_genpois(link = "log"),
  data = df_pos)

GP1.1 <- glmmTMB(formula = formula_str_nospace,
  family = truncated_genpois(link = "log"),
  data = df_pos)

summary(GP1)
summary(GP1.1)

AIC(GP1, GP1.1)

# Test overdispersion
check_overdispersion(GP1)
check_overdispersion(GP1.1)
# Overdispersion detected.

#===== Check ZAP model w/ trancated gen poisson ==========
Res6 <- simulateResiduals(fittedModel = GP1, plot = T)

# Plot residuals for Pois1
plotResiduals(Res6)
# Fine

# Plot model residuals against each covariate in the model:
plotResiduals(Res6, form = df_pos$eqr)
plotResiduals(Res6, form = df_pos$ppt)
plotResiduals(Res6, form = df_pos$tmin)
plotResiduals(Res6, form = df_pos$ws)
plotResiduals(Res6, form = df_pos$year)
plotResiduals(Res6, form = df_pos$doy)
# not so good

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res6, testOutliers = TRUE, testDispersion = TRUE)
# Fine

# Use simulated data to test zero-inflation
testZeroInflation(Res6)
# The model predicts the correct number of zeros

# Test spatial autocorrelation
res_space = recalculateResiduals(Res6, group = df_pos$site_id)
testSpatialAutocorrelation(res_space, groupLocations1$Xkm, groupLocations1$Ykm)
# Sample-variogram with distances up to 150 km
mydata <- data.frame(Res6$scaledResiduals, df_pos$Ykm, df_pos$Xkm)
coordinates(mydata)    <- c("df_pos.Ykm", "df_pos.Xkm")
Vario <- variogram(Res6$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# This is not a horizontal line

# test temporal autocorrelation
res_time = recalculateResiduals(Res6, group = df_pos$year)
testTemporalAutocorrelation(res_time, time = unique(df_pos$year))
# Fine

#==========  ZAP model w/ Zero-truncated Negative binomial distribution (quadratic parameterization) ==========
# in nbinom2 overdispersion decreases with increasing phi (the Poisson limit is reached as phi goes to infinity).
TNB2 <- glmmTMB(formula = formula_str,
  family = truncated_nbinom2(link = "log"),
  data = df_pos)

TNB2.1 <- glmmTMB(formula = formula_str_nospace,
  family = truncated_nbinom2(link = "log"),
  data = df_pos)

summary(TNB2)
summary(TNB2.1)

AIC(TNB2, TNB2.1)

check_overdispersion(TNB2)
check_overdispersion(TNB2.1)
# No overdispersion detected.

# Compare models
round(AIC(TP1, GP1, TNB2), 0)
round(AIC(TP1.1, GP1.1, TNB2.1), 0)

#===== ZAP model w/ Zero-truncated Negative binomial ==========
Res7 <- simulateResiduals(fittedModel = TNB2, plot = T)

# Plot residuals for Pois9
plotResiduals(Res7)
# Fine

# Plot model residuals against each covariate in the model:
plotResiduals(Res7, form = df_pos$eqr)
plotResiduals(Res7, form = df_pos$ppt)
plotResiduals(Res7, form = df_pos$tmin)
plotResiduals(Res7, form = df_pos$doy)
plotResiduals(Res7, form = df_pos$waterbody_type)
# some misery

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res7, testOutliers = TRUE, testDispersion = TRUE)
# Fine :)


# Use simulated data to test zero-inflation
testZeroInflation(TNB2)

# Test spatial autocorrelation
res_space = recalculateResiduals(Res7, group = df_pos$site_id)
testSpatialAutocorrelation(res_space, groupLocations1$Xkm, groupLocations1$Ykm)
# Sample-variogram with distances up to 150 km
mydata <- data.frame(Res7$scaledResiduals, df_pos$Ykm, df_pos$Xkm)
coordinates(mydata)    <- c("df_pos.Ykm", "df_pos.Xkm")
Vario <- variogram(Res7$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# This is not a horizontal line

# test temporal autocorrelation
res_time = recalculateResiduals(Res7, group = df_pos$year)
testTemporalAutocorrelation(res_time, time = unique(df_pos$year))
# this is fine

#==================== Create formula ===================

# Define the dependent variable and independent variables
dependent_var <- "vec_abund"
independent_vars <- c("eqr.std", "ppt.std", "tmin.std", "doy.std")
random_effect <- "(1 | site_id)"

# Construct the formula as a string
formula_str <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "), "+", random_effect)
formula_str_nospace <- paste(dependent_var, "~", paste(independent_vars, collapse = " + "))

# Convert the string to a formula object
formula_str <- as.formula(formula_str)
formula_str_nospace <- as.formula(formula_str_nospace)

#========== Tweedie model ==========
TWD1 <- glmmTMB(formula = formula_str,
                      family = tweedie(link = "log"),
                      data = df)

TWD1.1 <- glmmTMB(formula = formula_str_nospace,
                      family = tweedie(link = "log"),
                      data = df)

summary(TWD1)
summary(TWD1.1)

AIC(TWD1, TWD1.1)

check_overdispersion(TWD1)
check_overdispersion(TWD1.1)
# No overdispersion detected.

#===== Compared models ==========

Out <- AIC(Pois1, NB1, TWD1)
rownames(Out) <- c("Poisson",
                   "Neg Bin",
                   "Tweedie")
colnames(Out) <- c("df", "AIC")
round(Out,0)

#===== Check tweedie model ==========
Res8 <- simulateResiduals(fittedModel = TWD1, plot = F)

# Plot residuals
plotResiduals(Res8)
# Misery

# Plot model residuals against each covariate in the model:
plotResiduals(Res8, form = df$eqr)
plotResiduals(Res8, form = df$ppt)
plotResiduals(Res8, form = df$tmin)
plotResiduals(Res8, form = df$ws)
plotResiduals(Res8, form = df$year)
plotResiduals(Res8, form = df$doy)
# Misery

# Check normality of residuals, dispersion and presence of outliers
plotQQunif(Res8, testOutliers = TRUE, testDispersion = TRUE)
# Misery

# Use simulated data to test zero-inflation
testZeroInflation(Res8)
# Fine

# Test spatial autocorrelation
res_space = recalculateResiduals(Res8, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
# Sample-variogram with distances up to 150 km
mydata <- data.frame(Res8$scaledResiduals, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
Vario <- variogram(Res8$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# Fine

# test temporal autocorrelation
res_time = recalculateResiduals(Res8, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# fine

#==================== Visualise results ===================

# Model parameters
summary(TNB2)
summary(Bern1)

tab_model(Bern1,
          TNB2,
          show.intercept = F,
          show.est = T,
          show.se = T,
          show.ci = F,
          show.aic = T,
          show.df = F,
          show.r2 = F,
          show.dev = F,
          string.est = "Estimate",
          string.se = "Std. Error",
          string.p = "P-value",
          string.intercept = "(Intercept)",
          p.style = c("numeric"),
          title = "Zero-truncated hurdle model (ZANB)",
          dv.labels = c("Bernoulli distribution (Vector presence/absence)", "Zero-truncated negative binomal (Vector abundance)"),
          pred.labels = c("Ecological quality ratio (EQR)",
                          "Precipitation (12-month mean)",
                          "Minimum temperature (12-month mean)",
                          "Wind speed (12-month mean)",
                          "Year",
                          "Day of year (DOY)"))

#==================== Clean up ===================

library(pacman)
# Clear data
rm(list = ls())  # Removes all objects from environment
# Free unused memory
gc()
# Clear packages
p_unload(all)  # Remove all contributed packages
# Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear mind :)


#==================== End ===================
