####################### Load packages and library files #######################
library(tidyverse)
library(janitor) # cleans names of columns
library(lattice)
library(sf) # for geographic data
library(sp) # for geographic data
library(raster)
library(dismo)
library(splancs)
library(INLA)
library(ggmap)
library(gstat)
library(ggplot2)
library(grid)
library(gridExtra)
library(rgeos)
library(fields)
library(GGally)
library(car)
library('maps')
library('maptools')
library('mapdata')
data("worldHiresMapEnv")
source("Additional functions/HighstatLibV11.R")


####################### Load additonal settings and styles #######################
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, linewidth = 1),
                  strip.background = element_rect(fill = "white",
                                                  color = "white", linewidth = 1),
                  text = element_text(size = 12),
                  panel.grid.major = element_line(colour = "white", linewidth = 0.1),
                  panel.grid.minor = element_line(colour = "white", linewidth = 0.1))


####################### Import data #######################
# Calculated index data from Lithuanian rivers and lakes (2010 - 2023)
SA <- as_tibble(read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)) |>
  clean_names() |>
  # filter(waterbody_type == "river") |> # only keeps data from rivers
  # filter(waterbody_type == "lake") |> # only keeps data from lakes
  filter(year >= 2013 & year <= 2022) |> # Remove years less than 2013 or greater than 2022 (when looking at lakes and rivers combined)
  filter(!is.na(eqr)) |>  # Remove rows where EQR is NA
  mutate(vec_abund_pa    = ifelse(vec_abund == 0, 0, 1), # create vector with presence-absence data
         vec_abund_pos   = ifelse(vec_abund > 0, vec_abund, NA), # create vector with only positive data (i.e., not zero)
         vec_abund_log   = log10(vec_abund + 1), #log10 of vector abundance
         fyear           = factor(year, levels = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)), # make year a factor
         fmonth          = factor(month), # make month a factor
         fsite_id        = factor(site_id), # make site_id a factor
         date            = as.Date(date, format = "%d/%m/%Y"), # make the dates dates
         feqc            = factor(eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High"),
                                 labels = c("Bad", "Poor", "Moderate", "Good", "High"),
                                 ordered = T), # make EQC a factor and order the factor levels
         waterbody_name  = factor(waterbody_name), # make waterbody_name a factor
         fwaterbody_type = factor(waterbody_type, levels = c("lake", "river"), labels = c("Lake", "River")), # make waterbody_type a factor
         doy             = yday(date), # calculate sampling day of year (doy)
         agri_logit      = car::logit(agricultural_areas_200m, percents = F, adjust = 0.025),
         arti_logit      = car::logit(artificial_surfaces_200m, percents = F, adjust = 0.025),
         natu_logit      = car::logit(forest_and_semi_natural_areas_200m, percents = F, adjust = 0.025),
         water_logit     = car::logit(water_bodies_200m, percents = F, adjust = 0.025),
         wetland_logit   = car::logit(wetlands_200m, percents = F, adjust = 0.025)) |>
  rename(agriculture     = agricultural_areas_200m, # rename for brevity
         artificial      = artificial_surfaces_200m, # rename for brevity
         natural         = forest_and_semi_natural_areas_200m, # rename for brevity
         water           = water_bodies_200m, # rename for brevity
         wetland         = wetlands_200m) |> # rename for brevity
  dplyr::select(-river_type) |> # remove unneeded variables
  dplyr::select(-c(agricultural_areas_100m, artificial_surfaces_100m, forest_and_semi_natural_areas_100m, water_bodies_100m, wetlands_100m,
                   agricultural_areas_300m,  artificial_surfaces_300m, forest_and_semi_natural_areas_300m, water_bodies_300m, wetlands_300m)) |> # Remove 100m and 300m land use buffers
  arrange(desc(waterbody_type), site_id, year) # order the data.frame

# Convert tibble to dataframe because some older code does not recocgnise tibble
SA <- as.data.frame(SA)

# Insepct data dimensions
glimpse(SA)
names(SA)
str(SA)
head(SA)
dim(SA)


####################### Data coding issues #######################
# For INLA we need UTM coordinates
# See: https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

xy <- LongLatToUTM(x = SA$longitude, y = SA$latitude, zone = 34)
SA$Xutm <- xy[,2]
SA$Yutm <- xy[,3]

SA$Xkm <- SA$Xutm / 1000 # covert metres to KM
SA$Ykm <- SA$Yutm / 1000 # covert metres to KM


####################### Housekeeping #######################
# Missing values?
colSums(is.na(SA))
100 * colSums(is.na(SA)) / nrow(SA)

# How many observations do we have per year?
table(SA$fyear)
# slightly less observations in 2017, 2018, 2019

# How many observations do we have per location x year?
obvs <- table(SA$site_id, SA$fyear)
print(obvs <- cbind(obvs, total = rowSums(obvs)))

# how many sites do we have in total?
NROW(unique(SA$site_id))


####################### Data exploration #######################
# Plot all sites
# Spatial position of the sites
plot(x = SA$longitude,
     y = SA$latitude,
     pch = 3)
xyplot(latitude ~ longitude,
       aspect = "fill",
       data = SA)

range(SA$longitude, SA$latitude)
MyCex <- 2 * sqrt(SA$vec_abund + 1) / 10
glgmap <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57),
                  maptype = "terrain")
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude,
                        latitude),
                    pch = 19,
                    size = MyCex,
                    col = "red",
                    data = SA)
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
                    data = SA)
p <- p + xlab("Longitude") + ylab("Latitude")
p <- p + theme(text = element_text(size=15))
p <- p + facet_wrap( ~ fyear)
p # Some 2018 misery?

# Outliers
MyX <- c("year", "eqr")
Mydotplot(SA[, MyX])

# Collinearity
#  Use VIF values
#  Use Pearson correlations
#  Use scatterplots

# Variance inflation factors
MyX <- c("year",
         "eqr")
corvif(SA[, MyX])

MyX <- c("fyear",
         "eqr")
Mypairs(SA[, MyX])

SA |>
    ggpairs(columns = MyX,
          aes(alpha = 0.8), lower = list(continuous = "smooth_loess",
              combo = wrap("facethist", binwidth = 5))) + My_theme

# Is there correlation between fYear and covariates?
# Make a boxplot
boxplot(eqr ~ fyear,
        data = SA)

boxplot(eqr ~ fwaterbody_type,
        data = SA)
# This is ok...


# Are Month and year related?
plot(x = SA$year,
     y = SA$month,
     xlab = "Sampling year",
     ylab = "Sampling month")

# Add a little bit of random noise
# to the points.
plot(x = jitter(SA$year),
     y = jitter(SA$month),
     xlab = "Sampling year",
     ylab = "Sampling month")

# Yes...let's forget about month. We have data
# from only 5 months, April - May for Lakes, September - November for rivers,
# so there is probably no seasonality in the counts. And if
# there is, then the other covariatres (e.g. waterbody_type) will capture it.

# If you play a little bit more with scatterplots,
# then we eventually end up with the following plan:

#  Model vec_abund as a function of:
#      eqr - water quality
#      year - temporal variability
#      and potentially spatial correlation with X and Y

# Visualizing relationships
MyX <- c("longitude", "latitude",
         # "fyear", "fwaterbody_type",
         "eqr")
MyMultipanel.ggp2(Z = SA,
                  varx = MyX,
                  vary = "vec_abund",
                  ylab = "Vector abundance",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)
# That doesn't make me happy.


# plot covariates against response variable
par(mfrow = c(2,4), mar= c(5,5,2,2), cex.lab = 1.5)
boxplot(vec_abund_log ~ fyear,
        xlab = "Year",
        ylab = "Vector abundance",
        data = SA)

boxplot(vec_abund_log ~ fwaterbody_type,
        xlab = "Year",
        ylab = "Vector abundance",
        data = SA)

plot(x = SA$eqr,
     y = SA$vec_abund,
     xlab = "EQR",
     ylab = "Vector abundance")

# Zero inflation?
par(mfrow = c(1,1))
plot(table(SA$vec_abund), type = "h", main = "Vector abundance")
round(100 * table(SA$vec_abund)/nrow(SA), digits = 2)

nrow(SA)
sum(SA$vec_abund == 0)  # 1232/1979 zeros
round(100 * sum(SA$vec_abund == 0) / nrow(SA), 0)
# 62% of zeros, that's a lot of zeros!


####################### Start of analysis #######################
############ Poisson GLM ##########
# Fit a Poisson GLM and assess whether there is overdispersion.
# Standardize the covariates to avoid numerical problems.
df$eqr.std         <- MyStd(df$eqr)
df$agriculture.std <- MyStd(df$agri_logit)
df$natural.std     <- MyStd(df$natu_logit)
df$ppt.std         <- MyStd(df$ppt)
df$tmax.std        <- MyStd(df$tmax)
df$ws.std          <- MyStd(df$ws)

Poi <- inla(vec_abund ~ eqr.std + agriculture.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "poisson",
           control.compute = list(dic = TRUE, waic = TRUE),
           data = df)
muPoi <- Poi$summary.fitted.values[,"mean"]
EPoi <- (df$vec_abund - muPoi) / sqrt(muPoi)
summary(Poi)

# Calcuate the dispersion statistic
N <- nrow(SA)
Poi$names.fixed
p <- length(Poi$names.fixed)
Dispersion <- sum(EPoi^2) / (N - p)
Dispersion
# 164.6671... That is overdispersion!

# Why do we have overdispersion?
# A. Outliers Y?           ==> Remove them?
# B. Missing covariates?   ==> Add them (or add a latent variable)
# C. Missing interactions? ==> Add them
# D. Zero inflation?       ==> ZIP / ZAP
# E. Large variance?       ==> NB or Generalized Poisson
# F. Correlation?          ==> GLMM
# G. Non-linear patterns   ==> GAM
# H. Wrong link function   ==> Change it

# Your task is to find the culprit. If you pick the wrong one,
# then you may end up with biased parameters.
# How do you figure out which one to pick?
#  -Know your data
#  -Model validation
#  -Data exploration


# A. Outliers?
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = muPoi,
     y = EPoi,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)
# No clear outliers.


# Plot residuals vs each covariate in the model
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
plot(EPoi ~ eqr,
     xlab = "EQR",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ agriculture,
     xlab = "Agricultural coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ natural,
     xlab = "Natural & forested coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ ppt,
     xlab = "Precipitation",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ tmax,
     xlab = "Maximum temperatures",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ ws,
     xlab = "Wind speed",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

# Fit a smoother on the residuals and see whether it tells us something.
library(mgcv)
T1 <- gam(EPoi ~ s(eqr),
          data = SA)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EPoi ~ s(agriculture),
          data = SA)
summary(T2)
plot(T2)
#That is not really convincing.

T3 <- gam(EPoi ~ s(natural),
          data = SA)
summary(T3)
plot(T3)
#That is interesting.

T4 <- gam(EPoi ~ s(ppt),
          data = SA)
summary(T4)
plot(T4)
#That is not really convincing.

T5 <- gam(EPoi ~ s(tmax),
          data = SA)
summary(T5)
plot(T5)
#That is interesting.

T6 <- gam(EPoi ~ s(ws),
          data = SA)
summary(T6)
plot(T6)
#That is not really convincing.

# Plot residuals vs each covariate not in the model.
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
boxplot(EPoi ~ fyear,
        xlab = "Year",
        ylab = "Pearson residuals",
        data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ fmonth,
     xlab = "Month",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ water,
     xlab = "Water coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ wetland,
     xlab = "Wetland coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ q,
     xlab = "Discharge",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EPoi ~ tmin,
     xlab = "Minimum temperatures",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

# Fit a smoother on the residuals and see whether it tells us something.
T1 <- gam(EPoi ~ s(water),
          data = SA)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EPoi ~ s(q),
          data = SA)
summary(T2)
plot(T2)
#That is interesting.

T3 <- gam(EPoi ~ s(tmin),
          data = SA)
summary(T3)
plot(T3)
#That is interesting.

# also fit some box plots
boxplot(EPoi ~ fyear, data = SA)
boxplot(EPoi ~ fmonth, data = SA)

# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 200 km
mydata <- data.frame(EPoi, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
GLM.Poi <- variogram(EPoi ~ 1, cutoff = 300, mydata,  cressie = TRUE)
plot(GLM.Poi,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Not at all!

# Zero inflation?
# Simulation study
I2 <- inla(vec_abund ~ eqr.std + agriculture.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "poisson",
           data = SA,
           control.compute=list(config = TRUE))

NSim <- 10000
SimData <- inla.posterior.sample(n = NSim, result = I2)
N  <- nrow(SA)

X <- model.matrix(~eqr.std + agriculture.std + natural.std + ppt.std + tmax.std + ws.std,
                  data = SA)

MyParams <- colnames(X)
RowNum.Betas <- lapply(MyParams,
                 function(x)
                   grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas

Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xm <- as.matrix(X)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  FixedPart   <- Xm %*% Betas
  mu.i[,i]    <- exp(FixedPart)
  Ysim[,i]    <- rpois(n = nrow(SA), lambda = mu.i[,i])
}

# Calculate the number of zeros in the 1,000 data sets.
zeros <- vector(length = NSim)
for(i in 1:NSim){
  zeros[i] <- sum(Ysim[,i] == 0)
}

# Plot this data
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(0, 1500),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the Poisson model does not contain enough zeros.


############ Create mesh##########
library('maps')
library('maptools')
library('mapdata')
data("worldHiresMapEnv")

# creating a mesh for spatially correlated data
range(SA$longitude)
range(SA$latitude)

par(mfrow = c(1,1))
LithuaniaPoly <- map("world",
                 regions = c("lithuania"),
                 fill = TRUE,
                 col = "white",
                 boundary = TRUE,
                 interior = TRUE,
                 plot = TRUE,
                 xlim = c(21, 27),
                 ylim = c(53, 57))
points(x = SA$longitude, y = SA$latitude, col = "blue", pch = 1, cex = 1)

IDs <- sapply(strsplit(LithuaniaPoly$names, ":"), function(x) x[1])
LithuaniaSP <- map2SpatialPolygons(map = LithuaniaPoly,
                                   IDs = IDs,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))

# In latitude/longitude
plot(LithuaniaSP,
     ylim = c(53,57),
     xlim = c(21, 27))
points(x = SA$longitude, y = SA$latitude)

# locator() # this is used to select the points below :)

# create LT buffer
lithuania <- st_read("Additional functions/gadm41_LTU_shp/gadm41_LTU_0.shp")
lithuania_utm <- st_transform(lithuania, crs = 32634)  # UTM Zone 34N (covers Lithuania)
lithuania_buffered <- st_buffer(lithuania_utm, dist = (5*1000))  # 5,000 meters = 5 km buffer
lithuania_buffered_wgs84 <- st_transform(lithuania_buffered, crs = 4326)
lithuania_coords <- st_coordinates(lithuania_buffered_wgs84)
LocatX <- lithuania_coords[, 1]  # First column corresponds to longitude
LocatY <- lithuania_coords[, 2]   # Second column corresponds to latitude

Buffer <- matrix(cbind(LocatX, LocatY), ncol = 2)

# Convert this area into a spatial polygon
BufferPoly <- Polygon(Buffer, hole = FALSE)
BufferSP   <- SpatialPolygons(list(Polygons(list(BufferPoly), ID = '0')))
plot(BufferSP)

# Give the newly created buffer polygon a projection
BufferSP@proj4string <- LithuaniaSP@proj4string

# Plot coastline and selected area
par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(LithuaniaSP,
     main = "Lithuania",
     axes = TRUE,
     ylim = c(53,57),
     xlim = c(21, 27))
points(SA$longitude,
       SA$latitude,
       cex = 0.5,
       col = 2)
text(x = 20 ,y = 56.5,"A", cex = 1.5)

plot(LithuaniaSP,
     main = "Selected area",
     axes = TRUE,
     ylim = c(53,57),
     xlim = c(21, 27))
points(SA$longitude,
       SA$latitude,
       cex = 0.5,
       col = 2)
plot(BufferSP, add = TRUE, lwd = 3)
text(x = 20 ,y = 56.5, "B", cex = 1.5)

plot(BufferSP,
     main = "Lithuania with 5km buffer")
points(SA$longitude,
       SA$latitude,
       cex = 0.5,
       col = 2)
text(x = 20 ,y = 56.5,"C", cex = 1.5)

# Make the coastline less detailed
my.tol <- 0.1
my.area <- 0.1
LithuaniaSP.smooth <- thinnedSpatialPoly(LithuaniaSP,
                                         tolerance = my.tol,
                                         minarea = my.area,
                                         topologyPreserve = TRUE,
                                         avoidGEOS = FALSE)
BufferSP.smooth <- thinnedSpatialPoly(BufferSP,
                                      tolerance = my.tol,
                                      minarea = my.area,
                                      topologyPreserve = TRUE,
                                      avoidGEOS = FALSE)

Lithuania.UTM = spTransform(LithuaniaSP, CRS("+proj=utm +zone=34 +north ellps=WGS84 +datum=WGS84"))
Buffer.UTM = spTransform(BufferSP, CRS("+proj=utm +zone=34 +north ellps=WGS84 +datum=WGS84"))

plot(Lithuania.UTM,
     axes = TRUE,
     main = "Smoothed Lithuania",
     xlab = "X",
     ylab = "Y")
points(SA$Xutm,
       SA$Yutm,
       cex = 0.5,
       col = 2,
       pch = 1)
text(x = 20 ,y = 56.5, "D", cex = 1.5)

# Making the mesh
# Check spatial correlation
Loc <- as.matrix(SA[,c("Xutm", "Yutm")])
D <- dist(Loc)
par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D / 1000,
     freq = TRUE,
     main = "",
     xlab = "Distance between sites (km)",
     ylab = "Frequency")

plot(x = sort(D) / 1000,
     y = (1:length(D))/length(D),
     type = "l",
     xlab = "Distance between sites (km)",
     ylab = "Cumulative proportion")
# Most site pairs have distances between them clustered between 100 and 200 km,
# which is the core of your distance distribution.

# The cumulative plot shows that almost all sites are within 300 km of each other,
# with the vast majority within 200 km.
RangeGuess <- 75 * 1000
# What is the distance for which we would expect dependency? 75km?
# The smaller this value the better...but the longer the computing time

# When determining a range guess for MaxEdge in a spatial model, the idea is to
# align this with the scale over which spatial correlation might occur due to dispersal.
# Based on the dispersal information of dipteran insects (which generally disperse within
# 2–50 km but could be carried farther by wind), I suggested using 50–100 km as a range guess
# for MaxEdge. This would allow the model to capture the relevant spatial structures created
# by dispersal over this range, while also considering the distances between the sites.
# Recommended settings:
MaxEdge    <- RangeGuess / 5

# The convex option puts the boundary of the innerpart closer to the points.
# Saves some computing time but maybe not use it for a paper.
ConvHull   <- inla.nonconvex.hull(Loc, convex = 50 * 1000)
mesh1      <- inla.mesh.2d(loc = Loc,
                           boundary = ConvHull,
                           max.edge = c(1, 5) * MaxEdge,
                           cutoff  = MaxEdge / 5)

par(mfrow = c(1, 1))
plot(mesh1)
points(Loc, pch = 16)
mesh1$n

# Here is a mesh made with the Lithuania buffer zone
# Boundary <- inla.sp2segment(Lithuania.UTM)
Boundary <- inla.sp2segment(Buffer.UTM)
mesh2  <- inla.mesh.2d(loc = Loc,
                       boundary = Boundary,
                       max.edge = c(1, 5) * MaxEdge,
                       cutoff  = MaxEdge / 5)
plot(mesh2)
points(Loc, pch = 16)
mesh2$n

c(mesh1$n, mesh2$n)

# create spatial polygon for land (based on real lithuania coordinates)
x1 <- c(min(mesh1$loc[,1]), max(mesh1$loc[,1]),
        max(mesh1$loc[,1]), min(mesh1$loc[,1]))
y1 <- c(min(mesh1$loc[,2]), min(mesh1$loc[,2]),
        max(mesh1$loc[,2]), max(mesh1$loc[,2]))

AreaPoly <- Polygon(cbind(x1, y1), hole = FALSE)
AreaSP   <- SpatialPolygons(list(Polygons(list(AreaPoly),
                                          ID = "1")))
AreaSP@proj4string <- Lithuania.UTM@proj4string
plot(AreaSP)

Land1 <- gDifference(AreaSP, Lithuania.UTM)
plot(Land1)

# create spatial polygon for land (based on lithuania with buffer zone)
x2 <- c(min(mesh2$loc[,1]), max(mesh2$loc[,1]),
        max(mesh2$loc[,1]), min(mesh2$loc[,1]))
y2 <- c(min(mesh2$loc[,2]), min(mesh2$loc[,2]),
        max(mesh2$loc[,2]), max(mesh2$loc[,2]))

AreaPoly2 <- Polygon(cbind(x2, y2), hole = FALSE)
AreaSP2   <- SpatialPolygons(list(Polygons(list(AreaPoly2),
                                          ID = "1")))
AreaSP2@proj4string <- Buffer.UTM@proj4string
plot(AreaSP2)

Land2 <- gDifference(AreaSP2, Buffer.UTM)
plot(Land2)


############ Spatial analysis ##########
# lets start doing some spatial things! Yay!
# Define the weighting factors a_ik (also called the projector matrix).
# The sigma parameter represents the marginal standard deviation of the spatial random field.
# It controls the variability of the spatial process—essentially, how much variation is explained by the spatial random field.
A1 <- inla.spde.make.A(mesh1, loc = Loc)
A2 <- inla.spde.make.A(mesh2, loc = Loc)

# Define the SPDE.
# This is tricky stuff. We need to specify this:
# P(Range < range0) = 0.5  and P(sigma > sigma0) = 0.01
# The range0 value is our primary tool to control the amount
# of smoothing that the spatial random field will do.
# The larger it is, the smoother the spatial random field.
# It allows to avoid overfitting.
spde1 <- inla.spde2.pcmatern(mesh1,
                             prior.range = c(50 * 1000 , 0.5),
                             prior.sigma = c(1.5, 0.01))
                             # prior.range = c(50 * 1000 , 0.01),
                             # prior.sigma = c(1.5, 0.01))  This was the first attempt

spde2 <- inla.spde2.pcmatern(mesh2,
                             prior.range = c(50 * 1000, 0.5),
                             prior.sigma = c(1.5, 0.01))
                             # prior.range = c(50 * 1000, 0.01),
                             # prior.sigma = c(1.5, 0.01)) This was the first attempt

# We used a simple glm to get some feeling about sensible values
# for the PC priors.
range(SA$vec_abund)
# P(Range < 50 km ) = 0.05
# P(sigma > ) = 0.05
# SB = exp(u_i)
# some u_i have to be as large as 13.496 to cover 1360
# If u_i ~ N(0, sigma_u^2) then it is unlikley that sigma_u > 1.5
#P(sigma > 1.5) = 0.05
M1 <- glm(vec_abund ~ 1, data = SA)
summary(M1)

# Define the spatial field.
w1.index <- inla.spde.make.index(name = 'w', n.spde  = spde1$n.spde)
w2.index <- inla.spde.make.index(name = 'w', n.spde  = spde2$n.spde)

# Define the the stack
Xm <- model.matrix(~eqr.std +
                     agriculture.std + natural.std +
                     ppt.std + tmax.std + ws.std, data = SA)
N <- nrow(SA)
X <- data.frame(eqr.std         = Xm[, 2],
                agriculture.std = Xm[, 3],
                natural.std     = Xm[, 4],
                ppt.std         = Xm[, 5],
                tmax.std        = Xm[, 6],
                ws.std          = Xm[, 7])

# Poisson model
# And this is the stack for the Poisson model
Stack.mesh1 <- inla.stack(
  tag  = "Fit",
  data = list(y = SA$vec_abund),
  A    = list(1, 1, A1, 1),
  effects = list(
    Intercept  = rep(1, N),
    X  = as.data.frame(X),
    w = w1.index,
    iidx = 1:nrow(X)))

Stack.mesh2 <- inla.stack(
  tag  = "Fit",
  data = list(y = SA$vec_abund),
  A    = list(1, 1, A2, 1),
  effects = list(
    Intercept  = rep(1, N),
    X    = as.data.frame(X),
    w    = w2.index,
    iidx = 1:nrow(X)))

# Define the formula
fPois.mesh1 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde1)

fPois.mesh2 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde2)

# Executing the model in R-INLA
Pois.mesh1 <- inla(fPois.mesh1,
                    family = "poisson",
                    data = inla.stack.data(Stack.mesh1),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh1)))

Pois.mesh2 <- inla(fPois.mesh2,
                    family = "poisson",
                    data = inla.stack.data(Stack.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh2)))

# compare model diagnostics
dic  <- c(Pois.mesh1$dic$dic, Pois.mesh2$dic$dic)
waic <- c(Pois.mesh1$waic$waic, Pois.mesh2$waic$waic)
DicWaic     <- cbind(dic, waic)
rownames(DicWaic) <- c("Spatial Poisson GLM with mesh 1",
                       "Spatial Poisson GLM with mesh 2")
DicWaic

summary(Pois.mesh1)
summary(Pois.mesh2)

# Plotting the spatial random field

# This function is modified code from material on Haakon Bakka's website
# We will not explain what is inside this function. Just run it.
PlotField <- function(field, mesh, ContourMap, xlim, ylim, Add=FALSE,...){
  stopifnot(length(field) == mesh$n)
  # Plotting region to be the same as the study area polygon
  if (missing(xlim)) xlim <- ContourMap@bbox[1, ]
  if (missing(ylim)) ylim <- ContourMap@bbox[2, ]

  # inla.mesh.projector: it creates a lattice using the mesh and specified ranges.
  proj <- inla.mesh.projector(mesh,
                              xlim = xlim,
                              ylim = ylim,
                              dims = c(300, 300))
  # The function inla.mesh.project can then
  # be used to project the w's on this grid.
  field.proj <- inla.mesh.project(proj, field)

  # And plot the whole thing
  image.plot(list(x = proj$x,
                  y = proj$y,
                  z = field.proj),
             xlim = xlim,
             ylim = ylim,
             asp = 1,
             add = Add,
             ...)
}

summary(Pois.mesh1)

# Plot the spatial random field
w1.pm <- Pois.mesh1$summary.random$w$mean
w1.sd <- Pois.mesh1$summary.random$w$sd

w2.pm <- Pois.mesh2$summary.random$w$mean
w2.sd <- Pois.mesh2$summary.random$w$sd

# Its plotting time!
PlotField(field = w1.pm,
          mesh = mesh1,
          xlim = range(mesh1$loc[,1]+5000),
          ylim = range(mesh1$loc[,2]+5000))

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)

# Get rid of the colours outside Lithuania (i.e., the bad lands)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")

plot(Lithuania.UTM, add = TRUE)

# And the spatial random field for mesh 2
PlotField(field = w2.pm,
          mesh = mesh2,
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)

#Determine the area outside the study area
plot(Land2,
     col = "white",
     add = TRUE,
     border = "white")

plot(Buffer.UTM, add = TRUE)

# Model validation
# Plot fitted values versus observed data
N.rows <- 1:nrow(SA)
mu.SpatPois <- Pois.mesh1$summary.fitted.values[N.rows, "mean"] # this is the number of rows in SA
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.SpatPois,
     y = SA$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector abundance")

# Simulation study
Pois.mesh1.sim <- inla(fPois.mesh1,
                       family = "poisson",
                       data = inla.stack.data(Stack.mesh1),
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                       control.predictor = list(A = inla.stack.A(Stack.mesh1)))


set.seed(12345)
NSim          <- 10000
SimData       <- inla.posterior.sample(n = NSim, result = Pois.mesh1.sim)
N             <- nrow(SA)
ZerosPoisSpat <- vector(length = NSim)
YPoisSpat     <- matrix(nrow = N, ncol = NSim)

MyID_beta <- function(x) {grep(x, rownames(SimData[[1]]$latent), value = FALSE)} # allows for partial name match

MyParams <- colnames(X)
RowNum.Pos <- lapply(MyParams, MyID_beta)
RowNum.Pos <- as.numeric(RowNum.Pos)
RowNum.Pos

MyID_w <- function(x) {
  which(rownames(SimData[[1]]$latent) == x)
} # allows for exact name match

N1 <- mesh1$n # it was mesh1$n - 1 in the original code
MyParams <- paste("w", 1:N1, sep = ":") # changed the zero to 1
RowNum.w <- lapply(MyParams, MyID_w)
RowNum.w <- as.numeric(RowNum.w) # RowNum.w is a list and first needs to be unlisted
RowNum.w

Xm <- as.matrix(X)
Am1 <- as.matrix(A1)
for (i in 1:NSim){
  Beta  <- SimData[[i]]$latent[RowNum.Pos]
  w     <- SimData[[i]]$latent[RowNum.w]
  mu       <- exp(Xm %*% Beta + Am1 %*% w)
  YPoisSpat[,i]    <- rpois(N, lambda = mu)
  ZerosPoisSpat[i] <- sum(YPoisSpat[,i] == 0)
}

# plot
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosPoisSpat),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(0, 1500),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)

sum(sum(SA$vec_abund == 0) > ZerosPoisSpat) / 10000
sum(SA$vec_abund == 0)
# cry... there are too few zeros :'(


############ Poisson GLM with observation level random effects ##########
# Poisson GLM with observation level random effects for meshes 1 and 2.
# After that the Negative binomial GLM is fitted with meshes 1 and 2.

# Poisson + spatial correlation + OLRE
hyper.iid <- list(prec = list(prior = 'pc.prec', param = c(0.5, 0.001)))
fPois.olre.mesh1 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde1) +
  f(iidx, model="iid", hyper = hyper.iid)

fPois.olre.mesh2 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde2) +
  f(iidx, model="iid", hyper = hyper.iid)

Pois.olre.mesh1 <- inla(fPois.olre.mesh1,
                         family = "poisson",
                         data = inla.stack.data(Stack.mesh1),
                         control.compute = list(dic = TRUE, waic = TRUE),
                         control.predictor = list(A = inla.stack.A(Stack.mesh1)))
Pois.olre.mesh2 <- inla(fPois.olre.mesh2,
                     family = "poisson",
                    data = inla.stack.data(Stack.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh2)))
summary(Pois.olre.mesh1)
summary(Pois.olre.mesh2)

# Plot fitted values versus observed data
mu.Pois.olre <- Pois.olre.mesh1$summary.fitted.values[1:1436, "mean"] #the number of rows for fitted.APredictor (number of rows in SA)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.Pois.olre,
     y = SA$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers")

############ NB GLM + spatial correlation ##########
fNB.mesh1 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde1)

fNB.mesh2 <- y ~ -1 + Intercept +
  eqr.std +
  agriculture.std + natural.std +
  ppt.std + tmax.std + ws.std +
  f(w, model = spde2)

# Negative binomial with spatial correlation
NB.mesh1 <- inla(fNB.mesh1,
                  family = "nbinomial",
                  data = inla.stack.data(Stack.mesh1),
                  control.compute = list(dic = TRUE, waic = TRUE),
                  control.predictor = list(A = inla.stack.A(Stack.mesh1)))
NB.mesh2 <- inla(fNB.mesh2,
                    family = "nbinomial",
                    data = inla.stack.data(Stack.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh2)))

# Plot fitted values versus observed data
mu.NB <- NB.mesh1$summary.fitted.values[1:1436, "mean"] #the number of rows for fitted.APredictor (number of rows in SA)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.NB,
     y = SA$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers",
     xlim = c(0, 200),
     ylim = c(0, 200))


summary(NB.mesh1)
summary(NB.mesh2)

# And the spatial random field for mesh 1
w1NB.pm <- NB.mesh1$summary.random$w$mean
w1NB.sd <- NB.mesh1$summary.random$w$sd

PlotField(field = w1NB.pm,
          mesh = mesh1,
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Land1, add = TRUE)


PlotField(field = w1NB.sd,
          mesh = mesh1,
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Land1, add = TRUE)


summary(NB.mesh1)


# Simulation study NB GLM with spatial correlation
NB.mesh1.sim <- inla(fNB.mesh1,
                       family = "nbinomial",
                       data = inla.stack.data(Stack.mesh1),
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                       control.predictor = list(A = inla.stack.A(Stack.mesh1)))

NB.mesh1.sim$summary.hyper

set.seed(12345)
NSim          <- 10000
SimData       <- inla.posterior.sample(n = NSim, result = NB.mesh1.sim)
N             <- nrow(SA)
ZerosNBSpat   <- vector(length = NSim)
YNBSpat       <- matrix(nrow = N, ncol = NSim)

k <- SimData[[1]]$hyperpar[1]

MyParams <- colnames(X)
RowNum.Pos <- lapply(MyParams, MyID_beta)
RowNum.Pos <- as.numeric(RowNum.Pos)
RowNum.Pos

N1 <- mesh1$n # mesh1$n - 1
MyParams <- paste("w", 1:N1, sep = ":")
RowNum.w <- lapply(MyParams, MyID_w)
RowNum.w <- as.numeric(RowNum.w)
RowNum.w

Xm <- as.matrix(X)
Am1 <- as.matrix(A1)
k   <- SimData[[1]]$hyperpar[1]

library(MASS)
for (i in 1:NSim){
  Beta  <- SimData[[i]]$latent[RowNum.Pos]
  w     <- SimData[[i]]$latent[RowNum.w]
  mu       <- exp(Xm %*% Beta + Am1 %*% w)
  YNBSpat[,i]    <- rnegbin(N, mu = mu, theta = k)
  ZerosNBSpat[i] <- sum(YNBSpat[,i] == 0)
}

# plot
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosNBSpat),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(1000, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)

sum(sum(SA$vec_abund == 0) > ZerosNBSpat) / 10000
sum(SA$vec_abund == 0)
# cry... now there are too many zeros :'(


############ ZAP model with spatial correlation ##########
# ZAP model with spatial correlation
# Running the model in R-INLA
SA$vec_abund_pa
SA$vec_abund_pos

w1.pos.index <- inla.spde.make.index(name = 'wpos', n.spde  = spde1$n.spde)
w2.pos.index <- inla.spde.make.index(name = 'wpos', n.spde  = spde2$n.spde)

w1.01.index <- inla.spde.make.index(name = 'w01', n.spde  = spde1$n.spde)
w2.01.index <- inla.spde.make.index(name = 'w01', n.spde  = spde2$n.spde)

Xm <- model.matrix(~eqr.std +
                     agriculture.std + natural.std +
                     ppt.std + tmax.std + ws.std,
                   data = SA)

N <- nrow(SA)
X <- data.frame(Intercept.pos     = rep(1, N),
                eqr.pos           = Xm[, 2],
                agriculture.pos   = Xm[, 3],
                natural.pos       = Xm[, 4],
                ppt.pos           = Xm[, 5],
                tmax.pos          = Xm[, 6],
                ws.pos            = Xm[, 7])


X01 <- data.frame(Intercept.01         = rep(1, N),
                  eqr.01               = Xm[, 2],
                  agriculture.01       = Xm[, 3],
                  natural.01           = Xm[, 4],
                  ppt.01               = Xm[, 5],
                  tmax.01              = Xm[, 6],
                  ws.01                = Xm[, 7])

# And this is the stack for the ZAP model
StackPos.mesh1 <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(SA$vec_abund_pos, NA)),
  A    = list(1, A1),
  effects = list(
    Xpos = as.data.frame(X),
    wpos = w1.pos.index))

StackPos.mesh2 <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(SA$vec_abund_pos, NA)),
  A    = list(1, A2),
  effects = list(
         Xpos = as.data.frame(X),
         wpos = w2.pos.index))

Stack01.mesh1 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, SA$vec_abund_pa)),
  A    = list(1, A1),
  effects = list(
    X01 = as.data.frame(X01),
    w01 = w1.01.index))


Stack01.mesh2 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, SA$vec_abund_pa)),
  A    = list(1, A2),
  effects = list(
    X01 = as.data.frame(X01),
    w01 = w2.01.index))

Stack.ZA.mesh1 <- inla.stack(StackPos.mesh1, Stack01.mesh1)
Stack.ZA.mesh2 <- inla.stack(StackPos.mesh2, Stack01.mesh2)


#	Specify the model formula
fZA.mesh1  <- AllY ~ -1 + Intercept.pos + eqr.pos + agriculture.pos + natural.pos + ppt.pos + tmax.pos + ws.pos +
                       f(wpos, model = spde1) +
                       Intercept.01 + eqr.01 + agriculture.01 + natural.01 + ppt.01 + tmax.01 + ws.01 +
                       f(w01, model = spde1)

fZA.mesh2  <- AllY ~ -1 + Intercept.pos + eqr.pos + agriculture.pos + natural.pos + ppt.pos + tmax.pos + ws.pos +
                      f(wpos, model = spde2) +
                      Intercept.01 + eqr.01 + agriculture.01 + natural.01 + ppt.01 + tmax.01 + ws.01 +
                      f(w01, model = spde2)


# Run model
HyperZap <- list(theta = list(initial = -10, fixed = TRUE))
ZAP.mesh1 <- inla(fZA.mesh1,
                   family = c("zeroinflatedpoisson0", "binomial"),
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh1),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh1)))

ZAP.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatedpoisson0", "binomial"),
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


# Fitted values and Pearson residuals
# Fitted values of the ZAP, mesh 1
RowsPos <- inla.stack.index(Stack.mesh1, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.mesh1, tag='Fit01')$data

mu.ZTruncPois <- ZAP.mesh1$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZAP.mesh1$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZAP.mesh1$summary.fixed[1:7,"mean"]
Beta01  <- ZAP.mesh1$summary.fixed[8:14,"mean"]

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)

A1.m   <- as.matrix(A1)
# A2.m   <- as.matrix(A2)
wPos   <- ZAP.mesh1$summary.random$wpos$mean
w01    <- ZAP.mesh1$summary.random$w01$mean

mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
mu.ZTruncPois.self <- mu /  (1 - exp(-mu))
Pi.self <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

muZAP  <- Pi.self * mu.ZTruncPois.self
varZAP <- (Pi.self / (1 - exp(-mu))) * (mu + mu^2) - (  Pi.self * mu / (1 - exp(-mu))  )^2
EZAP   <- (SA$vec_abund - muZAP) / sqrt(varZAP)

# Calcuate the dispersion statistic
N <- nrow(SA)
p <- length(ZAP.mesh1$names.fixed)
Dispersion <- sum(EZAP^2) / (N - p)
Dispersion
# ~1.456767 That is still a little overdispersion!

# Simulation study ZAP model with spatial correlation
ZAP.mesh1.sim <- inla(fZA.mesh1,
                      family = c("zeroinflatedpoisson0", "binomial"),
                      control.family = list(list(hyper = HyperZap),
                                            list()),
                      data = inla.stack.data(Stack.ZA.mesh1),
                      control.compute = list(config = TRUE),
                      control.predictor = list(
                        link = 1,
                        A = inla.stack.A(Stack.ZA.mesh1)))

set.seed(12345)
NSim           <- 10000
SimData        <- inla.posterior.sample(n = NSim, result = ZAP.mesh1.sim)
N              <- nrow(SA)
ZerosZAPSpat   <- vector(length = NSim)
YZAPSpat       <- matrix(nrow = N, ncol = NSim)

MyParams.Pos <- colnames(Xpos)
RowNum.Pos   <- lapply(MyParams.Pos, MyID_beta)
RowNum.Pos   <- as.numeric(RowNum.Pos)
RowNum.Pos

MyParams.01 <- colnames(X01)
RowNum.01   <- lapply(MyParams.01, MyID_beta)
RowNum.01   <- as.numeric(RowNum.01)
RowNum.01

N1 <- mesh1$n # mesh1$n - 1
MyParams.wPos <- paste("wpos", 1:N1, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

N1 <- mesh1$n # mesh1$n - 1
MyParams.w01 <- paste("w01", 1:N1, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
A1.m   <- as.matrix(A1)

# load necessary package
library(VGAM)

# Initialize variables to track replacements
replacement_info <- data.frame(Iteration = integer(), Replacements = integer(), SkippedDueToNA = logical())

# Maximum number of skips allowed per iteration
max_skips <- 5

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = NSim, style = 3)

for (i in 1:NSim) {
  # Initialize replacement count, skip count, and control flags for this iteration
  replacements <- 0
  skips <- 0
  skipped_due_to_na <- FALSE
  skip_iteration <- FALSE

  repeat {
    BetaPos <- SimData[[i]]$latent[RowNum.Pos]
    Beta01 <- SimData[[i]]$latent[RowNum.01]
    wPos <- SimData[[i]]$latent[RowNum.wPos]
    w01 <- SimData[[i]]$latent[RowNum.w01]

    mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
    Pi <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

    # Check for NA values in mu or Pi
    if (any(is.na(mu)) || any(is.na(Pi))) {
      message(paste("Iteration", i, ": Found NA values in mu or Pi. Retrying."))
      skips <- skips + 1

      if (skips == max_skips) {
        message(paste("Iteration", i, ": Skipping due to persistent NA values after", max_skips, "retries."))
        skipped_due_to_na <- TRUE
        skip_iteration <- TRUE
        break
      }

      next # Retry the iteration
    }

    # Check for too-small values in mu and replace
    if (any(mu < 1e-8, na.rm = TRUE)) {
      while (any(mu < 1e-8, na.rm = TRUE) && skips < max_skips) {
        message(paste("Iteration", i, ": Found small mu values. Replacing with minimum threshold."))
        mu[mu < 1e-8] <- 1e-8  # Replace too-small values with a threshold
        skips <- skips + 1
        replacements <- replacements + 1 # Track replacements
      }

      if (skips == max_skips) {
        message(paste("Iteration", i, ": Skipping after", max_skips, "adjustments due to small mu values."))
        skipped_due_to_na <- TRUE
        skip_iteration <- TRUE
        break
      }
    }

    # Catch errors/warnings in VGAM::rzapois using tryCatch
    tryCatch({
      YZAPSpat[, i] <- VGAM::rzapois(N, lambda = mu, pobs0 = 1 - Pi)
      ZerosZAPSpat[i] <- sum(YZAPSpat[, i] == 0)
    }, warning = function(w) {
      message(paste("Iteration", i, ": Warning encountered in VGAM::rzapois:", conditionMessage(w)))
      skips <- skips + 1
      if (skips == max_skips) {
        message(paste("Iteration", i, ": Skipping due to repeated warnings after", max_skips, "retries."))
        skipped_due_to_na <- TRUE
        skip_iteration <- TRUE
      }
      next
    }, error = function(e) {
      message(paste("Iteration", i, ": Error encountered in VGAM::rzapois:", conditionMessage(e)))
      skipped_due_to_na <- TRUE
      skip_iteration <- TRUE
    })

    # Break the repeat loop if an error was encountered
    if (skip_iteration) break

    # If no issues, proceed with the rest of the iteration
    break # Exit the repeat loop and continue to the next iteration
  }

  # Store information about replacements or skips for this iteration
  if (replacements > 0 || skipped_due_to_na) {
    replacement_info <- rbind(replacement_info, data.frame(Iteration = i, Replacements = replacements, SkippedDueToNA = skipped_due_to_na))
  }

  # Update the progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar after completion
close(pb)

# Calculate percentage of iterations where replacements or skips occurred
total_replacements <- nrow(replacement_info)
(percentage_replacements <- (total_replacements / NSim) * 100)

# plot the output
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZAPSpat),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(0, 1000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)

sum(sum(SA$vec_abund == 0) > ZerosZAPSpat) / 10000
sum(SA$vec_abund == 0)

# Model validation with DHARMa
library(DHARMa)

# Simulated response values from the fitted model
simulated_response <- YZAPSpat
if (anyNA(YZAPSpat)) {
  cols_with_na <- apply(YZAPSpat, 2, function(x) any(is.na(x)))
  simulated_response <- YZAPSpat[, !cols_with_na]
  message(sum(cols_with_na), " problematic columns with NAs were removed.")
} else {
  simulated_response <- YZAPSpat
  message("No columns with NAs were found. The matrix is unchanged.")
}

# Get the observed response (actual counts)
observed_response <- SA$vec_abund
anyNA(observed_response)

# Use DHARMa to create scaled quantile residuals
residuals_sim <- createDHARMa(
  simulatedResponse = simulated_response,     # Matrix of simulated responses
  observedResponse = observed_response,       # Observed response (abundance)
  integerResponse = TRUE                      # TRUE for count data
)

# Residual diagnostics
plot(residuals_sim)

# Histogram of residuals
hist(residuals_sim$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals", breaks = 20, col = "lightblue", border = "white")

# Residuals vs. Predictors
predictors <- data.frame(EQR = SA$eqr,
                         Agriculture = SA$agriculture,
                         Natural = SA$natural,
                         Precipitation = SA$ppt,
                         Maximum_temperature = SA$tmax,
                         Wind_speed = SA$ws)

# Plot residuals vs. each predictor
par(mfrow = c(2, 4), mar = c(5,5,2,2), cex.lab = 1.5)
for (i in 1:ncol(predictors)) {
  plot(predictors[, i], residuals_sim$scaledResiduals,
       xlab = colnames(predictors)[i],
       ylab = "Scaled Residuals",
       main = paste("Residuals vs", colnames(predictors)[i]),
       pch = 19, col = "blue")
  abline(h = 0, col = "red")  # Add a horizontal line at y = 0
}

# Results of the ZAP model with spatial correlation
# Plot the spatial random field for the ZAP with mesh 1
wpm.ZAP.Pos <- ZAP.mesh1$summary.random$wpos$mean
wpm.ZAP.01  <- ZAP.mesh1$summary.random$w01$mean
wsd.ZAP.Pos <- ZAP.mesh1$summary.random$wpos$sd
wsd.ZAP.01  <- ZAP.mesh1$summary.random$w01$sd

# Plot the spatial random field again, and add white space for the non-study area
par(mar = c(5,5,2,2), cex.lab = 1.5)
PlotField(field = wpm.ZAP.Pos, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZAP.Pos, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

# Binary part of the ZANB
PlotField(field = wpm.ZAP.01, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZAP.01, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)


############ ZANB model with spatial correlation ##########
# ZANB model with spatial correlation
ZANB.mesh1 <- inla(fZA.mesh1,
                  family = c("zeroinflatednbinomial0", "binomial"),
                  control.family = list(list(hyper = HyperZap),
                                        list()),
                  data = inla.stack.data(Stack.ZA.mesh1),
                  control.compute = list(dic = TRUE, waic = TRUE),
                  control.predictor = list(
                    link = 1,
                    A = inla.stack.A(Stack.ZA.mesh1)))

ZANB.mesh2 <- inla(fZA.mesh2,
                  family = c("zeroinflatednbinomial0", "binomial"),
                  control.family = list(list(hyper = HyperZap),
                                        list()),
                  data = inla.stack.data(Stack.ZA.mesh2),
                  control.compute = list(dic = TRUE, waic = TRUE),
                  control.predictor = list(
                    link = 1,
                    A = inla.stack.A(Stack.ZA.mesh2)))

# Getting fitted values and Pearson residuals
# Fitted values of the ZANB, mesh 1
RowsPos <- inla.stack.index(Stack.ZA.mesh1, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.ZA.mesh1, tag='Fit01')$data

mu.ZTruncPois <- ZANB.mesh1$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZANB.mesh1$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZANB.mesh1$summary.fixed[1:7,"mean"]
Beta01  <- ZANB.mesh1$summary.fixed[8:14,"mean"]
k       <- ZANB.mesh1$summary.hyper[1,"mean"]
Xpos <- as.matrix(X)
X01  <- as.matrix(X01)

A1.m <- as.matrix(A1)
A2.m <- as.matrix(A2)
wPos   <- ZANB.mesh1$summary.random$wpos$mean
w01    <- ZANB.mesh1$summary.random$w01$mean

mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
P0 <- (k / (mu + k))^k
Pi.self <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

muZANB  <- (Pi.self / (1 - P0))  * mu
varZANB <- (Pi.self / (1 - P0))  * (mu^2 + mu + mu^2 / k ) - (  Pi.self / (1 - P0) * mu  )^2

EZANB <- (SA$vec_abund - muZANB) / sqrt(varZANB)
p <- length(ZANB.mesh1$names.fixed)
# p <- 2 * (8 + 2 + 2)
sum(EZANB^2) / (nrow(SA) - p)
# 0.915409 That is.... not overdispersion! :')))

# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

summary(ZANB.mesh1)

# Simulation study ZINB model with spatial correlation
ZANB.mesh1.sim <- inla(fZA.mesh1,
                       family = c("zeroinflatednbinomial0", "binomial"),
                       control.family = list(list(hyper = HyperZap),
                                             list()),
                       data = inla.stack.data(Stack.ZA.mesh1),
                       control.compute = list(config = TRUE),
                       control.predictor = list(
                         link = 1,
                         A = inla.stack.A(Stack.ZA.mesh1)))

set.seed(12345)
NSim           <- 10000
SimData        <- inla.posterior.sample(n = NSim, result = ZANB.mesh1.sim)
N              <- nrow(SA)
ZerosZANBSpat  <- vector(length = NSim)
YZANBSpat      <- matrix(nrow = N, ncol = NSim)

MyParams.Pos <- colnames(Xpos)
RowNum.Pos   <- lapply(MyParams.Pos, MyID_beta)
RowNum.Pos   <- as.numeric(RowNum.Pos)
RowNum.Pos

MyParams.01 <- colnames(X01)
RowNum.01   <- lapply(MyParams.01, MyID_beta)
RowNum.01   <- as.numeric(RowNum.01)
RowNum.01

N1 <- mesh1$n # mesh1$n - 1
MyParams.wPos <- paste("wpos", 1:N1, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

N1 <- mesh1$n # mesh1$n - 1
MyParams.w01 <- paste("w01", 1:N1, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
A1.m   <- as.matrix(A1)
k   <- SimData[[1]]$hyperpar[1]

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = NSim, style = 3)

for (i in 1:NSim) {
  BetaPos <- SimData[[i]]$latent[RowNum.Pos]
  Beta01 <- SimData[[i]]$latent[RowNum.01]
  wPos <- SimData[[i]]$latent[RowNum.wPos]
  w01 <- SimData[[i]]$latent[RowNum.w01]
  k   <- SimData[[i]]$hyperpar[1] # strictly speaking, simulated values should be used, but there is a tendency for k parameters to not look very random

  mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
  Pi <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

  # Generate the values using the VGAM function
  YZANBSpat[, i] <- VGAM::rzanegbin(N, munb = mu, size = k, pobs0 = 1 - Pi)
  ZerosZANBSpat[i] <- sum(YNBSpat[, i] == 0)

  # Update the progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar after completion
close(pb)

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZANBSpat),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(0, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)

sum(sum(SA$vec_abund == 0) > ZerosZANBSpat) / 1000
sum(SA$vec_abund == 0)

# Model validation with DHARMa
# Simulated response values from the fitted model
simulated_response <- YZANBSpat
if (anyNA(YZANBSpat)) {
  cols_with_na <- apply(YZANBSpat, 2, function(x) any(is.na(x)))
  simulated_response <- YZANBSpat[, !cols_with_na]
  message(sum(cols_with_na), " problematic columns with NAs were removed.")
} else {
  simulated_response <- YZANBSpat
  message("No columns with NAs were found. The matrix is unchanged.")
}

# Get the observed response (actual counts)
observed_response <- SA$vec_abund
anyNA(observed_response)

# Use DHARMa to create scaled quantile residuals
residuals_sim <- createDHARMa(
  simulatedResponse = simulated_response,     # Matrix of simulated responses
  observedResponse = observed_response,       # Observed response (abundance)
  integerResponse = TRUE                      # TRUE for count data
)

# Residual diagnostics
plot(residuals_sim)

# Histogram of residuals
hist(residuals_sim$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals", breaks = 20, col = "lightblue", border = "white")

# Residuals vs. Predictors
predictors <- data.frame(EQR = SA$eqr,
                         Agriculture = SA$agriculture,
                         Natural = SA$natural,
                         Precipitation = SA$ppt,
                         Maximum_temperature = SA$tmax,
                         Wind_speed = SA$ws)

# Plot residuals vs. each predictor
par(mfrow = c(2, 4), mar = c(5,5,2,2), cex.lab = 1.5)
for (i in 1:ncol(predictors)) {
  plot(predictors[, i], residuals_sim$scaledResiduals,
       xlab = colnames(predictors)[i],
       ylab = "Scaled Residuals",
       main = paste("Residuals vs", colnames(predictors)[i]),
       pch = 19, col = "blue")
  abline(h = 0, col = "red")  # Add a horizontal line at y = 0
}

# Results of the ZANB model with spatial correlation
wpm.ZANB.Pos <- ZANB.mesh1$summary.random$wpos$mean
wpm.ZANB.01  <- ZANB.mesh1$summary.random$w01$mean
wsd.ZANB.Pos <- ZANB.mesh1$summary.random$wpos$sd
wsd.ZANB.01  <- ZANB.mesh1$summary.random$w01$sd

# Plot the spatial random field again, and add white space for the non-study area
par(mfrow = c(1, 1), mar = c(5,5,2,2), cex.lab = 1.5)
PlotField(field = wpm.ZANB.Pos, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.Pos, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

# Binary part of the ZANB
PlotField(field = wpm.ZANB.01, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.01, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land1,
     col = "white",
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

############ Comparing all modelling results ##########
DIC.ZAP.mesh1 = sum(tapply(ZAP.mesh1$dic$local.dic, ZAP.mesh1$dic$family, sum))
DIC.ZAP.mesh2 = sum(tapply(ZAP.mesh2$dic$local.dic, ZAP.mesh2$dic$family, sum))
WAIC.ZAP.mesh1 = sum(tapply(ZAP.mesh1$waic$local.waic, ZAP.mesh1$dic$family, sum))
WAIC.ZAP.mesh2 = sum(tapply(ZAP.mesh2$dic$local.dic, ZAP.mesh2$dic$family, sum))

DIC.ZANB.mesh1 = sum(tapply(ZANB.mesh1$dic$local.dic, ZANB.mesh1$dic$family, sum))
DIC.ZANB.mesh2 = sum(tapply(ZANB.mesh2$dic$local.dic, ZANB.mesh2$dic$family, sum))
WAIC.ZANB.mesh1 = sum(tapply(ZANB.mesh1$waic$local.waic, ZANB.mesh1$dic$family, sum))
WAIC.ZANB.mesh2 = sum(tapply(ZANB.mesh2$waic$local.waic, ZAP.mesh2$dic$family, sum))

dic  <- c(Pois.mesh1$dic$dic, Pois.mesh2$dic$dic,
          Pois.olre.mesh1$dic$dic, Pois.olre.mesh2$dic$dic,
          NB.mesh1$dic$dic, NB.mesh2$dic$dic,
          DIC.ZAP.mesh1, DIC.ZAP.mesh2,
          DIC.ZANB.mesh1, DIC.ZANB.mesh2)

waic <- c(Pois.mesh1$waic$waic, Pois.mesh2$waic$waic,
          Pois.olre.mesh1$waic$waic, Pois.olre.mesh2$waic$waic,
          NB.mesh1$waic$waic, NB.mesh2$waic$waic,
          WAIC.ZAP.mesh1, WAIC.ZAP.mesh2,
          WAIC.ZANB.mesh1, WAIC.ZANB.mesh2)

Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Spatial Poisson GLM mesh 1",
                     "Spatial Poisson GLM mesh 2",
                     "Spatial Poisson GLM mesh 1 + olre",
                     "Spatial Poisson GLM mesh 2 + olre",
                     "Spatial NB GLM mesh 1",
                     "Spatial NB GLM mesh 2",
                     "Spatial ZAP model mesh 1",
                     "Spatial ZAP model mesh 2",
                     "Spatial ZANB model mesh 1",
                     "Spatial ZANB model mesh 2")
Z.out

# looks like the spatial ZAP model with mesh 2 is a clear standout butttt,
# when plotting the mesh, it looks BAAAAAAAD! Lets go with ZANB with mesh2 :)))))

MyData <- data.frame(
   Range  = c(Pois.mesh1$summary.hyper[1,"mean"],
              Pois.mesh2$summary.hyper[1,"mean"],
              Pois.olre.mesh1$summary.hyper[1,"mean"],
              Pois.olre.mesh2$summary.hyper[1,"mean"],
              NB.mesh1$summary.hyper[2,"mean"],
              NB.mesh2$summary.hyper[2,"mean"],
              ZAP.mesh1$summary.hyper[c(1,3),"mean"],
              ZAP.mesh2$summary.hyper[c(1,3),"mean"],
              ZANB.mesh1$summary.hyper[c(3,5),"mean"],
              ZANB.mesh2$summary.hyper[c(3,5),"mean"]) / 1000,

   sigma_u = c(Pois.mesh1$summary.hyper[2,"mean"],
               Pois.mesh2$summary.hyper[2,"mean"],
               Pois.olre.mesh1$summary.hyper[2,"mean"],
               Pois.olre.mesh2$summary.hyper[2,"mean"],
               NB.mesh1$summary.hyper[3,"mean"],
               NB.mesh2$summary.hyper[3,"mean"],
               ZAP.mesh1$summary.hyper[c(2,4),"mean"],
               ZAP.mesh2$summary.hyper[c(2,4),"mean"],
               ZANB.mesh1$summary.hyper[c(4,6),"mean"],
               ZANB.mesh2$summary.hyper[c(4,6),"mean"]))


colnames(MyData) <- c("Range of mesh", "sigma u")
rownames(MyData) <- c("Spatial Poisson GLM mesh 1",
                      "Spatial Poisson GLM mesh 2",
                      "Spatial Poisson GLM mesh 1 + olre",
                      "Spatial Poisson GLM mesh 2 + olre",
                      "Spatial NB GLM mesh 1",
                      "Spatial NB GLM mesh 2",
                      "Spatial ZAP model mesh 1, count part",
                      "Spatial ZAP model mesh 1, binary part",
                      "Spatial ZAP model mesh 2, count part",
                      "Spatial ZAP model mesh 2, binary part",
                      "Spatial ZANB model mesh 1, count part",
                      "Spatial ZANB model mesh 1, binary part",
                      "Spatial ZANB model mesh 2, count part",
                      "Spatial ZANB model mesh 2, binary part")
print(MyData, digits = 5)


############ Checking variograms of all models ##########
# Get Pearson residuals for some of the models.
EPoi.glm <- (SA$vec_abund - muPoi) / sqrt(muPoi)
ESpatPois <- (SA$vec_abund - mu.SpatPois) / sqrt(mu.SpatPois)
ENB   <- (SA$vec_abund - mu.NB) / sqrt(mu.NB + mu.NB^2 / NB.mesh1$summary.hyper[1, "mean"])

# Let's make a variogram of the Pearson residuals.
mydata <- data.frame(EPoi.glm = EPoi.glm,
                     EZAP = EZAP,
                     EZANB = EZANB,
                     ESpatPois = ESpatPois,
                     ENB = ENB,
                     Ykm = SA$Ykm,
                     Xkm = SA$Xkm)
coordinates(mydata)  <- c("Xkm", "Ykm")

GLM.Poi    <- variogram(EPoi.glm ~ 1, mydata,  cutoff = 300, cressie = TRUE)
Vario.pois <- variogram(ESpatPois ~ 1, mydata, cutoff = 300, cressie = TRUE)
Vario.NB   <- variogram(ENB ~ 1, mydata, cutoff = 300, cressie = TRUE)
Vario.ZAP  <- variogram(EZAP ~ 1, mydata, cutoff = 300, cressie = TRUE)
Vario.ZANB <- variogram(EZANB ~ 1, mydata, cutoff = 300, cressie = TRUE)

AllVarios <- data.frame(Gamma = c(#GLM.Poi$gamma,
                                  Vario.pois$gamma, Vario.NB$gamma, Vario.ZAP$gamma, Vario.ZANB$gamma),
                        Dist  = c(#GLM.Poi$dist,
                                  Vario.pois$dist, Vario.NB$dist, Vario.ZAP$dist, Vario.ZANB$dist),
                        ID    = factor(rep(c(
                          #"Poisson GLM",
                          "Spatial Poisson GLM", "Spatial NB GLM", "Spatial ZAP model", "Spatial ZANB model"), each = 15)),
                        levels =c(#"Poisson GLM",
                          "Spatial Poisson GLM", "Spatial NB GLM", "Spatial ZAP model", "Spatial ZANB model"))

# Relationships
p1 <- ggplot()
p1 <- p1 + xlab("Distance") + ylab("Sample-variogram")
p1 <- p1 + theme(text = element_text(size = 15))
p1 <- p1 + geom_point(data = AllVarios,
                      aes(x = Dist, y = Gamma))
p1 <- p1 + geom_smooth(data = AllVarios,
                       aes(x = Dist, y = Gamma))
p1 <- p1 + facet_wrap(~ ID)
p1


############ Model validation of ZANB model ##########
# Getting fitted values and Pearson residuals
# Fitted values of the ZANB, mesh 1
RowsPos <- inla.stack.index(Stack.ZA.mesh1, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.ZA.mesh1, tag='Fit01')$data

mu.ZTruncPois <- ZANB.mesh1$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZANB.mesh1$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZANB.mesh1$summary.fixed[1:8,"mean"]
Beta01  <- ZANB.mesh1$summary.fixed[9:16,"mean"]
k       <- ZANB.mesh1$summary.hyper[1,"mean"]
Xpos <- as.matrix(X)
X01  <- as.matrix(X01)

A1.m <- as.matrix(A1)
A2.m <- as.matrix(A2)
wPos   <- ZANB.mesh1$summary.random$wpos$mean
w01    <- ZANB.mesh1$summary.random$w01$mean

mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
P0 <- (k / (mu + k))^k
Pi.self <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

muZANB  <- (Pi.self / (1 - P0))  * mu
varZANB <- (Pi.self / (1 - P0))  * (mu^2 + mu + mu^2 / k ) - (  Pi.self / (1 - P0) * mu  )^2

EZANB <- (SA$vec_abund - muZANB) / sqrt(varZANB)
p <- length(ZANB.mesh1$names.fixed)
# p <- 2 * (8 + 2 + 2)
sum(EZANB^2) / (nrow(SA) - p)
# 0.9566208 That is.... not overdispersion! :')))

# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

# Simulation study ZANB model with spatial correlation
ZANB.mesh2.sim <- inla(fZA.mesh2,
                       family = c("zeroinflatednbinomial0", "binomial"),
                       control.family = list(list(hyper = HyperZap),
                                             list()),
                       data = inla.stack.data(Stack.ZA.mesh2),
                       control.compute = list(config = TRUE),
                       control.predictor = list(
                         link = 1,
                         A = inla.stack.A(Stack.ZA.mesh2)))

set.seed(12345)
NSim           <- 10000
SimData        <- inla.posterior.sample(n = NSim, result = ZANB.mesh2.sim)
N              <- nrow(SA)
ZerosZANBSpat  <- vector(length = NSim)
YZANBSpat      <- matrix(nrow = N, ncol = NSim)

MyParams.Pos <- colnames(Xpos)
RowNum.Pos   <- lapply(MyParams.Pos, MyID_beta)
RowNum.Pos   <- as.numeric(RowNum.Pos)
RowNum.Pos

MyParams.01 <- colnames(X01)
RowNum.01   <- lapply(MyParams.01, MyID_beta)
RowNum.01   <- as.numeric(RowNum.01)
RowNum.01

N1 <- mesh2$n # mesh1$n - 1
MyParams.wPos <- paste("wpos", 1:N1, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

N1 <- mesh2$n # mesh1$n - 1
MyParams.w01 <- paste("w01", 1:N1, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
A1.m   <- as.matrix(A1)
A2.m   <- as.matrix(A2)
k   <- SimData[[1]]$hyperpar[1]

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = NSim, style = 3)

for (i in 1:NSim) {
  BetaPos <- SimData[[i]]$latent[RowNum.Pos]
  Beta01 <- SimData[[i]]$latent[RowNum.01]
  wPos <- SimData[[i]]$latent[RowNum.wPos]
  w01 <- SimData[[i]]$latent[RowNum.w01]
  k   <- SimData[[i]]$hyperpar[1]

  mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
  Pi <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))

  # Generate the values using the VGAM function
  YZANBSpat[, i] <- VGAM::rzanegbin(N, munb = mu, size = k, pobs0 = 1 - Pi)
  ZerosZANBSpat[i] <- sum(YNBSpat[, i] == 0)

  # Update the progress bar
  setTxtProgressBar(pb, i)
}

close(pb)

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZANBSpat),
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(0, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0),
       y = 0,
       pch = 16,
       cex = 5,
       col = 2)

sum(sum(SA$vec_abund == 0) > ZerosZANBSpat) / 10000
sum(SA$vec_abund == 0)
#The model is still has too many zeros :/

# Model validation with DHARMa
# Simulated response values from the fitted model
simulated_response <- YZANBSpat
if (anyNA(YZANBSpat)) {
  cols_with_na <- apply(YZANBSpat, 2, function(x) any(is.na(x)))
  simulated_response <- YZANBSpat[, !cols_with_na]
  message(sum(cols_with_na), " problematic columns with NAs were removed.")
} else {
  simulated_response <- YZANBSpat
  message("No columns with NAs were found. The matrix is unchanged.")
}

# Get the observed response (actual counts)
observed_response <- SA$vec_abund
anyNA(observed_response)

# Use DHARMa to create scaled quantile residuals
residuals_sim <- createDHARMa(
  simulatedResponse = simulated_response,     # Matrix of simulated responses
  observedResponse = observed_response,       # Observed response (abundance)
  integerResponse = TRUE                      # TRUE for count data
)

# Residual diagnostics
plot(residuals_sim)

# Histogram of residuals
hist(residuals_sim$scaledResiduals, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals", breaks = 20, col = "lightblue", border = "white")

# Residuals vs. Predictors
predictors <- data.frame(EQR = SA$eqr,
                         Agriculture = SA$agriculture,
                         Artificial = SA$artificial,
                         Natural = SA$natural,
                         Precipitation = SA$ppt,
                         Maximum_temperature = SA$tmax,
                         Wind_speed = SA$ws)

# Plot residuals vs. each predictor
par(mfrow = c(2, 4), mar = c(5,5,2,2), cex.lab = 1.5)
for (i in 1:ncol(predictors)) {
  plot(predictors[, i], residuals_sim$scaledResiduals,
       xlab = colnames(predictors)[i],
       ylab = "Scaled Residuals",
       main = paste("Residuals vs", colnames(predictors)[i]),
       pch = 19, col = "blue")
  abline(h = 0, col = "red")  # Add a horizontal line at y = 0
}

# Fitted values vs observed values
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

# Plot residuals vs each covariate in the model
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
plot(EZANB ~ eqr,
     xlab = "EQR",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ agriculture,
     xlab = "Agricultural coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ artificial,
     xlab = "Artificial coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ natural,
     xlab = "Natural & forested coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ ppt,
     xlab = "Precipitation",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ tmax,
     xlab = "Maximum temperatures",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ ws,
     xlab = "Wind speed",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

# Fit a smoother on the residuals and see whether it tells us something.
library(mgcv)
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
T1 <- gam(EZANB ~ s(eqr),
          data = SA)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EZANB ~ s(agriculture),
          data = SA)
summary(T2)
plot(T2)
#That is not really convincing.

T3 <- gam(EZANB ~ s(artificial),
          data = SA)
summary(T3)
plot(T3)
#That is not really convincing.

T4 <- gam(EZANB ~ s(natural),
          data = SA)
summary(T4)
plot(T4)
#That is interesting.

T5 <- gam(EZANB ~ s(ppt),
          data = SA)
summary(T5)
plot(T5)
#That is not really convincing.

T6 <- gam(EZANB ~ s(tmax),
          data = SA)
summary(T6)
plot(T6)
#That is interesting.

T7 <- gam(EZANB ~ s(ws),
          data = SA)
summary(T7)
plot(T7)
#That is not really convincing.

# Plot residuals vs each covariate not in the model.
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
boxplot(EZANB ~ fyear,
        xlab = "Year",
        ylab = "Pearson residuals",
        data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ fmonth,
     xlab = "Month",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ water,
     xlab = "Water coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ wetland,
     xlab = "Wetland coverage",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ q,
     xlab = "Discharge",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

plot(EZANB ~ tmin,
     xlab = "Minimum temperatures",
     ylab = "Pearson residuals",
     data = SA)
abline(h = 0, lty = 2)

# Fit a smoother on the residuals and see whether it tells us something.
T1 <- gam(EZANB ~ s(water),
          data = SA)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EZANB ~ s(q),
          data = SA)
summary(T2)
plot(T2)
#That is interesting.

T3 <- gam(EZANB ~ s(tmin),
          data = SA)
summary(T3)
plot(T3)
#That is interesting.

# also fit some box plots
boxplot(EZANB ~ fyear, data = SA)
boxplot(EZANB ~ fmonth, data = SA)


# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 300 km
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
mydata <- data.frame(EZANB, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
Vario.ZANB <- variogram(EZANB ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario.ZANB,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Kinda!

# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 300 km
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
mydata <- data.frame(residuals_sim$scaledResiduals, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
Vario.ZANB <- variogram(residuals_sim$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario.ZANB,
     main = "",
     xlab = list(label = "Distance (km)", cex = 1.5),
     ylab = list(label = "Semi-variogram", cex = 1.5),
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? YESSSSS :D!

############ Model selection of ZANB model ##########
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
