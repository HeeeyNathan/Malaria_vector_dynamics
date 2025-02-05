################################################
# Load support files
library(tidyverse)
library(janitor) # cleans names of columns
library(sf) # for geographic data
library(sp) # for geographic data
library(lattice)
library(INLA)
library(gstat)
library(ggplot2)
library(fields)
library(dismo)
library(rgeos)
library(fields)
source("Additional functions/HighstatLibV11.R")

#################################################
# Load the data
# Calculated index data from rivers (2013 - 2022)
river_indices <- as_tibble(read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE)) %>% 
  clean_names() %>% 
  dplyr::select(-c(21:25, 31:35)) %>% 
  filter(waterbody_type == "river") %>% 
  filter(year >= 2013 & year <= 2022) %>% 
  mutate(fyear = factor(year),
         fsite_id = factor(site_id),
         date = as.Date(date, format = "%d/%m/%Y"), 
         friver_type = factor(river_type, levels = c("1", "2", "3", "4", "5"), 
                              labels = c("Type1", "Type2", "Type3", "Type4", "Type5"),
                              ordered = T),
         fstate = factor(state, levels = c("N", "HM", "A"), 
                         labels = c("Natural", "HeavilyModified", "Artificial"),
                         ordered = T),
         feqc = factor(eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High"), 
                       labels = c("Bad", "Poor", "Moderate", "Good", "High"),
                       ordered = T),
         waterbody_name = factor(waterbody_name),
         doy = yday(date)) %>%
  arrange(desc(waterbody_type), site_id, year)

# Convert tibble to dataframe
river_indices <- as.data.frame(river_indices) # this is needed because some older code does not recocgnise tibble

#################################################
# Housekeeping
# Define the function to determine the UTM zone for each coordinate
getUTMZone <- function(lon) {
  return((floor((lon + 180) / 6) %% 60) + 1)
}

# Calculate UTM zones
river_indices$zone <- sapply(river_indices$longitude, getUTMZone)

# Create a UTM CRS string for each row
river_indices$utm_crs <- paste0("+proj=utm +zone=", river_indices$zone, " +datum=WGS84")

# Initialize columns for UTM coordinates
river_indices$Xutm_correctzone <- NA
river_indices$Yutm_correctzone <- NA

# Convert latitude and longitude to spatial points
coords <- SpatialPoints(river_indices[, c("longitude", "latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

# Apply the UTM conversion for each point
for (i in 1:nrow(river_indices)) {
  utm_coord <- spTransform(SpatialPoints(coords[i, ], proj4string = CRS("+proj=longlat +datum=WGS84")), CRS(river_indices$utm_crs[i]))
  river_indices$Xutm_correctzone[i] <- coordinates(utm_coord)[1, 1]
  river_indices$Yutm_correctzone[i] <- coordinates(utm_coord)[1, 2]
}

# covert to km
river_indices$Xkm_correctzone <- river_indices$Xutm_correctzone / 1000
river_indices$Ykm_correctzone <- river_indices$Yutm_correctzone / 1000

# Remove the utm_crs column
river_indices$utm_crs <- NULL

# plot coordinates
ggplot(river_indices, aes(x = Xkm_correctzone, y = Ykm_correctzone)) +
  geom_point() +
  labs(title = "UTM Coordinates of River Indices", x = "UTM Easting", y = "UTM Northing") +
  theme_minimal()

# Define a function to convert to UTM and get the corresponding zone
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone, sep='')))
  return(as.data.frame(res))
}

xy <- LongLatToUTM(x = river_indices$longitude, y = river_indices$latitude, zone = 34)
river_indices$Xutm <- xy[,2]
river_indices$Yutm <- xy[,3]

# covert to km
river_indices$Xkm <- river_indices$Xutm / 1000
river_indices$Ykm <- river_indices$Yutm / 1000

# plot coordinates
ggplot(river_indices, aes(x = Xkm, y = Ykm)) +
  geom_point() +
  labs(title = "UTM Coordinates of River Indices", x = "UTM Easting", y = "UTM Northing") +
  theme_minimal()

# Inspect data
names(river_indices)
str(river_indices)
head(river_indices)
dim(river_indices)

#################################################
# Create boundry for spatial random field mesh
library('maps')
library('maptools')
library('mapdata')
data("worldHiresMapEnv")
mapdata <- data("worldHiresMapEnv")
range(river_indices$latitude) # in decimal degree format
range(river_indices$longitude) # in decimal degree format
BoundaryPoly <- map("world", 
                    regions = c("lithuania", "poland", "russia", "latvia", "belarus"),
                    fill = TRUE, 
                    col = "blue",
                    boundary = TRUE,
                    interior = TRUE,
                    plot = TRUE, 
                    ylim = c(53, 57),
                    xlim = c(21, 27))
points(x = river_indices$longitude, y = river_indices$latitude, col = "yellow", pch = 1, cex = 1)
points(x = river_indices$longitude, y = river_indices$latitude, col = "yellow", pch = 20, cex = 1)

IDs <- sapply(strsplit(BoundaryPoly$names, ":"), function(x) x[1])
BoundarySP <- map2SpatialPolygons(map = BoundaryPoly, 
                                  IDs = IDs,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))

# In latitude/longitude
plot(BoundarySP,
     ylim = c(53,57),
     xlim = c(21, 27))
points(x = river_indices$longitude, y = river_indices$latitude)

#Make the boundary less detailed
BoundarySP.smooth <- thinnedSpatialPoly(BoundarySP, 
                                        tolerance = 0.05, 
                                        minarea= 0.05, 
                                        topologyPreserve = TRUE, 
                                        avoidGEOS = FALSE)
Boundary.UTM <- spTransform(BoundarySP, 
                            CRS("+proj=utm +zone=34 +datum=WGS84"))

plot(Boundary.UTM)

#################################################
# Data exploration
par(mfrow = c(1,1), mar = c(4,5,1,2), cex.lab = 1.5)
plot(table(river_indices$vec_abund), ylab = "vec_abund")
sum(river_indices$vec_abund == 0) /nrow(river_indices) # 63% of zeros
# Lots of zeros!

pts <- river_indices[, c("longitude" , "latitude", "vec_abund")] 
coordinates(pts) <- c("longitude", "latitude") 
migmap <- gmap(x = pts, 
               type = "hybrid", 
               zoom = 7,
               map_key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc")
par(mfrow = c(1,1))
plot(migmap)

river_indicesPos <- river_indices[river_indices[,"vec_abund"] > 0, ]
ptsPos    <- river_indicesPos[, c("longitude" , "latitude", "vec_abund")] 
coordinates(ptsPos) <- c("longitude", "latitude") 

MyCex <- 5 * river_indicesPos[,"vec_abund"] / max(river_indicesPos[,"vec_abund"]) + 0.5
points(Mercator(ptsPos), 
       col = "darkred", 
       pch = 20, 
       cex = MyCex)

river_indices01 <- river_indices[river_indices[,"vec_abund"] == 0,]
pts.0 <- river_indices01[, c("longitude" , "latitude", "vec_abund")] 
coordinates(pts.0) <- c("longitude", "latitude") 

# points(Mercator(pts.0), 
#        col = "darkblue", 
#        pch = 20, 
#        cex = 1)
points(Mercator(pts.0), 
       col = "darkblue", 
       pch = 1, 
       cex = 1)
# The function gmap is in Mercator coordinates.
# To superimpose the sampling locations, we need
# to convert them via the Mercator function.

# Outliers
colnames(river_indices)
MyX <- c("year", "eqr",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m", "water_bodies_200m", "wetlands_200m",
         "ppt", "q", "tmax", "tmin", "ws")
Mydotplot(river_indices[, MyX])

MyX <- c("year", "eqr",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m",
         "ppt", "tmax", "tmin", "ws")
Mydotplot(river_indices[, MyX])

# Collinearity
Mypairs(river_indices[,MyX])
corvif(river_indices[,MyX])

# Missing values?
colSums(is.na(river_indices))  
100 * colSums(is.na(river_indices)) / nrow(river_indices)  
which(is.na(river_indices$eqr)) # observation #467

# remove observation with missing EQR data
river_indices <- river_indices[-467, ]
which(is.na(river_indices$eqr)) # removed :)

# How many observations do we have per year?
table(river_indices$fyear)
# slightly less observations in 2019

# How many observations do we have per location x year?
table(river_indices$site_id, river_indices$fyear)

# Relationships
MyX <- c("year", "eqr", "Xkm", "Ykm",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m", "water_bodies_200m", "wetlands_200m",
         "ppt", "q", "tmax", "tmin", "ws")
MyMultipanel.ggp2(river_indices,
                  varx = MyX,
                  vary = "vec_abund",
                  ylab = "Vector abundances",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)
MyX_red <- c("year", "eqr",
             "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m",
             "ppt", "tmax", "tmin", "ws")
MyMultipanel.ggp2(river_indices,
                  varx = MyX_red,
                  vary = "vec_abund",
                  ylab = "Vector abundances",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)

# Zero inflation?
sum(river_indices[, "vec_abund"] == 0) / nrow(river_indices) # 63% of zeros
# Potentially yes

###########################################
# Start of analysis
# Fit a Poisson GLM and assess whether there is overdispersion.
# Standardize the covariates to avoid numerical problems.
river_indices$eqr_std             <- MyStd(river_indices$eqr)
river_indices$agricultural_std    <- MyStd(river_indices$agricultural_areas_200m)
river_indices$artificial_std      <- MyStd(river_indices$artificial_surfaces_200m)
river_indices$forest_std          <- MyStd(river_indices$forest_and_semi_natural_areas_200m)
river_indices$waterbodies_std     <- MyStd(river_indices$water_bodies_200m)
river_indices$wetlands_std        <- MyStd(river_indices$wetlands_200m)
river_indices$ppt_std             <- MyStd(river_indices$ppt)
river_indices$q_std               <- MyStd(river_indices$q)
river_indices$tmax_std            <- MyStd(river_indices$tmax)
river_indices$tmin_std            <- MyStd(river_indices$tmin)
river_indices$ws_std              <- MyStd(river_indices$ws)

Poi <- inla(vec_abund ~ fyear + eqr_std + 
              agricultural_std + artificial_std + forest_std +
              ppt_std + tmax_std + tmin_std + ws_std,
            family = "poisson",
            control.compute = list(dic = TRUE, waic = TRUE),
            data = river_indices)
muPoi <- Poi$summary.fitted.values[,"mean"]
EPoi <- (river_indices$vec_abund - muPoi) / sqrt(muPoi)
summary(Poi)

# Calcuate the dispersion statistic
N <- nrow(river_indices)
Poi$names.fixed
p <- length(Poi$names.fixed)
Dispersion <- sum(EPoi^2) / (N - p)
Dispersion # 172.2004
# That is overdispersion!

# Why do we have overdispersion?
# A. Outliers Y?           ==> Remove them?
# B. Missing covariates?   ==> Add them (or add a latent variable)
# C. Missing interSBtions? ==> Add them
# D. Zero inflation?       ==> ZIP / ZAP
# E. Large variance?       ==> NB or GeneSBlized Poisson
# F. Correlation?          ==> GLMM
# G. Non-linear patterns   ==> GAM 
# H. Wrong link function   ==> Change it 

# A. Outliers?
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = muPoi, 
     y = EPoi,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)     
# No clear outliers.

# Plot residuals vs each covariate in the model
# Each covariate not in the model.
par(mfrow = c(3,3), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = river_indices$fyear, 
     y = EPoi,
     xlab = "Year",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$eqr, 
     y = EPoi,
     xlab = "EQR",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$agricultural_areas_200m, 
     y = EPoi,
     xlab = "Agricultural areas",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$artificial_surfaces_200m, 
     y = EPoi,
     xlab = "Artificial surfaces",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$forest_and_semi_natural_areas_200m, 
     y = EPoi,
     xlab = "Forest & semi-natural areas",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$ppt, 
     y = EPoi,
     xlab = "ppt",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$tmax, 
     y = EPoi,
     xlab = "tmax",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$tmin, 
     y = EPoi,
     xlab = "tmin",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$ws, 
     y = EPoi,
     xlab = "ws",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# reset plotting parametres
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)

# We can fit a smoother on the residuals and see whether it tells us something.
library(mgcv)
par(mfrow = c(3,3), mar = c(5,5,2,2), cex.lab = 1.5)
T1 <- gam(EPoi ~ s(eqr), 
          data = river_indices)
summary(T1)
plot(T1)
# Not really convincing

T2 <- gam(EPoi ~ s(agricultural_areas_200m), 
          data = river_indices)
summary(T2)
plot(T2)
# Not really convincing

T3 <- gam(EPoi ~ s(artificial_surfaces_200m), 
          data = river_indices)
summary(T3)
plot(T3)
# Not really convincing

T4 <- gam(EPoi ~ s(forest_and_semi_natural_areas_200m), 
          data = river_indices)
summary(T4)
plot(T4)
# Some non-linearity
# Not really convincing

T5 <- gam(EPoi ~ s(ppt), 
          data = river_indices)
summary(T5)
plot(T5)
# Not really convincing

T6 <- gam(EPoi ~ s(tmax), 
          data = river_indices)
summary(T6)
plot(T6)
# Some non-linearity
# Not really convincing

T7 <- gam(EPoi ~ s(tmin), 
          data = river_indices)
summary(T7)
plot(T7)
# Some non-linearity
# Not really convincing

T8 <- gam(EPoi ~ s(ws), 
          data = river_indices)
summary(T8)
plot(T8)
# Not really convincing

# T9
boxplot(EPoi ~ fyear, data = river_indices)

# reset plotting parametres
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)

# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 100 km
mydata <- data.frame(EPoi, river_indices$Ykm, river_indices$Xkm)
coordinates(mydata)    <- c("river_indices.Ykm", "river_indices.Xkm")
GLM.Poi <- variogram(EPoi ~ 1, cutoff = 200, mydata,  cressie = TRUE)
plot(GLM.Poi, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? No!

# Zero inflation?
# Simulation study
I2 <- inla(vec_abund ~ fyear + eqr_std +
             agricultural_std + artificial_std + forest_std +
             ppt_std + tmax_std + tmin_std + ws_std, 
           family = "poisson", 
           data = river_indices,
           control.compute=list(config = TRUE))

set.seed(12345)
SimData <- inla.posterior.sample(n = 1, result = I2, seed = 12345)
names(SimData[[1]])
SimData[[1]]$latent

X <- model.matrix(~ fyear + eqr_std +
                    agricultural_std + artificial_std + forest_std +
                    ppt_std + tmax_std + tmin_std + ws_std,
                  data = river_indices)

MyID <- function(x) {
  if (x == "(Intercept)") {
    return(grep("^\\(Intercept\\)", rownames(SimData[[1]]$latent)))
  } else {
    return(grep(x, rownames(SimData[[1]]$latent)))
  }
}

# Extract the indices of the parameters in the latent field
MyParams <- colnames(X)
RowNum.Betas <- sapply(MyParams, MyID)
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas

# Check if RowNum.Betas contains NA values
if (any(is.na(RowNum.Betas))) {
  stop("Some parameters were not found in SimData[[1]]$latent.")
}

# Extract Betas correctly
Betas <- SimData[[1]]$latent[RowNum.Betas]
Xm <- as.matrix(X)

FixedPart   <- Xm %*% Betas
mu   <- exp(FixedPart)
Ysim <- rpois(n = nrow(river_indices), lambda = mu)
table(Ysim)

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(Ysim),
     xlab = "Simulated vector abundances",
     ylab = "Frequencies")

NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = I2)
N  <- nrow(river_indices)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xm <- as.matrix(X)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  FixedPart   <- Xm %*% Betas
  mu.i[,i]    <- exp(FixedPart)
  Ysim[,i]    <- rpois(n = nrow(river_indices), lambda = mu.i[,i])
}
table(Ysim[,1])
table(Ysim[,2])
table(Ysim[,3])

# Now we have 1000 simulated data sets from the model.
# What shall we do with these simulated data sets?

# We could calculate the number of zeros in eSBh of the 1,000
# data sets.
zeros <- vector(length = NSim)
for(i in 1:NSim){
  zeros[i] <- sum(Ysim[,i] == 0)
}

table(zeros)

# From the 1,000 simulated data sets, in 2 simulated
# data sets we had 1141 zeros. In 2 simulated data sets
# we had 1148 zeros,etc......
# Your results will be different as mine.

# Just type 
Ysim[,1]
Ysim[,2]
Ysim[,3]
#etc

#Let's plot this as a table
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(0, 1000),
     main = "Simulation results")
points(x = sum(river_indices$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the Poisson model does not contain enough zeros.

###########################################
# Creating a mesh
# What are distances between sites?
Loc <- cbind(river_indices$Xutm, river_indices$Yutm)
D   <- dist(Loc)

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


# What is the distance for which we would expect dependency?
# 50 km? (1 degree = ~ 111 km)

RangeGuess <- 50 * 1000     #  50 km...Use for paper
# The smaller this value the better...but the longer the computer time

# Recommended settings:
MaxEdge <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 50 * 1000)
# The convex option puts the boundary of the innerpart
# closer to the points. Saves some computing time
# during this course. Maybe not use it for a paper.
ConvHull <- inla.nonconvex.hull(Loc)
mesh1 <- inla.mesh.2d(boundary = ConvHull, 
                      max.edge = c(1, 5) * MaxEdge, 
                      cutoff = MaxEdge / 5)
mesh1$n 
# 3600 vertices

par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(mesh1, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)

#  Define the weighting factors a_ik (also called the projector matrix).
A1  <- inla.spde.make.A(mesh1, loc = Loc)

# Define the SPDE.
spde1 <- inla.spde2.pcmatern(mesh1, 
                             prior.range = c(50 * 1000, 0.5), # 0.5 means we do not know what the range is, it can be smaller or larger than 50km 
                             prior.sigma = c(1.5, 0.01))
# This is tricky stuff. We need to specify this:
# P(Range < or > range0) = 0.5  and P(sigma > sigma0) = 0.01

# The range value is our primary tool to control the amount
# of smoothing that the spatial random field will do.
# The larger it is, the smoother the spatial random field.
# It allows to avoid overfitting.

# We decided this:
# P(Range < 50 km) = 0.5  and P(sigma > 1.5) = 0.01

# Define the spatial field.
w1.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spde1$n.spde)

# Defining a stack
Xm <- model.matrix(~ fyear + eqr_std +
                     agricultural_std + artificial_std + forest_std +
                     ppt_std + tmax_std + tmin_std + ws_std,
                   data = river_indices)
X <- data.frame(
  fyear2014   = Xm[, 2],
  fyear2015   = Xm[, 3],
  fyear2016   = Xm[, 4],
  fyear2017   = Xm[, 5],
  fyear2018   = Xm[, 6],
  fyear2019   = Xm[, 7],
  fyear2020   = Xm[, 8],
  fyear2021   = Xm[, 9],
  fyear2022   = Xm[, 10],
  Agriculture = Xm[, 11],
  Artificial  = Xm[, 12],
  Forest      = Xm[, 13],
  ppt         = Xm[, 14],
  tmax        = Xm[, 15],  
  tmin        = Xm[, 16], 
  ws          = Xm[, 17])

N <- nrow(river_indices)

Stack.mesh1 <- inla.stack(
  tag = "Fit",
  data = list(y = river_indices$vec_abund),
  A = list(1, 1, A1),
  effects = list(
    Intercept = rep(1, N),
    X         = as.data.frame(X),
    w         = w1.index)
)

# Defining a formula
fPois.mesh1 <- y ~ -1 + Intercept + 
  fyear2014 + fyear2015 + fyear2016 + fyear2017 + fyear2018 + fyear2019 + fyear2020 + fyear2021 + fyear2022 +
  Agriculture + Artificial + Forest + 
  ppt + tmax + tmin + ws +
  f(w, model = spde1)

# executing a Poisson GLM with spatial correlation
Pois.mesh1 <- inla(fPois.mesh1,
                   family = "poisson",
                   data = inla.stack.data(Stack.mesh1),
                   control.compute = list(dic = TRUE,
                                          waic = TRUE),
                   control.predictor = list(
                     A = inla.stack.A(Stack.mesh1)))
summary(Pois.mesh1)

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
                              dims = c(600, 600))
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

PlotField(field = w1.pm, 
          mesh = mesh1, 
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))

# dark blue areas = zero abundance of vectors
# dark red areas = high abundance of vectors

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)

# Add the boundary (in UTM)
Boundary.UTM = spTransform(BoundarySP.smooth, 
                        CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(Boundary.UTM, add = TRUE)

# Model validation 
# Plot fitted values versus observed data
mu.SpatPois <- Pois.mesh1$summary.fitted.values[1:1436, "mean"]
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.SpatPois,
     y = river_indices$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector abundances")

# Simulation study
Pois.mesh1.sim <- inla(fPois.mesh1,
                       family = "poisson",
                       data = inla.stack.data(Stack.mesh1),
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                       control.predictor = list(A = inla.stack.A(Stack.mesh1)))


set.seed(12345)
NSim          <- 10000
SimData       <- inla.posterior.sample(n = NSim, 
                                       result = Pois.mesh1.sim)
N             <- nrow(river_indices)
ZerosPoisSpat <- vector(length = NSim)
YPoisSpat     <- matrix(nrow = N, ncol = NSim)

MyID <- function(x){ which(rownames(SimData[[1]]$latent) == x) }

# Adjust MyParams to match rownames of SimData[[1]]$latent
MyParams <- colnames(X)
MyParams <- paste("Predictor", seq_along(MyParams), sep = ":")

# Find row numbers for fixed effects
RowNum.Pos <- lapply(MyParams, MyID)
RowNum.Pos <- as.numeric(RowNum.Pos)
print(RowNum.Pos)

# Inspect row names related to 'w' to understand their structure
print(rownames(SimData[[1]]$latent)[grepl("^w", rownames(SimData[[1]]$latent))])

# Adjust MyParams for random effects based on inspection
MyParams <- rownames(SimData[[1]]$latent)[grepl("^w", rownames(SimData[[1]]$latent))]
RowNum.w <- lapply(MyParams, MyID)
RowNum.w <- as.numeric(RowNum.w)
print(RowNum.w)

Xm <- as.matrix(X)
Am1 <- as.matrix(A1)
for (i in 1:NSim){
  Beta  <- SimData[[i]]$latent[RowNum.Pos]
  w     <- SimData[[i]]$latent[RowNum.w]
  mu       <- exp(Xm %*% Beta + Am1 %*% w)
  YPoisSpat[,i]    <- rpois(N, lambda = mu)
  ZerosPoisSpat[i] <- sum(YPoisSpat[,i] == 0)
}


# Figure 19.12
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosPoisSpat), 
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 10000 simulated data sets",
     xlim = c(0, 200),
     main = "Simulation results")
points(x = sum(Skate$SB == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(Skate$SB == 0) > ZerosPoisSpat) / 10000
sum(Skate$SB == 0)





# Make a stack. 
# "EQR", "latitude", "longitude",
# "Agricultural.areas_200m",  "Artificial.surfaces_200m", "Forest.and.semi.natural.areas_200m",
# "ppt", "tmax", "tmin", "ws"
Xyear <- model.matrix(~ fyear, data = river_indices)
N <- nrow(river_indices)
Covariates <- data.frame(
  Intercept   = rep(1, N),
  # fyear2011   = Xyear[,"fyear2011"],
  # fyear2012   = Xyear[,"fyear2012"],
  # fyear2013   = Xyear[,"fyear2013"],
  fyear2014   = Xyear[,"fyear2014"],
  fyear2015   = Xyear[,"fyear2015"],
  fyear2016   = Xyear[,"fyear2016"],
  fyear2017   = Xyear[,"fyear2017"],
  fyear2018   = Xyear[,"fyear2018"],
  fyear2019   = Xyear[,"fyear2019"],
  fyear2020   = Xyear[,"fyear2020"],
  fyear2021   = Xyear[,"fyear2021"],
  fyear2022   = Xyear[,"fyear2022"],
  # EQR         = MyStd(river_indices$EQR),
  Agriculture = MyStd(river_indices$Agricultural.areas_200m),
  Artificial  = MyStd(river_indices$Artificial.surfaces_200m),
  Forest      = MyStd(river_indices$Forest.and.semi.natural.areas_200m),
  ppt         = MyStd(river_indices$ppt),
  tmax        = MyStd(river_indices$tmax),  
  # tmin        = MyStd(river_indices$tmin),
  ws          = MyStd(river_indices$ws)
)

StackPoisGLM <- inla.stack(
  tag = "PoissonFit",
  data = list(y = river_indices$vec_abund),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w = wPois.index))

###############################################################
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
