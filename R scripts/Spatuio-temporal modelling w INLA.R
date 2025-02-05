# Load packages and library files
library(lattice)
library(sp)
# library(rgdal)
library(INLA)
library(ggmap)
library(gstat)
library(ggplot2)
library(rgeos)
library(fields)
library('maps')
library('maptools')
library('mapdata')
data("worldHiresMapEnv")

source("Additional functions/HighstatLibV11.R")

# Load data
# All Lithuanian macroinvertebrate data from lakes and rivers (2010 - 2023)
all_raw_data <- read.csv("Data/Bio_data_2024.06.03_long_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE)
all_raw_data <- all_raw_data[all_raw_data$year <= 2022, ]
all_raw_data$fyear <- factor(all_raw_data$year)
all_raw_data$fsite_id <- factor(all_raw_data$site_id)
all_raw_data$fsite_code <- factor(all_raw_data$site_code)
all_raw_data$fsample_id <- factor(all_raw_data$sample_id)
all_raw_data$date <- as.Date(all_raw_data$date, format = "%d/%m/%Y")
all_raw_data$friver_type = factor(all_raw_data$river_type)
all_raw_data$fstate = factor(all_raw_data$state)
all_raw_data$fEQC = factor(all_raw_data$EQC, levels = c("Bad", "Poor", "Moderate", "Good", "High"), ordered = T)
all_raw_data$waterbody_name = factor(all_raw_data$waterbody_name)
all_raw_data$waterbody_type = factor(all_raw_data$waterbody_type)
all_raw_data <- all_raw_data[order(-xtfrm(all_raw_data$waterbody_type), all_raw_data$site_id, all_raw_data$year), ]

# Calculated index data from lakes and rivers (2010 - 2023)
all_indices <- read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE)
all_indices <- subset(all_indices, year >= 2013 & year <= 2022)
all_indices$fyear <- factor(all_indices$year)
all_indices$fsite_id <- factor(all_indices$site_id)
all_indices$fsite_code <- factor(all_indices$site_code)
all_indices$fsample_id <- factor(all_indices$sample_id)
all_indices$date <- as.Date(all_indices$date, format = "%d/%m/%Y")
all_indices$friver_type = factor(all_indices$river_type)
all_indices$fstate = factor(all_indices$state)
all_indices$fEQC = factor(all_indices$EQC, levels = c("Bad", "Poor", "Moderate", "Good", "High"), ordered = T)
all_indices$waterbody_name = factor(all_indices$waterbody_name)
all_indices$waterbody_type = factor(all_indices$waterbody_type)
all_indices <- all_indices[order(-xtfrm(all_indices$waterbody_type), all_indices$site_id, all_indices$year), ]

# Calculated index data from rivers (2013 - 2023)
river_indices <- subset(all_indices, waterbody_type == "river")
river_indices <- river_indices[order(river_indices$site_id, river_indices$year), ]
rownames(river_indices) <- NULL

# check the data
names(river_indices)
str(river_indices)
head(river_indices)
dim(river_indices)

# Make map
names(river_indices)
river_factors <- subset(river_indices, select = c(site_id:river_type))
summary(river_factors$latitude)
summary(river_factors$longitude)
map <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57), maptype = "terrain")
p <- ggmap(map)
p <- p + geom_point(aes(longitude, latitude),
                    data = river_indices)
p <- p + xlab("Longitude") + ylab("Latitude")
p <- p + facet_wrap( ~ fyear)
p

# Convert coordinates
# For INLA we need UTM coordinates
# See: https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
LongLatToUTM <- function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

xy <- LongLatToUTM(x = river_indices$longitude, y = river_indices$latitude, zone = 34)
river_indices$Xutm <- xy[,2]
river_indices$Yutm <- xy[,3]

river_indices$Xkm <- river_indices$Xutm / 1000
river_indices$Ykm <- river_indices$Yutm / 1000

################################################################################
# DATA EXPLORATION
# Outliers in the covariates
MyVar <- c( "EQR", "latitude", "longitude",
            "Agricultural.areas_200m",  "Artificial.surfaces_200m", "Forest.and.semi.natural.areas_200m", "Water.bodies_200m", "Wetlands_200m",
            "ppt", "q", "tmax", "tmin", "ws")
Mydotplot(river_indices[,MyVar])

# Outliers response variable
Myvar <- c("dipt_abund", "vec_abund")
Mydotplot(river_indices[,Myvar])
# Lots of zeros!

# Missing values?
colSums(is.na(river_indices))  
100 * colSums(is.na(river_indices)) / nrow(river_indices)  
which(is.na(river_indices$fEQC)) # observation #467
which(is.na(river_indices$EQR)) # observation #467

# remove observation with missing EQR data
river_indices <- river_indices[-467, ]

# How many observations do we have per year?
table(river_indices$fyear)

# How many observations do we have per location x year?
table(river_indices$site_id, river_indices$fyear)

# Collinearity
MyVar <- c( "EQR", "latitude", "longitude",
            "Agricultural.areas_200m",  "Artificial.surfaces_200m", "Forest.and.semi.natural.areas_200m",
            "ppt", "tmax", "ws")

Mypairs(river_indices[,MyVar])
corvif(river_indices[,MyVar])

# Do we have collinearity within a year?
# there are some weird patterns in 2013 when it comes to EQR
xyplot(EQR ~ latitude | fyear,
       data = river_indices)
xyplot(EQR ~ longitude | fyear,
       data = river_indices)
xyplot(EQR ~ Agricultural.areas_200m | fyear,
       data = river_indices)
xyplot(EQR ~ Artificial.surfaces_200m | fyear,
       data = river_indices)
xyplot(EQR ~ Forest.and.semi.natural.areas_200m | fyear,
       data = river_indices)
xyplot(EQR ~ ppt | fyear,
       data = river_indices)
xyplot(EQR ~ tmax | fyear,
       data = river_indices)
xyplot(EQR ~ ws | fyear,
       data = river_indices)
xyplot(latitude ~ longitude | fyear,
       data = river_indices)
xyplot(latitude ~ Agricultural.areas_200m | fyear,
       data = river_indices)
xyplot(latitude ~ Artificial.surfaces_200m | fyear,
       data = river_indices)
xyplot(latitude ~ Forest.and.semi.natural.areas_200m | fyear,
       data = river_indices)
xyplot(latitude ~ ppt | fyear,
       data = river_indices)
xyplot(latitude ~ tmax | fyear,
       data = river_indices)
xyplot(latitude ~ ws | fyear,
       data = river_indices)
xyplot(longitude ~ Agricultural.areas_200m | fyear,
       data = river_indices)
xyplot(longitude ~ Artificial.surfaces_200m | fyear,
       data = river_indices)
xyplot(longitude ~ Forest.and.semi.natural.areas_200m | fyear,
       data = river_indices)
xyplot(longitude ~ ppt | fyear,
       data = river_indices)
xyplot(longitude ~ tmax | fyear,
       data = river_indices)
xyplot(longitude ~ ws | fyear,
       data = river_indices)
xyplot(Agricultural.areas_200m ~ Artificial.surfaces_200m | fyear,
       data = river_indices)
xyplot(Agricultural.areas_200m ~ Forest.and.semi.natural.areas_200m | fyear,
       data = river_indices)
xyplot(Agricultural.areas_200m ~ ppt | fyear,
       data = river_indices)
xyplot(Agricultural.areas_200m ~ tmax | fyear,
       data = river_indices)
xyplot(Agricultural.areas_200m ~ ws | fyear,
       data = river_indices)
xyplot(Artificial.surfaces_200m ~ Forest.and.semi.natural.areas_200m | fyear,
       data = river_indices)
xyplot(Artificial.surfaces_200m ~ ppt | fyear,
       data = river_indices)
xyplot(Artificial.surfaces_200m ~ tmax | fyear,
       data = river_indices)
xyplot(Artificial.surfaces_200m ~ ws | fyear,
       data = river_indices)
xyplot(Forest.and.semi.natural.areas_200m ~ ppt | fyear,
       data = river_indices)
xyplot(Forest.and.semi.natural.areas_200m ~ tmax | fyear,
       data = river_indices)
xyplot(Forest.and.semi.natural.areas_200m ~ ws | fyear,
       data = river_indices)
xyplot(ppt ~ tmax | fyear,
       data = river_indices)
xyplot(ppt ~ ws | fyear,
       data = river_indices)
xyplot(tmax ~ ws | fyear,
       data = river_indices)

# Zero inflation
# Dipteran abundance
sum(river_indices$dipt_abund == 0)  #Number of zeros
100 * sum(river_indices$dipt_abund == 0) / nrow(river_indices)  #0% of zeros
# None

plot(table(river_indices$dipt_abund), type = "h")
# We may need a model that can deal with
# the zeros....what about a ZAP?

# Vector abundance
sum(river_indices$vec_abund == 0)  #Number of zeros
100 * sum(river_indices$vec_abund == 0) / nrow(river_indices)  #63% of zeros
# That's a lot!

plot(table(river_indices$vec_abund), type = "h")
# We may need a model that can deal with
# the zeros....what about a ZAP?

# Relationships
MyVar <- c( "EQR", "latitude", "longitude",
            "Agricultural.areas_200m",  "Artificial.surfaces_200m", "Forest.and.semi.natural.areas_200m",
            "ppt", "tmax", "ws")
MyMultipanel.ggp2(river_indices,
                  varx = MyVar,
                  vary = "dipt_abund",
                  ylab = "Diptera abundances",
                  addSmoother = FALSE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)
MyMultipanel.ggp2(river_indices,
                  varx = MyVar,
                  vary = "vec_abund",
                  ylab = "Vector abundances",
                  addSmoother = FALSE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)

# Check skewed variables
# What happens of we transform this one?
p <- ggplot(river_indices, aes(y = vec_abund, x = log10(Agricultural.areas_200m)))
p <- p + geom_point() + ylab("Dipteran abundance") + xlab("Log10 transformed Agricultural areas (200m)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(river_indices, aes(y = vec_abund, x = log10(Artificial.surfaces_200m)))
p <- p + geom_point() + ylab("Dipteran abundance") + xlab("Log10 transformed Artificial surfaces (200m)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(river_indices, aes(y = vec_abund, x = log10(Forest.and.semi.natural.areas_200m)))
p <- p + geom_point() + ylab("Dipteran abundance") + xlab("Log10 transformed Forest and semi natural areas_200m (200m)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

# Summary data exploration
# 1. Lots of zeros in vector abundance response variable
# 2. Collinearity between some covariates
# 3. Fewer sampling locations in 2018
# 4. No clear relationships between response and covariates
# 5. Vague reasons to do a transformation on some covariates.

# We will:
#  1. Apply a Poisson GLM on the abundance data
#  2. Apply a zero-truncated Poisson on the abundance data
#  3. Combine components 1 and 2.

# Transformations
# river_indices$Agriculture.log <- log10(river_indices$Agricultural.areas_200m)
# river_indices$Artificial.log   <- log10(river_indices$Artificial.surfaces_200m)
# river_indices$Forest.log   <- log10(river_indices$Forest.and.semi.natural.areas_200m)

# Implementation Poisson GLM in R-INLA
# Start off with a model without spatial correlation
river_indices$EQR.std <- MyStd(river_indices$EQR)
river_indices$Agricultural.areas_200m.std <- MyStd(river_indices$Agricultural.areas_200m)
river_indices$Artificial.surfaces_200m.std <- MyStd(river_indices$Artificial.surfaces_200m)
river_indices$Forest.and.semi.natural.areas_200m.std <- MyStd(river_indices$Forest.and.semi.natural.areas_200m)
river_indices$ppt.std <- MyStd(river_indices$ppt)
river_indices$tmax.std <- MyStd(river_indices$tmax)
river_indices$ws.std <- MyStd(river_indices$ws)

Poi <- inla(vec_abund ~ fyear + EQR.std + 
              Agricultural.areas_200m.std + Artificial.surfaces_200m.std + Forest.and.semi.natural.areas_200m.std + 
              ppt.std + tmax.std + ws.std, 
            family = "poisson", 
            control.compute = list(dic = TRUE,
                                   waic = TRUE),
            data = river_indices)
muPoi <- Poi$summary.fitted.values[,"mean"]
EPoi <- (river_indices$vec_abund - muPoi) / sqrt(muPoi)
summary(Poi)
# Calcuate the dispersion statistic
N <- nrow(river_indices)
Poi$names.fixed
p <- length(Poi$names.fixed)
Dispersion <- sum(EPoi^2) / (N - p)
Dispersion

# That is overdispersion!
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
# Each covariate not in the model.
par(mfrow = c(2,4), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = river_indices$fyear, 
     y = EPoi,
     xlab = "Year",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$EQR, 
     y = EPoi,
     xlab = "EQR",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

plot(x = river_indices$Agricultural.areas_200m, 
     y = EPoi,
     xlab = "Agricultural.areas_200m",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$Artificial.surfaces_200m, 
     y = EPoi,
     xlab = "Artificial.surfaces_200m",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

plot(x = river_indices$Forest.and.semi.natural.areas_200m, 
     y = EPoi,
     xlab = "Forest.and.semi.natural.areas_200m",
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

plot(x = river_indices$ws, 
     y = EPoi,
     xlab = "ws",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)

# We can fit a smoother on the residuals and see whether it tells us something.
library(mgcv) 
T1 <- gam(EPoi ~ s(EQR), 
          data = river_indices)
summary(T1)
plot(T1)
#That is not really convincing.
T2 <- gam(EPoi ~ s(Agricultural.areas_200m), 
          data = river_indices)
summary(T2)
plot(T2)
#That is not really convincing.
T3 <- gam(EPoi ~ s(Artificial.surfaces_200m), 
          data = river_indices)
summary(T3)
plot(T3)
#That is not really convincing.
T4 <- gam(EPoi ~ s(Forest.and.semi.natural.areas_200m), 
          data = river_indices)
summary(T4)
plot(T4)
#That is not really convincing.
T5 <- gam(EPoi ~ s(ppt), 
          data = river_indices)
summary(T5)
plot(T5)
#That is not really convincing.
T6 <- gam(EPoi ~ s(tmax), 
          data = river_indices)
summary(T6)
plot(T6)
#Some non-linearity
#That is not really convincing.
T7 <- gam(EPoi ~ s(ws), 
          data = river_indices)
summary(T7)
plot(T7)
#That is not really convincing.

# Not in the book:
boxplot(EPoi ~ fyear, data = river_indices)

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
#Simulation study
I2 <- inla(vec_abund ~ fyear + EQR.std + 
             Agricultural.areas_200m.std + Artificial.surfaces_200m.std + Forest.and.semi.natural.areas_200m.std + 
             ppt.std + tmax.std + ws.std, 
           family = "poisson", 
           data = river_indices,
           control.compute=list(config = TRUE))

set.seed(12345)
SimData <- inla.posterior.sample(n = 1, result = I2, seed = 12345)
names(SimData[[1]])
SimData[[1]]$latent

X <- model.matrix(~fyear + EQR.std + 
                    Agricultural.areas_200m.std + Artificial.surfaces_200m.std + Forest.and.semi.natural.areas_200m.std + 
                    ppt.std + tmax.std + ws.std,
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

###############################################################################
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
# 55.5 km? (1 degree = ~ 111 km)

RangeGuess <- 55.555 * 1000     #  55.5 km...Use for paper
# The smaller this value the better...but the longer the computer time

# Recommended settings:
MaxEdge <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 55.5 *1000)
# The convex option puts the boundary of the innerpart
# closer to the points. Saves some computing time
# during this course. Maybe not use it for a paper.
ConvHull <- inla.nonconvex.hull(Loc)
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)
meshPois$n

par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(meshPois, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)

#  Define the weighting factors a_ik (also called the projector matrix).
APois  <- inla.spde.make.A(meshPois, loc = Loc)

# Define the SPDE.
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(50 * 1000, 0.5), # we do not know what the range is, it can be smaller or larger than 50km 
                                prior.sigma = c(1.5, 0.01))
# This is tricky stuff. We need to specify this:
# P(Range < range0) = 0.5  and P(sigma > sigma0) = 0.01

# The range value is our primary tool to control the amount
# of smoothing that the spatial random field will do.
# The larger it is, the smoother the spatial random field.
# It allows to avoid overfitting.

# We decided this:
# P(Range < 50 km) = 0.5  and P(sigma > 1.5) = 0.01

# Define the spatial field.
wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)

# Defining a stack
Xm <- model.matrix(~fyear + EQR.std + 
                     Agricultural.areas_200m.std + Artificial.surfaces_200m.std + Forest.and.semi.natural.areas_200m.std + 
                     ppt.std + tmax.std + ws.std,
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
  ws          = Xm[, 16])

N <- nrow(river_indices)

Stack1 <- inla.stack(
  tag = "Fit",
  data = list(y = river_indices$vec_abund),
  A = list(1, 1, APois),
  effects = list(
    Intercept = rep(1, N),
    X         = as.data.frame(X),
    w         = wPois.index)
  )

# Defining a formula
fPois <- y ~ -1 + Intercept + 
  fyear2014 + fyear2015 + fyear2016 + fyear2017 + fyear2018 + fyear2019 + fyear2020 + fyear2021 + fyear2022 +
  Agriculture + Artificial + Forest + 
  ppt + tmax + ws +
  f(w, model = spdePois)

# executing a Poisson GLM with spatial correlation
Pois <- inla(fPois,
             family = "poisson",
             data = inla.stack.data(Stack1),
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(
               A = inla.stack.A(Stack1)
             ))

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

# We first fit models 1 and 2
fPois1 <- formula(y ~ -1 + Intercept + 
  fyear2011 + fyear2012 + fyear2013 + fyear2014 + fyear2015 + fyear2016 + fyear2017 + fyear2018 + fyear2019 + fyear2020 + fyear2021 + fyear2022 +
  Agriculture + Artificial + Forest + 
  ppt + tmax + ws)

# Execute a Poisson GLM model without spatial correlation
Pois1 <- inla(fPois1,
              family = "poisson", 
              #control.inla = list(strategy = "gaussian"), # add this if computing time is too long (it is a rougher estimation)
              data = inla.stack.data(StackPoisGLM),
              control.compute = list(dic = TRUE, waic = TRUE),
              control.predictor = list(A = inla.stack.A(StackPoisGLM)))
summary(Pois1)

# Model validation
# Check for overdispersion
p   <- nrow(Pois1$summary.fixed)
mu1 <- Pois1$summary.fitted.values[1:N,"mean"]
E1  <- (river_indices$vec_abund - mu1) / sqrt(mu1)
sum(E1^2) / (N - p) # this value is larger than 1, meaning we have overdispersion
# Overdispersion!

# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")

# Plot residuals versus each covariate
# Not in the book:
river_indices$E1 <- E1
p <- ggplot()
p <- p + geom_point(data = river_indices, 
                    aes(x = Agricultural.areas_200m, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Agriculture (%)") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = river_indices, 
                     aes(x = Agricultural.areas_200m, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# Difficult to assess. Let's use mgcv for this
library(mgcv)
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(Agricultural.areas_200m), data = river_indices)
T2 <- gam(E1 ~ s(Agricultural.areas_200m, by = fyear), data = river_indices)
AIC(T0, T1, T2)

par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = river_indices$Agricultural.areas_200m, 
     y = river_indices$E1,
     xlab = "Agricultural.areas_200m",
     ylab = "Pearson residuals")
text(0.1, 250, "A", cex = 1.5)

plot(T1)
abline(h = 0, lty = 2)
text(0.1, 0.9, "B", cex = 1.5)

summary(T2)
par(mfrow = c(3,5))
plot(T2)

# some linear effects, some non-linear effects - different in each year?
# And it explains 4.5% of the variation in the residuals!

# Do it for every covariate.
# "Agricultural.areas_200m",  "Artificial.surfaces_200m", "Forest.and.semi.natural.areas_200m",
# "ppt", "tmax", "ws"

# Agricultural.areas_200m
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(Agricultural.areas_200m), data = river_indices)
T2 <- gam(E1 ~ s(Agricultural.areas_200m, by = fyear), data = river_indices)
AIC(T0, T1, T2)

#          df      AIC
# T0  2.00000 14469.64
# T1  3.00000 14471.63
# T2 25.92104 14433.14 <---
par(mfrow = c(3,5))
plot(T2, scale = FALSE)

# Artificial.surfaces_200m
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(Artificial.surfaces_200m), data = river_indices)
T2 <- gam(E1 ~ s(Artificial.surfaces_200m, by = fyear), data = river_indices)
AIC(T0, T1, T2)
#          df      AIC
# T0  2.00000 14469.64
# T1  3.00000 14471.63
# T2 21.78013 14443.76 <---
par(mfrow = c(3,5))
plot(T2, scale = FALSE)

# Forest.and.semi.natural.areas_200m
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(Forest.and.semi.natural.areas_200m), data = river_indices)
T2 <- gam(E1 ~ s(Forest.and.semi.natural.areas_200m, by = fyear), data = river_indices)
AIC(T0, T1, T2)
#          df      AIC
# T0  2.000000 14469.64
# T1  3.628826 14470.78
# T2 37.633313 14382.99 <---
par(mfrow = c(3,5))
plot(T2, scale = FALSE)

# Precipitation
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(ppt), data = river_indices)
T2 <- gam(E1 ~ s(ppt, by = fyear), data = river_indices)
AIC(T0, T1, T2)
#          df      AIC
# T0  2.00000 14469.64
# T1  3.00000 14471.35
# T2 31.32255 14454.83  <----
par(mfrow = c(3,5))
plot(T2, scale = FALSE)

# Maximum temperature
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(tmax), data = river_indices)
T2 <- gam(E1 ~ s(tmax, by = fyear), data = river_indices)
AIC(T0, T1, T2)
#          df      AIC
# T0  2.000000 14469.64
# T1  3.416019 14471.17
# T2 48.175335 14163.58  <---
par(mfrow = c(3,5))
plot(T2, scale = FALSE)

# Wind speed
T0 <- gam(E1 ~ 1, data = river_indices)
T1 <- gam(E1 ~ s(ws), data = river_indices)
T2 <- gam(E1 ~ s(ws, by = fyear), data = river_indices)
AIC(T0, T1, T2)
#         df      AIC
# T0  2.000000 14469.64 <---
# T1  3.413878 14471.10
# T2 19.872892 14479.39

# GAM: 
#   Agricultural.areas_200m changes per year but shape does not make sense
#   Artificial.surfaces_200m changes per year but shape does not make sense
#   Forest.and.semi.natural.areas_200m changes per year but shape does not make sense
#   Precipitation changes per year (some linear, some non-linear)
#   Temperature maximum changes per year (mostly non-linear)
#   Wind speed changes ...... keep it as it is

# Conclusion:
#  We need smoothers for precipitation and temperature
#  And these smoothers need to change by year.

# What about spatial correlation in the residuals?
# Check the Pearson residuals of I1 and I2 for
# any spatial dependency using a
# variogram.
MyData1 <- data.frame(E1 = E1, 
                      Xkm = river_indices$Xkm, 
                      Ykm = river_indices$Ykm)
coordinates(MyData1) <- c("Xkm", "Ykm")
V1 <- variogram(E1 ~ 1, 
                MyData1, 
                cressie = TRUE, 
                cutoff = 111.111)

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = V1$dist,
     y = V1$gamma,
     xlab = "Distance",
     ylab = "Sample variogram",
     ylim = c(0,max(V1$gamma)),
     pch = 16)
# Some spatial correlation in the residuals 
# We could do this again with residuals per year (resulting
# in 5 variograms)


# Simulation study for zero-inflation
Pois1.sim <- inla(fPois1,
                  family = "poisson", 
                  data=inla.stack.data(StackPoisGLM),
                  control.compute = list(config = TRUE),
                  control.predictor = list(A = inla.stack.A(StackPoisGLM)))


# Simulate 1000 sets of betas and ws from the model:
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = Pois1.sim)


# Determine on which rows the betas and the ws are:
MyParams <- rownames(Pois1.sim$summary.fixed)
MyID <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
RowNum.Betas <- lapply(MyParams, MyID)
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas

# Start a loop to extract betas, calculate
# the fitted values and simulate count data from 
# the model.
N  <- nrow(river_indices)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xmat <- as.matrix(Covariates)


for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  eta         <- Xmat %*% Betas 
  mu.i[,i]    <- exp(eta)
  Ysim[,i]    <- rpois(n = nrow(river_indices), lambda = mu.i[,i])
}

table(Ysim[,1])
table(Ysim[,2])
table(Ysim[,3])

# Now we have 1000 simulated data sets from the model.
# What shall we do with these simulated data sets?
# We could calculate the number of zeros in each of the 1,000
# data sets.
zeros <- vector(length = NSim)
for(i in 1:NSim){
  zeros[i] <- sum(Ysim[,i] == 0)
}

table(zeros)

#Let's plot this as a table
# Figure 21.10
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(1100, 1420),
     main = "Simulation results")
points(x = sum(SA$A_tobianus == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
#The red dot is the number of zeros in the original data set.
#The data simulated from the Poisson model
#         does not contain enough zeros.


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
