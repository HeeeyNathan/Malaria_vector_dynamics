#    Highland Statistics Ltd.
#    www.highstat.com
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#######################################################

# For details, see:
# Burrow distribution of three sandeel species relates to beam trawl fishing,
# sediment composition and water velocity, in Dutch coastal waters
# Tien et al. (2017)
# http://dx.doi.org/10.1016/j.seares.2017.05.001


# https://www.youtube.com/watch?v=dWCAtiraFUo

######################################################################
#Load packages and library files
library(tidyverse)
library(janitor) # cleans names of columns
library(lattice)
library(sf) # for geographic data
library(sp) # for geographic data
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
######################################################################



######################################################################
# Import the data
# Calculated index data from rivers (2013 - 2022)
SA <- as_tibble(read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)) %>% 
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
SA <- as.data.frame(SA) # this is needed because some older code does not recocgnise tibble

names(SA)
str(SA)
head(SA)
dim(SA)
######################################################################



######################################################################
# DATA EXPLORATION
# Sampling points of all years combined
range(SA$longitude, SA$latitude)
glgmap <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57), 
                  maptype= "terrain")    
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude, 
                        latitude),
                    data = SA) 
p <- p + xlab("Longitude") + ylab("Latitude")  
p <- p + theme(text = element_text(size=15))
p

# And by year
glgmap   <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57),
                    maptype= "terrain")    
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude, 
                        latitude),
                    data = SA,
                    size = 0.5) 
p <- p + xlab("Longitude") + ylab("Latitude")  
p <- p + theme(text = element_text(size=15))
p <- p + facet_wrap( ~ fyear)
p
# Some 2018 misery?

# Data coding issues
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

SA$Xkm <- SA$Xutm / 1000
SA$Ykm <- SA$Yutm / 1000

# Outliers in the covariates
colnames(SA)
MyX <- c("year", "eqr",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m", "water_bodies_200m", "wetlands_200m",
         "ppt", "q", "tmax", "tmin", "ws")
Mydotplot(SA[, MyX])

MyX <- c("year", "eqr",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m",
         "ppt", "tmax", "tmin", "ws")
Mydotplot(SA[, MyX])

# Outliers response variable
Myvar <- c("vec_abund")
Mydotplot(SA[,Myvar])
# Lots of zeros!

# Missing values?
colSums(is.na(SA))  
100 * colSums(is.na(SA)) / nrow(SA)  
which(is.na(SA$eqr)) # observation #467
which(is.na(SA$feqc)) # observation #467

# remove observation with missing EQR data
SA <- SA[-467, ]
which(is.na(SA$eqr)) # removed :)

# How many observations do we have per year?
table(SA$fyear)
# slightly less observations in 2018

# How many observations do we have per location x year?
obvs <- table(SA$site_id, SA$fyear)
print(obvs <- cbind(obvs, total = rowSums(obvs)))

# how many sites do we have in total?
NROW(unique(SA$site_id))

# Collinearity
MyVar <- c("year", "eqr",
         "agricultural_areas_200m", "artificial_surfaces_200m", "forest_and_semi_natural_areas_200m",
         "ppt", "tmax", "tmin", "ws")
Mypairs(SA[,MyVar])
corvif(SA[,MyVar])

# We use the following set of covariates:
MyVar <- c("eqr",
           "agricultural_areas_200m", "forest_and_semi_natural_areas_200m",
           "ppt", "tmax", "tmin", "ws")
Mypairs(SA[,MyVar])
corvif(SA[,MyVar])
# We also use year.            

# Do we have collinearity within a year?
xyplot(eqr ~ agricultural_areas_200m | fyear, data = SA) # something weird in 2013
xyplot(eqr ~ forest_and_semi_natural_areas_200m | fyear, data = SA) # something weird in 2013
xyplot(eqr ~ ppt | fyear, data = SA) # something weird in 2013
xyplot(eqr ~ tmax | fyear, data = SA) # something weird in 2013
xyplot(eqr ~ tmin | fyear, data = SA) # something weird in 2013
xyplot(eqr ~ ws | fyear, data = SA) # something weird in 2013
xyplot(agricultural_areas_200m ~ forest_and_semi_natural_areas_200m | fyear, data = SA)
xyplot(agricultural_areas_200m ~ ppt | fyear, data = SA)
xyplot(agricultural_areas_200m ~ tmax | fyear, data = SA)
xyplot(agricultural_areas_200m ~ tmin | fyear, data = SA)
xyplot(agricultural_areas_200m ~ ws | fyear, data = SA)
xyplot(forest_and_semi_natural_areas_200m ~ ppt | fyear, data = SA)
xyplot(forest_and_semi_natural_areas_200m ~ tmax | fyear, data = SA)
xyplot(forest_and_semi_natural_areas_200m ~ tmin | fyear, data = SA)
xyplot(forest_and_semi_natural_areas_200m ~ ws | fyear, data = SA)
xyplot(ppt ~ tmax | fyear, data = SA)
xyplot(ppt ~ tmin | fyear, data = SA)
xyplot(ppt ~ ws | fyear, data = SA)
xyplot(tmax ~ tmin | fyear, data = SA)
xyplot(tmax ~ ws | fyear, data = SA)

# 21.2.4 Zero inflation
sum(SA$vec_abund == 0)  #Number of zeros
100 * sum(SA$vec_abund == 0) / nrow(SA)  # 63% of zeros
# That's a lot!

plot(table(SA$vec_abund), type = "h")
# We may need a model that can deal with the zeros....what about a ZAP?

# Relationships
VarX <- c("year", "eqr",
          "agricultural_areas_200m", "forest_and_semi_natural_areas_200m",
          "ppt", "tmax", "tmin", 'ws')
MyMultipanel.ggp2(SA,
                  varx = VarX,
                  vary = "vec_abund",
                  ylab = "Vector abundance",
                  addSmoother = T,
                  addRegressionLine = F,
                  addHorizontalLine = T)


p <- ggplot(SA, aes(y = log(vec_abund), x = eqr))
p <- p + geom_point() + ylab("Vector abundance") + xlab("ecological quality")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = eqr))
p <- p + geom_point() + ylab("Vector abundance") + xlab("ecological quality")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = agricultural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Agricultural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = agricultural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Agricultural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = forest_and_semi_natural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Forest and Natural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = forest_and_semi_natural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Forest and Natural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = ppt))
p <- p + geom_point() + ylab("Vector abundance") + xlab("precipitation")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = ppt))
p <- p + geom_point() + ylab("Vector abundance") + xlab("precipitation")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = tmax))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmax temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = tmax))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmax temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = tmin))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmin temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = tmin))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmin temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = ws))
p <- p + geom_point() + ylab("Vector abundance") + xlab("wind speed")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = log(vec_abund), x = ws))
p <- p + geom_point() + ylab("Vector abundance") + xlab("wind speed")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p

###############################################################


# Summary data exploration
# 1. Lots of zeros
# 2. Collinearity between some covariates
# 3. Fewer sampling locations in 2018
# 4. No clear relationships between response and covariates
# 5. Vague reasons to do a transformation on some covariates.

# We will:
#  1. Apply a Bernoulli GLM on the absence/presence data
#  2. Apply a zero-truncated Poisson on the presence-only
#     data
#  3. Combine components 1 and 2.

# Transformations
# SA$eqr.SQ <- sqrt(SA$eqr)
# SA$ppt.SQ <- sqrt(SA$ppt)
# SA$tmax.SQ <- sqrt(SA$tmax)
# SA$tmin.SQ <- sqrt(SA$tmin)
# SA$ws.SQ <- sqrt(SA$ws)


###############################################################
# Implementation Poisson GLM in R-INLA

# What are distances between sites?
Loc <- cbind(SA$Xutm, SA$Yutm)
D   <- dist(Loc)

# plot
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(D / 1000, 
     freq = TRUE,
     main = "", 
     xlab = "Distance between sites (km)",
     ylab = "Frequency")


# What is the distance for which we would expect dependency?
# 50 km?

RangeGuess <- 50 * 1000     #  50km??????
# RangeGuess <- 10 * 1000    # 10km...takes less time during testing
# USE SECOND OPTION IF YOU HAVE A SLOW COMPUTER!!
# The smaller this value the better...but the longer the computing time

# Recommended settings:
MaxEdge <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 50 * 1000)
# The convex option puts the boundary of the innerpart
# closer to the points. Saves some computing time
# during this course. Maybe not use it for a paper.
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)
meshPois$n
# 3434 mesh points... ALOT!

# plot
par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(meshPois, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)



#########################################
#  Define the weighting factors a_ik (also called the projector matrix).
APois  <- inla.spde.make.A(meshPois, loc = Loc)
############################################

############################################
# Define the SPDE.
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(5 * 1000, 0.5), 
                                prior.sigma = c(1.5, 0.01))
# This is tricky stuff. We need to specify this:
# P(Range < range0) = 0.5  and P(sigma > sigma0) = 0.01

# The range0 value is our primary tool to control the amount
# of smoothing that the spatial random field will do.
# The larger it is, the smoother the spatial random field.
# It allows to avoid overfitting.

# We decided this:
# P(Range < 5 km) = 0.001  and P(sigma > 0.5) = 0.5

##########################################



##########################################
# Define the spatial field.
wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)
#####################################



#####################################
# Make a stack. 
XYear <- model.matrix(~ fyear, data = SA)
N <- nrow(SA)
Covariates <- data.frame(
  Intercept   = rep(1, N),
  fYear2014   = XYear[,"fyear2014"],
  fYear2015   = XYear[,"fyear2015"],
  fYear2016   = XYear[,"fyear2016"],
  fYear2017   = XYear[,"fyear2017"],
  fYear2018   = XYear[,"fyear2018"],
  fYear2019   = XYear[,"fyear2019"],
  fYear2020   = XYear[,"fyear2020"],
  fYear2021   = XYear[,"fyear2021"],
  fYear2022   = XYear[,"fyear2022"],
  eqr         = MyStd(SA$eqr),
  Agriculture = MyStd(SA$agricultural_areas_200m),
  # Artificial  = MyStd(SA$artificial_surfaces_200m),
  Natural     = MyStd(SA$forest_and_semi_natural_areas_200m),
  ppt         = MyStd(SA$ppt),
  tmax        = MyStd(SA$tmax),  
  tmin        = MyStd(SA$tmin)
  # ws          = MyStd(SA$ws)
)

StackPoisGLM <- inla.stack(
  tag = "PoissonFit",
  data = list(y = SA$vec_abund),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w = wPois.index))

# We first fit models 1 and 2
fPois1 <- y ~ -1 + Intercept + 
  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
  eqr + 
  Agriculture + 
  # Artificial + 
  Natural +
  ppt + 
  tmax + 
  tmin
# + ws


# Execute a Poisson GLM model without spatial correlation
Pois1 <- inla(fPois1,
              family = "poisson", 
              #control.inla = list(strategy = "gaussian"),
              data = inla.stack.data(StackPoisGLM),
              control.compute = list(dic = TRUE, waic = TRUE),
              control.predictor = list(A = inla.stack.A(StackPoisGLM)))
summary(Pois1)

# Model validation
# Check for overdispersion (should be lower than 1)
p   <- nrow(Pois1$summary.fixed)
mu1 <- Pois1$summary.fitted.values[1:N,"mean"]
E1  <- (SA$vec_abund - mu1) / sqrt(mu1)
sum(E1^2) / (N - p)
# 179+ = Overdispersion! 


# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")


# Plot residuals versus each covariate
SA$E1 <- E1

# EQR
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = eqr, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("EQR") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = eqr, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# Agriculture
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = agricultural_areas_200m, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Agricultural areas") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = agricultural_areas_200m, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# # Artificial surfaces
# p <- ggplot()
# p <- p + geom_point(data = SA, 
#                     aes(x = artificial_surfaces_200m, y = E1),
#                     shape = 1, 
#                     size = 1)
# p <- p + xlab("Artificial surfaces") + ylab("Pearson residuals")
# p <- p + theme(text = element_text(size=15)) 
# p <- p + geom_smooth(data = SA, 
#                      aes(x = artificial_surfaces_200m, y = E1))
# p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
# p <- p + geom_hline(yintercept=0, linetype="dashed")
# p

# Forest and Natural land
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = forest_and_semi_natural_areas_200m, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Forest and semi-narural areas") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = forest_and_semi_natural_areas_200m, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# Precipitation
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = ppt, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Precipitation") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = ppt, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# Maximum temperatures
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = tmax, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Tmax") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = tmax, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# Minimum temperatures
p <- ggplot()
p <- p + geom_point(data = SA, 
                    aes(x = tmin, y = E1),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Tmin") + ylab("Pearson residuals")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_smooth(data = SA, 
                     aes(x = tmin, y = E1))
p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
p <- p + geom_hline(yintercept=0, linetype="dashed")
p

# # Wind speed
# p <- ggplot()
# p <- p + geom_point(data = SA, 
#                     aes(x = ws, y = E1),
#                     shape = 1, 
#                     size = 1)
# p <- p + xlab("Wind speed") + ylab("Pearson residuals")
# p <- p + theme(text = element_text(size=15)) 
# p <- p + geom_smooth(data = SA, 
#                      aes(x = ws, y = E1))
# p <- p + facet_wrap( ~ fyear, ncol = 5, scale = "free_y")
# p <- p + geom_hline(yintercept=0, linetype="dashed")
# p

# Difficult to assess. Let's use mgcv for this
library(mgcv)
colnames(SA)

# eqr
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(eqr), data = SA)
T2 <- gam(E1 ~ s(eqr, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best

# plot
par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = SA$eqr, 
     y = SA$E1,
     xlab = "eqr",
     ylab = "Pearson residuals")
text(0.2, 295, "A", cex = 1.5)

plot(T1)
abline(h = 0, lty = 2)
text(0.2, 3.1, "B", cex = 1.5)

# eqr
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(eqr), data = SA)
T2 <- gam(E1 ~ s(eqr, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# agricultural_areas_200m
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(agricultural_areas_200m), data = SA)
T2 <- gam(E1 ~ s(agricultural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # artificial_surfaces_200m
# T0 <- gam(E1 ~ 1, data = SA)
# T1 <- gam(E1 ~ s(artificial_surfaces_200m), data = SA)
# T2 <- gam(E1 ~ s(artificial_surfaces_200m, by = fyear), data = SA)
# AIC(T0, T1, T2) # T2 is best
# summary(T2)
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# forest_and_semi_natural_areas_200m
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(forest_and_semi_natural_areas_200m), data = SA)
T2 <- gam(E1 ~ s(forest_and_semi_natural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# ppt
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(ppt), data = SA)
T2 <- gam(E1 ~ s(ppt, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# tmax
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(tmax), data = SA)
T2 <- gam(E1 ~ s(tmax, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# tmin
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(tmin), data = SA)
T2 <- gam(E1 ~ s(tmin, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is best
summary(T2)
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # ws
# T0 <- gam(E1 ~ 1, data = SA)
# T1 <- gam(E1 ~ s(ws), data = SA)
# T2 <- gam(E1 ~ s(ws, by = fyear), data = SA)
# AIC(T0, T1, T2) # T0 is best
# summary(T2)
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# summary
#   eqr             <- some non-linearity, but not consistent. Deviance explained: 2.54% of the variation in the residuals! T2 == BEST!
#   agriculture     <- interesting patterns. Some non-linearity, but not consistent. 2022 is interesting. Deviance explained: 5.55% of the variation in the residuals! T2 == BEST!
#   artificial      <- no strong relationships. Not really non-linear. Deviance explained: 3.24% of the variation in the residuals! T2 == BEST!
#   forest          <- interesting patterns. Some non-linearity, but not consistent. 2022 is interesting. Deviance explained: 8.92% of the variation in the residuals! T2 == BEST!
#   ppt             <- no strong relationships. Some non-linearity but not consistent. Deviance explained: 4.05% of the variation in the residuals! T2 == BEST!
#   tmax            <- Interesting. Some non-linearity but not consistent. Deviance explained: 19.4% of the variation in the residuals! T2 == BEST!
#   tmin            <- Interesting. Some non-linearity but not consistent. Deviance explained: 6.51% of the variation in the residuals! T2 == BEST!
#   ws              <- interesting patterns. Some non-linearity, but not consistent. Deviance explained: 1.27% of the variation in the residuals! T0 == BEST!

# new summary 
#   eqr relatively linear... keep it as it is.
#   agriculture changes per year and some non-linearly. 2022 is interesting... smoother?
#   artificial relatively linear... keep it as it is.
#   forest changes per year and some non-linearly. 2022 is interesting... smoother?
#   precipitation relatively linear, but with some non-linearilty... keep it as it is.
#   tmax relatively linear, but with some non-linearilty... keep it as it is.
#   tmin relatively linear, but with some non-linearilty... keep it as it is.
#   ws relatively linear... keep it as it is.

# Conclusion:
#  We maybe need need smoothers for forest areas and tmax
#  And these smoothers need to change by year.


# What about spatial correlation in the residuals?
# Check the Pearson residuals of I1 and I2 for
# any spatial dependency using a
# variogram.
MyData1 <- data.frame(E1 = E1, 
                      Xkm = SA$Xkm, 
                      Ykm = SA$Ykm)
coordinates(MyData1) <- c("Xkm", "Ykm")
V1 <- variogram(E1 ~ 1, 
                MyData1, 
                cressie = TRUE, 
                cutoff = 50)

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = V1$dist,
     y = V1$gamma,
     xlab = "Distance",
     ylab = "Sample variogram",
     ylim = c(0,max(V1$gamma)),
     pch = 16)
# Some spatial correlation in the residuals 


# Simulation study for zero-inflation
Pois1.sim <- inla(fPois1,
                  family = "poisson", 
                  data=inla.stack.data(StackPoisGLM),
                  control.compute = list(config = TRUE),
                  control.predictor = list(A = inla.stack.A(StackPoisGLM)))

# Simulate 1000 sets of betas and ws from the model:
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = Pois1.sim)
View(SimData[[1]]$latent)

# Determine on which rows the betas and the ws are:
(MyParams <- rownames(Pois1.sim$summary.fixed))
MyID <- function(x) { which(grepl(x, rownames(SimData[[1]]$latent), fixed = TRUE)) }
RowNum.Betas <- lapply(MyParams, MyID)
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas

# Start a loop to extract betas, calculate
# the fitted values and simulate count data from 
# the model.
N  <- nrow(SA)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xmat <- as.matrix(Covariates)

NROW(RowNum.Betas)
ncol(Xmat)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  eta         <- Xmat %*% Betas 
  mu.i[,i]    <- exp(eta)
  Ysim[,i]    <- rpois(n = nrow(SA), lambda = mu.i[,i])
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
     xlim = c(0, 1000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the Poisson model does not contain enough zeros.

###############################################################




###############################################################
# NOW FIT A POISSON GAM!!
# G1: Agriculture + forest + tmax smoothers




##########################################
# Implementation Poisson GAM in R-INLA
# Get basis functions for each smoother
library(mgcv)
colnames(SA)
BasisAgri <- smoothCon(s(agricultural_areas_200m, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

BasisNatu <- smoothCon(s(forest_and_semi_natural_areas_200m, k = 5, fx = TRUE),
                       data = SA,
                       knots = NULL, absorb.cons = TRUE)[[1]]$X
 
BasisTmax <- smoothCon(s(tmax, k = 5, fx = TRUE),
                       data = SA,
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

colnames(BasisAgri) <- paste("Agri", 1:4, sep = "")
colnames(BasisNatu) <- paste("Natu", 1:4, sep = "")
colnames(BasisTmax) <- paste("Tmax", 1:4, sep = "")

Covariates <- data.frame(
  Intercept   = rep(1, N),
  fYear2014   = XYear[,"fyear2014"],
  fYear2015   = XYear[,"fyear2015"],
  fYear2016   = XYear[,"fyear2016"],
  fYear2017   = XYear[,"fyear2017"],
  fYear2018   = XYear[,"fyear2018"],
  fYear2019   = XYear[,"fyear2019"],
  fYear2020   = XYear[,"fyear2020"],
  fYear2021   = XYear[,"fyear2021"],
  fYear2022   = XYear[,"fyear2022"],
  eqr         = MyStd(SA$eqr),
  # Agricultural= MyStd(SA$agricultural_areas_200m),
  # Artificial  = MyStd(SA$artificial_surfaces_200m),
  # Natural     = MyStd(SA$forest_and_semi_natural_areas_200m),
  ppt         = MyStd(SA$ppt),
  # tmax        = MyStd(SA$tmax),
  tmin        = MyStd(SA$tmin),
  # ws          = MyStd(SA$ws), 
  Tmax1       = BasisTmax[,"Tmax1"],
  Tmax2       = BasisTmax[,"Tmax2"],
  Tmax3       = BasisTmax[,"Tmax3"],
  Tmax4       = BasisTmax[,"Tmax4"],
  Natu1       = BasisNatu[,"Natu1"],
  Natu2       = BasisNatu[,"Natu2"],
  Natu3       = BasisNatu[,"Natu3"],
  Natu4       = BasisNatu[,"Natu4"],
  Agri1       = BasisAgri[,"Agri1"],
  Agri2       = BasisAgri[,"Agri2"],
  Agri3       = BasisAgri[,"Agri3"],
  Agri4       = BasisAgri[,"Agri4"]
)

lcs.Tmax <- inla.make.lincombs(Tmax1 = BasisTmax[,"Tmax1"],
                               Tmax2 = BasisTmax[,"Tmax2"],
                               Tmax3 = BasisTmax[,"Tmax3"],
                               Tmax4 = BasisTmax[,"Tmax4"])

lcs.Natu <- inla.make.lincombs(Natu1 = BasisNatu[,"Natu1"],
                               Natu2 = BasisNatu[,"Natu2"],
                               Natu3 = BasisNatu[,"Natu3"],
                               Natu4 = BasisNatu[,"Natu4"])

lcs.Agri <- inla.make.lincombs(Agri1 = BasisAgri[,"Agri1"],
                               Agri2 = BasisAgri[,"Agri2"],
                               Agri3 = BasisAgri[,"Agri3"],
                               Agri4 = BasisAgri[,"Agri4"])

names(lcs.Tmax)   <- paste(names(lcs.Tmax), "Tmax", sep = "")
names(lcs.Natu)   <- paste(names(lcs.Natu), "Natu", sep = "")
names(lcs.Agri)   <- paste(names(lcs.Agri), "Agri", sep = "")

All.lcs <- c(
  lcs.Tmax,
  lcs.Natu,
  lcs.Agri
  )

(N <- nrow(SA))
Stack <- inla.stack(
  tag = "Fit",
  data = list(y = SA$vec_abund),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w          = wPois.index))

# Define model
fGam1 <- formula(y ~ -1 + Intercept + 
                   fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                   eqr + 
                   # Agriculture +
                   # Artificial +
                   # Natural +
                   ppt + 
                   # tmax + 
                   tmin + 
                   # ws +
                   Tmax1 + Tmax2 + Tmax3 + Tmax4 +
                   Natu1 + Natu2 + Natu3 + Natu4 +
                   Agri1 + Agri2 + Agri3 + Agri4
                   )

# Execute a Bernoulli GLM model without spatial correlation
G1 <- inla(fGam1,
           family = "poisson", 
           lincomb = All.lcs,
           # control.inla = list(strategy = "gaussian"),
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))

#### Extract smoothers
(Ns <- nrow(SA))
LinComb <- G1$summary.lincomb.derived

f.Tmax    <- LinComb[1:Ns + 0 * Ns, "mean"]
SeLo.Tmax <- LinComb[1:Ns + 0 * Ns,"0.025quant"]
SeUp.Tmax <- LinComb[1:Ns + 0 * Ns,"0.975quant"]

f.Natu    <- LinComb[1:Ns + 1 * Ns, "mean"]
SeLo.Natu <- LinComb[1:Ns + 1 * Ns,"0.025quant"]
SeUp.Natu <- LinComb[1:Ns + 1 * Ns,"0.975quant"]

f.Agri    <- LinComb[1:Ns + 2 * Ns, "mean"] 
SeLo.Agri <- LinComb[1:Ns + 2 * Ns,"0.025quant"] 
SeUp.Agri <- LinComb[1:Ns + 2 * Ns,"0.975quant"]

OTmax <- order(SA$tmax)
ONatu <- order(SA$forest_and_semi_natural_areas_200m)
OAgri <- order(SA$agricultural_areas_200m)

MyData <- data.frame(
  mu   = c(
    f.Tmax[OTmax],
    f.Natu[ONatu],
    f.Agri[OAgri]), 
  SeUp = c(
    SeUp.Tmax[OTmax],
    SeUp.Natu[ONatu],
    SeUp.Agri[OAgri]), 
  SeLo = c(
    SeLo.Tmax[OTmax],
    SeLo.Natu[ONatu],
    SeLo.Agri[OAgri]), 
  Xaxis = c(
    sort(SA$tmax), 
    sort(SA$forest_and_semi_natural_areas_200m),
    sort(SA$agricultural_areas_200m)),
  ID    = factor(rep(c(
    "Tmax smoother",
    "Forest & natural areas smoother",
    "Agricultural areas smoother"), 
                     each = nrow(SA))))
View(MyData)

# Plotting the smoothers with the scale of the predictors
p <- ggplot()
p <- p + xlab("Covariate") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p <- p + facet_wrap(~ID, scales = "free", ncol = 3)                     
p

my.ggp.yrange <- c(0, 0, 0)
XPos <- c(
  SA$tmax, 
  SA$forest_and_semi_natural_areas_200m,
  SA$agricultural_areas_200m)
XID  <- rep(c(
  "Tmax smoother",
  "Forest & natural areas smoother",
  "Agricultural areas smoother"
  ), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p

# Plotting the predictors in the scale of the original data
MyData_exp <- data.frame(
  mu   = c(   
    exp(f.Tmax[OTmax]), 
    exp(f.Natu[ONatu]), 
    exp(f.Agri[OAgri])), 
  SeUp = c(
    exp(SeUp.Tmax[OTmax]),
    exp(SeUp.Natu[ONatu]),
    exp(SeUp.Agri[OAgri])), 
  SeLo = c(
    exp(SeLo.Tmax[OTmax]), 
    exp(SeLo.Natu[ONatu]), 
    exp(SeLo.Agri[OAgri])), 
  Xaxis = c(
    sort(SA$tmax), 
    sort(SA$forest_and_semi_natural_areas_200m),
    sort(SA$agricultural_areas_200m)),
  ID    = factor(rep(c(
    "Tmax smoother",
    "Forest & natural areas smoother",
    "Agricultural areas smoother"), 
    each = nrow(SA))))
View(MyData_exp)

p <- ggplot()
p <- p + xlab("Covariate") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData_exp, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData_exp, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p <- p + facet_wrap(~ID, scales = "free", ncol = 3)                     
p

my.ggp.yrange <- c(0, 0, 0)
XPos <- c(
  SA$tmax, 
  SA$forest_and_semi_natural_areas_200m,
  SA$agricultural_areas_200m)
XID  <- rep(c(
  "Tmax smoother",
  "Forest & natural areas smoother",
  "Agricultural areas smoother"
), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p


# Check for overdispersion
p   <- nrow(G1$summary.fixed)  #Not entirely correct!!
mu1 <- G1$summary.fitted.values[1:N,"mean"]
E1  <- (SA$vec_abund - mu1) / sqrt(mu1)
sum(E1^2) / (N - p)
# At 143, there is still overdispersion!

# Apply model validation:
# Plot Pearson residuals vs fitted values
# Not in the book:
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")


# Plot residuals versus each covariate
colnames(SA)
# Difficult to assess. Let's use mgcv for this
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(eqr), data = SA)
T2 <- gam(E1 ~ s(eqr, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# Agriculture
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(agricultural_areas_200m), data = SA)
T2 <- gam(E1 ~ s(agricultural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # Artificial
# T0 <- gam(E1 ~ 1, data = SA)
# T1 <- gam(E1 ~ s(artificial_surfaces_200m), data = SA)
# T2 <- gam(E1 ~ s(artificial_surfaces_200m, by = fyear), data = SA)
# AIC(T0, T1, T2) # T0 is the best!
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# Forest and Natural
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(forest_and_semi_natural_areas_200m), data = SA)
T2 <- gam(E1 ~ s(forest_and_semi_natural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# PPT
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(ppt), data = SA)
T2 <- gam(E1 ~ s(ppt, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# TMAX
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(tmax), data = SA)
T2 <- gam(E1 ~ s(tmax, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# TMIN
T0 <- gam(E1 ~ 1, data = SA)
T1 <- gam(E1 ~ s(tmin), data = SA)
T2 <- gam(E1 ~ s(tmin, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # WS
# T0 <- gam(E1 ~ 1, data = SA)
# T1 <- gam(E1 ~ s(ws), data = SA)
# T2 <- gam(E1 ~ s(ws, by = fyear), data = SA)
# AIC(T0, T1, T2) # T0 is the best!
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# What about spatial correlation in the residuals?
# Check the Pearson residuals of I1 and I2 for
# any spatial dependency using a variogram.
MyData1 <- data.frame(E1 = E1, 
                      Xkm = SA$Xkm, 
                      Ykm = SA$Ykm)
coordinates(MyData1) <- c("Xkm", "Ykm")
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = V1$dist,
     y = V1$gamma,
     xlab = "Distance",
     ylab = "Sample variogram",
     ylim = c(0,max(V1$gamma)),
     pch = 16)
# Some spatial correlation in the residuals 
# We could do this again with residuals per year (resulting
# in 10 variograms)

# Simulation study for zeros
G1.sim <- inla(fGam1,
               family = "poisson", 
               data=inla.stack.data(Stack),
               control.compute = list(config = TRUE),
               control.predictor = list(A = inla.stack.A(Stack)))


# Simulate 1000 sets of betas and ws from the model:
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = G1.sim)


# Determine on which rows the betas and the ws are:
(MyParams <- rownames(G1.sim$summary.fixed))
# MyID <- function(x){ which(rownames(SimData[[1]]$latent) == x) } # does not work
MyID <- function(x) { which(grepl(x, rownames(SimData[[1]]$latent), fixed = TRUE)) }
RowNum.Betas <- lapply(MyParams, MyID)
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas

# Start a loop to extract betas, calculate
# the fitted values and simulate count data from 
# the model.
N  <- nrow(SA)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xmat <- as.matrix(Covariates)

NROW(RowNum.Betas)
ncol(Xmat)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[RowNum.Betas]
  eta         <- Xmat %*% Betas 
  mu.i[,i]    <- exp(eta)
  Ysim[,i]    <- rpois(n = nrow(SA), lambda = mu.i[,i])
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

# Not in the book:
#Let's plot this as a table
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(0, 1000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
#The red dot is the number of zeros in the original data set.
#The data simulated from the Poisson model does not contain enough zeros.
##########################################






####################################################
# Poisson GAM with space/time dependency

fGam2 <- formula(y ~ -1 + Intercept + 
          fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
          eqr + 
          # Agriculture +
          # Artificial +
          # Natural +
          ppt + 
          # tmax + 
          tmin + 
          # ws +
          Tmax1 + Tmax2 + Tmax3 + Tmax4 +
          Natu1 + Natu2 + Natu3 + Natu4 +
          Agri1 + Agri2 + Agri3 + Agri4 +
          f(w, model = spdePois)
)

G2 <- inla(fGam2,
           lincomb = All.lcs,
           family = "poisson", 
           #control.inla = list(strategy = "gaussian"),
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))
summary(G2)

# Compare the Poisson GAM & Poisson GLN + spatial correlation
dic  <- c(Pois1$dic$dic, G1$dic$dic, G2$dic$dic)   
waic <- c(Pois1$waic$waic, G1$waic$waic, G2$waic$waic)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Poisson GLM",  
                     "Poisson GAM",
                     "Poisson GAM + SRF")
Z.out
# The GAM is better than the GLM.
# The model with spatial correlation is much (!) better!



################################################
# Again..there is a whole block of R code that is not
# in the book. This code plots the smoothers and applies
# model validation.

#### Extract smoothers G2
Ns <- nrow(SA)
f.Tmax    <- G2$summary.lincomb.derived[1:Ns + 0 * Ns, "mean"] 
SeLo.Tmax <- G2$summary.lincomb.derived[1:Ns + 0 * Ns,"0.025quant"] 
SeUp.Tmax <- G2$summary.lincomb.derived[1:Ns + 0 * Ns,"0.975quant"]

f.Natu   <- G2$summary.lincomb.derived[1:Ns + 1 * Ns, "mean"] 
SeLo.Natu <- G2$summary.lincomb.derived[1:Ns + 1 * Ns,"0.025quant"] 
SeUp.Natu <- G2$summary.lincomb.derived[1:Ns + 1 * Ns,"0.975quant"]

f.Agri    <- G2$summary.lincomb.derived[1:Ns + 2 * Ns, "mean"] 
SeLo.Agri <- G2$summary.lincomb.derived[1:Ns + 2 * Ns,"0.025quant"] 
SeUp.Agri <- G2$summary.lincomb.derived[1:Ns + 2 * Ns,"0.975quant"]

OTmax <- order(SA$tmax)
ONatu <- order(SA$forest_and_semi_natural_areas_200m)
OAgri <- order(SA$agricultural_areas_200m)

MyData <- data.frame(
  mu   = c(
    f.Tmax[OTmax],
    f.Natu[ONatu],    
    f.Agri[OAgri]), 
  SeUp = c(
    SeUp.Tmax[OTmax], 
    SeUp.Natu[ONatu], 
    SeUp.Agri[OAgri]), 
  SeLo = c(
    SeLo.Tmax[OTmax], 
    SeLo.Natu[ONatu], 
    SeLo.Agri[OAgri]), 
  Xaxis = c(
    sort(SA$tmax), 
    sort(SA$forest_and_semi_natural_areas_200m), 
    sort(SA$agricultural_areas_200m)),
  ID    = factor(rep(c(
    "Tmax smoother",
    "Forest and natural areas smoother", 
    "Agriculture smoother"
    ), each = nrow(SA))))

p <- ggplot()
p <- p + xlab("Covariate") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p <- p + facet_wrap(~ID, scales = "free", ncol = 3)                     
p

my.ggp.yrange <- c(0, 0, 0)
XPos <- c(
  SA$tmax, 
  SA$forest_and_semi_natural_areas_200m, 
  SA$agricultural_areas_200m)
XID  <- rep(c(
  "Tmax smoother",
  "Forest and natural areas smoother",  
  "Agriculture smoother"
  ), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p


# Short model validation G2
mu2 <- G2$summary.fitted.values[1:N,"mean"]
E2  <- (SA$vec_abund - mu2) / sqrt(mu2)


# Plot Pearson residuals vs fitted values
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2,
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")


# Plot residuals vs observed data
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2,
     y = SA$vec_abund,
     xlab = "Fitted values model 2",
     ylab = "Observed data")

# Check for overdispersion
p   <- nrow(G1$summary.fixed)  #Not entirely correct!!
mu2 <- G2$summary.fitted.values[1:N,"mean"]
E2  <- (SA$vec_abund - mu2) / sqrt(mu2)
sum(E2^2) / (N - p)
# At 17, there is still overdispersion! VERY CLOSE!
# And it is quite sloppy to use the same variable names as for the Poisson GLM.

# Apply model validation:
# Plot Pearson residuals vs fitted values
# Not in the book:
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2,
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")


# Plot residuals versus each covariate
colnames(SA)
# Difficult to assess. Let's use mgcv for this
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(eqr), data = SA)
T2 <- gam(E2 ~ s(eqr, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# Agriculture
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(agricultural_areas_200m), data = SA)
T2 <- gam(E2 ~ s(agricultural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # Artificial
# T0 <- gam(E2 ~ 1, data = SA)
# T1 <- gam(E2 ~ s(artificial_surfaces_200m), data = SA)
# T2 <- gam(E2 ~ s(artificial_surfaces_200m, by = fyear), data = SA)
# AIC(T0, T1, T2) # T0 is the best!
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# Forest and Natural
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(forest_and_semi_natural_areas_200m), data = SA)
T2 <- gam(E2 ~ s(forest_and_semi_natural_areas_200m, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# PPT
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(ppt), data = SA)
T2 <- gam(E2 ~ s(ppt, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# TMAX
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(tmax), data = SA)
T2 <- gam(E2 ~ s(tmax, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# TMIN
T0 <- gam(E2 ~ 1, data = SA)
T1 <- gam(E2 ~ s(tmin), data = SA)
T2 <- gam(E2 ~ s(tmin, by = fyear), data = SA)
AIC(T0, T1, T2) # T2 is the best!
# plot
par(mfrow = c(5,2))
plot(T2, scale = FALSE)

# # WS
# T0 <- gam(E2 ~ 1, data = SA)
# T1 <- gam(E2 ~ s(ws), data = SA)
# T2 <- gam(E2 ~ s(ws, by = fyear), data = SA)
# AIC(T0, T1, T2) # T0 is the best!
# # plot
# par(mfrow = c(5,2))
# plot(T2, scale = FALSE)

# What about spatial correlation in the residuals?
# Check the Pearson residuals of I1 and I2 for
# any spatial dependency using a variogram.
MyData1 <- data.frame(E1 = E1, 
                      Xkm = SA$Xkm, 
                      Ykm = SA$Ykm)
coordinates(MyData1) <- c("Xkm", "Ykm")
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


# What about spatial correlation in the residuals?
# Check the Pearson residuals of I1 and I2 for
# any spatial dependency using a
# variogram.

MyData2 <- data.frame(E2 = E2, 
                      Xkm = SA$Xkm, 
                      Ykm = SA$Ykm)
coordinates(MyData2) <- c("Xkm", "Ykm")
V2 <- variogram(E2 ~ 1, 
                MyData2, 
                cressie = TRUE, 
                cutoff = 50)
plot(V2, col = "black", pch = 19)


par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = V2$dist,
     y = V2$gamma,
     xlab = "Distance",
     ylab = "Sample variogram",
     ylim = c(0,max(V2$gamma)),
     pch = 16)
# Some spatial correlation in the residuals 
# We could do this again with residuals per year (resulting
# in 10 variograms)

G2.sim <- inla(fGam2,
               family = "poisson", 
               data=inla.stack.data(Stack),
               control.compute = list(config = TRUE),
               control.predictor = list(A = inla.stack.A(Stack)))


# Simulate 1000 sets of betas and ws from the model:
NSim <- 1000
SimData2 <- inla.posterior.sample(n = NSim, result = G2.sim)


# Determine on which rows the betas and the ws are:
MyParams <- rownames(G2.sim$summary.fixed)
# MyID <- function(x){ which(rownames(SimData2[[1]]$latent) == x) }
MyID <- function(x) { which(grepl(x, rownames(SimData[[1]]$latent), fixed = TRUE)) }
RowNum.Betas <- lapply(MyParams, MyID)
RowNum.Betas <- as.numeric(RowNum.Betas)
RowNum.Betas


N1 <- meshPois$n
MyParams <- paste("w:", 0:N1, sep = "")
# check data
print(N1)
NROW(MyParams)
MyID <- function(x){ which(rownames(SimData2[[1]]$latent) == x) }
# extract data
RowNum.w <- lapply(MyParams, MyID)
RowNum.w <- unlist(RowNum.w)
RowNum.w

# Start a loop to extract betas, calculate
# the fitted values and simulate count data from 
# the model.
N  <- nrow(SA)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xmat <- as.matrix(Covariates)
A    <- as.matrix(APois)

NROW(RowNum.Betas)
ncol(Xmat)

for (i in 1: NSim){
  Betas <- SimData2[[i]]$latent[RowNum.Betas]
  wi    <- SimData2[[i]]$latent[RowNum.w] 
  eta   <- Xmat %*% Betas + A %*% wi
  mu.i[,i]    <- exp(eta)
  Ysim[,i]    <- rpois(n = nrow(SA), lambda = mu.i[,i])
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
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(0, 1420),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
#The red dot is the number of zeros in the original data set.
#Success!!! The data simulated from the Poisson model contains enough zeros.

# END OF R CODE BLOCK NOT IN THE BOOK
################################################



#############################################
# Poisson GAM with spatial-temporal correlation

#######################
# Add a model with spatial-temporal correlation
# using the replicate correlation.
Repl <- as.numeric(SA$fyear) 
table(Repl)
NRepl <- length(unique(Repl))
NRepl #Number of replicated random fields we will get


# Define the weight factors aik
A.PoisRepl <- inla.spde.make.A(meshPois, 
                               loc = Loc, 
                               repl = Repl)

# Define the SPDE (same as before)
spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(5 * 1000, 0.5), 
                                     prior.sigma = c(1.5, 0.01))

# Smoother SRF:
Range0      <- 5 * 1000  #I think we use different values in the book
AlphaRange0 <- 0.0001   #I think we use different values in the book 
Sigma0      <- 0.5        #I think we use different values in the book
AlphaSigma0 <- 0.5  #I think we use different values in the book
spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(Range0, AlphaRange0), 
                                     prior.sigma = c(Sigma0, AlphaSigma0))


# Define the spatial field
wRepl.index <- inla.spde.make.index('w', 
                                    n.spde = meshPois$n,
                                    n.repl = NRepl)


StackPoisRepl <- inla.stack(
  tag = "Fit",
  data = list(y = SA$vec_abund),  
  A = list(1, A.PoisRepl),                  
  effects = list(   
    Covariates = Covariates,
    w          = wRepl.index))


fGam3 <- y ~ -1 + Intercept + 
  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
  eqr + 
  # Agriculture +
  # Artificial +
  # Natural +
  ppt + 
  # tmax + 
  tmin + 
  # ws +
  Tmax1 + Tmax2 + Tmax3 + Tmax4 +
  Natu1 + Natu2 + Natu3 + Natu4 +
  Agri1 + Agri2 + Agri3 + Agri4 +
  f(w, model = spde.PoisRepl, replicate = w.repl)

# And run the model with the spatial-temporal random field
G3 <- inla(fGam3,
           family = "poisson", 
           lincomb = All.lcs,
           data=inla.stack.data(StackPoisRepl),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(StackPoisRepl)))


# And compare the models with DICs and WAICs
dic  <- c(Pois1$dic$dic,
          G1$dic$dic, G2$dic$dic, G3$dic$dic)   
waic <- c(Pois1$waic$waic,
          G1$waic$waic, G2$waic$waic, G3$waic$waic)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Poisson GLM",  
                     "Poisson GAM",  
                     "Poisson GAM + SRF",
                     "Poisson GAM + spat.temp RF")
Z.out  



##########
# Plot smoothers model G3
Ns <- nrow(SA)
f.Tmax    <- G3$summary.lincomb.derived[1:Ns + 0 * Ns, "mean"] 
SeLo.Tmax <- G3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.025quant"] 
SeUp.Tmax <- G3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.975quant"]

f.Natu   <- G3$summary.lincomb.derived[1:Ns + 1 * Ns, "mean"] 
SeLo.Natu <- G3$summary.lincomb.derived[1:Ns + 1 * Ns,"0.025quant"] 
SeUp.Natu <- G3$summary.lincomb.derived[1:Ns + 1 * Ns,"0.975quant"]

f.Agri    <- G3$summary.lincomb.derived[1:Ns + 2 * Ns, "mean"] 
SeLo.Agri <- G3$summary.lincomb.derived[1:Ns + 2 * Ns,"0.025quant"] 
SeUp.Agri <- G3$summary.lincomb.derived[1:Ns + 2 * Ns,"0.975quant"]

OTmax <- order(SA$tmax)
ONatu <- order(SA$forest_and_semi_natural_areas_200m)
OAgri <- order(SA$agricultural_areas_200m)

MyData <- data.frame(
  mu   = c(
    f.Tmax[OTmax],
    f.Natu[ONatu],    
    f.Agri[OAgri]), 
  SeUp = c(
    SeUp.Tmax[OTmax], 
    SeUp.Natu[ONatu], 
    SeUp.Agri[OAgri]), 
  SeLo = c(
    SeLo.Tmax[OTmax], 
    SeLo.Natu[ONatu], 
    SeLo.Agri[OAgri]), 
  Xaxis = c(
    sort(SA$tmax), 
    sort(SA$forest_and_semi_natural_areas_200m), 
    sort(SA$agricultural_areas_200m)),
  ID    = factor(rep(c(
    "Tmax smoother",
    "Forest and natural areas smoother", 
    "Agriculture smoother"
  ), each = nrow(SA))))


p <- ggplot()
p <- p + xlab("Covariate") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p <- p + facet_wrap(~ID, scales = "free", ncol = 3)                     
p

my.ggp.yrange <- c(0, 0, 0)
XPos <- c(
  SA$tmax, 
  SA$forest_and_semi_natural_areas_200m, 
  SA$agricultural_areas_200m)
XID  <- rep(c(
  "Tmax smoother",
  "Forest and natural areas smoother",  
  "Agriculture smoother"
), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p


###############################################
# 21.9 Bernoulli part of the ZAP GAM

# This is from the Poisson section
Loc        <- cbind(SA$Xutm, SA$Yutm)
RangeGuess <- 5 * 1000     #  5km...Use for paper
MaxEdge    <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex=-0.05)
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)

APois    <- inla.spde.make.A(meshPois, loc = Loc)
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(5 * 1000, 0.0001), 
                                prior.sigma = c(.5, 0.05))

wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)


# This is from the GAM section
library(mgcv)
BasisTmax <- smoothCon(s(tmax, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

BasisNatu <- smoothCon(s(forest_and_semi_natural_areas_200m, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

BasisAgri <- smoothCon(s(agricultural_areas_200m, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

colnames(BasisTmax) <- paste("Tmax", 1:4, sep = "")
colnames(BasisNatu) <- paste("Natu", 1:4, sep = "")
colnames(BasisAgri) <- paste("Agri", 1:4, sep = "")

MyVar <- c( "eqr", 
            "agricultural_areas_200m", "forest_and_semi_natural_areas_200m",
            "ppt", "tmax", "tmin")

Covariates <- data.frame(
  Intercept   = rep(1, N),
  fYear2014   = XYear[,"fyear2014"],
  fYear2015   = XYear[,"fyear2015"],
  fYear2016   = XYear[,"fyear2016"],
  fYear2017   = XYear[,"fyear2017"],
  fYear2018   = XYear[,"fyear2018"],
  fYear2019   = XYear[,"fyear2019"],
  fYear2020   = XYear[,"fyear2020"],
  fYear2021   = XYear[,"fyear2021"],
  fYear2022   = XYear[,"fyear2022"],
  eqr         = MyStd(SA$eqr),
  ppt         = MyStd(SA$ppt),
  tmin        = MyStd(SA$tmin),
  Tmax1       = BasisTmax[,"Tmax1"],
  Tmax2       = BasisTmax[,"Tmax2"],
  Tmax3       = BasisTmax[,"Tmax3"],
  Tmax4       = BasisTmax[,"Tmax4"],
  Natu1       = BasisNatu[,"Natu1"],
  Natu2       = BasisNatu[,"Natu2"],
  Natu3       = BasisNatu[,"Natu3"],
  Natu4       = BasisNatu[,"Natu4"],
  Agri1       = BasisAgri[,"Agri1"],
  Agri2       = BasisAgri[,"Agri2"],
  Agri3       = BasisAgri[,"Agri3"],
  Agri4       = BasisAgri[,"Agri4"]
)

lcs.Agri <- inla.make.lincombs(Agri1 = BasisAgri[, "Agri1"], 
                               Agri2 = BasisAgri[, "Agri2"],
                               Agri3 = BasisAgri[, "Agri3"],
                               Agri4 = BasisAgri[, "Agri4"])

lcs.Natu <- inla.make.lincombs(Natu1 = BasisNatu[, "Natu1"], 
                               Natu2 = BasisNatu[, "Natu2"],
                               Natu3 = BasisNatu[, "Natu3"],
                               Natu4 = BasisNatu[, "Natu4"])

lcs.Tmax <- inla.make.lincombs(Tmax1 = BasisTmax[, "Tmax1"], 
                               Tmax2 = BasisTmax[, "Tmax2"],
                               Tmax3 = BasisTmax[, "Tmax3"],
                               Tmax4 = BasisTmax[, "Tmax4"])

names(lcs.Agri)   <- paste(names(lcs.Agri), "Agri", sep = "")
names(lcs.Natu)   <- paste(names(lcs.Natu), "Natu", sep = "")
names(lcs.Tmax)   <- paste(names(lcs.Tmax), "Tmax", sep = "")
All.lcs <- c(lcs.Agri, lcs.Natu, lcs.Tmax)

SA$vec_abund_pa <- ifelse(SA$vec_abund==0, 0, 1)

N <- nrow(SA)
Stack <- inla.stack(
  tag = "BernoulliFit",
  data = list(y = SA$vec_abund_pa),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w          = wPois.index))

# Define model
fGam1 <- formula(y ~ -1 + Intercept + 
                   fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                   eqr + ppt + tmin + 
                   Tmax1 + Tmax2 + Tmax3 + Tmax4 +
                   Natu1 + Natu2 + Natu3 + Natu4 +
                   Agri1 + Agri2 + Agri3 + Agri4)

fGam2 <-formula(y ~ -1 + Intercept + 
                  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                  eqr + ppt + tmin + 
                  Tmax1 + Tmax2 + Tmax3 + Tmax4 +
                  Natu1 + Natu2 + Natu3 + Natu4 +
                  Agri1 + Agri2 + Agri3 + Agri4 +
                  f(w, model = spdePois))



# Execute a Bernoulli GAM model without spatial correlation
B1 <- inla(fGam1,
           family = "binomial", 
           lincomb = All.lcs,
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))

B2 <- inla(fGam2,
           lincomb = All.lcs,
           family = "binomial", 
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))

# Copied from the spatial-temporal Poisson GAM
Repl <- as.numeric(SA$fyear) 
NRepl <- length(unique(Repl))

A.PoisRepl <- inla.spde.make.A(meshPois, 
                               loc = Loc, 
                               repl = Repl)

spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(5 * 1000, 0.001), 
                                     prior.sigma = c(.5, .5))

wRepl.index <- inla.spde.make.index('w', 
                                    n.spde = meshPois$n,
                                    n.repl = NRepl)


StackBernRepl <- inla.stack(
  tag = "BernoulliFit",
  data = list(y = SA$ATobianus01),  
  A = list(1, A.PoisRepl),                  
  effects = list(   
    Covariates = Covariates,
    w          = wRepl.index))

fGam3 <- y ~ -1 + Intercept + 
  fYear2010 + fYear2011 + fYear2012 + fYear2015 +
  Silt + Salinity + SampledArea + MediumSand  + 
  Temperature + FlatFish.SQ + Shrimp.SQ +
  Velo1 + Velo2 + Velo3 + Velo4 +
  Dept1 + Dept2 + Dept3 + Dept4 + 
  f(w, model = spde.PoisRepl, replicate = w.repl)

B3 <- inla(fGam3,
           family = "binomial", 
           lincomb = All.lcs,
           data=inla.stack.data(StackBernRepl),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(StackBernRepl)))


# And compare the models with DICs and WAICs
dic  <- c(B1$dic$dic, B2$dic$dic, B3$dic$dic)   
waic <- c(B1$waic$waic, B2$waic$waic, B3$waic$waic)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Bernoulli GAM",  
                     "Bernoulli GAM + SRF",
                     "Bernoulli GAM + spat.temp RF")
Z.out




# Figure 21.17
Ns <- nrow(SA)
f.Velo    <- B2$summary.lincomb.derived[1:Ns + 0 * Ns, "mean"] 
SeLo.Velo <- B2$summary.lincomb.derived[1:Ns + 0 * Ns,"0.025quant"] 
SeUp.Velo <- B2$summary.lincomb.derived[1:Ns + 0 * Ns,"0.975quant"]

f.Dept    <- B2$summary.lincomb.derived[1:Ns + 1 * Ns, "mean"] 
SeLo.Dept <- B2$summary.lincomb.derived[1:Ns + 1 * Ns,"0.025quant"] 
SeUp.Dept <- B2$summary.lincomb.derived[1:Ns + 1 * Ns,"0.975quant"]

IVelo <- order(SA$Velocity)
IDept <- order(SA$Depth)

MyData <- data.frame(
  mu   = c(   f.Velo[IVelo],    f.Dept[IDept]), 
  SeUp = c(SeUp.Velo[IVelo], SeUp.Dept[IDept]), 
  SeLo = c(SeLo.Velo[IVelo], SeLo.Dept[IDept]), 
  Xaxis = c(sort(SA$Velocity), sort(SA$Depth)),
  ID    = factor(rep(c("Velocity smoother", "Depth smoother"), each = nrow(SA))))


p <- ggplot()
p <- p + xlab("Covariate") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p <- p + facet_wrap(~ID, scales = "free", ncol = 3)                     
p
my.ggp.yrange <- c(0, 0)
XPos <- c(SA$Velocity, SA$Depth)
XID  <- rep(c("Velocity smoother", "Depth smoother"), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p


# Fixed effects
Out <- B2$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
Out



# Hyperparameters spatial random field
summary(B2)
SpFi.w <- inla.spde2.result(inla = B2,
                            name = "w",
                            spde = spdePois,
                            do.transfer = TRUE)

Kappa <- inla.emarginal(function(x) x, 
                        SpFi.w$marginals.kappa[[1]] )

sigmau <- inla.emarginal(function(x) sqrt(x), 
                         SpFi.w$marginals.variance.nominal[[1]] )

r <- inla.emarginal(function(x) x, 
                    SpFi.w$marginals.range.nominal[[1]] )

Kappa
sigmau
r  / 1000  # Distance at which dependency < 0.1





# Figure 21.18
# Plot spatial random field
# This function is modified code from material on Haakon Bakka's website
# We will not explain what is inside this function. Just run it.
PlotField2 <- function(field, mesh, ContourMap, xlim, ylim, Add=FALSE, MyMain, ...){
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
             main = MyMain,
             ...)  
}


# Plot the spatial random field 
# First we are going to create a spatial polygon in
# UTM coordinates for 'land'. We won't explain
# the next block in detail. Just run it.
range(SA$Latitude)
range(SA$Longitude)
coast_poly <- map("worldHires", c("Netherlands"), 
                  fill = TRUE, 
                  col = "transparent",
                  plot = TRUE, 
                  xlim = c(3.38, 4.05),
                  ylim = c(51.55, 52))
points(x = SA$Longitude, y = SA$Latitude)

IDs <- sapply(strsplit(coast_poly$names, ":"), function(x) x[1])
coast_poly_sp <- map2SpatialPolygons(coast_poly, 
                                     IDs = IDs,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))

gIsValid(coast_poly_sp)
Area.UTM = spTransform(coast_poly_sp, 
                       CRS("+proj=utm +zone=31 +south ellps=WGS84 +datum=WGS84"))
plot(Area.UTM)


w <- B2$summary.random$w$mean



par(mfrow = c(1,1), oma=c( 0,0,0,0), mar = c(4,4,1,1)) # margin of 4 spaces width at right hand side
w.pm <- B2$summary.random$w$mean
PlotField2(field = w.pm, 
           mesh = meshPois, 
           xlim = range(meshPois$loc[,1]), 
           ylim = range(meshPois$loc[,2]),
           MyMain = "")
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Area.UTM, add = TRUE, col = "white")
##############################################













#########################################################
# 21.10 Zero-truncated Poisson part of the ZAP GAM
SA$ATobianusPos <- ifelse(SA$A_tobianus > 0, SA$A_tobianus, NA)



# Copy some code from above.

# this is from the Poisson section
Loc        <- cbind(SA$Xutm, SA$Yutm)
RangeGuess <- 5 * 1000     #  5km...Use for paper
MaxEdge    <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex=-0.05)
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)

APois    <- inla.spde.make.A(meshPois, loc = Loc)
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(5 * 1000, 0.0001), 
                                prior.sigma = c(.5, 0.5))

wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)


# This is from the GAM section
library(mgcv)
BasisDept <- smoothCon(s(Depth, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X
colnames(BasisDept) <- paste("Dept", 1:4, sep = "")





Covariates <- data.frame(
  Intercept   = rep(1, N),
  fYear2010   = XYear[,"fYear2010"],
  fYear2011   = XYear[,"fYear2011"],
  fYear2012   = XYear[,"fYear2012"],
  fYear2015   = XYear[,"fYear2015"],
  Silt        = MyStd(SA$Silt),
  Salinity    = MyStd(SA$Salinity),
  SampledArea = MyStd(SA$SampledArea),
  MediumSand  = MyStd(SA$MediumSand),
  Temperature = MyStd(SA$Temperature),
  FlatFish.SQ = MyStd(SA$FlatFish.SQ),
  Shrimp.SQ   = MyStd(SA$Shrimp.SQ),
  Velocity    = MyStd(SA$Velocity),
  Dept1 = BasisDept[,"Dept1"],
  Dept2 = BasisDept[,"Dept2"],
  Dept3 = BasisDept[,"Dept3"],
  Dept4 = BasisDept[,"Dept4"]
)

lcs.Dept <- inla.make.lincombs(Dept1 = BasisDept[, "Dept1"], 
                               Dept2 = BasisDept[, "Dept2"],
                               Dept3 = BasisDept[, "Dept3"],
                               Dept4 = BasisDept[, "Dept4"])

names(lcs.Dept)   <- paste(names(lcs.Dept), "Dept", sep = "")
All.lcs <- c(lcs.Dept)


# Make a stack. 
N <- nrow(SA)
StackPP <- inla.stack(
  tag = "FitPos",
  data = list(y = SA$ATobianusPos),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w = wPois.index))


# We first fit models 1 and 2
# Define model
fzt1 <- formula(y ~ -1 + Intercept + 
                  fYear2010 + fYear2011 + fYear2012 + fYear2015 +
                  Silt + Salinity + SampledArea + MediumSand  + 
                  Temperature + FlatFish.SQ + Shrimp.SQ +
                  Velocity +
                  Dept1 + Dept2 + Dept3 + Dept4)

fzt2 <-formula(y ~ -1 + Intercept + 
                 fYear2010 + fYear2011 + fYear2012 + fYear2015 +
                 Silt + Salinity + SampledArea + MediumSand  + 
                 Temperature + FlatFish.SQ + Shrimp.SQ +
                 Velocity +
                 Dept1 + Dept2 + Dept3 + Dept4 +
                 f(w, model = spdePois))



# Execute a zero-truncated Poisson model without spatial correlation
PP1 <- inla(fzt1,
            family = 'zeroinflatedpoisson0',
            control.family = list(hyper = list(theta = list(initial = -10, fixed = TRUE))),
            lincomb = All.lcs,
            data = inla.stack.data(StackPP),
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(A = inla.stack.A(StackPP)))

PP1$summary.fixed


# Let's fit the second model.
PP2 <- inla(fzt2,
            family = 'zeroinflatedpoisson0',
            control.family = list(hyper = list(theta = list(initial = -10, fixed = TRUE))),
            lincomb = All.lcs,
            data = inla.stack.data(StackPP),
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(A = inla.stack.A(StackPP)))



# And the replicate model
# Copied from the spatial-temporal Poisson GAM
Repl <- as.numeric(SA$fYear) 
NRepl <- length(unique(Repl))

A.PoisRepl <- inla.spde.make.A(meshPois, 
                               loc = Loc, 
                               repl = Repl)

spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(5 * 1000, NA), 
                                     prior.sigma = c(0.5, NA))

# We first ran the model with this:
#spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
#                                     prior.range = c(5 * 1000, 0.001), 
#                                     prior.sigma = c(0.5, 0.5))

#P(range < 2 km) = 0.001
wRepl.index <- inla.spde.make.index('w', 
                                    n.spde = meshPois$n,
                                    n.repl = NRepl)


StackPPRepl <- inla.stack(
  tag = "PPFit",
  data = list(y = SA$ATobianusPos),  
  A = list(1,  A.PoisRepl),                  
  effects = list(   
    Covariates = Covariates,
    w          = wRepl.index))


fzt3 <- formula(y ~ -1 + Intercept + 
                  fYear2010 + fYear2011 + fYear2012 + fYear2015 +
                  Silt + Salinity + SampledArea + MediumSand  + 
                  Temperature + FlatFish.SQ + Shrimp.SQ +
                  Velocity +
                  Dept1 + Dept2 + Dept3 + Dept4 + 
                  f(w, model = spde.PoisRepl, replicate = w.repl))

# And run the model with the spatial-temporal random field
PP3 <- inla(fzt3,
            family = 'zeroinflatedpoisson0',
            control.family = list(hyper = list(theta = list(initial = -10, fixed = TRUE))),
            lincomb = All.lcs,
            data=inla.stack.data(StackPPRepl),
            control.compute = list(dic = TRUE, waic = TRUE),
            control.predictor = list(A = inla.stack.A(StackPPRepl)))




# And compare the three models with DICs and WAICs
Pos <- SA$A_tobianus > 0

dic1 <- PP1$dic$local.dic[Pos]
dic2 <- PP2$dic$local.dic[Pos]
dic3 <- PP3$dic$local.dic[Pos]
waic1 <- PP1$waic$local.waic[Pos]
waic2 <- PP2$waic$local.waic[Pos]
waic3 <- PP3$waic$local.waic[Pos]


dic <- c(sum(dic1, na.rm = TRUE),
         sum(dic2, na.rm = TRUE),
         sum(dic3, na.rm = TRUE))
waic <- c(sum(waic1, na.rm = TRUE),
          sum(waic2, na.rm = TRUE),
          sum(waic3, na.rm = TRUE))


Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("ZT GAM",  
                     "ZT GAM + SRF",
                     "ZT GAM + spat.temp RF")
Z.out
# Use the other set of PC priors to get the same results
# as in the book.





# Figure 21.19
# Plot depth smoother
#### Extract smoothers
Ns <- nrow(SA)
f.Dept    <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns, "mean"] 
SeLo.Dept <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.025quant"] 
SeUp.Dept <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.975quant"]

IDept <- order(SA$Depth)

MyData <- data.frame(
  mu   = c( f.Dept[IDept]), 
  SeUp = c(SeUp.Dept[IDept]), 
  SeLo = c(SeLo.Dept[IDept]), 
  Xaxis = c(sort(SA$Depth)),
  ID    = factor(rep(c("Depth smoother"), each = nrow(SA))))

# just add 3x exp for the figure with the exponential link.

p <- ggplot()
p <- p + xlab("Depth") + ylab("Smoother")
p <- p + theme(text = element_text(size = 15))
p <- p + geom_line(data = MyData, 
                   aes(x = Xaxis, y = mu))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Xaxis, 
                         ymax = SeUp, 
                         ymin = SeLo),
                     alpha = 0.6)
p
my.ggp.yrange <- c(0)
XPos <- c(SA$Depth)
XID  <- rep(c("Depth smoother"), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p





# Figure 21.20
# Plot the spatial random fields from model PP3
w     <- PP3$summary.random$w$mean
Years <- c(2009, 2010, 2011, 2012, 2015)

par(oma=c( 0,0,0,0), mar = c(4,4,1,1)) # margin of 4 spaces width at right hand side
set.panel(3,2) # 2X2 matrix of plots

for (i in 1:length(Years)){
  w.pm <- w[wRepl.index$w.repl == i]
  MyTitle <- Years[i]
  PlotField2(field = w.pm, 
             mesh = meshPois, 
             xlim = range(meshPois$loc[,1]), 
             ylim = range(meshPois$loc[,2]),
             MyMain = MyTitle)
  points(x = Loc[SA$Year==Years[i],1],
         y = Loc[SA$Year==Years[i],2], 
         cex = 0.5, 
         col = "black", 
         pch = 16)
  plot(Area.UTM, add = TRUE, col = "white")
}


# Numerical output of the model
summary(PP3)
Out <- PP3$summary.fixed[1:13, c("mean","0.025quant", "0.975quant" )]
print(Out, digits = 3)





# Model validation
mu.ZTruncPois <- PP3$summary.fitted.values[1:N,"mean"]


# Calculate the fitted values manually
# https://en.wikipedia.org/wiki/Zero-truncated_Poisson_distribution

X <- as.matrix(Covariates)
beta <- PP3$summary.fixed[,"mean"]
wpm <- PP3$summary.random$w$mean
eta <- X %*% betaPos + A.PoisRepl %*% wpm
mu <- exp(eta)
ExpY <- mu / (1 - exp(-mu))
VarY <- ExpY * (1 + mu - ExpY)
Ezt <- (SA$A_tobianus - ExpY) / sqrt(VarY)
Ezt <- as.vector(Ezt)
SA$Ezt <- as.vector(Ezt)



SAPos <- subset(SA, ATobianus01 > 0)
dim(SAPos)


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