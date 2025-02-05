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
  # filter(waterbody_type == "river") %>% 
  filter(year >= 2013 & year <= 2022) %>% # Remove years less than 2013 or greater than 2022
  filter(!is.na(eqr)) %>%  # Remove rows where EQR is NA
  mutate(vec_abund_pa   = ifelse(vec_abund == 0, 0, 1),
         fyear          = factor(year),
         fmonth         = factor(month),
         fsite_id       = factor(site_id),
         date           = as.Date(date, format = "%d/%m/%Y"), 
         # friver_type    = factor(river_type, levels = c("1", "2", "3", "4", "5"), 
         #                         labels = c("Type1", "Type2", "Type3", "Type4", "Type5"),
         #                         ordered = T),
         # fstate         = factor(state, levels = c("N", "HM", "A"), 
         #                         labels = c("Natural", "HeavilyModified", "Artificial"),
         #                         ordered = T),
         feqc           = factor(eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High"), 
                                 labels = c("Bad", "Poor", "Moderate", "Good", "High"),
                                 ordered = T),
         waterbody_name = factor(waterbody_name),
         fwaterbody_type = factor(waterbody_type),
         doy            = yday(date)) %>%
  rename(agriculture    = agricultural_areas_200m,
         artificial     = artificial_surfaces_200m,
         natural        = forest_and_semi_natural_areas_200m,
         water          = water_bodies_200m,
         wetland        = wetlands_200m) %>% 
  arrange(desc(waterbody_type), site_id, year) 

# Convert tibble to dataframe
SA <- as.data.frame(SA) # this is needed because some older code does not recocgnise tibble

# data dimensions
names(SA)
str(SA)
head(SA)
dim(SA)

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

# Housekeeping
# Missing values?
colSums(is.na(SA))  
100 * colSums(is.na(SA)) / nrow(SA)  
# which(is.na(SA$eqr)) # observation #467
# which(is.na(SA$feqc)) # observation #467

# remove observation with missing EQR data
# SA <- SA[-467, ]
# which(is.na(SA$eqr)) # removed :)

# How many observations do we have per year?
table(SA$fyear)
# slightly less observations in 2018

# How many observations do we have per location x year?
obvs <- table(SA$site_id, SA$fyear)
print(obvs <- cbind(obvs, total = rowSums(obvs)))

# how many sites do we have in total?
NROW(unique(SA$site_id))
######################################################################

######################################################################
# Data exploration
# Plot all sites
# Spatial position of the sites
plot(x = SA$longitude, 
     y = SA$latitude, 
     pch = 3)
xyplot(latitude ~ longitude, 
       aspect = "iso",
       data = SA)

range(SA$longitude, SA$latitude)
MyCex <- 2 * sqrt(SA$vec_abund + 1) / 10
register_google(key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc")
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

# plot only sites with vectors
SA_postive <- SA[SA[, "vec_abund"] > 0, ]
MyCex <- 2 * sqrt(SA_postive$vec_abund + 1) / 10
glgmap <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57), 
                  maptype = "terrain")    
p_pos <- ggmap(glgmap)
p_pos <- p_pos + geom_point(aes(longitude, 
                        latitude), 
                    pch = 19, 
                    size = MyCex, 
                    col = "red",
                    data = SA_postive) 
p_pos <- p_pos + xlab("Longitude") + ylab("Latitude")  
p_pos <- p_pos + theme(text = element_text(size=15))
p_pos

p_pos <- ggmap(glgmap)
p_pos <- p_pos + geom_point(aes(longitude, 
                        latitude), 
                    pch = 19, 
                    size = MyCex, 
                    col = "red",
                    data = SA_postive) 
p_pos <- p_pos + xlab("Longitude") + ylab("Latitude")  
p_pos <- p_pos + theme(text = element_text(size=15))
p_pos <- p_pos + facet_wrap( ~ fyear)
p_pos # Some 2018 misery?

# Outliers
MyX <- c("year", "eqr",
         "agriculture", "artificial", "natural", "water", "wetland",
         "ppt", "q", "tmax", "tmin", "ws")
Mydotplot(SA[, MyX])

# Collinearity
#  Use VIF values
#  Use Pearson correlations
#  Use scatterplots

# Variance inflation factors
MyX <- c("fyear", 
         "fmonth",
         "eqr", 
         "latitude", 
         "longitude",
         "agriculture", 
         "artificial", 
         "natural",
         "ppt", 
         "q", 
         "tmax", 
         "tmin", 
         "ws")
corvif(SA[, MyX])
# water and wetlands look weird. Lets drop them since they are likely not adding much and difficult to lobby for.
# We have some collinearity!

# This is with some variables removed
MyX <- c("fyear", 
         "fmonth",
         "eqr", 
         "latitude", 
         "longitude",
         "agriculture", 
         "artificial",
         "natural",
         "ppt", 
         "tmax", 
         "ws")   
corvif(SA[, MyX])
# agriculture and temporal terms appear to be correlated. Could be that there has been a shift in agricultural practices over time... Indeed there has been.

# And this is without the temporal terms
MyX <- c("eqr", 
         "fwaterbody_type",
         "latitude", 
         "longitude",
         "agriculture",
         "artificial",
         "natural",
         "ppt", 
         "tmax", 
         "ws")   
corvif(SA[, MyX])
# waterbody type is correlated to agriculture as well... lets remove it and keep agriculture as it seems more interesting.

# Let's make a scatterplot of the continuous covariates
MyX <- c("eqr", 
         "latitude", 
         "longitude",
         "agriculture",
         "artificial",
         "natural",
         "ppt", 
         "tmax", 
         "ws")   
Mypairs(SA[, MyX])

# Agricultural and natural areas are collinear highly collinear. 
# Agricultural and waterbody type are collinear highly collinear. 
# Just pick one. What about agriculture?

# If we waterbody type agriculture, how do the VIF values look?
# Variance inflation factors
corvif(SA[, MyX])
# The VIF values are now okay!

# Is there correlation between fYear and covariates?
# Make a boxplot
boxplot(eqr ~ fyear,
        data = SA)

boxplot(agriculture ~ fyear,
        data = SA)

boxplot(artificial ~ fyear,
        data = SA)

boxplot(natural ~ fyear,
        data = SA)

boxplot(tmax ~ fyear,
        data = SA)

boxplot(ppt ~ fyear,
        data = SA)

boxplot(ws ~ fyear,
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
# from only 3 September, October, November, so there
# is probably no seasonality in the counts. And if
# there is, then the other covariatres (e.g. temperature) will capture it.


# If you play a little bit more with scatterplots, 
# then we eventually end up with the following plan:

#  Model vec_abund as a function of: 
#      eqr - water quality
#      year - temporal variability
#      agriculture areas - disturbance?
#      natural areas - no disturbance?
#      average maximum temeperatures from 12 months before sampling?
#      average precipitation from 12 months before sampling?
#      average wind speed from 12 months before sampling?
#      and potentially spatial correlation with X and Y

# Not in the book:  
# Visualizing relationships
MyX <- c("longitude", "latitude", 
         "eqr", 
         "agriculture", "artificial", "natural",
         "ppt", "tmax", "ws")
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
boxplot(vec_abund ~ fyear, 
        xlab = "Year",
        ylab = "Vector abundance",
        data = SA)

plot(x = SA$eqr,
     y = SA$vec_abund,
     xlab = "EQR",
     ylab = "Vector abundance")

plot(x = SA$agriculture,
     y = SA$vec_abund,
     xlab = "Agriculture",
     ylab = "Vector abundance")

plot(x = SA$artificial,
     y = SA$vec_abund,
     xlab = "Artificial",
     ylab = "Vector abundance")

plot(x = SA$natural,
     y = SA$vec_abund,
     xlab = "Natural areas",
     ylab = "Vector abundance")

plot(x = SA$ppt,
     y = SA$vec_abund,
     xlab = "Precipitation",
     ylab = "Vector abundance")

plot(x = SA$tmax,
     y = SA$vec_abund,
     xlab = "Temperarature",
     ylab = "Vector abundance")

plot(x = SA$ws,
     y = SA$vec_abund,
     xlab = "Wind speed",
     ylab = "Vector abundance")


# Zero inflation?
par(mfrow = c(1,1))
plot(table(SA$vec_abund), type = "h", main = "Vector abundance")
round(100 * table(SA$vec_abund)/nrow(SA), digits = 2)

sum(SA$vec_abund == 0)  # 906/1436 zeros
round(100 * sum(SA$vec_abund == 0) / nrow(SA), 0)  # 62% of zeros
# That's a lot!
###########################################

###########################################
# Start of analysis
#
# Fit a Poisson GLM and assess whether there is overdispersion.
# Standardize the covariates to avoid numerical problems.
SA$eqr.std         <- MyStd(SA$eqr)
SA$agriculture.std <- MyStd(SA$agriculture)
SA$artificial.std  <- MyStd(SA$artificial)
SA$natural.std     <- MyStd(SA$natural)
SA$ppt.std         <- MyStd(SA$ppt)
SA$tmax.std        <- MyStd(SA$tmax)
SA$ws.std          <- MyStd(SA$ws)

I1 <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "poisson",
           control.compute = list(dic = TRUE, waic = TRUE),
           data = SA)
mu1 <- I1$summary.fitted.values[,"mean"]
E1  <- (SA$vec_abund - mu1) / sqrt(mu1)

# Calcuate the dispersion statistic
N <- nrow(SA)
I1$names.fixed
p <- length(I1$names.fixed)
Dispersion <- sum(E1^2) / (N - p)
Dispersion
# That is overdispersion!

# Why do we have overdispersion?
# A. Outliers Y?           ==> Remove them?
# B. Missing covariates?   ==> Add them (or add a latent variable)
# C. Missing interSBtions? ==> Add them
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
plot(x = mu1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)     
# No clear outliers.

# Plot residuals vs each covariate in the model
# And vs each covariate not in the model.
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = SA$eqr, 
     y = E1,
     xlab = "EQR",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)     

plot(x = SA$agriculture, 
     y = E1,
     xlab = "Agriculture",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)  

plot(x = SA$artificial, 
     y = E1,
     xlab = "Artificial",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)   

plot(x = SA$natural, 
     y = E1,
     xlab = "Natural",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)   

plot(x = SA$ppt, 
     y = E1,
     xlab = "Precipitation",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)  

plot(x = SA$tmax, 
     y = E1,
     xlab = "Maximum temperature",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)  

plot(x = SA$tmin, 
     y = E1,
     xlab = "Minimum temperature",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)  

plot(x = SA$ws, 
     y = E1,
     xlab = "Wind speed",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)  

# We can also fit a smoother on the residuals and see whether it tells us something.
library(mgcv)   
T1 <- gam(E1 ~ s(eqr), 
          data = SA)
summary(T1)
plot(T1)
# That is not really convincing.

T2 <- gam(E1 ~ s(agriculture), 
          data = SA)
summary(T2)
plot(T2)
# That is not really convincing.

T3 <- gam(E1 ~ s(artificial), 
          data = SA)
summary(T3)
plot(T3)
# That is not really convincing.

T4 <- gam(E1 ~ s(natural), 
          data = SA)
summary(T4)
plot(T4)
# That is interesting.

T5 <- gam(E1 ~ s(ppt), 
          data = SA)
summary(T5)
plot(T5)
# That is not really convincing.

T6 <- gam(E1 ~ s(tmax), 
          data = SA)
summary(T6)
plot(T6)
# That is interesting.

T8 <- gam(E1 ~ s(ws), 
          data = SA)
summary(T8)
plot(T8)
# That is not really convincing.


# Also check year:  
boxplot(E1 ~ fyear, data = SA)

summary(lm(E1 ~ fyear, data = SA))

# Zero inflation?
# That may well be possible.

# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 100 km
mydata <- data.frame(E1, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
V12 <- variogram(E1 ~ 1, mydata, cutoff = 200, cressie = TRUE)
plot(V12, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Kind of!


# Simulation study
I2 <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "poisson", 
           data = SA,
           control.compute=list(config = TRUE))

set.seed(12345)
Sim <- inla.posterior.sample(n = 1, result = I2, seed = 12345)
names(Sim[[1]])
Sim[[1]]$latent

MyParams <- c("(Intercept)", 
              "eqr.std", 
              "agriculture.std",
              "artificial.std",
              "natural.std",
              "ppt.std",
              "tmax.std",
              "ws.std")
rownum <- lapply(MyParams, 
                 function(x) 
                   grep(x, rownames(Sim[[1]]$latent), fixed = TRUE) )
rownum <- as.numeric(rownum)
rownum

X <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
                  data = SA)
Betas <- Sim[[1]]$latent[rownum]
Xm    <- as.matrix(X)


FixedPart  <- Xm %*% Betas
mu         <- exp(FixedPart)
Ysim       <- rpois(n = nrow(SA), lambda = mu)
table(Ysim)


# Not in the book:
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(Ysim),
     xlab = "Simulated vector abundance",
     ylab = "Frequencies")

# Not in the book:  
NSim <- 1000
SimData <- inla.posterior.sample(n = NSim, result = I2)
N  <- nrow(SA)
Ysim <- matrix(nrow = N, ncol = NSim)
mu.i <- matrix(nrow = N, ncol = NSim)
Xm   <- as.matrix(X)

for (i in 1: NSim){
  Betas <- SimData[[i]]$latent[1980:1987] # this corresponds to the row numbers calculated above
  FixedPart <- Xm %*% Betas
  mu.i[,i]  <- exp(FixedPart)
  Ysim[,i]  <- rpois(n = nrow(SA), lambda = mu.i[,i])
}

table(Ysim[,1])
table(Ysim[,2])
table(Ysim[,3])

# Now we have 1000 simulated data sets from the model.
# What shall we do with these simulated data sets?

# We could calculate the number of zeros in each of the 1,000 data sets.
zeros <- vector(length = NSim)
for(i in 1:NSim){
  zeros[i] <- sum(Ysim[,i] == 0)
}

#Let's plot this as a table
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(zeros), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(0, 1200),
     main = "Simulation results")

points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the Poisson model does not contain enough zeros.
#########################################


# So...what shall we do? Extend the model with spatial correlation, zero inflation, or both?
# You probably need to think very careful where the zeros are. Multiple sites with zeros that are next to each other can
# either be modeled with a zero inflated Poisson (ZIP) or zero altered Poisson (ZAP) model, but also with spatial correlation!


# What about fitting the following 3 models:
# I2: Poisson GLM + spatial correlation - actually I2 is without spatial correlation structure
# I3: ZIP model
# I4: ZIP model + spatial correlation

# And compare them? This is not the correct thing
# to do. It would be better to choose between I2, I3 and I4 based on biological knowledge.
#####################################################

#####################################################
# FIT A ZIP MODEL IN R-INLA
I2 <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "zeroinflatedpoisson1",
           data = SA,
           control.compute=list(config = TRUE, dic = TRUE, waic = TRUE))
summary(I2)

library(pscl)
ZI2 <- zeroinfl(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std | 1,
                data = SA)
summary(ZI2)

exp(0.50033)/(1 + exp(0.50033)) 

Betas2 <- I2$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas2, digits = 3)

Pi.inla <- I2$summary.hyper[1, "mean"]
Pi.inla

mu2 <- I2$summary.fitted.values[,"mean"]
ExpY <- (1 - Pi.inla) * mu2
VarY <- (1 - Pi.inla) * (mu2 + Pi.inla * mu2^2)
E2   <- (SA$vec_abund - ExpY) / sqrt(VarY)

# Calculate the dispersion statistic
N <- nrow(SA)
p <- length(I2$names.fixed) + 1
Dispersion <- sum(E2^2) / (N - p)
Dispersion
# still overdispersion, but considerably less!!!! 


# Simulation study
set.seed(12345)
library(VGAM)            
X <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std, data = SA)
NSim    <- 1000
SimData <- inla.posterior.sample(n = NSim, result = I2)
N       <- nrow(SA)
ZerosZIP <- vector(length = NSim)

MyParams <- c("(Intercept)", 
              "eqr.std", 
              "agriculture.std",
              "artificial.std",
              "natural.std",
              "ppt.std",
              "tmax.std",
              "ws.std")
rownum <- lapply(MyParams, 
                 function(x) 
                   grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
rownum <- as.numeric(rownum)
rownum

for (i in 1:NSim){
  Betas       <- SimData[[i]]$latent[1980:1987]
  Pi          <- SimData[[i]]$hyperpar 
  mu          <- exp(Xm %*% Betas) 
  Ysim        <- rzipois(N, lambda = mu, pstr0 = Pi)
  ZerosZIP[i] <- sum(Ysim == 0)
}

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZIP), 
     #axes = FALSE,
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(1000, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)
# The red dot is the number of zeros in the original data set.
# The data simulated from the ZIP model contains enough zeros.  
#########################################

# Sample-variogram with distances up to 250 km
mydata <- data.frame(E2, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
V12 <- variogram(E2 ~ 1, mydata, cutoff = 250, cressie = TRUE)
plot(V12, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogSBm", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Kind of!


# And we can compare I3 to the results of the following matching model
library(pscl)
ZI2 <- zeroinfl(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std  | 1,
                data = SA)
summary(I2)
summary(ZI2)

# Pi component:
# INLA: Pi = 0.622

# pscl:
round(exp(0.50033) / (1 + exp(0.50033)), 3)  # with rounding, the same

# Fixed parameters of both functions are similar
# What are the fitted values given by INLA?
# Here are the pscl fitted values: (1 - Pi) * mu = (1 - Pi) * exp(X * beta)
Xpscl <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std, 
                      data = SA)
beta.count <- coef(ZI2, "count")                      
beta.zero  <- coef(ZI2, "zero")

Pi.pscl <- exp(beta.zero) / (1 + exp(beta.zero))
mu.pscl <- exp(Xpscl %*% beta.count)
ExpY.pscl <- (1 - Pi.pscl) * mu.pscl # = fitted(ZI2) # =(1-Pi) * mu
ExpY.pscl <- as.numeric(ExpY.pscl)
ExpY.pscl

plot(x = ExpY.pscl,
     y = ExpY,
     xlim = c(0, 40),
     ylim = c(0, 40),
     xlab = "pscl fitted values",
     ylab = "INLA fitted values")
# What the hell is this?

plot(x = mu.pscl,
     y = mu2,
     xlim = c(0, 50),
     ylim = c(0, 50),
     xlab = "count part of pscl",
     ylab = "INLA fitted values")

# So...to get the fitted values from INLA we need this:

# # Check the predict 
# Xpred.pscl <- model.matrix(~eqr.std + agriculture.std + natural.std + ppt.std + tmax.std + ws.std, 
#                            data = MyData)
# 
# Pi.pscl <- exp(beta.zero) / (1 + exp(beta.zero))
# mu.pscl <- exp(Xpred.pscl %*% beta.count)
# PredY.pscl <- (1 - Pi.pscl) * mu.pscl 
# PredY.pscl <- as.numeric(PredY.pscl)
# PredY.pscl
# 
# plot(x = MyData$ExpY.inla,
#      y = PredY.pscl)
#################################################################



###################################################
# Implementing a ZAP model
I3 <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
           family = "zeroinflatedpoisson0", 
           data = SA,
           control.compute = list(dic = TRUE, waic = TRUE, config = TRUE))
summary(I3)


ZI3a <- hurdle(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std | 1,
               data = SA)
summary(ZI3a)
exp(-0.50033) / (1 + exp(-0.50033))
1 - exp(-0.50033) / (1 + exp(-0.50033)) # pi in the ZAP model

SA$vec_abund_pa
M1 <- glm(vec_abund_pa ~ 1, family = binomial, data = SA)
summary(M1)

# comparing models
dic  <- c(I1$dic$dic, I2$dic$dic, I3$dic$dic) 
waic <- c(I1$waic$waic, I2$waic$waic, I3$waic$waic)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Poisson GLM",  
                     "ZIP model",
                     "ZAP model")
Z.out

# ZAP approach 1: Fit the ZAP manually in two steps
# Make a variable 0-1, and make a variable that contains the positive data only
# SA$vec_abund_pa  <- ifelse(SA$vec_abund==0, 0, 1)
I4.01 <- inla(vec_abund_pa ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
              family = "binomial", 
              data = SA,
              control.compute = list(dic = TRUE, waic = TRUE))

VecPos <- subset(SA, vec_abund > 0)
TruncPos <- list(hyper = list(theta = list(initial = -10, 
                                           fixed = TRUE)))
I4.pos <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
               family = 'zeroinflatedpoisson0',
               control.family = TruncPos, 
               data = VecPos,
               control.compute = list(dic = TRUE, waic = TRUE))
summary(I4.01)
summary(I4.pos)

I4.pos$summary.fixed[, c("mean", "sd","0.025quant", "0.975quant")]
I4.01$summary.fixed[, c("mean", "sd", "0.025quant", "0.975quant")]

# Compare with hurdle:
ZI4 <- hurdle(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std | 
                eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
              data = SA)
summary(ZI4)

# Bernoulli part
X <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std, data = SA)
Beta01 <- I4.01$summary.fixed[,"mean"]
Pi     <- exp(X %*% Beta01) / (1 + exp(X %*% Beta01))

# Count part
betaTP        <- I4.pos$summary.fixed[,"mean"]
mu            <- exp(X %*% betaTP)
mu.ZTruncPois <-  mu / (1 - exp(-mu))

muzap <- Pi * mu.ZTruncPois 
varY <- (Pi / (1 - exp(-mu))) * (mu + mu^2) - (  Pi * mu / (1 - exp(-mu))  )^2

Ezap <- (SA$vec_abund - muzap) / sqrt(varY)

# Calcuate the dispersion statistic
N <- nrow(SA)
p <- length(I4.01$names.fixed)
Dispersion <- sum(Ezap^2) / (N - p)
Dispersion
# That is overdispersion!

plot(x = Ezap,
     y = SA$vec_abund)
# this looks pretty good to me!


# Using covariates in both parts of the ZAP model: Approach 2
N <- nrow(SA)
Xpos <- data.frame(IntercPos      = rep(1, N), 
                   eqrPos         = SA$eqr.std,
                   agriculturePos = SA$agriculture.std, 
                   artificialPos  = SA$artificial.std,
                   naturalPos     = SA$natural.std,
                   pptPos         = SA$ppt.std,
                   tmaxPos        = SA$tmax.std,
                   wsPos          = SA$ws.std)

X01 <- data.frame(Interc01      = rep(1, N), 
                  eqr01         = SA$eqr.std,
                  agriculture01 = SA$agriculture.std, 
                  artificial01  = SA$artificial.std,
                  natural01     = SA$natural.std,
                  ppt01         = SA$ppt.std,
                  tmax01        = SA$tmax.std,
                  ws01          = SA$ws.std)

SA$vec_abund_pos <- ifelse(SA$vec_abund > 0, SA$vec_abund, NA)
head(SA)


StackPos <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(SA$vec_abund_pos, NA)),  
  A    = list(1),                      
  effects = list(                 
    list(Xpos = Xpos))) 


Stack01 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, SA$vec_abund_pa)),  
  A    = list(1),                      
  effects = list(                 
    list(X01 = X01)))

#Combined stack
Stack4 <- inla.stack(StackPos, Stack01)


f4 <- AllY~ -1 + IntercPos + eqrPos + agriculturePos + artificialPos + naturalPos + pptPos + tmaxPos + wsPos +
                 Interc01 + eqr01 + agriculture01 + artificial01 + natural01 + ppt01 + tmax01 + ws01

HyperZap <- list(theta = list(intial = -10, fixed = TRUE))
I4 <- inla(f4,
           family = c("zeroinflatedpoisson0", "binomial"),
           # family = c("zeroinflatednbinomial0", "binomial"),
           control.family = list(list(hyper = HyperZap),
                                 list()),
           data = inla.stack.data(Stack4),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(
             link = 1,
             A = inla.stack.A(Stack4)))
summary(I4)


# Not in the book:  
joint <- tapply(I4$dic$local.dic, I4$dic$family, sum) # for count data and for pa data respectively
joint # for count data and for pa data respectively
sum(joint) # total for the joint model

# Not in the book:  
M4 <- hurdle(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std  | eqr.std + agriculture.std + natural.std + ppt.std + tmax.std + ws.std,
             data = SA)
summary(M4)
AIC(M4)

# GET DIC stuff
summary(I4)
I4.01$dic$dic + I4.pos$dic$dic
# seperate models appear to be slightly better
#################################


# ZAP, alias hurdle model
RowsPos <- inla.stack.index(Stack4, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack4, tag='Fit01')$data

# mu.ZTruncPois <- I4$summary.fitted.values[RowsPos, "mean"]
# Pi            <- I4$summary.fitted.values[Rows01, "mean"]

BetaPos <- I4$summary.fixed[1:8,"mean"]
Beta01  <- I4$summary.fixed[9:16,"mean"]

Xpos <- as.matrix(Xpos)
X01  <- as.matrix(X01)
mu <- exp(Xpos %*% BetaPos)

mu.ZTruncPois <- mu / (1 - exp(-mu))
Pi <- exp(X01 %*% Beta01) / (1 + exp(X01 %*% Beta01))

muZAP <- Pi * mu.ZTruncPois
varZAP  <- (Pi / (1 - exp(-mu))) * (mu + mu^2) - (  Pi * mu / (1 - exp(-mu))  )^2

EZAP <- (SA$vec_abund - muZAP) / sqrt(varZAP)

# Calcuate the dispersion statistic
N <- nrow(SA)
p <- length(I4$names.fixed)
Dispersion <- sum(EZAP^2) / (nrow(SA) - p)
Dispersion
# That is still overdispersion!

plot(x = EZAP,
     y = SA$vec_abund)
# looks pretty good to me

# SA$vec_abund_pa2  <- ifelse(SA$vec_abund == 0, 0, 1)
M1 <- glm(vec_abund_pa ~ 1,family = binomial, data = SA)
summary(M1)

# Model validation 
set.seed(12345)
library(VGAM)   

I4b <- inla(f4,
            family = c("zeroinflatedpoisson0", "binomial"),
            control.family = list(list(hyper = HyperZap),
                                  list()),
            data = inla.stack.data(Stack4),
            control.compute = list(config = TRUE),
            control.predictor = list(
              link = 1,
              A = inla.stack.A(Stack4)))

# Simulation study
NSim     <- 1000
SimData  <- inla.posterior.sample(n = NSim, result = I4b)
N        <- nrow(SA)
ZerosZAP <- vector(length = NSim)
YZAP     <- matrix(nrow = N, ncol = NSim)

MyVariablesPos <- c("IntercPos", "eqrPos", "agriculturePos", "artificialPos", "naturalPos", "pptPos", "tmaxPos", "wsPos")
MyVariables01 <- c("Interc01", "eqr01", "agriculture01", "artificial01", "natural01", "ppt01", "tmax01", "ws01")

RowsBetaPos <- lapply(MyVariablesPos, 
                   function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBetaPos <- as.numeric(RowsBetaPos)
RowsBetaPos

RowsBeta01 <- lapply(MyVariables01, 
                   function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBeta01 <- as.numeric(RowsBeta01)
RowsBeta01

for (i in 1:NSim){
  BetaPos     <- SimData[[i]]$latent[7917:7924]
  Beta01      <- SimData[[i]]$latent[7925:7932]
  mu          <- exp(Xpos %*% BetaPos)
  Pi          <- exp(X01 %*% Beta01) / (1 + exp(X01 %*% Beta01))
  YZAP[,i]    <- rzapois(N, lambda = mu, pobs0 = 1-Pi)
  ZerosZAP[i] <- sum(YZAP[,i] == 0)
}

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZAP), 
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(1000, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

# Spatial dependency?
# Let's make a variogram of the Pearson residuals.

# Sample-variogram with distances up to 250 km
mydata <- data.frame(EZAP, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
V12 <- variogram(EZAP ~ 1, mydata, cutoff = 250, cressie = TRUE)
plot(V12, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Yes!
####################################################




####################################################
# Adding spatial correlation to the mix
pts <- SA[, c("longitude" , "latitude", "vec_abund")] 
coordinates(pts) <- c("longitude", "latitude") 
migmap <- gmap(x = pts, 
               type = "terrain", 
               zoom = 7,5, 
               map_key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc",
               lonlat = T)
par(mfrow = c(1,1))
plot(migmap)

vec_present <- SA[SA[, "vec_abund"] > 0, ]
pts_present <- vec_present[, c("longitude" , "latitude", "vec_abund")] 
coordinates(pts_present) <- c("longitude", "latitude") 

MyCex <- 5 * vec_present[,"vec_abund"] / max(vec_present[,"vec_abund"]) + 0.5
points(pts_present, 
       col = "dark blue", 
       pch = 20, 
       cex = MyCex)

vec_01 <- SA[SA[,"vec_abund"] == 0,]
pts_01 <- vec_01[, c("longitude" , "latitude", "vec_abund")] 
coordinates(pts_01) <- c("longitude", "latitude") 

points(pts_01, 
       col = "dark red", 
       pch = 1, 
       cex = 1)

MyCex <- 2 * sqrt(SA$vec_abund + 1) / 10
p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude, 
                        latitude), 
                    pch = 19, 
                    size = MyCex, 
                    col = "yellow",
                    data = SA) 
p <- p + geom_point(aes(longitude,
                        latitude),
                    pch = 1,
                    size = 1,
                    col = "red",
                    data = SA[SA[,"vec_abund"] == 0,])
p <- p + xlab("Longitude") + ylab("Latitude")  
p <- p + theme(text = element_text(size=15))
p

p <- ggmap(glgmap)
p <- p + geom_point(aes(longitude, 
                        latitude), 
                    pch = 19, 
                    size = MyCex, 
                    col = "yellow",
                    data = SA) 
p <- p + geom_point(aes(longitude,
                        latitude),
                    pch = 1,
                    size = 1,
                    col = "red",
                    data = SA[SA[,"vec_abund"] == 0,])
p <- p + xlab("Longitude") + ylab("Latitude")  
p <- p + theme(text = element_text(size=15))
p <- p + facet_wrap( ~ fyear)
p # Some 2018 misery?

# lets start with a Poisson GLM
Poi <- inla(vec_abund ~ eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std,
            family = "poisson",
            control.compute = list(dic = TRUE,
                                   waic = TRUE),
            data = SA)

muPoi <- Poi$summary.fitted.values[, "mean"]
EPoi <- (SA$vec_abund - muPoi) / sqrt(muPoi)
p <- length(Poi$names.fixed)
Dispersion <- sum(EPoi^2) / (nrow(SA) - p)
Dispersion
# That is overdispersion!!!!!!!!!!

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

# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 250 km
mydata <- data.frame(EPoi, SA$Ykm, SA$Xkm)
coordinates(mydata)    <- c("SA.Ykm", "SA.Xkm")
GLM.Poi <- variogram(EPoi ~ 1, mydata, cutoff = 250, cressie = TRUE)
plot(GLM.Poi, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? Sort of...

# What are distances between sites?
Loc <- cbind(SA$Xutm, SA$Yutm)
D   <- dist(Loc)

# plot
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

# Most site pairs have distances between them clustered between 100 and 200 km, which is the core of your distance distribution.
# The cumulative plot shows that almost all sites are within 300 km of each other, with the vast majority within 200 km.

# Lets make a mesh!!!!
# What is the distance for which we would expect dependency?

# 75km?
RangeGuess <- 75 * 1000     #  75km??????
# The smaller this value the better...but the longer the computing time

# When determining a range guess for MaxEdge in a spatial model, the idea is to 
# align this with the scale over which spatial correlation might occur due to dispersal. 
# Based on the dispersal information of dipteran insects (which generally disperse within 
# 2–50 km but could be carried farther by wind), I suggested using 50–100 km as a range guess 
# for MaxEdge. This would allow the model to capture the relevant spatial structures created 
# by dispersal over this range, while also considering the distances between the sites.

# Recommended settings:
MaxEdge <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 50 * 1000)
# The convex option puts the boundary of the innerpart
# closer to the points. Saves some computing time
# during this course. Maybe not use it for a paper.
mesh1 <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)
mesh1$n
# 1581 mesh points

# plot
par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(mesh1, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)

#  Define the weighting factors a_ik (also called the projector matrix).
A1 <- inla.spde.make.A(mesh1, loc = Loc)

# The sigma parameter represents the marginal standard deviation of the spatial random field. 
# It controls the variability of the spatial process—essentially, how much variation is explained by the spatial random field.

# Define the SPDE.
# This is tricky stuff. We need to specify this:
# P(Range < range0) = 0.5  and P(sigma > sigma0) = 0.01
# The range0 value is our primary tool to control the amount
# of smoothing that the spatial random field will do.
# The larger it is, the smoother the spatial random field.
# It allows to avoid overfitting.

# So how do we define this? Well, insects can disperse anywhere between 1–50 km but of course could be carried farther by wind, so lets just try...
spde1 <- inla.spde2.pcmatern(mesh1,
                             # prior.range = c(50 * 1000, 0.01), # This specifies that the median value for the range is 50 km, and 0.01 is the probability that the true range is larger than this value.
                             prior.range = c(25 * 1000, 0.5),  # This specifies that the median value for the range is 25 km, and 0.5 is the probability that the true range is larger than this value.
                             # prior.sigma = c(1.5, 0.01))       # This specifies a prior mean of 1.5, and a probability of 0.01 that the true sigma value is larger than this.
                             prior.sigma = c(1, 0.01))         # This specifies a prior mean of 1, and a probability of 0.01 that the true sigma value is larger than this.

# How did we come to these values?
# We used a simple glm to get some feeling about sensible values
# for the PC priors.
range(SA$vec_abund)
# P(Range < 50 km ) = 0.05
# P(sigma > ) = 0.05
# SB = exp(u_i) 
# some u_i have to be as large as 5.24 to cover 190
# If u_i ~ N(0, sigma_u^2) then it is unlikley that sigma_u > 1.5
# P(sigma > 1.5) = 0.05
M1 <- glm(vec_abund ~ 1, data = SA)
summary(M1)


# Define the spatial field.
w1.index <- inla.spde.make.index(
  name = "w", 
  n.spde = spde1$n.spde)

# Define the the stack
Xm <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std, data = SA)
X <- data.frame(eqr.std         = Xm[, 2],
                agriculture.std = Xm[, 3],
                artificial.std  = Xm[, 4],
                natural.std     = Xm[, 5],
                ppt.std         = Xm[, 6],
                tmax.std        = Xm[, 7],
                ws.std          = Xm[, 8])

N <- nrow(SA)
Stack.mesh1 <- inla.stack(
  tag     = "Fit",
  data    = list(y = SA$vec_abund),
  A       = list(1, 1, A1),
  effects = list(
    Intercept = rep(1, N),
    X         = as.data.frame(X),
    w         = w1.index
  )
)

# Define the formula
fPois.mesh1 <- y ~ -1 + Intercept + 
                        eqr.std + 
                        agriculture.std + artificial.std + natural.std + 
                        ppt.std + tmax.std + ws.std +
                        f(w, model = spde1)

# Execute the model in R-INLA
Pois.mesh1 <- inla(fPois.mesh1,
                   family = "poisson",
                   data = inla.stack.data(Stack.mesh1),
                   control.compute = list(dic  = TRUE,
                                          waic = TRUE),
                   control.predictor = list(
                     A = inla.stack.A(Stack.mesh1)
                   ))
summary(Pois.mesh1)

# lets plot the spatial random field
# define the plotting function
PlotField <- function(field, mesh, ContourMap, xlim, ylim, Add = FALSE,...){
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

w1.pm <- Pois.mesh1$summary.random$w$mean  
w1.sd <- Pois.mesh1$summary.random$w$sd  

PlotField(field = w1.pm, 
          mesh = mesh1, 
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)

###############################################################
# create a spatial polygon in UTM coordinates for 'land'.
library('maps')
library('maptools')
library('mapdata')
data("worldHiresMapEnv")
mapdata <- data("worldHiresMapEnv")
range(SA$latitude) # in decimal degree format
range(SA$longitude) # in decimal degree format
BoundaryPoly <- map("world", 
                    regions = c("lithuania"),
                    fill = TRUE, 
                    col = "white",
                    boundary = TRUE,
                    interior = TRUE,
                    plot = TRUE, 
                    ylim = c(53, 57),
                    xlim = c(21, 27))
points(x = SA$longitude, y = SA$latitude, col = "blue", pch = 1, cex = 1)
# points(x = SA$longitude, y = SA$latitude, col = "blue", pch = 20, cex = 1)

IDs <- sapply(strsplit(BoundaryPoly$names, ":"), function(x) x[1])
BoundarySP <- map2SpatialPolygons(map = BoundaryPoly, 
                                  IDs = IDs,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))

# In latitude/longitude
plot(BoundarySP,
     ylim = c(53,57),
     xlim = c(21, 27))
points(x = SA$longitude, y = SA$latitude)

#Make the boundary less detailed
BoundarySP.smooth <- thinnedSpatialPoly(BoundarySP, 
                                        tolerance = 0.05, 
                                        minarea= 0.05, 
                                        topologyPreserve = TRUE, 
                                        avoidGEOS = FALSE)
Boundary.UTM <- spTransform(BoundarySP, 
                            CRS("+proj=utm +zone=34 +datum=WGS84"))

plot(Boundary.UTM)
###############################################################

PlotField(field = w1.pm, 
          mesh = mesh1, 
          xlim = range(mesh1$loc[,1]),
          ylim = range(mesh1$loc[,2]))

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)

plot(Boundary.UTM, col = NA, border = "black", add = TRUE)
# there is an issue here!!!! 


# Lets make a double ZAP model (pa and abundance) + spatial correlation
SA$vec_abund_pos
SA$vec_abund_pa

# Create two seperate spatial fields (contain the same Matern civariance parameters)
w1.pos.index <- inla.spde.make.index(name   = 'wpos',
                                     n.spde = spde1$n.spde)
w1.01.index <- inla.spde.make.index(name   = 'w01',
                                     n.spde = spde1$n.spde)


# create two sets of covariates
Xm <- model.matrix(~eqr.std + agriculture.std + artificial.std + natural.std + ppt.std + tmax.std + ws.std, data = SA)
N <- nrow(SA)
Xpos <- data.frame(Intercept.pos   = rep (1, N),
                   eqr.pos         = Xm[, 2],
                   agriculture.pos = Xm[, 3],
                   artificial.pos  = Xm[, 4],
                   natural.pos     = Xm[, 5],
                   ppt.pos         = Xm[, 6],
                   tmax.pos        = Xm[, 7],
                   ws.pos          = Xm[, 8])
X01 <- data.frame(Intercept.01     = rep (1, N),
                   eqr.01          = Xm[, 2],
                   agriculture.01  = Xm[, 3],
                   artificial.01   = Xm[, 4],
                   natural.01      = Xm[, 5],
                   ppt.01          = Xm[, 6],
                   tmax.01         = Xm[, 7],
                   ws.01           = Xm[, 8])

# create two stacks
StackPos.mesh1 <- inla.stack(
  tag      = "FitPos",
  data     = list(AllY = cbind(SA$vec_abund_pos, NA)),
  A        = list(1, A1),
  effects  = list(
    Xpos = as.data.frame(Xpos),
    wpos = w1.pos.index))

Stack01.mesh1 <- inla.stack(
  tag      = "Fit01",
  data     = list(AllY = cbind(NA, SA$vec_abund_pa)),
  A        = list(1, A1),
  effects  = list(
    X01 = as.data.frame(X01),
    w01 = w1.01.index))

Stack.ZA.mesh1 <- inla.stack(StackPos.mesh1, 
                             Stack01.mesh1)

# define the formula
fZA.mesh1 <- AllY ~ -1 +
  Intercept.pos + 
  eqr.pos + 
  agriculture.pos + 
  artificial.pos + 
  natural.pos + 
  ppt.pos + 
  tmax.pos + 
  ws.pos +
  f(wpos, model = spde1) +
  
  Intercept.01 +
  eqr.01 + 
  agriculture.01 +
  artificial.01 +
  natural.01 +
  ppt.01 +
  tmax.01 +
  ws.01 +
  f(w01, model = spde1)

# Fit a Zero Altered Poisson (ZAP) model with spatial correlation
HyperZap <- list(theta = list(initial = -10,
                              fixed = TRUE))
ZAP.mesh1 <- inla(fZA.mesh1,
                  family  = c("zeroinflatedpoisson0",
                             "binomial"),
                  control.family = list(list(
                    hyper = HyperZap),
                    list()),
                  data    = inla.stack.data(Stack.ZA.mesh1),
                  control.compute = list(dic = TRUE,
                                         waic = TRUE),
                  control.predictor = list(
                    link  = 1, 
                    A     = inla.stack.A(Stack.ZA.mesh1)))

# Fitted values of the ZAP, mesh 1
RowsPos <- inla.stack.index(Stack.mesh1, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.mesh1, tag='Fit01')$data

mu.ZTruncPois <- ZAP.mesh1$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZAP.mesh1$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZAP.mesh1$summary.fixed[1:8,"mean"]
Beta01  <- ZAP.mesh1$summary.fixed[9:16,"mean"]

Xpos <- as.matrix(X)
X01  <- as.matrix(X01)

A1.m <- as.matrix(A1)
wPos   <- ZAP.mesh1$summary.random$wpos$mean 
w01    <- ZAP.mesh1$summary.random$w01$mean 

mu <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
mu.ZTruncPois.self <- mu /  (1 - exp(-mu))
Pi.self <- exp(X01 %*% Beta01 + A1.m %*% w01) / (1 + exp(X01 %*% Beta01 + A1.m %*% w01))

muZAP  <- Pi.self * mu.ZTruncPois.self
varZAP <- (Pi.self / (1 - exp(-mu))) * (mu + mu^2) - (  Pi.self * mu / (1 - exp(-mu))  )^2
EZAP   <- (SA$vec_abund - muZAP) / sqrt(varZAP)

# Calculate the dispersion statistic
N <- nrow(SA)
p <- length(ZAP.mesh1$names.fixed) + 1
Dispersion <- sum(EZAP^2) / (N - p)
Dispersion
# Just about there :)

# observed vs fitted values
plot(x = EZAP,
     y = SA$vec_abund)
# looks pretty good to me
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZAP,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed skate numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

# Simulation study
ZAP.mesh1b <- inla(fZA.mesh1,
                  family  = c("zeroinflatedpoisson0",
                              "binomial"),
                  control.family = list(list(
                    hyper = HyperZap),
                    list()),
                  data    = inla.stack.data(Stack.ZA.mesh1),
                  control.compute = list(config = TRUE),
                  control.predictor = list(
                    link  = 1, 
                    A     = inla.stack.A(Stack.ZA.mesh1)))

NSim     <- 1000
SimData  <- inla.posterior.sample(n = NSim, result = ZAP.mesh1b)
N        <- nrow(SA)
ZerosZAP <- vector(length = NSim)
YZAP     <- matrix(nrow = N, ncol = NSim)

MyVariablesPos <- c("Intercept.pos", "eqr.pos", "agriculture.pos", "artificial.pos", "natural.pos", "ppt.pos", "tmax.pos", "ws.pos")
MyVariables01 <- c("Intercept.01", "eqr.01", "agriculture.01", "artificial.01", "natural.01", "ppt.01", "tmax.01", "ws.01")

RowsBetaPos <- lapply(MyVariablesPos, 
                      function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBetaPos <- as.numeric(RowsBetaPos)
RowsBetaPos

RowsBeta01 <- lapply(MyVariables01, 
                     function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBeta01 <- as.numeric(RowsBeta01)
RowsBeta01

for (i in 1:NSim){
  BetaPos     <- SimData[[i]]$latent[12547:12554]
  Beta01      <- SimData[[i]]$latent[12555:12562]
  mu          <- exp(Xpos %*% BetaPos)
  Pi          <- exp(X01 %*% Beta01) / (1 + exp(X01 %*% Beta01))
  YZAP[,i]    <- rzapois(N, lambda = mu, pobs0 = 1-Pi)
  ZerosZAP[i] <- sum(YZAP[,i] == 0)
}

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZAP), 
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(500, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

# Fit a Zero Altered negative binomial (ZANB) model with spatial correlation
ZANB.mesh1 <- inla(fZA.mesh1,
                   family  = c("zeroinflatednbinomial0",
                               "binomial"),
                   control.family = list(list(
                     hyper = HyperZap),
                     list()),
                   data    = inla.stack.data(Stack.ZA.mesh1),
                   control.compute = list(dic = TRUE,
                                          waic = TRUE),
                   control.predictor = list(
                     link  = 1, 
                     A     = inla.stack.A(Stack.ZA.mesh1)))

BetaPos <- ZANB.mesh1$summary.fixed[1:8, "mean"]
Beta01  <- ZANB.mesh1$summary.fixed[9:16, "mean"]
k       <- ZANB.mesh1$summary.hyper[1, "mean"]
wPos    <- ZANB.mesh1$summary.random$wpos$mean
w01     <- ZANB.mesh1$summary.random$w01$mean

mu      <- exp(Xpos %*% BetaPos + A1.m %*% wPos)
P0      <- (k / (mu + k)) ^k
Pi.self <- exp(X01 %*% Beta01 + A1.m %*% w01) /
           (1 + exp(X01 %*% Beta01 + A1.m %*% w01))
muZANB  <- (Pi.self / (1 - P0)) * mu
varZANB <- (Pi.self / (1 - P0)) * (mu^2 + mu +
                                   mu^2 / k) -
           (Pi.self / (1 - P0) * mu)^2
EZANB   <- (SA$vec_abund - muZANB) / sqrt(varZANB) 

# check dispersion
N <- nrow(SA)
p <- length(ZANB.mesh1$names.fixed) + 1
Dispersion <- sum(EZANB^2) / (N - p)
Dispersion
# that makes me happppppyyyy :)

# Model validation
plot(x  = EZANB,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector counts")

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = SA$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector counts",
     xlim = c(0, 150),
     ylim = c(0, 150))
# that makes me happppppyyyy :)

# Plot the spatial random field for the ZAP and ZANB models with mesh 1
# ZAP
wpm.ZAP.Pos <- ZAP.mesh1$summary.random$wpos$mean  
wpm.ZAP.01  <- ZAP.mesh1$summary.random$w01$mean  
wsd.ZAP.Pos <- ZAP.mesh1$summary.random$wpos$sd  
wsd.ZAP.01  <- ZAP.mesh1$summary.random$w01$sd  

#ZANB
wpm.ZANB.Pos <- ZANB.mesh1$summary.random$wpos$mean  
wpm.ZANB.01  <- ZANB.mesh1$summary.random$w01$mean  
wsd.ZANB.Pos <- ZANB.mesh1$summary.random$wpos$sd  
wsd.ZANB.01  <- ZANB.mesh1$summary.random$w01$sd  

#Plot the spatial random field, and add white space for the non-study area
# Postive abundance couts part of the ZANB
PlotField(field = wpm.ZANB.Pos, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Boundary.UTM  , add = TRUE)
# plot(None.StudyArea, 
#      col = "white", 
#      add = TRUE,
#      border = "white")
# plot(Coast.UTM, add = TRUE)

# Binary part of the ZANB
PlotField(field = wpm.ZANB.01, mesh = mesh1, xlim = range(mesh1$loc[,1]), ylim = range(mesh1$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Boundary.UTM  , add = TRUE)
# plot(None.StudyArea, 
#      col = "white", 
#      add = TRUE,
#      border = "white")
# plot(Coast.UTM, add = TRUE)

# These figures make me happy :))))))))))

# comparing the models
# ZAP
DIC.ZAP.mesh1  <- 
  sum(tapply(ZAP.mesh1$dic$local.dic,
             ZAP.mesh1$dic$family, sum))
WAIC.ZAP.mesh1 <- 
  sum(tapply(ZAP.mesh1$waic$local.waic,
             ZAP.mesh1$dic$family, sum))
# ZANB
DIC.ZANB.mesh1  <- 
  sum(tapply(ZANB.mesh1$dic$local.dic,
             ZANB.mesh1$dic$family, sum))
WAIC.ZANB.mesh1 <- 
  sum(tapply(ZANB.mesh1$waic$local.waic,
             ZANB.mesh1$dic$family, sum))

dic  <- c(Pois.mesh1$dic$dic, 
          DIC.ZAP.mesh1,
          DIC.ZANB.mesh1) 
waic <- c(Pois.mesh1$waic$waic,
          WAIC.ZAP.mesh1,
          WAIC.ZANB.mesh1)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Spatial Poisson GLM mesh 1",  
                     "Spatial ZAP model mesh 1",  
                     "Spatial ZANB model mesh 1")
Z.out

MyData <- data.frame(
  Range  = c(Pois.mesh1$summary.hyper[1,"mean"],
             ZAP.mesh1$summary.hyper[c(1,3),"mean"],
             ZANB.mesh1$summary.hyper[c(3,5),"mean"]) / 1000,
  
  sigma_u = c(Pois.mesh1$summary.hyper[2,"mean"],
              ZAP.mesh1$summary.hyper[c(2,4),"mean"],
              ZANB.mesh1$summary.hyper[c(4,6),"mean"]))

colnames(MyData) <- c("Range of mesh", "sigma u")
rownames(MyData) <- c("Spatial Poisson GLM mesh 1", 
                      "Spatial ZAP model mesh 1, count part", 
                      "Spatial ZAP model mesh 1, binary part",
                      "Spatial ZANB model mesh 1, count part", 
                      "Spatial ZANB model mesh 1, binary part")
print(MyData, digits = 5)

# Get Pearson residuals for some of the models.
EPoi.glm <- (SA$vec_abund - muPoi) / sqrt(muPoi)
mu.SpatPois <- Pois.mesh1$summary.fitted.values[1:1979, "mean"] # number of rows in original dataframe
ESpatPois <- (SA$vec_abund - mu.SpatPois) / sqrt(mu.SpatPois)

# Let's make a variogram of the Pearson residuals.
mydata <- data.frame(EPoi.glm = EPoi.glm,
                     EZAP = EZAP, 
                     EZANB = EZANB,
                     ESpatPois = ESpatPois,
                     Ykm = SA$Ykm, 
                     Xkm = SA$Xkm)
coordinates(mydata)  <- c("Xkm", "Ykm")

GLM.Poi    <- variogram(EPoi.glm ~ 1, mydata,  cutoff = 150, cressie = TRUE)
Vario.pois <- variogram(ESpatPois ~ 1, mydata, cutoff = 150, cressie = TRUE)
Vario.ZAP  <- variogram(EZAP ~ 1, mydata, cutoff = 150, cressie = TRUE)
Vario.ZANB <- variogram(EZANB ~ 1, mydata, cutoff = 150, cressie = TRUE)

AllVarios <- data.frame(Gamma = c(#GLM.Poi$gamma, Vario.pois$gamma, 
                                  Vario.ZAP$gamma, Vario.ZANB$gamma),
  Dist  = c(#GLM.Poi$dist, Vario.pois$dist, 
            Vario.ZAP$dist, Vario.ZANB$dist),
  ID    = factor(rep(c(#"Poisson GLM", "Spatial Poisson GLM", 
                       "Spatial ZAP model", "Spatial ZANB model"), each = 15)),
  levels =c(#"Poisson GLM", "Spatial Poisson GLM", 
            "Spatial ZAP model", "Spatial ZANB model"))

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

# Simulation study
ZANB.mesh1b <- inla(fZA.mesh1,
                    family  = c("zeroinflatednbinomial0",
                                "binomial"),
                    control.family = list(list(
                      hyper = HyperZap),
                      list()),
                    data    = inla.stack.data(Stack.ZA.mesh1),
                    control.compute = list(config = TRUE),
                    control.predictor = list(
                      link  = 1, 
                      A     = inla.stack.A(Stack.ZA.mesh1)))

NSim     <- 1000
SimData  <- inla.posterior.sample(n = NSim, result = ZANB.mesh1b)
N        <- nrow(SA)
ZerosZAP <- vector(length = NSim)
YZAP     <- matrix(nrow = N, ncol = NSim)

MyVariablesPos <- c("Intercept.pos", "eqr.pos", "agriculture.pos", "artificial.pos", "natural.pos", "ppt.pos", "tmax.pos", "ws.pos")
MyVariables01 <- c("Intercept.01", "eqr.01", "agriculture.01", "artificial.01", "natural.01", "ppt.01", "tmax.01", "ws.01")

RowsBetaPos <- lapply(MyVariablesPos, 
                      function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBetaPos <- as.numeric(RowsBetaPos)
RowsBetaPos

RowsBeta01 <- lapply(MyVariables01, 
                     function(x) grep(x, rownames(SimData[[1]]$latent), fixed = TRUE) )
RowsBeta01 <- as.numeric(RowsBeta01)
RowsBeta01

for (i in 1:NSim){
  BetaPos     <- SimData[[i]]$latent[12547:12554]
  Beta01      <- SimData[[i]]$latent[12555:12562]
  mu          <- exp(Xpos %*% BetaPos)
  Pi          <- exp(X01 %*% Beta01) / (1 + exp(X01 %*% Beta01))
  YZAP[,i]    <- rzapois(N, lambda = mu, pobs0 = 1-Pi)
  ZerosZAP[i] <- sum(YZAP[,i] == 0)
}

par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(table(ZerosZAP), 
     xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
     ylab = "Number of zeros in 1000 simulated data sets",
     xlim = c(500, 2000),
     main = "Simulation results")
points(x = sum(SA$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

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
