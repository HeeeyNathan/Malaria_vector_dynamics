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
  mutate(log_vec_abund = ifelse(vec_abund == 0, NA, log(vec_abund)),
         vec_abund_pa = ifelse(vec_abund == 0, 0, 1),
         fyear = factor(year),
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
# We may need a model that can deal with
# the zeros....what about a ZAP?


# Relationships
VarX <- c("year", "eqr",
          "agricultural_areas_200m", "forest_and_semi_natural_areas_200m",
          "ppt", "tmax", "tmin", "ws")
MyMultipanel.ggp2(SA,
                  varx = VarX,
                  vary = "vec_abund",
                  ylab = "Vector abundance",
                  addSmoother = T,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)

MyMultipanel.ggp2(SA,
                  varx = VarX,
                  vary = "log_vec_abund",
                  ylab = "Vector abundance",
                  addSmoother = T,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)


p <- ggplot(SA, aes(y = vec_abund, x = eqr))
p <- p + geom_point() + ylab("Vector abundance") + xlab("ecological quality")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = agricultural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Agricultural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = forest_and_semi_natural_areas_200m))
p <- p + geom_point() + ylab("Vector abundance") + xlab("Forest and Natural areas")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = ppt))
p <- p + geom_point() + ylab("Vector abundance") + xlab("precipitation")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = tmax))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmax temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = tmin))
p <- p + geom_point() + ylab("Vector abundance") + xlab("tmin temperatures")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
p

p <- ggplot(SA, aes(y = vec_abund, x = ws))
p <- p + geom_point() + ylab("Vector abundance") + xlab("wind speed")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(se = TRUE, col = "red", lwd = 1)
p <- p + facet_wrap(~ fyear)
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

###############################################
# 21.9 Bernoulli part of the ZAP GAM

# Define spatial random field
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

Loc        <- cbind(SA$Xutm, SA$Yutm)
RangeGuess <- 50 * 1000     #  try 50 km (but decrease if necessary... This should be based on biology (i.e., what is the distance at which we expect spatial dependency to diminish?)
MaxEdge    <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 50 * 1000) # Lets try with 50... This value changes the buffer zone around the points...
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)

meshPois$n
# 3434 mesh points... ALOT!

# plot to random spatial field
par(mfrow = c(1,1), mar=c(1, 1, 1, 1))
plot(meshPois, asp = 1)
points(Loc, col = 2, pch = 16, cex = 1)

APois    <- inla.spde.make.A(meshPois, loc = Loc)
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(50 * 1000, 0.5), # we have no idea what the range is (range can be <> 50km)... change to 0.01 if not working
                                prior.sigma = c(1.5, 0.01)) # As it is in chapter 21: 0.5, 0.05

wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)

# This is from the GAM section
library(mgcv)
BasisAgri <- smoothCon(s(agricultural_areas_200m, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

BasisNatu <- smoothCon(s(forest_and_semi_natural_areas_200m, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

# BasisPPT <-  smoothCon(s(ppt, k = 5, fx = TRUE), 
#                        data = SA, 
#                        knots = NULL, absorb.cons = TRUE)[[1]]$X

# BasisTmin <- smoothCon(s(tmin, k = 5, fx = TRUE), 
#                        data = SA, 
#                        knots = NULL, absorb.cons = TRUE)[[1]]$X

BasisTmax <- smoothCon(s(tmax, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X


# BasisWS   <- smoothCon(s(ws, k = 5, fx = TRUE), 
#                        data = SA, 
#                        knots = NULL, absorb.cons = TRUE)[[1]]$X

colnames(BasisAgri) <- paste("Agri", 1:4, sep = "")
colnames(BasisNatu) <- paste("Natu", 1:4, sep = "")
# colnames(BasisPPT) <- paste("PPT", 1:4, sep = "")
# colnames(BasisTmin) <- paste("Tmin", 1:4, sep = "")
colnames(BasisTmax) <- paste("Tmax", 1:4, sep = "")
# colnames(BasisWS) <- paste("WS", 1:4, sep = "")

MyVar <- c( "eqr", 
            "agricultural_areas_200m", "forest_and_semi_natural_areas_200m",
            "ppt", "tmax", "tmin", "ws")

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
  # agri        = MyStd(SA$agricultural_areas_200m),
  # natu        = MyStd(SA$forest_and_semi_natural_areas_200m),
  ppt         = MyStd(SA$ppt),
  tmin        = MyStd(SA$tmin),
  # tmax        = MyStd(SA$tmax),
  ws          = MyStd(SA$ws),  
  Agri1       = BasisAgri[,"Agri1"],
  Agri2       = BasisAgri[,"Agri2"],
  Agri3       = BasisAgri[,"Agri3"],
  Agri4       = BasisAgri[,"Agri4"],
  Natu1       = BasisNatu[,"Natu1"],
  Natu2       = BasisNatu[,"Natu2"],
  Natu3       = BasisNatu[,"Natu3"],
  Natu4       = BasisNatu[,"Natu4"],
  # PPT1        = BasisPPT[,"PPT1"],
  # PPT2        = BasisPPT[,"PPT2"],
  # PPT3        = BasisPPT[,"PPT3"],
  # PPT4        = BasisPPT[,"PPT4"],
  # Tmin1       = BasisTmin[,"Tmin1"],
  # Tmin2       = BasisTmin[,"Tmin2"],
  # Tmin3       = BasisTmin[,"Tmin3"],
  # Tmin4       = BasisTmin[,"Tmin4"],
  Tmax1       = BasisTmax[,"Tmax1"],
  Tmax2       = BasisTmax[,"Tmax2"],
  Tmax3       = BasisTmax[,"Tmax3"],
  Tmax4       = BasisTmax[,"Tmax4"]
  # , WS1         = BasisWS[,"WS1"],
  # WS2         = BasisWS[,"WS2"],
  # WS3         = BasisWS[,"WS3"],
  # WS4         = BasisWS[,"WS4"]
)

lcs.Agri <- inla.make.lincombs(Agri1 = BasisAgri[, "Agri1"],
                               Agri2 = BasisAgri[, "Agri2"],
                               Agri3 = BasisAgri[, "Agri3"],
                               Agri4 = BasisAgri[, "Agri4"])

lcs.Natu <- inla.make.lincombs(Natu1 = BasisNatu[, "Natu1"],
                               Natu2 = BasisNatu[, "Natu2"],
                               Natu3 = BasisNatu[, "Natu3"],
                               Natu4 = BasisNatu[, "Natu4"])

# lcs.PPT <- inla.make.lincombs(PPT1 = BasisPPT[, "PPT1"], 
#                               PPT2 = BasisPPT[, "PPT2"],
#                               PPT3 = BasisPPT[, "PPT3"],
#                               PPT4 = BasisPPT[, "PPT4"])


# lcs.Tmin <- inla.make.lincombs(Tmin1 = BasisTmin[, "Tmin1"], 
#                                Tmin2 = BasisTmin[, "Tmin2"],
#                                Tmin3 = BasisTmin[, "Tmin3"],
#                                Tmin4 = BasisTmin[, "Tmin4"])

lcs.Tmax <- inla.make.lincombs(Tmax1 = BasisTmax[, "Tmax1"], 
                               Tmax2 = BasisTmax[, "Tmax2"],
                               Tmax3 = BasisTmax[, "Tmax3"],
                               Tmax4 = BasisTmax[, "Tmax4"])

# lcs.WS <- inla.make.lincombs(WS1 = BasisWS[, "WS1"],
#                              WS2 = BasisWS[, "WS2"],
#                              WS3 = BasisWS[, "WS3"],
#                              WS4 = BasisWS[, "WS4"])

names(lcs.Agri)   <- paste(names(lcs.Agri), "Agri", sep = "")
names(lcs.Natu)   <- paste(names(lcs.Natu), "Natu", sep = "")
# names(lcs.PPT)    <- paste(names(lcs.PPT), "PPT", sep = "")
# names(lcs.Tmin)   <- paste(names(lcs.Tmin), "Tmin", sep = "")
names(lcs.Tmax)   <- paste(names(lcs.Tmax), "Tmax", sep = "")
# names(lcs.WS)     <- paste(names(lcs.WS), "WS", sep = "")

All.lcs <- c(
  lcs.Agri, 
  lcs.Natu, 
  # lcs.PPT,
  # lcs.Tmin,
  lcs.Tmax
  # ,lcs.WS
) # the order here is important!!!!!!!!!!!!!

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
                   eqr + 
                   # agri +
                   # natu +
                   ppt +
                   tmin +
                   # tmax +
                   ws + 
                   Agri1 + Agri2 + Agri3 + Agri4 +
                   Natu1 + Natu2 + Natu3 + Natu4 +
                   # PPT1 + PPT2 + PPT3 + PPT4 +
                   # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
                   Tmax1 + Tmax2 + Tmax3 + Tmax4 
                   # + WS1 + WS2 + WS3 + WS4 
                 )

fGam2 <-formula(y ~ -1 + Intercept + 
                  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                  eqr + 
                  # agri +
                  # natu +
                  ppt +
                  tmin +
                  # tmax +
                  ws + 
                  Agri1 + Agri2 + Agri3 + Agri4 +
                  Natu1 + Natu2 + Natu3 + Natu4 +
                  # PPT1 + PPT2 + PPT3 + PPT4 +
                  # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
                  Tmax1 + Tmax2 + Tmax3 + Tmax4 +
                # WS1 + WS2 + WS3 + WS4 +
                  f(w, model = spdePois)
                )

# Execute a Bernoulli GAM model without spatial correlation
B1 <- inla(fGam1,
           family = "binomial", 
           lincomb = All.lcs,
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))

summary(B1)

B2 <- inla(fGam2,
           lincomb = All.lcs,
           family = "binomial", 
           data = inla.stack.data(Stack),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(Stack)))

summary(B2)

# Copied from the spatial-temporal Poisson GAM
Repl <- as.numeric(SA$fyear) 
NRepl <- length(unique(Repl))

A.PoisRepl <- inla.spde.make.A(meshPois, 
                               loc = Loc, 
                               repl = Repl)

spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(50 * 1000, 0.5), # we have no idea what the range is (range can be <> 50km)... change to 0.01 if not working
                                     prior.sigma = c(1.5, 0.01))

wRepl.index <- inla.spde.make.index('w', 
                                    n.spde = meshPois$n,
                                    n.repl = NRepl)

StackBernRepl <- inla.stack(
  tag = "BernoulliFit",
  data = list(y = SA$vec_abund_pa),  
  A = list(1, A.PoisRepl),                  
  effects = list(   
    Covariates = Covariates,
    w          = wRepl.index))

fGam3 <- y ~ -1 + Intercept + 
  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
  eqr + 
  # agri +
  # natu +
  ppt +
  tmin +
  # tmax +
  ws + 
  Agri1 + Agri2 + Agri3 + Agri4 +
  Natu1 + Natu2 + Natu3 + Natu4 +
  # PPT1 + PPT2 + PPT3 + PPT4 +
  # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
  Tmax1 + Tmax2 + Tmax3 + Tmax4 +
  # WS1 + WS2 + WS3 + WS4 +
  f(w, model = spde.PoisRepl, replicate = w.repl)

B3 <- inla(fGam3,
           family = "binomial", 
           lincomb = All.lcs,
           data=inla.stack.data(StackBernRepl),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(A = inla.stack.A(StackBernRepl)))

summary(B3)

# And compare the models with DICs and WAICs
dic  <- c(B1$dic$dic, B2$dic$dic, B3$dic$dic)   
waic <- c(B1$waic$waic, B2$waic$waic, B3$waic$waic)
Z.out     <- cbind(dic, waic)
rownames(Z.out) <- c("Bernoulli GAM",  
                     "Bernoulli GAM + SRF",
                     "Bernoulli GAM + spat.temp RF")
Z.out


# Plotting the smoothers
Ns <- nrow(SA)
LinComb <- B3$summary.lincomb.derived # even though B3 is better, lets use B2 for now
Lincomb_Agri <- LinComb[1:Ns + 0 * Ns,]
Lincomb_Natu <- LinComb[1:Ns + 1 * Ns,]
# Lincomb_PPT  <- LinComb[1:Ns + 0 * Ns,]
# Lincomb_Tmin <- LinComb[1:Ns + 1 * Ns,]
Lincomb_Tmax <- LinComb[1:Ns + 2 * Ns,]
# Lincomb_WS   <- LinComb[1:Ns + 2 * Ns,]

f.Agri    <- LinComb[1:Ns + 0 * Ns,"mean"]
SeLo.Agri <- LinComb[1:Ns + 0 * Ns,"0.025quant"]
SeUp.Agri <- LinComb[1:Ns + 0 * Ns,"0.975quant"]

f.Natu    <- LinComb[1:Ns + 1 * Ns,"mean"]
SeLo.Natu <- LinComb[1:Ns + 1 * Ns,"0.025quant"]
SeUp.Natu <- LinComb[1:Ns + 1 * Ns,"0.975quant"]

# f.PPT    <- LinComb[1:Ns + 0 * Ns,"mean"]
# SeLo.PPT <- LinComb[1:Ns + 0 * Ns,"0.025quant"]
# SeUp.PPT <- LinComb[1:Ns + 0 * Ns,"0.975quant"]

# f.Tmin    <- LinComb[1:Ns + 1 * Ns,"mean"]
# SeLo.Tmin <- LinComb[1:Ns + 1 * Ns,"0.025quant"]
# SeUp.Tmin <- LinComb[1:Ns + 1 * Ns,"0.975quant"]

f.Tmax    <- LinComb[1:Ns + 2 * Ns,"mean"]
SeLo.Tmax <- LinComb[1:Ns + 2 * Ns,"0.025quant"]
SeUp.Tmax <- LinComb[1:Ns + 2 * Ns,"0.975quant"]

# f.WS    <- LinComb[1:Ns + 2 * Ns,"mean"]
# SeLo.WS <- LinComb[1:Ns + 2 * Ns,"0.025quant"]
# SeUp.WS <- LinComb[1:Ns + 2 * Ns,"0.975quant"]

IAgri <- order(SA$agricultural_areas_200m)
INatu <- order(SA$forest_and_semi_natural_areas_200m)
# IPPT <- order(SA$ppt)
# ITmin <- order(SA$tmin)
ITmax <- order(SA$tmax)
# IWS <- order(SA$ws)

MyData <- data.frame(
  mu = c(   
    f.Agri[IAgri],
    f.Natu[INatu],
    # f.PPT[IPPT],
    # f.Tmin[ITmin],
    f.Tmax[ITmax]
    #, f.WS[ITmax]
  ), 
  SeUp = c(
    SeUp.Agri[IAgri],
    SeUp.Natu[INatu],
    # SeUp.PPT[IPPT],
    # SeUp.Tmin[ITmin],
    SeUp.Tmax[ITmax]
    #, SeUp.WS[IWS]
    ), 
  SeLo = c(
    SeLo.Agri[IAgri],
    SeLo.Natu[INatu],
    # SeLo.PPT[IPPT],
    # SeLo.Tmin[ITmin],
    SeLo.Tmax[ITmax]
    # , SeLo.Tmax[IWS])
  ), 
  Xaxis = c(
    sort(SA$agricultural_areas_200m),
    sort(SA$forest_and_semi_natural_areas_200m),
    # sort(SA$ppt),
    # sort(SA$tmin),
    sort(SA$tmax)
    # ,  sort(SA$ws)
    ),
  ID = factor(rep(c(
    "Agricultural area smoother",
    "Natural area smoother",
    # "Precipitation smoother",
    # "Minimum temperature smoother",
    "Maximum temperature smoother"
    # , "Wind speed smoother"
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
XPos <- c(SA$agricultural_areas_200m, SA$forest_and_semi_natural_areas_200m, SA$tmax)
XID  <- rep(c(
  "Agricultural area smoother",
  "Natural area smoother",
  # "Precipitation",
  # "Minimum temperature smoother",
  "Maximum temperature smoother"
  # , "Wind speed"
), each = nrow(SA))

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
  mu   = c(   exp(f.Agri[IAgri]),    exp(f.Natu[INatu]),    exp(f.Tmax[ITmax])), 
  SeUp = c(exp(SeUp.Agri[IAgri]), exp(SeUp.Natu[INatu]), exp(SeUp.Tmax[ITmax])), 
  SeLo = c(exp(SeLo.Agri[IAgri]), exp(SeLo.Natu[INatu]), exp(SeLo.Tmax[ITmax])), 
  Xaxis = c(sort(SA$agricultural_areas_200m), sort(SA$forest_and_semi_natural_areas_200m), sort(SA$tmax)),
  ID    = factor(rep(c("Agricultural area smoother", "Natural area smoother", "Maximum temperature smoother"), each = nrow(SA))))

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
XPos <- c(SA$agricultural_areas_200m, SA$forest_and_semi_natural_areas_200m, SA$tmax)
XID  <- rep(c("Agricultural area smoother", "Natural area smoother", "Maximum temperature smoother"), each = nrow(SA))

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p

# Pull out fixed effects
Out <- B3$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
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
# 82.5 km

# Plot spatial random field
# This function is modified code from material on Haakon Bakka's website
# We will not explain what is inside this function. Just run it.
PlotField2 <- function(field, mesh, ContourMap, xlim, ylim, Add=FALSE, MyMain, ...) {
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
range(SA$longitude)
range(SA$latitude)
coast_poly <- map("world", 
                  regions = c("lithuania", "poland", "russia", "latvia", "belarus"),
                  fill = TRUE, 
                  col = "transparent",
                  boundary = TRUE,
                  interior = TRUE,
                  plot = TRUE, 
                  ylim = c(53, 57),
                  xlim = c(21, 27))
points(x = SA$longitude, y = SA$latitude)

IDs <- sapply(strsplit(coast_poly$names, ":"), function(x) x[1])
coast_poly_sp <- map2SpatialPolygons(coast_poly, 
                                     IDs = IDs,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))

#Make the boundary less detailed
gIsValid(coast_poly_sp)
Area.UTM = spTransform(coast_poly_sp, 
                       CRS("+proj=utm +zone=34 +datum=WGS84"))
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
plot(Area.UTM, add = TRUE, col = "transparent")
##############################################


#########################################################
# 21.10 Zero-truncated Poisson part of the ZAP GAM
SA$vec_abund_pos <- ifelse(SA$vec_abund > 0, SA$vec_abund, NA)

# this is from the Poisson section
Loc        <- cbind(SA$Xutm, SA$Yutm)
RangeGuess <- 50 * 1000     #  5km...Use for paper
MaxEdge    <- RangeGuess / 5

ConvHull <- inla.nonconvex.hull(Loc, convex = 50 * 1000)
meshPois <- inla.mesh.2d(boundary = ConvHull, 
                         max.edge = c(1, 5) * MaxEdge, 
                         cutoff = MaxEdge / 5)

APois    <- inla.spde.make.A(meshPois, loc = Loc)
spdePois <- inla.spde2.pcmatern(meshPois, 
                                prior.range = c(50 * 1000, 0.01), 
                                prior.sigma = c(1.5, 0.01))

wPois.index <- inla.spde.make.index(
  name    = 'w', 
  n.spde  = spdePois$n.spde)


# This is from the GAM section
library(mgcv)

BasisTmax <- smoothCon(s(tmax, k = 5, fx = TRUE), 
                       data = SA, 
                       knots = NULL, absorb.cons = TRUE)[[1]]$X

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
  agri        = MyStd(SA$agricultural_areas_200m),
  natu        = MyStd(SA$forest_and_semi_natural_areas_200m),
  ppt         = MyStd(SA$ppt),
  tmin        = MyStd(SA$tmin),
  # tmax        = MyStd(SA$tmax),
  ws          = MyStd(SA$ws),  
  # Agri1       = BasisAgri[,"Agri1"],
  # Agri2       = BasisAgri[,"Agri2"],
  # Agri3       = BasisAgri[,"Agri3"],
  # Agri4       = BasisAgri[,"Agri4"],
  # Natu1       = BasisNatu[,"Natu1"],
  # Natu2       = BasisNatu[,"Natu2"],
  # Natu3       = BasisNatu[,"Natu3"],
  # Natu4       = BasisNatu[,"Natu4"],
  # PPT1        = BasisPPT[,"PPT1"],
  # PPT2        = BasisPPT[,"PPT2"],
  # PPT3        = BasisPPT[,"PPT3"],
  # PPT4        = BasisPPT[,"PPT4"],
  # Tmin1       = BasisTmin[,"Tmin1"],
  # Tmin2       = BasisTmin[,"Tmin2"],
  # Tmin3       = BasisTmin[,"Tmin3"],
  # Tmin4       = BasisTmin[,"Tmin4"],
  Tmax1       = BasisTmax[,"Tmax1"],
  Tmax2       = BasisTmax[,"Tmax2"],
  Tmax3       = BasisTmax[,"Tmax3"],
  Tmax4       = BasisTmax[,"Tmax4"]
  # , WS1         = BasisWS[,"WS1"],
  # WS2         = BasisWS[,"WS2"],
  # WS3         = BasisWS[,"WS3"],
  # WS4         = BasisWS[,"WS4"]
)

lcs.Tmax <- inla.make.lincombs(Tmax1 = BasisTmax[, "Tmax1"], 
                               Tmax2 = BasisTmax[, "Tmax2"],
                               Tmax3 = BasisTmax[, "Tmax3"],
                               Tmax4 = BasisTmax[, "Tmax4"])

names(lcs.Tmax)   <- paste(names(lcs.Tmax), "Tmax", sep = "")
All.lcs <- c(lcs.Tmax)


# Make a stack. 
N <- nrow(SA)
StackPP <- inla.stack(
  tag = "FitPos",
  data = list(y = SA$vec_abund_pos),  
  A = list(1, APois),                 
  effects = list(
    Covariates = Covariates, 
    w = wPois.index))


# We first fit models 1 and 2
# Define model
fzt1 <- formula(y ~ -1 + Intercept + 
                  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                  eqr + 
                  agri +
                  natu +
                  ppt +
                  tmin +
                  # tmax +
                  ws + 
                  # Agri1 + Agri2 + Agri3 + Agri4 +
                  # Natu1 + Natu2 + Natu3 + Natu4 +
                  # PPT1 + PPT2 + PPT3 + PPT4 +
                  # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
                  Tmax1 + Tmax2 + Tmax3 + Tmax4 
                  # + WS1 + WS2 + WS3 + WS4
                )

fzt2 <-formula(y ~ -1 + Intercept + 
                 fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                 eqr + 
                 agri +
                 natu +
                 ppt +
                 tmin +
                 # tmax +
                 ws + 
                 # Agri1 + Agri2 + Agri3 + Agri4 +
                 # Natu1 + Natu2 + Natu3 + Natu4 +
                 # PPT1 + PPT2 + PPT3 + PPT4 +
                 # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
                 Tmax1 + Tmax2 + Tmax3 + Tmax4 + 
                 # WS1 + WS2 + WS3 + WS4 +
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

PP2$summary.fixed

# And the replicate model
# Copied from the spatial-temporal Poisson GAM
Repl <- as.numeric(SA$fyear) 
NRepl <- length(unique(Repl))

A.PoisRepl <- inla.spde.make.A(meshPois, 
                               loc = Loc, 
                               repl = Repl)

spde.PoisRepl <- inla.spde2.pcmatern(meshPois, 
                                     prior.range = c(50 * 1000, NA), 
                                     prior.sigma = c(1.5, NA))

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
  data = list(y = SA$vec_abund_pos),  
  A = list(1,  A.PoisRepl),                  
  effects = list(   
    Covariates = Covariates,
    w          = wRepl.index))


fzt3 <- formula(y ~ -1 + Intercept + 
                  fYear2014 + fYear2015 + fYear2016 + fYear2017 + fYear2018 + fYear2019 + fYear2020 + fYear2021 + fYear2022 +
                  eqr + 
                  agri +
                  natu +
                  ppt +
                  tmin +
                  # tmax +
                  ws + 
                  # Agri1 + Agri2 + Agri3 + Agri4 +
                  # Natu1 + Natu2 + Natu3 + Natu4 +
                  # PPT1 + PPT2 + PPT3 + PPT4 +
                  # Tmin1 + Tmin2 + Tmin3 + Tmin4 +
                  Tmax1 + Tmax2 + Tmax3 + Tmax4 + 
                  # WS1 + WS2 + WS3 + WS4 +
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
Pos <- SA$vec_abund > 0

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

# Plot depth smoother
#### Extract smoothers
Ns <- nrow(SA)
f.Tmax    <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns, "mean"] 
SeLo.Tmax <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.025quant"] 
SeUp.Tmax <- PP3$summary.lincomb.derived[1:Ns + 0 * Ns,"0.975quant"]

ITmax <- order(SA$tmax)

MyData <- data.frame(
  mu   = c( f.Tmax[ITmax]), 
  SeUp = c(SeUp.Tmax[ITmax]), 
  SeLo = c(SeLo.Tmax[ITmax]), 
  Xaxis = c(sort(SA$tmax)),
  ID    = factor(rep(c("Maximum temperature smoother"), each = nrow(SA))))

# just add 3x exp for the figure with the exponential link.

p <- ggplot()
p <- p + xlab("Maximum temperature") + ylab("Smoother")
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
XPos <- c(SA$tmax)
XID  <- rep(c("Maximum temperature smoother"), each = nrow(SA) )

MyData2 <- data.frame(Y     = rep(my.ggp.yrange, each = nrow(SA)),
                      Xaxis = XPos,
                      ID    = factor(XID))
p <- p + geom_text(data = MyData2,
                   aes(y = Y,
                       x = Xaxis,
                       label = "|"),
                   size = 1)
p

# Plot the spatial random fields from model PP3
w     <- PP3$summary.random$w$mean
Years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)

par(oma=c( 0,0,0,0), mar = c(4,4,1,1)) # margin of 4 spaces width at right hand side
set.panel(5,2) # 2X2 matrix of plots

for (i in 1:length(Years)){
  w.pm <- w[wRepl.index$w.repl == i]
  MyTitle <- Years[i]
  PlotField2(field = w.pm, 
             mesh = meshPois, 
             xlim = range(meshPois$loc[,1]), 
             ylim = range(meshPois$loc[,2]),
             MyMain = MyTitle)
  points(x = Loc[SA$year==Years[i],1],
         y = Loc[SA$year==Years[i],2], 
         cex = 0.5, 
         col = "black", 
         pch = 16)
  plot(Area.UTM, add = TRUE, col = "transparent")
}


# Numerical output of the model
summary(PP3)
Out <- PP3$summary.fixed[1:16, c("mean","0.025quant", "0.975quant" )]
print(Out, digits = 3)


# Model validation
mu.ZTruncPois <- PP3$summary.fitted.values[1:N,"mean"]


# Calculate the fitted values manually
# https://en.wikipedia.org/wiki/Zero-truncated_Poisson_distribution

X <- as.matrix(Covariates)
betaPos <- PP3$summary.fixed[,"mean"]
wpm <- PP3$summary.random$w$mean
eta <- X %*% betaPos + A.PoisRepl %*% wpm
mu <- exp(eta)
ExpY <- mu / (1 - exp(-mu))
VarY <- ExpY * (1 + mu - ExpY)
Ezt <- (SA$vec_abund - ExpY) / sqrt(VarY)
Ezt <- as.vector(Ezt)
SA$Ezt <- as.vector(Ezt)

SAPos <- subset(SA, vec_abund_pa > 0)
dim(SAPos)


beta <- PP3$summary.fixed[, "mean"]
wpm  <- PP3$summary.random$w$mean
eta  <- X %*% betaPos + A.PoisRepl %*% wpm
mu   <- exp(eta)

#fitted values of the Bernoulli GAM
Pi <- B3$summary.fitted.values[1:N, "mean"]

#calculate mean and variance of ZAP
ExpY <- Pi * (mu / (1-exp(-mu)))
VarY <- (Pi / (1 - exp (-mu))) * (mu + mu^2) - ((Pi * mu) / (1-exp (-mu)))^2

# ZAP pearson residuals
Ezap <- (SA$vec_abund - ExpY) / sqrt(VarY)
Ezap.orig <- as.numeric(Ezap)

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
