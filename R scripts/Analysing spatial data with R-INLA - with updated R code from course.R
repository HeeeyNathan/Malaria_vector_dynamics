#==================== RScript for analysis of macroinvertebrate biomonitoring data from Lithuania
#==================== Start =================== 
#==================== Load packages =================== 

# Load packages
library(arm)
library(car)
library(GGally)
library(lattice)
library(lawstat)
library(outliers)
library(tidyverse)
library(dplyr)
library(rlang)
library(grid)
library(gridExtra)
library(glmmTMB)
library(DHARMa)
library(sjPlot)
library(sjmisc)
library(raster)
library(sjlabelled)
library(janitor)
library(gstat)
library(sf)
library(sp)
library(maps)
library(maptools)
library(mapdata)
library(rgeos)
library(fields)
library(effects)
library(AICcmodavg)
library(performance)
library(ggpubr)
library(ggeffects)
library(ggmap)
library(dismo)
library(splancs)
library(INLA)
library(ggfigdone)

source("Additional functions/HighstatLibV11.R")
data("worldHiresMapEnv")

#==================== Import data =================== 

# Import the data
df <- as_tibble(read.csv("Data/Bio_Env_data_03.12.2024.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE)) |> 
  clean_names() |> 
  filter(year >= 2013 & year <= 2022) |> # Remove years less than 2013 or greater than 2022 (when looking at lakes and rivers combined)
  # filter(waterbody_type == "lake") |> # keep data from rivers only
  # filter(waterbody_type == "river") |> # keep data from lakes only
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
register_google(key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc")
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
  dplyr::select(vec_abund, eqr, ppt, q, tmax, tmin, doy, year) |> 
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
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = eqr)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Ecological quality") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$eqr), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = ppt)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Precipitation") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$ppt), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = q)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Discharge") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$q), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = tmax)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Maximum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$tmax), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = tmin)) +
  labs(y = "Vector abundance",
       x = "Minimum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$tmin), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = ws)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Wind speed") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  xlim(round(range(df$ws), 0))  + ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = year)) +
  geom_smooth(method = "gam") + 
  labs(y = "Vector abundance",
       x = "Time") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  ylim(range(df$vec_abund[df$vec_abund <= 300])) +
  My_theme

df |>
  # filter(vec_abund <= 300) |> 
  ggplot(aes(y = vec_abund, x = doy)) +
  geom_smooth(method = "gam") + 
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
  # filter(vec_abund <= 300) |> 
  ggplot(aes(vec_abund)) +
  geom_freqpoly(bins = 15) +
  labs(x = "Vector abundance",
       y = "Frequency") +
  My_theme
# High number of zeros and count positively skewed
# ZEROS IN THE RESPONSE VARIABLE

df |> 
  # filter(vec_abund <= 300) |> 
  summarise(percentage_zero = sum(vec_abund == 0) / n() * 100)
# 64% zeros - too many?
# Need to fit a model then simulate from it to be sure


# COLLINEARITY

df |> 
  # filter(vec_abund <= 300) |> 
  ggpairs(columns = c("eqr", "ppt", "q", "tmax", "tmin", "ws", "doy", "year"), 
          aes(alpha = 0.8), lower = list(continuous = "smooth_loess", 
          combo = wrap("facethist", binwidth = 5))) + My_theme

df |> 
  # filter(vec_abund <= 300) |> 
  dplyr::select(eqr, ppt, q, tmax, tmin, ws, year, doy) |> 
  corvif()

# Perhaps tmin and q can cause some trouble

df |> 
  # filter(vec_abund <= 300) |> 
  dplyr::select(eqr, ppt, tmax, ws, year, doy) |> 
  corvif()

# RELATIONSHIPS

# Plot figure
grid.arrange(
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = eqr, y = vec_abund_pa), alpha = 0.5) +
  geom_smooth(aes(x = eqr, y = vec_abund_pa), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = eqr, y = vec_abund_pa), method = 'gam', se = T, col = "blue")+
  labs(x = "Ecological quality", y = "Vector presence/absence") +
  My_theme, 
  
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = eqr, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = eqr, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = eqr, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Ecological quality", y = "Vector abundance") +
  My_theme, 
  
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = ppt, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = ppt, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = ppt, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Precipitation", y = "Vector abundance") +
  My_theme,  

df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = tmax, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = tmax, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = tmax, y = vec_abund), method = 'gam', se = T, col = "blue") +
  labs(x = "Maximum temperature", y = "Vector abundance") +
  My_theme, 
  
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = ws, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = ws, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = ws, y = vec_abund), method = 'gam', se = T, col = "blue")+
  labs(x = "Wind speed", y = "Vector abundance") +
  My_theme, 
  
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot() +
  geom_point(aes(x = year, y = vec_abund), alpha = 0.5) +
  geom_smooth(aes(x = year, y = vec_abund), method = 'lm', se = T, col = "red")+
  geom_smooth(aes(x = year, y = vec_abund), method = 'gam', se = T, col = "blue")+
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  labs(x = "Time", y = "Vector abundance") +
  My_theme, 
  
df |> 
  # filter(vec_abund <= 300) |> 
  ggplot(aes(x = fyear, y = vec_abund, fill = fyear)) +
  geom_point(alpha = 0.5) +
  geom_boxplot(alpha = 0.5) + 
  labs(x = "Time", y = "Vector abundance") +
  scale_x_discrete(breaks = 2013:2022, limits = as.character(2013:2022)) +
  My_theme, 

df |> 
  # filter(vec_abund <= 300) |> 
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

#==================== Standardize data =================== 

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

#==================== General INLA settings =================== 

#' We define some common settings for INLA.
MyControlCompute  <- list(config = TRUE,    #' Allow for posterior simulation
                          dic = TRUE,       #' Calculate DIC
                          waic = TRUE,      #' Calculate WAIC
                          residuals = TRUE) #' Get residuals (see below)
MyControlPredictor  <- list(compute = TRUE, #' Calculate fitted values
                            link = 1)       #' Predict on the scale of the response variable.


#==================== Dataset cleanup =================== 
# Drop columns with any NA values
df <- df[, colSums(is.na(df)) == 0]

#==================== Start of analysis =================== 
#========== Poisson GLM ==========
# Fit a Poisson GLM and assess whether there is overdispersion.
Poi <- inla(vec_abund ~ eqr.std + ppt.std + tmax.std + waterbody_type,
           control.compute = MyControlCompute,
           control.predictor = MyControlPredictor,
           quantiles = c(0.025, 0.975),
           family = "poisson",
           data = df)
summary(Poi)


#' Fitted values from INLA:
N <- nrow(df)
df$Fit.i1 <- Poi$summary.fitted.values[1:N, "mean"]  #' Fitted values.

#' Pearson residuals from INLA, following the definition of Pearson
#' residuals: (Y_i - E[Y_i]) / sqrt(var(Y_i))
df$E1.i1 <- (df$vec_abund - df$Fit.i1) / sqrt(df$Fit.i1)


# Model validation for the INLA Poisson GLM Poi----

#* Getting scaled quantile residuals----

#' We would like to simulate 1000 data sets, because:
#'  1. We can use these to assess whether the model is overdispersed.
#'  2. In case of zero-inflation, we can use the 1000 simulated data
#'     sets to assess whether the model can cope with the zero
#'     inflation.
#'  3. We can use them to obtain scaled quantile residuals.


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, Poi)


#' Extract the 1000 sets of regression parameters
MyVar <- rownames(Poi$summary.fixed)
MyVar    #' Names of the regression parameters
Betas1000 <- inla.posterior.sample.eval(MyVar,
                                        SimData)
Betas1000[,1] #' First set of simulated betas
Betas1000[,2] #' Second set of simulated betas
Betas1000[,3] #' Third set of simulated betas


#' Calculate 1000 times the fitted values via mu = exp(X * beta).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmax.std + waterbody_type,
                  data = df)
X <- as.matrix(X)


#' Start a loop. In each iteration:
#'   1. Calculate the fitted values mu.
#'   2. Simulate Poisson data with the mean equal to mu.
N    <- nrow(df)                      #' Sample size
Ysim <- matrix(nrow = N, ncol = 1000) #' Create space

#' Start the loop (can be done more efficient with lapply).
for (i in 1:1000){
   mu <- exp( X %*% Betas1000[,i] )       #' Simulated fitted values.
   Ysim[,i] <- rpois(n = N, lambda = mu)  #' Simulated count data
}

#' We now have 10000 simulated data sets from the model.
Ysim[, 1] #' First simulated data set.
Ysim[, 2] #' Second simulated data set.
Ysim[, 3] #' Third simulated data set.
#' Etc.

#' Or:
par(mfrow = c(2,2))
hist(df$vec_abund, main ="Observed SR")
hist(Ysim[,1], main = "First simulated data set")
hist(Ysim[,2], main = "Second simulated data set")
hist(Ysim[,3], main = "Third simulated data set")
par(mfrow = c(1,1))
#' The observed data seems to be slightly different?



#' Now we have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
E1.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$vec_abund,
                       fittedPredictedResponse = df$Fit.i1,
                       integerResponse = TRUE)
#' Now we have scaled quantile residuals.


#* Check for overdispersion----

#' We will use the scaled quantile residuals to assess for overdispersion.
testDispersion(E1.sqr)
#' Big trouble.


#* Check for homogeneity of variance----

#' Plot the scaled quantile residuals versus (ranked) fitted values.
plotResiduals(E1.sqr, quantreg = TRUE, smoothScatter = FALSE)
#' Big trouble!


#* Check for normality of the residuals----

#' In DHARMa, we verify whether the scaled quantile residuals are
#' uniform distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(E1.sqr, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)
#' Big trouble!

#* Plot residuals versus the covariates----

#' Plot the scaled quantile residuals versus each covariate
#' in the model.
plotResiduals(E1.sqr, form = df$eqr)            #' Big trouble.
plotResiduals(E1.sqr, form = df$ppt)            #' Big trouble.
plotResiduals(E1.sqr, form = df$tmax)           #' Big trouble.
plotResiduals(E1.sqr, form = df$waterbody_type) #' Big trouble.


#* Check for spatial dependency----
#' Make a variogram of the residuals
#' Use scaled quantile residuals:
df$E1 <- residuals(E1.sqr)

#' Make a sample variogram of the residuals.
MyData <- data.frame(E1  = df$E1,
                     Xkm = df$Xkm,
                     Ykm = df$Ykm)

#' Convert to sf object.
MyData_sf <- st_as_sf(x = MyData,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Apply the variogram function from gstat.
V1 <- variogram(E1 ~ 1,
                data = MyData_sf,
                # cutoff = 150,
                cressie = TRUE)

#' Plot the variogram
p <- ggplot(data = V1, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_smooth(se = TRUE, span = 0.9) +
  labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size = 15),
        legend.position="none")
p
#' We have spatial correlation up to about 100 km.
#' We need to deal with that.

#' We will use this in the next exercise:
write.table(V1, file = "Outputs/variogramDiperta_Pois.txt")

#' We first need to get a sense what the distances are between the
#' sampling locations.
Loc <- cbind(df$Xkm, df$Ykm)
head(Loc)

#' This is in km. Avoid using coordinates with large values small.
#' Use km instead of meters!

#' Distances between sites (i.e. trees).
D <- dist(Loc)
par(mfrow = c(1,1), mar = c(5,5,2,2))
hist(D,
     freq = TRUE,
     main = "",
     xlab = "Distance between sites (km)",
     ylab = "Frequency")
#' Small scale for these data is anything between 0 and 0.2-ish km



#*  Subsection 7.7: Conclusions model validation----

#' We have overdispersion.
#' We have very strange residual patterns.
#' We have spatial dependency up to 100 km.

# Why do we have overdispersion?
# A. Outliers Y?           ==> Remove them?
# B. Missing covariates?   ==> Add them (or add a latent variable)
# C. Missing interactions? ==> Add them
# D. Zero inflation?       ==> ZIP / ZAP
# E. Large variance?       ==> NB or Generalized Poisson
# F. Correlation?          ==> GLMM
# G. Non-linear patterns   ==> GAM 
# H. Wrong link function   ==> Change it 

# Task is to find the culprit. If you pick the wrong one,
# then you may end up with biased parameters.
# How do you figure out which one to pick?
#  -Know your data
#  -Model validation
#  -Data exploration


#========== Create mesh ==========
library(maps)
library(maptools)
library(mapdata)
data("worldHiresMapEnv")
library(rgeoboundaries)
library(sf)

# creating a mesh for spatially correlated data
range(df$longitude)
range(df$latitude)

Lithuania.shp <- geoboundaries("Lithuania")
LithuaniaPoly <- st_transform(Lithuania.shp, crs = 4326)

# Plot the map
plot(st_geometry(LithuaniaPoly), 
     col = "white", 
     border = "black", 
     xlim = c(21, 27), 
     ylim = c(53, 57),
     main = "Lithuania Map with Points")
# Overlay points on the map
points(x = df$longitude, y = df$latitude, col = "blue", pch = 1, cex = 1)

LithuaniaSP <- as(LithuaniaPoly, "Spatial")

# In latitude/longitude
plot(LithuaniaSP,
     ylim = c(53,57),
     xlim = c(21, 27))
points(x = df$longitude, y = df$latitude, col = "blue", pch = 1, cex = 1)

# locator() # this is used to select the points below :)

# create LT buffer
lithuania <- st_read("Additional functions/gadm41_LTU_shp/gadm41_LTU_0.shp")
lithuania_utm <- st_transform(Lithuania.shp, crs = 32634) # UTM Zone 34N (covers Lithuania)
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
points(df$longitude,
       df$latitude,
       cex = 0.5,
       col = 2)
text(x = 20 ,y = 56.5,"A", cex = 1.5)

plot(LithuaniaSP, 
     main = "Selected area",
     axes = TRUE,
     ylim = c(53,57),
     xlim = c(21, 27))
points(df$longitude,
       df$latitude,
       cex = 0.5,
       col = 2)
plot(BufferSP, add = TRUE, lwd = 3)
text(x = 20 ,y = 56.5, "B", cex = 1.5)

plot(BufferSP,
     main = "Lithuania with 5km buffer")
points(df$longitude,
       df$latitude,
       cex = 0.5,
       col = 2)
text(x = 20 ,y = 56.5,"C", cex = 1.5)

# Make the borders less detailed
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
points(df$Xutm,
       df$Yutm,
       cex = 0.5,
       col = 2,
       pch = 1)
text(x = 20 ,y = 56.5, "D", cex = 1.5)

# Making the mesh
# Check spatial correlation
Loc <- as.matrix(df[,c("Xutm", "Yutm")])
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
RangeGuess <- 50 * 1000 
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
ConvHull   <- inla.nonconvex.hull(Loc, convex = 15 * 1000)
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

# Here is a mesh made with the Lithuania border
LT_Boundary <- inla.sp2segment(Lithuania.UTM)
mesh3  <- inla.mesh.2d(loc = Loc,
                       boundary = LT_Boundary,
                       max.edge = c(1, 5) * MaxEdge, 
                       cutoff  = MaxEdge / 5)
plot(mesh3)
points(Loc, pch = 16)
mesh3$n



c(mesh1$n, mesh2$n, mesh3$n)

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

# create spatial polygon for land (based on lithuania without buffer zone)
x3 <- c(min(mesh3$loc[,1]), max(mesh3$loc[,1]),
        max(mesh3$loc[,1]), min(mesh3$loc[,1]))
y3 <- c(min(mesh3$loc[,2]), min(mesh3$loc[,2]),
        max(mesh3$loc[,2]), max(mesh3$loc[,2]))

AreaPoly3 <- Polygon(cbind(x3, y3), hole = FALSE)
AreaSP3   <- SpatialPolygons(list(Polygons(list(AreaPoly3),
                                          ID = "1")))
AreaSP3@proj4string <- Lithuania.UTM@proj4string
plot(AreaSP3)

Land3 <- gDifference(AreaSP3, Lithuania.UTM)
plot(Land3)


#========== Spatial analysis ==========
# lets start doing some spatial things! Yay!
# Define the weighting factors a_ik (also called the projector matrix).
# The sigma parameter represents the marginal standard deviation of the spatial random field. 
# It controls the variability of the spatial process—essentially, how much variation is explained by the spatial random field.
plot(mesh1)
plot(mesh2)
plot(mesh3)

A1 <- inla.spde.make.A(mesh1, loc = Loc)
A2 <- inla.spde.make.A(mesh2, loc = Loc)
A3 <- inla.spde.make.A(mesh3, loc = Loc)

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

spde3 <- inla.spde2.pcmatern(mesh3, 
                             prior.range = c(50 * 1000, 0.5), 
                             prior.sigma = c(1.5, 0.01))
                             # prior.range = c(50 * 1000, 0.01), 
                             # prior.sigma = c(1.5, 0.01)) This was the first attempt

# We used a simple glm to get some feeling about sensible values
# for the PC priors.
range(df$vec_abund)
# P(Range < 50 km ) = 0.05
# P(sigma > ) = 0.05
# SB = exp(u_i) 
# some u_i have to be as large as 13.496 to cover 1360
# If u_i ~ N(0, sigma_u^2) then it is unlikley that sigma_u > 1.5
#P(sigma > 1.5) = 0.05
M1 <- glm(vec_abund ~ 1, data = df)
summary(M1)

# Define the spatial field.
w1.index <- inla.spde.make.index(name = 'w', n.spde  = spde1$n.spde)
w2.index <- inla.spde.make.index(name = 'w', n.spde  = spde2$n.spde)
w3.index <- inla.spde.make.index(name = 'w', n.spde  = spde3$n.spde)

# Define the the stack
Xm <- model.matrix(~eqr.std + ppt.std + tmax.std + 
                     doy.std + waterbody_type, data = df)
N <- nrow(df)
X <- data.frame(eqr.std         = Xm[, 2],
                ppt.std         = Xm[, 3],
                tmax.std        = Xm[, 4],
                doy.std         = Xm[, 5],
                waterbody_type  = Xm[, 6])

# Poisson model
# And this is the stack for the Poisson model
Stack.mesh1 <- inla.stack(
  tag  = "Fit",
  data = list(y = df$vec_abund),  
  A    = list(1, 1, A1, 1),                      
  effects = list( 
    Intercept  = rep(1, N),
    X  = as.data.frame(X),
    w = w1.index,
    iidx = 1:nrow(X)))

Stack.mesh2 <- inla.stack(
  tag  = "Fit",
  data = list(y = df$vec_abund),  
  A    = list(1, 1, A2, 1),                      
  effects = list(      
    Intercept  = rep(1, N),
    X    = as.data.frame(X),
    w    = w2.index,
    iidx = 1:nrow(X)))

Stack.mesh3 <- inla.stack(
  tag  = "Fit",
  data = list(y = df$vec_abund),  
  A    = list(1, 1, A3, 1),                      
  effects = list(      
    Intercept  = rep(1, N),
    X    = as.data.frame(X),
    w    = w3.index,
    iidx = 1:nrow(X)))


# Define the formula
fPois.mesh1 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde1) 

fPois.mesh2 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde2) 

fPois.mesh3 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde3) 

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

Pois.mesh3 <- inla(fPois.mesh3,
                    family = "poisson",
                    data = inla.stack.data(Stack.mesh3),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh3)))

# compare model diagnostics
dic  <- c(Pois.mesh1$dic$dic, Pois.mesh2$dic$dic, Pois.mesh3$dic$dic)
waic <- c(Pois.mesh1$waic$waic, Pois.mesh2$waic$waic, Pois.mesh3$waic$waic)
DicWaic     <- cbind(dic, waic)
rownames(DicWaic) <- c("Spatial Poisson GLM with mesh 1",  
                       "Spatial Poisson GLM with mesh 2",
                       "Spatial Poisson GLM with mesh 3")  
DicWaic

summary(Pois.mesh1)
summary(Pois.mesh2)
summary(Pois.mesh3)

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

# Plot the spatial random field 
w1.pm <- Pois.mesh1$summary.random$w$mean  
w1.sd <- Pois.mesh1$summary.random$w$sd  

w2.pm <- Pois.mesh2$summary.random$w$mean  
w2.sd <- Pois.mesh2$summary.random$w$sd 

w3.pm <- Pois.mesh3$summary.random$w$mean  
w3.sd <- Pois.mesh3$summary.random$w$sd 

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
          xlim = range(mesh2$loc[,1]),
          ylim = range(mesh2$loc[,2]))

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

# And the spatial random field for mesh 2
PlotField(field = w3.pm, 
          mesh = mesh3, 
          xlim = range(mesh3$loc[,1]),
          ylim = range(mesh3$loc[,2]))

# Add the sampling locations (in UTM)
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)

#Determine the area outside the study area
plot(Land3, 
     col = "white", 
     add = TRUE,
     border = "white")

plot(Lithuania.UTM, add = TRUE)

# Model validation 
# Plot fitted values versus observed data
N.rows <- 1:nrow(df)
mu.SpatPois <- Pois.mesh2$summary.fitted.values[N.rows, "mean"] # this is the number of rows in df
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.SpatPois,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector abundance")

# Simulation study
Pois.mesh2.sim <- inla(fPois.mesh2,
                       family = "poisson",
                       data = inla.stack.data(Stack.mesh2),
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                       control.predictor = list(A = inla.stack.A(Stack.mesh2)))


set.seed(12345)
NSim          <- 10000
SimData       <- inla.posterior.sample(n = NSim, result = Pois.mesh2.sim)
N             <- nrow(df)
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

N1 <- mesh2$n # it was mesh1$n - 1 in the original code
MyParams <- paste("w", 1:N1, sep = ":") # changed the zero to 1
RowNum.w <- lapply(MyParams, MyID_w)
RowNum.w <- as.numeric(RowNum.w) # RowNum.w is a list and first needs to be unlisted
RowNum.w

Xm <- as.matrix(X)
Am2 <- as.matrix(A2)
for (i in 1:NSim){
  Beta  <- SimData[[i]]$latent[RowNum.Pos]
  w     <- SimData[[i]]$latent[RowNum.w]
  mu       <- exp(Xm %*% Beta + Am2 %*% w)
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
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosPoisSpat) / 10000
sum(df$vec_abund == 0)
# cry... there are too few zeros :'(

#========== Poisson GLM with observation level random effects ==========
# Poisson GLM with observation level random effects for meshes 1 and 2. 
# After that the Negative binomial GLM is fitted with meshes 1 and 2.

# Poisson + spatial correlation + OLRE
hyper.iid <- list(prec = list(prior = 'pc.prec', param = c(0.5, 0.001))) 
fPois.olre.mesh1 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde1) +
  f(iidx, model="iid", hyper = hyper.iid)

fPois.olre.mesh2 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde2) +
  f(iidx, model="iid", hyper = hyper.iid)

fPois.olre.mesh3 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde3) +
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
Pois.olre.mesh3 <- inla(fPois.olre.mesh3,
                        family = "poisson",
                        data = inla.stack.data(Stack.mesh3),
                        control.compute = list(dic = TRUE, waic = TRUE),
                        control.predictor = list(A = inla.stack.A(Stack.mesh3)))
summary(Pois.olre.mesh1)
summary(Pois.olre.mesh2)
summary(Pois.olre.mesh3)

# Plot fitted values versus observed data
mu.Pois.olre <- Pois.olre.mesh1$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.Pois.olre,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers")

mu.Pois.olre2 <- Pois.olre.mesh2$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.Pois.olre2,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers")

mu.Pois.olre3 <- Pois.olre.mesh3$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.Pois.olre2,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers")

#========== NB GLM + spatial correlation ==========
fNB.mesh1 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde1) 

fNB.mesh2 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde2) 

fNB.mesh3 <- y ~ -1 + Intercept + 
  eqr.std + ppt.std + tmax.std + doy.std + waterbody_type +
  f(w, model = spde3) 

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
NB.mesh3 <- inla(fNB.mesh3,
                    family = "nbinomial",
                    data = inla.stack.data(Stack.mesh3),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(A = inla.stack.A(Stack.mesh3)))

# Plot fitted values versus observed data
mu.NB <- NB.mesh1$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.NB,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers",
     xlim = c(0, 200),
     ylim = c(0, 200))

mu.NB1 <- NB.mesh2$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.NB1,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers",
     xlim = c(0, 200),
     ylim = c(0, 200))

mu.NB2 <- NB.mesh3$summary.fitted.values[1:2089, "mean"] #the number of rows for fitted.APredictor (number of rows in df)
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = mu.NB2,
     y = df$vec_abund,
     xlab = "Fitted values",
     ylab = "Observed vector numbers",
     xlim = c(0, 200),
     ylim = c(0, 200))

summary(NB.mesh1)
summary(NB.mesh2)
summary(NB.mesh3)

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

# And the spatial random field for mesh 2
w2NB.pm <- NB.mesh2$summary.random$w$mean  
w2NB.sd <- NB.mesh2$summary.random$w$sd  

PlotField(field = w2NB.pm, 
          mesh = mesh2, 
          xlim = range(mesh2$loc[,1]),
          ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Land2, add = TRUE)

PlotField(field = w2NB.sd, 
          mesh = mesh2, 
          xlim = range(mesh2$loc[,1]),
          ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Land2, add = TRUE)

# And the spatial random field for mesh 3
w3NB.pm <- NB.mesh3$summary.random$w$mean  
w3NB.sd <- NB.mesh3$summary.random$w$sd  

PlotField(field = w3NB.pm, 
          mesh = mesh3, 
          xlim = range(mesh3$loc[,1]),
          ylim = range(mesh3$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land3, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Land3, add = TRUE)

PlotField(field = w3NB.sd, 
          mesh = mesh3, 
          xlim = range(mesh3$loc[,1]),
          ylim = range(mesh3$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land3, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Land3, add = TRUE)

# Simulation study NB GLM with spatial correlation
NB.mesh2.sim <- inla(fNB.mesh2,
                       family = "nbinomial",
                       data = inla.stack.data(Stack.mesh2),
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                       control.predictor = list(A = inla.stack.A(Stack.mesh2)))

NB.mesh2.sim$summary.hyper

set.seed(12345)
NSim          <- 10000
SimData       <- inla.posterior.sample(n = NSim, result = NB.mesh2.sim)
N             <- nrow(df)
ZerosNBSpat   <- vector(length = NSim)
YNBSpat       <- matrix(nrow = N, ncol = NSim)

k <- SimData[[1]]$hyperpar[1]

MyParams <- colnames(X)
RowNum.Pos <- lapply(MyParams, MyID_beta)
RowNum.Pos <- as.numeric(RowNum.Pos)
RowNum.Pos

N1 <- mesh2$n # mesh1$n - 1
MyParams <- paste("w", 1:N1, sep = ":")
RowNum.w <- lapply(MyParams, MyID_w)
RowNum.w <- as.numeric(RowNum.w)
RowNum.w

Xm  <- as.matrix(X)
Am2 <- as.matrix(A2)
k   <- SimData[[1]]$hyperpar[1]

library(MASS)
for (i in 1:NSim){
  Beta  <- SimData[[i]]$latent[RowNum.Pos]
  w     <- SimData[[i]]$latent[RowNum.w]
  mu       <- exp(Xm %*% Beta + Am2 %*% w)
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
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosNBSpat) / 10000
sum(df$vec_abund == 0)
# cry... now there are too many zeros :'(


#========== ZAP model with spatial correlation ==========
# ZAP model with spatial correlation
# Running the model in R-INLA
df$vec_abund_pa
df$vec_abund_pos

w1.pos.index <- inla.spde.make.index(name = 'wpos', n.spde  = spde1$n.spde)
w2.pos.index <- inla.spde.make.index(name = 'wpos', n.spde  = spde2$n.spde)
w3.pos.index <- inla.spde.make.index(name = 'wpos', n.spde  = spde3$n.spde)

w1.01.index <- inla.spde.make.index(name = 'w01', n.spde  = spde1$n.spde)
w2.01.index <- inla.spde.make.index(name = 'w01', n.spde  = spde2$n.spde)
w3.01.index <- inla.spde.make.index(name = 'w01', n.spde  = spde3$n.spde)

Xm <- model.matrix(~eqr.std + ppt.std + tmax.std + doy.std + waterbody_type, data = df)

N <- nrow(df)
X <- data.frame(Intercept.pos      = rep(1, N), 
                eqr.pos            = Xm[, 2],
                ppt.pos            = Xm[, 3], 
                tmax.pos           = Xm[, 4],
                doy.pos            = Xm[, 5],
                waterbody_type.pos = Xm[, 6])


X01 <- data.frame(Intercept.01      = rep(1, N), 
                  eqr.01            = Xm[, 2],
                  ppt.01            = Xm[, 3], 
                  tmax.01           = Xm[, 4],
                  doy.01            = Xm[, 5],
                  waterbody_type.01 = Xm[, 6])

# And this is the stack for the ZAP model
StackPos.mesh1 <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(df$vec_abund_pos, NA)),  
  A    = list(1, A1),                      
  effects = list(            
    Xpos = as.data.frame(X),
    wpos = w1.pos.index))

StackPos.mesh2 <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(df$vec_abund_pos, NA)),  
  A    = list(1, A2),                      
  effects = list(            
         Xpos = as.data.frame(X),
         wpos = w2.pos.index))

StackPos.mesh3 <- inla.stack(
  tag  = "FitPos",
  data = list(AllY = cbind(df$vec_abund_pos, NA)),  
  A    = list(1, A3),                      
  effects = list(            
         Xpos = as.data.frame(X),
         wpos = w3.pos.index))

Stack01.mesh1 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, df$vec_abund_pa)),  
  A    = list(1, A1),                      
  effects = list(      
    X01 = as.data.frame(X01),
    w01 = w1.01.index))

Stack01.mesh2 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, df$vec_abund_pa)),  
  A    = list(1, A2),                      
  effects = list(      
    X01 = as.data.frame(X01),
    w01 = w2.01.index))

Stack01.mesh3 <- inla.stack(
  tag  = "Fit01",
  data = list(AllY = cbind(NA, df$vec_abund_pa)),  
  A    = list(1, A3),                      
  effects = list(      
    X01 = as.data.frame(X01),
    w01 = w3.01.index))

Stack.ZA.mesh1 <- inla.stack(StackPos.mesh1, Stack01.mesh1)
Stack.ZA.mesh2 <- inla.stack(StackPos.mesh2, Stack01.mesh2)
Stack.ZA.mesh3 <- inla.stack(StackPos.mesh3, Stack01.mesh3)

#	Specify the model formula
fZA.mesh1  <- AllY ~ -1 + Intercept.pos + eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos +
                       f(wpos, model = spde1) +
                       Intercept.01 + eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 +
                       f(w01, model = spde1)

fZA.mesh2  <- AllY ~ -1 + Intercept.pos +eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos +
                      f(wpos, model = spde2) +
                      Intercept.01 + eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 +
                      f(w01, model = spde2)

fZA.mesh3  <- AllY ~ -1 + Intercept.pos +eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos +
                      f(wpos, model = spde3) +
                      Intercept.01 + eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 +
                      f(w01, model = spde3)


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

ZAP.mesh3 <- inla(fZA.mesh3,
                   family = c("zeroinflatedpoisson0", "binomial"),
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh3),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh3)))

# compare model diagnostics
dic  <- c(ZAP.mesh1$dic$dic, ZAP.mesh2$dic$dic, ZAP.mesh3$dic$dic)
waic <- c(ZAP.mesh1$waic$waic, ZAP.mesh2$waic$waic, ZAP.mesh3$waic$waic)
DicWaic     <- cbind(dic, waic)
rownames(DicWaic) <- c("ZAP model with spatial mesh 1",  
                       "ZAP model with spatial mesh 2",
                       "ZAP model with spatial mesh 3")  
DicWaic

summary(ZAP.mesh1)
summary(ZAP.mesh2)
summary(ZAP.mesh3)

# Fitted values and Pearson residuals
# Fitted values of the ZAP, mesh 1
RowsPos <- inla.stack.index(Stack.mesh2, tag = 'FitPos')$data
Rows01 <- inla.stack.index(Stack.mesh2, tag = 'Fit01')$data

mu.ZTruncPois <- ZAP.mesh2$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZAP.mesh2$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZAP.mesh2$summary.fixed[1:6,"mean"]
Beta01  <- ZAP.mesh2$summary.fixed[7:12,"mean"]

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)

# A1.m   <- as.matrix(A1)
A2.m   <- as.matrix(A2)
wPos   <- ZAP.mesh2$summary.random$wpos$mean 
w01    <- ZAP.mesh2$summary.random$w01$mean 

mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
mu.ZTruncPois.self <- mu /  (1 - exp(-mu))
Pi.self <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))

muZAP  <- Pi.self * mu.ZTruncPois.self
varZAP <- (Pi.self / (1 - exp(-mu))) * (mu + mu^2) - (  Pi.self * mu / (1 - exp(-mu))  )^2
EZAP   <- (df$vec_abund - muZAP) / sqrt(varZAP)

# Calcuate the dispersion statistic
N <- nrow(df)
p <- length(ZAP.mesh1$names.fixed)
Dispersion <- sum(EZAP^2) / (N - p)
Dispersion
# 1.253457 Good!

# Simulation study ZAP model with spatial correlation
ZAP.mesh2.sim <- inla(fZA.mesh2,
                      family = c("zeroinflatedpoisson0", "binomial"),  
                      control.family = list(list(hyper = HyperZap),
                                            list()),
                      data = inla.stack.data(Stack.ZA.mesh2),
                      control.compute = list(config = TRUE),
                      control.predictor = list(
                        link = 1,
                        A = inla.stack.A(Stack.ZA.mesh2)))

set.seed(12345)
NSim           <- 10000
SimData        <- inla.posterior.sample(n = NSim, result = ZAP.mesh2.sim)
N              <- nrow(df)
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

# N1 <- mesh1$n # mesh1$n - 1
N2 <- mesh2$n # mesh2$n - 1
MyParams.wPos <- paste("wpos", 1:N2, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

# N1 <- mesh1$n # mesh1$n - 1
N2 <- mesh2$n # mesh2$n - 1
MyParams.w01 <- paste("w01", 1:N2, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
A1.m   <- as.matrix(A1)
A2.m   <- as.matrix(A2)

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
    
    mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
    Pi <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))
    
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
     xlim = c(0, 1500),
     main = "Simulation results")
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosZAPSpat) / 10000
sum(df$vec_abund == 0)

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
observed_response <- df$vec_abund
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
predictors <- data.frame(EQR            = df$eqr, 
                         PPT            = df$ppt, 
                         TMAX           = df$tmax,
                         DOY            = df$doy,
                         Waterbody_type = df$waterbody_type)

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
# Plot the spatial random field for the ZAP with mesh 2
wpm.ZAP.Pos <- ZAP.mesh2$summary.random$wpos$mean  
wpm.ZAP.01  <- ZAP.mesh2$summary.random$w01$mean  
wsd.ZAP.Pos <- ZAP.mesh2$summary.random$wpos$sd  
wsd.ZAP.01  <- ZAP.mesh2$summary.random$w01$sd  

# Plot the spatial random field again, and add white space for the non-study area
par(mfrow = c(1, 1), mar = c(5,5,2,2), cex.lab = 1.5)
PlotField(field = wpm.ZAP.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)
plot(Buffer.UTM, add = TRUE)

PlotField(field = wsd.ZAP.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)
plot(Buffer.UTM, add = TRUE)

# Binary part of the ZAP
PlotField(field = wpm.ZAP.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)
plot(Buffer.UTM, add = TRUE)


PlotField(field = wsd.ZAP.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)
plot(Buffer.UTM, add = TRUE)


#========== ZANB model with spatial correlation ==========
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

ZANB.mesh3 <- inla(fZA.mesh3,
                  family = c("zeroinflatednbinomial0", "binomial"),
                  control.family = list(list(hyper = HyperZap),
                                        list()),
                  data = inla.stack.data(Stack.ZA.mesh3),
                  control.compute = list(dic = TRUE, waic = TRUE),
                  control.predictor = list(
                    link = 1,
                    A = inla.stack.A(Stack.ZA.mesh3)))

# compare model diagnostics
dic  <- c(ZANB.mesh1$dic$dic, ZANB.mesh2$dic$dic, ZANB.mesh3$dic$dic)
waic <- c(ZANB.mesh1$waic$waic, ZANB.mesh2$waic$waic, ZANB.mesh3$waic$waic)
DicWaic     <- cbind(dic, waic)
rownames(DicWaic) <- c("ZANB model with spatial mesh 1",  
                       "ZANB model with spatial mesh 2",
                       "ZANB model with spatial mesh 3")  
DicWaic

summary(ZANB.mesh1)
summary(ZANB.mesh2)
summary(ZANB.mesh3)

# Getting fitted values and Pearson residuals
# Fitted values of the ZANB, mesh 1
RowsPos <- inla.stack.index(Stack.ZA.mesh2, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.ZA.mesh2, tag='Fit01')$data

mu.ZTruncPois <- ZANB.mesh2$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZANB.mesh2$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZANB.mesh2$summary.fixed[1:6,"mean"]
Beta01  <- ZANB.mesh2$summary.fixed[7:12,"mean"]
k       <- ZANB.mesh2$summary.hyper[1,"mean"]
Xpos <- as.matrix(X)
X01  <- as.matrix(X01)

# A1.m <- as.matrix(A1)
A2.m <- as.matrix(A2)
wPos   <- ZANB.mesh2$summary.random$wpos$mean 
w01    <- ZANB.mesh2$summary.random$w01$mean 

mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
P0 <- (k / (mu + k))^k
Pi.self <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))

muZANB  <- (Pi.self / (1 - P0))  * mu
varZANB <- (Pi.self / (1 - P0))  * (mu^2 + mu + mu^2 / k ) - (  Pi.self / (1 - P0) * mu  )^2

EZANB <- (df$vec_abund - muZANB) / sqrt(varZANB)
p <- length(ZANB.mesh1$names.fixed)
# p <- 2 * (8 + 2 + 2)
sum(EZANB^2) / (nrow(df) - p)
# 0.8917274 That is.... not overdispersion! :')))

# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = df$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

summary(ZANB.mesh2)

# Simulation study ZINB model with spatial correlation
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
N              <- nrow(df)
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

# N1 <- mesh1$n # mesh1$n - 1
N2 <- mesh2$n # mesh1$n - 1
MyParams.wPos <- paste("wpos", 1:N2, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

# N1 <- mesh1$n # mesh1$n - 1
N2 <- mesh2$n # mesh1$n - 1
MyParams.w01 <- paste("w01", 1:N2, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
# A1.m   <- as.matrix(A1)
A2.m   <- as.matrix(A2)
k   <- SimData[[1]]$hyperpar[1]

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = NSim, style = 3)

for (i in 1:NSim) {
  BetaPos <- SimData[[i]]$latent[RowNum.Pos]
  Beta01 <- SimData[[i]]$latent[RowNum.01]
  wPos <- SimData[[i]]$latent[RowNum.wPos]
  w01 <- SimData[[i]]$latent[RowNum.w01]
  k   <- SimData[[i]]$hyperpar[1] # strictly speaking, simulated values should be used, but there is a tendency for k parameters to not look very random
  
  mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
  Pi <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))
  
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
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosZANBSpat) / 1000
sum(df$vec_abund == 0)

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
observed_response <- df$vec_abund
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
predictors <- data.frame(EQR            = df$eqr, 
                         PPT            = df$ppt, 
                         TMAX           = df$tmax,
                         DOY            = df$doy,
                         Waterbody_type = df$waterbody_type)

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
wpm.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$mean  
wpm.ZANB.01  <- ZANB.mesh2$summary.random$w01$mean  
wsd.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$sd  
wsd.ZANB.01  <- ZANB.mesh2$summary.random$w01$sd  

# Plot the spatial random field again, and add white space for the non-study area
par(mfrow = c(1, 1), mar = c(5,5,2,2), cex.lab = 1.5)
PlotField(field = wpm.ZANB.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

# Binary part of the ZANB
PlotField(field = wpm.ZANB.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

#========== Comparing all modelling results ==========
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


#========== Checking variograms of all models ==========
# Get Pearson residuals for some of the models.
EPoi.glm <- (df$vec_abund - muPoi) / sqrt(muPoi)
ESpatPois <- (df$vec_abund - mu.SpatPois) / sqrt(mu.SpatPois)
ENB   <- (df$vec_abund - mu.NB) / sqrt(mu.NB + mu.NB^2 / NB.mesh1$summary.hyper[1, "mean"])

# Let's make a variogram of the Pearson residuals.
mydata <- data.frame(EPoi.glm = EPoi.glm,
                     EZAP = EZAP, 
                     EZANB = EZANB,
                     ESpatPois = ESpatPois,
                     ENB = ENB,
                     Ykm = df$Ykm, 
                     Xkm = df$Xkm)
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


#========== Model validation of ZANB model ==========
# Getting fitted values and Pearson residuals
# Fitted values of the ZANB, mesh 1
RowsPos <- inla.stack.index(Stack.ZA.mesh2, tag = 'FitPos')$data
Rows01  <- inla.stack.index(Stack.ZA.mesh2, tag = 'Fit01')$data

mu.ZTruncPois <- ZANB.mesh2$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZANB.mesh2$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZANB.mesh1$summary.fixed[1:6,"mean"]
Beta01  <- ZANB.mesh1$summary.fixed[7:12,"mean"]
k       <- ZANB.mesh1$summary.hyper[1,"mean"]
Xpos <- as.matrix(X)
X01  <- as.matrix(X01)

# A1.m <- as.matrix(A1)
A2.m <- as.matrix(A2)
wPos   <- ZANB.mesh2$summary.random$wpos$mean 
w01    <- ZANB.mesh2$summary.random$w01$mean 

mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
P0 <- (k / (mu + k))^k
Pi.self <- exp(X01 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01 %*% Beta01 + A2.m %*% w01))

muZANB  <- (Pi.self / (1 - P0))  * mu
varZANB <- (Pi.self / (1 - P0))  * (mu^2 + mu + mu^2 / k ) - (  Pi.self / (1 - P0) * mu  )^2

EZANB <- (df$vec_abund - muZANB) / sqrt(varZANB)
p <- length(ZANB.mesh2$names.fixed)
# p <- 2 * (8 + 2 + 2)
sum(EZANB^2) / (nrow(df) - p)
# 0.885135 That is.... not overdispersion! :')))

# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = df$vec_abund,
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
N              <- nrow(df)
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

N2 <- mesh2$n # mesh2$n - 1
MyParams.wPos <- paste("wpos", 1:N2, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

N2 <- mesh2$n # mesh2$n - 1
MyParams.w01 <- paste("w01", 1:N2, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

Xpos    <- as.matrix(X)
X01     <- as.matrix(X01)
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
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosZANBSpat) / 10000
sum(df$vec_abund == 0)
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
observed_response <- df$vec_abund
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
predictors <- data.frame(EQR            = df$eqr, 
                         PPT            = df$ppt, 
                         TMAX           = df$tmax,
                         DOY            = df$doy,
                         WATERBODY_TYPE = df$waterbody_type)

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
     y = df$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

# Plot residuals vs each covariate in the model
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
plot(EZANB ~ eqr,
     xlab = "EQR",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")   

plot(EZANB ~ ppt,
     xlab = "Average precipitation",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")   

plot(EZANB ~ tmax,
     xlab = "Average maximum temperature",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")     

plot(EZANB ~ doy,
     xlab = "Day of year",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")   

plot(EZANB ~ waterbody_type,
     xlab = "Waterbody type",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")   

# Fit a smoother on the residuals and see whether it tells us something.
library(mgcv)
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
T1 <- gam(EZANB ~ s(eqr), 
          data = df)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EZANB ~ s(ppt), 
          data = df)
summary(T2)
plot(T2)
#That is not really convincing.

T3 <- gam(EZANB ~ s(tmax), 
          data = df)
summary(T3)
plot(T3)
#That is not really convincing.

T4 <- gam(EZANB ~ s(doy), 
          data = df)
summary(T4)
plot(T4)
#That is interesting.

T5 <- boxplot(EZANB ~ waterbody_type, 
          data = df)
summary(T5)
plot(T5)
#That is not really convincing.

# Plot residuals vs each covariate not in the model.
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
boxplot(EZANB ~ fyear, 
        xlab = "Year",
        ylab = "Pearson residuals",
        data = df)
abline(h = 0, lty = 2, col = "red")    

plot(EZANB ~ month,
     xlab = "Month",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")  

plot(EZANB ~ tmin,
     xlab = "Average minimum temperature",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")  

plot(EZANB ~ ws,
     xlab = "Wind speed",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")  

plot(EZANB ~ q,
     xlab = "Discharge",
     ylab = "Pearson residuals",
     data = df)
abline(h = 0, lty = 2, col = "red")  

# Fit a smoother on the residuals and see whether it tells us something.
par(mfrow = c(2,3), mar= c(5,5,2,2), cex.lab = 1.5)
T1 <- gam(EZANB ~ s(tmin), 
          data = df)
summary(T1)
plot(T1)
#That is not really convincing.

T2 <- gam(EZANB ~ s(ws), 
          data = df)
summary(T2)
plot(T2)
#That is interesting.

T3 <- gam(EZANB ~ s(q), 
          data = df)
summary(T3)
plot(T3)
#That is interesting.

# also fit some box plots
boxplot(EZANB ~ fyear, data = df)
boxplot(EZANB ~ month, data = df)


# Spatial dependency?
# Let's make a variogram of the Pearson residuals.
# Sample-variogram with distances up to 300 km
par(mfrow = c(1,1), mar= c(5,5,2,2), cex.lab = 1.5)
mydata <- data.frame(EZANB, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
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
mydata <- data.frame(residuals_sim$scaledResiduals, df$Ykm, df$Xkm)
coordinates(mydata)    <- c("df.Ykm", "df.Xkm")
Vario.ZANB <- variogram(residuals_sim$scaledResiduals ~ 1, mydata, cutoff = 150, cressie = TRUE)
plot(Vario.ZANB, 
     main = "", 
     xlab = list(label = "Distance (km)", cex = 1.5), 
     ylab = list(label = "Semi-variogram", cex = 1.5), 
     pch = 16, col = 1, cex = 1.4)
# Is this a horizontal band of points? YESSSSS :D!

#========== Model selection of ZANB model ==========
#===== Optimal random structure =====
fZA.mesh2.A  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos + 
  # f(wpos, model = spde2) +
  Intercept.01 + 
  eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 +
  f(w01, model = spde2)

fZA.mesh2.B  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos + 
  f(wpos, model = spde2) +
  Intercept.01 + 
  eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 # + 
  # f(w01, model = spde2)

fZA.mesh2.C  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + ppt.pos + tmax.pos + doy.pos + waterbody_type.pos + 
  # f(wpos, model = spde2) +
  Intercept.01 + 
  eqr.01 + ppt.01 + tmax.01 + doy.01 + waterbody_type.01 # +
  # f(w01, model = spde2)


# Run model
ZANB.mesh2A <- inla(fZA.mesh2.A,
                    family = c("zeroinflatednbinomial0", "binomial"),
                    control.family = list(list(hyper = HyperZap),
                                          list()),
                    data = inla.stack.data(Stack.ZA.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(
                      link = 1,
                      A = inla.stack.A(Stack.ZA.mesh2)))

ZANB.mesh2B <- inla(fZA.mesh2.B,
                    family = c("zeroinflatednbinomial0", "binomial"),
                    control.family = list(list(hyper = HyperZap),
                                          list()),
                    data = inla.stack.data(Stack.ZA.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(
                      link = 1,
                      A = inla.stack.A(Stack.ZA.mesh2)))

ZANB.mesh2C <- inla(fZA.mesh2.C,
                    family = c("zeroinflatednbinomial0", "binomial"),
                    control.family = list(list(hyper = HyperZap),
                                          list()),
                    data = inla.stack.data(Stack.ZA.mesh2),
                    control.compute = list(dic = TRUE, waic = TRUE),
                    control.predictor = list(
                      link = 1,
                      A = inla.stack.A(Stack.ZA.mesh2)))

summary(ZANB.mesh2)

DIC.ZANB.mesh2.A = sum(tapply(ZANB.mesh2A$dic$local.dic, ZANB.mesh2A$dic$family, sum))
DIC.ZANB.mesh2.B = sum(tapply(ZANB.mesh2B$dic$local.dic, ZANB.mesh2B$dic$family, sum))
DIC.ZANB.mesh2.C = sum(tapply(ZANB.mesh2C$dic$local.dic, ZANB.mesh2C$dic$family, sum))

WAIC.ZANB.mesh2.A = sum(tapply(ZANB.mesh2A$waic$local.waic, ZANB.mesh2A$dic$family, sum))
WAIC.ZANB.mesh2.B = sum(tapply(ZANB.mesh2B$waic$local.waic, ZANB.mesh2B$dic$family, sum))
WAIC.ZANB.mesh2.C = sum(tapply(ZANB.mesh2C$waic$local.waic, ZANB.mesh2C$dic$family, sum))

Z <- matrix(nrow = 4, ncol = 2)
Z[,1] <- c(DIC.ZANB.mesh2, DIC.ZANB.mesh2.A, DIC.ZANB.mesh2.B, DIC.ZANB.mesh2.C)
Z[,2] <- c(WAIC.ZANB.mesh2, WAIC.ZANB.mesh2.A, WAIC.ZANB.mesh2.B, WAIC.ZANB.mesh2.C)
colnames(Z) <- c("DIC", "WAIC")
rownames(Z) <- c("Zero trunc (count) & binary", "binary", "Zero trunc", "None")
Z

# the ZANB model with spatial correlation in both the count and binary parts of the model is preffered.

#===== Optimal fixed structure =====
# Model selection fixed components
# Just like the step function in R, we will fix the variance
# parameter k of the negative binomial distribution. Formulated
# differently, we will use the k from the full model in any sub-model.
# step() and anova() do the same!
# Hyper.NB <- list(size = list(initial = 1, fixed = TRUE))
ZANB.mesh2$summary.hyper[1,"mean"]
HyperZap <- list(theta = list(initial = -10, fixed = TRUE),
                 size = list(initial = 0.471187, fixed = TRUE))  # dont know what to do here :/

# HyperZap <- list(theta = list(initial = -10, fixed = TRUE))

#==== Round 1 ====
# Round 1: dropped one variable from each model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# tmax from count part
fZA.mesh2.3  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# doy from count part
fZA.mesh2.4  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# waterbpdy_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# ppt from binary part
fZA.mesh2.7  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  tmax.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# tmax from binary part
fZA.mesh2.8  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# waterbody_type from binary part
fZA.mesh2.10  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  tmax.01 + 
  doy.01 + 
  f(w01, model = spde2)

# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.3 <- inla(fZA.mesh2.3,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.4 <- inla(fZA.mesh2.4,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.7 <- inla(fZA.mesh2.7,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.8 <- inla(fZA.mesh2.8,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))

ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))

ZANB.mesh2.10 <- inla(fZA.mesh2.10,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2 = sum(tapply(ZANB.mesh2$dic$local.dic, ZANB.mesh2$dic$family, sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.3 = sum(tapply(ZANB.mesh2.3$dic$local.dic, ZANB.mesh2.3$dic$family, sum))
DIC.ZANB.mesh2.4 = sum(tapply(ZANB.mesh2.4$dic$local.dic, ZANB.mesh2.4$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.7 = sum(tapply(ZANB.mesh2.7$dic$local.dic, ZANB.mesh2.7$dic$family, sum))
DIC.ZANB.mesh2.8 = sum(tapply(ZANB.mesh2.8$dic$local.dic, ZANB.mesh2.8$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))
DIC.ZANB.mesh2.10 = sum(tapply(ZANB.mesh2.10$dic$local.dic, ZANB.mesh2.10$dic$family, sum))

Z <- matrix(nrow = 11, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.3, DIC.ZANB.mesh2.4, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.7, DIC.ZANB.mesh2.8, DIC.ZANB.mesh2.9, DIC.ZANB.mesh2.10)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "tmax count part",
                 "doy count part",
                 "waterbody_type count part",
                 "eqr binary part", 
                 "ppt binary part", 
                 "tmax binary part",
                 "doy binary part",
                 "waterbody_type binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"]
print(delta_DIC)

# DIC
#                                 DIC
# Full model                 8425.929
# eqr count part             8432.966
# ppt count part             8443.737
# tmax count part            8427.474
# doy count part             8426.364
# waterbody_type count part  8424.668
# eqr binary part            8567.885
# ppt binary part            8426.199
# tmax binary part           8424.286 <----- drop
# doy binary part            8426.921
# waterbody_type binary part 8426.104

#==== Round 2 ====
# Round 2: dropped tmax from binary model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# tmax from count part
fZA.mesh2.3  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# doy from count part
fZA.mesh2.4  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# waterbpdy_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  ppt.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# ppt from binary part
fZA.mesh2.7  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  waterbody_type.01 + 
  f(w01, model = spde2)

# waterbody_type from binary part
fZA.mesh2.10  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.3 <- inla(fZA.mesh2.3,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.4 <- inla(fZA.mesh2.4,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.7 <- inla(fZA.mesh2.7,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.10 <- inla(fZA.mesh2.10,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2 = sum(tapply(ZANB.mesh2$dic$local.dic, ZANB.mesh2$dic$family, sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.3 = sum(tapply(ZANB.mesh2.3$dic$local.dic, ZANB.mesh2.3$dic$family, sum))
DIC.ZANB.mesh2.4 = sum(tapply(ZANB.mesh2.4$dic$local.dic, ZANB.mesh2.4$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.7 = sum(tapply(ZANB.mesh2.7$dic$local.dic, ZANB.mesh2.7$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))
DIC.ZANB.mesh2.10 = sum(tapply(ZANB.mesh2.10$dic$local.dic, ZANB.mesh2.10$dic$family, sum))

Z <- matrix(nrow = 10, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.3, DIC.ZANB.mesh2.4, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.7, DIC.ZANB.mesh2.9, DIC.ZANB.mesh2.10)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "tmax count part",
                 "doy count part",
                 "waterbody_type count part",
                 "eqr binary part", 
                 "ppt binary part", 
                 "doy binary part",
                 "waterbody_type binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
(delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"])


# DIC
#                                 DIC
# Full model                 8424.250
# eqr count part             8431.456
# ppt count part             8440.477
# tmax count part            8426.774
# doy count part             8424.653
# waterbody_type count part  8422.922
# eqr binary part            8567.323
# ppt binary part            8424.285
# doy binary part            8425.583
# waterbody_type binary part 8422.791 <----- drop


#==== Round 3 ====
# Round 3: dropped waterbody_type from binary model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# tmax from count part
fZA.mesh2.3  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# doy from count part
fZA.mesh2.4  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# waterbpdy_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from binary part
fZA.mesh2.7  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  doy.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  f(w01, model = spde2)


# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.3 <- inla(fZA.mesh2.3,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.4 <- inla(fZA.mesh2.4,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.7 <- inla(fZA.mesh2.7,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2   = sum(tapply(ZANB.mesh2$dic$local.dic,   ZANB.mesh2$dic$family,   sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.3 = sum(tapply(ZANB.mesh2.3$dic$local.dic, ZANB.mesh2.3$dic$family, sum))
DIC.ZANB.mesh2.4 = sum(tapply(ZANB.mesh2.4$dic$local.dic, ZANB.mesh2.4$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.7 = sum(tapply(ZANB.mesh2.7$dic$local.dic, ZANB.mesh2.7$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))

Z <- matrix(nrow = 9, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.3, DIC.ZANB.mesh2.4, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.7, DIC.ZANB.mesh2.9)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "tmax count part",
                 "doy count part",
                 "waterbody_type count part",
                 "eqr binary part", 
                 "ppt binary part", 
                 "doy binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
(delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"])


# DIC
#                                DIC
# Full model                8424.487
# eqr count part            8428.125
# ppt count part            8439.130
# tmax count part           8423.103
# doy count part            8421.497 <----- drop
# waterbody_type count part 8423.534
# eqr binary part           8565.612
# ppt binary part           8421.655
# doy binary part           8424.567


#==== Round 4 ====
# Round 4: dropped doy from count part model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# tmax from count part
fZA.mesh2.3  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# waterbpdy_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  ppt.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from binary part
fZA.mesh2.7  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  ppt.01 + 
  f(w01, model = spde2)


# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.3 <- inla(fZA.mesh2.3,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.7 <- inla(fZA.mesh2.7,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2   = sum(tapply(ZANB.mesh2$dic$local.dic,   ZANB.mesh2$dic$family,   sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.3 = sum(tapply(ZANB.mesh2.3$dic$local.dic, ZANB.mesh2.3$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.7 = sum(tapply(ZANB.mesh2.7$dic$local.dic, ZANB.mesh2.7$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))

Z <- matrix(nrow = 8, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.3, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.7, DIC.ZANB.mesh2.9)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "tmax count part",
                 "waterbody_type count part",
                 "eqr binary part", 
                 "ppt binary part", 
                 "doy binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
(delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"])


# DIC
#                                DIC
# Full model                8421.498
# eqr count part            8427.373
# ppt count part            8438.236
# tmax count part           8421.349
# waterbody_type count part 8430.820
# eqr binary part           8564.679
# ppt binary part           8421.022 <----- drop
# doy binary part           8424.377


#==== Round 5 ====
# Round 5: dropped ppt from binary part model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# tmax from count part
fZA.mesh2.3  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# waterbody_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  doy.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  tmax.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  f(w01, model = spde2)


# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.3 <- inla(fZA.mesh2.3,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2   = sum(tapply(ZANB.mesh2$dic$local.dic,   ZANB.mesh2$dic$family,   sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.3 = sum(tapply(ZANB.mesh2.3$dic$local.dic, ZANB.mesh2.3$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))

Z <- matrix(nrow = 7, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.3, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.9)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "tmax count part",
                 "waterbody_type count part",
                 "eqr binary part", 
                 "doy binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
(delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"])


# DIC
#                                DIC
# Full model                8421.100
# eqr count part            8426.882
# ppt count part            8437.217
# tmax count part           8420.182 <----- drop
# waterbody_type count part 8427.648
# eqr binary part           8564.226
# doy binary part           8422.886



#==== Round 6 ====
# Round 6: dropped tmax from count part model
#	Specify the model formula
# full model
fZA.mesh2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from count part
fZA.mesh2.1  <- AllY ~ -1 + 
  Intercept.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# ppt from count part
fZA.mesh2.2  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# waterbody_type from count part
fZA.mesh2.5  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  doy.01 + 
  f(w01, model = spde2)

# eqr from binary part
fZA.mesh2.6  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  doy.01 + 
  f(w01, model = spde2)

# doy from binary part
fZA.mesh2.9  <- AllY ~ -1 + 
  Intercept.pos + 
  eqr.pos + 
  ppt.pos + 
  waterbody_type.pos + 
  f(wpos, model = spde2) + 
  
  Intercept.01 + 
  eqr.01 + 
  f(w01, model = spde2)


# fit the models
ZANB.mesh2 <- inla(fZA.mesh2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.1 <- inla(fZA.mesh2.1,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.2 <- inla(fZA.mesh2.2,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.5 <- inla(fZA.mesh2.5,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.6 <- inla(fZA.mesh2.6,
                   family = c("zeroinflatednbinomial0", "binomial"),  
                   control.family = list(list(hyper = HyperZap),
                                         list()),
                   data = inla.stack.data(Stack.ZA.mesh2),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(
                     link = 1,
                     A = inla.stack.A(Stack.ZA.mesh2)))


ZANB.mesh2.9 <- inla(fZA.mesh2.9,
                     family = c("zeroinflatednbinomial0", "binomial"),  
                     control.family = list(list(hyper = HyperZap),
                                           list()),
                     data = inla.stack.data(Stack.ZA.mesh2),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(
                       link = 1,
                       A = inla.stack.A(Stack.ZA.mesh2)))


DIC.ZANB.mesh2   = sum(tapply(ZANB.mesh2$dic$local.dic,   ZANB.mesh2$dic$family,   sum))
DIC.ZANB.mesh2.1 = sum(tapply(ZANB.mesh2.1$dic$local.dic, ZANB.mesh2.1$dic$family, sum))
DIC.ZANB.mesh2.2 = sum(tapply(ZANB.mesh2.2$dic$local.dic, ZANB.mesh2.2$dic$family, sum))
DIC.ZANB.mesh2.5 = sum(tapply(ZANB.mesh2.5$dic$local.dic, ZANB.mesh2.5$dic$family, sum))
DIC.ZANB.mesh2.6 = sum(tapply(ZANB.mesh2.6$dic$local.dic, ZANB.mesh2.6$dic$family, sum))
DIC.ZANB.mesh2.9 = sum(tapply(ZANB.mesh2.9$dic$local.dic, ZANB.mesh2.9$dic$family, sum))

Z <- matrix(nrow = 6, ncol = 1)
Z[,1] <- c(DIC.ZANB.mesh2, 
           DIC.ZANB.mesh2.1, DIC.ZANB.mesh2.2, DIC.ZANB.mesh2.5, 
           DIC.ZANB.mesh2.6, DIC.ZANB.mesh2.9)

colnames(Z) <- c("DIC")
rownames(Z) <- c("Full model", 
                 "eqr count part", 
                 "ppt count part", 
                 "waterbody_type count part",
                 "eqr binary part", 
                 "doy binary part")
Z
(model2drop <- rownames(Z)[which.min(Z)])
(delta_DIC <- Z["Full model", "DIC"] - Z[model2drop, "DIC"])


# DIC
#                                DIC
# Full model                8419.968
# eqr count part            8425.868
# ppt count part            8435.751
# waterbody_type count part 8429.694
# eqr binary part           8564.144
# doy binary part           8423.203


#========== Model validation of optimal ZANB model ==========
# Lets check out the covariates
summary(ZANB.mesh2)
ZANB.mesh2$summary.fixed[, c("mean", "0.025quant", "0.975quant")]

# Getting fitted values and Pearson residuals
# Fitted values of the ZANB, mesh 1
RowsPos <- inla.stack.index(Stack.ZA.mesh2, tag='FitPos')$data
Rows01  <- inla.stack.index(Stack.ZA.mesh2, tag='Fit01')$data

mu.ZTruncPois <- ZANB.mesh2$summary.fitted.values[RowsPos, "mean"]
Pi            <- ZANB.mesh2$summary.fitted.values[Rows01, "mean"]

BetaPos <- ZANB.mesh2$summary.fixed[1:4,"mean"]
Beta01  <- ZANB.mesh2$summary.fixed[5:7,"mean"]
k       <- ZANB.mesh2$summary.hyper[1,"mean"]
Xpos    <- as.matrix(X[, c(1:3, 6),])
X01.1   <- as.matrix(X01[, c(1:2, 5)])

A2.m <- as.matrix(A2)
wPos   <- ZANB.mesh2$summary.random$wpos$mean 
w01    <- ZANB.mesh2$summary.random$w01$mean 

mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
P0 <- (k / (mu + k))^k
Pi.self <- exp(X01.1 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01.1 %*% Beta01 + A2.m %*% w01))

muZANB  <- (Pi.self / (1 - P0))  * mu
varZANB <- (Pi.self / (1 - P0))  * (mu^2 + mu + mu^2 / k ) - (  Pi.self / (1 - P0) * mu  )^2

EZANB <- (df$vec_abund - muZANB) / sqrt(varZANB)
p <- length(ZANB.mesh2$names.fixed)
# p <- 2 * (8 + 2 + 2)
sum(EZANB^2) / (nrow(df) - p)
# 0.04095438 That is.... not overdispersion! :')))

# Model validation
par(mar = c(5,5,2,2), cex.lab = 1.5)
plot(x  = muZANB,
     y = df$vec_abund,
     xlab = "Fitted values ZANB model",
     ylab = "Observed vector numbers",
     xlim = c(0, 150),
     ylim = c(0, 150))

# Simulation study ZINB model with spatial correlation
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
N              <- nrow(df)
ZerosZANBSpat  <- vector(length = NSim)
YZANBSpat      <- matrix(nrow = N, ncol = NSim)

MyParams.Pos <- colnames(Xpos)
RowNum.Pos   <- lapply(MyParams.Pos, MyID_beta)
RowNum.Pos   <- as.numeric(RowNum.Pos)
RowNum.Pos

MyParams.01 <- colnames(X01.1)
RowNum.01   <- lapply(MyParams.01, MyID_beta)
RowNum.01   <- as.numeric(RowNum.01)
RowNum.01

N2 <- mesh2$n # mesh2$n - 1
MyParams.wPos <- paste("wpos", 1:N2, sep = ":")
RowNum.wPos <- lapply(MyParams.wPos, MyID_w)
RowNum.wPos <- as.numeric(RowNum.wPos)
RowNum.wPos

N2 <- mesh2$n # mesh2$n - 1
MyParams.w01 <- paste("w01", 1:N2, sep = ":")
RowNum.w01 <- lapply(MyParams.w01, MyID_w)
RowNum.w01 <- as.numeric(RowNum.w01)
RowNum.w01

A2.m   <- as.matrix(A2)
k   <- SimData[[1]]$hyperpar[1]

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = NSim, style = 3)

for (i in 1:NSim) {
  BetaPos <- SimData[[i]]$latent[RowNum.Pos]
  Beta01 <- SimData[[i]]$latent[RowNum.01]
  wPos <- SimData[[i]]$latent[RowNum.wPos]
  w01 <- SimData[[i]]$latent[RowNum.w01]
  k   <- SimData[[i]]$hyperpar[1] # strictly speaking, simulated values should be used, but there is a tendency for k parameters to not look very random
  
  mu <- exp(Xpos %*% BetaPos + A2.m %*% wPos)
  Pi <- exp(X01.1 %*% Beta01 + A2.m %*% w01) / (1 + exp(X01.1 %*% Beta01 + A2.m %*% w01))
  
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
points(x = sum(df$vec_abund == 0), 
       y = 0, 
       pch = 16, 
       cex = 5, 
       col = 2)

sum(sum(df$vec_abund == 0) > ZerosZANBSpat) / 10000
sum(df$vec_abund == 0)

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
observed_response <- df$vec_abund
anyNA(observed_response)

library(DHARMa)
# Use DHARMa to create scaled quantile residuals
residuals_sim <- createDHARMa(
  simulatedResponse = simulated_response,     # Matrix of simulated responses
  observedResponse = observed_response,       # Observed response (abundance)
  integerResponse = TRUE                      # TRUE for count data
)

# Residual diagnostics
plot(residuals_sim)

# Histogram of residuals
hist(residuals_sim, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals", breaks = 20, col = "lightblue", border = "white")
scaled_residuals <- residuals(residuals_sim)
hist(scaled_residuals, main = "Histogram of Scaled Residuals", xlab = "Scaled Residuals", breaks = 20, col = "lightblue", border = "white")

# Residuals vs. Predictors
predictors <- data.frame(EQR            = df$eqr, 
                         PPT            = df$ppt, 
                         TMAX           = df$tmax,
                         DOY            = df$doy,
                         WATERBODY_TYPE = df$waterbody_type)

# Plot residuals vs. each predictor
par(mfrow = c(2, 3), mar = c(5,5,2,2), cex.lab = 1.5)
for (i in 1:ncol(predictors)) {
  plot(predictors[, i], scaled_residuals,
       xlab = colnames(predictors)[i],
       ylab = "Scaled Residuals",
       main = paste("Residuals vs", colnames(predictors)[i]),
       pch = 19, col = "blue")
  abline(h = 0, col = "red")  # Add a horizontal line at y = 0
}

# Results of the ZANB model with spatial correlation
wpm.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$mean  
wpm.ZANB.01  <- ZANB.mesh2$summary.random$w01$mean  
wsd.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$sd  
wsd.ZANB.01  <- ZANB.mesh2$summary.random$w01$sd  

# Plot the spatial random field again, and add white space for the non-study area
par(mfrow = c(2, 2), mar = c(5,5,2,2), cex.lab = 1.5)
PlotField(field = wpm.ZANB.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

# Binary part of the ZANB
PlotField(field = wpm.ZANB.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

PlotField(field = wsd.ZANB.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
# plot(Water.UTM  , add = TRUE)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)



#========== Extract important data from optimal ZANB model ==========
#===== Plot spatial random field ===== 
# Results of the ZANB model with spatial correlation
wpm.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$mean  
wpm.ZANB.01  <- ZANB.mesh2$summary.random$w01$mean  
wsd.ZANB.Pos <- ZANB.mesh2$summary.random$wpos$sd  
wsd.ZANB.01  <- ZANB.mesh2$summary.random$w01$sd  

# count part of the model
options(scipen = 999)
# png("C:/Users/natha/OneDrive/University of Johannesburg/Post-Doc/Nature Research Centre/How Changes in Bioversity Change Biodiversity/Seminars/October 2024/ZANBwSpatial_Count.png",
#     width = 10, height = 10, unit = "in", res = 450)
# svg("C:/Users/natha/OneDrive/University of Johannesburg/Post-Doc/Nature Research Centre/How Changes in Bioversity Change Biodiversity/Seminars/October 2024/ZANBwSpatial_Count.svg",
#     width = 10, height = 10)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)

PlotField(field = wpm.ZANB.Pos, mesh = mesh2,
          xlim = range(mesh2$loc[,1]),
          ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
plot(Land2, col = "white", add = TRUE, border = "white")
plot(Lithuania.UTM, add = TRUE)
text(x = min(range(mesh2$loc[,1]) + 210000),  # x position in the middle of the x-range
     y = min(range(mesh2$loc[,2]) + 1),  # y position in the middle of the y-range
     labels = paste("Mean SD of spatial random field:", round(mean(ZANB.mesh2$summary.random$wpos$sd), 2)),
     cex = 1.5,
     col = "Black")
# dev.off()

PlotField(field = wsd.ZANB.Pos, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

# Binary part of the ZANB
options(scipen = 999)
# png("C:/Users/natha/OneDrive/University of Johannesburg/Post-Doc/Nature Research Centre/How Changes in Bioversity Change Biodiversity/Seminars/October 2024/ZANBwSpatial_Binary.png",
#     width = 10, height = 10, unit = "in", res = 450)
# svg("C:/Users/natha/OneDrive/University of Johannesburg/Post-Doc/Nature Research Centre/How Changes in Bioversity Change Biodiversity/Seminars/October 2024/ZANBwSpatial_Binary.svg",
#     width = 10, height = 10)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2), cex.lab = 1.5)

PlotField(field = wpm.ZANB.01, mesh = mesh2,
          xlim = range(mesh2$loc[,1]),
          ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2],
       cex = 0.5,
       col = "black",
       pch = 16)
plot(Land2, col = "white", add = TRUE, border = "white")
plot(Lithuania.UTM, add = TRUE)
text(x = min(range(mesh2$loc[,1]) + 210000),  # x position in the middle of the x-range
     y = min(range(mesh2$loc[,2]) + 1),  # y position in the middle of the y-range
     labels = paste("Mean SD of spatial random field:", round(mean(ZANB.mesh2$summary.random$w01$sd), 2)),
     cex = 1.5,
     col = "Black")
# dev.off()

PlotField(field = wsd.ZANB.01, mesh = mesh2, xlim = range(mesh2$loc[,1]), ylim = range(mesh2$loc[,2]))
points(x = Loc[,1],
       y = Loc[,2], 
       cex = 0.5, 
       col = "black", 
       pch = 16)
plot(Land2, 
     col = "white", 
     add = TRUE,
     border = "white")
plot(Lithuania.UTM, add = TRUE)

#===== Make neat table of model outputs ===== 
summary(ZANB.mesh2)

# Extract fixed effects
fixed_effects <- ZANB.mesh2$summary.fixed
print(fixed_effects)

# Extract random effects
random_effects <- ZANB.mesh2$summary.random
print(random_effects)

# Extract model hyperparameters
hyperparameters <- ZANB.mesh2$summary.hyperpar
print(hyperparameters)

# Extract DIC and WAIC (model fit metrics)
dic <- ZANB.mesh2$dic$dic
waic <- ZANB.mesh2$waic$waic
effective_params <- ZANB.mesh2$dic$p.eff  # Effective number of parameters from DIC

# Load necessary libraries
library(gt)

# Separate Positive Fixed Effects (those with ".pos" in the name)
pos_fixed_effects <- fixed_effects[grepl("pos", rownames(fixed_effects)), ]
pos_fixed_table <- data.frame(
  Term = rownames(pos_fixed_effects),
  Mean = pos_fixed_effects$mean,
  SD = pos_fixed_effects$sd,
  Lower_95_CI = pos_fixed_effects$`0.025quant`,  # Renaming columns for valid names
  Upper_95_CI = pos_fixed_effects$`0.975quant`
)

# Separate Binary (Zero-inflated) Fixed Effects (those with ".01" in the name)
bin_fixed_effects <- fixed_effects[grepl("01", rownames(fixed_effects)), ]
bin_fixed_table <- data.frame(
  Term = rownames(bin_fixed_effects),
  Mean = bin_fixed_effects$mean,
  SD = bin_fixed_effects$sd,
  Lower_95_CI = bin_fixed_effects$`0.025quant`,  # Renaming columns for valid names
  Upper_95_CI = bin_fixed_effects$`0.975quant`
)

# Function to create and save gt table as PNG
save_gt_table <- function(data, title, filename) {
  table <- gt(data) |>
    tab_header(title = md(title)) |>
    fmt_number(columns = 2:5, decimals = 3) |>
    cols_label(
      Term = "Term",
      Mean = "Mean",
      SD = "Standard Deviation",
      Lower_95_CI = "Lower 95% CI",
      Upper_95_CI = "Upper 95% CI"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) |>
    opt_table_outline() |>
    opt_row_striping() |>
    gtsave(filename = filename, expand = 10)  # Save as PNG
}

# Save the Positive Fixed Effects table
save_gt_table(
  data = pos_fixed_table,
  title = "Positive Fixed Effects",
  filename = "Tables/positive_fixed_effects.png"
)

# Save the Binary Fixed Effects table
save_gt_table(
  data = bin_fixed_table,
  title = "Binary Fixed Effects (Zero-inflated Model)",
  filename = "Tables/binary_fixed_effects.png"
)

# Extract the Range and sigma_u from the model's hyperparameters
range_km <- ZANB.mesh2$summary.hyperpar[c(3, 5), "mean"] / 1000  # Convert range mean to kilometers
range_sd_km <- ZANB.mesh2$summary.hyperpar[c(3, 5), "sd"] / 1000  # Convert range sd to kilometers
range_lower_km <- ZANB.mesh2$summary.hyperpar[c(3, 5), "0.025quant"] / 1000  # Convert lower 95% CI to kilometers
range_upper_km <- ZANB.mesh2$summary.hyperpar[c(3, 5), "0.975quant"] / 1000  # Convert upper 95% CI to kilometers

# Extract sigma_u values (no conversion needed)
sigma_u <- ZANB.mesh2$summary.hyperpar[c(4, 6), "mean"]
sigma_u_sd <- ZANB.mesh2$summary.hyperpar[c(4, 6), "sd"]
sigma_u_lower <- ZANB.mesh2$summary.hyperpar[c(4, 6), "0.025quant"]
sigma_u_upper <- ZANB.mesh2$summary.hyperpar[c(4, 6), "0.975quant"]

# Create a data frame for the custom hyperparameters table
custom_hyper_table <- data.frame(
  Parameter = c("Range for wpos (km)", "Range for w01 (km)", 
                "Stdev for wpos (sigma_u)", "Stdev for w01 (sigma_u)"),
  Mean = c(range_km, sigma_u),
  SD = c(range_sd_km, sigma_u_sd),
  Lower_95_CI = c(range_lower_km, sigma_u_lower),
  Upper_95_CI = c(range_upper_km, sigma_u_upper)
)

# Function to create and save gt table as PNG
save_gt_custom_hyper_table <- function(data, title, filename) {
  table <- gt(data) |>
    tab_header(title = md(title)) |>
    fmt_number(columns = 2:5, decimals = 3) |>
    cols_label(
      Parameter = "Parameter",
      Mean = "Mean",
      SD = "Standard Deviation",
      Lower_95_CI = "Lower 2.5% CI",
      Upper_95_CI = "Upper 97.5% CI"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) |>
    opt_table_outline() |>
    opt_row_striping() |>
    gtsave(filename = filename, expand = 10)  # Save as PNG
}

# Function to save the diagnostic table
save_gt_diagnostic_table <- function(data, title, filename) {
  table <- gt(data) |>
    tab_header(title = md(title)) |>
    fmt_number(columns = 2, decimals = 2) |>
    cols_label(
      Metric = "Metric",
      Value = "Value"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) |>
    opt_table_outline() |>
    opt_row_striping() |>
    gtsave(filename = filename, expand = 10)  # Save as PNG
}

# Save the custom hyperparameters table
save_gt_custom_hyper_table(
  data = custom_hyper_table,
  title = "Key Model Hyperparameters (Range in km, sigma_u)",
  filename = "Tables/key_hyperparameters.png"
)

# # Save the Diagnostic Data table
# save_gt_diagnostic_table(
#   data = diagnostic_table,
#   title = "Model Diagnostics",
#   filename = "Tables/model_diagnostics.png"
# )


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