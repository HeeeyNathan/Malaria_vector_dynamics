#'    Highland Statistics Ltd.
#'    www.highstat.com

#'    This program is distributed in the hope that it will be useful,
#'    but WITHOUT ANY WARRANTY; without even the implied warranty of
#'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



# Section 1: Data description----

#' The data used in this exercise were provided by Federico Cortés
#' (INIDEP, Mar del Plata, Argentina). In Zuur et al. (2012), zero
#' inflated counts of three skate species, Sympterygia bonapartii,
#' Rioraja agassizi and Atlantoraja castelnaui, were analysed.

#' The data were collected from the Rio de la Plata Basin, a major
#' river basin of the Argentine coast. Chondrichthyan fishes
#' (sharks, skates, rays, and chimaeras) use the Río de la Plata
#' coastal region as a feeding, mating, egg laying, and nursery area.
#' Skates are a ubiquitous group in the region (Massa et al., 2004) and,
#' as benthic mesopredators, play an important functional role in
#' structuring the benthic system (Myers et al., 2007).

#' Skates of the area are represented by species with a variation of
#' life history traits (total length, age and length at maturity, and
#' reproductive frequency), and it can be expected that environmental
#' variables play a role in their distribution. The effects of these
#' environmental factors have not been quantified.

#' The 2012 book chapter modelled the distribution and habitat
#' preferences of three skate species using fishery survey data of
#' the Rio de la Plata coastal region. Counts of 3 skate species were
#' analysed using Poisson GLMM, NB GLMM and zero inflated models.



#' In this exercise, we will use two species:
#'   RA (Rioraja agassizii, Rio skate ).
#'   SB (Sympterygia bonapartii; Smallnose fanskate).

#' But we could have done the exercise with all three species.


#'  We will ask the following questions:
#'    -Do these species respond in the same way to the covariates? E.g.  Is
#'     the effect of temperature the same, or does it differ per species?
#'     Or: Do both species have the same temporal trend?

#'    -In the spatial pattern the same for both species, or does it differ
#'     per species? If it does differ, where in space?




# Section 2: Import the data and load the packages----


#* Subsection 2.1: Load the data----

setwd("C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/MOBILR Project (2024-2027)/R-course/Spatio-temporal regression modelling using R-INLA/AllData_OnlineSpaTemGLMInla/AllData")
Skates <- read.table(file = "Skate2.txt",
                     header = TRUE,
                     stringsAsFactors = TRUE,
                     dec = ".")
dim(Skates)
names(Skates)
str(Skates)





#* Subsection 2.2: Load packages----

#' We need the following packages:
library(lattice)
library(ggplot2)
library(sf)
library(gstat)
library(reshape)
library(mgcv)
library(gratia)
library(DHARMa)
library(plyr)
library(dplyr)
library(tidyr)
library(scales)
library(MASS)
library(rnaturalearth)
library(INLA)
library(fmesher)
source("HighstatLibV15.R") #' Our support file.




# Section 3: Prepare the data----

#* Subsection 3.1: Defining the response variables----

Skates$RA01 <- ifelse(Skates$RA > 0, 1, 0)
Skates$SB01 <- ifelse(Skates$SB > 0, 1, 0)

# need to repeat the data exploration for each individual species

#* Subsection 3.2: Defining the categorical covariates----

Skates$fYear       <- factor(Skates$Year)
Skates$fMonth      <- factor(Skates$Month)
Skates$fBottomType <- factor(Skates$BottomType,
                             levels = c(1, 2, 3, 4),
                             labels = c("Mud",
                                        "SandMud",
                                        "Sand",
                                        "SandShellRest"))



#* Subsection 3.3: Get UTM coordinates----

#' We have longitude and latitude. We also want Xkm and Ykm. This is
#' UTM zone 21 on the southern hemisphere.
#' Convert the spatial coordinates to spatial data frame.
sf_obj <- st_as_sf(Skates,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)  # WGS84

#' Transform to UTM Zone 21S.
sf_obj_utm <- st_transform(sf_obj, 32721)

#' Extract Xkm and Ykm.
Skates$Xkm <- st_coordinates(sf_obj_utm)[, 1] / 1000  #' Convert to kilometers
Skates$Ykm <- st_coordinates(sf_obj_utm)[, 2] / 1000  #' Convert to kilometers

head(Skates[,c("Longitude", "Latitude", "Xkm", "Ykm")])




#* Subsection 3.4: Standardise covariates----

#' It is advisable to always standardise covariates.
Skates$Temp.std      <- MyStd(Skates$Temperature)
Skates$SweptArea.std <- MyStd(Skates$SweptArea)




#* Subsection 3.5: Get the coastline----


#' Get the border/coastline of Argentina and Uruguay
RiodelaPlataBasin <- ne_countries(scale = "large",
                         country = c('argentina', 'uruguay'),
                         returnclass = "sf")

ggplot() +
  geom_sf(data = RiodelaPlataBasin) +
  coord_sf(xlim = c(-50, -60), ylim = c(-30, -40)) +
  geom_point(data = Skates,
             aes(x = Longitude, y = Latitude),
             size = 0.1) +
  theme_minimal()


#' Next, we want to get the study area as we see it, without having
#' to use xlim and ylim commands. To do this, define a bounding box and
#' crop the bounding box and the Argentinian map.
bb <- st_bbox(c(xmin = -50, xmax = -59,
                ymin = -33, ymax = -40),
              crs = st_crs(RiodelaPlataBasin))

#' Crop RiodelaPlataBasin to the bounding box.
CroppedRiodelaPlataBasin <- st_crop(RiodelaPlataBasin, bb)


#' This is what we now have.
ggplot(data = CroppedRiodelaPlataBasin) +
  geom_sf(fill = "transparent") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = Skates,
             aes(x = Longitude, y = Latitude),
             alpha = 0.5,
             size = 0.5)
#' Note that we did not use any xlim and ylim commands.


#' Convert CroppedRiodelaPlataBasin to UTM.
CroppedRiodelaPlataBasin_UTM <- st_transform(x = CroppedRiodelaPlataBasin,
                                     crs = "+proj=utm +zone=21 +south +ellps=WGS84 +units=km +datum=WGS84")


#' Plot coastline + sampling locations using ggplot2
ggplot(data = CroppedRiodelaPlataBasin_UTM) +
  geom_sf(fill = "transparent") +
  geom_point(data = Skates,
             aes(x = Xkm, y = Ykm),
             size = 0.75,
             alpha = 0.5) +
  theme_minimal() +
  labs(title = "Study area in UTM")





#* Subsection 3.6: INLA specific setting----

#' We define some common settings for INLA.
MyControlCompute  <- list(config = TRUE,    #' Allow for posterior simulation
                          dic = TRUE,       #' Calculate AIC
                          waic = TRUE,      #' Calculate WAIC
                          residuals = TRUE) #' Get residuals (see below)
MyControlPredictor  <- list(compute = TRUE, #' Calculate fitted values
                            link = 1)       #' Predict on the scale of





# Section 4: Data exploration----

#' See the previous two exercises




# Section 5: Model formulation of the Bernoulli GLMs----


#' We will apply the following two Bernoulli GLMs:

#'   SB01_i ~ Bernoulli(Pi1_i)
#'   RA01_i ~ Bernoulli(Pi2_i)

#'   E[SB01_i] = Pi1_i
#'   var[SB01_i] = Pi1_i * (1 - Pi1_i)

#'   E[RA01_i] = Pi2_i
#'   var[RA01_i] = Pi2_i * (1 - Pi2_i)


#'   logit(Pi1_i) = Intercept + Temperature_i + SweptArea_i +
#'                  Year_i  + u1_i

#'   logit(Pi2_i) = Intercept + Temperature_i + SweptArea_i +
#'                  Year_i  + u2_i


#' To keep the code simple, we decided to drop BottomType_i.
#' That saves a lot of coding due to all the levels.

#' You can even have different covariates for each of the species.


#' We already know from previous analyses that both species need a
#' spatial random field.

#' We can fit these models in two separate steps (as we did before),
#' or we can also fit them in a multivariate GLM (without adding any
#' extra dependency...for the moment).
#' We will do that now. Why? Because the next step is to add shared
#' dependency.

#' THIS NEXT PART IS CRUCIAL FOR THIS ANALYSIS

head(Skates)
#' First, we need to recode the data.
#' Create the data in long-format by species.
SkatesLong <- data.frame(
  fYear         = rep(Skates$fYear, 2), # < here for me, it would be (dataset 1 $ lakes, dataset 2 $ rivers)
  Latitude      = rep(Skates$Latitude, 2),
  Longitude     = rep(Skates$Longitude, 2),
  Temp.std      = rep(Skates$Temp.std, 2),
  fBottomType   = rep(Skates$fBottomType, 2),
  SweptArea.std = rep(Skates$SweptArea.std, 2),
  RA01          = c(Skates$RA01, rep(NA, nrow(Skates))),
  SB01          = c(rep(NA, nrow(Skates)), Skates$SB01),
  Xkm           = rep(Skates$Xkm, 2),
  Ykm           = rep(Skates$Ykm, 2),
  Species       = rep(c("RA01", "SB01"), each = nrow(Skates)))

SkatesLong$Species <- factor(SkatesLong$Species )


# View the first rows of the new data frame
head(SkatesLong)
tail(SkatesLong)

# The species can be from different or the same sites, it does not matter

#' In this case, both species were sampled at the same sites. But
#' for what we just did, and for what is to come, this is not required.
#' All we need is two blocks of data under each other.




# Section 6: Multivariate independent Bernoulli GLMs----

#' We will implement the following 8 steps:
#'  1. Make a mesh.
#'  2. Define the weighting factors a_ik (i.e. the projector matrix A).
#'  3. Define the SPDE.
#'  4. Define the spatial field.
#'  5. Make a stack. It tells INLA at which points on the mesh we sampled
#'     the response variable and the covariates.
#'  6. Specify the model formula in terms of the response variable,
#'     covariates and the spatial correlated term.
#'  7. Run the spatial model in INLA.
#'  8. Inspect the results.



#* Subsection 6.1: Make a mesh----

#' In this section, we make one mesh. This will be based
#' on the data in SkatesLong. It is also possible to
#' make a mesh, projector matrix, priors, and stack for the
#' separate blocks (i.e. species), before combining the stacks.
#' In that case, each species can have its own priors (and mesh resolution).


#' We first need to get a sense what the distances are between the
#' sampling locations.
Loc <- cbind(SkatesLong$Xkm, SkatesLong$Ykm) # <- you can also make a mesh based on data 1 and then a mesh on data 2
D   <- dist(Loc)
par(mfrow = c(1,1), mar = c(5,5,2,2))
hist(D,
     freq = TRUE,
     main = "",
     xlab = "Distance between sites (km)",
     ylab = "Frequency")
#' Small scale for these data is anything between 0 and 100-ish km
#' In this specific example, distances between the sampling locations
#' for both species is identical. If they are not, then make a different
#' mesh per species.



#' fm_nonconvex_hull makes a non-convex area around the sampling
#' locations. You can control how close the blue line is to the
#' sites with the 'convex' argument.
NonConBoundary <- fm_nonconvex_hull(Loc, convex = -0.04)

#' Results
plot(NonConBoundary)
points(Loc)


#' Define the resolution of the mesh. Because we only use one mesh for
#' both species, there is only one MeshResolution.
MeshResolution <- 50               #' in km.
MaxEdge        <- MeshResolution / 5

#' Make the mesh
mesh <- fm_mesh_2d(boundary = NonConBoundary,
                   max.edge = c(1, 5) * MaxEdge,
                   cutoff   = MaxEdge / 5)

#' Size of mesh
mesh$n


#' Ensure the mesh has the same crs as CroppedRiodelaPlataBasin_UTM
fm_crs(mesh) <- st_crs(CroppedRiodelaPlataBasin_UTM)


#' Use ggplot to plot mesh.
ggplot() +
  theme_minimal() +
  labs(title = "Study area in UTM") +
  geom_fm(data = mesh) +
  geom_point(data = SkatesLong,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.5,
             size = 0.5)  +
  geom_sf(data = CroppedRiodelaPlataBasin_UTM,
          fill = "transparent",
          col = "red")

#' We will use this mesh for both species. If the spatial resolution
#' differs per block (c.q. species), then consider making two meshes,
#' each with different settings.




#* Subsection 6.2: Define the projector matrix A----

#' Define the projector matrix A for: u = A * w.
#' Right now, we only have one of these as there is only one mesh.
#' But if each block has its own set of locations, and you
#' are using two meshes, then you need two of these A matrices as well.
A <- inla.spde.make.A(mesh, loc = Loc)
dim(A)  #' 664 sites (2 x 332) and 3240 nodes in the mesh.





#* Subsection 6.3: Define the SPDE----

#' We need to specify priors for the two Matern correlation
#' parameters Range and sigma. Right now, both species use the same
#' priors. If the biology dictates that the spatial behaviour is
#' rather different for the two species, then consider using two different
#' sets of priors. That can still be done with one mesh.


#' We will use the following PC priors:
#'   P(Range < 75 km) = 0.05
#'   P(sigma > 2) = 0.05

#' In INLA coding, this is:
spde <- inla.spde2.pcmatern(mesh,
                            prior.range = c(75, 0.05),
                            prior.sigma = c(2, 0.05))

#' Continuing with the idea that species may have different spatial patterns,
#' we could have made a spde1 and an spde2 with different priors (with
#' the same mesh)




#* Subsection 6.4: Define the spatial field----

# HERE IS WHERE YOU WILL DEFINE THE W's where you have two seperate meshes, then you use these later

#' Next, we define the spatial random field.
w1.index <- inla.spde.make.index(name = 'w1',
                                 n.spde = spde$n.spde)

w2.index <- inla.spde.make.index(name = 'w2',
                                 n.spde = spde$n.spde)

#' Note that we call the spatial random fields 'w1' and 'w2'. This
#' is rather crucial!




#* Subsection 6.5: Make a stack----

#' Make the X matrix using model.matrix().
X <- model.matrix(~ Temp.std + SweptArea.std + fYear + fBottomType,
                  data = SkatesLong)
X <- as.data.frame(X) #' Avoids an error message in INLA
colnames(X)

#' Both species use the same covariates in the logit link function,
#' but this is not required.


#' Define sample size.
N <- nrow(SkatesLong)


#' Now comes the tricky part for multivariate models. We need a stack
#' for each species. The CombinedData data contains two columns.
#' Each column is a species. Each row can have only one non-NA value.
#' For species 1 we use Stack1. And each covariate has a '1' at the end
#' so that it is a covariate for species 1. Obvioulsy, you can use
#' different names.

#' Define the stack for for the RA data.
Stack1 <- inla.stack(
  tag = "SpeciesRA",                                       #' Name of stack
  data = list(CombinedData = cbind(SkatesLong$RA01, NA)),  #' Response variable
  A = list(1, 1, 1,1,1,1, A),
  effects = list(
    Intercept1                 = rep(1, N),           #' Intercept
    Temp1                      = X$Temp.std,          #' Covariate
    SweptArea1                 = X$SweptArea.std,     #' Covariate
    fYear1.1999                = X$fYear1999,         #' Covariate
    fYear1.2003                = X$fYear2003,         #' Covariate
    fYear1.2005                = X$fYear2005,         #' Covariate
    w1                         = w1.index))      #' Spatial random field

# THIS IS THE PLACE WHERE WE ARE ADDED THE DIFFERENT MESHES


#' Now we make the stack for species 2. The response variable goes
#' in the second column of CombinedData. And we need to make sure that
#' each row of CombinedData has only 1 value (the rest should be NA).
#' The covariates have the extension '2' so that these are covariate
#' effects for species 2.
Stack2 <- inla.stack(
  tag = "SpeciesRB",                                      #' Name of stack
  data = list(CombinedData = cbind(NA,SkatesLong$SB01)),  #' Response variable
  A = list(1, 1, 1, 1, 1, 1, A),
  effects = list(
    Intercept2                 = rep(1, N),           #' Intercept
    Temp2                      = X$Temp.std,          #' Covariate
    SweptArea2                 = X$SweptArea.std,     #' Covariate
    fYear2.1999                = X$fYear1999,         #' Covariate
    fYear2.2003                = X$fYear2003,         #' Covariate
    fYear2.2005                = X$fYear2005,         #' Covariate
    w2                         = w2.index))      #' Spatial random field

#' We are using the same mesh for both species, hence the same A
#' matrix in both species. But the ws differ per species.


#' Combine the two stacks.
StackCombined <- inla.stack(Stack1, Stack2)





#* Subsection 6.6: Specify the model formula----

#' Specify the model formula in terms of the response variables,
#' covariates and the spatial correlated terms.

#' This is the model for both species with spatial dependency.
#'  - All the terms ending with a '1' are taken from Stack 1 and
#'    apply for species 1.
#'  - All the terms ending with a '2' are taken from Stack 2 and
#'    apply for species 2.
#'  - Right now, nothing is shared between the species, and everything is
#'    different for the two species. This is the same as fitting two
#'    univariate models.

Form1 <- CombinedData ~ -1 +
                     Intercept1 +  Temp1 + SweptArea1 +
                     fYear1.1999 + fYear1.2003 + fYear1.2005 +
                     f(w1, model = spde) +
                     Intercept2 +  Temp2 + SweptArea2 +
                     fYear2.1999 + fYear2.2003 + fYear2.2005 +
                     f(w2, model = spde)



#* Subsection 6.7: Fit the INLA model----

#' Fit the Bernoulli GLMs.
#' Note the double family argument!
I1 <- inla(Form1,
           family = c("binomial", "binomial"), # <- can change to anything you want
           data = inla.stack.data(StackCombined),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(StackCombined),
                                    compute = TRUE,
                                    link = 1))
summary(I1)
#' This should be comparable to fitting two univariate GLMs (without BottomType).
#' Assuming we used the same priors.


#' You can access the spatial correlation terms via:
w1.pm <- I1$summary.random$w1$mean
w2.pm <- I1$summary.random$w2$mean

#' Instead of plotting the results of this model, we will now
#' utilise the multivariate nature of this model.




# Section 7: Multivariate dependent Bernoulli GLMs with shared Temp effect----

#' We will now focus on the question:
#' Does the temperature effect differ per species?
#' To answer this question,  we will fit a model in which the
#' temperature effect is the same for both species. And we will then
#' compare the DICs and WAICs for the model above, and the one we are
#' now going to fit.

#' We can utilise the same mesh, priors and w1, and w2. We only
#' need to adjust the names of the covariates in the two stacks.
#' Note that we will now use the name 'Temp' in Stack1 and in Stack2.


#' Define the stack for for the RA data.
Stack1 <- inla.stack(
  tag = "SpeciesRA",                                   #' Name of stack
  data = list(CombinedData = cbind(SkatesLong$RA01, NA)),  #' Response variable
  A = list(1, 1, 1, 1, 1, 1, A),
  effects = list(
    Intercept1                 = rep(1, N),           #' Intercept
    Temp                       = X$Temp.std,          #' Covariate
    SweptArea1                 = X$SweptArea.std,     #' Covariate
    fYear1.1999                = X$fYear1999,         #' Covariate
    fYear1.2003                = X$fYear2003,         #' Covariate
    fYear1.2005                = X$fYear2005,         #' Covariate
    w1                         = w1.index))           #' Spatial random field


#' Define the stack for for the RB data.
Stack2 <- inla.stack(
  tag = "SpeciesRB",                                      #' Name of stack
  data = list(CombinedData = cbind(NA,SkatesLong$SB01)),  #' Response variable
  A = list(1, 1, 1, 1, 1, 1, A),
  effects = list(
    Intercept2                 = rep(1, N),           #' Intercept
    Temp                       = X$Temp.std,          #' Covariate
    SweptArea2                 = X$SweptArea.std,     #' Covariate
    fYear2.1999                = X$fYear1999,         #' Covariate
    fYear2.2003                = X$fYear2003,         #' Covariate
    fYear2.2005                = X$fYear2005,         #' Covariate
    w2                         = w2.index))           #' Spatial random field


StackCombined <- inla.stack(Stack1, Stack2)


#' Here is the mathematical expression of the model we are going to fit:
#'   logit(Pi1_i) = beta_01 +                         #Intercept
#'                  beta    x Temperature_i +
#'                  beta_21 x SweptArea_i +
#'                  beta_31 x Year1999_i +
#'                  beta_41 x Year2003_i +
#'                  beta_51 x Year2005_i +
#'                  u1_i

#'   logit(Pi2_i) = beta_02 +                        #Intercept
#'                  beta    x Temperature_i +
#'                  beta_22 x SweptArea_i +
#'                  beta_32 x Year1999_i +
#'                  beta_42 x Year2003_i +
#'                  beta_52 x Year2005_i +
#'                  u2_i


#' This is the model with a common temperature effect
Form2 <- CombinedData ~ -1 +
                       Intercept1 +
                       Temp +
                       SweptArea1 +
                       fYear1.1999 + fYear1.2003 + fYear1.2005 +
                       f(w1, model = spde) +
                       Intercept2 +
                       #Temp +    #' No need to repeat Temp
                       SweptArea2 +
                       fYear2.1999 + fYear2.2003 + fYear2.2005 +
                       f(w2, model = spde)


I2 <- inla(Form2,
           family = c("binomial", "binomial"),
           data = inla.stack.data(StackCombined),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(StackCombined),
                                    compute = TRUE,
                                    link = 1))
summary(I2)
#' Note that there is now only 1 regression parameter for Temperature,
#' affecting both species in the same way.
#' And it is important for both species.


#' Is this an improvement?
DICs <- c(I1$dic$dic, I2$dic$dic)
DICs

#' A model with 2 slopes gives nearly the aem DIC as a model
#' wit 1 slope. That means that we migth as well use a model
#' with one slope. Hence, temperature affects both species in
#' the same way.

# the interaction between species ID and temperature is not significant
# and therefore the model with two slopes is unnecessary.






# Section 8: Multivariate dependent Bernoulli GLMs with shared spatial term----

#' We will now focus on the question:
#' Do the species exhibit similar spatial patterns, while taking
#' into account the covariate effects?

#' To answer this question,  we will fit a model in which the
#' spatial term is the same for both species. And we will then
#' compare the DICs and WAICs for the model above, and the one we are
#' now going to fit.

#' We can utilise the same mesh, priors and w1, and w2. We only
#' need to adjust the names of the covariates in the two stacks.
#' We are still using a w1 and w2 in both stacks. But we will show
#' how w2 can be a copy of w1 in the formula.


#' Define the stack for for the RA data.
Stack1 <- inla.stack(
  tag = "SpeciesRA",                                   #' Name of stack
  data = list(CombinedData = cbind(SkatesLong$RA01, NA)),  #' Response variable
  A = list(1, 1, 1, 1, 1, 1, A),
  effects = list(
    Intercept1                 = rep(1, N),           #' Intercept
    Temp1                      = X$Temp.std,          #' Covariate
    SweptArea1                 = X$SweptArea.std,     #' Covariate
    fYear1.1999                = X$fYear1999,         #' Covariate
    fYear1.2003                = X$fYear2003,         #' Covariate
    fYear1.2005                = X$fYear2005,         #' Covariate
    w1                         = w1.index))           #' Spatial random field


#' Define the stack for for the RB data.
Stack2 <- inla.stack(
  tag = "SpeciesRB",                                      #' Name of stack
  data = list(CombinedData = cbind(NA,SkatesLong$SB01)),  #' Response variable
  A = list(1, 1, 1, 1, 1, 1, A),
  effects = list(
    Intercept2                 = rep(1, N),           #' Intercept
    Temp2                      = X$Temp.std,          #' Covariate
    SweptArea2                 = X$SweptArea.std,     #' Covariate
    fYear2.1999                = X$fYear1999,         #' Covariate
    fYear2.2003                = X$fYear2003,         #' Covariate
    fYear2.2005                = X$fYear2005,         #' Covariate
    w2                         = w2.index))           #' Spatial random field


StackCombined <- inla.stack(Stack1, Stack2)


#' Here is the mathematical expression of the model we are going to fit:
#'   logit(Pi1_i) = beta_01 +                         #Intercept
#'                  beta_11 x Temperature_i +
#'                  beta_21 x SweptArea_i +
#'                  beta_31 x Year1999_i +
#'                  beta_41 x Year2003_i +
#'                  beta_51 x Year2005_i +
#'                  u1_i

#'   logit(Pi2_i) = beta_02 +                        #Intercept
#'                  beta_12    x Temperature_i +
#'                  beta_22 x SweptArea_i +
#'                  beta_32 x Year1999_i +
#'                  beta_42 x Year2003_i +
#'                  beta_52 x Year2005_i +
#'                  u2_i...which is a copy if u1_i


#' See:
https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLAfeatures.html#sec:sharedterms

#' We are now going to share the spatial term between the two species.
#' This is done with the 'copy' argument.

#' Instead of the copy argument, we could have made one w via:
w.index <- inla.spde.make.index(name = 'w', n.spde = spde$n.spde)
#' Then define one common w in both stacks, and use f(w, model = spde)
#' Just as we did for Temp.


#' This is the model with a common spatial effect
Form3 <- CombinedData ~ -1 +
                        Intercept1 +
                        Temp1 + SweptArea1 +
                        fYear1.1999 + fYear1.2003 + fYear1.2005 +
                        f(w1, model = spde) +
                        Intercept2 +
                        Temp2 + SweptArea2 +
                        fYear2.1999 + fYear2.2003 + fYear2.2005 +
                        f(w2, copy = "w1")


I3 <- inla(Form3,
           family = c("binomial", "binomial"),
           data = inla.stack.data(StackCombined),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(StackCombined),
                                    compute = TRUE,
                                    link = 1))
summary(I3)
#' Note that there is now only 1 regression parameter for Temperature,
#' affecting both species in the same way.
#' And it is important for both species.


#' Is this an improvement?
DICs <- c(I1$dic$dic, I2$dic$dic, I3$dic$dic)
DICs

#' We definitely need to allow for different spatial patterns per species!



#' Task/homework:
#'  - How can we test whether the temporal trend differs per species?
#'  - How can we allow for different priors for the range per species?
#'  - How can we allow for different mesh configurations per species?
#'  - Instead of doing this in the context of two species,
#'    how else can you utilise the concept of multiple likelihoods
#'    in one model?




