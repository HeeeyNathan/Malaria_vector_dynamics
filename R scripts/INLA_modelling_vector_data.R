# Section 1: Data description-----

#* Subsection 1.1: Data source----

#' The data used in this exercise were provided by the Lithuanian
#' Environmental Protection Agency (LEPA). If you want to use these data 
#' for any other purpose, please contact LEPA directly. Their website address
#' is: https://aaa.lrv.lt/lt/

#' The a subset of this data were also published in:
#'  Baker, N.J., Baker, N.J., Pilotto, F., Welti, E.A.R., Osadčaja, D. & 
#'  Palinauskas, V. (2024). Recovery or reorganisation? Long-term increases 
#'  in riverine taxonomic and functional diversity are confounded by compositional 
#'  dynamics. Hydrobiologia. https://link.springer.com/article/10.1007/s10750-024-05665-5

#'  Sinclair, J.S., Welti, E.A.R., Altermatt, F., Álvarez-Cabria, M., Aroviita, J., 
#'  Baker, N.J., Barešová, L., Barquín, J., Bonacina, L., Bonada, N., Cañedo-Argüelles, 
#'  M., Csabai, Z., de Eyto, E., Dohet, A., Dörflinger, G., Eriksen, T.E., Evtimova, V., 
#'  Feio, M.J., Ferréol, M., … Haase, P. 2024. Multi-decadal improvements in the ecological 
#'  quality of European rivers are not consistently reflected in biodiversity metrics. 
#'  Nature Ecology & Evolution, 8, 430-441. https://doi.org/10.1038/s41559-023-02305-4


#* Subsection 1.2: Funding source----

#'  The collation and processing of this dataset was supported by the Diana Osadcaja and 
#'  the Lithuanian Environmental Protection Agency (https://aaa.lrv.lt/) who collected 
#'  the data. The project was funded by the Lithuanian Research Council (project number 
#'  S-PD-22-72).


#* Subsection 1.3: Background information----

#' This dataset contains count data of 962 macroinvertebrate taxa (taxa is used due to
#' mixed identification levels) collected from freshwater bodies across lithuania. 
#' The initial biomonitoring dataset was created as part of Lithuania's national monitoring 
#' scheme, in line with the European Union's Water Framwork Directive. The dataset spans
#' a period from 2010 to 2023, covering both lentic (rivers) and lotic (lakes) freshwater 
#' bodies. The dataset includes count data of macroinvertebrates from 953 sampling sites; 615 
#' riverine sites and 338 lake sites. LEPA employs a staggered sampling approach, whereby 
#' most sites are resampled in 3-year increments, with only a few sites (~20%) being sampled
#' more consistently. 
#' 
#' For river sites (n = 615), the average number of sites sampled per year is 151, with 
#' distances between sites averaging 132 km and remaining relatively consistent through 
#' time. On average, sites were resampled 3 times within the 14-year observation period 
#' (2010 - 2023), with the majority of sites being sampled twice (mode). Around 85% (521) 
#' of sites had time series less than 5 years in length, while 15% (94 sites) had time 
#' series greater than or equal to 5 years. The dataset contains count data of 761 
#' unique taxa, with the total abundance of these taxa being 1 156 915 individuals. The 
#' identification of these taxa was mostly at the species (61%) and genus (29%) levels, 
#' with the remaining 10% of taxa being identified to the family (9%) or sub-family-levels 
#' (1%). 
#' 
#' For lake sites (n = 338), the average number of sites sampled per year is 60, with the 
#' distances between sites averaging 127 km and remianing relatively consistent through time. 
#' On average sites were resampled twice within the 11-year observation period (2013 - 2023), 
#' with most sites being resampled twice (mode). 86% #' (290 sites) of sites had time series 
#' less than 3 years in length, while 13% (38 sites) had time series greater than or equal 
#' to 3 years. The dataset contains count data of 675 unique taxa, with the total abundance 
#' of these taxa being 320 837 individuals. The identification of these taxa was mostly at 
#' the species (66%) and genus (27%) levels, with the remaining 8% of taxa being identified 
#' to the family (7%) or sub-family-levels (1%). 
#' 
#' In Baker et al. (2024), a subset of this dataset was used to determine temporal changes
#' in macroinvertebrate communities between 2010 and 2022. This investigation is a regional 
#' follow-up to Haase et al. (2023) - Nature, and Sinclair et al. (2024) - Nat. Ecol. Evol. 
#' which also included parts of the larger Lithuanian dataset. 
#' 
#' Lithuania is in the boreal ecoregion, with mean annual precipitation and temperature 
#' (from 1991 to 2020) being 679 mm and 7.38 °C, respectively. Situated at the edge of 
#' the East European Plain, its low-lying post-glaciated landscape has a maximum elevation 
#' of 297 m a.s.l., with 758 streams greater than 10 km long transecting the country. The 
#' largest and most economically important river is the Nemunas, which originates in Belarus, 
#' forms the catchment area for 72% of Lithuania’s 64,800 km2 territory, and drains into 
#' the Baltic Sea. Other noteworthy catchments include the basins of the Venta, Lielupė, 
#' and Dauguva rivers. All Lithuanian rivers are calcareous and lowland (< 200 m a.s.l.), 
#' with their river typology being based on catchment size and slope Despite heavy 
#' deforestation during the soviet regime, the coverage of postsoviet forests in Lithuania
#' comprised mostly of pine, spruce, and birch—has recovered from 31.3% in 1992 to 35.5% in 
#' 2023. Lithuania’s built-up areas (3.73% coverage) have increased since 1971, while the 
#' coverage of arable land (45.99%), meadows/pastures (5.55%), wetlands (5.62%), and other 
#' land uses (5.78%) has declined. There majority of lakes are located in the north-east
#' part of the country.

#' Most of Lithuania’s arable land is restricted to a central strip that runs north to south, 
#' from above the city of Šiauliai to below the city of Kaunas. This arable region transects 
#' the country into two distinct climatic regions across an east-west gradient. The eat of the
#' country is categorized by higher average temperatures, higher precipitation, more sunshine 
#' hours and higher wind speeds, while the east is cooler, drier, and calmer. These regions can
#' also be delimited by lowlands in the west and highlands in the east. More information is 
#' available here: https://www.meteo.lt/en/climate/lithuanian-climate/air-temperature/

#' Lithuania is geographically located along an important migratory pathway of birds migrating 
#' northward in the spring and southward in the fall. The worlds second oldest ornithological
#' station, Ventas Ragas, is located in the western part of the country, on the Curonian lagoon. 
#' Here, and at a nearby ornithological station called Ribachy located on the Curonian Spit,
#' birds are screened for avian malarial parasites of the Haemosporidian genera Leucocytozoon, 
#' Haemoproteus, and Plasmodium. These parasites are transmitted by insect vectors, namely members
#' of the Simulidae (black flies), the Ceratopogoniidae (biting midges), the Culicidae (mosquitos), 
#' during feeding on blood and have been increasing in prevalence among birds since observations
#' began (see below).

print(readRDS("Plots/Curonian_Lagoon_Malaria_Prevalence.rds"))

#' Evidence suggests that Diptera (i.e., true flies) are increasing in both abundance and richness
#' throughout Lithuania (Baker et al., 2024), potentially explaining the increase in prevalence of 
#' malarial haemosporidian parasite prevalence in birds.

print(readRDS("Plots/LT_slopeDistributions_TaxoGroups.rds"))

#' However, the driving factors behind these increases and what factors control their presence and 
#' abundance are still not well understood. Furthermore, initial data exploration appears to show 
#' that vector presence and abundance is highest in the eastern Lithuania, which is comparatively
#' undersampled, both in terms of bird malaria and vector dynamics, compared to western Lithuania 
#' near the curonian lagoon. 

#' The aim of this study is to determine the driving forces behind vector presence and abundance, in 
#' an attempt to better understand vector and malarial dynamics, while simultaneously identifying
#' underrepresented and under sampled regions that may yeild better more sucess in finding bird
#' malarial parasites, thereby shedding light on the processes shaping malarial spread amongst birds 
#' and thus potentially informing human-malaria relationships.

#' The species investigated in the paper are:
#'   -Freshwater macroinvertebrates, which are categorized as: 
#'    insects in their nymph and larval stages, snails, worms, 
#'    crayfish, and clams that spend at least part of their 
#'    lives in water and large enough to see without the aid 
#'    of a microscope.
#'   -The initial data set contained counts for all the macro-
#'    invertebrate species collected  at a site, however, only
#'    dipteran data were retained. 

#' Below are some bullet points describing the data collection.
#'  1. River and lake sites form part of Lithuania's national biomonitoring 
#'     scheme

#'  2. At each site, macroinvertebrate sampling included both quantitative 
#'     (standard multihabitat kick-sampling) and qualitative (hand-picking 
#'     organisms from underwater objects) components (Šidagytė-Copilas & 
#'     Arbačiauskas, 2022).

#'  3. The quantitative samples were collected using the multihabitat method.
#'     Within a site, 10 subsamples (later to be pooled into one sample) were 
#'     collected from all available microhabitats proportionally to their 
#'     estimated distribution. Each subsample was collected using the kick 
#'     method by disturbing the 40 cm length area in front of the standard 
#'     handnet (25 × 25 cm opening, 0.5 mm mesh size), resulting in 0.1 m2 of 
#'     the bottom area sampled per subsample, and 1 m2 per the whole sample.

#'  4. Additional qualitative samples were collected by searching for 
#'     macroinvertebrates attached to underwater objects (roots,stones, plants, 
#'     etc.). In cases where qualitative sampling led to the inclusion of a 
#'     taxon not identified during the quantitative sampling, taxa were 
#'     included with an assigned abundance of one.

#'  5. Most sites are resampled in 3-year increments, with few sites having 
#'     more regular sampling. 

#' Below are some bullet points describing the initial data processing collection.
#'  1. To ensure our biological dataset conformed to contemporary scientific 
#'     nomenclature (Grenié et al., 2021, 2022), taxon names were individually 
#'     checked by Lithuanian macroinvertebrate experts and verified against the 
#'     2023 Global Biodiversity Information Facility (GBIF) taxonomic backbone. 
#'     All non-freshwater macroinvertebrates were removed, including 
#'     microinvertebrates (e.g. hydrachnidia, copepoda) and non-freshwater 
#'     invertebrates (e.g. terrestrial, semi-terrestrial taxa).

#'  2. For each site-year, the following taxonomic diversity indices were 
#'     calculated: total diptera abundance, diptera richness, vector abundance,
#'     and vector richness. The vegan R package (Oksanen et al., 2022) was used 
#'     to calculate all other taxonomic indices.

#'  3. For each site and year, an ecological quality ratio was calculated according 
#'     to the methods outlined in (Šidagytė-Copilas & Arbačiauskas, 2022). The EQR
#'     is in accordance with the EU's water framework directive and describes the 
#'     ecological quality of a site compared to a comparable reference site. The 
#'     EQR is bound between zero and one. From the EQR, an ecological quality class
#'     (EQC) is assisned. This metric is the primary covariate in our analysis.

#'  4. Environmental data linked to the study were accessible for a subset of sites.
#'     The environmental dataset comprised monthly measurements of diverse variables, 
#'     encompassing climatic parameters including water temperature (°C) and discharge 
#'     (m3 s−1), and water quality parameters such as alkalinity (mmol l−1), dissolved 
#'     oxygen (mg l−1), pH, electrical conductivity (μS cm−1), ammonium (mg l−1), 
#'     nitrate (mg l− 1), nitrite (mg l−1), mineralised nitrogen (mg l−1), total 
#'     nitrogen (mg l−1), ortho-phosphate (mg l−1), total phosphorus (mg l−1), and
#'     suspended solids (mg l−1). Additionally, worldclim data was used to extract 
#'     climatic variables from each site-year combination. The following parameters 
#'     were extracted for each site and year: precipitation (ppt), discharge (q), 
#'     maximum temperature (tmax), minimum temperature (tmin), and wind speed (ws).
#'     
#'     For each biological site-year, the specific month of the biological sampling 
#'     was determined. Subsequently, mean annual values of the predictor variables 
#'     were computed using data from the preceding 12 months.
#'     
#'  5. Elevation data for each unique site-year combination was extracted using 
#'     Google's Maps Elevation API. 
#'     
#'  6. Landuse data for each unique site-year combination was extracted using 
#'     the 2018 Corine landcover dataset. The coarsest level of naming was used for
#'     the landcover (LABEL 1) to avoid too many collinear covaraites. We used 1000m
#'     buffer zones around the sites to account for relatively small-scale effects 
#'     of landuse on dipteran communities.

#' The basic statistical unit of the analysis is the sum of abundances of vectors 
#' species counted within sample from a biomonitoring survey within a given year.

#' Since many site-year observations had vector abundances of zero, the data are 
#' likely zero inflated with a large range (more on this later). 


#* Subsection 1.4: The variables----
#' Response variable in this exercise:
#'   Counts:
#'    The number of vector individuals (black flies, biting midges, and mosquitos)
#'    at a sampling site in a specific year


#' Covariates:
#'   -EQR (0 - 1)
#'   -precipitation (ppt)
#'   -discharge (q)
#'   -maximum temperature (tmax)
#'   -minimum temperature (tmin)
#'   -wind speed (ws)
#'   -elevation (m.a.s.l.)
#'   -Agricultural areas (%)
#'   -Artificial.surfaces (%)
#'   -Forest.and.semi.natural.areas (%)
#'   -Water.bodies (%)
#'   -Wetlands (%)
#'   -Year
#'   -Day of year? Not Julian day (no. of days since a specific date) - 1 January 4713 BCE
#'   -Waterbody_type (Lake vs River)
#'   -Latitude (y coordinate of sampling site)
#'   -Longitude (x coordinate of sampling site)


#' Dependency:
#'  - Spatial correlation.
#'  - Temporal correlation.
#'  - Spatial-Temporal correlation.




#* Subsection 1.5: Underlying questions----

#' Aim of the analysis:
#'  - Determine factors influencing vector abundance (and presence).
#'  - Determine areas in which sampling should be conducted to maximise chances of catching malarial vectors.


# Section 2: Import the data----
#* Subsection 2.1: Load the packages----

#' Load all packages and our support file.
library(lattice)
library(ggplot2)
library(mgcv)
library(sf)
library(gratia)
library(plyr)
library(tidyr)
library(DHARMa)
# library(rnaturalearth)
library(rgeoboundaries)
library(gstat)
library(INLA)
library(fmesher)
library(MASS)
library(animation)
library(scales)
library(httr)
library(googleway)
library(tidyverse)
library(janitor)
library(GGally)
library(ggmap)
source(file = "Additional functions/HighstatLibV15.R")


#* Subsection 2.2: Load the data----

#' Load the dipteran vector data.
# Import the data
df <- read.csv("Data/Bio_Env_data_15.1.2025.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names = FALSE) |> 
  clean_names() |> 
  # filter(year >= 2013 & year <= 2022) |> # Remove years less than 2013 or greater than 2022 (when looking at lakes and rivers combined)
  # filter(waterbody_type == "lake") |> # keep data from rivers only
  # filter(waterbody_type == "river") |> # keep data from lakes only
  # filter(!is.na(eqr)) |>  # Remove rows where EQR is NA
  mutate(dipt_abund_pa    = ifelse(dipt_abund == 0, 0, 1), # create vector with presence-absence data
         dipt_abund_pos   = ifelse(dipt_abund > 0, dipt_abund, NA), # create vector with only positive data (i.e., not zero)
         vec_abund_pa     = ifelse(vec_abund == 0, 0, 1), # create vector with presence-absence data
         vec_abund_pos    = ifelse(vec_abund > 0, vec_abund, NA), # create vector with only positive data (i.e., not zero)
         fyear            = factor(year, levels = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)), # make year a factor
         cyear            = year - median(year), # centered year - helps model convergence to center variables for the model
         iyear            = year - min(year) + 1, # year as an index starting from 1
         year             = as.numeric(year), # year as numeric
         month            = factor(month, levels = c(4, 5, 9, 10, 11),
                                          labels = c("April", "May", "September", "October", "November"),
                                          ordered = F), # make month a factor
         state            = factor(state, levels = c("A", "HM", "N"),
                                          labels = c("Artificial", "Heavily_modified", "Natural"),
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
  rename(agriculture      = agricultural_areas,
         artificial       = artificial_surfaces,
         natural          = forest_and_semi_natural_areas,
         water            = water_bodies,
         wetlands         = wetlands) |> 
  select(-c(observation_period, sampling_events)) |> # removes unnecessary columns
  arrange(desc(waterbody_type), site_id, year) |>  # order the data.frame
  as.data.frame() # Convert tibble to dataframe because some older code does not recognize tibble


#' What do we have?
names(df)
glimpse(df)
head(df)


#' The analysis in Section 10 (spatial-temporal auto-regressive GLM)
#' may take some time, depending on your computer speed. To ensure
#' that the model runs on not-so-fast computers, we have selected
#' a small mesh size. We also made to short cuts in the spatial-temporal
#' analysis. For a real analysis of these data, you need to modify
#' these settings and get a fairly fast computer (or some patience).


# Section 3: Data coding----

#' We will relabel some of the names to make them more intuitive.

#' This is the response variable.
df$Counts <- df$vec_abund

#' And the covariates:
df$Lat    <- df$latitude
df$Long   <- df$longitude
df$Year   <- df$year

#' For some graphs, we need year as a categorical variable.
df$fyear


#' Convert longitude and latitude to UTM in zone 20. The UTM_Transform
#' function is on our support file.
XY.utm <- UTM_Transform(x = df$Long,
                        y = df$Lat,
                        zone = 34,
                        Hemisphere = "north")

df$Xkm <- XY.utm[,"X"] / 1000
df$Ykm <- XY.utm[,"Y"] / 1000



#' We define some common settings for INLA.
MyControlCompute  <- list(config = TRUE,    #' Allow for posterior simulation
                          dic = TRUE,       #' Calculate AIC
                          waic = TRUE,      #' Calculate WAIC
                          residuals = TRUE) #' Get residuals (see below)
MyControlPredictor  <- list(compute = TRUE, #' Calculate fitted values
                            link = 1)       #' Predict on the scale of
                                            #' the response variable.

#' Define the penalised complexity prior for the RW2 smoother.
U <- 1
MyPCPrior <- list(theta = list(prior = "pc.prec", 
                               param = c(U, 0.01)))


#' To avoid a potential INLA error message, execute:
m <- get("inla.models", inla.get.inlaEnv())
m$latent$rw2$min.diff = NULL
assign("inla.models", m, inla.get.inlaEnv())


# Section 4: House keeping----

#* Subsection 4.1: Number of observations by site and year----

# How many observations do we have per year?
#' Calculate how many observations we have per year?
table(df$year)

# Likely need to drop 2023 because it has far fewer sites than the other years. 
# Also, 2010, 2011, and 2012 are only available for rivers, so might as well drop 
# those too.

df <- df |> 
  filter(year >= 2013 & year <= 2022) # Remove years less than 2013 or greater than 2022

# How many observations do we have per location per year?
#' Calculate the observation period and number of repeated samplings for each site
df <- df |>
  group_by(site_id) |>
  mutate(
    observation_period = max(year) - min(year) + 1,
    sampling_events = n_distinct(year)
  ) |>
  ungroup() |>
  arrange(desc(waterbody_type), site_id, year)

# After removing the years 2010, 2011, 2012, and 2023, some sites were not sampled in
# the remaining years, leaving them with a value of zero. Further, many sites were only
# sampled once. These site should be remove since they are not technically time series.
# Let's remove these...

# Lets keep these for now

# Keep sites with less than 2 sampling events throughout the observation period
df <- df |> 
  filter(sampling_events >= 1) |> 
  arrange(desc(waterbody_type), site_id, year)

#* Subsection 4.2: Missing values----

# Missing values?
colSums(is.na(df))  
100 * colSums(is.na(df)) / nrow(df)  

# There are manu NA values, including some ~1% in our focal covariate: EQR. Let's remove
# the rows with missing EQR values and then remove columns containing many NAs which are
# unnecessary for this analysis.

# Remove rows where EQR is NA
df <- df |> 
  filter(!is.na(eqr))

# Remove columns containing NA values
df <- df |> 
  select(where(~ all(!is.na(.))))

#* Subsection 4.3: Check values after cleaning----

# How many sites do we have in total?
NROW(unique(df$site_id))

# We still have 699 sites

# How many lake sites vs rivers sites do we have?
df |> 
  group_by(waterbody_type) |> 
  summarise(unique_sites = n_distinct(site_id)) |> 
  arrange(desc(unique_sites))

# We have more than double the amount of lake sites compared to river sites

# How sites were sampled per year?
table(df$year)

# That is better, but 2018 and 2019 still have fewer sites sampled. Lets keep them in for now


# Section 5: Data exploration----

# Start by defining a preferred figure format, called 'My_theme'
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, linewidth = 1),
                  strip.background = element_rect(fill = "white",
                                                  color = "white", linewidth = 1),
                  text = element_text(size = 14),
                  panel.grid.major = element_line(colour = "white", linewidth = 0.1),
                  panel.grid.minor = element_line(colour = "white", linewidth = 0.1),
                  legend.position = "none")

#* Subsection 5.1: Outliers----

#' Make a Cleveland dotplot of the response variable 'spider richness' and the
#' continuous covariates.
MyVar <- c("Counts", "eqr", "ppt", "tmax", "tmin", "q", "ws", "elevation", 
           "agriculture", "artificial", "natural",
           "year")
Mydotplot(df[,MyVar])
#' All ok.


#* Subsection 5.2: Collinearity----

df |> 
  ggpairs(columns = MyVar, 
          aes(alpha = 0.8), lower = list(continuous = "smooth_loess", 
          combo = wrap("facethist", binwidth = 5))) + My_theme

df |> 
  select(all_of(MyVar)) |> 
  corvif()

# Perhaps tmax and q can cause some trouble. Let's remove them and see

MyVar_red <- c("eqr", "ppt", "tmin", "ws", "elevation", 
               "agriculture", "artificial", "natural",
               "year")

df |> 
  ggpairs(columns = MyVar_red, 
          aes(alpha = 0.8), lower = list(continuous = "smooth_loess", 
          combo = wrap("facethist", binwidth = 5))) + My_theme

df |> 
  select(all_of(MyVar_red)) |> 
  corvif()

# seems okay now, but maybe we can drop some terms later?


#* Subsection 5.3: Relationships----

# Then plot
p1 <- df |> 
  ggplot(aes(y = Counts, x = eqr)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector abundance",
       x = "Ecological quality") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$eqr), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$eqr), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p1
# a slightly positive relationship
p1 + facet_grid(~waterbody_type)
# a similar relationship between lakes and rivers


p2 <- df |> 
  ggplot(aes(y = Counts, x = ppt)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Precipitation") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$ppt), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$ppt), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p2
# not much of a relationship
p2 + facet_grid(~waterbody_type)
# maybe some difference between lakes and rivers... interaction?

p3 <- df |>
  ggplot(aes(y = Counts, x = q)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Discharge") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$q), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$q), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p3
# not much of a relationship
p3 + facet_grid(~waterbody_type)
# not much there.

p4 <- df |>
  ggplot(aes(y = Counts, x = tmax)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Maximum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$tmax), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$tmax), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p4
# not much of a relationship
p4 + facet_grid(~waterbody_type)
# differences between lakes and rivers, maybe some non-linearity there?

p5 <- df |>
  ggplot(aes(y = Counts, x = tmin)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Minimum temperature") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$tmin), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$tmin), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p5
# maybe some slightly negative relationship
p5 + facet_grid(~waterbody_type)
# differences between lakes and rivers, maybe some non-linearity there?

p6 <- df |>
  ggplot(aes(y = vec_abund, x = ws)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Wind speed") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$ws), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$ws), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p6
# not much of a relationship
p6 + facet_grid(~waterbody_type)
# not much there.

p7 <- df |>
  ggplot(aes(y = Counts, x = elevation)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Elevation") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$elevation), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$elevation), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p7
# a postive relationship
p7 + facet_grid(~waterbody_type)
# not much difference between lakes and rivers

p8 <- df |>
  ggplot(aes(y = Counts, x = agriculture)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Agricultural areas (%)") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$agriculture), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$agriculture), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p8
# not much there
p8 + facet_grid(~waterbody_type)
# maybe a slight negative relationship for rivers, but not clear


p9 <- df |>
  ggplot(aes(y = Counts, x = artificial)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Artificial surfaces (%)") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$artificial), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$artificial), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p9
# not much there
p9 + facet_grid(~waterbody_type)
# not much there

p10 <- df |>
  ggplot(aes(y = Counts, x = natural)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Forest & semi natural areas (%)") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  # xlim(round(range(df$natural), 0))  + ylim(range(df$Counts)) +
  xlim(round(range(df$natural), 0))  + ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p10
# not much there
p10 + facet_grid(~waterbody_type)
# maybe a slight postive relationship for rivers, but not much there


p11 <- df |>
  ggplot(aes(y = Counts, x = year)) +
  geom_smooth(method = "gam") +
  labs(y = "Vector counts",
       x = "Time") +
  geom_jitter(shape = 19, size = 3.5, height = 0.5,
              width = 0.5, alpha = 0.5) +
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  # ylim(range(df$Counts)) +
  ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p11
# not much there
p11 + facet_grid(~waterbody_type)
# some non-linearity in rivers perhaps?

#' What about using using year as a factor? A factor allows for
#' more sudden changes as compared to a smoother.
p12 <- df |>
  ggplot(aes(y = Counts, x = fyear)) +
  geom_boxplot(size = 0.5) +
  labs(y = "Vector counts",
       x = "Year") +
  ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p12
# not much there
p12 + facet_grid(~waterbody_type)
# no clear patterns


p13 <- df |>
  ggplot(aes(y = Counts, x = waterbody_type)) +
  geom_boxplot(size = 0.5) +
  labs(y = "Vector counts",
       x = "Waterbody_type") +
  ylim(range(df$Counts[df$Counts <= 300])) +
  My_theme
p13
# the range for rivers is much larger

#' What about plotting the time series for each sampling site?
p13 <- df |> 
  ggplot(aes(x = year, y = Counts,
           group = site_id)) +
  scale_x_continuous(breaks = 2013:2022, limits = c(2013, 2022)) +
  xlab("Year") + ylab("Counts") +
  geom_line()
p13
table(df$site_id, df$year)


#* Subsection 5.4: Normality and zero inflation----

# Frequency plot
p1 <- df |> 
  ggplot(aes(Counts)) +
  geom_freqpoly(bins = 15) +
  labs(x = "Vector counts",
       y = "Frequency") +
  My_theme
p1
# High number of zeros and count positively skewed

# How many zeros do we have?
df |> 
  summarise(percentage_zero = sum(Counts == 0) / n() * 100)
# 64% zeros - too many!!!


#* Subsection 5.5: Dependency----

# Spatial position of the sites
xyplot(latitude ~ longitude, 
       aspect = "fill",
       data = df)
#' Spatial data! We have 699 spatial locations, corresponding to the site_id.

range(df$longitude, df$latitude)
MyCex <- 3 * sqrt(df$vec_abund + 1) / 10
register_google(key = "AIzaSyClYan86_4y43ON6djumMthyP-fjm1yeGc")
glgmap <- get_map(location = c(left = 21, bottom = 54, right = 27, top = 57), 
                  maptype = "terrain")    
p <- ggmap(glgmap)
p <- p + geom_point(aes(x = longitude, 
                        y = latitude, 
                        color = waterbody_type,
                        shape = waterbody_type), 
                    size = MyCex, 
                    data = df)
p <- p + scale_color_manual(values = c("black", "red"))
p <- p + scale_shape_manual(values = c(19, 17))
p <- p + xlab("Longitude") + ylab("Latitude")  
p <- p + theme(text = element_text(size=15))
p

# And by year
p <- p + facet_wrap(~fyear)
p # Some 2018 misery?

#' It is spatial-temporal data!
#' Maybe we should remove the 2018 data also?
#' Comparatively less sites were sampled in 2018!


#' Here is another approach to plot the sampling locations.
#' Get world map (medium resolution)
lithuania.shp <- geoboundaries("Lithuania")
lithuania <- st_transform(lithuania.shp, crs = 4326)

#' Plot Lithuania using ggplot, and limit the plotting to a certain xlim
#' and ylim.
ggplot(data = lithuania) +
    geom_sf(fill = "transparent") +
    theme_minimal() +
    labs(title = "Study area")  +
    xlim(-21, -27) + ylim(53, 57)


#' Once we present the results of the spatial models, we would
#' like to remove that part of the the spatial term that covers the baltic sea.
#' To do this, we need to do a bit of sf magic.

#' First we want to get the study area as we see it, without having to
#' to xlim and ylim commands. To do this, define a bounding box and
#' crop the bounding box and the Canada map.

#' Define bounding box (xmin, xmax, ymin, ymax).
bb <- st_bbox(c(xmin = 21, xmax = 27,
                ymin = 53, ymax = 57),
              crs = st_crs(lithuania))

#' Crop Canada to the bounding box.
CroppedLithuania <- st_crop(lithuania, bb)

#' This is what we now have.
ggplot(data = CroppedLithuania) +
    geom_sf(fill = "transparent") +
    theme_minimal() +
    labs(title = "Study area") +
    geom_point(data = df, aes(x = Long, y = Lat))
#' Note that we did not use any xlim and ylim commands.
#' You could adjust bounding box values a little bit.




#' Now we want to create a polygon for land and for the sea. Here's a
#' step-by-step guide to create a polygon that represents the sea:
#'  1. Create a bounding box that encompasses the entire region of interest
#'     (including the sea).
#'  2. Create a polygon from the cropped land area (CroppedLithuania).
#'  3. Subtract the land polygon from the bounding box to create a polygon
#'     representing the sea.

#' Implementation:
#'  1. Define bounding box as a polygon.
#'  We will use the same coordinates as above.
#'  Note that this need to be a complete polygon (last row = first row).
bb_coords <- matrix(
  c(21, 53,
    27, 53,
    27, 57,
    21, 57,
    21, 53), # Closing the loop
  byrow = TRUE, ncol = 2)

bb_coords


#' 2. Create a polygon from the bb_coords coordinates.
bb_polygon <- st_polygon(list(bb_coords))

#' Convert this polygon to an sf object. Give it the same crs as
#' CroppedLithuania
bb_sf <- st_sfc(bb_polygon, crs = st_crs(CroppedLithuania))
# plot(bb_sf) #' Empty box.


#' 3. Subtract the cropped land area (CroppedCanada) from the bounding box
#' to create the sea polygon
SeaPolygon <- st_difference(bb_sf, st_geometry(CroppedLithuania))


#' Plot the result.
#'  The SeaPolygon is in light blue.
#'  Land (CroppedCanada) is in transparent with a black boundary.
ggplot() +
  geom_sf(data = st_sf(SeaPolygon),
          alpha = 0.5,
          fill = "lightblue") + # Sea
  geom_sf(data = CroppedLithuania,
          fill = "transparent",
          color = "black") + # Land
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = df,
             aes(x = Long, y = Lat),
             size = 0.5,
             alpha = 0.5)


#' Convert CroppedLithuania and SeaPolygon to UTM coordinates.
#' Transform to UTM zone 20.
CroppedLithuania_UTM <- st_transform(x = CroppedLithuania,
                                     crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km +datum=WGS84")

SeaPolygon_UTM <- st_transform(x = SeaPolygon,
                               crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km +datum=WGS84")


#' Plot the result (in UTM).
ggplot() +
  geom_sf(data = st_sf(SeaPolygon_UTM),
          alpha = 0.5,
          fill = "lightblue") + # Sea
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          color = "black") + # Land
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = df,
             aes(x = Xkm, y = Ykm),
             size = 0.5,
             alpha = 0.5)


#* Subsection 5.6: Conclusions----

#' 1. We have count data with plenty of zeros. The range of the data is large. 
#'    I anticipate a negative binomial distribution or worst case a ZIP or ZINB
#'    (unless I can explain them with a time component or spatial component).
#' 2. We have spatial-temporal data.
#' 3. There appears to be a positive relationship between EQR, precipiation, minimum 
#'    temperature and elevation, and there seems to be some variation with waterbody
#'    type. Interaction term?
#' 4. Maybe we need to remove data from 2018 as fewer sites were sampled during that year. <---- not worried about this
#' 5. Seasonality should be captured by waterbody type: Rivers always sampled in autumn, 
#'    lakes always sampled in spring.



# Section 6: Poisson GLM without spatial dependency in INLA----

#' The response variable is a count (abundance) and we will therefore
#' start with a Poisson distribution.

#' Let us start simple with an ordinary Poisson GLM, and take it from
#' there.

#' This is our starting model:

#'  Counts_i ~ Poisson(mu_i)
#'  E[Counts_i] = mu_i
#'  var[Counts_i] = mu_i
#'  mu_i  = exp( Intercept + eqr + ppt + tmin + ws + elevation +
#'               agriculture  + artificial + natural +
#'               year + fwaterbody_type )

#' To avoid numerical estimation problems, we will use standardised
#' covariates.

#* Subsection 6.1: Standardize covariates----

df <- df |> 
  mutate(
    eqr.std         = MyStd(eqr),
    ppt.std         = MyStd(ppt),
    q.std           = MyStd(q),
    tmin.std        = MyStd(tmin),
    tmax.std        = MyStd(tmax),
    ws.std          = MyStd(ws),
    elevation.std   = MyStd(elevation),
    agriculture.std = MyStd(agriculture),
    artificial.std  = MyStd(artificial),
    natural.std     = MyStd(natural),
    water.std       = MyStd(water),
    wetlands.std    = MyStd(wetlands),
    year.std        = MyStd(year),
    doy.std         = MyStd(doy)
)




# Section 7: Execute the Poisson GLM----

I1 <- inla(Counts ~ 1 + eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                        agriculture.std + artificial.std + natural.std +
                        f(year, model = "rw2", scale.model = TRUE, hyper = MyPCPrior, constr = TRUE) + 
                        waterbody_type,
           control.compute = MyControlCompute,
           control.predictor = MyControlPredictor,
           quantiles = c(0.025, 0.975),
           family = "poisson",
           data = df)
summary(I1)


#' Fitted values from INLA:
N <- nrow(df)
df$Fit.i1 <- I1$summary.fitted.values[1:N, "mean"]  #' Fitted values.

#' Pearson residuals from INLA, following the definition of Pearson
#' residuals: (Y_i - E[Y_i]) / sqrt(var(Y_i))
df$E1.i1 <- (df$Counts - df$Fit.i1) / sqrt(df$Fit.i1)

#* Subsection 7.1: Check the smoothers----
#' Combine the three smoothers in I1C$summary.random
MyData <- rbind(I1$summary.random$year)
head(MyData)


#' Rename some of the columns.
MyData <- dplyr::rename(MyData, 
                        Covariates = `ID`,
                        mu    = mean, 
                        SeLo  = `0.025quant`, 
                        SeUp  = `0.975quant`)
head(MyData)


#' Coding problem: 
#'    When there are knots (covariates with the same value), only 1 
#'    value is estimated at that knot.
#' To see this: 
NX <- c(nrow(I1$summary.random$year))
NX #' Number of values per RW2 term.


#' We add the covariate names to MyData, while taking into account that
#' the smoother lengths may differ.
MyData$Variable <- rep(c("Year"), NX)


#' And define this as a factor.
MyData$Variable <- factor(MyData$Variable,
                          levels = c("Year"))
head(MyData)


#' Now we can plot the smoothers.
p1 <- ggplot() + 
  geom_line(data = MyData, 
            aes(x = Covariates, 
                y = mu), 
            colour = "black") +
  geom_ribbon(data = MyData, 
              aes(x = Covariates, 
                  ymax = SeUp, 
                  ymin = SeLo),
              fill = "blue",
              alpha = 0.2) + 
  geom_rug(data = MyData,
           aes(x = Covariates)) +
  geom_hline(yintercept = 0, lty = 2) +
  xlab("Covariates") + ylab("RW2 Smoother") + 
  theme(text = element_text(size = 15)) 
p1
#' Conclusions:
#' ??????


# Section 8: Model validation for the INLA Poisson GLM I1----

#* Subsection 8.1: Getting scaled quantile residuals----

#' We would like to simulate 1000 data sets, because:
#'  1. We can use these to assess whether the model is overdispersed.
#'  2. In case of zero-inflation, we can use the 1000 simulated data
#'     sets to assess whether the model can cope with the zero
#'     inflation.
#'  3. We can use them to obtain scaled quantile residuals.


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, I1)


#' Extract the 1000 sets of regression parameters
MyVar <- rownames(I1$summary.fixed)
MyVar    #' Names of the regression parameters
Betas1000 <- inla.posterior.sample.eval(MyVar,
                                        SimData)
Betas1000[,1] #' First set of simulated betas
Betas1000[,2] #' Second set of simulated betas
Betas1000[,3] #' Third set of simulated betas


#' Calculate 1000 times the fitted values via mu = exp(X * beta).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                        agriculture.std + artificial.std + natural.std +
                        year.std + waterbody_type,
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
hist(df$Counts, main ="Observed SR")
hist(Ysim[,1],  main = "First simulated data set")
hist(Ysim[,2],  main = "Second simulated data set")
hist(Ysim[,3],  main = "Third simulated data set")
par(mfrow = c(1,1))
#' The observed data seems to be slightly different?



#' Now we have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
E1.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$Counts,
                       fittedPredictedResponse = df$Fit.i1,
                       integerResponse = TRUE)

#' Now we have scaled quantile residuals.




#* Subsection 8.2: Check for overdispersion----

#' We will use the scaled quantile residuals to assess for
#' overdispersion.
testDispersion(E1.sqr)
#' Big trouble.



#* Subsection 8.3: Check for homogeneity of variance----

#' Plot the scaled quantile residuals versus (ranked) fitted values.
plotResiduals(E1.sqr, quantreg = TRUE, smoothScatter = FALSE)
#' Big trouble!



#* Subsection 8.4: Check for normality of the residuals----

#' In DHARMa, we verify whether the scaled quantile residuals are
#' uniform distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(E1.sqr, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)
#' Big trouble!



#* Subsection 8.5: Plot residuals versus the covariates----

#' Plot the scaled quantile residuals versus each covariate
#' in the model.
plotResiduals(E1.sqr, form = df$eqr)            #' Big trouble.
plotResiduals(E1.sqr, form = df$ppt)            #' Big trouble.
plotResiduals(E1.sqr, form = df$tmin)           #' Big trouble.
plotResiduals(E1.sqr, form = df$ws)             #' Big trouble.
plotResiduals(E1.sqr, form = df$elevation)      #' Big trouble.
plotResiduals(E1.sqr, form = df$agriculture)    #' Big trouble.
plotResiduals(E1.sqr, form = df$artificial)     #' Big trouble.
plotResiduals(E1.sqr, form = df$natural)        #' Big trouble.
plotResiduals(E1.sqr, form = df$year)           #' Big trouble.
plotResiduals(E1.sqr, form = df$waterbody_type) #' Big trouble.


#* Subsection 8.6: Zero inlfation----
testZeroInflation(E1.sqr)
# Big trouble!


#* Subsection 8.6: Check for spatial dependency----

#' Option 1: Plot the residuals vs spatial locations. Look for patterns.
#' Option 2: Apply Moran's I test on the residuals.
#' Option 3: Make a variogram of the residuals


#' Option 1:  Plot the residuals vs spatial locations
#' Use scaled quantile residuals:
df$E1 <- residuals(E1.sqr)

#' Note that scaled quantile residuals are around 0.5, whereas
#' Pearson residuals are around 0.
#' Define colour and point size.
df$MyCol  <- ifelse(df$E1 >= 0.5, "red", "blue")
df$MySize <- rescale(abs(df$E1), to = c(0, 3))

p <- ggplot(data = CroppedLithuania_UTM) +
  geom_sf(fill = "transparent") +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm,
                 col = MyCol,
                 size = MySize),
             alpha = 0.5)  +
  scale_color_identity() +
  scale_size_continuous(range = c(1, 3)) +
  theme_minimal() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Residuals")
p + facet_grid(~waterbody_type) # <- nice preparation for later
#' Spatial spatial patterns?




#' Option 2: Apply Moran's I test on the residuals
#' DHARMa can test for spatial correlation using Moran's I test.
# testSpatialAutocorrelation(E1.sqr,
#                            x = df$Xkm,
#                            y = df$Ykm)

# Can also add a few centimetres to each site using rnorm to allow it to run

# test spatial autocorrelation
groupLocations = aggregate(df[, c("Xkm", "Ykm")], list(df$site_id), mean)
res_space = recalculateResiduals(E1.sqr, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
#' There is spatial dependency in the residuals.

#' Option 3: Make a variogram.
#' Make a sample variogram of the residuals.
MyData <- data.frame(E1  = df$E1,
                     Xkm = df$Xkm,
                     Ykm = df$Ykm)

# make a subset for lakes and rivers and rerun the variogram to see the differences between the river and lake dependency

#' Convert to sf object.
MyData_sf <- st_as_sf(x = MyData,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Apply the variogram function from gstat.
V1 <- variogram(E1 ~ 1,
                data = MyData_sf,
                cressie = TRUE)

#' Plot the variogram
p <- ggplot(data = V1, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_smooth(se = TRUE, span = 0.9) +
  labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size=15),
        legend.position="none")
p
#' We have spatial correlation up to about 100 km.
#' We need to deal with that.



#* Subsection 8.7: Check for temporal dependency----
# test temporal autocorrelation
# Need to be careful here. plot the residuals vs year as a better option.
# Or make a variagram with 1 dimension, year.
res_time = recalculateResiduals(E1.sqr, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# Fine



#* Subsection 8.8: Conclusions model validation----

#' We have overdispersion.
#' We have non-linear residual patterns.
#' We have spatial dependency up to 100 km.



# Section 9: Poisson GLM with spatial dependency in INLA----


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



#* Subsection 9.1: Make a mesh (example 1)----

#' We first need to get a sense what the distances are between the
#' sampling locations.
Loc <- cbind(df$Xkm, df$Ykm)
head(Loc)

#' This is in km. Avoid using coordinates with large values small.
#' Use km instead of meters!

#' Distances between sites.
D <- dist(Loc)
par(mfrow = c(1,1), mar = c(5,5,2,2))
hist(D,
     freq = TRUE,
     main = "",
     xlab = "Distance between sites (km)",
     ylab = "Frequency")
#' Small scale for these data is anything between 0 and 100-ish km


#' Next we make the mesh. Main tools to control the mesh:
#' - max.edge: The largest allowed triangle length.
#'             The lower the value for max.edge the higher the resolution.
#' - cutoff:   Sites with a distance value smaller than the cutoff
#'             are modelled by a single vertex in the mesh.
#' - boundary: Useful for islands and fjords.
#  - offset:   Determines how far the inner and outer boundaries extend.


#' - We need an inner area and an outer area. This is to avoid
#'   numerical estimation problems for the points close to the
#'   boundary.
#' - The blue line below splits the data in an inner area
#'   and outer area. The resolution of the outer area can be
#'   lower. The general recommendation is: 1:5.
#'   This explains the:   max.edge = c(1, 5) * MaxEdge in the code below.

#' - General recommendation is to set the cutoff to 1/5 of the
#'   maxedge of the inner area.
#'   This explains the:  cutoff = MaxEdge / 5.

#' - The general recommendation is to come up with an initial
#'   guess for the range (100 km based on the variogram
#'   applied to the residuals of the model without a spatial
#'   term).
#'   This explains the:  RangeGuess <- 100

#' - The maxedge in the inner area is than 1/5 of this.
#'   Hence the:  MaxEdge <- RangeGuess / 5

#' See also:
#' Section 3.1 at: https://haakonbakka.bitbucket.io/btopic104.html

#' The finer the mesh, the (potentially) more accurate the solution,
#' but the longer the computing time.


#' Select a value for range guess (same units at the sampling locations).
RangeGuess <- 50
MaxEdge    <- RangeGuess / 5

#' Older INLA versions used inla.mesh.2d() to make a mesh. This is
#' now depreciated in favour of fm_mesh_2d().
mesh1 <- fm_mesh_2d(loc = Loc,
                    max.edge = c(1, 5) * MaxEdge,
                    cutoff = MaxEdge / 5)

#' Here is the mesh we just created.
par(mfrow = c(1, 1), mar=c(0, 0, 0, 0))
plot(mesh1, asp=1, main = "")
points(Loc, col = 2, pch = 16, cex = 1)


#' This is the size of the mesh
mesh1$n
#' That is a small mesh size.


#' We can also use ggplot to plot the mesh. Ensure mesh1 has the same
#' CRS as Lithuania_UTM. This step can be dropped if you don't use a
#' CRS for your coordinates.
fm_crs(mesh1) <- st_crs(CroppedLithuania_UTM)


#' Plot the mesh using ggplot2.
ggplot() +
  theme_minimal() +
  labs(title = "Border in UTM") +
  geom_fm(data = mesh1) +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.5)  +
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          col = "red")
#' Nice.



#* Subsection 9.2: Make a mesh (example 2)----

#' Do you know the difference between convex and non-convex?
#' An grape is convex. A banana is non-convex.

#' The disadvantage of the mesh that we just made is that the
#' blue line defines a convex area. As a result there are a lot
#' of triangles in the south-western part of the La Palma, and we do
#' not have any sampling locations there.  Maybe a non-convex area is
#' better as it avoids a lot of small triangles in that part.

RangeGuess <- 50               #' Define the resolution of the mesh.
MaxEdge    <- RangeGuess / 5   #' Define the resolution of the mesh.

#' fm_nonconvex_hull makes a non-convex area around the sampling
#' locations. You can control how close the blue line is to the
#' sites with the 'convex' argument. Useful if you have isolated
#' patches of sampling locations.
NonConBoundary <- fm_nonconvex_hull(Loc, convex = -0.08)

#' Make mesh2.
mesh2 <- fm_mesh_2d(boundary = NonConBoundary,
                    max.edge = c(1, 5) * MaxEdge,
                    cutoff   = MaxEdge / 5)


#' #' Ensure the mesh has the same crs.
fm_crs(mesh2) <- st_crs(CroppedLithuania_UTM)

#' Use ggplot to plot mesh2.
ggplot() +
  theme_minimal() +
  labs(title = "Border in UTM") +
  geom_fm(data = mesh2) +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.5)  +
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          col = "red")
#' Note that the blue line now follows the spatial locations of
#' the sampling locations.


#' The number of nodes in this mesh is:
mesh2$n #' Slightly less

#' We will use both meshes in the remaining part of this exercise.



#* Subsection 9.3: Define the projector matrix A----

#' We now define the weighting factors a_ik (also called
#' the projector matrix). This is the A matrix and it can be
#' used to calculate the spatial correlated random effects u via:
#' u = A * w, where the w's are estimated on the nodes of the mesh.

#'  Define projector matrix for meshes 1 and for mesh 2.
A1 <- inla.spde.make.A(mesh1, loc = Loc)
A2 <- inla.spde.make.A(mesh2, loc = Loc)
dim(A1)  #' 1854  sites and 2911 nodes in the mesh.
dim(A2)  #' 1854  sites and 2843 nodes in the mesh.


#' There are the weighting factors in these matrices.
A1[1,] #' Sampling locations are on the nodes.
A1[2,] #' Each row is a site.
A1[3,] #' The columns are the weighting factors.

A2[1,] #' Same for mesh 2.
A2[2,] #' Each row is a site.
A2[3,] #' The columns are the weighting factors.



#* Subsection 9.4: Define the SPDE----

#' We need to specify priors for the two Matern correlation
#' parameters Range and sigma. This is not the RangeGuess that we
#' defined earlier.


#' In short: The user needs to select range0, sigma0 and alpha in:
#'   P(Range < range0) = alpha
#'   P(sigma > sigma0) = alpha


#' These are defined in INLA as:
#'    prior.range = c(range0, alpha)
#'    prior.sigma = c(sigma0, alpha)


#' These are Penalised Complexity (PC) priors
#' See:  Constructing Priors that Penalize the Complexity
#'       of Gaussian Random Fields.
#'       Fuglstad et al.
#'       arXiv:1503.00256v4 [stat.ME] 27 Nov 2017


#' Examples PC priors:
#'  P(Range < 100 km) = 0.05
#'  Most likely the range is larger than 100 km.

#'  P(Range < 1 km) = 0.05
#'  Most likely the range is larger than 1 km.


#'  P(Range < 100 km) = 0.95
#'  Most likely the range is smaller than 100 km.

#'  P(Range < 1 km) = 0.95
#'  Most likely the range is smaller than 1 km.


#'  For semi-diffuse priors use alpha = 0.5:
#'  P(Range < 100 km) = 0.5
#'  No idea. The range can be anything, though we think that
#'  it is big-ish.

#   P(Range < 1 km) = 0.5
#'  No idea. The range can be anything, though we think that
#'  it is small-ish.


#' Specifying a sensible value for sigma is slightly more difficult.
#' Suppose that the covariates are not important. In that case the
#' random effects u_i need to do the hard work.

#'  Here is a quick and dirty way to get an impression of
#'  likely values for sigma. This trick only works for models
#'  with a log-link function.
#'  1. Log-transform the Y data:
df$LogCounts <- log(df$Counts + 1)

#' 2. Apply a lm on LogSR with only an intercept.
Test <- lm(LogCounts ~ 1,
           data = df)
summary(Test)

#' This output can be written as:
#'  LogSR_i = 0.89550 + Residuals_i
#'  Residual_i ~ N(0, 1.418^2)

#' And this can be written as:
#'   SR_i = exp(0.89550 + Residuals_i)
#'
#' It is actually a little bit more complicated (look up a
#' lognormal distribution). But this gives an indication that
#' the u_i ~ N(0, 1.418^2) would be a decent starting point.
#' Maybe we should be conservative, and use:
#'  u_i ~ N(0, 2^2)

#' This gives the following prior:  P(sigma_u > 2) = 0.05
#' This states that it is unlikely that sigma_u is larger than 2.



#' Summarising, we will use the following PC priors:
#'   P(Range < 75 km) = 0.5 <-  the range is likely larger than 75km
#'   P(sigma_u > 2) = 0.05 <- the sigma is likely smaller than 1.5


#' In INLA coding, this is:
spde1 <- inla.spde2.pcmatern(mesh1,
                             prior.range = c(75, 0.05),
                             prior.sigma = c(2, 0.05))
spde2 <- inla.spde2.pcmatern(mesh2,
                             prior.range = c(75, 0.05),
                             prior.sigma = c(2, 0.05))



#* Subsection 9.5: Define the spatial field----

#' Next, we define the spatial random intercept u.
#' Recall that the u = A * w.

#' The size of the mesh, mesh1$n, is also in:
spde1$n.spde

# For mesh 1 we use:
w1.index <- inla.spde.make.index(name = 'w',
                                 n.spde = spde1$n.spde,
                                 n.group = 1,
                                 n.repl = 1)
w1.index

#' The size of the mesh, mesh2$n, is also in:
spde2$n.spde

# For mesh 2 we can use:
w2.index <- inla.spde.make.index(name = 'w',
                                 n.spde = spde2$n.spde,
                                 n.group = 1,
                                 n.repl = 1)



#* Subsection 9.6: Make a stack----

#' Make the X matrix using model.matrix()
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type,
                  data = df)
X <- as.data.frame(X) #' Avoids an error message in INLA
colnames(X)


#' This is not relevant for this specific model:
#' In INLA, you can't use ':' in the variable names. Note that the
#' interaction term uses : in its name. Replace the : by '_'.
#' OldColnames  <- colnames(X)
#' NewColNames  <- gsub(pattern = ":", replacement = "_", x = OldColnames)
#' colnames(X)  <- NewColNames
#' head(X)



#' We now define the stack. This is INLA's way to combine data that
#' has been sampled at different spatial resolutions. .
N <- nrow(df)                          #' Sample size
Stack1 <- inla.stack(
    tag = "Fit",                       #' Name of stack
    data = list(Counts = df$Counts),   #' Response variable
    A = list(1, 1, A1),                #' The order matches the order in effects.
    effects = list(
       Intercept = rep(1, N),          #' Use our own intercept
       X         = X[,-1],             #' Dump the intercept from the model.matrix
       w         = w1.index))          #' Spatial random field


#' And this the stack based on mesh 2.
Stack2 <- inla.stack(
    tag = "Fit",                       #' Name of stack
    data = list(Counts = df$Counts),   #' Response variable
    A = list(1, 1, A2),                #' The order matches the order in effects.
    effects = list(
   Intercept = rep(1, N),              #' Use our own intercept
          X  = X[,-1],                 #' Dump the intercept from the model.matrix
          w  = w2.index))              #' Spatial random field



#* Subsection 9.7: Specify the model formula----

#' Specify the model formula in terms of the response variable,
#' covariates and the spatial correlated term. Having the colnames
#' is handy at this stage:
colnames(X)


f1.lm <- Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                       agriculture.std + artificial.std + natural.std + 
                       f(year, model = "rw2", scale.model = TRUE, hyper = MyPCPrior, constr = TRUE) + 
                       waterbody_type.L

f2.mesh1 <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                           agriculture.std + artificial.std + natural.std + 
                           f(year, model = "rw2", scale.model = TRUE, hyper = MyPCPrior, constr = TRUE) + 
                           waterbody_type.L +
                           f(w, model = spde1)

#' This is the model with spatial dependency, based on mesh 2.
f2.mesh2 <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                    agriculture.std + artificial.std + natural.std + 
                       f(year, model = "rw2", scale.model = TRUE, hyper = MyPCPrior, constr = TRUE) + 
                       waterbody_type.L +
                       f(w, model = spde2)



#* Subsection 9.8: Execute the INLA models----


#' First we run the model without spatial dependency.
I1.lm <- inla(f1.lm,
              family = "poisson",
              data = inla.stack.data(Stack1),
              control.compute = MyControlCompute,
              control.predictor = list(A = inla.stack.A(Stack1)))


#' This is the spatial model based on mesh 1.
I2.mesh1 <- inla(f2.mesh1,
                 family = "poisson",
                 data = inla.stack.data(Stack1),
                 control.compute = MyControlCompute,
                 control.predictor = list(A = inla.stack.A(Stack1)))


#' This is the spatial model based on mesh 2.
I2.mesh2 <- inla(f2.mesh2,
                 family = "poisson",
                 data = inla.stack.data(Stack2),
                 control.compute = MyControlCompute,
                 control.predictor = list(A = inla.stack.A(Stack2)))



#* Subsection 9.9: Compare the INLA models----

#' Compare DIC and WAIC.
DICs <- c(I1.lm$dic$dic, I2.mesh1$dic$dic, I2.mesh2$dic$dic)
WAICs <- c(I1.lm$waic$waic, I2.mesh1$waic$waic, I2.mesh2$waic$waic)
Results <- data.frame(Models = c("Poisson GLM",
                                 "Poisson GLM + SRF with mesh 1",
                                 "Poisson GLM + SRF with mesh 2"),
                      DIC = DICs,
                      WAIC = WAICs)
Results

#' Conclusions:
#'  - Adding spatial dependency greatly improves the models.
#'  - Mesh 2 is better than mesh 1.

#' We will use mesh2 in the remaining part of the analyses



# Section 10: Imposed spatial dependency----

#' Here is some fancy code to extract the posterior mean values of the
#' spatial parameters.
SpatialParams <- MySpatialParams(Model = I2.mesh2,
                                 ThisSpde = spde2)

Kappa   <- SpatialParams[1]  #' Parameter kappa for the Mattern correlation function
Sigma.u <- SpatialParams[2]  #' Sigma of the u
Range   <- SpatialParams[3]  #' expressed in metres

#' This is the important part:
Sigma.u   #' u_i ~ N(0, 7.289291^2 * Spatial correlation) - we initial sigma would not be larger than 2 (spde has big and negative and positive values)
Range     #' Distance at which the correlation diminishes.
          #' That is a small range! Much smaller than we specified
          #' with the PC prior. <-  75 km 

# i.e., the wrong distribution

#' Visualise the correlation structure.
#' First we obtain the locations of each point of the mesh.
LocMesh <- mesh2$loc[,1:2]

#' And then we calculate the distance between each vertex.
D <- as.matrix(dist(LocMesh))

#' Using the estimated parameters from the model (see above)
#' we can calculate the imposed Matern correlation values.
d.vec <- seq(0, max(D), length = 100)
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1)
Cor.M[1] <- 1

#' Which we plot here:
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec,
     y = Cor.M,
     pch = 16,
     type = "l",
     cex.lab = 1.5,
     xlab = "Distance (km)",
     ylab = "Correlation",
     xlim = c(0, 30))
abline(h = 0.8, lty = 2, col = 2)
abline(h = 0.5, lty = 2, col = 2)
abline(h = 0.1, lty = 2, col = 2)

#' Define strong correlation as correlation between 1 - 0.8.
#' Define moderate correlation as correlation between 0.8 - 0.5.
#' Define weak correlation as correlation between 0.5 - 0.1.
#' Define diminishing correlation as correlation smaller than 0.1.

#' In this case, we have:
#'  - Strong correlation between sites separated between 0 - 2.5 km.
#'  - Moderate correlation between sites separated between 2.5 - 4 km.
#'  - Weak correlation between sites separated between 4 - 8 km.
#'  - Diminishing correlation between sites separated by more than 8 km



# Section 11: Plotting the spatial dependency----


#' Next we present the spatial component, the wks.
#' Their posterior mean values can be obtained via
w.pm <- I2.mesh2$summary.random$w$mean
length(w.pm)

#' This is a vector of length 2843 by 1. Each value in w.pm belongs
#' to a specific vertex on mesh 2. It is tempting to plot these,
#' but that won't work as geom_raster() wants to have data on a
#' regular grid. INLA has nice tools to predict the w.pm values on a
#' grid.



#' More recent INLA versions use fm_evaluator() and fm_evaluate().
#' We first need to call fm_evaluator() followed by a call to
#' fm_evaluate(). See below.
#'  - The dims = c(400, 400) specifies a 400-by-400 grid within
#'    the ranges of the mesh.
#'  - Proj contains the A matrix for this 400-by-400 grid.
#'  - fm_evaluate() gives the values of the SRF on a regular grid
#'    defined by fm_evaluator().
Proj <- fm_evaluator(mesh = mesh2,
                     dims = c(400, 400))
#' Proj$x contains the 400 values along the x-axis.
#' Proj$y contains the 400 values along the y-axis.
#' Proj$proj$A contains the projector matrix.

SRF.Proj <- fm_evaluate(projector = Proj,
                        field  = w.pm)
#' SRF.Proj contains SRF values on the grid that was just defined.

#' Extract the relevant information so that we can use ggplot
#' code from previous exercises to plot the results
MyData      <- expand.grid(Xkm = Proj$x, Ykm = Proj$y)
MyData$w.pm <- as.vector(SRF.Proj)

#' We now have a grid with 400 x 400 = 160,000 points, and
#' the w.pm values at these points.


#' For all sites not on Lithuania, set the w.pm to NA.
#' To do this, we use the st_contains function from the
#' sf package. It will tell us which points are on
#' Lithuania (InLithuania = 1) or outside Lithuania 
#' (OutLithuania = NA).

#' Create an sf object from the coordinates
sputm <- st_as_sf(MyData,
                  coords = c("Xkm", "Ykm"),
                  crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km +datum=WGS84")


#' Set CRS if necessary.
#st_crs(InLaPalma) <- st_crs(LaPalma_UTM)


#' Determine which points in MyData are contained within CroppedLithuania_UTM
InLithuania <- st_contains(CroppedLithuania_UTM, sputm)

#' Get the indices of the points contained within Lithuania
#' contained_indices <- unlist(st_contains(InLithuania, sputm))
contained_indices <- unlist(InLithuania)



#' Create a logical vector where TRUE indicates the point is in La Palma
in_Lithuania <- rep(FALSE, nrow(MyData))
in_Lithuania[contained_indices] <- TRUE

# Update the 'w.pm' column of MyData.
MyData$w.pm[!in_Lithuania] <- NA
head(MyData)



#' Plot the w.pm values.
SRF.mesh2 <- ggplot(data = CroppedLithuania_UTM) +
  coord_fixed() +
  geom_sf(fill = "transparent") +
  geom_raster(data = MyData,
              aes(x = Xkm,
                  y = Ykm,
                  fill = w.pm)) + # <---- here you can put exp(w.pm) - quick and dirty, otherwise use emarginal from INLA
  scale_fill_gradient2(midpoint = 0,
                       low = "blue",
                       mid = "white",
                       high = "red",
                       na.value = NA) +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.3,
             size = 0.4)  +
  theme_minimal() +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Spatial random field")
SRF.mesh2


#' Interpretation:
#' Recall that the model is of the form:
#'  E[Counts_i] = mu_i
#'  mu_i = exp( Intercept + Covariates_i + u_i )
#'       = exp( Intercept + Covariates_i) * exp(u_i)

#' What you see in the graph on the right is the information in
#' Counts_i that cannot be explained by the covariates, and is spatial
#' in nature (the u_i...or actually the w_k).

#' In other words, while taking into account the covariate effects,
#' there is a second process present in the Counts_i data, and it is
#' visualised in the figure.

#' Note:
#'  -The covariates and the u_i are estimated at the same time.
#'  - We are visualising the u_i and w_k. If you were to visualise
#'    exp(w_k) you get multiplication factors. In the red areas you
#'    have exp(20) = 485165195 times more expected species richness as compared
#'    to white areas. That does not seem right!


# Section 12: Accessing the u_i----


# Can we access the u_i values? Yes...that is very simple.

#' The Loc below contains the sampling locations.
Proj.ui <- fm_evaluator(mesh = mesh2, loc = Loc)
#' This gives a different A matrix as before.

#' We can now use u = A * w.pm to calculate the u.
u.pm <- fm_evaluate(projector  = Proj.ui, field = w.pm)

#' So, we have produced the u_i values at specific sampling locations.
#' This also means that you can predict the u_i at any location; just
#' adjust the Loc coordinates.

#' And u.pm is the same as   A1 %*% w.pm   where the A1 was defined
#' in Subsection 9.3.


#' Bottom line is that we can predict a u_i value at any
#' location inside the mesh.

#' Here are the u's with their spatial coordinates.
MyData.u <- data.frame(u.pm = u.pm,
                       Xkm = Loc[,1],
                       Ykm = Loc[,2])

#' If you want, you can give it the appropriate CRS,
#' and superimpose the u.pm values on the map (e.g.
#' with different colours based on the sign, and make
#' the dot size proportional to the value).




# Section 13: Model validation for the INLA Poisson GLM I2.mesh2----

#* Subsection 13.1: Getting scaled quantile residuals----

#' We would like to simulate 1000 data sets, because:
#'  1. We can use these to assess whether the model is overdispersed.
#'  2. In case of zero-inflation, we can use the 1000 simulated data
#'     sets to assess whether the model can cope with the zero
#'     inflation.
#'  3. We can use them to obtain scaled quantile residuals.


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, I2.mesh2)


#' Extract the 1000 sets of regression parameters using
#' inla.posterior.sample.eval()
MyVar <- rownames(I2.mesh2$summary.fixed)
MyVar        #' Names of the regression parameters
Betas1000 <- inla.posterior.sample.eval(MyVar, SimData)



#' Now the spatial correlation random effects.
#' Determine the names of the random effects
names.SimData <- attributes(SimData[[1]]$latent)$dimnames[[1]]
wNames <- names.SimData[grep("w:", names.SimData)] # <--- needed to change this to w: because wind speed and waterbody_type is messing it up

#' Determine on which rows the random effects are.
MyID     <- function(x){ which(rownames(SimData[[1]]$latent) == x) } # <--- check if this is the same function that I (Nathan) made myself
RowsW <- lapply(wNames, MyID)
RowsW <- as.numeric(RowsW)
RowsW
NROW(RowsW)

#' If these are not numbers, then type:  SimData[[1]]$latent and
#' see how you need to change the paste function above
#' These are the row numbers in SimData[[1]]$latent that
#' contain the simulated random effect values




#' Calculate 1000 times the fitted values via mu = exp(X * beta).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std +
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type,
                  data = df)
X <- as.matrix(X)



#' Start a loop. In each iteration:
#'   1. Calculate the fitted values mu.
#'   2. Simulate Poisson data with the mean equal to mu.
N    <- nrow(df)                      #' Sample size
Ysim <- matrix(nrow = N, ncol = 1000) #' Create space

#' Start the loop (can be done more efficient with lapply).
for (i in 1:1000){
   Betas <- Betas1000[,i]
   wk    <- SimData[[i]]$latent[RowsW]
   FixedPart   <- X %*% Betas
   SpatialPart <- A2 %*% wk
   mu          <- exp(FixedPart + SpatialPart)[,1]
   Ysim[,i]    <- rpois(n = N, lambda = mu)  #' Simulated Poisson count data.
}

#' We now have 1000 simulated data sets from the model.
Ysim[, 1] #' First simulated data set.
Ysim[, 2] #' Second simulated data set.
Ysim[, 3] #' Third simulated data set.
#' Etc.

#' Or:
par(mfrow = c(2,2))
hist(df$Counts, main ="Observed Counts")
hist(Ysim[,1], main = "First simulated data set")
hist(Ysim[,2], main = "Second simulated data set")
hist(Ysim[,3], main = "Third simulated data set")
par(mfrow = c(1,1))
#' Actually, it looks pretty good!



#' We have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
N <- nrow(df)
df$Fit <- I2.mesh2$summary.fitted.values[1:N, "mean"]  #' Fitted values.


E2.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$Counts,
                       fittedPredictedResponse = df$Fit,
                       integerResponse = TRUE)
#' Now we have scaled quantile residuals.



#* Subsection 13.2: Check for overdispersion----

#' We will use the scaled quantile residuals to assess for
#' overdispersion.
testDispersion(E2.sqr)
#' Looks good.


#' Dispersion statistic obtained via a frequentist
#' way of thinking:
E2.pr <- (df$Counts - df$Fit ) /sqrt(df$Fit )
sum(E2.pr^2) / (N - 11) # <--- in principle, this should be 13 because the 11 ignores the parameters from the spatial random field
#' About 17.991. That is just at the point where you start to worry.

#' Let us recap what is happening. The model without spatial correlation
#' is heavily overdispersed. The model with spatial correlation is
#' not overdispersed. That means that the spatial term itself
#' is working relatively well. We can control it by adjusting the 
#' priors on the range parameter:

#'   P(Range < 100 km) = 0.00001 # <---- forces INLA to take the range for at least 5 km!!!! Could help solve the underdisperson problem.
#'   P(sigma > 4) = 0.05

#* Subsection 13.3: Check for homogeneity of variance----

#' Plot the scaled quantile residuals versus (ranked) fitted values.
plotResiduals(E2.sqr, quantreg = TRUE, smoothScatter = FALSE)
#' Less Big trouble!



#* Subsection 13.4: Check for normality of the residuals----

#' In DHARMa, we verify whether the scaled quantile residuals are
#' uniform distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(E2.sqr,
           testUniformity = TRUE,
           testOutliers = TRUE,
           testDispersion = FALSE)
#' Big trouble!



#* Subsection 13.5: Plot residuals versus the covariates----

#' Plot the scaled quantile residuals versus each covariate
#' in the model.
plotResiduals(E2.sqr, form = df$eqr)            #' Less big trouble.
plotResiduals(E2.sqr, form = df$ppt)            #' Some trouble.
plotResiduals(E2.sqr, form = df$tmin)           #' Some trouble.
plotResiduals(E2.sqr, form = df$ws)             #' Some trouble.
plotResiduals(E2.sqr, form = df$elevation)      #' Some trouble.
plotResiduals(E2.sqr, form = df$agriculture)    #' Some trouble.
plotResiduals(E2.sqr, form = df$artificial)     #' Some trouble.
plotResiduals(E2.sqr, form = df$natural)        #' Some trouble.
plotResiduals(E2.sqr, form = df$year)           #' Some trouble.
plotResiduals(E2.sqr, form = df$waterbody_type) #' Some trouble.



#* Subsection 13.6: Zero inlfation----
testZeroInflation(E2.sqr)
# Big trouble!



#* Subsection 13.7: Check for spatial dependency----

#' Option 1: Plot the residuals vs spatial locations. Look for patterns.
#' Option 2: Apply Moran's I test on the residuals.
#' Option 3: Make a variogram of the residuals


#' Option 1:  Plot the residuals vs spatial locations
#' Use scaled quantile residuals:
df$E2 <- residuals(E2.sqr)

#' Note that scaled quantile residuals are around 0.5, whereas
#' Pearson residuals are around 0.
#' Define colour and point size.
df$MyCol  <- ifelse(df$E2 >= 0.5, "red", "blue")
df$MySize <- rescale(abs(df$E2), to = c(0, 3))

p <- ggplot(data = CroppedLithuania_UTM) +
  geom_sf(fill = "transparent") +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm,
                 col = MyCol,
                 size = MySize),
             alpha = 0.5)  +
  scale_color_identity() +
  scale_size_continuous(range = c(1, 3)) +
  theme_minimal() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Residuals")
p + facet_grid(~waterbody_type)
#' Spatial spatial patterns?



#' Option 2: Apply Moran's I test on the residuals
#' DHARMa can test for spatial correlation using Moran's I
#' test.
# testSpatialAutocorrelation(E2.sqr,
#                            x = df$Xkm,
#                            y = df$Ykm)

# test spatial autocorrelation
# groupLocations = aggregate(df[, c("Xkm", "Ykm")], list(df$site_id), mean)
res_space = recalculateResiduals(E2.sqr, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
#' There is much less spatial dependency in the residuals.

#' Option 3: Make a variogram.
#' Make a sample variogram of the residuals.
MyData <- data.frame(E2  = df$E2,
                     Xkm = df$Xkm,
                     Ykm = df$Ykm)

#' Convert to sf object.
MyData_sf <- st_as_sf(x = MyData,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Apply the variogram function from gstat.
V2 <- variogram(E2 ~ 1,
                data = MyData_sf,
                cressie = TRUE)

#' Plot the variogram
p <- ggplot(data = V2, aes(x = dist, y = gamma)) +
      geom_point() +
      geom_smooth(se = TRUE, span = 0.9) +
      labs(x = "Distance (in km)", y = "Semi-variogram") +
      theme(text = element_text(size=15),
            legend.position="none")
p
#' That is an odd pattern!


#' Let us compare this with the variogram obtained from the
#' scaled quantile residuals of the model without spatial dependency.
#' Read the variogram results from the previous exercise.
p <- ggplot() +
  geom_point(data = V1,
             aes(x = dist, y = gamma),
             col = "red") +
  geom_point(data = V2,
             aes(x = dist, y = gamma),
             col = "blue") +
  geom_smooth(data = V1,
              aes(x = dist, y = gamma),
              col = "red",
              se = TRUE,
              span = 0.9) +
  geom_smooth(data = V2,
              aes(x = dist, y = gamma),
              col = "blue",
              se = TRUE,
              span = 0.9) +
    labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size=15),
        legend.position="none")
p
#' You need to see the results in perspective!
#' Blue is the spatial Poisson GLM.
#' Red is the Poisson GLM.

#* Subsection 13.8: Check for temporal dependency----
# test temporal autocorrelation
res_time = recalculateResiduals(E2.sqr, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# Fine



#* Subsection 13.9: Conclusions model validation----

#' We have strange patterns in the residuals of all the covariates.
#' We have zero inflation.
#' The spatial autocorrelation is a flat line, but the model with
#' the spatial random field has higher values than the model without
#' the spatial random field.... weird.



# Section 14: Compare models I1, I2.mesh1 and I2.mesh2----

#' Let's plot the results of the model, without and with the
#' spatial correlation side by side.
Out1 <- I1$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2 <- I2.mesh1$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out3 <- I2.mesh2$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
rownames(Out1) <- rownames(Out2) <- rownames(Out3) <- I1$names.fixed

#' Names for the models:
MyNames <- c("GLM",
             "Spatial GLM mesh 1",
             "Spatial GLM mesh 2")

#' Compare results of the two models using MyCompareBetasofModels
#' (which is in our support file).
MyCompareBetasofModels(AllModels = list(Out1, Out2, Out3),
                       ModelNames = MyNames)

#' There are some differences between the model without, and with
#' spatial dependency. There are also differences between
#' the two spatial models.



# Section 15: Model formulation NB GLM----

#' We could have applied the following Poisson GLM to the vector data.

#'  -Counts_ij is the number of vectors at site i in year j.
#'  -The subscript i refers to site.
#'  -The subscript j refers to year.

#'  Counts_ij ~ Poisson(mu_ij)
#'  E(Counts_ij) = mu_ij
#'  var(Counts_ij) = mu_ij

#' As to mu_ij, we will consider:
#'  Model 1: log(mu_ij) =  Intercept + covariates + fYear_ij
#'  Model 2: log(mu_ij) =  Intercept + covariates + fYear_ij + u_i
#'  Model 3: log(mu_ij) =  Intercept + covariates + fYear_ij + u_is
#'  Model 4: log(mu_ij) =  Intercept + covariates + fYear_ij + u_is

#' Model 1: Year is a factor.
#' Model 2: Year is a factor and u_i is spatial correlation.
#' Model 3: Year is a factor and u_is is spatial correlation that
#'          is allowed to change over the years. The
#'          change from year-to-year can be random.
#'          But the SRF for each year shares the same spatial parameters.
#' Model 4: Year is a factor and u_is is spatial correlation that
#'          is allowed to change over the years. The
#'          change from year-to-year is fairly smooth; we will use
#'          an auto-regressive process .


#' As to the spatial models, we should have formulated the
#' underlying questions like:
#'  - Is the spatial pattern constant in time (model 2).
#'  - Does the spatial correlation change rapidly from
#'    year-to-year (model 3)?
#'  - Does the spatial correlation change smoothly over time
#'    (model 4)?


#' These questions justify the application of a model
#' with spatial correlation (model 2), replicate correlation (model 3)
#' and auto-regressive correlation (model 4). Model 1 is a reference
#' model. It is also an option to use a GLMM as reference model, with
#' random effect site.


#' Initial models using the Poisson distribution were generally overdispersed, 
#' and could not cope with the zero inflation. I therefore apply a NB GLM.
#' Such a model is specified by:

#'  Counts_ij ~ NB(mu_ij, k)
#'  E(Counts_ij) = mu_ij
#'  var(Counts_ij) = mu_ij + mu_ij^2 / k

#' We will use the same four models for mu_ij.

#' A zero-inflated poisson (ZIP) is an alternative approach.
#' A zero-inflated negative binomial (ZINB) is an alternative approach.
#' A zero-altered negative binomial (ZANB) is an alternative approach.


#* Subsection 15.1: Define new settings----
#' We define some common settings for INLA.
MyControlCompute  <- list(config = TRUE,    #' Allow for posterior simulation
                          dic = TRUE,       #' Calculate AIC
                          waic = TRUE,      #' Calculate WAIC
                          residuals = TRUE) #' Get residuals (see below)
MyControlPredictor  <- list(compute = TRUE, #' Calculate fitted values
                            link = 1)       #' Predict on the scale of
                                            #' the response variable.


# Section  16: NB GLM without spatial correlation----


#' Execute the reference model.
#' model 1: mu_ij = exp(Intercept + covariates)
I1 <- inla(Counts ~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type,
           data = df,
           control.compute = MyControlCompute,
           family = "nbinomial")


#' Numerical output for the regression parameters.
BetasI1 <- I1$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
print(BetasI1, digits = 2)


#' Posterior mean of theta (as in: mu + mu^2 / theta)
theta.pd <- I1$marginals.hyperpar$`size for the nbinomial observations (1/overdispersion)`
theta.pm <- inla.emarginal(function(x) x, theta.pd)
theta.pm


#' Visualise the betas using ggplot2.
#' 1. Create a data frame.
BetasI1_df <- data.frame(Covariate = rownames(BetasI1),
                         Mean      = BetasI1[,"mean"],
                         Lower     = BetasI1[,"0.025quant"],
                         Upper     = BetasI1[, "0.975quant"])

#' 2. Plot the info in BetasI1_df.
ggplot(BetasI1_df, aes(x = Mean, y = Covariate)) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper),
                  fatten = 1.5,
                  color = "blue") +
  geom_vline(aes(xintercept = 0),
             linetype = "dotted",
             col = "red") +
  labs(title = "Posterior means and 95% Credible Intervals",
       x = "Value",
       y = "Covariate") +
  theme_minimal()
#' If a 95% CI crosses the dotted red line, then the covariate is
#' important.



# Section 17: Model validation NB GLM----

#* Subsection 17.1: Getting scaled quantile residuals----

#' We will simulate 1000 data sets, because:
#'  1. We can use these to assess whether the model is overdispersed.
#'  2. We can use these to assess whether
#'     the model can cope with the zero inflation.
#'  3. We can use them to obtain scaled quantile residuals.


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, I1)




#' Here is some fancy code to grab the positions of specific variables.
#' Custom function.
MyGrep <- function(x, SimulatedData){
  # SimulatedData is the name of the object containing the simulation results
  # x is an element of BetasInModel
  names.SimData <- attributes(SimulatedData[[1]]$latent)$dimnames[[1]]
  names.SimData[grep(x, names.SimData)]
}


#' What are the names of the regression parameters?
BetasInModel <- rownames(I1$summary.fixed)
BetasInModel

#' Get their location in the object with simulation results.
BetaNames    <- unlist(lapply(BetasInModel,
                              FUN = MyGrep,
                              SimulatedData = SimData))
MyID     <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
RowsBeta <- lapply(BetaNames, MyID)
RowsBeta <- as.numeric(RowsBeta)
RowsBeta




#' Calculate 1000 times the fitted values via: mu = exp(X * beta).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type, data = df)
X <- as.matrix(X)


#' Start a loop. In each iteration:
#'   1. Calculate the fitted values mu.
#'   2. Simulate NB data with the mean equal to mu.
N    <- nrow(df)                      #' Sample size.
Ysim <- matrix(nrow = N, ncol = 1000) #' Create space.

#' Start the loop (can be done more efficient with lapply).
for (i in 1:1000){
  Betas <- SimData[[i]]$latent[RowsBeta]
  FixedPart   <- X %*% Betas
  SpatialPart <- 0                         #' Will change later.
  mu  <- exp(FixedPart + SpatialPart)[,1]
  Ysim[,i] <- rnegbin(n = N,               #' Simulated count data
                      mu = mu,
                      theta = theta.pm)
}


#' Now we have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
N <- nrow(df)
df$Fit <- I1$summary.fitted.values[1:N, "mean"]  #' Fitted values.

E3.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$Counts,
                       fittedPredictedResponse = df$Fit,
                       integerResponse = TRUE)
#' Now we have scaled quantile residuals.


#* Subsection 17.2: Check for overdispersion and zero-inflation----

testDispersion(E3.sqr)    #' Fine
testZeroInflation(E3.sqr) #' Fine



#* Subsection 17.3: Check spatial dependency----

#' We will make a variogram for the residuals.
#' First we need to know the distances between the sites:
Loc <- df[, c("Xkm", "Ykm")]
hist(dist(Loc))
#' That is between 0 and 500-ish km. We should have produced this graph in
#' the data exploration section.

#' Combine residuals and coordinates.
Data4Vario <- data.frame(Res = E3.sqr$scaledResiduals,
                         Xkm = df$Xkm,
                         Ykm = df$Ykm)

#' Convert to sf object.
MyData_sf <- st_as_sf(x = Data4Vario,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Calculate the variogram.
V1 <- variogram(Res ~ 1,
                data = MyData_sf,
                #cutoff = 200,
                cressie = TRUE)

#' Plot the variogram.
ggplot() +
  geom_point(data = V1,
             aes(x = dist, y = gamma)) +
  geom_smooth(data = V1,
              aes(x = dist, y = gamma),
              se = TRUE,
              span = 1,
              alpha = 0.3) +
  labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size = 15))
#' We have spatial dependency.







# Section 18: NB GLM with spatial dependency----

#' We will execute the following model.
#' Model 2. log(mu_ij) =  Intercept + covariates + u_i


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



#* Subsection 18.1: Make a mesh (example 1)----

#' We first need to get a sense what the distances are between the
#' sampling locations.
Loc <- cbind(df$Xkm, df$Ykm)
head(Loc)

#' This is in km. Avoid using coordinates with large values.
#' Use km instead of meters!

#' Distances between sites (i.e. trees).
D <- dist(Loc)
par(mfrow = c(1,1), mar = c(5,5,2,2))
hist(D,
     freq = TRUE,
     main = "",
     xlab = "Distance between sites (km)",
     ylab = "Frequency")

#' Small scale distances for these data is anything between
#' 0 and 100-ish km



#' Next we make the mesh.
#' Select a value for range guess. After trying various values, and
#' running the entire code in this file we settled on the following
#' value.
RangeGuess <- 50 # <--- should be 50
MaxEdge    <- RangeGuess / 5

#' One of the reasons for not using a smaller value for RangeGuess
#' is computing time for the spatial-temporal models. For a real analysis,
#' we would use RangeGuess <- 50, and let the computer run for a full
#' night.


#' Make the mesh.
mesh2a <- fm_mesh_2d(loc = Loc,
                     max.edge = c(1, 5) * MaxEdge,
                     cutoff = MaxEdge / 5)

#' Here is the mesh we just created.
par(mfrow = c(1, 1), mar=c(0, 0, 0, 0))
plot(mesh2a, asp=1, main = "")
points(Loc, col = 2, pch = 16, cex = 1)


#' This is the size of the mesh:
mesh2a$n
#' That is a smallish mesh size. But for the spatial-temporal
#' models with 18 years, we will end up with 18 * mesh2a$n nodes!
#' That is:
18 * mesh2a$n   #' That is a lot.


#' We can also use ggplot to plot the mesh.
ggplot() +
  theme_minimal() +
  labs(title = "Border in UTM") +
  geom_fm(data = mesh2a) +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.5)
#' Nice....but we are consuming a fair amount of vertices
#' in the southern part of the study area. Can we improve this?
#' Those are also in the sea!




#* Subsection 18.2: Make another mesh (example 2)----

#' Make another mesh, with few triangles in the southern part.

#' fm_nonconvex_hull makes a non-convex area around the sampling
#' locations. You can control how close the blue line is to the
#' sites with the 'convex' argument. Useful if you have isolated
#' patches of sampling locations.
NonConvexHull <- fm_nonconvex_hull(Loc, convex = -0.08)
plot(NonConvexHull)
points(Loc)


RangeGuess <- 50  #' Again, we could do with a smaller value,
                  #' but that would increase computing time for the
                  #' spatial-temporal models. It was initially 150

#' Recommended settings
MaxEdge <- RangeGuess / 5
mesh2b <- fm_mesh_2d(boundary = NonConvexHull,
                     max.edge = c(1, 5) * MaxEdge,
                     cutoff   = MaxEdge / 5)
#' max.edge: Maximum allowed triangle edge lengths in
#'           the inner domain and in the outer extension
#' cutoff:   Minimum allowed distance between points. Points
#'           at a closer distance than the supplied value are
#'           replaced by a single vertex


#' Use ggplot to plot mesh2b.
ggplot() +
  theme_minimal() +
  labs(title = "Border in UTM") +
  geom_fm(data = mesh2b) +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm),
             alpha = 0.5)
#' Note that the blue line now follows the spatial locations of
#' the sampling locations.

mesh2b$n #' Fine for the moment, but we need to multiply this with
         #' 18 for the spatial-temporal models.
#' That is:
18 * mesh2b$n   #' That is a lot.



#* Subsection 18.3: Define the projector matrix A----

#' We now define the projector matrix. This is used to
#' calculate:  u = A * w.
A2a <- inla.spde.make.A(mesh2a, loc = Loc)
A2b <- inla.spde.make.A(mesh2b, loc = Loc)
dim(A2a)  #' 1638 sites and 1674 nodes in the mesh.
dim(A2b)  #' 1638 sites and 1678 nodes in the mesh.




#* Subsection 18.4: Define the SPDE----

#' We need to specify priors for the two Matern correlation
#' parameters Range and sigma. This is not the RangeGuess that we
#' defined earlier.

#' In short: The user needs to select range0, sigma0 and alpha in:
#'   P(Range < range0) = alpha
#'   P(sigma > sigma0) = alpha

#' These are defined in INLA as:
#'    prior.range = c(range0, alpha)
#'    prior.sigma = c(sigma0, alpha)



#' Prior for the range.
#'  This is a bit a blind guess, but we will use:
#'   P(Range < 75) = 0.05


#' Prior for sigma.
#'  Here is a quick and dirty way to get an impression of
#'  likely values for sigma. This trick only works for models
#'  with a log-link function.

#'  1. Log-transform the Y data:
df$LogCounts <- log(df$Counts + 1)

#' 2. Apply a lm on LogSR with only an intercept.
Test <- lm(LogCounts ~ 1,
           data = df)
summary(Test)

#' This output can be written as:
#'  LogCounts_i = 0.89550 + Residuals_i
#'  Residual_i ~ N(0, 1.418 ^2)

#' And this can be written as:
#'   LogCounts_i = exp(0.89550 + Residuals_i)
#'
#' It is actually a little bit more complicated (look up a
#' lognormal distribution). But this gives an indication that
#' the u_i ~ N(0, 1.418^2) would be a decent starting point.
#' Maybe we should be conservative, and use:
#'  u_i ~ N(0, 2^2)

#' This gives the following prior:  P(sigma_u > 2) = 0.05
#' This states that it is unlikely that sigma_u is larger than 2.



#' Summarising, we will use the following PC priors:
#'   P(Range < 75 km) = 0.05
#'   P(sigma > 2) = 0.05


#' In INLA coding, this is:
spde2a <- inla.spde2.pcmatern(mesh2a,
                              prior.range = c(75, 0.05),
                              prior.sigma = c(2, 0.05))

spde2b <- inla.spde2.pcmatern(mesh2b,
                              prior.range = c(75, 0.05),
                              prior.sigma = c(2, 0.05))



#* Subsection 18.5: Define the spatial field----

#' Next, we define the spatial random intercept u.
#' For mesh2a we use:
w2a.index <- inla.spde.make.index(name = 'w',
                                  n.spde = spde2a$n.spde,
                                  n.group = 1,
                                  n.repl = 1)

#' For mesh2b we use:
w2b.index <- inla.spde.make.index(name = 'w',
                                  n.spde = spde2b$n.spde,
                                  n.group = 1,
                                  n.repl = 1)



#* Subsection 18.6: Make a stack----

#' Make the X matrix using model.matrix()
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type, data = df)
X <- as.data.frame(X) #' Avoids an error message in INLA
colnames(X)


#' This is not relevant for this specific model:
#' In INLA, you can't use ':' in the variable names. Note that the
#' interaction term uses : in its name. Replace the : by '_'.
#OldColnames  <- colnames(X)
#NewColNames  <- gsub(pattern = ":", replacement = "_", x = OldColnames)
#colnames(X)  <- NewColNames
#head(X)



#' Sample size
N <- nrow(df)


#' We now define the stack for mesh2a.
Stack2a <- inla.stack(
  tag = "Fit",                       #' Name of stack
  data = list(Counts = df$Counts), #' Response variable
  A = list(1, 1, A2a),               #' The order matches the order in effects.
  effects = list(
    Intercept = rep(1, N),  #' Use our own intercept
    X         = X[,-1],     #' Dump the default intercept from the model.matrix
    w         = w2a.index)) #' Spatial random field


#' And this the stack based on mesh2b.
Stack2b <- inla.stack(
  tag = "Fit",                       #' Name of stack
  data = list(Counts = df$Counts), #' Response variable
  A = list(1, 1, A2b),               #' The order matches the order in effects.
  effects = list(
    Intercept = rep(1, N),       #' Use our own intercept
    X         = X[,-1],          #' Dump the default intercept from the model.matrix
    w         = w2b.index))      #' Spatial random field





#* Subsection 18.7: Specify the model formula----

#' Specify the model formula in terms of the response variable,
#' covariates and the spatial correlated term. Having the colnames
#' is handy at this stage:
colnames(X)



#' This is the model without spatial dependency (again).
f1 <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                     agriculture.std + artificial.std + natural.std + 
                     year.std + waterbody_type.L


#' This is the model with spatial dependency, based on mesh 2a.
f2a <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                      agriculture.std + artificial.std + natural.std + 
                      year.std + waterbody_type.L + 
                      f(w, model = spde2a)


#' This is the model with spatial dependency, based on mesh2b.
f2b <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                      agriculture.std + artificial.std + natural.std + 
                      year.std + waterbody_type.L + 
                      f(w, model = spde2b)



#* Subsection 18.8: Execute the INLA models----

#' This is the NB GLM without spatial dependency (again).
I1 <- inla(f1,
           family = "nbinomial",
           data = inla.stack.data(Stack2a),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(Stack2a)))


#' This is the spatial NB GLM based on mesh2a.
I2a <- inla(f2a,
            family = "nbinomial",
            data = inla.stack.data(Stack2a),
            control.compute = MyControlCompute,
            control.predictor = list(A = inla.stack.A(Stack2a)))


#' This is the spatial NB GLM based on mesh2b.
I2b <- inla(f2b,
            family = "nbinomial",
            data = inla.stack.data(Stack2b),
            control.compute = MyControlCompute,
            control.predictor = list(A = inla.stack.A(Stack2b)))



#* Subsection 18.9: Compare the INLA models----

#' Compare DIC and WAIC.
DICs <- c(I1$dic$dic, I2a$dic$dic, I2b$dic$dic)
WAICs <- c(I1$waic$waic, I2a$waic$waic, I2b$waic$waic)
Results <- data.frame(Models = c("NB GLM",
                                 "NB GLM + SRF with mesh2a",
                                 "NB GLM + SRF with mesh2b"),
                      DIC = DICs,
                      WAIC = WAICs)
Results

#' Conclusions:
#'  - Adding spatial dependency improves the models.
#'  - Mesh2a is better than mesh2b, but the difference is minimal.

#' We will use mesh2a in the remaining part of this exercise.

summary(I2a)

# Section 19: Imposed spatial dependency----

#' Here is some code to extract the posterior mean values of the
#' spatial parameters.
SpatialParams <- MySpatialParams(Model = I2a,
                                 ThisSpde = spde2a)

Kappa   <- SpatialParams[1]  #' Parameter kappa for the Mattern correlation function
Sigma.u <- SpatialParams[2]  #' Sigma of the u expressed in metres.
Range   <- SpatialParams[3]  #' Range.

#' This is the important part:
Sigma.u   #' u_i ~ N(0, 1.1238^2 * Spatial correlation)
Range     #' Distance at which the correlation diminishes.
          #' That is a large value!


#' Visualise the correlation structure.
#' First we obtain the locations of each point of the mesh.
LocMesh <- mesh2a$loc[,1:2]

#' And then we calculate the distance between each vertex.
D <- as.matrix(dist(LocMesh))

#' Using the estimated parameters from the model (see above)
#' we can calculate the imposed Matern correlation values.
d.vec <- seq(0, max(D), length = 100)
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1)
Cor.M[1] <- 1

#' Which we plot here:
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec,
     y = Cor.M,
     pch = 16,
     type = "l",
     cex.lab = 1.5,
     xlab = "Distance (km)",
     ylab = "Correlation",
     xlim = c(0, 200))
abline(h = 0.8, lty = 2, col = 2)
abline(h = 0.5, lty = 2, col = 2)
abline(h = 0.1, lty = 2, col = 2)

#' Define strong correlation as correlation between 1 - 0.8.
#' Define moderate correlation as correlation between 0.8 - 0.5.
#' Define weak correlation as correlation between 0.5 - 0.1.
#' Define diminishing correlation as correlation smaller than 0.1.

#' In this case, we have:
#'  - Strong correlation between sites separated between 0 - 15 km.
#'  - Moderate correlation between sites separated between 15 - 25 km.
#'  - Weak correlation between sites separated between 25 - 40 km.
#'  - Diminishing correlation between sites separated by more than 40 km



# Section 20: Plotting the spatial dependency----

#' Next we present the spatial component, the wks.
#' Their posterior mean values can be obtained via
w.pm <- I2a$summary.random$w$mean
length(w.pm)


#' This is a vector of length 1674 by 1. Each value in w.pm belongs
#' to a specific vertex on mesh 1. It is tempting to plot these,
#' but that won't work as geom_raster() wants to have data on a
#' regular grid. INLA has nice tools to predict the w.pm values on a
#' grid.

#' More recent INLA versions use fm_evaluator() and fm_evaluate().
#' We first need to call fm_evaluator() followed by a call to
#' fm_evaluate(). See below.
#'  - The dims = c(400, 400) specifies a 400-by-400 grid within
#'    the ranges of the mesh.
#'  - Proj contains the A matrix for this 400-by-400 grid.
#'  - fm_evaluate() gives the values of the SRF on a regular grid
#'    defined by fm_evaluator().
Proj <- fm_evaluator(mesh = mesh2a,
                     dims = c(400, 400))
#' Proj$x contains the 400 values along the x-axis.
#' Proj$y contains the 400 values along the y-axis.
#' Proj$proj$A contains the projector matrix.

SRF.Proj <- fm_evaluate(projector = Proj,
                        field  = w.pm)
#' SRF.Proj contains SRF values on the grid that was just defined.

#' Extract the relevant information so that we can use ggplot
#' code from previous exercises to plot the results
MyData      <- expand.grid(Xkm = Proj$x, Ykm = Proj$y)
MyData$w.pm <- as.vector(SRF.Proj)



#' We will now plot SRF, but we want to omit those parts
#' of the SRF that are outside the study area (i.e. in the sea).
#' To do this, we carry out the following steps.
#'  1. Convert MyData to a sf object.
#'  2. Determine which points in inside the study. Use st_contains() for this.
#'  3. Set the SRF values of the sites outside the study area to NA.


#' 1. Convert MyData to a sf object.
MyData.sp <- st_as_sf(MyData,
                      coords = c("Xkm", "Ykm"),
                      crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km  +datum=WGS84")

#' Determine which points in MyData are contained within the study area
Contained <- st_contains(CroppedLithuania_UTM, MyData.sp)

#' Extract indices of the points that are on land.
contained_indices <- unlist(Contained)

#' Create a logical vector where TRUE indicates the point is on La Palma
OnLand <- rep(FALSE, nrow(MyData))
OnLand[contained_indices] <- TRUE
OnLand

#' 3. Set the values of the SRF that are not on land to NA.
MyData$w.pm[!OnLand]  <- NA


#' Plot the result (in UTM).
ggplot() +
  geom_sf(data = st_sf(SeaPolygon_UTM),
          alpha = 0.5,
          fill = "lightblue") + # Sea
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          color = "black") + # Land
  geom_raster(data = MyData,
              aes(x = Xkm,
                  y = Ykm,
                  fill = w.pm)) +
  scale_fill_gradient2(name = "Spatial smoother",
                       midpoint = 0,
                       low = "blue",
                       mid = "white",
                       high = "red",
                       na.value = NA) +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = df,
             aes(x = Xkm, y = Ykm),
             size = 0.5,
             alpha = 0.5)

#' This is quite clear spatial correlation.

# Section 20.1: Model validation for the INLA NB GLM I2.mesh1----

#* Subsection 20.1.1: Getting scaled quantile residuals----

#' We would like to simulate 1000 data sets, because:
#'  1. We can use these to assess whether the model is overdispersed.
#'  2. In case of zero-inflation, we can use the 1000 simulated data
#'     sets to assess whether the model can cope with the zero
#'     inflation.
#'  3. We can use them to obtain scaled quantile residuals.


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, I2a)


#' Extract the 1000 sets of regression parameters
MyVar <- rownames(I2a$summary.fixed)
MyVar    #' Names of the regression parameters
Betas1000 <- inla.posterior.sample.eval(MyVar,
                                        SimData)
Betas1000[,1] #' First set of simulated betas
Betas1000[,2] #' Second set of simulated betas
Betas1000[,3] #' Third set of simulated betas


#' Now the spatial correlation random effects.
#' Determine the names of the random effects.
#' In the code below, we are using:  ^w:\d+$
#' This will only match strings that start with "w:", followed
#' exclusively by one or more digits, and then end.
names.SimData <- attributes(SimData[[1]]$latent)$dimnames[[1]]
names.SimData
wNames <- names.SimData[grep("^w:\\d+$", names.SimData)]

#' This is probably easier: wNames <- paste("w",1:mesh3$n, sep = ":")
#' Determine on which rows the random effects are.
MyID  <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
RowsW <- lapply(wNames, MyID)
RowsW <- as.numeric(RowsW)
RowsW
NROW(RowsW)
mesh2a$n


#' Calculate 1000 times the fitted values via: mu = exp(X * beta + A3 * w).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type, data = df)
X <- as.matrix(X)


#' Get the theta from the selected NB GLM:
theta.pd <- I2a$marginals.hyperpar$`size for the nbinomial observations (1/overdispersion)`
theta.pm <- inla.emarginal(function(x) x, theta.pd)
theta.pm


#' Start a loop. In each iteration:
#'   1. Calculate the fitted values mu.
#'   2. Simulate NB data with the mean equal to mu.
N    <- nrow(df)                        #' Sample size
Ysim <- matrix(nrow = N, ncol = 1000)   #' Create space

#' Start the loop (can be done more efficient with lapply).
for (i in 1:1000){
  Betas <- Betas1000[,i]
  wk    <- SimData[[i]]$latent[RowsW]
  FixedPart   <- X %*% Betas
  SpatialPart <- A2a %*% wk    #' Note the A2a. This is based on mesh2a.
  mu  <- exp(FixedPart + SpatialPart)[,1]
  Ysim[,i] <- rnegbin(n = N,                   #' Simulated NB Owl count data
                      mu = mu,
                      theta = theta.pm)
}

#' We now have 1000 simulated data sets from the model.
Ysim[, 1] #' First simulated data set.
Ysim[, 2] #' Second simulated data set.
Ysim[, 3] #' Third simulated data set.
#' Etc.

#' Or:
par(mfrow = c(2,2))
hist(df$Counts, main ="Observed NEggs")
hist(Ysim[,1], main = "First simulated data set")
hist(Ysim[,2], main = "Second simulated data set")
hist(Ysim[,3], main = "Third simulated data set")
par(mfrow = c(1,1))
#' No idea whether the observed data is similar from the simulated data.



#' Now we have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
N <- nrow(df)
df$Fit2 <- I2a$summary.fitted.values[1:N, "mean"]  #' Fitted values.

E4.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$Counts,
                       fittedPredictedResponse = df$Fit2,
                       integerResponse = TRUE)
#' Now we have scaled quantile residuals.



#* Subsection 20.1.2: Check for overdispersion----

#' We will use the scaled quantile residuals to assess for
#' overdispersion.
par(mfrow = c(1,1), mar = c(5,5,2,2))
testDispersion(E4.sqr)
#' Fine.



#* Subsection 20.1.3: Check for homogeneity of variance----

#' Plot the scaled quantile residuals versus (ranked) fitted values.
plotResiduals(E4.sqr, quantreg = TRUE, smoothScatter = FALSE)
#' Some trouble!



#* Subsection 20.1.4: Check for uniformity of the residuals----

#' In DHARMa, we verify whether the scaled quantile residuals are
#' uniform distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(E4.sqr, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)
#' OK!



#* Subsection 20.1.5: Plot residuals versus the covariates----

#' Plot the scaled quantile residuals versus each covariate
#' in the model.
plotResiduals(E4.sqr, form = df$eqr)            #' okay
plotResiduals(E4.sqr, form = df$ppt)            #' Fine
plotResiduals(E4.sqr, form = df$tmin)           #' Fine
plotResiduals(E4.sqr, form = df$ws)             #' Fine
plotResiduals(E4.sqr, form = df$elevation)      #' Okay
plotResiduals(E4.sqr, form = df$agriculture)    #' Fine
plotResiduals(E4.sqr, form = df$artificial)     #' Fine
plotResiduals(E4.sqr, form = df$natural)        #' Fine
plotResiduals(E4.sqr, form = df$year)           #' Some trouble.
plotResiduals(E4.sqr, form = df$year, asFactor = TRUE)           #' Some trouble.
plotResiduals(E4.sqr, form = df$waterbody_type, asFactor = TRUE) #' Some trouble.



#* Subsection 20.1.6: Check for zero inflation----

testZeroInflation(E4.sqr)
#' OK.



#* Subsection 20.1.7: Check for spatial dependency----

#' Option 1: Plot the residuals vs spatial locations. Look for patterns.
#' Option 2: Apply Moran's I test on the residuals.
#' Option 3: Make a variogram of the residuals


#' Option 1:  Plot the residuals vs spatial locations
#' Use scaled quantile residuals:
df$E2 <- residuals(E4.sqr) # <-- better if you have zero inflation

#' #' Or use Pearson residuals.
#' df$E2 <- (df$Counts - df$Fit2) / sqrt(df$Fit2 + df$Fit2^2 / theta.pm)

#' Note that scaled quantile residuals are around 0.5, whereas
#' Pearson residuals are around 0.
#' Define colour and point size.
df$MyCol  <- ifelse(df$E2 >= 0.5, "red", "blue") # <- if you use pearson, then it should be 0, not 0.5
df$MySize <- rescale(abs(df$E2), to = c(0, 3))

p <- ggplot() +
  geom_point(data = df,
             aes(x = Xkm,
                 y = Ykm,
                 col = MyCol,
                 size = MySize),
             alpha = 0.5)  +
  scale_color_identity() +
  scale_size_continuous(range = c(1, 3)) +
  theme_minimal() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Residuals")
p + facet_grid(~waterbody_type)
#' Spatial spatial patterns?



#' Option 2: Apply Moran's I test on the residuals
#' DHARMa can test for spatial correlation using Moran's I
#' test.
# testSpatialAutocorrelation(df$E2, #E2.sqr,
#                            x = df$Xkm,
#                            y = df$Ykm,
#                            plot = FALSE)

# test spatial autocorrelation
# groupLocations = aggregate(df[, c("Xkm", "Ykm")], list(df$site_id), mean)
res_space = recalculateResiduals(E4.sqr, group = df$site_id)
testSpatialAutocorrelation(res_space, groupLocations$Xkm, groupLocations$Ykm)
#' No spatial autocorrelation.

#' Option 3: Make a variogram.
#' Use scaled quantile residuals:
df$E2 <- residuals(E4.sqr)
#' Make a sample variogram of the residuals.
MyData <- data.frame(E2  = df$E2,
                     Xkm = df$Xkm,
                     Ykm = df$Ykm)

#' Convert to sf object.
MyData_sf <- st_as_sf(x = MyData,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Apply the variogram function from gstat.
V2 <- variogram(E2 ~ 1,
                data = MyData_sf,
                # cutoff = 150,
                cressie = TRUE)

#' Plot the variogram
p1 <- ggplot(data = V2, aes(x = dist, y = gamma)) +
         geom_point() +
         geom_smooth(se = FALSE) +
         labs(x = "Distance (in km)", y = "Semi-variogram") +
         theme(text = element_text(size=15),
               legend.position="none")
p1
#' Conclusion:
#'   -That is a strange pattern

#' Let us compare this with the variogram obtained from the
#' scaled quantile residuals of the model without spatial dependency.
#' Read the variogram results from the previous exercise.
p <- ggplot() +
  geom_point(data = V1,
             aes(x = dist, y = gamma),
             col = "red") +
  geom_point(data = V2,
             aes(x = dist, y = gamma),
             col = "blue") +
  geom_smooth(data = V1,
              aes(x = dist, y = gamma),
              col = "red",
              se = TRUE,
              span = 0.9) +
  geom_smooth(data = V2,
              aes(x = dist, y = gamma),
              col = "blue",
              se = TRUE,
              span = 0.9) +
    labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size=15),
        legend.position="none")
p
#' You need to see the results in perspective!
#' Blue is the spatial Negative binomial GLM.
#' Red is the Negative binomial GLM.

# blue is based on the residuals of all years combined.
# need to split the variogram to split the variogram for each year - check candadian owl excercise.
# Next is to do spatial temporal with replicate or AR1

#* Subsection 20.1.8: Check for temporal dependency----
# test temporal autocorrelation
res_time = recalculateResiduals(E4.sqr, group = df$year)
testTemporalAutocorrelation(res_time, time = unique(df$year))
# Fine



# Section 21: NB GLM with replicate spatial-temporal term----


#* Subsection 21.1: Define blocking structure----

#' Define a vector that identifies the blocking structure.
df$YearNum <- as.numeric(as.factor(df$year))
Repl <- df$YearNum
Repl
#' This is a vector with values 1 1 1 2 2 2 3 3 3 3... etc.
#' All observations in the same block will be spatially correlated.
#' The correlation itself can differ per block, but all blocks
#' share the same range and sigma_u.


#' How many blocks do we have? Should be 10.
NRepl <- length(unique(Repl)) #' Number of groups.
NRepl



#* Subsection 21.2: Define the projector matrix A----

#' Define the projector matrix. We can use
#' mesh2a for this. No need to change the mesh.
A3Repl <- inla.spde.make.A(mesh2a,
                           loc = Loc,
                           repl = Repl)  #' 18 blocks.
dim(A3Repl)
#' The rows refer to the sites.
#' The columns are weighting factors.
#' Weighting factors 10 blocks.
#' recall: u = A * w



#* Subsection 21.3: Define spde----

#' Define the SPDE. We use the same priors for the range and sigma_u
#' as in the previous section.
spde3Repl <- inla.spde2.pcmatern(mesh2a,
                                 prior.range = c(75, 0.05),
                                 prior.sigma = c(2, 0.05))



#* Subsection 21.4: Define SRF w----

#' Define the SRF
w3Repl <- inla.spde.make.index('w',
                               n.spde = mesh2a$n,
                               n.repl = NRepl)



#* Subsection 21.5: Stack for the replicate spatial-temporal GLM----

#' Make the X matrix using model.matrix()
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type, data = df)
X <- as.data.frame(X) #' Avoids an error message in INLA
colnames(X)


#' This is not relevant for this specific model:
#' In INLA, you can't use ':' in the variable names. Note that the
#' interaction term uses : in its name. Replace the : by '_'.
#OldColnames  <- colnames(X)
#NewColNames  <- gsub(pattern = ":", replacement = "_", x = OldColnames)
#colnames(X)  <- NewColNames
#head(X)


#' We now define the stack. This is INLA's way to combine data that
#' has been sampled at different spatial resolutions. .

#' Sample size
N <- nrow(df)

Stack3SpatTemp <- inla.stack(
  tag = "Fit",                       #' Name of stack
  data = list(Counts = df$Counts), #' Response variable
  A = list(1, 1, A3Repl),            #' The order matches the order in effects.
  effects = list(
    Intercept = rep(1, N),           #' Use our own intercept
    X         = X[,-1],              #' Dump the intercept from the model.matrix
    w         = w3Repl))             #' Spatial random field

#' This is the stack for the NB GLM with replicate spatial-temporal
#' correlation.



#* Subsection 21.6: Specify the model formula----

#' Specify the model formula in terms of the response variable,
#' covariates and the spatial correlated term. Having the colnames
#' is handy at this stage:
colnames(X)


#' This is the model with spatial dependency, based on mesh 2a.
f3 <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                     agriculture.std + artificial.std + natural.std + 
                     year.std + waterbody_type.L + 
                 f(w, model = spde3Repl, replicate = w.repl) # <---  w.repl is correct, should not be w3Repl



#* Subsection 21.7: Execute the replicate spatial-temporal GLM----

#' This is the NB GLM with replicate spatial dependency.
I3 <- inla(f3,
           family = "nbinomial",
           data = inla.stack.data(Stack3SpatTemp),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(Stack3SpatTemp)))



#* Subsection 21.8: Compare the models----

#' Compare DIC and WAIC.
DICs <- c(I1$dic$dic, I2a$dic$dic, I2b$dic$dic, I3$dic$dic)
WAICs <- c(I1$waic$waic, I2a$waic$waic, I2b$waic$waic, I3$waic$waic)
Results <- data.frame(Models = c("NB GLM",
                                 "NB GLM + SRF with mesh2a",
                                 "NB GLM + SRF with mesh2b",
                                 "NB GLM + replicate SRF with mesh2a"),
                      DIC = DICs,
                      WAIC = WAICs)
Results

#' Conclusion:
#'  - Adding replicate spatial-temporal dependency does not
#'    improve the model.





#* Subsection 21.9: Present the replicate SRF----

#' Although the model is not better, we do show its results
#' as coding is non-trivial.

#' The posterior mean values of the replicate SRF can be obtained
#' via:
w.pm <- I3$summary.random$w$mean
length(w.pm)

#' For each year we have a set of w's. Hence, we can make 18 pictures;
#' one for each year. We want to have 18 panels in 1 ggplot graph.
#' Each panel should show the SRF for that specific year.

#' First we need the levels of fYear.
NamesYear <- levels(as.factor(df$year))
NamesYear

#' Create an object in which we can store the 10 sets of w.pm on a
#' 100-by-100 grid.
MyData.All <- NULL

#' Start a loop.
#'  1. Extract the SRF (i.e. the w's) for year i.
#'  2. Project the SRF on a 100-by-100 grid for that year.
#'  3. Store the results in MyData$w.pm.
#'  4. Determine which of the 100 * 100 rows in MyData
#'     are not on land. Set these MyData$w.pm to NA for plotting
#'     purposes.
#'  5. Store the MyData$w.pm in MyData.All

#' We are using a 100-by-100 grid to speed up the ggplot2 plotting.

for (i in 1:NRepl){
  #' 1. Extract the SRF for year i.
  wpm.i <- w.pm[w3Repl$w.repl == i]
  #wsd.i <- wsd[wRepl$w.repl == i]

  #' 2. Project the SRF on a 100-by-100 grid for year i.
  Proj     <- fm_evaluator(mesh = mesh2a, dims = c(100, 100))
  SRF.Proj <- fm_evaluate(projector = Proj,  field  = wpm.i)

  #' 3. Store the results in MyData$w.pm.
  MyData      <- expand.grid(Xkm = Proj$x, Ykm = Proj$y)
  MyData$w.pm <- as.vector(SRF.Proj)

  #'  4. Determine which of the 100 * 100 rows in MyData
  #'     are not on land. Set these MyData$w.pm to NA for plotting
  #'     purposes.

  #'     Convert MyData to a sf object.
  MyData.sp <- st_as_sf(MyData,
                        coords = c("Xkm", "Ykm"),
                        crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km  +datum=WGS84")

  #'     Determine which points in MyData are contained within the
  #'     study area
  Contained <- st_contains(CroppedLithuania_UTM, MyData.sp)

  #'     Extract indices of the points that are on land.
  contained_indices <- unlist(Contained)

  #'     Create a logical vector where TRUE indicates the point is
  #'     on land.
  OnLand <- rep(FALSE, nrow(MyData))
  OnLand[contained_indices] <- TRUE

  #'     Not on land: set to NA
  MyData$w.pm[!OnLand] <- NA


  #'  5. Store the MyData$w.pm in MyData.All
  MyData$Year <- NamesYear[i]
  MyData.All  <- rbind(MyData.All,MyData)
}

#' Convert Year to a factor for facetting in ggplot.
MyData.All$fYear <- factor(MyData.All$Year)


#' And some ggplot2 coding to plot the results.
ggplot() +
  geom_sf(data = st_sf(SeaPolygon_UTM),
          alpha = 0.5,
          fill = "lightblue") + # Sea
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          color = "black") + # Land
  geom_raster(data = MyData.All,
              aes(x = Xkm,
                  y = Ykm,
                  fill = w.pm)) +
  scale_fill_gradient2(name = "Spatial smoother",
                       midpoint = 0,
                       low = "blue",
                       mid = "white",
                       high = "red",
                       na.value = NA) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = "Study area") +
  geom_point(data = df,
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             alpha = 0.5) +
  facet_wrap(~ fYear, ncol = 5)
#' From year to year, the spatial random field changes. Not much in
#' this example, but it does change.



#' #' If you want to see the changes more clearly, then
#' #' use the animation package. The code below was partly
#' #' written by Dr. Christoph Kopp
#' 
#' #' Ignore the warning on nmax.
#' saveHTML({
#'   ani.options(interval = 1, nmax = 50, use.dev = FALSE)
#'   par(mar = c(3, 3, 2, 0.5),
#'       mgp = c(2, 0.5, 0),
#'       tcl = -0.3,
#'       cex.axis = 0.8,
#'       cex.lab = 0.8,
#'       cex.main = 1)
#' 
#'   #' Grab the names of the years.
#'   AllYear <- levels(MyData.All$fYear)
#' 
#'   #' Run a loop in which for each year k, we plot the
#'   #' w.pm values of that year.
#'   for(k in AllYear){
#'     #' Grab the SRF for year k.
#'     MyDataYeari <- subset(MyData.All, fYear == k)
#' 
#'     #' Plot it.
#'     pi <- ggplot() +
#'            geom_sf(data = st_sf(SeaPolygon_UTM),
#'                    alpha = 0.5,
#'                    fill = "lightblue") + # Sea
#'            geom_sf(data = CroppedCanada_UTM,
#'                    fill = "transparent",
#'                    color = "black") + # Land
#'            geom_raster(data = MyDataYeari,
#'                        aes(x = Xkm,
#'                            y = Ykm,
#'                            fill = w.pm)) +
#'            scale_fill_gradient2(name = "Spatial smoother",
#'                                 midpoint = 0,
#'                                 low = "blue",
#'                                 mid = "white",
#'                                 high = "red",
#'                                 limits = c(-1.3, 1.6), #' <-- ADJUST FOR YOUR DATA
#'                                 na.value = NA) +
#'            theme_minimal() +
#'            theme(axis.text.x = element_blank(),
#'                  axis.text.y = element_blank()) +
#'            labs(title = k) +
#'            geom_point(data = Owls,
#'                       aes(x = Xkm, y = Ykm),
#'                       size = 0.1,
#'                       alpha = 0.5)
#'     print(pi)
#'   }
#' }, img.name = "inla-plot",
#'    title = "INLA-simulation",
#' description = c("Spatial-temporal replicate field"))
#' 
#' #' A browser should now pop up showing the 18 SRFs in sequence.
#' #' You can control the speed.




# Section 22: When and where are the changes?----

#' Do you want to know where in space there are important
#' year-to-year changes, and in which years?
#' To figure this out, you can carry out the following steps.

#' 1. Define a MyData with 200-by-200 spatial grid points. Calculate
#'    the expected counts for each grid point in each year using the
#'    replicate model.
#' 2. Copy MyData to TempData, and increase Year with 1.
#' 3. Using TempData, predict expected counts for each grid point in each year.
#' 4. Calculate the differences between the predicted values in the two
#'    data frames.
#' 5. Use posterior simulation to get 95% CIs.
#' 6. Visualise the results.

#' Not too difficult to code!






# Section 23: AR1 NB GLM----

#' We will now allow for spatial correlation that changes over time
#' according to an auto-regressive process.

#' u_ij = phi * u_{i,j-1} + noise_ij
#' j is the year subscript.




#* Subsection 23.1: Define blocking structure----

#' The ar1 coding is very similar to that of the replicate
#' correlation. First we define a blocking structure.
#' All observations in the same block are spatially correlated.
#' From block to block the changes over time follow the ar1 structure.

#' Blocking structure, not necessarily ordered.
df$YearSeq <- as.numeric(as.factor(df$year))
df$YearSeq

#' We have 10 blocks
NYears <- length(unique(df$YearSeq))
NYears


#' If you run the GLM with spatial-temporal ar1 correlation,
#' then computing time will be about 5 - 10 minutes on a fast
#' computer. That is too long for a course exercise. We will
#' therefore use a little trick. The same trick becomes a
#' necessity for irregular spaced time series data.

#' We will define time knots. And at each knot, we will
#' calculate a spatial random field. From knot to knot,
#' we will impose the ar1 correlation. As an example:

Knots <- seq(1, 10, by = 3)   #BUT THIS IS WRONG! See END OF FILE.
# should be Knots <- seq(1, 18, by = 3)   #BUT THIS IS WRONG! See END OF FILE.
Knots

#' The model will now calculate a spatial random field for year = 1,
#' year = 4, year = 7, and year 10.

#' To get the spatial random effects u_ij in year 2, the model will take
#' a weighted average of the SRFs of knots 1 and 4.
#' The weights are given by the time difference (year 2 is closer to
#' the first knot, so its weighting factor is larger that of the second
#' knot).
#'
#' Knots: 1        4        7        10
#'        1  2  3  4  5  6  7  8  9  10

#' u_ij in year 3 =  0.66 * SRF-at-knot-2  +  0.33 * SRF-at-knot-5


#' This is the mesh for the knots.
mesh.t <- inla.mesh.1d(Knots, degree = 1)

#' If you want to see the weighting factors, here they are:
Atest <- inla.spde.make.A(mesh = mesh.t,
                          loc = df$YearSeq)
Atest

#' Summarising: The random effects u_ij in years between the knots
#'              are weighted averages of the two SRF at the
#'              nearest 2 knots.



#' The total numer of knots (and sptial random fields):
NGroups <- length(Knots)
NGroups


#' If you use:
#' Knots <- seq(1, 10, by = 3)
#' Then you will get a SRF in each year. Takes about 15 minutes on
#' a fast computer.


#* Subsection 23.2: Define projector matrix A----

#' Define the projector matrix. We can use
#' mesh2a for this. No need to change the mesh.
A4.ar1 <- inla.spde.make.A(mesh2a,
                           loc = Loc,
                           group = df$YearSeq,
                           n.group = NGroups,  #' <--- number of knots
                           group.mesh = mesh.t)
dim(A4.ar1) #' As before, 2089 observations, and 10 * mesh2a$n nodes.





#* Subsection 23.3: Define spde----

#' Define the SPDE. We use the same priors for the range and sigma_u
#' as in the previous section.
spde4ar1 <- inla.spde2.pcmatern(mesh2a,
                                prior.range = c(75, 0.05),
                                prior.sigma = c(2, 0.05))



#* Subsection 23.4: Define SRF w----

#' Define the SRF
#' We now need to define the number of groups.
w4ar1 <- inla.spde.make.index('w',
                               n.spde  = spde4ar1$n.spde,
                               n.group = NGroups)    #' <- Number of knots



#* Subsection 23.5: Stack for the replicate GAM----

#' Sample size
N <- nrow(df)


#' Define the stack.
Stack4.ar1 <- inla.stack(
  tag = "Fit",                       #' Name of stack
  data = list(Counts = df$Counts),   #' Response variable
  A = list(1, 1, A4.ar1),            #' The order matches the order in effects.
  effects = list(
    Intercept = rep(1, N),           #' Use our own intercept
    X         = X[,-1],              #' Dump the intercept from the model.matrix
    w         = w4ar1))              #' Spatial random field

#* Subsection 23.6: Specify the model formula----

#' This is the model with spatial-temporal auto-regression
#' correlation, based on mesh2a.
f4 <-  Counts ~ -1 + Intercept + eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                     agriculture.std + artificial.std + natural.std + 
                     year.std + waterbody_type.L + 
                     f(w,
                       model = spde4ar1,
                       group = w.group,
                       control.group = list(model='ar1'))



#* Subsection 23.7: Execute the ar1 spatial-temporal GLM----

#' This is the NB GLM with ar1 spatial-temporal dependency.
#' We are going to use some numerical approximations for faster
#' calculations.
#' If you use this, then it is an option to use it for all earlier
#' models as well.
#

#' Computing time is about 5 minutes. You can also load our workspace;
#' then you don't have to execute the following model. See the course
#' website, or the code in Subsection 2.1.
I4 <- inla(f4,
           family = "nbinomial",
           data = inla.stack.data(Stack4.ar1),
           control.compute = MyControlCompute,
           control.predictor = list(A = inla.stack.A(Stack4.ar1)),
  
  
           #' Faster calculation:
           control.inla = list(strategy = 'gaussian',
                               int.strategy = 'eb')) # <--- remove these lines for more precision

# comment that faster calculation part out if we want a more precise model.

summary(I4)
#' Note that the rho is close to 1. That is a very high value,
#' and indicates very slow changes over time. But we already
#' knew that. This comes from "GroupRho for w" in the hyperparameters part





#* Subsection 23.8: Compare the INLA models----

#' Compare DIC and WAIC.
DICs <- c(I1$dic$dic, I2a$dic$dic, I2b$dic$dic, I3$dic$dic,
          I4$dic$dic)
WAICs <- c(I1$waic$waic, I2a$waic$waic, I2b$waic$waic, I3$waic$waic,
           I4$waic$waic)
Results <- data.frame(Models = c("NB GLM",
                                 "NB GLM + SRF with mesh2a",
                                 "NB GLM + SRF with mesh2b",
                                 "NB GLM + replicate SRF",
                                 "NB GLM + ar1 SRF"),
                      DIC = DICs,
                      WAIC = WAICs)
Results
#' The ar1 is the best!



#* Subsection 23.9: Results ar1 spatial-temporal GLM----


#' Numerical output for the regression parameters.
BetasI4 <- I4$summary.fixed[, c("mean", "0.025quant", "0.975quant")]
print(BetasI4, digits = 2)


#' Posterior mean of theta (as in: mu + mu^2 / theta)
theta.pd <- I4$marginals.hyperpar$`size for the nbinomial observations (1/overdispersion)`
theta.pm <- inla.emarginal(function(x) x, theta.pd)
theta.pm


#' Visualise the betas using ggplot2.
#' 1. Create a data frame.
BetasI4_df <- data.frame(Covariate = rownames(BetasI4),
                         Mean      = BetasI4[,"mean"],
                         Lower     = BetasI4[,"0.025quant"],
                         Upper     = BetasI4[, "0.975quant"])

#' 2. Explicitly set the order of the covariates to match their original order
BetasI4_df$Covariate <- factor(BetasI4_df$Covariate, levels = rownames(BetasI4))

#' 3. Plot the info in BetasI1_df.
ggplot(BetasI4_df, aes(x = Mean, y = Covariate)) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper),
                  fatten = 1.5,
                  color = "blue") +
    geom_vline(aes(xintercept = 0),
             linetype = "dotted",
             col = "red") +
  geom_vline(aes(xintercept = 0),
             linetype = "dotted",
             col = "red") +
  labs(title = "Posterior means and 95% Credible Intervals",
       x = "Value",
       y = "Covariate") +
  theme_minimal()
#' If a 95% CI crosses the dotted red line, then the level for
#' that year is not different from 2002.



#' Next we present the spatial-temporal component. This code is
#' copy-paste from the replicate model.

#' We want to have 18 panels in 1 ggplot graph. Each panel should
#' show the SRF for that specific year.

#' The posterior mean values can be obtained via:
w.pm <- I4$summary.random$w$mean
length(w.pm)


#' Start a loop.
#'  1. Extract the SRF for year i.
#'  2. Project the SRF on a 100-by-100 grid for that year.
#'  3. Store the results in MyData$w.pm.
#'  4. Determine which of the 100 * 100 rows in MyData
#'     are not on land. Set these MyData$w.pm to NA for plotting
#'     purposes.
#'  5. Store the MyData$w.pm in MyData.All

#' These are the years.
NamesYear  <- levels(as.factor(df$year))
NamesKnots <- Knots


#' Create an object in which we can store the w.pm on a
#' 100-by-100 grid.
MyData.All <- NULL

#' Start a loop to extract the SRF for each knot.
MyData.All <- NULL
for (i in 1:NGroups){
  #' 1. Extract the SRF for year i.
  wpm.i <- w.pm[w4ar1$w.group == i]
  #wsd.i <- wsd[wRepl$w.group == i]

  #' 2. Project the SRF on a 100-by-100 grid for year i.
  Proj     <- fm_evaluator(mesh = mesh2a, dims = c(100, 100))
  SRF.Proj <- fm_evaluate(projector = Proj,  field  = wpm.i)

  #'  3. Store the results in MyData$w.pm.
  MyData      <- expand.grid(Xkm = Proj$x, Ykm = Proj$y)
  MyData$w.pm <- as.vector(SRF.Proj)

  #'  4. Determine which of the 100 * 100 rows in MyData
  #'     are not on land. Set these MyData$w.pm to NA for plotting
  #'     purposes.
  MyData.sp <- st_as_sf(MyData,
                        coords = c("Xkm", "Ykm"),
                        crs = "+proj=utm +zone=34 +north +ellps=WGS84 +units=km  +datum=WGS84")

  #' Determine which points in MyData are contained within the study area
  Contained <- st_contains(CroppedLithuania_UTM, MyData.sp)

  #' Extract indices of the points that are on land.
  contained_indices <- unlist(Contained)

  #' Create a logical vector where TRUE indicates the point is on La Palma
  OnLand <- rep(FALSE, nrow(MyData))
  OnLand[contained_indices] <- TRUE

  #' Not on land: set to NA
  MyData$w.pm[!OnLand] <- NA

  #'  5. Store the MyData$w.pm in MyData.All
  #MyData$Year <- NamesYear[i]
  MyData$Knots <- NamesKnots[i]
  MyData.All  <- rbind(MyData.All,
                       MyData)
}

#' Add year as a factor for facetting in ggplot.
MyData.All$fKnots <- factor(MyData.All$Knots)



#' And plot everything in one ggplot.
ggplot() +
  geom_sf(data = st_sf(SeaPolygon_UTM),
          alpha = 0.5,
          fill = "lightblue") + # Sea
  geom_sf(data = CroppedLithuania_UTM,
          fill = "transparent",
          color = "black") + # Land
  geom_raster(data = MyData.All,
              aes(x = Xkm,
                  y = Ykm,
                  fill = w.pm)) +
  scale_fill_gradient2(name = "Spatial smoother",
                       midpoint = 0,
                       low = "blue",
                       mid = "white",
                       high = "red",
                       na.value = NA) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = "Study area") +
  geom_point(data = df,
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             alpha = 0.5) +
  facet_wrap(~ fKnots, ncol = 3)

#' From year-to-year the SRF is changing slowly.
#' That is because the rho is relativel high.
summary(I4)




#' #' If you want to see the changes more clearly, then
#' #' use the animation package. Code below was partly
#' #' written by Dr. Christoph Kopp
#' #' These are the years and the knots.
#' AllYears  <- levels(MyData.All$fYear)
#' NamesKnots <- Knots
#' 
#' 
#' #' Ignore the warning on nmax.
#' saveHTML({
#'   ani.options(interval = 1, nmax = 50, use.dev = FALSE)
#'   par(mar = c(3, 3, 2, 0.5),
#'       mgp = c(2, 0.5, 0),
#'       tcl = -0.3,
#'       cex.axis = 0.8,
#'       cex.lab = 0.8,
#'       cex.main = 1)
#' 
#' 
#'   #' Start a loop to plot the SRF for each year.
#'   for(k in 1:NGroups){
#'     #' Grab the results from year (or knot) i.
#'     MyDataKnoti <- subset(MyData.All, fKnots == NamesKnots[k])
#' 
#'     #' Plot results for year i
#'     pi <- ggplot() +
#'       geom_sf(data = st_sf(SeaPolygon_UTM),
#'               alpha = 0.5,
#'               fill = "lightblue") + # Sea
#'       geom_sf(data = CroppedCanada_UTM,
#'               fill = "transparent",
#'               color = "black") + # Land
#'       geom_raster(data = MyDataKnoti,
#'                   aes(x = Xkm,
#'                       y = Ykm,
#'                       fill = w.pm)) +
#'       scale_fill_gradient2(name = "Spatial smoother",
#'                            midpoint = 0,
#'                            low = "blue",
#'                            mid = "white",
#'                            high = "red",
#'                            lim = c(-1.8, 1.9),   #' <---
#'                            na.value = NA) +
#'       theme_minimal() +
#'       theme(axis.text.x = element_blank(),
#'             axis.text.y = element_blank()) +
#'       labs(title = k)
#'     print(pi)
#'   }
#' }, img.name = "inla-plot",
#' title = "INLA-simulation",
#' description = c("Spatial-temporal replicate field"))




# Section 24: Model validation ar1 spatial-temporal GLM----


#* Subsection 24.1: Getting scaled quantile residuals----


#' Do the posterior simulation of regression parameters 1000 times.
SimData <- inla.posterior.sample(n = 1000, I4)


#' Here is some fancy code to grab the positions of specific variables.
#' Custom function.
MyGrep <- function(x, SimulatedData){
  # SimulatedData is the name of the object containing the simulation results
  # x is an element of BetasInModel
  names.SimData <- attributes(SimulatedData[[1]]$latent)$dimnames[[1]]
  names.SimData[grep(x, names.SimData)]
}


#' What are the names of the regression parameters?
BetasInModel <- rownames(I4$summary.fixed)

#' Get their location in the object with simulation results.
BetaNames    <- unlist(lapply(BetasInModel,
                              FUN = MyGrep,
                              SimulatedData = SimData))
MyID     <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
RowsBeta <- lapply(BetaNames, MyID)
RowsBeta <- as.numeric(RowsBeta)
RowsBeta



#' Now the spatial correlation random effects.
#' Determine the names of the random effects.
#' In the code below, we are using:  ^w:\d+$
#' This will only match strings that start with "w:", followed
#' exclusively by one or more digits, and then end.
names.SimData <- attributes(SimData[[1]]$latent)$dimnames[[1]]
names.SimData
wNames <- names.SimData[grep("^w:\\d+$", names.SimData)]

#' This is probably easier: wNames <- paste("w",1:mesh3$n, sep = ":")
#' Determine on which rows the random effects are.
RowsW <- lapply(wNames, MyID)
RowsW <- as.numeric(RowsW)
RowsW



#' Calculate 1000 times the fitted values via: mu = exp(X * beta + A * w).
#' Get the design matrix X.
X <- model.matrix(~ eqr.std + ppt.std + tmin.std + ws.std + elevation.std + 
                    agriculture.std + artificial.std + natural.std + 
                    year.std + waterbody_type, data = df)
X <- as.matrix(X)


#' Start a loop. In each iteration:
#'   1. Calculate the fitted values mu.
#'   2. Simulate NB data with the mean equal to mu.
N    <- nrow(df)                      #' Sample size.
Ysim <- matrix(nrow = N, ncol = 1000) #' Create space.

#' Start the loop (can be done more efficient with lapply).
for (i in 1:1000){
  Betas       <- SimData[[i]]$latent[RowsBeta]
  wk          <- SimData[[i]]$latent[RowsW]
  FixedPart   <- X %*% Betas
  SpatialPart <- A4.ar1 %*% wk            #' Note the A4.ar1.                         #' Will change later.
  mu  <- exp(FixedPart + SpatialPart)[,1]
  Ysim[,i] <- rnegbin(n = N,               #' Simulated count data
                      mu = mu,
                      theta = theta.pm)
}

par(mfrow = c(2,2))
hist(df$Counts, main ="Observed SR")
hist(Ysim[,1],  main = "First simulated data set")
hist(Ysim[,2],  main = "Second simulated data set")
hist(Ysim[,3],  main = "Third simulated data set")
par(mfrow = c(1,1))
#' The observed data seems to be slightly different?


#' Now we have 1000 simulated data sets. Give them all to
#' DHARMa, and it will calculate scaled quantile residuals.
N <- nrow(df)
df$Fit <- I4$summary.fitted.values[1:N, "mean"]  #' Fitted values.

E5.sqr <- createDHARMa(simulatedResponse = Ysim,
                       observedResponse = df$Counts,
                       fittedPredictedResponse = df$Fit,
                       integerResponse = TRUE)
#' Now we have scaled quantile residuals.



#* Subsection 24.2: Check for overdispersion and zero-inflation----

testDispersion(E5.sqr)
#' A bit of underdispersion?
#' Try the Poisson version of the model.

testZeroInflation(E5.sqr)
#' Fine

#* Subsection 24.3: Check for homogeneity of variance----

#' Plot the scaled quantile residuals versus (ranked) fitted values.
plotResiduals(E5.sqr, quantreg = TRUE, smoothScatter = FALSE)
#' Some trouble!



#* Subsection 24.4: Check for uniformity of the residuals----

#' In DHARMa, we verify whether the scaled quantile residuals are
#' uniform distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(E5.sqr, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)
#' OK!



#* Subsection 24.5: Plot residuals versus the covariates----

#' Plot the scaled quantile residuals versus each covariate
#' in the model.
plotResiduals(E5.sqr, form = df$eqr)            #' okay <- quantreg = T <- force it
plotResiduals(E5.sqr, form = df$ppt)            #' Fine
plotResiduals(E5.sqr, form = df$tmin)           #' Fine
plotResiduals(E5.sqr, form = df$ws)             #' Fine
plotResiduals(E5.sqr, form = df$elevation)      #' Okay
plotResiduals(E5.sqr, form = df$agriculture)    #' Fine
plotResiduals(E5.sqr, form = df$artificial)     #' Fine
plotResiduals(E5.sqr, form = df$natural)        #' Fine
plotResiduals(E5.sqr, form = df$year)           #' Some trouble.
plotResiduals(E5.sqr, form = df$year, asFactor = TRUE)           #' Some trouble.
plotResiduals(E5.sqr, form = df$waterbody_type, asFactor = TRUE) #' Some trouble.

E5.sqr$scaledResiduals
# actual residuals that we can use for the gam trick
# grab it, then make numeric, then do the rest.
# loop that runs the plot residuals for each covariate and extracted the residuals and the p-value from the test
# store everything under each other and then plot it via ggplot
# instead of 11 pictures, have everything in one ggplot and colour everything by p-value
# can present this as an online supplement but only if the referee asks / to anticipate what a reviewer will ask for



#* Subsection 24.6: Check spatial dependency----

#' Combine residuals and coordinates. We also added year to this
#' data frame. We will explain in a moment why.
Data4Vario <- data.frame(Res   = E5.sqr$scaledResiduals,
                         Xkm   = df$Xkm,
                         Ykm   = df$Ykm,
                         fYear = as.factor(df$year))

#' Convert to sf object.
MyData_sf <- st_as_sf(x = Data4Vario,
                      coords = c("Xkm", "Ykm"),
                      crs = NA)  #' Non-Cartesian coordinates.

#' Calculate the variogram.
V4 <- variogram(Res ~ 1,
                data = MyData_sf,
                cutoff = 100,
                cressie = TRUE)

#' Plot the variogram.
ggplot() +
  geom_point(data = V4,
             aes(x = dist, y = gamma)) +
  geom_smooth(data = V4,
              aes(x = dist, y = gamma),
              se = TRUE,
              span = 1,
              alpha = 0.3) +
  labs(x = "Distance (in km)", y = "Semi-variogram") +
  theme(text = element_text(size = 15))
#' We have no remaining spatial dependency.



#' Well...not entire correct. What is this variogram telling
#' is? We gave it residuals from all years in one long
#' vector plus the spatial coordinates. It does not know
#' that the residuals come from 10 blocks. We should also
#' calculate a variogram for the residuals of each year.



#' That means 10 variograms.
#' Instead of doing this manually, we wrote some fancy code to do this
#' automatically. There is no need to fully understand this, but we
#' did annotate the R code a little bit.

# Step 1: Split the MyData_sf object by fYear.
SplitData <- split(MyData_sf, MyData_sf$fYear)
SplitData

#' Step 2: lapply()
#' Use lapply() to execute the function CalculateVariogram() for each
#' element (i.e. residuals from a specific year) in SplitData.
CalculateVariogram <- function(year) {
  v <- variogram(Res ~ 1,
                 data <- SplitData[[year]],
                 cutoff = 100,
                 cressie = TRUE)
  v$fYear <- as.numeric(year)
  return(v)}

#' For each element in SplitData (i.e. for each year), calculate the variogram.
VarioList <- lapply(names(SplitData), CalculateVariogram)
VarioList
#' np:    number of site combinations within a distance band.
#' dist:  Distance band for which the variogram is being calculated.
#' gamma: Variogram values.
#' fYear: Year identifier.
#' The rest is not relevant.

#' Step 3: unlist
#' VarioList is a list with results. We want to have it as a data frame.
V_all <- do.call(what = rbind,
                 args = VarioList)

#' This function is stacking the elements of VarioList on top of
#' each other to create a single data frame.
#' The function do.call calls the rbind function with as argument VarioList.
#' Do something (what) with something (args).


#' Now we can plot the results.
ggplot() +
  geom_point(data = V_all,
             aes(x = dist, y = gamma)) +
  geom_smooth(data = V_all,
              aes(x = dist, y = gamma),
              se = TRUE,
              span = 1,
              alpha = 0.3) +
  labs(x = "Distance (in km)", y = "Semi-variogram") +
  facet_wrap(~ fYear, scales = "fixed") +
  theme(text = element_text(size = 15))
#' The variograms of the residuals by year do certainly not show spatial
#' dependency.

#' We conclude that there is no strong spatial dependency in the residuals.
#' To be more precise:
#'  -The graph on the right shows that there is no spatial dependency within
#'   a year.
#'  -The previous graph showed that if we look at all residuals (irrespective
#'   of year) there is neither spatial dependency.

# 2013, 2028, 2020, 2021, 2022
# try multiple likelihood
# Or use the AR1 term per year, not with knots.

# Section 25: Compare models----

#' Let's plot the results of the model, without and with the
#' spatial correlation side by side.
Out2 <- I1$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh1 <- I2a$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh2 <- I2b$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
rownames(Out2)       <- I1$names.fixed
rownames(Out2.mesh1) <- I2a$names.fixed
rownames(Out2.mesh2) <- I2b$names.fixed


#' Names for the models:
MyNames <- c("NB GLM",
             "Spatial NB GLM mesh 1",
             "Spatial NB GLM mesh 2")

#' Compare results of the two models using MyCompareBetasofModels
#' (which is in our support file).
MyCompareBetasofModels(AllModels = list(Out2, Out2.mesh1, Out2.mesh2),
                       ModelNames = MyNames)

#' Let's plot the results of the model, without and with the
#' spatial correlation side by side.
Out2 <- I1$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh1 <- I2a$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh2 <- I2b$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh1.repYear <- I3$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
Out2.mesh1.ar1Year <- I4$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
rownames(Out2)       <- I1$names.fixed
rownames(Out2.mesh1) <- I2a$names.fixed
rownames(Out2.mesh2) <- I2b$names.fixed
rownames(Out2.mesh1.repYear) <- I3$names.fixed
rownames(Out2.mesh1.ar1Year) <- I4$names.fixed


#' Names for the models:
MyNames <- c("NB GLM",
             "Spatial NB GLM mesh 1",
             "Spatial NB GLM mesh 2",
             "Spatial NB GLM mesh 1 with rep ST term",
             "Spatial NB GLM mesh 1 with AR1 ST term")

#' Compare results of the two models using MyCompareBetasofModels
#' (which is in our support file).
MyCompareBetasofModels(AllModels = list(Out2, Out2.mesh1, Out2.mesh1, Out2.mesh1.repYear, Out2.mesh1.ar1Year),
                       ModelNames = MyNames)

#' There are some differences between the model without, and with
#' spatial dependency. There are no major differences between
#' the two spatial models.


#' if the theta is ultra small, then you should be worried
I1$summary.hyperpar[, "mean"]
I2a$summary.hyperpar[1, "mean"]
I2b$summary.hyperpar[1, "mean"]
I3$summary.hyperpar[1, "mean"]
I4$summary.hyperpar[1, "mean"]


# Section 26: Prediction----

#' A question we quite often get is:
#'  Can we predict for a specific location?
#'  The answer is yes. We will show how to do this now.

#' Let us first write down the fitted model.
#' Here are the results (again).

#' Posterior mean values and 95% CI for the regression parameters:
BetaFinal <- I4$summary.fixed[,c("mean", "0.025quant", "0.975quant")]
print(BetaFinal, digits = 2)

#' Visualise the betas using ggplot2.
#' 1. Create a data frame.
BetaFinal_df <- data.frame(Covariate = rownames(BetaFinal),
                           Mean      = round(BetaFinal[,"mean"], 2),
                           Lower     = round(BetaFinal[,"0.025quant"], 2),
                           Upper     = round(BetaFinal[,"0.975quant"], 2))

#' Explicitly set the order of the covariates to match their original order
BetaFinal_df$Covariate <- factor(BetaFinal_df$Covariate, levels = rownames(BetaFinal))

#' 2. Plot the info in Betas2_df
ggplot(BetaFinal_df, aes(x = Mean, y = Covariate)) +
  geom_pointrange(aes(xmin = Lower, xmax = Upper),
                  fatten = 1.5,
                  color = "blue") +
  geom_vline(aes(xintercept = 0),
             linetype = "dotted",
             col = "red") +
  labs(title = "Posterior means and 95% Credible Intervals",
       x = "Value",
       y = "Covariate") +
  theme_minimal()
#' If a 95% CI crosses the dotted red line, then the covariate is
#' important.

# intercept is the baseline where everything is at the average.
# Lakes have higher dipteran counts, but this is not statistically important.
# If you use a spatial-temporal model, you may very well find that the confidence intervals will 
# get a bit smaller, making the differences between lakes and rivers statistically important.

#' Here is the posterior mean of the theta:
theta.pd <- I4$marginals.hyperpar$`size for the nbinomial observations (1/overdispersion)`
theta.pm <- inla.emarginal(function(x) x, theta.pd)
round(theta.pm, digits = 2)
BetaFinal_df

#' The model is:
#' Counts_i ~ NB(mu_i, theta = 0.14)
#' E[Counts_i] = mu_i
#' var[Counts_i] = mu_i + mu_i^2 / 1.293

#' mu_i = exp(1.42 + 
#'            0.78 * eqr.std +
#'            0.44 * ppt.std +
#'            0.27 * tmin.std +
#'            0.10 * ws.std +
#'            0.61 * elevation.std +
#'            0.11 * agriculture.std +
#'            0.11 * artificial.std +
#'            0.06 * natural.std +
#'            0.00 * year.std +
#'            0.31 * waterbody_type.L + u_i)


#' Recall that this is the mesh:
p1 <- ggplot() +
         geom_sf(data = CroppedLithuania_UTM, fill = "lightblue") +
         geom_point(data = df,
                    aes(x = Xkm, y = Ykm),
                    col = grey(0.2),
                    size = 0.5,
                    alpha = 0.5) +
         theme_minimal()
p1

#' Suppose that we want to predict for
#' the red point (it is a new point)
MyData2 <- data.frame(Xkm = mean(df$Xkm),
                      Ykm = mean(df$Ykm) + 0.3)
p1 + geom_point(data = MyData2,
                aes(x = Xkm,
                    y = Ykm),
                col = "red")


#' That is this point.
MyData2

# ENDED HERE FOR NOW
#' In order to predict the the number of dipteran values, we also need
#' covariate values. Because the red dot is not a sampling location,
#' we do not have any measured covariate values for it.
#' In a real application, you would interpolate these, or
#' you would know them somehow based on your question. For the
#' sake of this example, we pretend that we know all the
#' covariate values at the red site:

MyData2$TreeHeight.std     <- -0.5
MyData2$Distance2Edge.std  <- -0.25
MyData2$NearestTallOak.std <-  0
MyData2$HerbCover.std      <-  0.3
MyData2$fNLowBranches <- factor("Low", levels = levels(BF$fNLowBranches))
MyData2$fShelter      <- factor("Low", levels = levels(BF$fShelter))
MyData2$fBuckthorn    <- factor("Low", levels = levels(BF$fBuckthorn))
MyData2$fBramble      <- factor("Low", levels = levels(BF$fBramble))
MyData2$fSmallOak     <- factor("High", levels = levels(BF$fSmallOak))

#' In other words, we pretend that at the red site we have the following
#' covariate conditions:
MyData2


#' Because of the stack we need to do the prediction slightly different:
#'  1. Use the stack for the original data.
#'  2. Make a stack for the data for which predictions are needed.
#'  3. Combine them.
#'  4. Run INLA
#'  5. Extract the relevant pieces and plot them.



#' 1. See above for the stack of the observed data.

#' 2. Make a stack for the data for which predictions are needed.
#' For these covariate conditions, we make an X matrix for prediction:
Xp <- model.matrix(~ TreeHeight.std + Distance2Edge.std +
                     NearestTallOak.std + HerbCover.std +
                     fNLowBranches + fShelter + fBuckthorn +
                     fBramble + fSmallOak,
                  data = MyData2)
Xp <- as.data.frame(Xp)
Xp

#' If we predict based on only the covariates, then we can use the
#' stack below. It will calculate:
#'    mu = exp(Intercept + Xp * beta)

StackPred <- inla.stack(
               tag = "Predict",
               data = list(NEggs = NA),
               A = list(1, 1),
               effects = list(
               Intercept = rep(1, nrow(Xp)),  #' Intercept
               Xp = Xp[,-1]))                 #' Covariates for prediction..

#' But we also want to include the effect of the spatial term. In other
#' words, we want to predict based on:

#'    mu = exp(Intercept + Xp * beta + A * w)

#' This means we also need to make an A matrix for the spatial term.
#' We do that as follows. We first grab the coordinates for which we
#' want to make predictions. In this case, that is only one place
#' (you can easily add more).
LocPred <- MyData2[, c("Xkm", "Ykm")] #' Give coordinates to inla.spde.make.A()
LocPred <- as.matrix(LocPred)         #' Has to be a matrix to avoid error.
A3.Pred <- inla.spde.make.A(mesh3, loc = LocPred)
dim(A3.Pred)  #' 1 row and 2018 vertices


#' This is our stack for prediction:  exp(Intercept + Xp * beta + A3.Pred * w)
StackPred <- inla.stack(
  tag = "Predict",
  data = list(NEggs = NA),
  A = list(1, 1, A3.Pred),
  effects = list(
    Intercept = rep(1, nrow(Xp)),  #' Intercept.
    Xp        = Xp[,-1],           #' Covariates for prediction.
    w         = w3.index))         #' w's from mesh 3.



#' 3. Combine the two stacks.
All.stacks <- inla.stack(Stack3, StackPred)



#' 4. Run INLA with the combined stack.
#' This is the spatial NB GLM based on mesh 3.
Pred2.mesh3 <- inla(f2.mesh3,
                 family = "nbinomial",
                 data = inla.stack.data(All.stacks),
                 control.compute = MyControlCompute,
                 control.predictor = list(link = 1,
                                          A = inla.stack.A(All.stacks)))

#' The link = 1 will ensure that the exp() is applied for the
#' predicted values.




#' 5. Extract the relevant pieces and plot it.

#' It is now a little bit a headache to extract the predicted values.
#' This is the crucial part for extracting the correct rows.
index.Fit  <- inla.stack.index(All.stacks, tag = "Fit")$data
index.Pred <- inla.stack.index(All.stacks, tag = "Predict")$data

#' The `tag` option gives an index for the rows belonging to the
#' 'Fit' stack, and an index for the rows of the 'Predict' stack.


#' These allow us to extract the correct rows:
Fit2.fit  <- Pred2.mesh3$summary.fitted.values[index.Fit, c(1,3,5)]   #' 245  by 3
Fit2.pred <- Pred2.mesh3$summary.fitted.values[index.Pred, c(1,3,5)]  #' 100 by 3


#' It is the second one we need as these are for the covariate values in
#' MyData. Extract the relevant information and add this to the
#' MyData object.
MyData2$mu    <- Fit2.pred[, "mean"]
MyData2$selow <- Fit2.pred[, "0.025quant"]
MyData2$seup  <- Fit2.pred[, "0.975quant"]
MyData2

#' Hence, at the red dot, and for the given (made up) covariate
#' values, the expected number of eggs is 0.38, and its 95% credible
#' interval goes from 0.106 to 1.03.


#' You can easily extend this code if you want to predict for a whole
#' series of covariate values or locations.

#' You can also use INLA to predict covariate values at unmeasured
#' covariate values (just model and predict TreeHeight.std as a function of
#' an intercept and a spatial random field, and repeat for all other
#' covariates).

# Section 27: Clean up----

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