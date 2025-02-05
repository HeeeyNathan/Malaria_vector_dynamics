# libraries
library(ggplot2)
library(ggside)
library(patchwork)
library(tidyquant)
library(tidyverse)
library(arm)
library(car)
library(effects)
library(GGally)
library(lattice)
library(lawstat)
library(mgcv)
library(outliers)
library(rlang)
library(gridExtra)
library(glmmTMB)
library(tidyverse)
library(DHARMa)
library(AICcmodavg)
library(performance)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggridges)

###############################################################################################################

# Load data in long format
all_lf <- as_tibble(read.csv("Data/Bio_data_2024.06.03_long_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE))  %>% 
  filter(order == "Diptera") %>% 
  filter(year <= 2022) %>% 
  filter(!grepl("delete", notes)) %>% 
  mutate(fyear = factor(year),
         site_id = factor(site_id),
         site_code = factor(site_code),
         sample_id = factor(sample_id),
         date = as.Date(date, format = "%d/%m/%Y"), 
         river_type = factor(river_type)) %>%
  dplyr::select(-notes, -site_name, -c(rank:infraorder), -c(subfamily:taxon_name), -c(observation_period:sampling_events)) %>% 
  arrange(desc(waterbody_type), site_id, year)
# Check for duplicated family names in each unique site_year
any(duplicated(all_lf[c("site_code", "family")])) # check for duplicates
# Are there missing values?
colSums(is.na(all_lf))

indices_lf <- as_tibble(read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE))  %>% 
  filter(year <= 2022) %>% 
  mutate(fyear = factor(year),
         site_id = factor(site_id),
         site_code = factor(site_code),
         sample_id = factor(sample_id),
         date = as.Date(date, format = "%d/%m/%Y"),
         river_type = factor(river_type)) %>%
  arrange(desc(waterbody_type), site_id, year)

all_fam <- all_lf %>%
  group_by(site_id, site_code, sample_id, day, month, year, fyear, date, 
           latitude, longitude, waterbody_name, waterbody_type, state, river_type, family, 
           Agricultural.areas_100m, Artificial.surfaces_100m, Forest.and.semi.natural.areas_100m, Water.bodies_100m, Wetlands_100m,
           Agricultural.areas_200m, Artificial.surfaces_200m, Forest.and.semi.natural.areas_200m, Water.bodies_200m, Wetlands_200m,
           Agricultural.areas_300m, Artificial.surfaces_300m, Forest.and.semi.natural.areas_300m, Water.bodies_300m, Wetlands_300m,
           ppt, q, tmax, tmin, ws) %>%
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(desc(waterbody_type), site_id, year)
# Check for duplicated family names in each unique site_year
any(duplicated(all_fam[c("site_code", "family")])) # check for duplicates

river_fam <- all_fam %>% 
  filter(waterbody_type == "river")

lake_fam <- all_fam %>% 
  filter(waterbody_type == "lake") %>% 
  dplyr::select(-river_type, -waterbody_name, -state)
  
all_vec <- all_fam %>%
  filter(family %in% c("Simuliidae", "Ceratopogonidae", "Culicidae")) %>% 
  arrange(desc(waterbody_type), site_id, year)

river_vec <- river_fam %>% 
  filter(family %in% c("Simuliidae", "Ceratopogonidae", "Culicidae")) %>% 
  arrange(site_id, year)

lake_vec <- lake_fam %>% 
  filter(family %in% c("Simuliidae", "Ceratopogonidae", "Culicidae")) %>% 
  arrange(site_id, year)

river_indices <- indices_lf %>% 
  filter(waterbody_type == "river")

lake_indices <- indices_lf %>% 
  filter(waterbody_type == "lake") %>% 
  dplyr::select(-river_type, -waterbody_name, -state)

# Inspect data
glimpse(all_lf, vec.len = 2)
glimpse(all_fam, vec.len = 2)
glimpse(all_vec, vec.len = 2)
glimpse(river_fam, vec.len = 2)
glimpse(lake_fam, vec.len = 2)
glimpse(river_vec, vec.len = 2)
glimpse(lake_vec, vec.len = 2)

# Initial plotting
ridge <- ggplot(lake_vec, aes(x = log(abundance), y = family, fill = after_stat(x))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Abundance", option = "C") +
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal()
ridge


ggplot(subset(river_indices, vec_abund != 0), aes(x = ppt, y = log(vec_abund))) +
  geom_point(size = 4, aes(fill = "river_type")) +
  geom_smooth(method = "gam", se = T, color = "blue") +  # Keep the regression line black
  ggtitle("Vector family richness by tmax by River Type") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Use a predefined color palette


# Initial plotting
p1 <-
  all_lf %>%
  ggplot(aes(date, log(abundance), color = NULL)) +
  geom_point(size = 2.2, alpha = 0.4) +
  geom_smooth(aes(color = NULL), se = TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = NULL),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  geom_ysidedensity(
    aes(x = after_stat(density),
        fill = NULL),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  # theme_tq() +
  My_theme +
  labs(title = "Dipteran abundance through time" ,
       subtitle = "Density Plot",
       x = "time", 
       y = "log(abundance)") +  
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)
p1

p2 <-
  all_lf_vec %>%
  ggplot(aes(date, log(abundance), color = NULL)) +
  geom_point(size = 2.2, alpha = 0.4) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = NULL),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  My_theme +
  facet_wrap(.~ waterbody_type) +
  labs(title = "Dipteran abundance through time by waterbody type " ,
       subtitle = "Density Plot",
       x = "Time", 
       y = "log(abundance)") +  
  theme(ggside.panel.scale.x = 0.5,
        ggside.panel.scale.y = 0.5)
p2

p3 <-
  all_lf_vec %>%
  ggplot(aes(date, log(abundance), color = family)) +
  geom_point(size = 2.2, alpha = 0.4) +
  geom_smooth(aes(color = NULL), se = TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = family),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  geom_ysidedensity(
    aes(x = after_stat(density),
        fill = family),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  # theme_tq() +
  My_theme +
  labs(title = "Dipteran abundance through time by vector family" ,
       subtitle = "Density Plot",
       x = "time", 
       y = "log(abundance)") +  
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)
p3


p4 <-
  all_lf_vec %>%
  ggplot(aes(date, log(abundance), color = family)) +
  geom_point(size = 2.2, alpha = 0.4) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = family),
    alpha = 0.5,
    size = 0.3,
    position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  My_theme +
  facet_wrap(.~ waterbody_type) +
  labs(title = "Dipteran abundance through time by vector family and waterbody type " ,
       subtitle = "Density Plot",
       x = "Time", 
       y = "log(abundance)") +  
  theme(ggside.panel.scale.x = 0.5,
        ggside.panel.scale.y = 0.5)
p4

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
