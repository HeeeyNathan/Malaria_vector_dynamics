# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(nlme)

# Read the dataset
data <- read.csv("Data/Parus_caeruleus_haemosporidian_infections.csv") %>%
  mutate(year = as.factor(year)) %>% 
  mutate(period = as.factor(period)) %>% 
  mutate(sample_id = as.factor(sample_id))

# Inspecting dataframe:
str(data, vec.len = 2)

# Removing any data not from Juvenile birds
data_filtered <- data %>% 
  filter(age != "sad")

# view the data
View(data_filtered)

# Calculate prevalence and richness for each year:
yearly_stats <- data_filtered %>%
  group_by(year) %>%
  summarise(
    prevalence = mean(infection_binary) * 100,  # Prevalence as percentage
    richness = length(unique(infection[infection != "negative"])),  # Unique non-negative entries
    lineage_richness = length(unique(lineage_molecular_clean[lineage_molecular_clean != "negative"])))
print(yearly_stats)

# Early and late period
period_stats <- data_filtered %>%
  group_by(period) %>%
  summarise(
    prevalence = sum(infection_binary, na.rm = TRUE) / n() * 100,
    richness = n_distinct(infection[infection != "negative"], na.rm = TRUE),
    lineage_richness = length(unique(lineage_molecular_clean[lineage_molecular_clean != "negative"])))
print(period_stats)

# Statistical test for binary prevalence data
# Chi-square test for each year
yearly_prevalence <- table(data_filtered$year, data_filtered$infection_binary)
(chi_square_yearly <- chisq.test(yearly_prevalence))
# Chi-square test for each period
period_prevalence <- table(data_filtered$period, data_filtered$infection_binary)
(chi_square_period <- chisq.test(period_prevalence))

# Simple models w year as a fixed term
levels(data_filtered$year)
Gaus1 <- glmmTMB(infection_binary ~ year,
                 data = data_filtered)
summary(Gaus1)

# Compare which years differ using 'relevel'
data_filtered$year <- relevel(data_filtered$year, ref = "2003")
summary(glmmTMB(infection_binary ~ year, data = data_filtered))
data_filtered$year <- relevel(data_filtered$year, ref = "2004")
summary(glmmTMB(infection_binary ~ year, data = data_filtered))
data_filtered$year <- relevel(data_filtered$year, ref = "2018")
summary(glmmTMB(infection_binary ~ year, data = data_filtered))
data_filtered$year <- relevel(data_filtered$year, ref = "2019")
summary(glmmTMB(infection_binary ~ year, data = data_filtered))

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

