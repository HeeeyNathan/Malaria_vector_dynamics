# Load libraries
library(tidyverse)

# Load data
# All Lithuanian macroinvertebrate data from lakes and rivers (2010 - 2023)
all_raw_data <- as_tibble(read.csv("Data/Bio_data_2024.06.03_long_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE)) %>% 
  filter(year <= 2022) %>% 
  mutate(fyear = factor(year),
         site_id = factor(site_id),
         site_code = factor(site_code),
         sample_id = factor(sample_id),
         date = as.Date(date, format = "%d/%m/%Y"), 
         river_type = factor(river_type),
         state = factor(state),
         EQC = factor(EQC, levels = c("Bad", "Poor", "Moderate", "Good", "High"), ordered = T),
         waterbody_name = factor(waterbody_name),
         waterbody_type = factor(waterbody_type)) %>%
  arrange(desc(waterbody_type), site_id, year)

all_indices <- as_tibble(read.csv("Data/Diptera_indices_wLandcover_and_ENV_edited.csv", h = T, sep = ",", stringsAsFactors = FALSE, check.names=FALSE))  %>% 
  filter(year <= 2022) %>% 
  mutate(fyear = factor(year),
         site_id = factor(site_id),
         site_code = factor(site_code),
         sample_id = factor(sample_id),
         EQC = factor(EQC, levels = c("Bad", "Poor", "Moderate", "Good", "High"), ordered = T),
         date = as.Date(date, format = "%d/%m/%Y"),
         river_type = factor(river_type)) %>%
  arrange(desc(waterbody_type), site_id, year)

# Riverine dipteran abundance though time 
ggplot(subset(all_indices, c(waterbody_type == "river" & dipt_abund > 0)), aes(x = year, y = log(dipt_abund), colour = river_type)) +
  geom_point(size = 4, aes(fill = river_type), alpha = 0.5) +
  geom_smooth(method = "gam", se = T, color = "blue") +  # Keep the regression line black
  theme_minimal(base_size = 14) +
  scale_color_brewer() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  labs(title = "Dipteran abundance through time coloured by river type",
       subtitle = "Log-transformed dipteran abundances across river sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year",
       colour = "river_type") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10))

# Riverine vector abundance through time
ggplot(subset(all_indices, c(waterbody_type == "river" & vec_abund > 0)), aes(x = year, y = log(vec_abund), colour = river_type)) +
  geom_point(size = 4, aes(fill = river_type), alpha = 0.5) +
  geom_smooth(method = "gam", se = T, color = "blue") +  # Keep the regression line black
  theme_minimal(base_size = 14) +
  scale_color_brewer() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  labs(title = "Vector abundance through time coloured by river type",
       subtitle = "Log-transformed vector abundances across river sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran vector family and year",
       colour = "river_type") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10))

# Lake dipteran abundance though time 
ggplot(subset(all_indices, c(waterbody_type == "lake" & dipt_abund > 0)), aes(x = year, y = log(dipt_abund))) +
  geom_point(size = 4, alpha = 0.5) +
  geom_smooth(method = "gam", se = T, color = "blue") +  # Keep the regression line black
  theme_minimal(base_size = 14) +
  scale_color_brewer() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  labs(title = "Dipteran abundance through time",
       subtitle = "Log-transformed dipteran abundances across lake sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10))

# Lake vector abundance through time
ggplot(subset(all_indices, c(waterbody_type == "lake" & vec_abund > 0)), aes(x = year, y = log(vec_abund))) +
  geom_point(size = 4, alpha = 0.5) +
  geom_smooth(method = "gam", se = T, color = "blue") +  # Keep the regression line black
  theme_minimal(base_size = 14) +
  scale_color_brewer() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  labs(title = "Vector abundance through time",
       subtitle = "Log-transformed vector abundances across lake sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran vector family and year") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10))









# Pull out diptera only
diptera <- all_data %>% 
  filter(order == "Diptera") %>% 
  filter(year <= 2022) %>% 
  filter(!grepl("delete", notes)) %>% 
  dplyr::select(site_id:year, fyear, date:longitude, waterbody_name:river_type, family, 
                # Agricultural.areas_100m, Artificial.surfaces_100m, Forest.and.semi.natural.areas_100m, Water.bodies_100m, Wetlands_100m,
                # Agricultural.areas_200m, Artificial.surfaces_200m, Forest.and.semi.natural.areas_200m, Water.bodies_200m, Wetlands_200m,
                # Agricultural.areas_300m, Artificial.surfaces_300m, Forest.and.semi.natural.areas_300m, Water.bodies_300m, Wetlands_300m,
                # ppt, q, tmax, tmin, ws,
                abundance) %>% 
  arrange(desc(waterbody_type), site_id, year)
any(duplicated(diptera[c("site_code", "family")])) # check for duplicates

# summarise duplicate rows into succinct diptera database
diptera_family <- diptera %>%
  group_by(site_id, site_code, sample_id, day, month, year, fyear, date, 
           latitude, longitude, waterbody_name, waterbody_type, state, river_type, family) %>% 
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(desc(waterbody_type), site_id, year)
any(duplicated(diptera_family[c("site_code", "family")])) # check for duplicates

# summarise diptera family data by year
diptera_family_year <- diptera_family %>% 
  group_by(year, fyear, waterbody_type, family) %>% 
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>% 
    arrange(desc(waterbody_type), year, family)

# summarise diptera data by year
diptera_year <- diptera_family %>% 
  group_by(year, fyear, waterbody_type) %>% 
  summarise(abundance = sum(abundance, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(desc(waterbody_type), year)

# Exploratory plotting
# Changes in dipterans through time
# Define colouring
waterbody_cols <- c("#009292", "#DB6D00")
#line plot
ggplot(diptera_year, aes(x = year, y = abundance, colour = waterbody_type)) +
  geom_point(aes(size = 2, alpha = 0.7), show.legend = c(size = FALSE, color = TRUE, alpha = FALSE)) +
  geom_smooth(method = "gam", se = T) +
  scale_color_manual(values = waterbody_cols) +
  theme_minimal() +
  labs(title = "Changes in dipteran abundance in over time",
       x = "Year",
       y = "Abundance",
       color = "Waterbody type") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))

# Changes in dipteran families through time
# Rivers
# Define colouring
river_family_cols <- c("gray", "gray", "gray", "#009292", "gray", "gray", "#B66DFF", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray","gray", "#DB6D00", "gray", "gray", "gray", "gray", "gray")
# scatter plot
ggplot(subset(diptera_family_year, waterbody_type == "river"), aes(x = year, y = log(abundance + 1), color = family)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(values = river_family_cols) +
  theme_minimal(base_size = 14) +
  labs(title = "Trends in dipteran family abundance in rivers between 2010 and 2022",
       subtitle = "Log-transformed abundance across river sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year.",
       colour = "Diptera family") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
# line plot
ggplot(subset(diptera_family_year, waterbody_type == "river"), aes(x = year, y = log(abundance + 1), color = family)) +
  geom_line(aes(linewidth = ifelse(family %in% c("Ceratopogonidae", "Culicidae", "Simuliidae"), 1, 0.5), color = family), 
            alpha = 0.7, show.legend = c(linewidth = FALSE, color = TRUE)) +
  scale_color_manual(values = river_family_cols) +
  theme_minimal(base_size = 14) +
  labs(title = "Changes in dipteran family abundance in rivers between 2010 and 2022",
       subtitle = "Log-transformed abundance across river sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year.",
       colour = "Diptera family") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13))

# Lakes
# Define colouring
lake_family_cols <- c("gray", "#009292", "gray", "gray", "#B66DFF", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "#DB6D00","gray", "gray", "gray", "gray")
# scatter plot
ggplot(subset(diptera_family_year, waterbody_type == "lake"), aes(x = year, y = log(abundance + 1), color = family)) +
  geom_point(alpha = 0.7) + 
  scale_color_manual(values = lake_family_cols) +
  theme_minimal(base_size = 14) +
  labs(title = "Trends in dipteran family abundance in lakes between 2013 and 2022",
       subtitle = "Log-transformed abundance across lake sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year",
       colour = "Diptera family") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
#line plot
ggplot(subset(diptera_family_year, waterbody_type == "lake"), aes(x = year, y = log(abundance + 1), color = family)) +
  geom_line(aes(linewidth = ifelse(family %in% c("Ceratopogonidae", "Culicidae", "Simuliidae"), 1, 0.5), color = family), 
            alpha = 0.7, show.legend = c(linewidth = FALSE, color = TRUE)) +
  scale_color_manual(values = lake_family_cols) +
  theme_minimal(base_size = 14) +
  labs(title = "Changes in dipteran family abundance in lakes between 2013 and 2022",
       subtitle = "Log-transformed abundance across lake sites in Lithuania",
       x = "Year",
       y = "log(abundance + 1)",
       caption = "Data aggregated by dipteran family and year",
       colour = "Diptera family") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

