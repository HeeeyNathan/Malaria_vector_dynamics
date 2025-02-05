# Load the required packages
library(tidyverse) # General processing and data formating
library(QBMS) # for extracting TerraClimate data
library(lubridate) # for formating dates and timeseries data
library(purrr) # for programing funcitons

# Load data
Data <- as_tibble(read.csv("Outputs/Diptera_indices_wCorine2018_14.1.2025.csv", sep = ",", h = T)) |> 
    mutate(sample_id = as.factor(sample_id)) |>
    mutate(site_id = as.factor(site_id)) |>
    mutate(site_code = as.factor(site_code)) |>
    mutate(date = as.Date(date, format = "%Y-%m-%d")) |>
    mutate(waterbody_type = as.factor(waterbody_type)) |>
    mutate(latitude = as.double(latitude)) |>
    mutate(longitude = as.double(longitude)) |>
    arrange(desc(waterbody_type), site_id, year)

# Calculate the observation period and number of repeated samplings for each site
sampling_info <- Data |>
  group_by(site_id) |>
  summarize(
    observation_period = max(year) - min(year) + 1,
    sampling_events = n_distinct(year))

# Join data
Data <- Data |>
  left_join(sampling_info, by = "site_id") |> 
  arrange(desc(waterbody_type), site_id, year)

# Remove duplicated rows 
Data_filtered <- Data |>
  select(sample_id, site_code, site_id, date, day, month, year, waterbody_type, latitude, longitude) |>
  distinct()

# Extracting the data
lon <- Data_filtered$longitude
lat <- Data_filtered$latitude

# Download offline versions of the TerraClimate data to aid in speeding up extraction
# ini_terraclimate('2009-01-01', '2023-12-31', c('ppt', 'q', 'tmax', 'tmin', 'ws')) # downloads offline datasets for the years and variables specified

# Get TerraClimate data for a given coordinate(s)
# terraclimate_data <- get_terraclimate(lat, lon, '2009-01-01', '2023-12-31', c('ppt', 'q', 'tmax', 'tmin', 'ws'), offline = TRUE) # links downloaded data to sets of coordinates and years provided
# saveRDS(terraclimate_data, "Outputs/LT_linked_terraclimate_data_updated.RDS") # save extracted data to save time
terraclimate_data <- readRDS("Outputs/LT_linked_terraclimate_data_updated.RDS") # saves time to always extract these data
str(terraclimate_data$climate) # check structure of linked climate data
View(terraclimate_data$climate[[1]]) # check climate data for site LTR1_4.10.2010

# Calculate means for an individual sample_id to test if the code below works as intended
## means calculated from the 12 months preceeding biological sampling e.g., if sampling for LTR1_2010 was conducted on the 4.10.2010, then calculate mean environmental data from 01.10.2009 to 30.09.2010

# extract climate data for the first row of the diptera_filtered dataset (i.e., LTR1_4.10.2010)
LTR1_4.10.2010 <- as_tibble(terraclimate_data$climate[[1]])
# Specific date and the preceding 12 months period
specific_date <- ymd(Data_filtered$date[1]) # date of biological sampling
start_date <- (specific_date %m-% months(12)) |> 
  floor_date(unit = "month") # identifies the start date for mean calculations (12 months prior to biological sampling)
end_date <- (specific_date %m-% months(1)) |> 
  floor_date(unit = "month") |> 
  ceiling_date(unit = "month") %m-% days(1) # identifies the end date for mean calculations (the month prior to biological sampling)
# Create a Date column in your data
LTR1_4.10.2010 <- LTR1_4.10.2010 |>
  mutate(date = make_date(year, month, 1))
# Filter data for the 12 months preceding the specific date
LTR1_4.10.2010_filtered <- LTR1_4.10.2010 |>
  filter(date >= start_date & date <= end_date)
# Calculate means
LTR1_4.10.2010_means <- LTR1_4.10.2010_filtered |>
  summarise(across(c(ppt, q, tmax, tmin, ws), ~ mean(.x, na.rm = TRUE))) # means calculated from the 12 months prior to biological sampling
print(LTR1_4.10.2010_means) # inspect calculated means

# streamlining the process
process_climate_data <- function(climate_data, sample_date) {
  specific_date <- ymd(sample_date)
  start_date <- (specific_date %m-% months(12)) |> floor_date(unit = "month")
  end_date <- (specific_date %m-% months(1)) |> floor_date(unit = "month") |> ceiling_date(unit = "month") %m-% days(1)
  
  climate_data |>
    mutate(date = make_date(year, month, 1)) |>
    filter(date >= start_date & date <= end_date) |>
    summarise(across(c(ppt, q, tmax, tmin, ws), ~ mean(.x, na.rm = TRUE)))
}

# Calculate means with the function defined above
calc_means <- map2_df(terraclimate_data$climate, Data_filtered$date, process_climate_data)
# Add sample_id to the results
calc_means <- bind_cols(Data_filtered |> select(sample_id), calc_means)
# Print the final dataframe
print(calc_means)

# Do the same test but on a random row to test if the code is correct
random_index <- sample(nrow(Data_filtered), 1)
# extract climate data for the first row of the diptera_filtered dataset
random_sample <- as_tibble(terraclimate_data$climate[[random_index]])
# Specific date and the preceding 12 months period
specific_date <- ymd(Data_filtered$date[random_index]) # date of biological sampling
start_date <- (specific_date %m-% months(12)) |> 
  floor_date(unit = "month") # identifies the start date for mean calculations (12 months prior to biological sampling)
end_date <- (specific_date %m-% months(1)) |> 
  floor_date(unit = "month") |> 
  ceiling_date(unit = "month") %m-% days(1) # identifies the end date for mean calculations (the month prior to biological sampling)
# Create a Date column in your data
random_sample <- random_sample |>
  mutate(date = make_date(year, month, 1))
# Filter data for the 12 months preceding the specific date
random_sample_filtered <- random_sample |>
  filter(date >= start_date & date <= end_date)
# Calculate means
random_sample_means <- random_sample_filtered |>
  summarise(across(c(ppt, q, tmax, tmin, ws), ~ mean(.x, na.rm = TRUE))) # means calculated from the 12 months prior to biological sampling
print(random_sample_means) # inspect calculated means
print(calc_means[random_index,]) # compare to loop calculated means

# save output
write.csv(calc_means, "Outputs/LT_river_lake_terraclimate_data_14.1.2025.csv", row.names = F)

# load EQR and EQC data
eqr <- read.csv("Data/Ecological_ratios_and_classes_03.12.2024.csv", sep = ",", h = T) |> 
  mutate(across(everything(), ~ replace(., . == "", NA)))

# Join env data to diptera data
Data_env <- Data |>
  left_join(calc_means, by = "sample_id") |> 
  dplyr::select(c(1:13, 43:44), everything())

# Bind EQR and EQC data
Data_env <- Data_env |>
  left_join(eqr, by = "site_code") |> 
  dplyr::select(c(1:12, 50:51), everything())

# save output
write.csv(Data_env, "Data/Bio_Env_data_14.1.2025.csv", row.names = F)

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