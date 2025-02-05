# Load the required library
library(tidyverse)
library(dplyr)
library(janitor)

# read in the data
# Biological data sampling dates
bio_data <- as_tibble(read.csv("Outputs/Diptera_indices.csv", sep = ",", h = T)) |> 
  clean_names() |> 
         mutate(date           = as.Date(date, format = "%d/%m/%Y"))
bio_dates <- bio_data |> 
  dplyr::select(c(site_code, site_id, date, day, month, year))

# Environmental data (monthly resolution)
env_monthly <- as_tibble(read.csv("Data/Environmental_data_monthly_26.02.2024.csv", sep = ",", h = T)) |> 
  dplyr::select(-c(contributing_days))

# Environmental data (daily resolution)
env_daily <- as_tibble(readRDS("Data/Environmental_data_gapless_26.02.2024.rds"))

## Ensure date columns in both datasets are correct 
bio_dates$date_new <- as.Date(bio_dates$date, format = "%d.%m.%Y") # make new column with correct date format
bio_dates$year_month <- format(bio_dates$date_new, "%Y-%m") # Add a year month column that can link to monthly means
env_monthly$year_month # date/year_month are already in the correct format
colnames(env_daily)[colnames(env_daily) == "date"] <- "date_new" # date is already in the correct format, so just change column name
env_daily$year_month <- format(env_daily$date_new, "%Y-%m") # Add a year month column that can link to monthly means

# Convert the year_month column to a proper Date object
bio_dates$year_month <- as.Date(paste(bio_dates$year_month, "-01", sep = ""), format = "%Y-%m-%d")
env_monthly$year_month <- as.Date(paste(env_monthly$year_month, "-01", sep = ""), format = "%Y-%m-%d")

## TEST DATA
# # Create a vector of multiple values you want to subset by
# values_to_subset <- c("LTR1", "LTR1011", "LTR1013")
# # # Subset the dataframe using the 'filter' function from 'dplyr'
# bio_dates_test <- filter(bio_dates, site_id %in% values_to_subset)
# env_monthly_test <- filter(env_monthly, site_id %in% values_to_subset)

# Create a function to process data for a given site
process_site_data <- function(site_name, bio_dates, env_monthly) {
  
  # make site subset
  bio_data <- subset(bio_dates, site_id == site_name)
  env_data <- subset(env_monthly, site_id == site_name)
  
  # sort the datasets by the date
  bio_data <- bio_data[order(bio_data$year_month), ]
  env_data <- env_data[order(env_data$year_month), ]
  
  # Create a list to store the results for each variable
  result_list <- list()
  
  # List of variables to process
  vars_to_process <- c("flow_m3.s_mean", "velocity_m.s_mean", "suspended_solids_mg.l_mean", "pH_mean", "temperature_C_mean", "oxygen_dissolved_mg.l_mean",
                       "oxygen_saturation_mean", "BOD7_mg.l_mean", "ChDS_Cr_mg.l_mean", "NH4_N_mg.l_mean", "NO2_N_mg.l_mean", "NO3_N_mg.l_mean", "mineral_N_mg.l_mean",
                       "total_N_mg.l_mean", "PO4_P_mg.l_mean", "total_P_mg.l_mean", "electrical_conductivity_mikroS.cm_mean", "alkalinity_mmol.l_mean")
  
  # Iterate over each variable and calculate average and missing months
  for (var in vars_to_process) {
    # Iterate over each biological sampling
    for (i in 1:nrow(bio_data)) {
      bio_sampling_date <- as.Date(bio_data$year_month[i])
      start_date <- bio_sampling_date - months(13)
      
      # Filter data for the 12-month period
      filtered_data <- env_data |>
        filter(year_month > start_date & year_month <= bio_sampling_date) |>
        slice(-n())
      
      # Calculate the number of missing days
      num_entries <- sum(!is.na(filtered_data[[var]]))
      missing_months <- 12 - num_entries
      
      # Calculate average value
      if (missing_months <= 8) {
        average_value <- mean(filtered_data[[var]], na.rm = TRUE)
      } else {
        average_value <- NA
      }
      
      # Assign the average value to the sampling data
      col_name <- paste0(sub(".mean", "", var))
      col_missing_name <- paste0("missing_months_", sub(".mean", "", var))
      bio_data[[col_name]][i] <- average_value
      bio_data[[col_missing_name]][i] <- missing_months
    }
  }
  
  return(bio_data)
}

# List of unique site names
unique_sites <- unique(bio_dates$site_id)

# Process data for each site and store the results in a list
processed_data_list <- lapply(unique_sites, process_site_data, bio_dates = bio_dates, env_monthly = env_monthly)

# Combine everything into a single data frame
BioData_linkedto_envData <- do.call(rbind, processed_data_list) |> 
  dplyr::select(-c(date_new, year_month),
                -starts_with("missing_")) 

final_data <- bio_data |>
    dplyr::select(-c(site_id, date, day, month, year)) |> 
           left_join(BioData_linkedto_envData, by = "site_code") |> 
    dplyr::select(c(site_id, sample_id, site_code, day, month, year, date, latitude, longitude, waterbody_name, waterbody_type, state, river_type), everything())

# save file
write.csv(final_data, "Outputs/Env_Linked_YrlyMeans_02.12.2024.csv", row.names = F)

##### CLEAN UP --------------------
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