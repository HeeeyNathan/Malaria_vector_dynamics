# Add required library
library(tidyverse) #used to reshape the data

# Import WIDE data
data_wide <- as_tibble(read.csv("Data/LT_lake_diptera_taxalist_wide.csv", sep = ",", header = T, stringsAsFactors = F))

# Convert from SHORT to LONG form data
data_long <- data_wide %>%
  pivot_longer(
      cols = -c(taxonname, family),  # Exclude Taxonname and Family from pivoting
      names_to = "sample_id",           # Column name for the sample identifiers
      values_to = "abundance"            # Column name for the counts
    ) %>%
      filter(abundance != 0) # remove zeros from "abundance" column

# Export LONG data
write.csv(data_long, "Data/LT_lake_diptera_taxalist_long.csv", row.names = F)

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