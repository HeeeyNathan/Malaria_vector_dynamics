# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(rphylopic)
library(scales)
library(stringr)
library(rphylopic)
library(vegan)

# Load your dataset (adjust the file path as needed)
haemosporidian <- read.csv("Data/Parus_caeruleus_haemosporidian_infections.csv")

# Summarize infection prevalence by period, calculating percentages
infection_summary <- haemosporidian %>%
  group_by(period) %>%
  summarise(Infected = sum(infection_binary), 
            Total = n()) %>%
  mutate(Not_Infected = Total - Infected,
         Infected_Percentage = (Infected / Total) * 100,
         Not_Infected_Percentage = (Not_Infected / Total) * 100)

# Melt data into long format for easy plotting
infection_long <- infection_summary %>%
  dplyr::select(period, Infected_Percentage, Not_Infected_Percentage) %>%
  gather(key = "Infection_Status", value = "Percentage", Infected_Percentage, Not_Infected_Percentage)

# Rename infection statuses for clarity
infection_long$Infection_Status <- factor(infection_long$Infection_Status, 
                                          levels = c("Not_Infected_Percentage", "Infected_Percentage"),
                                          labels = c("Not Infected", "Infected"))

# Update the period labels for the y-axis
infection_long$period <- factor(infection_long$period, 
                                levels = c("old", "new"),
                                labels = c("2003/2004", "2018/2019"))

# Create contingency table for Chi-square test
contingency_table <- table(haemosporidian$period, haemosporidian$infection_binary)

# Perform Chi-square test
chisq_test <- chisq.test(contingency_table)
p_value <- chisq_test$p.value
chi_square_stat <- chisq_test$statistic

# Simplify the p-value for the caption
p_value_simplified <- ifelse(p_value <= 0.05, "p ≤ 0.05", "p > 0.05")

# Plot overall infection prevalence as percentages across the two periods with labels
p <- ggplot(infection_long, aes(x = period, y = Percentage, fill = Infection_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "black", size = 5) +
  labs(title = "Haemosporian parasite prevalence in Parus caeruleus \nLocation: Curonian Lagoon (Rybachy), juveniles sampled",
       subtitle = paste0("Chi-Squared Test: χ² = ", round(chi_square_stat, 2), 
                         ", ", p_value_simplified),
       x = "", y = "Prevalence of infection (%)") +
  scale_fill_manual(values = c("Infected" = "#f2cc84", "Not Infected" = "#95ccba")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 14),  # Adjust tick mark label size
    axis.title = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
p
saveRDS(p, "Plots/Curonian_Lagoon_Malaria_Prevalence.rds")
ggsave("Plots/Curonian_Lagoon_Malaria_Prevalence.png", plot = p, width = 8, height = 8, units = "in", dpi = 900, device = "png", bg = "white")

##### Lets do it by genus now #####
# Create contingency tables for each parasite genus
haem_table <- table(haemosporidian$period, !is.na(haemosporidian$Haemoproteus))
leuco_table <- table(haemosporidian$period, !is.na(haemosporidian$Leucocytozoon))
plasmo_table <- table(haemosporidian$period, !is.na(haemosporidian$Plasmodium))

# Perform Chi-Squared test for Haemoproteus
haem_chi <- chisq.test(haem_table)

# Perform Fisher's Exact Test for Leucocytozoon and Plasmodium
leuco_fisher <- fisher.test(leuco_table)
plasmo_fisher <- fisher.test(plasmo_table)

# Extract p-values from the tests
haem_p_value <- haem_chi$p.value
leuco_p_value <- leuco_fisher$p.value
plasmo_p_value <- plasmo_fisher$p.value

# Simplify p-values for subtitle display
haem_p_simplified <- ifelse(haem_p_value <= 0.05, "p ≤ 0.05", "p > 0.05")
leuco_p_simplified <- ifelse(leuco_p_value <= 0.05, "p ≤ 0.05", "p > 0.05")
plasmo_p_simplified <- ifelse(plasmo_p_value <= 0.05, "p ≤ 0.05", "p > 0.05")

# Calculate the number of birds with mixed infections using the "infection_status" column
mixed_infection_count <- haemosporidian %>%
  filter(infection_status == "mixed") %>%
  nrow()

# Summarize infection prevalence by parasite genus and period
parasite_summary <- haemosporidian %>%
  group_by(period) %>%
  summarise(
    Haemoproteus = sum(!is.na(Haemoproteus)) / n() * 100,
    Leucocytozoon = sum(!is.na(Leucocytozoon)) / n() * 100,
    Plasmodium = sum(!is.na(Plasmodium)) / n() * 100
  ) %>%
  gather(key = "Parasite", value = "Prevalence", Haemoproteus, Leucocytozoon, Plasmodium)

# Update the period labels
parasite_summary$period <- factor(parasite_summary$period, 
                                  levels = c("old", "new"),
                                  labels = c("2003/2004", "2018/2019"))

# Create a data frame for significant asterisk labels with test types
significance_labels <- data.frame(
  Parasite = c("Haemoproteus", "Plasmodium"),
  period = rep("2018/2019", 2),
  Prevalence = c(
    max(parasite_summary$Prevalence[parasite_summary$Parasite == "Haemoproteus" & parasite_summary$period == "2018/2019"]) + 2,
    max(parasite_summary$Prevalence[parasite_summary$Parasite == "Plasmodium" & parasite_summary$period == "2018/2019"]) + 2
  ),
  label = c("* Chi-Squared: χ²", "* Fisher's exact")
)

# Plot the prevalence of each parasite genus by period (position = "dodge")
p <- ggplot(parasite_summary, aes(x = period, y = Prevalence, fill = Parasite)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Dodge bars
  geom_text(aes(label = paste0(round(Prevalence, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 5) +  # Add percentage labels
  
  # Add the Chi-Squared label for Haemoproteus in #f2cc84
  geom_text(data = significance_labels[significance_labels$Parasite == "Haemoproteus",], 
            aes(x = period, y = Prevalence, label = label), 
            position = position_dodge(width = 1), size = 6, vjust = -0.1, hjust = 1,
            color = "#f2cc84", fontface = "bold") + 
  
  # Add the Fisher's exact label for Plasmodium in #84a9f2
  geom_text(data = significance_labels[significance_labels$Parasite == "Plasmodium",], 
            aes(x = period, y = Prevalence, label = label), 
            position = position_dodge(width = 1), size = 6, vjust = -0.1, hjust = 0.0, 
            color = "#84a9f2", fontface = "bold") + 
  
  labs(title = "Prevalence of haemosporidian genera in Parus caeruleus \nLocation: Curonian Lagoon (Rybachy), juveniles sampled",
       subtitle = paste0("Mixed infections: ", mixed_infection_count, 
                         " | Haemoproteus: ", haem_p_simplified, 
                         " | Leucocytozoon: ", leuco_p_simplified, 
                         " | Plasmodium: ", plasmo_p_simplified),
       x = "", y = "Prevalence of infection (%)") +
  scale_fill_manual(values = c("Haemoproteus" = "#f2cc84", 
                               "Leucocytozoon" = "#95ccba", 
                               "Plasmodium" = "#84a9f2")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 14),  # Adjust tick mark label size
    axis.title = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
p
ggsave("C:/Users/natha/OneDrive/University of Johannesburg/Post-Doc/Nature Research Centre/How Changes in Bioversity Change Biodiversity/Seminars/October 2024/Curonian_Lagoon_Malaria_Prevalence_by_Genus.png", plot = p, width = 8, height = 8, units = "in", dpi = 900, device = "png", bg = "white")