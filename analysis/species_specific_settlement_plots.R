#Plotting rockfish species settlement to Oregon SMURFs

#Author: Daniel Ottmann
#Created: 2016
#Updated: 2025 - Cameron Royer
###############################################################################
#           Readme

#This script is modified from Daniel Ottmann's original settlement plots to 
#produce species specific settlement rates in the same format as the original
#settlement plots script.

# This script is designed plot barplots comparing settlement among years and between marine reserves and comparison area of central and southern Oregon.
# It alaso plots seasonal trends in os settlement in each region.
# All these figures are modified from Ottmann et al (2018).

#data required:
#
#Species specific recruitment table - created in the legacy_recruitment_plus_species R script.
#props_sfla -includes settlement rates for smel and sfla based on proportions ID'ed
################################################################################
# Load packages
library(dplyr)
library(ggplot2)

#Setting working directory
setwd("../OYTB recruitment/")
list.files()

# Load raw data
settlement <- read.csv("OYTB_Recruitment2024.csv", sep = "\t", stringsAsFactors = FALSE) # Note that we use the table that ccomes out from "RecruitmentRate_Table3.R" that takes the mean from all SMURFs within each region

head(settlement)


# Adjust the data processing steps
df <- settlement %>%
  mutate(spp = case_when(
    spp == "sfla" ~ "SFLA",
    spp == "smel" ~ "SMEL",
    spp == "un_ID" ~ "un_ID",
    spp == "total" ~ "Total",
    TRUE ~ spp
  )) %>%
  filter(year != 2011,
         spp == "SFLA" & julian %in% c(121:255) |  # June 1 - Sept 15; 121 is May 1
           spp == "SMEL" & julian %in% c(121:255) |
           spp == "un_ID" &julian %in% c(121:255) |
           spp == "Total"  & julian %in% c(121:255))

# Annual mean recruitment calculation:
temp <- df %>%
  group_by(year, spp) %>%
  summarise(annual_mean_settlement_rate = mean(rate))

# Join the annual mean rate back to the original data:
df1 <- left_join(df, temp, by = c("year", "spp"))

#Normalize settlement rates per annual mean_rate
df1 <- df1 %>%
  mutate(norm_settlement_rate = ifelse(annual_mean_settlement_rate == 0, 0, rate / annual_mean_settlement_rate)) %>%
  group_by(region, spp, mr) %>%
  summarise(norm_mean_settlement_rate = mean(norm_settlement_rate, na.rm = TRUE),
            se = sqrt(sd(norm_settlement_rate, na.rm = TRUE) / n()))

# Calculate the sum for SFLA and SMEL
df1_total <- df1 %>%
  filter(spp %in% c("SFLA", "SMEL")) %>%
  group_by(region, mr) %>%
  summarise(
    norm_mean_settlement_rate = sum(norm_mean_settlement_rate, na.rm = TRUE),
    se = sqrt(sum(se^2, na.rm = TRUE))  # Combine standard errors for the sum
  )

# Add the 'Total' rows to the summary data
df1_total <- df1_total %>%
  mutate(spp = "Total")

# Combine the 'Total' data back with the original data
df1_corrected <- bind_rows(df1 %>% filter(spp != "Total"), df1_total)

# Plot for SFLA, SMEL, and Total with a single set of error bars for Total
p <- ggplot(data = df1_corrected, aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(
    aes(ymin = norm_mean_settlement_rate - se, ymax = norm_mean_settlement_rate + se),
    width = .4, size = 1, stat = "identity", position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = c("cadetblue", "royalblue4"), labels = c("Non-reserve", "Reserve")) +
  facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))

# Display the plot
p

# Save the plot
png(filename = "../OYTB recruitment/OYTB settlement plots/OYTB_settlement_plots/OYTB_settlement_mr.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()


# Plot central and southern regions separate:
# Central region
p <- df1_corrected %>%
  filter(region == "Central") %>%
  ggplot(aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve")) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())
p
png(filename = "../OYTB recruitment/OYTB settlement plots/OYTB_settlement_plots/OYTB_settlement_central_mr.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()

# Southern region
p <- df1_corrected %>%
  filter(region == "South") %>%
  ggplot(aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve")) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())
p
png(filename = "../OYTB recruitment/OYTB settlement plots/OYTB_settlement_plots/OYTB_settlement_southern_mr.png", width = 12, height = 14, units = "cm", res = 400)
print(p)
dev.off()

# Plot differences among years
df2 <- df %>%
  group_by(region, year, spp) %>%
  summarise(mean_settlement_rate = mean(rate),
            se = sqrt(sd(rate) / n()))

# Remove entries for the South region in 2012 and 2013
df2 <- df2 %>%
  filter(!(region == "South" & year %in% c(2012, 2013, 2021)))

#################################
#########Troubleshooting#########
#################################
# Check for missing or zero values in mean_settlement_rate and se
df2 %>% 
  filter(is.na(mean_settlement_rate) | mean_settlement_rate == 0 | is.na(se) | se == 0)
# Check the summarized data to ensure 2017 is included
df2 %>% filter(year == 2017)

#################################
# Final plot for both regions:
p <- ggplot(data = df2, aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
                width = .4, size = 1, stat = "identity", position = position_dodge(width = 0.8)) +
  facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20)) +
  scale_fill_viridis_d()
p
# Save the plot
png(filename = "../OYTB recruitment/OYTB settlement plots/OYTB_settlement_plots/OYTB_settlement_years.png", width = 12, height = 14, units = "cm", res = 400)
print(p)
dev.off()
