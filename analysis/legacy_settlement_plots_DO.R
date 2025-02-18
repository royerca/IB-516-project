
# Plotting fish settlement to Oregon SMURFS

# Author: Daniel Ottmann
# Created: 2016
# Last update: April 2021

###############################################################################
#       Readme

# This script is designed plot barplots comparing settlement among years and between marine reserves and comparison area of central and southern Oregon.
# It alaso plots seasonal trends in os settlement in each region.
# All these figures are modified from Ottmann et al (2018).

###############################################################################


###############################################################################
# Load packages
library(dplyr)
library(ggplot2)

#Setting working directory
setwd("C:/Users/16616/Desktop/Files/Oregon State University/Graduate School/Rockfish work/OYTB ID/OYTB recruitment/")
list.files()

# Load raw data
settlement <- read.csv("C:/Users/16616/Desktop/Files/Oregon State University/Graduate School/Rockfish work/OYTB ID/OYTB recruitment/Recruitment2024.csv", sep = "\t", stringsAsFactors = FALSE) # Note that we use the table that ccomes out from "RecruitmentRate_Table3.R" that takes the mean from all SMURFs within each region


df <- settlement %>%
  mutate(spp = case_when(spp == "SDIP" ~ "SR",
                         spp == "SMAR" ~ "Cabezon",
                         spp == "SNIG" ~ "Tiger",
                         T ~ spp)) %>%
  filter(year != 2011,
         spp == "Cabezon" & julian %in% c(121:255) | #June 1 - Sept 15; 121 is may1
           spp == "OYTB" & julian %in% c(121:255) |
           spp == "QGBC" &   julian %in% c(121:255) |
           spp == "SR"  & julian %in% c(121:255) |
           spp == "Tiger"  & julian %in% c(121:255))

df <- df %>%
  mutate(year = as.numeric(year))

#####################################################


#####################################################
# Plot differences between marine reserves and comparison areas

# Because we care about differences between marine reserves and we want to ignore annual effets,
# we have to normalize settlement per year (See Ottmann et al 2018).

# Get annual mean recruitment:
temp <- df %>%
  group_by(year, spp) %>%
  summarise(annual_mean_settlement_rate = mean(rate))

# Add it to df in a new data frame:
df1 <- left_join(df, temp, by = c("year", "spp"))




###temporary(?) fix for tiger rockfish data issues
tiger_data <- df1 %>% filter(spp == "Tiger")
print(tiger_data)

df1 %>% filter(spp == "Tiger") %>% summarise(mean_rate = mean(rate), mean_annual = mean(annual_mean_settlement_rate))

df1 %>% filter(spp == "Tiger") %>% print()

temp <- df1 %>%
  group_by(year, spp) %>%
  summarise(annual_mean_settlement_rate = mean(rate, na.rm = TRUE), .groups = "drop")

df1 <- left_join(df, temp, by = c("year", "spp"))

# Normalize settlement rate per annual meanrate:
df1 <- df1 %>%
  mutate(norm_settlement_rate = rate / annual_mean_settlement_rate) %>%
  group_by(region, spp, mr) %>%
  summarise(
    norm_mean_settlement_rate = mean(norm_settlement_rate, na.rm = TRUE),
    se = sqrt(sd(norm_settlement_rate, na.rm = TRUE) / n()),
    .groups = "drop"
  )
df1 %>% filter(spp == "Tiger") %>% print()
####
###BACK to Dani's code

# Plot both regions together:
p <-   ggplot(data = df1, aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, size = 1, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("cadetblue", "royalblue4"),labels = c("Non-reserve", "Reserve"))+
  facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))

png(filename = "plots_settlement_mr.png", width = 16, height = 14, units = "cm", res = 400)
p
dev.off()


# Plot central and southern regions separate:
p <- df1 %>%
  filter(region == "Central") %>%
  ggplot(aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())

png(filename = "plots_settlement_central_mr.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()

p <- df1 %>%
  filter(region == "South") %>%
  ggplot(aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())

png(filename = "plots_settlement_southern_mr.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()



#####################################################
# Plot differences among years

# Here we do care about annual differences, so we do not need to normalize anything

# Create the data frame:
df2 <- df %>%
  group_by(region, year, spp) %>%
  summarise(mean_settlement_rate = mean(rate),
            se = sqrt(sd(rate) / n()))

# Create placeholders for south 2012, 2013, and 2021 when we did not sample so that both facets of figure are comprable
ph1 <- data.frame("South", 2012, "Cabezon", 0, 0); colnames(ph1) <- c("region","year","spp","mean_settlement_rate","se")
ph2 <- data.frame("South", 2012, "SR", 0, 0); colnames(ph2) <- c("region","year","spp","mean_settlement_rate","se")
ph3 <- data.frame("South", 2012, "QGBC", 0, 0); colnames(ph3) <- c("region","year","spp","mean_settlement_rate","se")
ph4 <- data.frame("South", 2012, "Tiger", 0, 0); colnames(ph4) <- c("region","year","spp","mean_settlement_rate","se")
ph5 <- data.frame("South", 2012, "OYTB", 0, 0); colnames(ph5) <- c("region","year","spp","mean_settlement_rate","se")

ph6 <- data.frame("South", 2013, "Cabezon", 0, 0); colnames(ph6) <- c("region","year","spp","mean_settlement_rate","se")
ph7 <- data.frame("South", 2013, "SR", 0, 0); colnames(ph7) <- c("region","year","spp","mean_settlement_rate","se")
ph8 <- data.frame("South", 2013, "QGBC", 0, 0); colnames(ph8) <- c("region","year","spp","mean_settlement_rate","se")
ph9 <- data.frame("South", 2013, "Tiger", 0, 0); colnames(ph9) <- c("region","year","spp","mean_settlement_rate","se")
ph10 <- data.frame("South", 2013, "OYTB", 0, 0); colnames(ph10) <- c("region","year","spp","mean_settlement_rate","se")

ph11 <- data.frame("South", 2021, "Cabezon", 0, 0); colnames(ph11) <- c("region","year","spp","mean_settlement_rate","se")
ph12 <- data.frame("South", 2021, "SR", 0, 0); colnames(ph12) <- c("region","year","spp","mean_settlement_rate","se")
ph13 <- data.frame("South", 2021, "QGBC", 0, 0); colnames(ph13) <- c("region","year","spp","mean_settlement_rate","se")
ph14 <- data.frame("South", 2021, "Tiger", 0, 0); colnames(ph14) <- c("region","year","spp","mean_settlement_rate","se")
ph15 <- data.frame("South", 2021, "OYTB", 0, 0); colnames(ph15) <- c("region","year","spp","mean_settlement_rate","se")

df3 <- rbind(df2, ph1, ph2, ph3, ph4, ph5, ph6, ph7, ph8, ph9, ph10, ph11, ph12, ph13, ph14, ph15)

# Plot both regions in a figure:
p <- ggplot(data = df3, aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
                width = .4, size =1, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))

png(filename = "plots_settlement_years.png", width = 18, height = 14, units = "cm", res = 400)
p + scale_fill_viridis_d()
dev.off()


####GOOD SO FAR, haven't gone through the remaining code
############################################################################################
# Plot each region in different figures:

# Central Oregon:
p <- df2 %>%
  filter(region == "Central", year > 2013) %>%                                             # You can remove the , > 2013 to get all the 2012-19 period
  ggplot(aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  #  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())

png(filename = "plots_settlement_central_years.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()


# Southern Oregon:
p <- df2 %>%
  filter(region == "South") %>%
  ggplot(aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
                width = .4, stat = "identity", position = position_dodge(width = 0.8)) +
  #  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_bw() +
  theme(legend.title = element_blank())

png(filename = "plots_settlement_southern_years.png", width = 12, height = 14, units = "cm", res = 400)
p
dev.off()


######################################################################################
# Seasonal trends:

# Plot it out:  Ottmann et al (2018) used GAM models (and plotted the smooth with method = "GAM").
# For simplicity, here we plot it with the default method = "loess".
# There is very little difference in the final figures.

# Plot it out:
p <- ggplot() +
  geom_smooth(data = filter(df, spp %in% c("OYTB")), aes(x = julian, y = rate, color = as.factor(year), fill = as.factor(year)), alpha = .15, size = 1) +
#  geom_point(data = df, aes(x = julian, y = rate, color = as.factor(year))) +   # If we add points the upper y-limt increases
  facet_grid(spp~., scales = "free") +
  ylab("Recruitment rate (fish/SMURF/day)") +
  xlab("Day of the year") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))

png(filename = "plots_oytb_settlement_seasonal.png", width = 14, height = 18, units = "cm", res = 400)
p + scale_fill_viridis_d() + scale_color_viridis_d()
dev.off()


#                                   END OF SCRIPT
#####################################################################################
