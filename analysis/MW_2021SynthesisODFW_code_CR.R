
## Load Libraries
library(devtools)

library(ggplot2) # Plotting and Data Wrangling
library(lemon)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(viridis)
library(viridisLite)
library(dplyr)
#elibrary(odfwTools)
library(gridExtra)
library(forcats)
library(lubridate)
library(expss)
library(pracma)
library(ggpubr)
library(ggrepel)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(lemon)

library(knitr) #Make pretty tables
library(kableExtra)
library(magick)
library(webshot)

library(lme4) #GLMM and GAM Statistical Analyses
library(DHARMa)
library(optimx)
library(dfoptim)
library(sjPlot)
library(mgcv)
library(mgcViz)
library(glmmTMB)
library(sjmisc)
library(sjlabelled)
library(multcomp)
library(emmeans)
library(MuMIn)
library(tidymv)

library(plsdepot) #Partial Least Squares Regression 
library(missMDA)

#Setting working directory
setwd("C:/Users/16616/OneDrive - Oregon State University/Desktop/Files/Oregon State University/Graduate School/SMURF/SMURF '24/Access")

# Load raw data
settlement <- read.delim("Recruitment24.txt", stringsAsFactors=FALSE) 
# Note that we use the table that comes out from "MW_Danis_Code....R" that calculates the settlement rate and centers it over the sampling interval

# Wrangle Data
df <- settlement %>%
  mutate(spp = case_when(spp == "SDIP" ~ "SR", #Rename to common names
                         spp == "SMAR" ~ "Cabezon",
                         spp == "SNIG" ~ "Tiger",
                         spp == "all_spp" ~ "Total",
                         T ~ spp)) %>%
  filter(year != 2011,
         spp == "Cabezon" & julian %in% c(121:255) | #Only keep top 5 spp of interest, but keep may 1 - sept 15
           spp == "OYTB" & julian %in% c(121:255) |
           spp == "QGBC" &   julian %in% c(121:255) |
           spp == "SR"  & julian %in% c(121:255) |
           spp == "Tiger"  & julian %in% c(121:255) |
           spp == "Total" & julian %in% c(121:255))


df2 <- settlement %>% #Use df2 if we are interested in all species (e.g., table, pie chart)
  mutate(spp = case_when(spp == "SDIP" ~ "SR", #Rename to common names
                         spp == "SMAR" ~ "Cabezon",
                         spp == "SNIG" ~ "Tiger",
                         spp == "CEMB" ~ "Calico Sculpin", 
                         spp == "GMAE" ~ "Northern Clingfish", 
                         spp == "HDEC" ~ "Kelp Greenling", 
                         spp == "LMUC" ~ "Snailfish", 
                         spp == "LPUL" ~ "Snailfish", 
                         spp == "RMUS" ~ "Kelp Clingfish",
                         spp == "SPAU" ~ "Bocaccio",
                         spp == "all_spp" ~ "Total",
                         T ~ spp)) %>%
  filter(year != 2011, # Remove 2011, different sampling methods
         spp != "no_fish", 
         #spp != "Total", # Not the correct total
         spp != "Calico Sculpin") # only 1 ever


## Abundance 
### Year 

total.tab <- df2 %>%
  group_by(region, year, spp) %>%
  summarise(total = sum(count)) %>%
  ungroup() %>%
  dplyr::select(-region)

t1 <- total.tab %>%
  kbl(caption = "Table 1. Total number of settlement stage fishes collected between 2012-2023.", col.names = c(" ", "Total Fishes")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  pack_rows(index = c("Central" = 11, "South" = 6))

t1


### Region 


# Define subset of data to use

tab <- df2 %>%
  group_by(region, year, spp) %>%
  summarise(sampsize = sum(count)) %>%
  pivot_wider(names_from = "year", values_from = "sampsize") %>%
  rowwise() %>%
  mutate(Species_total = round(sum(c_across("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2021", "2022", "2023"), na.rm=T))) %>%
  ungroup() 

temp1 <- tab %>%
  filter(region == "Central") %>%
  mutate(spp = fct_relevel(spp, "Bocaccio", "Cabezon", "Kelp Clingfish", "Kelp Greenling", "Northern Clingfish", "OYTB", "QGBC", "Snailfish", "SR", "Tiger", "unID", "Total")) %>%
  arrange(spp)

temp2 <- tab %>%
  filter(region == "South") %>%
  mutate(spp = fct_relevel(spp, "Bocaccio", "Cabezon", "Kelp Clingfish", "Kelp Greenling", "Northern Clingfish", "OYTB", "QGBC", "Snailfish", "SR", "Tiger", "unID", "Total")) %>%
  arrange(spp)

tab <- rbind(temp1, temp2) %>% # here the rows are in the correct order for the table
  ungroup() %>%
  dplyr::select(-region)

t2 <- tab %>%
  kbl(caption = "Table 2. Number of settlement-stage fishes collected between 2012-2023 by region (OYTB = olive, yellowtail, black rockfishes; QGBC = quillback, gopher, black and yellow, china, copper rockfishes; SR = splitnose, redbanded rockfishes) ", col.names = c(" ", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2021", "2022", "2023", "Species Total")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  pack_rows(index = c("Central" = 11, "South" = 6)) %>%
  add_header_above(c(" " = 1, "Year" = 11, " " = 1)) %>%
  kable_styling(font_size=12) %>%
  row_spec(c(15,24), bold = TRUE) %>%
  scroll_box(height = "600px")

t2
ggsave("Total_Settlement_table.png",width = 17.5, height = 17.4,units = "cm")
dev.off()
### Site


# Define subset of data to use

tab2 <- df2 %>%
  group_by(year, site, spp) %>%
  summarise(sampsize = sum(count)) %>%
  pivot_wider(names_from = "year", values_from = "sampsize") %>%
  rowwise() %>%
  mutate(Species_total = round(sum(c_across("2012":"2023"), na.rm=T))) %>%
  ungroup() 


temp3 <- tab2 %>%
  filter(site %in% c("CF", "OR")) %>%
  mutate(site = fct_relevel(site, "CF", "OR")) %>%
  mutate(spp = fct_relevel(spp, "Bocaccio", "Cabezon", "Kelp Clingfish", "Kelp Greenling", "Northern Clingfish", "OYTB", "QGBC", "Snailfish", "SR", "Tiger", "unID", "Total")) %>%
  arrange(site, spp)

temp4 <- tab2 %>%
  filter(site %in% c("HH", "RR")) %>%
  mutate(site = fct_relevel(site, "HH", "RR")) %>%
  mutate(spp = fct_relevel(spp, "Bocaccio", "Cabezon", "Kelp Clingfish", "Kelp Greenling", "Northern Clingfish", "OYTB", "QGBC", "Snailfish", "SR", "Tiger", "unID", "Total")) %>%
  arrange(site, spp)

table3 <- rbind(temp3, temp4) %>% # here the rows are in the correct order for the table
  ungroup() %>%
  dplyr::select(-site)

t3 <- table3 %>%
  kbl(caption = "Table 3. Number of settlement-stage fishes collected between 2012-2023 by site (OYTB = olive, yellowtail, black rockfishes; QGBC = quillback, gopher, black and yellow, china, copper rockfishes; SR = splitnose, redbanded rockfishes).", col.names = c(" ", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019","2021", "2022", "2023", "Species Total")) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  pack_rows(index = c("Cape Foulweather Comparison Area" = 12, "Otter Rock Marine Reserve" = 12, "Humbug Mountain Comparison Area" = 12, "Redfish Rocks Marine Reserve" = 12)) %>%
  add_header_above(c(" " = 1, "Year" = 11, " " = 1)) %>%
  kable_styling(font_size=12) %>%
  row_spec(c(12,24, 36, 48), bold = TRUE) %>%
  scroll_box(height = "600px")

t3


### Overall
## Doughnut charts like Fish Recruitment pamphlet

# All regions, sites, years: Define a new subset to use
df2$spp <- as.factor(df2$spp)
df2$year <- as.factor(df2$year)

donut <- df2 %>%
  filter(spp != "Total" & spp != "unID" & spp != "Kelp Clingfish" & spp != "Bocaccio" & year != "2012" & year != "2013") %>% # Kelp cling and bocac < 1%
  group_by(spp) %>%
  summarise(count2 = sum(count)) %>%
  arrange(desc(count2))


# Compute percentages
donut$fraction = donut$count2 / sum(donut$count2)

# Compute a good label
donut$label <- paste0((round((donut$fraction)*100)), "%")

# Label Positions
donut$labpos <- cumsum(donut$fraction) - donut$fraction/2

# Make plot
mycols <- c('lightseagreen', 'antiquewhite4' ,'gray20', 'darkgoldenrod', 'darkslategrey', 'cadetblue', 'lightgoldenrod', 'darkseagreen3', 'lightcyan3', 'gray40','red4', 'pink')
names(mycols) = donut$spp
print(mycols)

# First as a stacked bar
plot <- donut %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  ggplot(aes(x = "", y = fraction, fill = spp)) +
  geom_bar(stat = "identity") +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  scale_fill_manual(values = mycols) +
  geom_label(aes(label = label, y = labpos),color = "white", size = 5, fontface = 'bold', show.legend = FALSE, nudge_x = .5)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  theme_void()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot
ggsave(filename = "percentages_stacked_bar.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

# Then a pie chart
p <- donut %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  ggplot(aes(x = "", y = fraction, fill = spp)) +
  scale_fill_manual(values = mycols) +
  labs(fill = "Species") +
  geom_bar(stat = "identity", color = "white") +
  theme(axis.ticks = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
  geom_label_repel(aes(label = label, y = labpos),color = "white", size = 5, fontface = 'bold', show.legend = FALSE, nudge_x = 0.5) + #the nudge puts the label outside the pie chart
  coord_polar("y", start=0)+
  theme(axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid  = element_blank())+
  theme_void()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
p
ggsave(filename = "percentages_pie.png",width = 17.5, height = 17.4,units = "cm")
dev.off()
# All Years Donut
all_years_donut <- donut %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  ggplot(aes(y = fraction, fill = spp)) +
  geom_bar(aes(x = 2), stat = "identity", color = "white") +
  geom_label_repel(aes(x = 2, label = label, y = labpos), color = "white", size = 5, fontface = 'bold', show.legend = FALSE) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  annotate(geom = "text", x=0.5, y=0, label = "Overall", size = 12, color = "grey") +
  scale_fill_manual(values = mycols) +
  labs(fill = "Species") +
  theme_void() +
  theme(text = element_text(size=20))
all_years_donut
ggsave(filename = "percentages_donut.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

### Year
annual.bar <- df2 %>%
  mutate(year = as.factor(year)) %>%
  filter(spp != "Total" & spp != "unID") %>% 
  group_by(year, spp) %>%
  summarise(count2 = sum(count)) %>%
  mutate(fraction = count2/sum(count2)) %>%
  arrange(desc(fraction)) %>%
  filter(fraction > 0.005) %>%
  mutate(label = paste0((round(fraction*100)), "%")) %>%
  mutate(labpos = cumsum(fraction)-fraction/2) %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  
  ggplot(aes(y = fraction, x = year, fill = spp)) + 
  geom_bar(stat = "identity", color = "white") +
  ylab("Proportion of Total Catch") +
  xlab("Year") +
  labs(fill = "Species") +
  theme_classic() +
  theme(text = element_text(size=14)) +
  scale_fill_manual(values = mycols) 

annual.bar
ggsave(filename = "percentages_annual_bar.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

### Region: Central and South
cs.bar <- df2 %>%
  mutate(year = as.factor(year)) %>%
  filter(spp != "Total" & spp != "unID") %>% # remove spp that are less than 1%
  group_by(year, region, spp) %>%
  summarise(count2 = sum(count)) %>%
  mutate(fraction = count2/sum(count2)) %>%
  arrange(desc(fraction)) %>%
  filter(fraction > 0.005) %>%
  mutate(label = paste0((round(fraction*100)), "%")) %>%
  mutate(labpos = cumsum(fraction)-fraction/2) %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  
  ggplot(aes(y = fraction, x = year, fill = spp)) + 
  geom_bar(stat = "identity", color = "white") +
  facet_wrap(~region) +
  ylab("Proportion of Total Catch") +
  xlab("Year") +
  labs(fill = "Species") +
  theme_classic() +
  theme(text = element_text(size=10)) +
  scale_fill_manual(values = c(mycols)) 
cs.bar
ggsave(filename = "percentages_stacked_NvS.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

### Site: CF/OR

CFOR.bar <- df2 %>%
  mutate(year = as.factor(year)) %>%
  filter(site %in% c("CF", "OR"), spp != "Total" & spp != "unID") %>% # remove spp that are less than 1%
  group_by(year, site, spp) %>%
  summarise(count2 = sum(count)) %>%
  mutate(fraction = count2/sum(count2)) %>%
  arrange(desc(fraction)) %>%
  filter(fraction > 0.005) %>%
  mutate(label = paste0((round(fraction*100)), "%")) %>%
  mutate(labpos = cumsum(fraction)-fraction/2) %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%

  ggplot(aes(y = fraction, x = year, fill = spp)) + 
  geom_bar(stat = "identity", color = "white") +
  facet_wrap(~site) +
  ylab("Proportion of Total Catch") +
  xlab("Year") +
  labs(fill = "Species") +
  theme_classic() +
  theme(text = element_text(size=10)) +
  scale_fill_manual(values = c(mycols))
CFOR.bar
ggsave(filename = "percentages_stacked_CFvOR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()


### Site: HH/RR
HHRR.bar <- df2 %>%
  mutate(year = as.factor(year)) %>%
  filter(site %in% c("HH", "RR"), spp != "Total" & spp != "unID") %>% # remove spp that are less than 1%
  group_by(year, site, spp) %>%
  summarise(count2 = sum(count)) %>%
  mutate(fraction = count2/sum(count2)) %>%
  arrange(desc(fraction)) %>%
  filter(fraction > 0.005) %>%
  mutate(label = paste0((round(fraction*100)), "%")) %>%
  mutate(labpos = cumsum(fraction)-fraction/2) %>%
  mutate(spp = fct_reorder(spp, fraction)) %>%
  
  ggplot(aes(y = fraction, x = year, fill = spp)) + 
  geom_bar(stat = "identity", color = "white") +
  facet_wrap(~site) +
  ylab("Proportion of Total Catch") +
  xlab("Year") +
  labs(fill = "Species") +
  theme_classic() +
  theme(text = element_text(size=10)) +
  scale_fill_manual(values = c(mycols)) 
HHRR.bar
ggsave(filename = "percentages_stacked_HHvsRR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

# Settlement Patterns
# Plot differences between marine reserves and comparison areas

# Because we care about differences between marine reserves and we want to ignore annual effects,
# we have to normalize settlement per year (See Ottmann et al 2018).

dat <- filter(df, spp != "Total")

# Get annual mean recruitment:
temp <- dat %>%
  group_by(year, spp) %>%
  summarise(annual_mean_settlement_rate = mean(rate))

# Add it to df in a new data frame:
temp_dat <- left_join(dat, temp, by = c("year", "spp"))

# Normalize settlement rate per annual meanrate:
temp_dat <- temp_dat %>%
  mutate(norm_settlement_rate = rate / annual_mean_settlement_rate) %>%
  group_by(region, spp, mr) %>%
  summarise(norm_mean_settlement_rate = mean(norm_settlement_rate),
            se = sqrt(sd(norm_settlement_rate) / n()))

# Plot:

label.df1 <- data.frame(spp = c("OYTB"),
                        norm_mean_settlement_rate = c(1.25),
                        mr = "TRUE")

label.df2 <- data.frame(spp = c("SR"),
                        norm_mean_settlement_rate = c(1.75),
                        mr = "TRUE")

label.df3 <- data.frame(spp= c("Tiger"),
                        norm_mean_settlement_rate = c(1.0),
                        mr = "TRUE")

p <-   ggplot(data = filter(temp_dat, region == "Central"), aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, size = 1, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("lightgrey", "darkslategrey"),labels = c("Cape Foulweather Comparison Area", "Otter Rock Marine Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=14))
p <-p + geom_text(data = label.df1, label = "*", size = 8) +
  geom_text(data = label.df2, label = "***", size = 8)
p
ggsave(filename = "percentages_bar_CFvsOR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()


q <-   ggplot(data = filter(temp_dat, region == "South"), aes(x = spp, y = norm_mean_settlement_rate, fill = mr)) +
  geom_bar(colour = "black", stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(aes(ymin = (norm_mean_settlement_rate - se), ymax = (norm_mean_settlement_rate + se)),
                width = .4, size = 1, stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("cadetblue", "royalblue4"),labels = c("Humbug Mountain Comparison Area", "Redfish Rocks Marine Reserve"))+
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=14))

q + geom_text(data = label.df3, label = "***", size = 8)
ggsave(filename = "percentages_bar_HHvsRR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()
## Like ODFW MR Shiny Line figures


##SMEL AND SFLA Settlement rates
# Define a new subset to use
# Central Sites Only- SMEL 
c_set_spp_yr <- df %>% #can change this to df2 if you want to see all spp
  filter(region == "Central") %>%
  filter(alpha != "SMEL") %>%
  group_by(year, alpha, site) %>%
  summarise(N = n(), #counts number of observations in the group after group_by()
            mean.set = mean(rate),
            sd.set = sd(rate),
            se.set = sd(rate)/sqrt(N))


# Plot
p1 <- ggplot(c_set_spp_yr, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~spp, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012, 2023,2))+
  theme_classic()+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=20))

p1 <- p1 + scale_color_manual(values = c("lightgrey", "darkslategrey"),labels = c("Cape Foulweather Comparison Area", "Otter Rock Marine Reserve"))

p1
ggsave(filename = "SMEL_MeanSettlementRate_CFvsOR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

#For southern sites only
s_set_spp_yr <- df %>% #can change this to df2 if you want to see all spp
  filter(region == "South") %>%
  filter(alpha != "SMEL") %>%
  group_by(year, alpha, site) %>%
  summarise(N = n(), #counts number of observations in the group after group_by()
            mean.set = mean(rate),
            sd.set = sd(rate),
            se.set = sd(rate)/sqrt(N))

#Plot it
p2 <- ggplot(s_set_spp_yr, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~spp, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012, 2023,2))+
  theme_classic()+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=20))

p2 <- p2 + scale_color_manual(values = c("lightseagreen", "royalblue4"),labels = c("Humbug Mountain", "Redfish Rocks Marine Reserve"))

p2
ggsave(filename = "SMEL_MeanSettlementRate_HHvsRR.png",width = 17.5, height = 17.4,units = "cm")
dev.off()

# Plot
tots.smel <- rbind(s_set_spp_yr, c_set_spp_yr)
tots.smel <- ggplot(tots.smel, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~ region, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  guides(fill=guide_legend(title=NULL)) +
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012,2023,1))+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=18))

tots.smel <- tots.smel + scale_color_manual(values = c("lightseagreen", "royalblue4", "lightgrey", "darkslategrey"),labels = c("Humbug Mountain Comparison Area", "Redfish Rocks Marine Reserve", "Cape Foulweather", "Otter Rock Marine Reserve"))

tots.smel

ggsave("Tots_SMEL_settlement_03.29.24.1.png", tots.smel)


#####PLOT FOR SFLA ONLY 3.29.24
#North sites
c_sfla_set_spp_yr <- df %>% #can change this to df2 if you want to see all spp
  filter(region == "Central") %>%
  filter(alpha != "SFLA") %>%
  group_by(year, alpha, site) %>%
  summarise(N = n(), #counts number of observations in the group after group_by()
            mean.set = mean(rate),
            sd.set = sd(rate),
            se.set = sd(rate)/sqrt(N))
# Plot
p1.sfla <- ggplot(c_sfla_set_spp_yr, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~region, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  guides(fill=guide_legend(title=NULL)) +
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012,2023,2))+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=20))

p1.sfla <- p1.sfla + scale_color_manual(values = c("lightgrey", "darkslategrey"),labels = c("Cape Foulweather", "Otter Rock Marine Reserve"))

p1.sfla

ggsave("North_SFLA_settlement_03.29.24.png", p1.sfla)

#Southern sites- SFLA
s_sfla_set_spp_yr <- df %>% #can change this to df2 if you want to see all spp
  filter(region == "South") %>%
  filter(alpha != "SFLA") %>%
  group_by(year, alpha, site) %>%
  summarise(N = n(), #counts number of observations in the group after group_by()
            mean.set = mean(rate),
            sd.set = sd(rate),
            se.set = sd(rate)/sqrt(N))
# Plot
p2.sfla <- ggplot(s_sfla_set_spp_yr, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~region, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  guides(fill=guide_legend(title=NULL)) +
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012,2023,2))+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=20))

p2.sfla <- p2.sfla + scale_color_manual(values = c("lightseagreen", "royalblue4"),labels = c("Humbug Mountain", "Redfish Rocks Marine Reserve"))

p2.sfla
ggsave("South_SFLA_settlement_04.1.24.png", p1.sfla)
dev.off()

#BOTH SITES
# Plot
tots.sfla <- rbind(s_sfla_set_spp_yr, c_sfla_set_spp_yr)
tots.sfla <- ggplot(tots.sfla, aes(x = year, y = mean.set, colour = site))+
  #facet_rep_wrap(~ region, scales = "fixed", repeat.tick.labels = "x") +
  geom_line(size = 1.1, position = position_dodge(.4))+
  geom_point(position = position_dodge(width = .4))+
  geom_errorbar(aes(ymin = mean.set - se.set, ymax = mean.set + se.set), width = 0, size = 1, position = position_dodge(.4))+
  guides(fill=guide_legend(title=NULL)) +
  ylab('Mean settlement rate (No. fish/SMURF/day)')+
  xlab('')+
  labs(colour = '')+
  scale_x_continuous(breaks = seq(2012,2023,1))+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = c(.7,.95),
        legend.background = element_blank(),
        legend.text=element_text(size=20), 
        text = element_text(size=20),
        axis.text=element_text(size=18))

tots.sfla <- tots.sfla + scale_color_manual(values = c("lightseagreen", "royalblue4", "lightgrey", "darkslategrey"),labels = c("Humbug Mountain Comparison Area", "Redfish Rocks Marine Reserve", "Cape Foulweather", "Otter Rock Marine Reserve"))

tots.sfla
ggsave("tots_SFLA_settlement_03.29.24.png", p1.sfla)

# Plot differences for Cabezon, OYTB, QGBC, SR, Tiger among years
#Settlement plot
# Here we do care about annual differences, so we do not need to normalize anything

# Create the data frame:
dat2 <- dat %>%
  group_by(region, year, spp) %>%
  summarise(mean_settlement_rate = mean(rate),
            se = sqrt(sd(rate) / n()))

# Create placeholders for south 2012, 2013, 2020, 20221 when we did not sample so that both facets of figure are comprable
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

ph11 <- data.frame("South", 2020, "Cabezon", 0, 0); colnames(ph11) <- c("region","year","spp","mean_settlement_rate","se")
ph12 <- data.frame("South", 2020, "SR", 0, 0); colnames(ph12) <- c("region","year","spp","mean_settlement_rate","se")
ph13 <- data.frame("South", 2020, "QGBC", 0, 0); colnames(ph13) <- c("region","year","spp","mean_settlement_rate","se")
ph14 <- data.frame("South", 2020, "Tiger", 0, 0); colnames(ph14) <- c("region","year","spp","mean_settlement_rate","se")
ph15 <- data.frame("South", 2020, "OYTB", 0, 0); colnames(ph15) <- c("region","year","spp","mean_settlement_rate","se")

ph16 <- data.frame("South", 2021, "Cabezon", 0, 0); colnames(ph16) <- c("region","year","spp","mean_settlement_rate","se")
ph17 <- data.frame("South", 2021, "SR", 0, 0); colnames(ph17) <- c("region","year","spp","mean_settlement_rate","se")
ph18 <- data.frame("South", 2021, "QGBC", 0, 0); colnames(ph18) <- c("region","year","spp","mean_settlement_rate","se")
ph19 <- data.frame("South", 2021, "Tiger", 0, 0); colnames(ph19) <- c("region","year","spp","mean_settlement_rate","se")
ph20 <- data.frame("South", 2021, "OYTB", 0, 0); colnames(ph20) <- c("region","year","spp","mean_settlement_rate","se")

ph21 <- data.frame("Central", 2020, "Cabezon", 0, 0); colnames(ph21) <- c("region","year","spp","mean_settlement_rate","se")
ph22 <- data.frame("Central", 2020, "SR", 0, 0); colnames(ph22) <- c("region","year","spp","mean_settlement_rate","se")
ph23 <- data.frame("Central", 2020, "QGBC", 0, 0); colnames(ph23) <- c("region","year","spp","mean_settlement_rate","se")
ph24 <- data.frame("Central", 2020, "Tiger", 0, 0); colnames(ph24) <- c("region","year","spp","mean_settlement_rate","se")
ph25 <- data.frame("Central", 2020, "OYTB", 0, 0); colnames(ph25) <- c("region","year","spp","mean_settlement_rate","se")

df3 <- rbind(dat2, ph1, ph2, ph3, ph4, ph5, ph6, ph7, ph8, ph9, ph10, ph11, ph12, ph13, ph14, ph15, ph16, ph17, ph18, ph19, ph20, ph21, ph22, ph23, ph24, ph25)

# Plot both regions in a figure:
#df3$spp<- as.factor(df3$spp)
p <- ggplot(data = df3, aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge2")+
  #geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
   #             width = .2, size =.2, stat = "identity", position = position_dodge(width = .9)) +
  #  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  #facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))
p + scale_fill_viridis_d()
ggsave(filename = "Settlementpatterns_allyears.png",width = 30, height = 17,units = "cm")
dev.off()

#without regional differences -not currently working (4.2.24)
df3$spp<- as.factor(df3$spp)
p <- ggplot(data = df3, aes(x = spp, y = mean_settlement_rate, fill = as.factor(year))) +
  geom_bar(colour = "black", stat = "identity", position = "dodge")+
  #geom_errorbar(aes(ymin = (mean_settlement_rate - se), ymax = (mean_settlement_rate + se)),
   #            width = .2, size =.2, stat = "identity", position = position_dodge2(.5)) +
  #  scale_fill_manual(values = c("steelblue3", "seagreen2"),labels = c("Non-reserve", "Reserve"))+
  #facet_grid(region ~.) +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Species group") +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=20))
p + scale_fill_viridis_d()
ggsave(filename = "Settlementpatterns_allyears.png",width = 30, height = 17,units = "cm")
dev.off()

# Pairwise contrasts calculated by emmeans then exported to csv

comptab <- read.csv("annual_pairwise_comparisions_spp_region.csv", header = TRUE)
colnames(comptab)[1] <- "Region"

kbl(comptab, caption = "Table 4. Annual pairwise comparisions by species or species complex and region. A black dot indicates p < 0.05, ** indicates p < 0.01, and *** indicates p < 0.001. Species complexes include OYTB (olive, yellowtail, black rockfishes), QGBC (quillback, gopher, black and yellow, china rockfishes), and SR (splitnose, redbanded rockfishes)." ) %>%
  kable_styling(full_width = F) %>%
  scroll_box(height = "600px")

# Wrangle data a different way

dat <- settlement %>%
  mutate(alpha = case_when(alpha == "SDIP" ~ "SR",
                         alpha == "SMAR" ~ "Cabezon",
                         alpha == "SNIG" ~ "Tiger",
                         T ~ alpha)) %>%
  filter(year != 2011,
         alpha == "Cabezon" & julian %in% c(121:255) | #June 1 - Sept 15; 121 is may1
         alpha == "OYTB" & julian %in% c(121:255) |
         alpha  == "QGBC" &   julian %in% c(121:255) |
         alpha == "SR"  & julian %in% c(121:255) |
         alpha == "Tiger"  & julian %in% c(121:255))


# Seasonal trends: (Code from Dani Ottmann 2021)

# Plot it out:  Ottmann et al (2018) used GAM models (and plotted the smooth with method = "GAM").
# For simplicity, here we plot it with the default method = "loess".
# There is very little difference in the final figures.

# Plot it out:
p <- ggplot() +
  geom_smooth(data = dat, aes(x = julian, y = rate, color = as.factor(year), fill = as.factor(year)), alpha = .15, size = .6) +
  geom_point(data = dat, aes(x = julian, y = rate, color = as.factor(year))) +   # If we add points the upper y-limt increases
  facet_grid(alpha~region, scales = "free") +
  ylab("Settlement rate (fish/SMURF/day)") +
  xlab("Day of the year") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_classic() +
  theme(legend.title = element_blank(), text = element_text(size=16))

p + scale_color_viridis_d() + scale_fill_viridis_d()
p
ggsave("PLSR.png", p)

fish<-read.csv("SMURF_PLSR_cab_r.csv", header = TRUE)

colnames(fish)[1] <- "year"

fish2 <- dplyr::mutate_all(fish, function(x) as.numeric(as.character(x)))

# Take care of missing values using missMDA package
nas <- estim_ncpPCA(fish2, ncp.min = 0, ncp.max = 5, method.cv = "Kfold")
nas

impute <- imputePCA(fish2, ncp = 2)
fish.complete <- impute$completeObs
colnames(fish.complete) <- c("year", "PDO winter", "PDO spring", "ONI Winter", "ONI Spring", "Nearshore Ichthyo", "S. Copepod", "N. Copepod", "BEUTI", "CUTI", "SST Winter", "SST Spring", "STI", "Avg Settlement Rate", "Peak Settlement Day", "Max Settlement Rate", "Avg Settlement Length")

fish.mod <- plsreg1(fish.complete[,c(2,4,6,7,8,9,10,11,13)], fish.complete[,14], comps = 3, crosval = F)


plsr_regcoefs_weights <- read.csv("PLSR_cabezon_central_weights_regcoefs.csv", header = TRUE)
plsr_regcoefs_weights <- plsr_regcoefs_weights[-c(10:19),]
plsr_regcoefs_weights <- plsr_regcoefs_weights %>%
  mutate_if(is.numeric, round, digits=3)

kbl(plsr_regcoefs_weights, caption = "Table 6. Regression coefficients and predictor variable weights (contribution of each predictor to each PLSR component) ", col.names = c("Variable", "PLS Regression Coefficients", "C1 Weight2", "C2 Weight2", "C3 Weight2")) %>%
  kable_styling(full_width = F) 

### Cumulative variance explained
plsr_cumulativevarianceexplained <- read.csv("PLSR_cabezon_central_cumulativevarianceexplained.csv", header = TRUE)
colnames(plsr_cumulativevarianceexplained)[1] <- "Variable"
plsr_cumulativevarianceexplained <- plsr_cumulativevarianceexplained %>%
  mutate_if(is.numeric, round, digits=3)


kbl(plsr_cumulativevarianceexplained, caption = "Table 7. Cumulative variance in predictor and response variables explained (R2) by each component in the PLSR") %>%
  kable_styling(full_width = F) 

