####################################################################################################################################################
# TCRMP FISH Stegates Planifrons Density From 2013-2025
# Owner: Lila Goodman
# Created On: 10-11-2025
# Last Edit: 21-07-2026
##########################################################################

####Libraries########
library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(MASS)

#GOAL: average Stegates planifrons desnity at every TCRMP site from 2013-2025

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_SITE$Island[TCRMP_SITE$Island == "STT"] <- "St. Thomas"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STJ"] <- "St. John"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STX"] <- "St. Croix" # changed to proper names

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data

TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset


TDENS_SP <- dplyr::filter(CLEAN_TCRMP_FISH, ScientificName %in% c(
                            "Stegastes planifrons"
                          )) %>% 
  mutate(Damselfish_Density = ((SppTotal/100))) # DENSITY OF DAMSELFISH in 100M^2 area conversion

#######################################################################################################################################################################  STEGATES PLANIFRONS ACROSS USVI ##############################

TUSVI_TDMSP <- TDENS_SP %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Island = Island ,
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TUSVI_DENSITY_SP <- TUSVI_TDMSP  %>% # Mean of damselfish density along each site per year and Island
  group_by(Island, SampleYear)  %>% 
  summarise(
    Density_Mean = (sum(Mean_Density))/10 , #10 is the transect per site
    Location = Location
  )

TUSVI_DENSITY_SP_LPLOT <- ggplot(TUSVI_DENSITY_SP, aes(x = SampleYear , y = Density_Mean, group= Island, color = Island)) +
  geom_line() +
  scale_color_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John"   = "forestgreen",
    "St. Croix"  = "orange")) +
  scale_x_continuous(breaks = seq(min(TUSVI_DENSITY_SP$SampleYear), max(TUSVI_DENSITY_SP$SampleYear), by = 1)) +
  labs(title = "Mean Stegates planifrons Density at TCRMP Sites in the USVI from 2013-2025") +
  labs(x= "Year", y= "Stegates planifrons Mean Density (100m^2)") +
  labs(caption = "Figure No. ?. Mean stegates planifrons abundance at TCRMP sites from 2013-2025. These sites are located on St. Thomas, St. Croix, and St. John.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TUSVI_DENSITY_SP_LPLOT)


TUSVI_SITE_DSP <- TUSVI_DENSITY_SP %>% 
  group_by(Location) %>% 
  summarise(
    Island = Island ,
    MEAN_Density = mean(Density_Mean),
    SD_Density = sd((Density_Mean)),
    SEM_Density = sd((Density_Mean)) / sqrt(length((Density_Mean)))) %>%
  unique()

TUSVI_DSP <- TUSVI_SITE_DSP %>% 
  mutate(
    Island = factor(Island, levels = c("St. Thomas", "St. John", "St. Croix"))
  ) %>%
  arrange(Island, Location) %>%
  mutate(Location = factor(Location, levels = unique(Location)))

TUSVI_DSP$Location <- factor(
  TUSVI_DSP$Location,
  levels = TUSVI_DSP %>%
    arrange(Island, Location) %>%
    pull(Location) %>%
    unique()
) # organizing plot to be by island

TUSVI_SITE_DSP_BPLOT <- ggplot(TUSVI_DSP, aes(x = Location , y = MEAN_Density , fill = Island)) + 
  geom_col() + 
  scale_fill_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John" = "forestgreen",
    "St. Croix" = "orange"
  )) +
  geom_errorbar(aes(ymin = MEAN_Density-SEM_Density, ymax = MEAN_Density+SEM_Density), width = .9)  +
  labs(title = "Mean Stegates Planifrons Density at Territorial Coral Reef Monitoring (TCRMP) Fish Sites in the US Virgin Islands") +
  labs(y = "Stegates planifrons Mean Density(100m^2)")+
  labs(x = "TCRMP Sites") +
  labs(caption = "Figure No. . Mean Stegates planifrons density (100/m^2) and SEM of TCRMP sites preformed by fish surveys from 2013-2025. Sites are separated by island of St. Croix(orange) St. Thomas(blue), and St. John(green).") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1 , hjust = 1))


################################################################################################################################################################## STEGATES PLANIFRONS ABUNDANCE BY ISLAND ############################

################# ST. THOMAS ########################################
TSTT_DSP <- dplyr::filter(TDENS_SP, #Only including St. Thomas sites
                         Island %in% c(
                           "St. Thomas"
                         ))

TSTT_TDMSP <- TSTT_DSP %>% # mean of stegates planifrons density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTT_DENSITY_SP <- TSTT_TDMSP  %>% # Mean of stegates planifrons density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTT_DENSITY_SP_LPLOT <- ggplot(TSTT_DENSITY_SP, aes(x = SampleYear , y = Location_Density_Mean, group= Location, color = Location)) + # St. Thomas plot of Stegates planiforns from 2013-2025
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTT_DENSITY_SP$SampleYear), max(TSTT_DENSITY_SP$SampleYear), by = 1)) +labs(title = "Mean Stegates Planifrons Density at St. Thomas from 2013-2025")+
  labs(x= "Year", y= "Stegates planifrons Mean Density (100m^2)") +
  labs(caption = "Figure No. ?. Mean Stegates planifrons density at St. Thomas TCRMP sites from 2013-2025.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTT_DENSITY_SP_LPLOT)

################# ST. CROIX ############################

TSTX_DSP <- dplyr::filter(TDENS_SP, #Only including St. Croix sites
                        Island %in% c(
                          "St. Croix"
                        ))

TSTX_TDMSP <- TSTX_DSP %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTX_DENSITY_SP <- TSTX_TDMSP  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTX_DENSITY_SP_LPLOT <- ggplot(TSTX_DENSITY_SP, aes(x = SampleYear , y = Location_Density_Mean, group= Location, color = Location)) + # St. Croix plot of Stegates planifrons from 2013-2025
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_DENSITY_SP$SampleYear), max(TSTX_DENSITY_SP$SampleYear), by = 1)) +labs(title = "Mean Stegates Planifrons Density at St. Croix from 2013-2025")+
  labs(x= "Year", y= "Stegates planifrons Mean Density (100m^2)") +
  labs(caption = "Figure No. ?. Mean Stegates planifrons density at St. Croix TCRMP sites from 2013-2025.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#################### ST. JOHN ################################

TSTJ_DSP <- dplyr::filter(TDENS_SP, #Only including St. John sites
                           Island %in% c(
                             "St. John"
                           ))


TSTJ_TDMSP <- TSTJ_DSP %>% # mean of stegates planifrons density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTJ_DENSITY_SP <- TSTJ_TDMSP  %>% # Mean of stegates planifrons density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTJ_DENSITY_SP_LPLOT <- ggplot(TSTJ_DENSITY_SP, aes(x = SampleYear , y = Location_Density_Mean, group= Location, color = Location)) + # Plot of St. John sites and stegates planifrons density
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTJ_DENSITY_SP$SampleYear), max(TSTJ_DENSITY_SP$SampleYear), by = 1)) +
  labs(title = "Mean Stegates Planifrons Density at St. John from 2013-2025")+
  labs(x= "Year", y= "Stegates planifrons Mean Density (100/m^2)") +
  labs(caption = "Figure No. ?. Mean Stegates planifrons density at St. John TCRMP sites from 2013-2025.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

############################################################################################################################################################################################################################################################### STATS TIME #######################################

# Is there a significance difference of SP abundance across the islands in the USVI?

### ASUMPTIONS 

#1.) Homoscedasticity
bartlett.test(Abundance_Mean ~ Island, data = TUSVI_ABUNDANCE_SP) 

# p-value = 7.404e-05, I reject the hypothesis. This dataset DOES NOT HAVE homogenity of variances

TUSVI_ABUNDANCE_SP$Abundance_Log <- log(TUSVI_ABUNDANCE_SP$Abundance_Mean) # log it 
bartlett.test(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SP) 
#p-value = 0.4195, I reject to fail the hypothesis, the log version has homogenity of variances

hist(TUSVI_ABUNDANCE_SP$Abundance_Log)
#it looks bimodel?

#2.) Normality
shapiro.test(TUSVI_ABUNDANCE_SP$Abundance_Log)
#p-value = 0.01415 , DOES NOT PASS, USE CAUTION

TUSVI_ASP_ANOVA <- aov(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SP)
summary(TUSVI_ASP_ANOVA)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#Island       2 17.219   8.609   77.64 8.78e-14 ***
#Residuals   36  3.992   0.111   
# SP abundance is significantly different across the USVI

################### POST HOC ###################

TukeyHSD(TUSVI_ASP_ANOVA)

TUSVI_Island_ANOVA<- emmeans(TUSVI_ASP_ANOVA, ~ Island)

TUSVI_ISLAND_POSTHOC <- cld(TUSVI_Island_ANOVA, Letter = letters)

TUSVI_ISLAND_POSTHOC$.group <- trimws(TUSVI_ISLAND_POSTHOC$.group)

TUSVI_ISLAND_POSTHOC$Post_Hoc <- min(TUSVI_ASP_ANOVA$Abundance_Log, na.rm = TRUE) * 1.1

TUSVI_ASP_BPLOT <- ggplot(TUSVI_ASP_ANOVA, aes(x = Island , y = Abundance_Log , fill = Island)) +
  geom_boxplot() +
  geom_text(data = TUSVI_ISLAND_POSTHOC,
            aes(x = Island,
                y = Post_Hoc,
                label = .group),
            vjust = 2.1 ,
            inherit.aes = FALSE,
            size = 6) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_manual(values = c(
    "STT" = "coral",
    "STJ"   = "green2",
    "STX"  = "cyan2")) +
  labs(title = "Stegates planifrons Mean Abundance Across TCRMP Sites From 2013-2025") +
  labs(caption = "Stegates planifrons Mean Abundance on St. Thomas (STT), St. Croix (STX), and St. John(STJ). This data did not pass the Shapiro-Wilks test.") +
  labs(x = "Island" , y = "Stegates planifrons Mean Abundance log(Abundance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Stegates planifrons abundance at TCRMP sites is significantly different on St. Thomas in comparison to St. Croix or St. John.   
