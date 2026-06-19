####################################################################################################################################################
# TCRMP FISH Damselfish Abundance From 2013-2025
# Owner: Lila Goodman
# Created On: 10-11-2025
# Last Edit: 18-06-2026
##########################################################################

####Libraries########
library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(MASS)
#GOAL: Average Stegates adustus abundance every TCRMP site from 2013-2025

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data


TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset


TABUND_SA <- dplyr::filter(CLEAN_TCRMP_FISH, ScientificName %in% c(
  "Stegastes adustus")) # filtering dusky species ONLY

#######################################################################################################################################################################  STEGATES ADUSTUS ACROSS USVI ###############################

TUSVI_TAMSA <- TABUND_SA %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Island = Island ,
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TUSVI_ABUNDANCE_SA <- TUSVI_TAMSA  %>% # Mean of damselfish abundance along each site per year and Island
  group_by(Island, SampleYear)  %>% 
  summarise(
    Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TUSVIABUNDANCE_SA_LPLOT <- ggplot(TUSVI_ABUNDANCE_SA, aes(x = SampleYear , y = Abundance_Mean, group= Island, color = Island)) +
  geom_line() +
  scale_color_manual(values = c(
    "STT" = "coral",
    "STJ"   = "green2",
    "STX"  = "cyan2")) +
  scale_x_continuous(breaks = seq(min(TUSVI_ABUNDANCE_SA$SampleYear), max(TUSVI_ABUNDANCE_SA$SampleYear), by = 1)) +
  labs(title = "Average Stegates adustus Abundance at TCRMP Sites in the USVI from 2013-2025") +
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. ?. Average stegates adustus abundance at TCRMP sites from 2013-2025. These sites are located on St. Thomas (STT), St. Croix (STX), and St. John (STJ).") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TUSVIABUNDANCE_SA_LPLOT)

################################################################################################################################################################## STEGATES ADUSTUS ABUNDANCE BY ISLAND ############################

################### ST. THOMAS ##################################

TSTT_ASA <- dplyr::filter(TABUND_SA, #Only including St. Thomas sites
                         Island %in% c(
                           "STT"
                         ))

TSTT_TAMSA <- TSTT_ASA %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTT_ABUNDANCE_SA <- TSTT_TAMSA  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTT_ABUNDANCE_SA_LPLOT <- ggplot(TSTT_ABUNDANCE_SA, aes(x = SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTT_ABUNDANCE_SA$SampleYear), max(TSTT_ABUNDANCE_SA$SampleYear), by = 1)) +
  labs(title = "Average Stegates adustus Abundance at TCRMP Sites on St. Thomas from 2013-2025") +
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. ?. Average Stegates adustus Abundance at TCRMP Sites on St. Thomas from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTT_ABUNDANCE_SA_LPLOT)

##################### St. Croix #####################

TSTX_ASA <- dplyr::filter(TABUND_SA, #Only including St. Thomas sites
                        Island %in% c(
                          "STX"
                        ))

TSTX_TAMA <- TSTX_ASA %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTX_ABUNDANCE_SA <- TSTX_TAMA  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTX_ABUNDANCE_SA_LPLOT <- ggplot(TSTX_ABUNDANCE_SA, aes(x = SampleYear , y = Location_Abundance_mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_ABUNDANCE_SA$SampleYear), max(TSTX_ABUNDANCE_SA$SampleYear), by = 1)) +
  labs(title = "Average Stegates adustus Abundance at TCRMP Sites on St. Croix from 2013-2025") +
  labs(x= "Year", y= "Stegates adustus Mean Abundance") +
  labs(caption = "Figure No. ?. Average Stegates adustus Abundance at TCRMP Sites on St. Croix from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTX_ABUNDANCE_SA_LPLOT)

############## St. John #################################

TSTJ_ASA <- dplyr::filter(TABUND_SA, #Only including St. Thomas sites
                        Island %in% c(
                          "STJ"
                        ))


TSTJ_TAMA <- TSTJ_ASA %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTJ_ABUNDANCE_SA <- TSTJ_TAMA  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTJ_ABUNDANCE_SA_LPLOT <- ggplot(TSTJ_ABUNDANCE_SA, aes(x = SampleYear , y = Location_Abundance_mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTJ_ABUNDANCE_SA$ SampleYear), max(TSTJ_ABUNDANCE_SA$ SampleYear), by = 1)) +
  labs(title = "Average Stegates adustus Abundance at TCRMP Sites on St. John from 2013-2025") +
  labs(x= "Year", y= "Stegates adustus Mean Abundance") +
  labs(caption = "Figure No. ?. Average Stegates adustus Abundance at TCRMP Sites on St. John from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTJ_ABUNDANCE_SA_LPLOT)

############################################################################################################################################################################################################################################################### STATS TIME #######################################

# Is there a significance difference of SA abundance across the islands in the USVI?

### ASUMPTIONS 

TUSVI_ASA_ANOVA <- aov(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SA)

#1.) Homoscedasticity
bartlett.test(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SA) 

# p-value = 6.765e-06, I reject the hypothesis. This dataset DOES NOT HAVE homogenity of variances

TUSVI_ABUNDANCE_SA$Abundance_Log <- log(TUSVI_ABUNDANCE_SA$Abundance_Mean) # log it 
#p-value = 0.4257, I reject to fail the hypothesis, the log version has homogenity of variances

TUSVI_ABUNDANCE_SA$Abundance_Log <- log(TUSVI_ABUNDANCE_SA$Abundance_Mean) # log it 

hist(TUSVI_ABUNDANCE_SA$Abundance_Log)
#it looks left skewed

#2.) Normality
shapiro.test(TUSVI_ABUNDANCE_SA$Abundance_Log)
# p-value = 0.01099 , DOES NOT PASS, USE CAUTION

TUSVI_ASA_ANOVA <- aov(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SA)
summary(TUSVI_ASA_ANOVA)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#Island       2 23.496  11.748   79.53 6.17e-14 ***
#Residuals   36  5.318   0.148   

# yes there is a significance difference of SA abundance across the islands in the USVI

################### POST HOC ###################

TukeyHSD(TUSVI_ASA_ANOVA)

TUSVI_Island_ANOVA<- emmeans(TUSVI_ASA_ANOVA, ~ Island)

TUSVI_ISLAND_POSTHOC <- cld(TUSVI_Island_ANOVA, Letter = letters)

TUSVI_ISLAND_POSTHOC$.group <- trimws(TUSVI_ISLAND_POSTHOC$.group)

TUSVI_ISLAND_POSTHOC$Post_Hoc <- min(TUSVI_ASA_ANOVA$Abundance_Log, na.rm = TRUE) * 1.1

TUSVI_ASA_BPLOT <- ggplot(TUSVI_ASA_ANOVA, aes(x = Island , y = Abundance_Log , fill = Island)) +
  geom_boxplot() +
  geom_text(data = TUSVI_ISLAND_POSTHOC,
            aes(x = Island,
                y = Post_Hoc,
                label = .group),
            vjust = 2.1 ,
            inherit.aes = FALSE,
            size = 6) +
  scale_fill_manual(values = c(
    "STT" = "coral",
    "STJ"   = "green2",
    "STX"  = "cyan2")) +
  labs(title = "Stegates adustus Mean Abundance Across TCRMP Sites From 2013-2025") +
  labs(caption = "Stegates adustus Mean Abundance on St. Thomas, St. Croix, and St. John. This data did not pass the Shapiro-Wilks test.") +
  labs(x = "Island" , y = "Stegates adustus Mean Abundance log(Abundance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## all these diddly darn fish have significantly different abundances across the USVI  


############################################################################
# Is there a significance difference of SA Abundance across sites?
############################################################################
################     ST. THOMAS   ##########################################

### NOTE : HIND BANK EAST FSA HAD TO BE REMOVED BECAUSE THERE WAS ONE OBSERVATION 
####
TSTT_ASA_CLEAN <- TSTT_ABUNDANCE_SA %>% 
  dplyr::filter(Location !=  "Hind Bank East FSA")

TSTT_ABUNDANCE_SA$Abundance_Log <- log(TSTT_ABUNDANCE_SA$Location_Abundance_Mean) # log it 

#1.) Homoscedasticity

bartlett.test(Abundance_Log ~ Location, data = TSTT_ASA_CLEAN) 

# p-value = p-value < 2.2e-16, I reject the hypothesis. This dataset DOES NOT HAVE homogenity of variances
hist(TSTT_ASA_CLEAN$Abundance_Log)
#fucky wucky

#2.) Normality
shapiro.test(TSTT_ASA_CLEAN$Abundance_Log)
# p-value = 0.0002108 , DOES NOT PASS, USE CAUTION
# this data is fucked


V1_TSTT_ASA_ANOVA <- aov(Abundance_Log ~ Location, data = TSTT_ABUNDANCE_SA)
##REMINDER THIS DOES NOT MEET ASSUMPTIONS
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Location     14  191.2  13.655   15.25 <2e-16 ***
#Residuals   133  119.1   0.895   

############################################################################
################### ST. CROIX #############################################

### THIS DATA DOES NOT MEET ASSUMPTIONS
TSTX_ABUNDANCE_SA$Abundance_Log <- log(TSTX_ABUNDANCE_SA$Location_Abundance_Mean) # log it 

bartlett.test(Abundance_Log ~ Location, data = TSTX_ABUNDANCE_SA) 
#p-value < 2.2e-16 reject hypothesis

shapiro.test(TSTX_ABUNDANCE_SA$Abundance_Log)
#p-value = 0.00362 reject hypothesis

V1_TSTX_ASA_ANOVA <- aov(Abundance_Log ~ Location, data = TSTX_ABUNDANCE_SA)
summary(V1_TSTX_ASA_ANOVA)
##REMINDER THIS DOES NOT MEET ASSUMPTIONS
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Location     13 198.38  15.260   29.25 <2e-16 ***
#Residuals   117  61.04   0.522   
################################################################################################  ST. JOHN   #############################################



