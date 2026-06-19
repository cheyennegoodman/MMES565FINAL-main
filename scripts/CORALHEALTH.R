######################################
# CORAL ANALYSIS FINALLY WOOT WOOT SO HAPPY #############
####Libraries########
library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(MASS)

# GOAL:Clean up the coral data by looking at 2013-2025 of data
#look at corals with and without damselfish prescence and compare health (size,recent mortality, old mortality and interactions(?)) per SITE! 

#GOAL:To look at coral-damselfish interactions in the sense of:
# How many coral species are generally affected by damselfish? 
# What species of damselfish are interacting with corals? 
# How many times does damselfish species bitethe coral? 

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") #MERGING TCRMP SITE WITH CORAL

CORAL_INTERACTION_RAW <- read.csv("TCRMP_datasets/TCRMP_CORAL_HEALTH/DEC2025/TCRMP_coralhealth_interaction_master_May2002_Dec2025_external.csv") ## RAW TCRMP CORAL DATA

#each row describes a specific coral individual (colony_id), coral species, location, sample date, survey year period, method, category of interaction, code for coral interaction, the meaning of the interaction code, and extent of interaction. 

#I need to separate the data by site, per year. I want to look at different coral species and which ones have more damselfish presence. 
# I need year, site, transect, species, damselfish pres, and bites

unique(CORAL_INTERACTION_RAW$surveyyear)

CORAL_INTERACTION_CLEAN <- subset(CORAL_INTERACTION_RAW, select = -c(sampledate, method, period))  %>% 
dplyr::filter(!dplyr::between(as.numeric (as.character(surveyyear)), 2002, 2012)) #filter data to 2013-2025

DAMSONLY <- CORAL_INTERACTION_CLEAN  %>% 
  dplyr::filter(code_meaning %in% c( "Stegastes planifrons: not distinguished between Interaction, Predator, Predation" ,
                                 "Stegastes adustus: not distinguished between Interaction, Predator, Predation" ,
                                 "Damselfish spp.: not distinguished between Interaction, Predator, Predation" , 
                                 "Stegastes leucostictus: not distinguished between Interaction, Predator, Predation",
                                 "Stegastes diencaeus",
                                 "Stegastes variabilis",
                                 "Stegastes partitus",
                                 "Microspathodon chrysurus",
                                 "Stegastes adustus",
                                 "Damselfish spp",
                                 "Stegastes planifrons",
                                 "Stegastes leucostictus"))
  
T_USVI_DCI <- ggplot(DAMSONLY, aes(x = species, fill = code_meaning)) + # coral species affected by damselfish bites and what species of damselfish are biting
  geom_bar() +
  labs(title = "Overall Coral-Damselfish Interaction By Species in the USVI") +
  labs(fill = "Damselfish species") +
  labs(x = "Coral Species" , y = "Damselfish Interactions") +
  labs(caption = "Fig. No. ?. Damselfish species interactions on coral species at TCRMP sites from 2013-2025.") +
theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################################################################### Damselfish and coral bites 


TTCDI <- DAMSONLY %>% # average damselfish bites on each transect per site and year
  group_by(location, surveyyear, transect) %>% 
  summarise(
    Damselfish_Bites = (mean(extent)) ,
    Damselfish_Species = code_meaning ,
  .groups = "drop") %>% 
  unique()

TCDI <- TTCDI  %>% # Mean damselfish bites at each site per year
  group_by(location, surveyyear)  %>% 
  summarise(
    Damselfish_Species = Damselfish_Species,
    Damselfish_Bites = (sum(Damselfish_Bites)/6) , # There are 6 transects per site
    .groups = "drop") %>% 
    unique()

TUSVI_BDAMS <- TCDI %>%  # Mean damselfish bites per site 
  group_by(location,)  %>% 
  summarise(
    Damselfish_Species = Damselfish_Species,
    Damselfish_Bites = mean(Damselfish_Bites),
    .groups = "drop") %>% 
  unique()

TUSVI_BDAMS <- TUSVI_BDAMS %>% rename(Location = location)
  
CLEAN_TUSVI_BDAMS <- left_join(TUSVI_BDAMS, TCRMP_SITE[, c("Location", "Island" )], by = "Location") #attaching island names and depth to dataset

TSTT_BDAMS <- dplyr::filter(CLEAN_TUSVI_BDAMS, #Only including St. Thomas sites
                          Island %in% c(
                            "STT"
                          ))
TSTT_BDAMS_BPLOT <- ggplot(TSTT_BDAMS, aes(x = Location, y = Damselfish_Bites, fill = Damselfish_Species)) + # average damselfish bites per site
  geom_col(position = "dodge") +
  labs(title = "Average Damselfish Bites on Coral on St. Thomas") +
  labs(fill = "Damselfish species") +
  labs(x = "Coral Species" , y = "Damselfish Bites") +
  labs(caption = "Fig. No. ?. Average damselfish bites at TCRMP sites from 2013-2025.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(TSTT_BDAMS_BPLOT)
############################################################################## ST CROIX
TSTX_BDAMS <- dplyr::filter(CLEAN_TUSVI_BDAMS, #Only including St. Thomas sites
                            Island %in% c(
                              "STX"
                          ))

################################################################################# ST JOHN
TSTJ_BDAMS <- dplyr::filter(CLEAN_TUSVI_BDAMS, #Only including St. Thomas sites
                            Island %in% c(
                              "STJ"
                            ))

########################################################################################################################################################################################################################################
Coral_MAP<- subset(DAMSONLY, select = c(Location, SampleYear, Period, Transect, SPP, DamSpp)) %>%
  mutate(Location = toupper(Location)) 
Coral_MAP$DamPres <- 1
Coral_MAP2 <- subset(Coral1, select = c(Location, SampleYear, Period, Transect, SPP, DamSpp))  
# I need to create a code where the empties are 0 and any input in the damspp slot is a 1
# I am going to make a slot that is 1 for all the fish and then divide by 6 (6 transects per site)
TRANSECT_MEAN_CORAL_PRES <- Coral_MAP %>% # mean of coral damselfish interaction along each transect by year and location
  group_by(Location, Transect, SampleYear) %>%   summarise(
    Sum_Presence= sum(DamPres),
    Mean_Presence  = mean(DamPres) ,
    .groups = "drop") %>% 
  unique()

DAMCORAL_PRES <- TRANSECT_MEAN_CORAL_PRES  %>% # Mean of coral-damselfish presence along each site per year 
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Pres_mean = (sum(Mean_Presence))/6 #6 is the transect per site
  )

CORAL_DAM_INTERACT <- DAMCORAL_PRES  %>% # Mean of damselfish biomass along each site 
  group_by(Location)  %>% 
  summarise(
    Location_Pres_mean = (mean(Location_Pres_mean))
  )
## noise changes 
## macroalage percent cover 
## surface coral  map out lesions 
## linear regression number of damselfish and interaction
## arrange map by depth 
## conditions and captions on maps 
## 

MAP_DAMCORAL_INTERACT <- 
  merge(CORAL_DAM_INTERACT, TCRMP_SITE[, c ("Location" , "Latitude" , "Longitude", "Island") ] , by = "Location" )

################woot woot I got my data for my new map!! ####################
# time to save it 
write.csv(MAP_DAMCORAL_INTERACT, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Overall_Coral_Damselfish.csv"), row.names = FALSE)

##############################################################################################################################################################################################################################

### OVERALL CORAL DISTRIBUTION FROM 2013-2022 ###

## Goal: look at how coral cover has changed each year ## 

## I want percentage of the Coral species out of overall coral population. Then, I want to see within the coral species, how many were diseased/ bleached that year. I may want to dive into coral


CORAL<- subset(CORAL_HEALTH_CLEAN, select = -c(Dict, Lobo, Hali, DamSpp, Pred1, Pred1ID, Pred2, Pred2ID))  %>%  # adding a new column to count all individual corals
  mutate(
    CCount = 1 # coral count is CCount
  )

# Sum of all corals of each transect per year 
# example, There are 32 AA and 160 overal corals in the area 32/160

TRANSECT_SUM_CORAL<- CORAL %>%  
  group_by(Location, SampleYear, SPP)  %>%  
  summarise(
    Location_Coral_Mean = (sum(CCount))/6 , #average of coral species per transect 
    Location_Coral_Sum = (sum(CCount)) , 
    Coral_Total = 
  )

LOCATION_CORAL_COVER <- TRANSECT_SUM_CORAL %>%  
  group_by(Location, SampleYear)  %>%  
  summarise(
    Location_Coral_Cover = (Location_Coral_Sum/(sum(CCount))) , #average of coral species per transect 
    Location_Coral_Sum = Location_Coral_Sum ,
    Location_Coral_Mean = Location_Coral_Mean
  )
