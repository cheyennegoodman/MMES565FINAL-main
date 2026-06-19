####################################################################################################################################################### TCRMP Algal Heights From 2013-2026
# Owner: Lila Goodman
# Created On: 10-06-2026
# Last Edit: 10-06-2026
###########################################################################

## Libraries ##
library(tidyverse)
library(readxl)
library(ggplot2)
####################

# GOAL: 
  # Get mean cover of algae (overall and species) from 2013-2026
  # Get mean height of algae (overall and species) from 2013-2026
  # Make maps of algae cover
  # Make maps of algae height 

################## ALGAL HEIGHT #################################
ALGAE_METADATA <- read.csv("TCRMP_datasets/TCRMP_ALGAE/JAN2022/TCRMP_Master_Algae_Heights_Jan2022.xlsx - Metadata.csv")

ALGAE_RAW <- read.csv("TCRMP_datasets/TCRMP_ALGAE/MAY2026/TCRMP_Master_Algae_Heights_May2026_external.csv")
# Each row is the site, Sample date, Sample year, Sample Month, Period, Recorder, and Transect of one algae individual and its height

ALGAE_RAW[is.na(ALGAE_RAW)] <- 0 #replace NAs with 0 

ALGAE_CLEAN <- subset(ALGAE_RAW, select = -c(SampleDate, SampleMonth, Period, Recorder, Notes, X, X.1, X.2)) %>%  # removing unneccessary columns
  mutate(Location = toupper(Location)) %>% 
  dplyr::filter(!dplyr::between(as.numeric (SampleYear),2012, 2012)) %>% 
  mutate( Location = toupper(Location))
  #filter data to 2013-2022 
  
ANNUAL_ALGAE <- ALGAE_CLEAN %>% 
  group_by(Location, SampleYear) %>% 
  summarise(
  Transect_Height = (sum(Height))/6) #dividing by 6 because there are 6 transects
#### Cool, this means there is X amount of algae individuals at each site per year

OVERALL_ALGAE_COVER <- ANNUAL_ALGAE %>% 
  group_by(Location) %>% 
  summarise(
    Location_Abundance = mean(Transect_Height)
  ) # getting the mean abundance of algae in each location from 2013-2022

################################################################################################################################################################################# MAP STUFF #####################################

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") %>% 
  mutate( Location = toupper(Location))# TCRMP FISH data
 # TCRMP FISH data

MAP_ANNUAL_ALGAE <- left_join(ANNUAL_ALGAE, TCRMP_SITE[, c("Location", "Latitude", "Longitude", "Island" )], by = "Location") # MAP mean algal heights from 2013-2026

MAP_OVERALL_ALGAE <- left_join(OVERALL_ALGAE_COVER, TCRMP_SITE[, c("Location", "Latitude", "Longitude", "Island" )], by = "Location") #MAP for Mean algal heights of 2013-2026

## saving these datasets for map creation ##

write.csv(ANNUAL_ALGAE, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Annual_Algal_Heights.csv"), row.names = FALSE)
  
write.csv(MAP_OVERALL_ALGAE, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Mean_Algal_Heights.csv"), row.names = FALSE)
  
############################################################################################################################################################################## ALGAE BY SPECIES ####################################### ######### GOAL:
# Look at algae 













#########################################################################################################################################################
  