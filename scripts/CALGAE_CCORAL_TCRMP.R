#######################################################################################################################################################TCRMP Benthic Cover From 2013-2026
# Owner: Lila Goodman
# Created On: 10-06-2026
# Last Edit: 10-06-2026
###########################################################################

## Libraries ##
library(tidyverse)
library(readxl)
library(ggplot2)
###################


#GOALS:
  # Isolate macroalgal cover from 2013 - 2026
  # Create map of macroalgal cover from 2013-2026

  # Isolate coral cover from 2013-2026
  # Create map of coral cover from 2013-2026 

BENTHIC_RAW <- read.csv("TCRMP_datasets/TCRMP_BENTHIC_COVER/FALL2025/TCRMP_Master_BenthicCover_Fall2025_external.csv") # raw TCRMP Benthic dataset

BENTHIC_CLEAN <- subset( BENTHIC_RAW, select = -c(SampleMonth, Period, FilmDate, GorgCov, SpoCov, ZoanCov, OtherLiveCov, CalgCov, CyanCov, NonLiveCov, UnkCov)) %>% 
  mutate(Location = toupper(Location)) %>% 
  dplyr::filter(!dplyr::between(as.numeric(SampleYear), 2001,2012)) # filtering the years

###########################################################################################################################################################################  MACROALGAL COVER  ##################################

ANNUAL_MACRO <- BENTHIC_CLEAN %>% 
  group_by(Location, SampleYear) %>% 
            summarise(
              Transect_Macroalgae = ((sum(MacaCov)/6))) # divided by number of transect (6)
            
OVERALL_MACRO <- BENTHIC_CLEAN %>% 
  group_by(Location) %>% 
            summarise(
              Transect_Macroalage = ((mean(MacaCov)))) # 6 is the number of transects 


############################################################################################################################################################################## MAP MACRO ALGAE ##################################

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") %>% 
  mutate( Location = toupper(Location))# TCRMP FISH data

MAP_ANNUAL_MACRO <- left_join(ANNUAL_MACRO, TCRMP_SITE [, c("Location" , "Latitude" , "Longitude" , "Island")], by = "Location") #Yearly Macroalgae mean at TCRMP sites from 2013-2026

MAP_OVERALL_MACRO <- left_join(OVERALL_MACRO , TCRMP_SITE [, c("Location", "Latitude" , "Longitude" , "Island")], by = "Location") # Macroalgal Cover mean at TCRMP sites from 2013-2026


### recording data for maps #######

write.csv(MAP_ANNUAL_MACRO, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Annual_Macroalgal_Cover.csv"), row.names = FALSE)


write.csv(MAP_OVERALL_MACRO, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Overall_Macroalgal_Cover.csv"), row.names = FALSE)

################################################################################################################################################################################################################################################# CORAL COVER #################################

ANNUAL_CORAL_COVER <- BENTHIC_CLEAN %>% 
        group_by(Location, SampleYear) %>% 
        summarise(
          Coral_Cover = (sum(CoralCov)/6) # 6 transects per site 
        )

OVERALL_CORAL_COVER <- BENTHIC_CLEAN %>% 
      group_by( Location) %>% 
      summarise(
        Coral_Cover = (mean(CoralCov))
      )
  
################################################################################# MAP TIME ########################

MAP_ANNUAL_CORAL_COVER <- left_join(ANNUAL_CORAL_COVER , TCRMP_SITE [, c("Location", "Latitude" , "Longitude" , "Island")], by = "Location") # Annual Coral Cover at TCRMP sites from 2013-2026

  
MAP_CORAL_COVER <- left_join(OVERALL_CORAL_COVER , TCRMP_SITE [, c("Location", "Latitude" , "Longitude" , "Island")], by = "Location") # Coral Cover mean at TCRMP sites from 2013-2026

########### saving data #######################3

write.csv(MAP_ANNUAL_CORAL_COVER, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Annual_Coral_Cover.csv"), row.names = FALSE)

write.csv(MAP_CORAL_COVER, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Overall_Coral_Cover.csv"), row.names = FALSE)
  
  
  #######