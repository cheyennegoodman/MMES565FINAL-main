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

#GOAL: average damselfish abundance for every TCRMP site from 2013-2025

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data


TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset

TABUND_DAMSEL <- dplyr::filter(CLEAN_TCRMP_FISH,
                              ScientificName %in% c(
                                "Microspathodon chrysurus",
                                "Stegastes partitus",
                                "Stegastes variabilis",
                                "Stegastes adustus",
                                "Stegastes leucostictus",
                                "Stegastes planifrons"
                              )) # filter fish species to damselfish only
##########################################################################################################################################################
#####################  DAMSELFISH DENSITY       #############################

TTRANS_MEAN_ABUND_DAMSEL <- TABUND_DAMSEL %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Sum_Abundance= sum(SppTotal),
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TABUNDANCE_DAMSELFISH <- TTRANS_MEAN_ABUND_DAMSEL  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TMAP_ABUNDANCE_DAMSELFISH <- TABUNDANCE_DAMSELFISH  %>% # Mean of damselfish biomass along each site 
  group_by(Location)  %>% 
  summarise(
    Location_Abundance_mean = (mean(Location_Abundance_mean))
  )

########### MAP STUFF #################
# print(head(MAP_ABUND_DAMSELFISH)) 
#SITE_ABUND_DAMSELFISH <- 
# left_join(ABUNDANCE_DAMSELFISH, TCRMP_SITE[, c("Location", "Latitude", "Longitude", "Island" )], by = "Location")

#SITE_MAP_ABUND_DAMSELFISH <-  
# left_join(MAP_ABUND_DAMSELFISH, TCRMP_SITE[, c("Location", "Latitude", "Longitude", "Island" )], by = "Location")

#saved desired datasets for ArcGIS

#write.csv(SITE_ABUND_DAMSELFISH, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Annual_Damselfish_Abundance.csv"), row.names = FALSE)


#write.csv(SITE_MAP_ABUND_DAMSELFISH, file.path("C:/Users/Owner/OneDrive/Documents/GOODMAN_THESIS/RCODE/CREATED_DATASETS", "Average_Damselfish_Abundance.csv"), row.names = FALSE)
##############################################################################################################################################################

##################### St. Thomas ############################
STT_ADAMS <- dplyr::filter(TABUND_DAMSEL, #Only including St. Thomas sites
                          Island %in% c(
                             "STT"
                           ))

STT_TAMD <- STT_ADAMS %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTT_ABUNDANCE_DAMSELFISH <- STT_TAMD  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTT_ABUNDANCE_LPLOT <- ggplot(TSTT_ABUNDANCE_DAMSELFISH, aes(x = SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTT_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTT_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) +
  labs(title = "Damselfish Abundance at St. Thomas TCRMP Sites From 2013-2025") +
  labs(x = "Year", y= "Damselfish Mean Abundance") +
  labs(caption = "Figure No. 4. Average Damselfish Abundance of TCRMP Sites on St. Thomas, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################# St. Croix #####################

SX_ADAMS <- dplyr::filter(TABUND_DAMSEL, #Only including St. Thomas sites
                          Island %in% c(
                            "STX"
                          ))

STX_TAMD <- SX_ADAMS %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTX_ABUNDANCE_DAMSELFISH <- STX_TAMD  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTX_ABUNDANCE_LPLOT <- ggplot(TSTX_ABUNDANCE_DAMSELFISH, aes(x=SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTX_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) + # damselfish abundance from 2013-2025 on St Croix 
  labs(title = "Damselfish Abundance at St. Croix TCRMP Sites From 2013-2025") +
  labs(x= "Year", y= "Damselfish Mean Abundance") +
  labs(caption = "Figure No. 6. Average Damselfish Abundance at TCRMP Sites on St. Croix, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTX_ABUNDANCE_LPLOT)

############## ST. JOHN ######################

STJ_ADAMS <- dplyr::filter(TABUND_DAMSEL, #Only including St. Thomas sites
                           Island %in% c(
                             "STJ"
                          ))

TSTJ_TAMD <- STJ_ADAMS %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTJ_ABUNDANCE_DAMSELFISH <- TSTJ_TAMD  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTJ_ABUNDANCE_LPLOT <- ggplot(TSTJ_ABUNDANCE_DAMSELFISH, aes(x=SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) + 
  geom_line() + # damselfish abundance from 2013-2025 on St John
  scale_x_continuous(breaks = seq(min(TSTJ_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTJ_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) +
  labs(title = "Damselfish Abundance at St. John TCRMP Sites From 2013-2025") +
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. 5. Average Damselfish Abundance at TCRMP Sites St. John, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTJ_ABUNDANCE_LPLOT)

############################################################################################################################################################################# DAMSELFISH ABUNDANCE BY ISLAND #########################

################## St. Thomas #################

TSTT_ALL_ABUNDANCE_DAMSELFISH <- TSTT_ABUNDANCE_DAMSELFISH  %>% # Mean of damselfish abundance on St. Thomas from 2013-2025
  group_by(SampleYear)  %>% 
  summarise(
    Abundance_Mean = (mean(Location_Abundance_Mean))
  )

TSTT_ALL_ABUNDANCE_DAMSELFISH_LPLOT <- ggplot(TSTT_ALL_ABUNDANCE_DAMSELFISH, aes(x = SampleYear , y = Abundance_Mean)) + 
  geom_line() + # damselfish abundance from 2013-2025 on St Thomas 
  scale_x_continuous(breaks = seq(min(TSTT_ALL_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTT_ALL_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= " Damselfish Mean Abundance") +
  labs(caption = "Figure No. ?. St. Thomas Average Damselfish Abundance From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################ ST. CROIX ###################

TSTX_ALL_ABUNDANCE_DAMSELFISH <- TSTX_ABUNDANCE_DAMSELFISH  %>% # Mean of damselfish abundance on St. Croix from 2013-2025
  group_by(SampleYear)  %>% 
  summarise(
    Abundance_Mean = (mean(Location_Abundance_Mean))
  )

TSTX_ALL_ABUNDANCE_DAMSELFISH_LPLOT <- ggplot(TSTX_ALL_ABUNDANCE_DAMSELFISH, aes(x = SampleYear , y = Abundance_Mean)) + 
  geom_line() + # damselfish abundance from 2013-2025 on St Croix 
  scale_x_continuous(breaks = seq(min(TSTX_ALL_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTX_ALL_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. ?. St. Croix Average Damselfish Abundance From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################# ST. JOHN #########################

TSTJ_ALL_ABUNDANCE_DAMSELFISH <- TSTJ_ABUNDANCE_DAMSELFISH  %>% # Mean of damselfish abundance on St. John from 2013-2021
  group_by(SampleYear)  %>% 
  summarise(
    Abundance_Mean = (mean(Location_Abundance_Mean))
  )

STJ_ALL_ABUNDANCE_DAMSELFISH_LPLOT <- ggplot(TSTJ_ALL_ABUNDANCE_DAMSELFISH, aes(x = SampleYear , y = Abundance_Mean)) + 
  geom_line() + # damselfish abundance from 2013-2022 on St John 
  scale_x_continuous(breaks = seq(min(TSTJ_ALL_ABUNDANCE_DAMSELFISH$SampleYear), max(TSTJ_ALL_ABUNDANCE_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. ?. St. John Average Damselfish Abundance From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

######### MERGING THEM TOGETHER ############
##adding columns of site to each dataset

TSTT_ALL_ABUNDANCE_DAMSELFISH$Site <- "St. Thomas"
TSTX_ALL_ABUNDANCE_DAMSELFISH$Site <- "St. Croix"
TSTJ_ALL_ABUNDANCE_DAMSELFISH$Site <- "St. John"


TUSVI_ABUNDANCE <- bind_rows(TSTT_ALL_ABUNDANCE_DAMSELFISH ,
                            TSTX_ALL_ABUNDANCE_DAMSELFISH, 
                            TSTJ_ALL_ABUNDANCE_DAMSELFISH)

TUSVI_ABUNDANCE_LPLOT <- ggplot(TUSVI_ABUNDANCE, aes(x = SampleYear , y = Abundance_Mean, group = Site , color = Site)) + 
  geom_line() + # damselfish abundance from 2013-2025 in USVI
  scale_color_manual( values = c( 
    "St. Thomas" = "coral" ,
    "St. John" = "green3" , 
    "St. Croix" = "cyan2" 
  )) +
  scale_x_continuous(breaks = seq(min(TUSVI_ABUNDANCE$SampleYear), max(TUSVI_ABUNDANCE$SampleYear), by = 1)) + 
  labs(title = " Average Damselfish Abundance Across US Virgin Islands From 2013-2025") +
  labs(x= "Year", y= "Mean Abundance") +
  labs(caption = "Figure No. ?. Average Damselfish Abundance From 2013-2025 Using TCRMP Sites Across the USVI By Island") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##############################################################################################################################################################################################################################

# Lila Finally Gets To Statsitical parts 

# GOAL: 
#### Make sure data meets the assumptions for an anova
#### Preform an ANOVA to see if there is sig dif of damselfish abundance between islands and damselfish abundance between sites 

## QUESTION ##
# Does damselfish abundance (Dependent variable) change with site/ island (Independent variable)? 

#H0 The mean abundance of damselfish is the same across 3 islands from 2013-2025

#H0 The mean abundance of damselfish is different across the 3 islands from 2013-2025

############ASSUMPTIONS##############

#1.) Homoscedasticity
bartlett.test(Abundance_Mean ~ Site, data = TUSVI_ABUNDANCE) 
#p-value = 0.02 <0.05. The data does not meet the requirements for homogenity of residuals 
TUSVI_ABUNDANCE$Abundance_Log <- log(TUSVI_ABUNDANCE$Abundance_Mean) # log it 
bartlett.test(Abundance_Log ~ Site, data = TUSVI_ABUNDANCE) 

#p-value = 0.1889 , we fail to reget the hypothesis. This dataset has homogenity of variances

TUSVI_ADAM_ANOVA <- aov(Abundance_Log ~ Site, data = TUSVI_ABUNDANCE)
summary(TUSVI_ADAM_ANOVA)

#2.) Normality
shapiro.test(TUSVI_ABUNDANCE$Abundance_Log)
#W = 0.98973, p-value = 0.9736, >0.05 I fail to reget the hypothesis. This data shows normality

################# ANOVA ########################

TUSVI_ADAM_ANOVA <- aov(Abundance_Log ~ Site, data = TUSVI_ABUNDANCE)
summary(TUSVI_ADAM_ANOVA)


# There is signifigance of differences of site with damselfish abundance yash!! 

# CURRENT ANOVA BELOW 
#           Df Sum Sq Mean Sq F value   Pr(>F)    
#Site         2  1.677  0.8387   17.98 3.85e-06 ***
#Residuals   36  1.679  0.0466        
# St Croix is significantly different from St. Thomas and St. John when it comes to damselfish abundance at TCRMP sites.
TukeyHSD(TUSVI_ADAM_ANOVA)

 TIsland_ANOVA<- emmeans(TUSVI_ADAM_ANOVA, ~ Site)
 
 TISLAND_POSTHOC <- cld(TIsland_ANOVA, Letter = letters)
 
 TISLAND_POSTHOC$.group <- trimws(TISLAND_POSTHOC$.group)

 TISLAND_POSTHOC$y_pos <- min(TUSVI_ADAM_ANOVA$Abundance_Log, na.rm = TRUE) * 1.1
 
TUSVI_ADAM_BPLOT <- ggplot(TUSVI_ADAM_ANOVA, aes(x = Site , y = Abundance_Log , fill = Site )) +
  geom_boxplot() +
  geom_text(data = TISLAND_POSTHOC,
            aes(x = Site,
                y = y_pos,
                label = .group),
            vjust = 2.1 ,
            inherit.aes = FALSE,
            size = 6) +
  scale_fill_manual(values = c(
    "St. Thomas" = "coral",
    "St. John"   = "green2",
    "St. Croix"  = "cyan2")) +
  scale_y_continuous(limits = c(1.75, 4)) +
  labs(title = "Damselfish Mean Abundance Across TCRMP Sites From 2013-2025") +
  labs( x = "Island" , y = "Damselfish Mean Abundance log(Abundance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##### now with SITE! #####
########## St. Thomas #######################

TSTT_ABUNDANCE_DAMSELFISH$Abundance_Log <- log(TSTT_ABUNDANCE_DAMSELFISH$Location_Abundance_Mean) # log it 

TSTT_ADAM_ANOVA <- aov(Abundance_Log ~ Location, data = TSTT_ABUNDANCE_DAMSELFISH)

#1.) Homoscedasticity
bartlett.test(Abundance_Log ~ Location, data = TSTT_ABUNDANCE_DAMSELFISH) 

#p-value = 0.004478 , we reject the hypothesis. This dataset DOES NOT HAVE homogenity of variances

#USE CAUTION 


hist(TSTT_ABUNDANCE_DAMSELFISH$Abundance_Log)
#it looks normal

#2.) Normality
shapiro.test(TSTT_ABUNDANCE_DAMSELFISH$Abundance_Log)
#W = 0.99, p-value = 0.2404, >0.05 I fail to reget the hypothesis. This data shows normality

TSTT_ADAM_ANOVA <- aov(Abundance_Log ~ Location, data = TSTT_ABUNDANCE_DAMSELFISH)
summary(TSTT_ADAM_ANOVA) # location is significantly different from one another: P-value = 1.04e-13
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#Location     13  20.10  1.5463   8.915 1.04e-13 ***
# Residuals   166  28.79  0.1735   
TukeyHSD(TSTT_ADAM_ANOVA)

TSTT_ANOVA<- emmeans(TSTT_ADAM_ANOVA, ~ Location)

TSTT_POSTHOC <- cld(TSTT_ANOVA, Letter = letters)

TSTT_POSTHOC$.group <- trimws(TSTT_POSTHOC$.group)

TSTT_POSTHOC$y_pos <- max(TSTT_ABUNDANCE_DAMSELFISH$Abundance_Log, na.rm = TRUE) * 1.1


TSTT_ADAM_BPLOT <- ggplot(TSTT_ABUNDANCE_DAMSELFISH, aes(x = Location , y = Abundance_Log , fill = Location )) +
          geom_boxplot() +
          geom_text(data = TSTT_POSTHOC,
                    aes(x = Location,
                        y = y_pos,
                        label = .group),
                    vjust = 2.1 ,
                    inherit.aes = FALSE,
                    size = 6) +
          labs(title = "Damselfish Mean Abundance Across TCRMP Sites on St. Thomas, USVI From 2013-2025") +
          labs( x = "Island" , y = "Damselfish Mean Abundance log(Abundance)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        

######################################################################################      ST JOHN          #################################

TSTJ_ABUNDANCE_DAMSELFISH$Abundance_Log <- log(TSTJ_ABUNDANCE_DAMSELFISH$Location_Abundance_Mean) # log it 

bartlett.test(Abundance_Log ~ Location, data = TSTJ_ABUNDANCE_DAMSELFISH) 
# p-value = 0.9652, i fail to reject the hypothesis. This data has homogenity of variances. 

shapiro.test(TSTJ_ABUNDANCE_DAMSELFISH$Abundance_Log)
#p-value = 0.1746, I fail to reject the hypothesis, this data has normality of residuals

TSTJ_ADAM_ANOVA <- aov(Abundance_Log ~ Location, data = TSTJ_ABUNDANCE_DAMSELFISH)
summary(TSTJ_ADAM_ANOVA)
# P-value = 8.42e-11, There is signifigant differences in damselfish abundance across St. John TCRMP sites. 
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#Location     2 11.614   5.807   47.31 8.42e-11 ***
#Residuals   36  4.419   0.123      

TukeyHSD(TSTJ_ADAM_ANOVA)

# Fish Bay has signifgantly different damselfish abundance compared to the other St. John TCRMP sites

TSTJ_ANOVA<- emmeans(TSTJ_ADAM_ANOVA, ~ Location)

TSTJ_POSTHOC <- cld(TSTJ_ANOVA, Letter = letters)

TSTJ_POSTHOC$.group <- trimws(TSTJ_POSTHOC$.group)

TSTJ_POSTHOC$y_pos <- min(TSTJ_ABUNDANCE_DAMSELFISH$Abundance_log, na.rm = TRUE) * 1.1

TSTJ_ADAM_BPLOT <- ggplot(TSTJ_ABUNDANCE_DAMSELFISH, aes(x = Location , y = Abundance_Log , fill = Location )) +
  geom_boxplot() +
  geom_text(data = TSTJ_POSTHOC,
            aes(x = Location,
                y = y_pos,
                label = .group),
            vjust = 1.1 ,
            inherit.aes = FALSE,
            size = 6) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(title = " Damselfish Mean Abundance Across TCRMP Sites on St. John From 2013-2025") +
  labs( x = "Location" , y = "Damselfish Mean Abundance log(Abundance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################################################################################################################################################ ST. CROIX ################################

TSTX_ABUNDANCE_DAMSELFISH$Abundance_Log <- log(TSTX_ABUNDANCE_DAMSELFISH$Location_Abundance_Mean) # log it 

bartlett.test(Abundance_Log ~ Location, data = TSTX_ABUNDANCE_DAMSELFISH) 
#p-value = 0.9464, failed to reject hypothesis

shapiro.test(TSTX_ABUNDANCE_DAMSELFISH$Abundance_Log)
# p-value =  p-value = 0.01692, REJECT the hypothesis

leveneTest(Abundance_Log ~ Location, data = TSTX_ABUNDANCE_DAMSELFISH) 
# p-value = 0.9934


TSTX_ADAM_ANOVA <- aov(Abundance_Log ~ Location, data = TSTX_ABUNDANCE_DAMSELFISH)
summary(TSTX_ADAM_ANOVA)
#P-value is <2e-16, St. Croix TCRMP sites has signifgantly different damselfish abundances
#             Df Sum Sq Mean Sq F value Pr(>F)    
#Location     11  61.17   5.561    30.5 <2e-16 ***
#Residuals   139  25.35   0.182                   
TukeyHSD(TSTX_ADAM_ANOVA)

TSTX_ANOVA<- emmeans(TSTX_ADAM_ANOVA, ~ Location)

TSTX_POSTHOC <- cld(TSTX_ANOVA, Letter = letters)

TSTX_POSTHOC$.group <- trimws(TSTX_POSTHOC$.group)

TSTX_POSTHOC$y_pos <- max(TSTX_ABUNDANCE_DAMSELFISH$Abundance_Log, na.rm = TRUE) * 1.1


TSTX_ADAM_BPLOT <- ggplot(TSTX_ABUNDANCE_DAMSELFISH, aes(x = Location , y = Abundance_Log , fill = Location )) +
  geom_boxplot() +
  geom_text(data = TSTX_POSTHOC,
            aes(x = Location,
                y = y_pos,
                label = .group),
            inherit.aes = FALSE,
            size = 6) +
  labs(title = "Damselfish Mean Abundance at TCRMP Sites on St. Croix") +
  labs( x = "Location" , y = "Damselfish Mean Abundance log(Abundance)") +
  labs(caption = "This dataset did not pass Shapiro-Wilk test") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### southern sites (besides Great Pond) are not sigifgantly different
#### Great Pond is similar to deeper sites (Cane Bay and Buck STX DEEP)
### Buck Island, Castle, Eagle Ray and HBE FSA are all signifgantly different


## watch out for psuedo-replication on coral health and interactions 

## damselfish and coral distribution using NCRMP data 
