####################################################################################################################################################
# TCRMP FISH Damselfish Density From 2013-2025
# Owner: Lila Goodman
# Created On: 10-11-2025
# Last Edit: 23-07-2026
##########################################################################

####Libraries########
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(MASS)

#GOAL: average damselfish abundance for every TCRMP site from 2013-2025

### The TCRMP Fish Cenus is a dataset from 2003-2025. Divers record density on 25x4m transects for various fish. 

TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_SITE$Island[TCRMP_SITE$Island == "STT"] <- "St. Thomas"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STJ"] <- "St. John"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STX"] <- "St. Croix" # changed to proper names

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data


TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset

TDENS_DAMSEL <- dplyr::filter(CLEAN_TCRMP_FISH,
                              ScientificName %in% c(
                                "Microspathodon chrysurus",
                                "Stegastes partitus",
                                "Stegastes variabilis",
                                "Stegastes adustus",
                                "Stegastes leucostictus",
                                "Stegastes planifrons"
                              )) %>% # filter fish species to damselfish only
  mutate(Damselfish_Density = ((SppTotal/100))) # DENSITY OF DAMSELFISH in 100M^2 area conversion 

##########################################################################################################################################################
#####################  DAMSELFISH DENSITY       #############################

TTRANS_MEAN_DENS_DAMSEL <- TDENS_DAMSEL %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Sum_Density= sum(Damselfish_Density),
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop",
    Island = Island) %>% 
  unique()

TDENSITY_DAMSELFISH <- TTRANS_MEAN_DENS_DAMSEL  %>% # Mean of damselfish density along each site per year and location USE THIS FOR ANOVA
  group_by(Location, SampleYear)  %>% 
  summarise(
    Trans_Density_Mean = (sum(Mean_Density))/10 , #10 is the transect per site
    Island = Island 
  )

TMAP_DENSITY_DAMSELFISH <- TDENSITY_DAMSELFISH  %>% # Mean of damselfish density along each site 
  group_by(Location)  %>% 
  summarise(
    Location_Density_mean = (mean(Trans_Density_Mean)),
    SD_Density = sd(Trans_Density_Mean),
    SEM_Density = sd(Trans_Density_Mean) / sqrt(length(Trans_Density_Mean)),
    Island = Island
  )

SITE_DENSITY_DAMSELFISH <- TMAP_DENSITY_DAMSELFISH %>% 
  mutate(
    Island = factor(Island, levels = c("St. Thomas", "St. John", "St. Croix"))
  ) %>%
  arrange(Island, Location) %>%
  mutate(Location = factor(Location, levels = unique(Location)))

SITE_DENSITY_DAMSELFISH$Location <- factor(
  SITE_DENSITY_DAMSELFISH$Location,
  levels = SITE_DENSITY_DAMSELFISH %>%
    arrange(Island, Location) %>%
    pull(Location) %>%
    unique()
) # organizing plot to be by island

SITE_DENSITY_DAMS_BPLOT <- ggplot(SITE_DENSITY_DAMSELFISH, aes(x = Location , y = Location_Density_mean , fill = Island)) + 
  geom_col() + 
  scale_fill_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John" = "forestgreen",
    "St. Croix" = "orange"
  )) +
  geom_errorbar(aes(ymin = Location_Density_mean-SEM_Density, ymax = Location_Density_mean+SEM_Density), width = .9)  +
  labs(title = "Mean Damselfish Density at Territorial Coral Reef Monitoring (TCRMP) Fish Sites in the US Virgin Islands") +
  labs(y = "Damselfish Mean Density(100m^2)")+
  labs(x = "TCRMP Sites") +
  labs(caption = "Figure No. . Mean damselfish density (100/m^2) and SEM of TCRMP sites preformed by fish surveys from 2013-2025. Sites are separated by island of St. Croix(orange) St. Thomas(blue), and St. John(green).") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1 , hjust = 1))
  

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
STT_DDAMS <- dplyr::filter(TDENS_DAMSEL, #Only including St. Thomas sites
                          Island %in% c(
                             "St. Thomas"
                           ))

STT_TDMD <- STT_DDAMS %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density = mean(Damselfish_Density) ,
    Island = Island,
    .groups = "drop") %>% 
  unique()

TSTT_DENSITY_DAMSELFISH <- STT_TDMD  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 , #10 is the transect per site,
    SD_Density = sd(Mean_Density),
    SEM_Density = sd(Mean_Density) / sqrt(length(Mean_Density)),
    Island = Island
  )

TSTT_DENSITY_LPLOT <- ggplot(TSTT_DENSITY_DAMSELFISH, aes(x = SampleYear , y = Location_Density_Mean, group = Location, color = Location)) +
  geom_line() +
  scale_x_continuous(breaks = 2013:2025) +
  labs(title = "Mean Damselfish Density at St. Thomas TCRMP Sites From 2013-2025") +
  labs(x = "Year", y= "Damselfish Mean Density (100m^2)") +
  labs(caption = "Figure No. 4. Mean Damselfish Density of TCRMP Sites on St. Thomas, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# The only importance of these line graphs is to see if there is a sigifigance of damselfish density with time (doesnt look like it). 

################# St. Croix #####################

SX_ADAMS <- dplyr::filter(TDENS_DAMSEL, #Only including St. Thomas sites
                          Island %in% c(
                            "St. Croix"
                          ))

STX_TAMD <- SX_ADAMS %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTX_DENSITY_DAMSELFISH <- STX_TAMD  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTX_DENSITY_LPLOT <- ggplot(TSTX_DENSITY_DAMSELFISH, aes(x=SampleYear , y = Location_Density_Mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_DENSITY_DAMSELFISH$SampleYear), max(TSTX_DENSITY_DAMSELFISH$SampleYear), by = 1)) + # damselfish density from 2013-2025 on St Croix 
  labs(title = "Mean Damselfish Density at St. Croix TCRMP Sites From 2013-2025") +
  labs(x= "Year", y= "Damselfish Mean Density(100m^2)") +
  labs(caption = "Figure No. 6. Mean Damselfish Density at TCRMP Sites on St. Croix, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTX_DENSITY_LPLOT)

############## ST. JOHN ######################

STJ_ADAMS <- dplyr::filter(TDENS_DAMSEL, #Only including St. Thomas sites
                           Island %in% c(
                             "St. John"
                          ))

TSTJ_TAMD <- STJ_ADAMS %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTJ_DENSITY_DAMSELFISH <- TSTJ_TAMD  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTJ_DENSITY_LPLOT <- ggplot(TSTJ_DENSITY_DAMSELFISH, aes(x=SampleYear , y = Location_Density_Mean, group= Location, color = Location)) + 
  geom_line() + # damselfish density from 2013-2025 on St John
  scale_x_continuous(breaks = seq(min(TSTJ_DENSITY_DAMSELFISH$SampleYear), max(TSTJ_DENSITY_DAMSELFISH$SampleYear), by = 1)) +
  labs(title = "Mean Damselfish Density at St. John TCRMP Sites From 2013-2025") +
  labs(x= "Year", y= "Mean Damselfish Density (100m^2)") +
  labs(caption = "Figure No. 5. Mean Damselfish Density at TCRMP Sites St. John, USVI from 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTJ_DENSITY_LPLOT)

############################################################################################################################################################################# DAMSELFISH DENSITY BY ISLAND #########################

################## St. Thomas #################

TSTT_ALL_DENSITY_DAMSELFISH <- TSTT_DENSITY_DAMSELFISH  %>% # Mean of damselfish density on St. Thomas from 2013-2025
  group_by(SampleYear)  %>% 
  summarise(
    Density_Mean = (mean(Location_Density_Mean))
  )

TSTT_ALL_DENSITY_DAMSELFISH_LPLOT <- ggplot(TSTT_ALL_DENSITY_DAMSELFISH, aes(x = SampleYear , y = Density_Mean)) + 
  geom_line() + # damselfish density from 2013-2025 on St Thomas 
  scale_x_continuous(breaks = seq(min(TSTT_ALL_DENSITY_DAMSELFISH$SampleYear), max(TSTT_ALL_DENSITY_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= " Mean Damselfish Density (100m^2)") +
  labs(caption = "Figure No. ?. St. Thomas Average Damselfish Density From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# trend looks downward

################ ST. CROIX ###################

TSTX_ALL_DENSITY_DAMSELFISH <- TSTX_DENSITY_DAMSELFISH  %>% # Mean of damselfish density on St. Croix from 2013-2025
  group_by(SampleYear)  %>% 
  summarise(
    Density_Mean = (mean(Location_Density_Mean))
  )

TSTX_ALL_DENSITY_DAMSELFISH_LPLOT <- ggplot(TSTX_ALL_DENSITY_DAMSELFISH, aes(x = SampleYear , y = Density_Mean)) + 
  geom_line() + # damselfish density from 2013-2025 on St Croix 
  scale_x_continuous(breaks = seq(min(TSTX_ALL_DENSITY_DAMSELFISH$SampleYear), max(TSTX_ALL_DENSITY_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= "Mean Damselfish Density (100m^2)") +
  labs(caption = "Figure No. ?. St. Croix Average Damselfish Density From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#also looks overall downwards 

################# ST. JOHN #########################

TSTJ_ALL_DENSITY_DAMSELFISH <- TSTJ_DENSITY_DAMSELFISH  %>% # Mean of damselfish density on St. John from 2013-2021
  group_by(SampleYear)  %>% 
  summarise(
    Density_Mean = (mean(Location_Density_Mean))
  )

STJ_ALL_DENSITY_DAMSELFISH_LPLOT <- ggplot(TSTJ_ALL_DENSITY_DAMSELFISH, aes(x = SampleYear , y = Density_Mean)) + 
  geom_line() + # damselfish density from 2013-2022 on St John 
  scale_x_continuous(breaks = seq(min(TSTJ_ALL_DENSITY_DAMSELFISH$SampleYear), max(TSTJ_ALL_DENSITY_DAMSELFISH$SampleYear), by = 1)) + 
  labs(x= "Year", y= "Mean Damselfish Density (100m^2)") +
  labs(caption = "Figure No. ?. St. John Average Damselfish Density From 2013-2025") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#slight downward trend

######### MERGING THEM TOGETHER ############
##adding columns of site to each dataset

TSTT_ALL_DENSITY_DAMSELFISH$Site <- "St. Thomas"
TSTX_ALL_DENSITY_DAMSELFISH$Site <- "St. Croix"
TSTJ_ALL_DENSITY_DAMSELFISH$Site <- "St. John"


TUSVI_DENSITY <- bind_rows(TSTT_ALL_DENSITY_DAMSELFISH ,
                           TSTX_ALL_DENSITY_DAMSELFISH, 
                           TSTJ_ALL_DENSITY_DAMSELFISH)

TUSVI_DENSITY_LPLOT <- ggplot(TUSVI_DENSITY, aes(x = SampleYear , y = Density_Mean, group = Site , color = Site)) + 
  geom_line() + # damselfish abundance from 2013-2025 in USVI
  scale_color_manual( values = c( 
    "St. Thomas" = "steelblue" ,
    "St. John" = "forestgreen" , 
    "St. Croix" = "orange" 
  )) +
  scale_x_continuous(breaks = seq(min(TUSVI_DENSITY$SampleYear), max(TUSVI_DENSITY$SampleYear), by = 1)) + 
  labs(title = " Average Damselfish Density Across US Virgin Islands From 2013-2025") +
  labs(x= "Year", y= "Mean Damselfish Density (100m^2)") +
  labs(caption = "Figure No. ?. Average Damselfish Density From 2013-2025 Using TCRMP Sites Across the USVI By Island") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##############################################################################################################################################################################################################################

# Lila Finally Gets To Statsitical parts 
## THESIS QUESTION FOR THIS DATASET #############
# Is there a significance of damselfish density across the USVI?
# Is there a signifcance of damselfish density across TCRMP sites? 
# Has Damselfish population/ density increase from 2013-2025 at TCRMP Sites?

# GOAL: 
#### Make sure data meets the assumptions for an anova
## Explain why I am doing an ANOVA

####  between islands and damselfish abundance between sites 

## QUESTION ##
# Does damselfish density (Dependent variable) change with site/ island (Independent variable)? 

#H0 The mean density of damselfish is the same across 3 islands from 2013-2025

#H0 The mean density of damselfish is different across the 3 islands from 2013-2025

############ASSUMPTIONS##############

TDD_ANOVA <- aov(Trans_Density_Mean ~ Location, data = TDENSITY_DAMSELFISH)
summary(TDD_ANOVA)

#1.) Independence
# Each value is independent from one another and do not affect one another. 

#2.) Normality
TFISH_NBOXPLOT <- boxplot(Trans_Density_Mean ~ Location, data = TDENSITY_DAMSELFISH,
        main = "Normality of TCRMP Fish Data 2013-2025",
        xlab = "Island",
        ylab = "Fish Density (100m^2)")
# NOTE: There are several outliers in each site 

TFISH_NHISPLOT <- hist(TDENSITY_DAMSELFISH$Trans_Density_Mean)
#severely right skewed

TFISH_RSTAND <- rstandard(TDD_ANOVA)

TFISH_SHAP_TEST <- shapiro.test(TFISH_RSTAND)
#W = 0.94888, p-value < 2.2e-16, I fail to reject the hypothesis. This dataset shows normality. 

# Homoscedasticity

residuals_data <- residuals(TDD_ANOVA)
predicted_values <- fitted(TDD_ANOVA)

plot(predicted_values, residuals_data, 
     xlab="Predicted Values", ylab="Residuals",
     main="Checking Homoscedasticity")
abline(h=0, col="blue")
hist(residuals_data, breaks=20, main="Histogram of Residuals", xlab="Residuals")
# data fits onto a normal distributed bell curve

#3.) Homogenity of Variances
SITE_DENSITY_DAMSELFISH %>% 
  group_by(Location) %>%
  summarise(
    n = n(),
    unique_values = n_distinct(Location_Density_mean)
  ) %>%
  print(n = Inf)
bartlett.test(Trans_Density_Mean ~ Location, data = TDENSITY_DAMSELFISH) 

#Bartlett's K-squared = 2060.6, df = 32, p-value < 2.2e-16
# I fail to reject the null hypothesis, this dataset does not have homogenity of variances; however, this dataset is robust (n > 30) and Type 1 error will not be dramatic. 
leveneTest(Trans_Density_Mean ~ Location, data = TDENSITY_DAMSELFISH)


TUSVI_ADAM_ANOVA <- aov(Abundance_Log ~ Site, data = TUSVI_ABUNDANCE)
summary(TUSVI_ADAM_ANOVA)


## Transforming the data (log) ##
SITE_DENSITY_DAMSELFISH$Density_Log <- log(SITE_DENSITY_DAMSELFISH$Location_Density_mean) # log it 
shapiro.test(SITE_DENSITY_DAMSELFISH$Density_Log)

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
