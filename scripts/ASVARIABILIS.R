####################################################################################################################################################
# TCRMP FISH Stegates Variabilis Density From 2013-2025
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

#GOAL: average Stegates variabilis abundance at every TCRMP site from 2013-2025
TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_SITE$Island[TCRMP_SITE$Island == "STT"] <- "St. Thomas"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STJ"] <- "St. John"
TCRMP_SITE$Island[TCRMP_SITE$Island == "STX"] <- "St. Croix" # changed to proper names

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data

TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset


TDENS_SV <- dplyr::filter(CLEAN_TCRMP_FISH,
                          ScientificName %in% c(
                            "Stegastes variabilis"
                          )) %>% 
  mutate(Damselfish_Density = ((SppTotal/100))) # DENSITY OF DAMSELFISH in 100M^2 area conversion 

####################################################################################################################################################################  STEGATES VARIABILIS ACROSS USVI ##############################

TUSVI_TDMSV <- TDENS_SV %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Island = Island ,
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TUSVI_DENSITY_SV <- TUSVI_TDMSV  %>% # Mean of damselfish density along each site per year and Island
  group_by(Island, SampleYear)  %>% 
  summarise(
    Location = Location ,
    Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TUSVI_DENSITY_SV_LPLOT <- ggplot(TUSVI_DENSITY_SV, aes(x = SampleYear , y = Density_Mean, group= Island, color = Island)) +
  geom_line() +
  scale_fill_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John"   = "forestgreen",
    "St. Croix"  = "orange")) +
  scale_x_continuous(breaks = seq(min(TUSVI_DENSITY_SV$SampleYear), max(TUSVI_DENSITY_SV$SampleYear), by = 1)) +
  labs(title = "Average Stegates variabilis Density at TCRMP Sites in the USVI from 2013-2025") +
  labs(x= "Year", y= "Stegates variabilis Mean Density") +
  labs(caption = "Figure No. ?. Average Stegates variabilis density at TCRMP sites from 2013-2025. These sites are located on St. Thomas, St. Croix, and St. John.") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TUSVI_DENSITY_SV_LPLOT)


TUSVI_SITE_DSV <- TUSVI_DENSITY_SV %>% 
  group_by(Location) %>% 
  summarise(
    Island = Island ,
    MEAN_Density = mean(Density_Mean),
    SD_Density = sd((Density_Mean)),
    SEM_Density = sd((Density_Mean)) / sqrt(length((Density_Mean)))) %>%
  unique()

TUSVI_DSV <- TUSVI_SITE_DSV %>% 
  mutate(
    Island = factor(Island, levels = c("St. Thomas", "St. John", "St. Croix"))
  ) %>%
  arrange(Island, Location) %>%
  mutate(Location = factor(Location, levels = unique(Location)))

TUSVI_DSV$Location <- factor(
  TUSVI_DSV$Location,
  levels = TUSVI_DSV %>%
    arrange(Island, Location) %>%
    pull(Location) %>%
    unique()
) # organizing plot to be by island

TUSVI_SITE_DSV_BPLOT <- ggplot(TUSVI_DSV, aes(x = Location , y = MEAN_Density , fill = Island)) + 
  geom_col() + 
  scale_fill_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John" = "forestgreen",
    "St. Croix" = "orange"
  )) +
  geom_errorbar(aes(ymin = MEAN_Density-SEM_Density, ymax = MEAN_Density+SEM_Density), width = .9)  +
  labs(title = "Mean Stegates Variabilis Density at Territorial Coral Reef Monitoring (TCRMP) Fish Sites in the US Virgin Islands") +
  labs(y = "Stegates variabilis Mean Density(100m^2)")+
  labs(x = "TCRMP Sites") +
  labs(caption = "Figure No. . Mean Stegates variabilis density (100/m^2) and SEM of TCRMP sites preformed by fish surveys from 2013-2025. Sites are separated by island of St. Croix(orange) St. Thomas(blue), and St. John(green).") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1 , hjust = 1))


################### ST. THOMAS ###########################


TSTT_DSV <- dplyr::filter(TDENS_SV, #Only including St. Thomas sites
                         Island %in% c(
                           "St. Thomas"
                         ))

TSTT_TDMSV <- TSTT_DSV %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTT_DENSITY_SV <- TSTT_TDMSV  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTT_DENSITY_SV_LPLOT <- ggplot(TSTT_DENSITY_SV, aes(x = SampleYear , y = Location_Density_Mean, group = Location, color = Location)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTT_DENSITY_SV$SampleYear), max(TSTT_DENSITY_SV$SampleYear), by = 1)) +
  labs(title = "Mean Stegates Variabilis Density at TCRMP Sites on St. Thomas") +
  labs(x= "Year", y= "Stegates variabilis Mean Density (100m^2)") +
  labs(caption = "Figure No. ?. Mean Stegates variabilis density at TCRMP Sites on St. Thomas") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTT_DENSITY_SV_LPLOT)

############################# ST. CROIX ##################################

TSTX_DSV <- dplyr::filter(TDENS_SV, #Only including St. Croix sites
                        Island %in% c(
                          "St. Croix"
                        ))

TSTX_TDMSV <- TSTX_DSV %>% # mean of damselfish density along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTX_DENSITY_SV <- TSTX_TDMSV  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTX_DENSITY_SV_LPLOT <- ggplot(TSTX_DENSITY_SV, aes(x = SampleYear , y = Location_Density_Mean, group = Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_DENSITY_SV$SampleYear), max(TSTX_DENSITY_SV$SampleYear), by = 1)) +
  labs(title = "Mean Stegates Variabilis Density at TCRMP Sites on St. Croix") +
  labs(x= "Year", y= "Stegates variabilis Mean Density (100m^2)") + 
  labs(caption = "Figure No. ?. Mean Stegates variabilis Density at TCRMP Sites on St. Croix") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTX_DENSITY_SV_LPLOT)

##################### ST. JOHN ##################################

TSTJ_TDMSV <- dplyr::filter(TDENS_SV, #Only including St. John sites
                           Island %in% c(
                             "St. John"
                           ))

TSTJ_TDMSV <- TSTJ_TDMSV %>% # mean of damselfish denity along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Density  = mean(Damselfish_Density) ,
    .groups = "drop") %>% 
  unique()

TSTJ_DENSITY_SV <- TSTJ_TDMSV  %>% # Mean of damselfish density along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Density_Mean = (sum(Mean_Density))/10 #10 is the transect per site
  )

TSTJ_DENSITY_SV_LPLOT <- ggplot(TSTJ_DENSITY_SV, aes(x = SampleYear , y = Location_Density_Mean, group = Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTJ_DENSITY_SV$SampleYear), max(TSTJ_DENSITY_SV$SampleYear), by = 1)) +
  labs(title = "Mean Stegates Variabilis Density at TCRMP Sites on St. John") +
  labs(x= "Year", y= "Stegates variabilis Mean Density (100m^2)") + 
  labs(caption = "Figure No. ?. Mean Stegates variabilis sensity at TCRMP Sites on St. John") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTJ_DENSITY_SV_LPLOT)

############################################################################################################################################################################################################################################################# STATS TIME #######################################

# Is there a significance difference of SA abundance across the islands in the USVI?

### ASUMPTIONS 

#1.) Homoscedasticity
bartlett.test(Abundance_Mean ~ Island, data = TUSVI_ABUNDANCE_SV) 
#p-value = 5.303e-06, I reject the hypothesis. This dataset DOES NOT HAVE homogenity of variances

TUSVI_ABUNDANCE_SV$Abundance_Log <- log(TUSVI_ABUNDANCE_SV$Abundance_Mean) # log it 
bartlett.test(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SV) 
#p-value = 0.3927, I reject to fail the hypothesis, the log version has homogenity of variances

hist(TUSVI_ABUNDANCE_SV$Abundance_Log)
#it decently normal

#2.) Normality
shapiro.test(TUSVI_ABUNDANCE_SV$Abundance_Log)
#p-value = 0.1343 I fail to reject the hypothesis!!

TUSVI_ASV_ANOVA <- aov(Abundance_Log ~ Island, data = TUSVI_ABUNDANCE_SV)
summary(TUSVI_ASV_ANOVA)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#Island       2  24.83  12.413   9.725 0.000457 ***
#Residuals   34  43.40   1.276     
# significance difference of SV abundance across the islands in the USVI

################### POST HOC ###################

TukeyHSD(TUSVI_ASV_ANOVA)

TUSVI_Island_ANOVA<- emmeans(TUSVI_ASV_ANOVA, ~ Island)

TUSVI_ISLAND_POSTHOC <- cld(TUSVI_Island_ANOVA, Letter = letters)

TUSVI_ISLAND_POSTHOC$.group <- trimws(TUSVI_ISLAND_POSTHOC$.group)

TUSVI_ISLAND_POSTHOC$Post_Hoc <- min(TUSVI_ASV_ANOVA$Abundance_Log, na.rm = TRUE) * 1.1

TUSVI_ASV_BPLOT <- ggplot(TUSVI_ASV_ANOVA, aes(x = Island , y = Abundance_Log , fill = Island)) +
  geom_boxplot() +
  geom_text(data = TUSVI_ISLAND_POSTHOC,
            aes(x = Island,
                y = Post_Hoc,
                label = .group),
            vjust = 2.1 ,
            inherit.aes = FALSE,
            size = 6) +
  scale_y_continuous(limits = c(-2.5, 5)) +
  scale_fill_manual(values = c(
    "St. Thomas" = "steelblue",
    "St. John"   = "forestgreen",
    "St. Croix"  = "orange")) +
  labs(title = "Stegates variabilis Mean Density Across TCRMP Sites From 2013-2025") +
  labs(caption = "Stegates variabilis Mean Density on St. Thomas(STT) , St. Croix(STX), and St. John(STJ).") +
  labs(x = "Island" , y = "Stegates variabilis Mean Density (100m^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## For Stegates variabilis abundance, St. John and St. Thomas are significantly different to one another, but St. Croix is not. 