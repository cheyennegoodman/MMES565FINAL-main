####################################################################################################################################################
# TCRMP FISH Damselfish Abundance From 2013-2026
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

#GOAL: average Stegates variabilis abundance at every TCRMP site from 2013-2025
TCRMP_SITE <- read.csv("TCRMP_datasets/TCRMP_Site_Metadata.xls - SiteMetadata.csv") # loading TCRMP SITE data

TCRMP_FISH_RAW <- read.csv("TCRMP_datasets/TCRMP_FISH/APR2026/TCRMP_Master_Fish_Census_Apr2026_Abundance.csv") # TCRMP FISH data

TCRMP_FISH <- subset(TCRMP_FISH_RAW, select = -c(SampleDate, SampleMonth , Period, CommonName, Observer, TrophicGroup, X0.5, X6.10, X11.20, X21.30, X31.40, X41.50, X51.60, X61.70, X71.80, X81.90, X91.100, X101.110, X111.120, X121.130, X131.140, X141.150, X.150)) %>%  #removing excess columns
  dplyr::filter(!dplyr::between(as.numeric (SampleYear), 2003, 2012)) #filter data to 2013-2025

CLEAN_TCRMP_FISH <- left_join(TCRMP_FISH, TCRMP_SITE[, c("Location", "Depth", "Island" )], by = "Location") #attaching island names and depth to dataset


TABUND_SV <- dplyr::filter(CLEAN_TCRMP_FISH,
                          ScientificName %in% c(
                            "Stegastes variabilis"
                          ))

####################################################################################################################################################################  STEGATES VARIABILIS ACROSS USVI ##############################

TUSVI_TAMSV <- TABUND_SV %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Island = Island ,
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TUSVI_ABUNDANCE_SV <- TUSVI_TAMSV  %>% # Mean of damselfish abundance along each site per year and Island
  group_by(Island, SampleYear)  %>% 
  summarise(
    Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TUSVIABUNDANCE_SV_LPLOT <- ggplot(TUSVI_ABUNDANCE_SV, aes(x = SampleYear , y = Abundance_Mean, group= Island, color = Island)) +
  geom_line() +
  scale_color_manual(values = c(
    "STT" = "coral",
    "STJ"   = "green2",
    "STX"  = "cyan2")) +
  scale_x_continuous(breaks = seq(min(TUSVI_ABUNDANCE_SV$SampleYear), max(TUSVI_ABUNDANCE_SV$SampleYear), by = 1)) +
  labs(title = "Average Stegates variabilis Abundance at TCRMP Sites in the USVI from 2013-2025") +
  labs(x= "Year", y= "Stegates variabilis Mean Abundance") +
  labs(caption = "Figure No. ?. Average Stegates variabilis abundance at TCRMP sites from 2013-2025. These sites are located on St. Thomas (STT), St. Croix (STX), and St. John (STJ).") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TUSVIABUNDANCE_SV_LPLOT)

################### ST. THOMAS ###########################


TSTT_ASV <- dplyr::filter(TABUND_SV, #Only including St. Thomas sites
                         Island %in% c(
                           "STT"
                         ))

TSTT_TAMSV <- TSTT_ASV %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTT_ABUNDANCE_SV <- TSTT_TAMSV  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTT_ABUNDANCE_SV_LPLOT <- ggplot(TSTT_ABUNDANCE_SV, aes(x = SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTT_ABUNDANCE_SV$SampleYear), max(TSTT_ABUNDANCE_SV$SampleYear), by = 1)) +
  labs(title = "Stegates Variabilis Abundance at TCRMP Sites on St. Thomas") +
  labs(x= "Year", y= "Stegates variabilis Mean Abundance") +
  labs(caption = "Figure No. ?. Average Stegates variabilis Abundance at TCRMP Sites on St. Thomas") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTT_ABUNDANCE_SV_LPLOT)

############################# ST. CROIX ##################################

TSTX_ASV <- dplyr::filter(TABUND_SV, #Only including St. Thomas sites
                        Island %in% c(
                          "STX"
                        ))

TSTX_TAMSV <- TSTX_ASV %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTX_ABUNDANCE_SV <- TSTX_TAMSV  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTX_ABUNDANCE_SV_LPLOT <- ggplot(TSTX_ABUNDANCE_SV, aes(x = SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTX_ABUNDANCE_SV$SampleYear), max(TSTX_ABUNDANCE_SV$SampleYear), by = 1)) +
  labs(title = "Stegates Variabilis Abundance at TCRMP Sites on St. Croix") +
  labs(x= "Year", y= "Stegates variabilis Mean Abundance") + 
  labs(caption = "Figure No. ?. Average Stegates variabilis Abundance at TCRMP Sites on St. Croix") +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTX_ABUNDANCE_SV_LPLOT)

##################### ST. JOHN ##################################

TSTJ_TAMSV <- dplyr::filter(TABUND_SV, #Only including St. Thomas sites
                           Island %in% c(
                             "STJ"
                           ))

TSTJ_TAMSV <- TSTJ_TAMSV %>% # mean of damselfish abundance along each transect by year and location
  group_by(Location, Transect, SampleYear) %>% 
  summarise(
    Mean_Abundance  = mean(SppTotal) ,
    .groups = "drop") %>% 
  unique()

TSTJ_ABUNDANCE_SV <- TSTJ_TAMSV  %>% # Mean of damselfish abundance along each site per year and location
  group_by(Location, SampleYear)  %>% 
  summarise(
    Location_Abundance_Mean = (sum(Mean_Abundance))/10 #10 is the transect per site
  )

TSTJ_ABUNDANCE_SV_LPLOT <- ggplot(TSTJ_ABUNDANCE_SV, aes(x = SampleYear , y = Location_Abundance_Mean, group= Location, color = Location)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(min(TSTJ_ABUNDANCE_SV$SampleYear), max(TSTJ_ABUNDANCE_SV$SampleYear), by = 1)) +
  labs(title = "Stegates Variabilis Abundance at TCRMP Sites on St. John") +
  labs(x= "Year", y= "Stegates variabilis Mean Abundance") + 
  labs(caption = "Figure No. ?. Average Stegates variabilis Abundance at TCRMP Sites on St. John") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(TSTJ_ABUNDANCE_SV_LPLOT)

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
    "STT" = "coral",
    "STJ"   = "green2",
    "STX"  = "cyan2")) +
  labs(title = "Stegates variabilis Mean Abundance Across TCRMP Sites From 2013-2025") +
  labs(caption = "Stegates variabilis Mean Abundance on St. Thomas(STT) , St. Croix(STX), and St. John(STJ).") +
  labs(x = "Island" , y = "Stegates variabilis Mean Abundance log(Abundance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## For Stegates variabilis abundance, St. John and St. Thomas are significantly different to one another, but St. Croix is not. 