##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº 2:Descriptive Figures (PELA)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(ggpubr)
library(ggthemes)
library(MetBrewer)
library(sjlabelled)
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASETS

argentina  <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Argentina_2011_2015.sav")
bolivia    <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Bolivia_2015_2020.sav")
brasil     <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Brasil_2011_2014.sav")
chile      <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Chile_2014_2018.sav")
colombia   <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Colombia_2018_2022.sav")
costarica  <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Costa_Rica_2018_2022.sav")
ecuador    <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Ecuador_2017_2021.sav")
elsalvador <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/El_Salvador_2018_2021.sav")
guatemala  <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Guatemala_2016_2020.sav")
honduras   <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Honduras_2018_2022.sav")
mexico     <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Mexico_2018_2021.sav")
#nicaragua  <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Nicaragua_2018_2022.sav", enc = "UTF-8")
panama     <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Panama_2019_2023.sav")
paraguay   <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Paraguay_2013_2018.sav")
peru       <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Peru_2016_2021.sav")
repdom     <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Republica_Dominicana_2016_2021.sav")
uruguay    <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Uruguay_2015_2020.sav")
venezuela  <- read_data(path = "0A_Datasets/2_PELA/0_Ultimo_Periodo/Venezuela_2016_2021.sav")


##########################################################################################################################################
#STEP 2: RECODING VARIABLES

#Recode Education 
argentina$educ  <- car::recode(argentina$SOCD7,  "1:4=0;5:6=1;8:9=0;98:99=0")
bolivia$educ    <- car::recode(bolivia$SOCD7,    "1:4=0;5:6=1;8:9=0;98:99=0")
brasil$educ     <- car::recode(brasil$SOCD7,     "1:4=0;5:6=1;8:9=0;98:99=0")
chile$educ      <- car::recode(chile$SOCD7,      "1:4=0;5:6=1;8:9=0;98:99=0")
colombia$educ   <- car::recode(colombia$SOCD7,   "1:4=0;5:6=1;8:9=0;98:99=0")
costarica$educ  <- car::recode(costarica$SOCD7,  "1:4=0;5:6=1;8:9=0;98:99=0")
ecuador$educ    <- car::recode(ecuador$SOCD7,    "1:4=0;5:6=1;8:9=0;98:99=0")
elsalvador$educ <- car::recode(elsalvador$SOCD7, "1:4=0;5:6=1;8:9=0;98:99=0")
guatemala$educ  <- car::recode(guatemala$SOCD7,  "1:4=0;5:6=1;8:9=0;98:99=0")
honduras$educ   <- car::recode(honduras$SOCD7,   "1:4=0;5:6=1;8:9=0;98:99=0")
mexico$educ     <- car::recode(mexico$SOCD7,     "1:4=0;5:6=1;8:9=0;98:99=0")
panama$educ     <- car::recode(panama$SOCD7,     "1:4=0;5:6=1;8:9=0;98:99=0")
paraguay$educ   <- car::recode(paraguay$SOCD7,   "1:4=0;5:6=1;8:9=0;98:99=0")
peru$educ       <- car::recode(peru$SOCD7,       "1:4=0;5:6=1;8:9=0;98:99=0")
repdom$educ     <- car::recode(repdom$SOCD7,     "1:4=0;5:6=1;8:9=0;98:99=0")
uruguay$educ    <- car::recode(uruguay$SOCD7,    "1:4=0;5:6=1;8:9=0;98:99=0")
venezuela$educ  <- car::recode(venezuela$SOCD7,  "1:4=0;5:6=1;8:9=0;98:99=0")

#Create Labels
data <- data.frame(country = c("Argentina (2011-2015)",
                               "Bolivia (2015-2020)",
                               "Brazil (2011-2014)",
                               "Chile (2014-2018)",
                               "Colombia(2018-2022)",
                               "Costa Rica (2018-2022)",
                               "Ecuador (2017-2021)",
                               "El Salvador (2018-2021)",
                               "Guatemala (2016-2020)",
                               "Honduras (2018-2022)",
                               "Mexico (2018-2021)",
                               "Panama (2019-2023)",
                               "Paraguay (2013-2018)",
                               "Peru (2016-2021)",
                               "Dominican Republic (2016-2021)",
                               "Uruguay (2015-2020)",
                               "Venezuela (2016-2021)"))

#Occupational Level of Legislators
data$leg_occup <- c(7.46,19.35,2.31,5.88,8.11,4.55,1.13,5.06,3.85,7.61,8.73,8.19,1.82,2.74,0.00,13.04,7.46)

#Education Level of Legislators
data$leg_educ <- c(round(table(argentina$educ)[2]/dim(argentina)[1]*100,3),
               round(table(bolivia$educ)[2]/dim(bolivia)[1]*100,3),
               round(table(brasil$educ)[2]/dim(brasil)[1]*100,3),
               round(table(chile$educ)[2]/dim(chile)[1]*100,3),
               round(table(colombia$educ)[2]/dim(colombia)[1]*100,3),
               round(table(costarica$educ)[2]/dim(costarica)[1]*100,3),
               round(table(ecuador$educ)[2]/dim(ecuador)[1]*100,3),
               round(table(elsalvador$educ)[2]/dim(elsalvador)[1]*100,3),
               round(table(guatemala$educ)[2]/dim(guatemala)[1]*100,3),
               round(table(honduras$educ)[2]/dim(honduras)[1]*100,3),
               round(table(mexico$educ)[2]/dim(mexico)[1]*100,3),
               round(table(panama$educ)[2]/dim(panama)[1]*100,3),
               round(table(paraguay$educ)[2]/dim(paraguay)[1]*100,3),
               round(table(peru$educ)[2]/dim(peru)[1]*100,3),
               round(table(repdom$educ)[2]/dim(repdom)[1]*100,3),
               round(table(uruguay$educ)[2]/dim(uruguay)[1]*100,3),
               round(table(venezuela$educ)[2]/dim(venezuela)[1]*100,3))

#Occupational Level of Legislators
data$pob_occup <- c(46.2,65.8,52.3,49.2,52.2,34,49.4,43.4,35.5,34.8,54.3,34.5,65.8,62.2,37.8,54.2,69.5)


#Education Level of General Population
data$pob_educ   <- c(11.15,7.96, 7.058, 16.9, 12.96, 14.2, 14.55, 6.41, 5.04, 3.28, 15.697, 14.49, NA, 15.24, 15.36, 14.53, NA)

#Education Level of Adult Population
data$pob17_educ <- c(15.08, 11.76, 9.296, 21.7, 16.52, 20.9, 20.71, 9.58, 7.12,5.28, 21.52, 20.48,NA, 20.86, 22.53, 18.75, NA)

data %>% na.omit() %>%
  mutate(dif = pob_occup - leg_occup,
         country1 = fct_reorder(country, dif, max),
         group = "Occupation") %>% 
ggplot() +
  geom_bar(mapping = aes(x= country1, y = dif), stat = "identity", fill = "#dd5129") + 
  coord_flip() +
  ylim(0,100) +
  facet_wrap(~group) + 
  labs(x = "Country", y = "Occupational Gap (%)") + 
  theme_light() + 
  theme(plot.title = element_text(colour="black", size=14, face="bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) -> fig1a

data %>% na.omit() %>%
  mutate(dif =  leg_educ - pob17_educ,
         country1 = fct_reorder(country, dif, max),
         group = "Education") %>% 
  ggplot() +
  geom_bar(mapping = aes(x= country1, y = dif), stat = "identity", fill = "#0f7ba2") + 
  coord_flip() +
  ylim(0,100) +
  facet_wrap(~group) + 
  labs(x = "Country", y = "Educational Gap (%)") + 
  theme_light() + 
  theme(plot.title = element_text(colour="black", size=14, face="bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black")) -> fig1b

### Create Figure 1
annotate_figure(ggarrange(fig1a, fig1b,
                          common.legend = TRUE,
                          ncol = 2, nrow = 1,
                          legend = "bottom"),
                bottom = text_grob("Source: PELA (2021), LatinoBarometer (2020), National Census.", 
                                   color = "black",hjust = 1, x = 1, 
                                   face = "italic", size = 10))
ggsave(filename = "Latin_America_Gaps.png", plot = last_plot(),
       device = "png", path = "0B_Figures/2_PELA/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

##########################################################################################################################################
##TECHNICAL DETAILS FOR REPLICATION

#Macbook Pro 13 inch 2017
#mac OS Big Sur 11.2.3
#
#A .Rproj file was used in the development of this Problem Set.
#> sessionInfo() 