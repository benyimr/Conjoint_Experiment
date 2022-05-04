##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº 1:Descriptive Figures (V-DEM)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(ggcorrplot)    
library(ggthemes)
library(tayloRswift)
library(rnaturalearth)
library(rnaturalearthdata)
library(sjlabelled)
library(sjmisc)
library(sf)
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASETS

## Load V-Dem
vdem   <- read_data(path = "0A_Datasets/1_VDEM/V-Dem-CY-Full+Others-v12.dta", enc = "UTF-8")

## Load World (shape file)
world  <- ne_countries(scale = "medium", returnclass = "sf")


##########################################################################################################################################
#STEP 2: DATA PROCESSING

## Variables (information from Codebook)

#v2lgdsadlo      Representation of disadvantaged social groups 
#                Type C (Variables coded by Country Experts)
#                Considering all disadvantaged social groups in the country, how well represented are these groups, as a whole,
                 #in the national legislature?
#                Disadvantage refers to socioeconomic disadvantage. Specifically, in order to be considered disadvantaged 
#                members of a social group must have an average income that is significantly below the median national income.
#                0 (1): They have no representation at all.
#                1 (2): They are highly under-represented relative to their proportion of the general population.
#                2 (3): They are slightly under-represented relative to their proportion of the general population.
#                3 (4): They are represented roughly equal relative to their proportion of the general population.
#                4 (5): They are over-represented relative to their proportion of the general population.
                 
#v2lgdsadlobin   Representation of disadvantaged social groups binary (C) 
#                Type C (Variables coded by Country Experts)
#                Are there disadvantaged groups in the society?
#                Disadvantage refers to socioeconomic disadvantage. Specifically, in order to be considered disadvantaged 
#                members of a social group must have an average income that is significantly below the median national income.
#                0: No.
#                1: Yes.

#v2pepwrses      Power distributed by socioeconomic position
#                Type C (Variables coded by Country Experts)
#                Is political power distributed according to socioeconomic position?
#                All societies are characterized by some degree of economic (wealth and income) inequality. In some societies, 
#                income and wealth are distributed in a grossly unequal fashion. In others, the difference between rich and 
#                poor is not so great. Here, we are concerned not with the degree of social inequality but rather with the 
#                political effects of this inequality. Specifically, we are concerned with the extent to which wealth and 
#                income translates into political power.
#                0: Wealthy people enjoy a virtual monopoly on political power. Average and poorer people have almost no 
#                   influence.
#                1: Wealthy people enjoy a dominant hold on political power. People of average income have little say. Poorer 
#                   people have essentially no influence.
#                2: Wealthy people have a very strong hold on political power. People of average or poorer income have some 
#                   degree of influence but only on issues that matter less for wealthy people.
#                3: Wealthy people have more political power than others. But people of average income have almost as much 
#                   influence and poor people also have a significant degree of political power.
#                4: Wealthy people have no more political power than those whose economic status is average or poor. Political 
#                   power is more or less equally distributed across economic groups.

#v2xpe_exlecon   Exclusion by Socio-Economic Group
#                Type D (Indices)
#                Index of (political) exclusion by socio-economic group
#                Exclusion is when individuals are denied access to services or participation in governed spaces (spaces that 
#                are part of the public space and the government should regulate, while excluding private spaces and 
#                organizations except when exclusion in those private spheres is linked to exclusion in the public sphere) 
#                based on their identity or belonging to a particular group. The point estimates for this index have been 
#                reversed such that the directionality is opposite to the input variables. That is, lower scores indicate a 
#                normatively better situation (e.g. more democratic) and higher scores a normatively worse situation (e.g. 
#                less democratic). Note that this directionality is opposite of that of other V-Dem indices, which generally 
#                run from normatively worse to better.
#                Interval, from low to high (0-1).  

#v2peapsecon     Access to public services distributed by socio-economic position
#                Type C (Variables coded by Country Experts)
#                Is access to basic public services, such as order and security, primary education, clean water, and 
#                healthcare, distributed equally according to socioeconomic position?
#                This question asks if socio-economic position is an important cleavage in society for the distribution of 
#                public services. Thus, if there are inequalities in access to public services, but these are not mainly due 
#                to differentiation between particular socio-economic position, the code should be “4” (equal). The situation 
#                could of course vary by type of public service, such that a socio-economic group is denied access to some 
#                basic public services but not others. Please base your response on whether access to most of the 
#                aforementioned services are distributed equally or unequally.
#                0: Extreme. Because of poverty or low income, 75 percent (%) or more of the population lack access to basic 
#                   public services of good quality.
#                1: Unequal. Because of poverty or low income, 25 percent (%) or more of the population lack access to basic 
#                   public services of good quality.
#                2: Somewhat Equal. Because of poverty or low income, 10 to 25 percent (%) of the population lack access to 
#                   basic public services of good quality.
#                3: Relatively Equal. Because of poverty or low income, 5 to 10 percent (%) of the population lack access to 
#                   basic public services of good quality.
#                4: Equal. Because of poverty or low income, less than 5 percent (%) of the population lack access to basic 
#                   public services of good quality.

#v2peasjsoecon   Access to state jobs by socio-economic position
#                Type C (Variables coded by Country Experts)
#                Are state jobs equally open to qualified individuals regardless of socio-economic position?
#                Socio-economic position defines groups based on attributes of wealth, occupation, or other economic 
#                circumstances such as owning property.
#                0: Extreme. Because of poverty or low income, 75 percent (%) or more of the population, even if qualified, 
#                   lack access to state jobs.
#                1: Unequal. Because of poverty or low income, makes 25 percent (%) or more of the population, even if 
#                   qualified, lack access to state jobs.
#                2: Somewhat Equal. Because of poverty or low income, 10 to 25 percent (%) of the population, even if 
#                   qualified, lack access to state jobs.
#                3: Relatively Equal. Because of poverty or low income, 5 to 10 percent (%) of the population, even if 
#                   qualified, lack access to state jobs.
#                4: Equal. Because of poverty or low income, less than 5 percent (%) of the population, even if qualified, 
#                   lack access to state jobs.

#v2peasbecon     Access to state business opportunities by socio-economic position
#                Type C (Variables coded by Country Experts)
#                Are state business opportunities equally available to qualified individuals regardless of socio-economic 
#                position?
#                State business opportunities refer to the ability to compete for or receive a public procurement contract, 
#                to partner with the government in public-private partnerships, etc.
#                Socio-economic position defines groups based on attributes of wealth, occupation, or other economic 
#                circumstances such as owning property.
#                0: Extreme. Because of poverty or low income makes 75 percent (%) or more of the population, even if 
#                   qualified, lack access to state business opportunities.
#                1: Unequal. Because of poverty or low income makes 25 percent (%) or more of the population, even if 
#                   qualified, lack access to state business opportunities.
#                2: Somewhat Equal. Because of poverty or low income makes 10 to 25 percent (%) of the population, even if 
#                   qualified, lack access to state business opportunities.
#                3: Relatively Equal. Because of poverty or low income makes 5 to 10 percent (%) of the population, even if 
#                   qualified, lack access to state business opportunities.
#                4: Equal. Because of poverty or low income makes less than 5 percent (%) of the population, even if 
#                   qualified, lack access to state business opportunities.

#v2clacjust      Social class equality in respect for civil liberty
#                Type C (Variables coded by Country Experts)
#                Do poor people enjoy the same level of civil liberties as rich people do?
#                This question specifies the extent to which the level of civil liberties is generally the same across 
#                socioeconomic groups so that people with a low social status are not treated worse than people with high 
#                social status. Here, civil liberties are understood to include access to justice, private property rights, 
#                freedom of movement, and freedom from forced labor.
#                0: Poor people enjoy much fewer civil liberties than rich people.
#                1: Poor people enjoy substantially fewer civil liberties than rich people.
#                2: Poor people enjoy moderately fewer civil liberties than rich people.
#                3: Poor people enjoy slightly fewer civil liberties than rich people.
#                4: Poor people enjoy the same level of civil liberties as rich people.

## Variables Version

#Model Estimates (Measurement Model Output): This version has no special suffix (e.g. v2elmulpar). This version of the 
#                variables provides country–year (country–date in the alternative dataset) point estimates from the V-Dem
#                measurement model (see Pemstein et al. 2019). The measurement model aggregates the ratings provided by 
#                multiple country experts and, taking disagreement and measurement error into account, produces a probability 
#                distribution over country–year scores on a standardized interval scale (see the V-Dem Methodology document). 
#                The point estimates are the median values of these distributions for each country–year. The scale of a 
#                measurement model variable is similar to a normal ("Z") score (e.g. typically between -5 and 5, with 0
#                approximately representing the mean for all country–years in the sample) though it does not necessarily 
#                follow a normal distribution. For most purposes, these are the preferred versions of the variables for time 
#                series regression and other estimation strategies.

#Model Estimates Measure of Uncertainty (Measurement Model Highest Posterior Density (HPD) Intervals): This version has the 
#                suffixes: "codelow" and "codehigh" (e.g. v2elmulpar_codelow and v2elmulpar_codehigh). These two kinds of 
#                variables ["code low" and "code high"] demarcate the interval in which the measurement model places 68 
#                percent of the probability mass for each country–year score, which is approximately equivalent to one 
#                standard deviation upper and lower bounds. If the underlying posterior distribution is skewed, the HPDs 
#                reflect this with unequal distances between the point estimate and the high and low estimates. We also 
#                provide a standard calculation for standard deviation which is marked with the suffix "sd" (e.g., 
#                v2elmulpar_sd). The SD might be used to compute the standard frequentist confidence intervals.

#Original Scale (Linearized Original Scale Posterior Prediction): This version has the suffix “_osp,” (e.g. v2elmulpar_osp). 
#               In this version of the variables, we have linearly translated the measurement model point estimates back to the 
#               original ordinal scale of each variable (e.g. 0–4 for v2elmulpar_osp) as an interval measure. The decimals in 
#               the _osp version roughly indicate the distance between the point estimate from the linearized measurement model 
#               posterior prediction and the threshold for reaching the next level on the original ordinal scale. Thus, a _osp 
#               value of 1.25 indicates that the median measurement model posterior predicted value was closer to the ordinal 
#               value of 1 than 2 on the original scale. Technically, it calculates the sum of the posterior probabilities that 
#               the estimate is in a particular category: If a particular country–year-variable has a probability of 90% to be 
#               in category “4”, a 10% probability of being in category “3”, and 0% probability of being in categories “2”, 
#               “1”, and “0”, the result is a value of 3.9 (4   0.9+3   0.1 = 3.6+0.3). Since there is no conventional 
#               theoretical justification for linearly mapping ordinal posterior predictions onto an interval scale, these 
#               scores should primarily be used for heuristic purposes. Using the “Ordinal Scale” estimates—or incorporating 
#               the properties of ordinal probit models into the estimation procedure—is thus preferable to using the _osp 
#               estimates in statistical analyses. However, since the _osp version maps onto the coding criteria found in the 
#               V-Dem Codebook, and is strongly correlated with the Measurement Model output (typically at .98 or higher), 
#               some users may find the _osp version useful in estimating quantities such as marginal effects with a clear 
#               substantive interpretation. If a user uses _osp data in statistical analyses it is imperative that she confirm 
#               that the results are compatible with estimations using Measurement Model output.

# Original Scale Measure of Uncertainty (Linearized Original Scale HPD Intervals): This version has the suffixes – "codelow" 
#                                       and "codehigh" (e.g. v2elmulpar_osp_codelow and v2elmulpar_osp_codehigh). We estimate 
#                                       these quantities in a similar manner as the Measurement Model Highest Posterior Density 
#                                       Intervals. These two variables ["code low" and "code high"] demarcate the interval in 
#                                       which the measurement model places 70 percent of the probability mass for each 
#                                       country–year score, which is approximately equivalent to one standard deviation upper 
#                                       and lower bounds. If the underlying posterior distribution is skewed, the HPDs reflect 
#                                       this with unequal distances between the point estimate and the high and low estimates.
#                                       We also provide a standard calculation for standard deviation which is marked with the 
#                                       suffix "sd" (e.g., v2elmulpar_sd). The SD might be used to compute the standard 
#                                       frequentist confidence intervals.

#Ordinal Scale (Measurement Model Estimates of Original Scale Value): This version has the suffix "_ord" (e.g. v2elmulpar_ord). 
#              This method translates the measurement model estimates back to the original ordinal scale of a variable (as 
#              represented in the Codebook) after taking coder disagreement and measurement error into account. More precisely, 
#              it represents the most likely ordinal value on the original codebook scale into which a country–year would fall, 
#              given the average coder’s usage of that scale. More specifically, we assign each country–year a value that 
#              corresponds to its integerized median ordinal highest posterior probability category over Measurement Model 
#              output.

#Ordinal Scale Measure of Uncertainty (Original Scale Value HPD Intervals): This version has the suffixes - "codelow" and 
#                                     "codehigh" (e.g. v2elmulpar_ord_codelow and v2elmulpar_ord_codehigh). We estimate these 
#                                     values in a similar manner as the Measurement Model Highest Posterior Density Intervals. 
#                                     These two variables ["code low" and "code high"] demarcate the interval in which the 
#                                     measurement model places 70 percent of the probability mass for each country–year score, 
#                                     which is approximately equivalent to one standard deviation upper and lower bounds. If 
#                                     the underlying posterior distribution is skewed, the HPDs reflect this with unequal 
#                                     distances between the point estimate and the high and low estimates. We also provide a 
#                                     standard calculation for standard deviation which is marked with the suffix "sd" (e.g. 
#                                     v2elmulpar_sd). The SD might be used to compute the standard frequentist confidence 
#                                     intervals.

#Number of Coders per Country, Variable and Year/Date (*_nr): The number of V-Dem Country Experts (regular coders, bridge- and 
#                                                             lateral coders) who provided data on country, variable and year. 
#                                                             V-Dem’s methodology is based on the assumption that we have a 
#                                                             minimum of five Country Experts for every single 
#                                                             country–variable-year. Sometimes, however, we end up with fewer 
#                                                             than five Country Experts. From v7 of the Country–Year, and the 
#                                                             Country-Date type datasets, we provide all data we have for full 
#                                                             transparency. By providing the number of Country Experts for each 
#                                                             country–variable-year/date, we suggest that users primarily base 
#                                                             analyses on observations based on five or more coders. We 
#                                                             strongly advise against using observations based on three or 
#                                                             fewer coders. This concerns all C type variables.

#Arithmetic Mean of Coder Answers per Country–Year (*_mean): It is commonplace to aggregate respondents’ data to the level of 
#                                                            country or country–year using arithmetic mean in order to merge it
#                                                            with other country–level data. V–Dem Institute provides such 
#                                                            variables for every expert–coded variable aggregated by the 
#                                                            Measurement Model in Country–Date/Year dataset.

# Variable Tag
# Prefix + Index (if V-Dem index) + Section + Abbreviated title

#v2: V-Dem variables (A, B, C)
#v2x[two-letter designation]_: Indices specific for certain areas (see below). For example, v2xel_ would be an index in the 
#                              election-specific area. Sometimes used in aggregations of higher-level indices (i.e. v2x_ type 
#                              indices D)

#cl: Civil liberty
#lg: Legislature
#pe: Political equality


## Select relevant variables, filter latest year (2021).
vdem %>% filter(year == 2021) %>% 
  select(country_name:COWcode,
         contains("v2lgdsadlo"), #v2lgdsadlobin
         contains("v2pepwrses"),
         contains("v2xpe_exlecon"),
         contains("v2peapsecon"),
         contains("v2peasjsoecon"),
         contains("v2peasbecon"),
         contains("v2clacjust"),
         contains("v2x_polyarchy"),
         contains("v2x_libdem"),
         contains("v2x_partipdem"),
         contains("v2x_delibdem"),
         contains("v2x_egaldem"),
         e_gdp,e_gdppc,
         e_regiongeo,e_regionpol,e_regionpol_6C)-> vdem1

## Store output
write_rds(vdem1, file = "0A_Datasets/1_VDEM/VDEM_Selected_2021.rds")


## Recode country (necessary for merge)
vdem1$temp <- car::recode(vdem1$country_name,"'Burma/Myanmar'='Myanmar';'United States of America'='United States';
                          'South Sudan'='S. Sudan';'North Korea'='Dem. Rep. Korea';'South Korea'='Korea';
                          'Central African Republic'='Central African Rep.';
                          'Democratic Republic of the Congo'='Dem. Rep. Congo';'Republic of the Congo'='Congo';
                          'Dominican Republic'='Dominican Rep.';'The Gambia'='Gambia';'Laos'='Lao PDR';
                          'Eswatini'='Swaziland';'Palestine/Gaza'='Palestine';
                          'Bosnia and Herzegovina'='Bosnia and Herz.';'Czech Republic'='Czech Rep.';
                          'Equatorial Guinea'='Eq. Guinea';'North Macedonia'='Macedonia';
                          'Sao Tome and Principe'='São Tomé and Principe';'Solomon Islands'='Solomon Is.'")
vdem1$name <- ifelse(vdem1$temp == "Ivory Coast", "Côte d'Ivoire", vdem1$temp)


##########################################################################################################################################
#STEP 3: UNIVARIATE FREQUENCIES

### NOTE: We therefore strongly advise against using point estimates for country-variable-years with three or fewer ratings.

## Representation of disadvantaged social groups
frq(vdem1$v2lgdsadlo_ord)

### Filter based on number of experts
vdem1 %>% filter(v2lgdsadlo_nr > 3) %>% frq(v2lgdsadlo_ord) #6+86=92 (58.97%)


## Power distributed by socioeconomic position
frq(vdem1$v2pepwrses_ord)

### Filter based on number of experts
vdem1 %>% filter(v2pepwrses_nr > 3) %>% frq(v2pepwrses_ord) # 12+33+63=108 (64.29%)


## Exclusion by Socio-Economic Group
frq(vdem1$v2xpe_exlecon)

### Filter based on number of experts
vdem1 %>% filter(v2pepwrses_nr > 3,v2clacjust_nr>3,v2peapsecon_nr>3,v2peasjsoecon_nr>3,v2peasbecon_nr>3) %>% 
  select(v2xpe_exlecon) %>% frq()
vdem1 %>% filter(v2pepwrses_nr > 3,v2clacjust_nr>3,v2peapsecon_nr>3,v2peasjsoecon_nr>3,v2peasbecon_nr>3) %>% 
  select(v2xpe_exlecon) %>%
  group_var(size = 0.1) %>%
  set_labels(
    labels = c("[0.0-0.1]"=1,"[0.1-0.2]"=2,"]0.2-0.3]"=3,
               "]0.3-0.4]"=4,"]0.4-0.5]"=5,"]0.5-0.6]"=6,
               "]0.6-0.7]"=7,"]0.7-0.8]"=8,"]0.8-0.9]"=9,
               "]0.9-1.0]"=10)) %>%
  select(v2xpe_exlecon_gr) %>%
  frq() 


## Access to public services distributed by socio-economic position
frq(vdem1$v2peapsecon_ord)

### Filter based on number of experts
vdem1 %>% filter(v2peapsecon_nr > 3) %>% frq(v2peapsecon_ord) 


## Access to state jobs by socio-economic position
frq(vdem1$v2peasjsoecon_ord)

### Filter based on number of experts
vdem1 %>% filter(v2peasjsoecon_nr > 3) %>% frq(v2peasjsoecon_ord) 


## Access to state business opportunities by socio-economic position
frq(vdem1$v2peasbecon_ord)

### Filter based on number of experts
vdem1 %>% filter(v2peasbecon_nr > 3) %>% frq(v2peasbecon_ord) 


## Social class equality in respect for civil liberty
frq(vdem1$v2clacjust_ord)

### Filter based on number of experts
vdem1 %>% filter(v2clacjust_nr > 3) %>% frq(v2clacjust_ord) 


##########################################################################################################################################
#STEP 4: MERGING DATASET

## Merging
world1 <- left_join(x = world, y = vdem1, by = "name")

## Recoding variables

### Representation of disadvantaged social groups (Version 1)
world1$representation_00 <- ifelse(world1$v2lgdsadlo_nr > 3, yes = world1$v2lgdsadlo_ord,no = NA)
world1$representation_01 <- car::recode(world1$representation_00,"0='None';1='Highly under-represented';
                                        2='Slightly under-represented';3='Adequately represented';
                                        4='Over-represented';NA='No Information'")
world1$representation_01 <- factor(world1$representation_01,
                                   levels = c("None","Highly under-represented","Slightly under-represented",
                                              "Adequately represented","Over-represented","No Information"))

### Representation of disadvantaged social groups (Version 2)
world1$representation_02 <- car::recode(world1$representation_00,"c(0,1)='High Under-representation';
                                        2='Low Under-representation';3='Adequate Representation';
                                        4='Over-Representation';NA='No Information'")
world1$representation_02 <- factor(world1$representation_02,
                                   levels = c("High Under-representation","Low Under-representation",
                                              "Adequate Representation","Over-Representation","No Information"))


### Power distributed by socioeconomic position (Version 1)
world1$power_00 <- ifelse(world1$v2pepwrses_nr > 3, yes = world1$v2pepwrses_ord,no = NA)
world1$power_01 <- car::recode(world1$power_00,"0='Monopoly';1='Dominant hold';2='Strong hold';
                               3='Weak hold';4='Equality';NA='No Information'")
world1$power_01 <- factor(world1$power_01,levels = c("Monopoly","Dominant hold","Strong hold",
                                               "Weak hold","Equality","No Information"))

### Power distributed by socioeconomic position (Version 2)
world1$power_02 <- car::recode(world1$power_00,"c(0,1)='Highly Unequal';2='Moderately Unequal';
                               3='Slightly Unequal';4='Equality';NA='No Information'")
world1$power_02 <- factor(world1$power_02,levels = c("Highly Unequal","Moderately Unequal",
                                                     "Slightly Unequal","Equality","No Information"))


### Exclusion by Socio-Economic Group (Ordinal Version)
world1 %>%
  mutate(temp1 = ifelse(v2pepwrses_nr    > 3, 1, 0),
         temp2 = ifelse(v2clacjust_nr    > 3, 1, 0),
         temp3 = ifelse(v2peapsecon_nr   > 3, 1, 0),
         temp4 = ifelse(v2peasjsoecon_nr > 3, 1, 0),
         temp5 = ifelse(v2peasbecon_nr   > 3, 1, 0),
         temp6 = temp1+temp2+temp3+temp4+temp5,
         exclusion_01 = ifelse(temp6==5, v2xpe_exlecon, NA),
         exclusion_02 = car::recode(exclusion_01,"0:0.2='Very Low';
                                    0.2:0.4='Low';0.4:0.6='Moderate';
                                    0.6:0.8='High';0.8:1.0='Very High';
                                    NA='No Information'")) -> world1
world1$exclusion_02 <- factor(world1$exclusion_02,levels = c("Very Low","Low","Moderate",
                                                     "High","Very High","No Information"))


## Store output
write_rds(world1, file = "0A_Datasets/1_VDEM/World_Map_VDEM.rds")


##########################################################################################################################################
#STEP 5: WORLD MAP

## Set the Theme
theme_set(theme_bw())

## Representation 1
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = representation_01), color = "white", lwd=0.1) + 
  labs(fill = "Level of Representation", caption = "Source: V-Dem (2022).") +
  scale_fill_manual(values = c("#f2f0f7","#cbc9e2","#9e9ac8","#6a51a3","gray")) +
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Representation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Representation 2
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = representation_02),color = "white", lwd=0.1) + 
  labs(fill = "Level of Representation", caption = "Source: V-Dem (2022).") +
  scale_fill_manual(values = c("#cbc9e2","#9e9ac8","#6a51a3","gray")) +
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Representation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


## Power 1
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = power_01),color = "white", lwd=0.1) + 
  labs(fill = "Power Position of the Wealthiest", caption = "Source: V-Dem (2022).") +
  scale_fill_manual(values = c("#f2f0f7","#cbc9e2","#9e9ac8","#6a51a3","gray")) +
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Power_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Power 2
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = power_02),color = "white", lwd=0.1) + 
  labs(fill = "Power Position of the Wealthiest", caption = "Source: V-Dem (2022).") +
  scale_fill_manual(values = c("#cbc9e2","#9e9ac8","#6a51a3","gray")) +
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Power_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


## Exclusion 1
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = exclusion_01), color = "white", lwd=0.1) + 
  labs(fill = "Exclusion by Socio-Economic Group", caption = "Source: V-Dem (2022).") +
  scale_fill_distiller() + 
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Exclusion_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Exclusion 2
ggplot(data = world1) +
  geom_sf(mapping = aes(fill = exclusion_02), color = "white", lwd=0.1) + 
  labs(fill = "Exclusion by Socio-Economic Group", caption = "Source: V-Dem (2022).") +
  scale_fill_manual(values = c("#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d","gray")) +
  scale_y_continuous(limits=c(-55,90)) + 
  theme(plot.title = element_text(colour="black", size=22, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour="black", size=18, hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Map_Exclusion_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


##########################################################################################################################################
#STEP 6: CORRELATION MATRIX

## Correlation Matrix
vdem1 %>%
  filter(v2lgdsadlo_nr > 3,v2pepwrses_nr > 3,v2peapsecon_nr > 3,
         v2peasjsoecon_nr > 3,v2peasbecon_nr > 3,v2clacjust_nr > 3) %>%
  select(v2lgdsadlo,v2pepwrses,v2xpe_exlecon,
         v2peapsecon,v2peasjsoecon,v2peasbecon,
         v2clacjust) %>%
  rename('Political Representation' = v2lgdsadlo,
         'Distribution of Power' = v2pepwrses,
         'Political Exclusion' = v2xpe_exlecon,
         'Public Services' = v2peapsecon,
         'State Jobs' = v2peasjsoecon,
         'Business Opportunities' = v2peasbecon,
         'Civil Liberties' = v2clacjust) %>%
  cor() %>% round(.,1)%>%
  ggcorrplot(method = "circle",
             type = "upper",
             legend.title = "Correlation Coefficient",
             insig = "blank")  +
  labs(x = "", y = "",
       caption = "Source: V-Dem (2021).") +
  theme_few() + 
  theme(legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.spacing.x = unit(1, "cm"),
        legend.spacing.y = unit(0.5, "cm")) 


##########################################################################################################################################
# STEP 7: TEMPORAL TRENDS

## Recode region (continent)
vdem$region <- car::recode(vdem$country_name, "c('Afghanistan','Armenia','Azerbaijan','Bahrain','Bangladesh', 'Bhutan',
                    'Burma/Myanmar','China','Timor-Leste','Georgia','Hong Kong','India','Indonesia','Iran','Iraq','Israel','Japan',
                    'Jordan','Kazakhstan','Kuwait','Kyrgyzstan','Laos','Lebanon','Malaysia','Maldives','Mongolia','Nepal','North Korea',
                    'Oman','Pakistan','Palestine/British Mandate','Palestine/Gaza','Palestine/West Bank','Philippines','Qatar',
                    'Republic of Vietnam','Saudi Arabia','Russia','Singapore','South Korea','South Yemen','Sri Lanka','Syria',
                    'Taiwan','Tajikistan','Thailand','Turkey','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen')='Asia'; 
                    c('Albania','Austria','Belarus','Belgium','Bosnia and Herzegovina','Bulgaria','Croatia','Cyprus','Czech Republic',
                    'Denmark','Estonia','Finland','France','German Democratic Republic','Germany','Greece','Hungary','Iceland','Ireland',
                    'Italy','Kosovo','Latvia','Lithuania','Luxembourg','Malta','Moldova','Montenegro','Netherlands','North Macedonia',
                    'Norway','Poland','Portugal','Romania','Serbia','Slovakia','Slovenia','Spain','Sweden','Switzerland','Ukraine',
                    'United Kingdom')='Europe';c('Algeria','Angola','Benin','Botswana','Burkina Faso','Burundi','Cambodia','Cameroon',
                    'Cape Verde','Central African Republic','Chad','Comoros','Democratic Republic of the Congo','Djibouti',
                    'Equatorial Guinea','Eritrea','Egypt','Eswatini','Ethiopia','Gabon','Ghana','Guinea','Guinea-Bissau','Ivory Coast',
                    'Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania','Mauritius','Morocco','Mozambique',
                    'Namibia','Niger','Nigeria','Republic of the Congo','Rwanda','Sao Tome and Principe','Senegal','Seychelles',
                    'Sierra Leone','Somalia','Somaliland','South Africa','South Sudan','Sudan','Tanzania','The Gambia','Togo','Tunisia',
                    'Uganda','Zambia','Zanzibar','Zimbabwe')='Africa';c('Argentina','Barbados','Bolivia','Brazil','Canada','Chile',
                    'Colombia','Costa Rica','Cuba','Dominican Republic','Ecuador','El Salvador','Guatemala','Guyana','Haiti','Honduras',
                    'Jamaica','Mexico','Nicaragua','Panama','Paraguay','Peru','Suriname','Trinidad and Tobago','United States of America',
                    'Uruguay','Venezuela')='America';c('Australia','Fiji','New Zealand','Papua New Guinea','Solomon Islands',
                    'Vanuatu')='Oceania'")

## Select variables, summarise by country-year
vdem %>%  select(country_name:COWcode,region,e_regionpol_6C,
                 v2pepwrses,   v2pepwrses_ord,v2pepwrses_nr, 
                 v2lgdsadlo,   v2lgdsadlo_ord,v2lgdsadlo_nr,
                 v2xpe_exlecon,
                 v2peapsecon,  v2peapsecon_ord,v2peapsecon_nr,
                 v2peasjsoecon,v2peasjsoecon_ord,v2peasjsoecon_nr,
                 v2peasbecon,  v2peasbecon_ord,v2peasbecon_nr,
                 v2clacjust,   v2clacjust_ord, v2clacjust_nr) %>%
  filter(year>1979) %>%
  group_by(country_name,year) %>%
  summarise(power           = mean(v2pepwrses[v2pepwrses_nr>3],       na.rm = TRUE), 
            representation  = mean(v2lgdsadlo[v2lgdsadlo_nr>3&v2pepwrses_nr>3&v2peapsecon_nr>3&
                                                v2peasjsoecon_nr>3&v2peasbecon_nr>3&v2clacjust_nr>3], na.rm = TRUE),
            exclusion       = mean(v2xpe_exlecon[v2pepwrses_nr>3], na.rm = TRUE), 
            pub_services    = mean(v2peapsecon[v2peapsecon_nr>3],     na.rm = TRUE),
            state_jobs      = mean(v2peasjsoecon[v2peasjsoecon_nr>3], na.rm = TRUE), 
            state_business  = mean(v2peasbecon[v2peasbecon_nr>3],     na.rm = TRUE),
            civil_liberties = mean(v2clacjust[v2clacjust_nr>3],       na.rm = TRUE)) %>% droplevels() -> vdem2


## Select variables, summarise by region-year
vdem %>%  select(country_name:COWcode,region,e_regionpol_6C,
                 v2pepwrses,   v2pepwrses_ord,v2pepwrses_nr, 
                 v2lgdsadlo,   v2lgdsadlo_ord,v2lgdsadlo_nr,
                 v2xpe_exlecon,
                 v2peapsecon,  v2peapsecon_ord,v2peapsecon_nr,
                 v2peasjsoecon,v2peasjsoecon_ord,v2peasjsoecon_nr,
                 v2peasbecon,  v2peasbecon_ord,v2peasbecon_nr,
                 v2clacjust,   v2clacjust_ord, v2clacjust_nr) %>%
  filter(year>1979) %>%
  group_by(region,year) %>%
  summarise(power           = mean(v2pepwrses[v2pepwrses_nr>3],       na.rm = TRUE), 
            representation  = mean(v2lgdsadlo[v2lgdsadlo_nr>3&v2pepwrses_nr>3&v2peapsecon_nr>3&
                                                v2peasjsoecon_nr>3&v2peasbecon_nr>3&v2clacjust_nr>3], na.rm = TRUE),
            exclusion       = mean(v2xpe_exlecon[v2pepwrses_nr>3], na.rm = TRUE), 
            pub_services    = mean(v2peapsecon[v2peapsecon_nr>3],     na.rm = TRUE),
            state_jobs      = mean(v2peasjsoecon[v2peasjsoecon_nr>3], na.rm = TRUE), 
            state_business  = mean(v2peasbecon[v2peasbecon_nr>3],     na.rm = TRUE),
            civil_liberties = mean(v2clacjust[v2clacjust_nr>3],       na.rm = TRUE)) %>% droplevels() -> vdem3

## Select variables, global trend
vdem %>%  select(country_name:COWcode,region,e_regionpol_6C,
                 v2pepwrses,   v2pepwrses_ord,v2pepwrses_nr, 
                 v2lgdsadlo,   v2lgdsadlo_ord,v2lgdsadlo_nr,
                 v2xpe_exlecon,
                 v2peapsecon,  v2peapsecon_ord,v2peapsecon_nr,
                 v2peasjsoecon,v2peasjsoecon_ord,v2peasjsoecon_nr,
                 v2peasbecon,  v2peasbecon_ord,v2peasbecon_nr,
                 v2clacjust,   v2clacjust_ord, v2clacjust_nr) %>%
  filter(year>1979) %>%
  group_by(year) %>%
  summarise(power           = mean(v2pepwrses[v2pepwrses_nr>3],       na.rm = TRUE), 
            representation  = mean(v2lgdsadlo[v2lgdsadlo_nr>3&v2pepwrses_nr>3&v2peapsecon_nr>3&
                                                v2peasjsoecon_nr>3&v2peasbecon_nr>3&v2clacjust_nr>3], na.rm = TRUE),
            exclusion       = mean(v2xpe_exlecon[v2pepwrses_nr>3], na.rm = TRUE), 
            pub_services    = mean(v2peapsecon[v2peapsecon_nr>3],     na.rm = TRUE),
            state_jobs      = mean(v2peasjsoecon[v2peasjsoecon_nr>3], na.rm = TRUE), 
            state_business  = mean(v2peasbecon[v2peasbecon_nr>3],     na.rm = TRUE),
            civil_liberties = mean(v2clacjust[v2clacjust_nr>3],       na.rm = TRUE)) %>% droplevels() %>%
  mutate(region1 = "World") -> vdem4


### Combine datasets
vdem5 <- full_join(x = vdem3, y = vdem4, by = "year")
write_rds(x = vdem5, file = "0A_Datasets/1_VDEM/Temporal_Trends_VDEM.rds")


### Representation 1
ggplot(data = vdem5) + 
  geom_line(mapping = aes(x = year, y = representation.x, group = region, color = region), size = 1) + 
  geom_line(mapping = aes(x = year, y = representation.y, group = region1, color = region1), size = 2.5) + 
  geom_hline(mapping = aes(yintercept = 0.78), linetype = "longdash") +
  scale_color_brewer(palette="Spectral") + 
  labs(x = "Year", y = "Score", color = "Region",
       caption = "Source: V-Dem (2020).",
       subtitle = "Note: the dashed line marks the threshold between an average score of 'Highly under-represented \nor Less' and 'Slightly under-represented or More'.") + 
  ylim(-1.2,1.2) + 
  theme_few() + 
  theme(plot.title = element_text(colour="black", size=14, face="bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Trend_Representation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM", width = 50, height = 25, units = "cm", scale = 0.9)
  

### Power 1
ggplot(data = vdem5) + 
  geom_line(mapping = aes(x = year, y = power.x, group = region, color = region), size = 1) + 
  geom_line(mapping = aes(x = year, y = power.y, group = region1, color = region1), size = 2.5) + 
  geom_hline(mapping = aes(yintercept = 0.95), linetype = "longdash") +
  scale_color_brewer(palette="Spectral") +
  labs(x = "Year", y = "Score", color = "Region",
       caption = "Source: V-Dem (2020).",
       subtitle = "Note: the dashed line marks the threshold between an average score of 'Strong Hold \nor Less' and 'Weak Hold or More'.") + 
  ylim(-0.5,2) + 
  theme_few() + 
  theme(plot.title = element_text(colour="black", size=14, face="bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Trend_Power_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM", width = 50, height = 25, units = "cm", scale = 0.9)


### Exclusion 1
ggplot(data = vdem5) + 
  geom_line(mapping = aes(x = year, y = exclusion.x, group = region, color = region), size = 1) + 
  geom_line(mapping = aes(x = year, y = exclusion.y, group = region1, color = region1), size = 2.5) + 
  geom_hline(mapping = aes(yintercept = 0.42), linetype = "longdash") +
  scale_color_brewer(palette="Spectral") + 
  labs(x = "Year", y = "Score", color = "Region",
       caption = "Source: V-Dem (2020).",
       subtitle = "Note: the dashed line marks the Median.") +  
  ylim(0,1) + 
  theme_few() + 
  theme(plot.title = element_text(colour="black", size=14, face="bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(colour="black", size=10, face="bold"),
        legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
ggsave(filename = "World_Trend_Exclusion_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/1_VDEM", width = 50, height = 25, units = "cm", scale = 0.9)


##########################################################################################################################################
##TECHNICAL DETAILS FOR REPLICATION

#Macbook Pro 13 inch 2017
#mac OS Big Sur 11.2.3
#
#A .Rproj file was used in the development of this Code.
#> sessionInfo() 
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Monterey 12.2.1
#
#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] forcats_0.5.1           stringr_1.4.0           dplyr_1.0.8             purrr_0.3.4             readr_2.1.1            
#[6] tidyr_1.2.0             tibble_3.1.6            tidyverse_1.3.1         sf_1.0-5                sjmisc_2.8.9           
#[11] sjlabelled_1.1.8        rnaturalearthdata_0.1.0 rnaturalearth_0.1.0     tayloRswift_0.1.0       ggthemes_4.2.4         
#[16] ggcorrplot_0.1.3        ggplot2_3.3.5          
#
#loaded via a namespace (and not attached):
#  [1] TH.data_1.1-0       minqa_1.2.4         colorspace_2.0-2    RcppEigen_0.3.3.9.1 ellipsis_0.3.2      class_7.3-20       
#[7] estimability_1.3    ggstance_0.3.5      parameters_0.16.0   fs_1.5.2            rstudioapi_0.13     proxy_0.4-26       
#[13] farver_2.1.0        fansi_1.0.2         mvtnorm_1.1-3       lubridate_1.8.0     xml2_1.3.3          codetools_0.2-18   
#[19] splines_4.1.2       knitr_1.37          jsonlite_1.7.3      nloptr_2.0.0        ggeffects_1.1.1     broom_0.7.12       
#[25] dbplyr_2.1.1        effectsize_0.6.0.1  compiler_4.1.2      httr_1.4.2          sjstats_0.18.1      emmeans_1.7.2      
#[31] backports_1.4.1     assertthat_0.2.1    Matrix_1.4-0        survey_4.1-1        cli_3.2.0           s2_1.0.7           
#[37] tools_4.1.2         coda_0.19-4         gtable_0.3.0        glue_1.6.1          reshape2_1.4.4      wk_0.6.0           
#[43] Rcpp_1.0.8          carData_3.0-5       cellranger_1.1.0    vctrs_0.3.8         sjPlot_2.8.10       nlme_3.1-155       
#[49] iterators_1.0.13    lmtest_0.9-39       insight_0.15.0      xfun_0.29           lme4_1.1-27.1       rvest_1.0.2        
#[55] lifecycle_1.0.1     MASS_7.3-55         zoo_1.8-9           scales_1.1.1        hms_1.1.1           sandwich_3.0-1     
#[61] RColorBrewer_1.1-2  stringi_1.7.6       bayestestR_0.11.5   foreach_1.5.1       e1071_1.7-9         boot_1.3-28        
#[67] shape_1.4.6         rlang_1.0.1         pkgconfig_2.0.3     lattice_0.20-45     labeling_0.4.2      tidyselect_1.1.1   
#[73] plyr_1.8.6          magrittr_2.0.2      R6_2.5.1            generics_0.1.2      multcomp_1.4-18     DBI_1.1.2          
#[79] pillar_1.7.0        haven_2.4.3         withr_2.4.3         units_0.7-2         survival_3.2-13     datawizard_0.2.3   
#[85] abind_1.4-5         sp_1.4-6            performance_0.8.0   modelr_0.1.8        crayon_1.5.0        car_3.0-12         
#[91] KernSmooth_2.23-20  utf8_1.2.2          tzdb_0.2.0          grid_4.1.2          readxl_1.3.1        reprex_2.0.1       
#[97] digest_0.6.29       classInt_0.4-3      xtable_1.8-4        cregg_0.4.0         glmnet_4.1-3        munsell_0.5.0      
#[103] mitools_2.4   