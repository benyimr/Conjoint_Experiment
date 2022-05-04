##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº7:  Diagnostic Checks
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(cregg)
library(estimatr)
library(ggthemes)
library(lmtest)
library(MetBrewer)
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


##########################################################################################################################################
#STEP 2: FORMATTING VARIABLES

## Coercing Attributes to Factors (Wave 1)
conjoint_01$Sex                    <- factor(conjoint_01$sex_c,     levels = c("Male","Female"))
conjoint_01$Age                    <- factor(conjoint_01$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_01$Ideology               <- factor(conjoint_01$ideol_c,   levels = c("Center","Right","Center-Right",
                                                                               "Center-Left","Left"))
conjoint_01$`Political Experience` <- factor(conjoint_01$pol_exp_c, levels = c("None","Activist","Elected Official"))
conjoint_01$`Party Membership`     <- factor(conjoint_01$party_c,   levels = c("Independent supported by Party",
                                                                               "Member of Party","Independent"))
conjoint_01$Residence              <- factor(conjoint_01$resid_c,   levels = c("Live in another County","Same County"))
conjoint_01$Religion               <- factor(conjoint_01$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_01$`Social Class`         <- factor(conjoint_01$class_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_01$Occupation             <- factor(conjoint_01$occup_c,   levels = c("Accountant","Teacher",
                                                                               "Construction worker","Salesperson",
                                                                               "Civil Engineer in Mining","Physician"))

## Coercing Attributes to Factors (Wave 3)
conjoint_03$Sex                     <- factor(conjoint_03$sex_c,     levels = c("Male","Female"))
conjoint_03$Age                     <- factor(conjoint_03$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_03$Ideology                <- factor(conjoint_03$ideol_c,   levels = c("Center","Right","Center-Right",
                                                                                "Center-Left","Left"))
conjoint_03$`Political Experience`  <- factor(conjoint_03$pol_exp_c, levels = c("None","Activist","Deputy"))
conjoint_03$`Party Membership`      <- factor(conjoint_03$party_c,   levels = c("Independent supported by Party",
                                                                                "Member of Party","Independent"))
conjoint_03$Residence               <- factor(conjoint_03$resid_c,   levels = c("Live in another County","Same County"))
conjoint_03$Religion                <- factor(conjoint_03$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_03$`Social Class`          <- factor(conjoint_03$class_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_03$Occupation              <- factor(conjoint_03$occup_c,   levels = c("Accountant","Teacher","Nurse Technician",
                                                                                "Construction worker","Salesperson","Janitor",
                                                                                "Lawyer","Civil Engineer in Mining","Physician"))
conjoint_03$classf1_c               <- car::recode(conjoint_03$classf_c,"'Check'=NA")
conjoint_03$occupf1_c               <- car::recode(conjoint_03$occupf_c,"'Check'=NA")
conjoint_03$`Father's Social Class` <- factor(conjoint_03$classf1_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_03$`Father's Occupation`    <- factor(conjoint_03$occupf1_c,   levels = c("Accountant","Teacher","Nurse Technician",
                                                                                   "Construction worker","Salesperson","Janitor",
                                                                                   "Lawyer","Civil Engineer in Mining","Physician"))


## Coercing Respondent Characteristics to Numeric (Wave 1)
conjoint_01 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(range_age_num),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         educ_num2  = as.numeric(car::recode(educ_num1,"c(1,2,3)=1;4=2")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         occup_num2 = as.numeric(car::recode(occup_num1,"c(2,3)=2;c(4)=3;c(5)=4")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         gse_num2   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;c(3,4,5)=3")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num),
         ideol_num2 = as.numeric(car::recode(ideol_num,"c(1,2)=1;c(3)=2;c(4,5)=3;c(6,99)=4"))) -> conjoint_01

## Coercing Respondent Characteristics to Numeric (Wave 1)
conjoint_03 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(range_age_num),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         educ_num2  = as.numeric(car::recode(educ_num1,"c(1,2,3)=1;4=2")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         occup_num2 = as.numeric(car::recode(occup_num1,"c(2,3)=2;c(4)=3;c(5)=4")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         gse_num2   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;c(3,4,5)=3")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num),
         ideol_num2 = as.numeric(car::recode(ideol_num,"c(1,2)=1;c(3)=2;c(4,5)=3;c(6,99)=4"))) -> conjoint_03

# Education: Basica (1-3); Media (4-5); Técnica (6-7); Universitaria (8-10)
# Occupation: Vacío, Estudiante, Dueño(a) de Casa, CESANTE/JUBILADO (0, 7,8, 9, 10)
#             Trabajos menores ocasionales e informales,Oficio menor, obrero no calificado, jornalero, servicio doméstico con contrato,  (1,2)
#             Obrero calificado, capataz, microempresario (3)
#             Empleado administrativo medio y bajo (4)
#             Ejecutivo medio y alto (5,6)
# GSE:        C1 (1), C2 (2), C3 (3), D-E (4,5)


##########################################################################################################################################
#STEP 3: SUBSETS OF DATASETS

## Identify Cases without variance in the responses

### Wave 1, Vote Choice
conjoint_01 %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% dplyr::select(idperson) %>% pull() -> cases_filter_01

### Wave 1, Evaluation of Competence
conjoint_01 %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% dplyr::select(idperson) %>% pull() -> cases_filter_02

### Wave 3, Vote Choice
conjoint_03 %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% dplyr::select(idperson) %>% pull() -> cases_filter_03

### Wave 3, Evaluation of Competence
conjoint_03 %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% dplyr::select(idperson) %>% pull() -> cases_filter_04

### Wave 1, Perception of Representation
conjoint_03 %>%
  group_by(idperson) %>%
  summarise(sd_repr = sd(repr)) %>%
  filter(sd_repr!=0) %>% dplyr::select(idperson) %>% pull() -> cases_filter_05


## Full Sample, Variance in Vote Choice and Evaluation and Screeners 1 and 2 (Wave 1)
conjoint_01 %>% filter(idperson %in% cases_filter_01) %>%
  filter(idperson %in% cases_filter_02) %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_01_filtered


## Full Sample, Variance in Vote Choice, Evaluation, and Representation and Screener (Wave 3)
conjoint_03 %>% filter(idperson %in% cases_filter_03) %>%
  filter(idperson %in% cases_filter_04) %>% filter(idperson %in% cases_filter_05) %>%
  filter(attention1 == 4) -> conjoint_03_filtered

## Remove objects
rm(cases_filter_01,cases_filter_02,cases_filter_03,cases_filter_04,cases_filter_05)


##########################################################################################################################################
#STEP 4: FREQUENCY OF ATTRIBUTES

## Specify formulas

### VD: Vote Choice VI: Social Class/Occupation
formula_1 <- as.formula("choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + `Social Class`")
formula_2 <- as.formula("choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + Occupation")

### VD: Evaluation of Competence VI: Social Class/Occupation
formula_3 <- as.formula("eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + `Social Class`")
formula_4 <- as.formula("eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + Occupation")

### VD: Perception of Representation VI: Social Class/Occupation
formula_5 <- as.formula("repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + `Social Class`")
formula_6 <- as.formula("repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + Residence + Religion + Occupation")


## Create Frequency Plots  (Nizami)

### Wave 1, Full Sample, Social Class
cj_freqs(data = conjoint_01, formula = formula_1, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Full_Social_Class_Wave_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave 1, Full Sample, Occupation
cj_freqs(data = conjoint_01, formula = formula_2, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Full_Occupation_Wave_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave 1, Filtered Sample, Social Class
cj_freqs(data = conjoint_01_filtered, formula = formula_1, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Filtered_Social_Class_Wave_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave 1, Filtered Sample, Occupation
cj_freqs(data = conjoint_01_filtered, formula = formula_2, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Filtered_Occupation_Wave_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave 3, Full Sample, Social Class
cj_freqs(data = conjoint_03, formula = formula_1, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Full_Social_Class_Wave_03.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave 3, Full Sample, Occupation
cj_freqs(data = conjoint_03, formula = formula_2, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Full_Occupation_Wave_03.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave 3, Filtered Sample, Social Class
cj_freqs(data = conjoint_03_filtered, formula = formula_1, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Filtered_Social_Class_Wave_03.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave 3, Filtered Sample, Occupation
cj_freqs(data = conjoint_03_filtered, formula = formula_2, id = ~idperson) %>%
  ggplot() + geom_bar(mapping = aes(x = level, y = estimate, fill = feature), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) +
  labs(x= "", y = "Frequency",fill="Attribute") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Distribution_Attributes_Filtered_Occupation_Wave_03.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


##########################################################################################################################################
#STEP 5: ASSUMPTION Nº1 (STABILITY AND NO CARRYOVER EFFECTS)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class
bind_rows(
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Full_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Full_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Full_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Full_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Filtered_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Filtered_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Filtered_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_01_Filtered_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Representation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Full_Representation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Representation, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Representation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Representation, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 1) %>% mutate(Task = 1), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 2) %>% mutate(Task = 2),
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 3) %>% mutate(Task = 3), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 4) %>% mutate(Task = 4),
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 5) %>% mutate(Task = 5), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 6) %>% mutate(Task = 6),
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 7) %>% mutate(Task = 7), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = task == 8) %>% mutate(Task = 8)) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Task, ncol =4, nrow = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Carryover_Effect__Wave_03_Filtered_Representation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


## Empty list to store models
models <- list()
tests  <- list()

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class
models$model_01 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_02 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_01 <- waldtest(models$model_01,models$model_02)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Occupation
models$model_03 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_04 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_02 <- waldtest(models$model_03,models$model_04)

## Wave: 1, Sample: Full, DV: Competence, IV: Social Class
models$model_05 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_06 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_03 <- waldtest(models$model_05,models$model_06)

## Wave: 1, Sample: Full, DV: Competence, IV: Occupation
models$model_07 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_08 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_04 <- waldtest(models$model_07,models$model_08)


## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
models$model_09 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_10 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_05 <- waldtest(models$model_09,models$model_10)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Occupation
models$model_11 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_12 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_06 <- waldtest(models$model_11,models$model_12)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Social Class
models$model_13 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_14 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_07 <- waldtest(models$model_13,models$model_14)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Occupation
models$model_15 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_16 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_08 <- waldtest(models$model_15,models$model_16)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class
models$model_17 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_18 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_09 <- waldtest(models$model_17,models$model_18)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Occupation
models$model_19 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_20 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_10 <- waldtest(models$model_19,models$model_20)

## Wave: 3, Sample: Full, DV: Competence, IV: Social Class
models$model_21 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_22 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_11 <- waldtest(models$model_21,models$model_22)

## Wave: 3, Sample: Full, DV: Competence, IV: Occupation
models$model_23 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_24 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_12 <- waldtest(models$model_23,models$model_24)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
models$model_25 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_26 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_13 <- waldtest(models$model_25,models$model_26)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
models$model_27 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_28 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_14 <- waldtest(models$model_27,models$model_28)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
models$model_29 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_30 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_15 <- waldtest(models$model_29,models$model_30)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Occupation
models$model_31 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_32 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_16 <- waldtest(models$model_31,models$model_32)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Social Class
models$model_33 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_34 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_17 <- waldtest(models$model_33,models$model_34)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Occupation
models$model_35 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_36 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_18 <- waldtest(models$model_35,models$model_36)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
models$model_37 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_38 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_19 <- waldtest(models$model_37,models$model_38)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
models$model_39 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_40 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(task),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_20 <- waldtest(models$model_39,models$model_40)

## Definitive format

### Empty list to store F-Tests
templist <- list()

### Loop to extract results
for(i in 1:20){
  tests[[i]] %>% broom::tidy() %>% mutate(type = i) -> templist[[i]]
}

### Data.frame format
do.call("rbind.data.frame", templist) %>% 
  mutate(type_esp = car::recode(type,"'1' ='Wave 1, Sample Full, Vote Choice, Social Class';
                                      '2' ='Wave 1, Sample Full, Vote Choice, Occupation';
                                      '3' ='Wave 1, Sample Full, Competence, Social Class';
                                      '4' ='Wave 1, Sample Full, Competence, Occupation';
                                      '5' ='Wave 1, Sample Filtered, Vote Choice, Social Class';
                                      '6' ='Wave 1, Sample Filtered, Vote Choice, Occupation';
                                      '7' ='Wave 1, Sample Filtered, Competence, Social Class';
                                      '8' ='Wave 1, Sample Filtered, Competence, Occupation';
                                      '9' ='Wave 3, Sample Full, Vote Choice, Social Class';
                                      '10'='Wave 3, Sample Full, Vote Choice, Occupation';
                                      '11'='Wave 3, Sample Full, Competence, Social Class';
                                      '12'='Wave 3, Sample Full, Competence, Occupation';
                                      '13'='Wave 3, Sample Full, Representation, Social Class';
                                      '14'='Wave 3, Sample Full, Representation, Occupation';
                                      '15'='Wave 3, Sample Filtered, Vote Choice, Social Class';
                                      '16'='Wave 3, Sample Filtered, Vote Choice, Occupation';
                                      '17'='Wave 3, Sample Filtered, Competence, Social Class';
                                      '18'='Wave 3, Sample Filtered, Competence, Occupation';
                                      '19'='Wave 3, Sample Filtered, Representation, Social Class';
                                      '20'='Wave 3, Sample Filtered, Representation, Occupation'")) -> tests_df
 
## Store results
write_rds(x = tests_df, file = "0C_Objects/Carryover_Effects.rds")

## Remove objects
rm(i, models, templist,tests, tests_df)


##########################################################################################################################################
#STEP 6: ASSUMPTION Nº2 (NO PROFILE-ORDER EFFECTS)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01, formula = formula_1, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Full_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01, formula = formula_2, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Full_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01, formula = formula_3, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Full_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Full, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01, formula = formula_4, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Full_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_1, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Filtered_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_2, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Filtered_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_3, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Filtered_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_01_filtered, formula = formula_4, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_01_Filtered_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_1, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_2, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_3, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_4, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_5, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Representation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03, formula = formula_6, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Full_Representation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_1, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Vote_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_2, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Vote_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_3, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Evaluation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_4, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Evaluation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Representation, IV: Social Class
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_5, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Representation_Social_Class.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

## Wave: 3, Sample: Filtered, DV: Representation, IV: Occupation
bind_rows(
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = cand == "A") %>% mutate(Profile = "Profile 1"), 
  cregg::amce(data = conjoint_03_filtered, formula = formula_6, id = ~ idperson, subset = cand == "B") %>% mutate(Profile = "Profile 2")) %>%
  mutate(estimate1 = ifelse(is.na(std.error)==TRUE, NA, estimate)) %>% 
  ggplot() + geom_pointrange(mapping = aes(x= level, y = estimate1, ymin = lower, ymax = upper)) + 
  facet_wrap(~Profile,ncol = 2) +
  scale_x_discrete(limits=rev) + 
  geom_hline(mapping = aes(yintercept =0), linetype = "longdash") + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Profile_Order_Effect__Wave_03_Filtered_Representation_Occupation.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


## Empty list to store models
models <- list()
tests  <- list()

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class
models$model_01 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_02 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_01 <- waldtest(models$model_01,models$model_02)

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Occupation
models$model_03 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_04 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_02 <- waldtest(models$model_03,models$model_04)

## Wave: 1, Sample: Full, DV: Competence, IV: Social Class
models$model_05 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_06 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_03 <- waldtest(models$model_05,models$model_06)

## Wave: 1, Sample: Full, DV: Competence, IV: Occupation
models$model_07 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
models$model_08 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_01, clusters = idperson, se_type = "CR2")
tests$test_04 <- waldtest(models$model_07,models$model_08)


## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
models$model_09 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_10 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_05 <- waldtest(models$model_09,models$model_10)

## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Occupation
models$model_11 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_12 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_06 <- waldtest(models$model_11,models$model_12)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Social Class
models$model_13 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_14 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_07 <- waldtest(models$model_13,models$model_14)

## Wave: 1, Sample: Filtered, DV: Competence, IV: Occupation
models$model_15 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
models$model_16 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_01_filtered, clusters = idperson, se_type = "CR2")
tests$test_08 <- waldtest(models$model_15,models$model_16)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class
models$model_17 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_18 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_09 <- waldtest(models$model_17,models$model_18)

## Wave: 3, Sample: Full, DV: Vote Choice, IV: Occupation
models$model_19 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_20 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_10 <- waldtest(models$model_19,models$model_20)

## Wave: 3, Sample: Full, DV: Competence, IV: Social Class
models$model_21 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_22 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_11 <- waldtest(models$model_21,models$model_22)

## Wave: 3, Sample: Full, DV: Competence, IV: Occupation
models$model_23 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_24 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_12 <- waldtest(models$model_23,models$model_24)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
models$model_25 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_26 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_13 <- waldtest(models$model_25,models$model_26)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
models$model_27 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_28 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_14 <- waldtest(models$model_27,models$model_28)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
models$model_29 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_30 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_15 <- waldtest(models$model_29,models$model_30)

## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Occupation
models$model_31 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_32 <- lm_robust(formula = choice ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_16 <- waldtest(models$model_31,models$model_32)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Social Class
models$model_33 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_34 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_17 <- waldtest(models$model_33,models$model_34)

## Wave: 3, Sample: Filtered, DV: Competence, IV: Occupation
models$model_35 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
models$model_36 <- lm_robust(formula = eval ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03_filtered, clusters = idperson, se_type = "CR2")
tests$test_18 <- waldtest(models$model_35,models$model_36)

## Wave: 3, Sample: Full, DV: Representation, IV: Social Class
models$model_37 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class`*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_38 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + `Social Class` + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_19 <- waldtest(models$model_37,models$model_38)

## Wave: 3, Sample: Full, DV: Representation, IV: Occupation
models$model_39 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation*as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
models$model_40 <- lm_robust(formula = repr ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
                               Residence + Religion + Occupation + as.factor(cand),
                             data = conjoint_03, clusters = idperson, se_type = "CR2")
tests$test_20 <- waldtest(models$model_39,models$model_40)

## Definitive format

### Empty list to store F-Tests
templist <- list()

### Loop to extract results
for(i in 1:20){
  tests[[i]] %>% broom::tidy() %>% mutate(type = i) -> templist[[i]]
}

### Data.frame format
do.call("rbind.data.frame", templist) %>% 
  mutate(type_esp = car::recode(type,"'1' ='Wave 1, Sample Full, Vote Choice, Social Class';
                                      '2' ='Wave 1, Sample Full, Vote Choice, Occupation';
                                      '3' ='Wave 1, Sample Full, Competence, Social Class';
                                      '4' ='Wave 1, Sample Full, Competence, Occupation';
                                      '5' ='Wave 1, Sample Filtered, Vote Choice, Social Class';
                                      '6' ='Wave 1, Sample Filtered, Vote Choice, Occupation';
                                      '7' ='Wave 1, Sample Filtered, Competence, Social Class';
                                      '8' ='Wave 1, Sample Filtered, Competence, Occupation';
                                      '9' ='Wave 3, Sample Full, Vote Choice, Social Class';
                                      '10'='Wave 3, Sample Full, Vote Choice, Occupation';
                                      '11'='Wave 3, Sample Full, Competence, Social Class';
                                      '12'='Wave 3, Sample Full, Competence, Occupation';
                                      '13'='Wave 3, Sample Full, Representation, Social Class';
                                      '14'='Wave 3, Sample Full, Representation, Occupation';
                                      '15'='Wave 3, Sample Filtered, Vote Choice, Social Class';
                                      '16'='Wave 3, Sample Filtered, Vote Choice, Occupation';
                                      '17'='Wave 3, Sample Filtered, Competence, Social Class';
                                      '18'='Wave 3, Sample Filtered, Competence, Occupation';
                                      '19'='Wave 3, Sample Filtered, Representation, Social Class';
                                      '20'='Wave 3, Sample Filtered, Representation, Occupation'")) -> tests_df

## Store results
write_rds(x = tests_df, file = "0C_Objects/Profile_Order_Effects.rds")

## Remove objects
rm(i, models, templist,tests, tests_df)


##########################################################################################################################################
#STEP 7: ASSUMPTION Nº3 (RANDOMIZATION OF THE PROFILES)

### Wave: 1, Sample: Full, IV: Social Class, DV: Sex of Respondent
mm(data = conjoint_01, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Age of Respondent
mm(data = conjoint_01, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Education of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Education of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Geographic Residence of Respondent 
mm(data = conjoint_01, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Social Class, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Social_Class__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 1, Sample: Full, IV: Occupation, DV: Sex of Respondent
mm(data = conjoint_01, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Age of Respondent
mm(data = conjoint_01, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Education of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Education of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Geographic Residence of Respondent 
mm(data = conjoint_01, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_01, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Full, IV: Occupation, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_01, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Full_Occupation__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 1, Sample: Filtered, IV: Social Class, DV: Sex of Respondent
mm(data = conjoint_01_filtered, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Age of Respondent
mm(data = conjoint_01_filtered, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Education of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Education of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Geographic Residence of Respondent 
mm(data = conjoint_01_filtered, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Social Class, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Social_Class__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 1, Sample: Filtered, IV: Occupation, DV: Sex of Respondent
mm(data = conjoint_01_filtered, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Age of Respondent
mm(data = conjoint_01_filtered, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Education of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Education of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Geographic Residence of Respondent 
mm(data = conjoint_01_filtered, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_01_filtered, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 1, Sample: Filtered, IV: Occupation, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_01_filtered, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_01_filtered$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_01_Filtered_Occupation__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)



### Wave: 3, Sample: Full, IV: Social Class, DV: Sex of Respondent
mm(data = conjoint_03, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Age of Respondent
mm(data = conjoint_03, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Education of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Education of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Geographic Residence of Respondent 
mm(data = conjoint_03, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Social Class, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Social_Class__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 3, Sample: Full, IV: Occupation, DV: Sex of Respondent
mm(data = conjoint_03, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Age of Respondent
mm(data = conjoint_03, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Education of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Education of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Geographic Residence of Respondent 
mm(data = conjoint_03, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_03, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Full, IV: Occupation, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_03, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Full_Occupation__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 3, Sample: Filtered, IV: Social Class, DV: Sex of Respondent
mm(data = conjoint_03_filtered, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Age of Respondent
mm(data = conjoint_03_filtered, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Education of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Education of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Geographic Residence of Respondent 
mm(data = conjoint_03_filtered, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Social Class, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + `Social Class`, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Social_Class__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


### Wave: 3, Sample: Filtered, IV: Occupation, DV: Sex of Respondent
mm(data = conjoint_03_filtered, 
   formula = sex_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$sex_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Sex of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Sex.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Age of Respondent
mm(data = conjoint_03_filtered, 
   formula = age_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$age_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Age of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Age.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Education of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = educ_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$educ_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Education_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Education of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = educ_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$educ_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Educational Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Education_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Occupation of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = occup_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$occup_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Occupation of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Occupation_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Occupation of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = occup_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$occup_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Occupation of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Occupation_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = gse_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$gse_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute",  title = "Socio-Economic Level of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_SEL_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Socio-Economic Position of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = gse_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$gse_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Socio-Economic Level of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_SEL_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Geographic Residence of Respondent 
mm(data = conjoint_03_filtered, 
   formula = zone_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$zone_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Residence of Respondent") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Zone_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Ideology of Respondent (Version 1)
mm(data = conjoint_03_filtered, 
   formula = ideol_num1 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$ideol_num1, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 1)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Ideology_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)

### Wave: 3, Sample: Filtered, IV: Occupation, DV: Ideology of Respondent (Version 2)
mm(data = conjoint_03_filtered, 
   formula = ideol_num2 ~ Sex + Age + Ideology + `Political Experience` + `Party Membership` + 
     Residence + Religion + Occupation, id = ~idperson) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x = level, y = estimate, ymin = lower, ymax = upper, col = feature)) +
  geom_hline(mapping = aes(yintercept = mean(conjoint_03_filtered$ideol_num2, na.rm = TRUE)), linetype = "longdash") +
  coord_flip() + 
  scale_x_discrete(limits=rev) + 
  scale_fill_manual(values=met.brewer(name = "Cross", n = 8, type = "discrete")) + 
  labs(x= "", y = "Frequency",col="Attribute", title = "Ideology of Respondent (Version 2)") + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) 
ggsave(filename = "Diagnostic_Plot_Randomization__Wave_03_Filtered_Occupation__R_Ideology_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/5_Diagnostics/", 
       width = 50, height = 25, units = "cm", scale = 0.9)


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
#  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4     readr_2.1.1     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
#[9] tidyverse_1.3.1 MetBrewer_0.2.0 lmtest_0.9-39   zoo_1.8-9       ggthemes_4.2.4  estimatr_0.30.4 cregg_0.4.0    
#
#loaded via a namespace (and not attached):
#  [1] nlme_3.1-155       fs_1.5.2           lubridate_1.8.0    insight_0.15.0     httr_1.4.2         tools_4.1.2        backports_1.4.1   
#[8] utf8_1.2.2         R6_2.5.1           sjlabelled_1.1.8   DBI_1.1.2          colorspace_2.0-2   withr_2.4.3        tidyselect_1.1.1  
#[15] emmeans_1.7.2      compiler_4.1.2     rvest_1.0.2        performance_0.8.0  cli_3.2.0          xml2_1.3.3         sandwich_3.0-1    
#[22] labeling_0.4.2     bayestestR_0.11.5  scales_1.1.1       mvtnorm_1.1-3      digest_0.6.29      minqa_1.2.4        pkgconfig_2.0.3   
#[29] htmltools_0.5.2    lme4_1.1-27.1      dbplyr_2.1.1       fastmap_1.1.0      readxl_1.3.1       rlang_1.0.1        rstudioapi_0.13   
#[36] cjoint_2.1.0       shiny_1.7.1        farver_2.1.0       generics_0.1.2     jsonlite_1.7.3     car_3.0-12         sjPlot_2.8.10     
#[43] magrittr_2.0.2     Formula_1.2-4      texreg_1.37.5      parameters_0.16.0  Matrix_1.4-0       Rcpp_1.0.8         munsell_0.5.0     
#[50] fansi_1.0.2        abind_1.4-5        lifecycle_1.0.1    stringi_1.7.6      multcomp_1.4-18    carData_3.0-5      MASS_7.3-55       
#[57] ggstance_0.3.5     grid_4.1.2         promises_1.2.0.1   sjmisc_2.8.9       crayon_1.5.0       lattice_0.20-45    ggeffects_1.1.1   
#[64] haven_2.4.3        splines_4.1.2      sjstats_0.18.1     hms_1.1.1          knitr_1.37         pillar_1.7.0       boot_1.3-28       
#[71] estimability_1.3   effectsize_0.6.0.1 codetools_0.2-18   reprex_2.0.1       glue_1.6.1         mitools_2.4        modelr_0.1.8      
#[78] tzdb_0.2.0         vctrs_0.3.8        nloptr_2.0.0       httpuv_1.6.5       cellranger_1.1.0   gtable_0.3.0       assertthat_0.2.1  
#[85] datawizard_0.2.3   xfun_0.29          mime_0.12          xtable_1.8-4       broom_0.7.12       survey_4.1-1       coda_0.19-4       
#[92] later_1.3.0        survival_3.2-13    TH.data_1.1-0      ellipsis_0.3.2