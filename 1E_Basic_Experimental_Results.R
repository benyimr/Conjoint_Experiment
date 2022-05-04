##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº 5:  Experimental Results (Basic: AMCE and MM)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(cjoint)
library(cregg)
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


##########################################################################################################################################
#STEP 2: FORMATTING VARIABLES

## Coercing to factors (Wave 1)
conjoint_01$Sex                    <- factor(conjoint_01$sex_c,     levels = c("Male","Female"))
conjoint_01$Age                    <- factor(conjoint_01$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_01$Ideology               <- factor(conjoint_01$ideol_c,   levels = c("Center","Right","Center-Right",
                                                                               "Center-Left","Left"))
conjoint_01$`Political Experience` <- factor(conjoint_01$pol_exp_c, levels = c("None","Activist","Elected Official"))
conjoint_01$`Party Membership`     <- factor(conjoint_01$party_c,   levels = c("Independent supported by Party",
                                                                               "Member of Party","Independent"))
conjoint_01$Residence              <- factor(conjoint_01$resid_c,   levels = c("Same County","Live in another County"))
conjoint_01$Religion               <- factor(conjoint_01$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_01$`Social Class`         <- factor(conjoint_01$class_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_01$Occupation             <- factor(conjoint_01$occup_c,   levels = c("Accountant","Teacher",
                                                                               "Construction worker","Salesperson",
                                                                               "Civil Engineer in Mining","Physician"))

## Coercing to factors (Wave 3)
conjoint_03$Sex                     <- factor(conjoint_03$sex_c,     levels = c("Male","Female"))
conjoint_03$Age                     <- factor(conjoint_03$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_03$Ideology                <- factor(conjoint_03$ideol_c,   levels = c("Center","Right","Center-Right",
                                                                                "Center-Left","Left"))
conjoint_03$`Political Experience`  <- factor(conjoint_03$pol_exp_c, levels = c("None","Activist","Deputy"))
conjoint_03$`Party Membership`      <- factor(conjoint_03$party_c,   levels = c("Independent supported by Party",
                                                                                "Member of Party","Independent"))
conjoint_03$Residence               <- factor(conjoint_03$resid_c,   levels = c("Same County","Live in another County"))
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

## FOR CJOINT PACKAGE 

### Inputs for Design (attributes list, Wave 1) 
attribute_list_01 <- list()

attribute_list_01[["Sex"]]                  <- c("Male","Female")
attribute_list_01[["Age"]]                  <- c("25 years","50 years","70 years")
attribute_list_01[["Ideology"]]             <- c("Right","Center-Right","Center","Center-Left","Left")
attribute_list_01[["Political Experience"]] <- c("None","Activist","Elected Official")
attribute_list_01[["Party Membership"]]     <- c("Member of Party","Independent supported by Party","Independent")
attribute_list_01[["Residence"]]            <- c("Same County","Live in another County")
attribute_list_01[["Religion"]]             <- c("Catholic","Evangelical", "Irreligious")
attribute_list_01[["Social Class"]]         <- c("Working Class","Middle Class","Upper Class")
attribute_list_01[["Occupation"]]           <- c("Construction worker","Salesperson","Accountant",
                                                 "Teacher","Civil Engineer in Mining","Physician")

### Inputs for Design (attributes list, Wave 3)
attribute_list_03 <- attribute_list_01

attribute_list_03[["Political Experience"]]  <- c("None","Activist","Deputy")
attribute_list_03[["Occupation"]]            <- c("Construction worker","Salesperson","Janitor",
                                                  "Accountant","Teacher","Nurse Technician",
                                                  "Lawyer","Civil Engineer in Mining","Physician")
attribute_list_03[["Father's Social Class"]] <- c("Working Class","Middle Class","Upper Class")
attribute_list_03[["Father's Occupation"]]   <- c("Construction worker","Salesperson","Janitor",
                                                  "Accountant","Teacher","Nurse Technician",
                                                  "Lawyer","Civil Engineer in Mining","Physician")


## FOR CJOINT PACKAGE 

### Inputs for Design (weights, Wave 1)
marginal_weights_01 <- list()

marginal_weights_01[["Sex"]]                  <- c(1/2,1/2)
marginal_weights_01[["Age"]]                  <- c(1/3,1/3,1/3)
marginal_weights_01[["Ideology"]]             <- c(1/8,2/8,2/8,2/8,1/8)
marginal_weights_01[["Political Experience"]] <- c(2/4,1/4,1/4)
marginal_weights_01[["Party Membership"]]     <- c(1/3,1/3,1/3)
marginal_weights_01[["Residence"]]            <- c(1/2,1/2)
marginal_weights_01[["Religion"]]             <- c(5/10,2/10,3/10)
marginal_weights_01[["Social Class"]]         <- c(1/3,1/3,1/3)
marginal_weights_01[["Occupation"]]           <- c(1/6,1/6,1/6,1/6,1/6,1/6)

### Inputs for Design (weights, Wave 3)
marginal_weights_03 <- marginal_weights_01
marginal_weights_03[["Occupation"]]            <- c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9)
marginal_weights_03[["Father's Social Class"]] <- c(1/3,1/3,1/3)
marginal_weights_03[["Father's Occupation"]]   <- c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9)


## FOR CJOINT PACKAGE 

### Create design
candidates_design_01 <- makeDesign(type = "constraints", attribute.levels = attribute_list_01,
                                   level.probs = marginal_weights_01)
candidates_design_03 <- makeDesign(type = "constraints", attribute.levels = attribute_list_03,
                                   level.probs = marginal_weights_03)


## Remove objects
rm(attribute_list_01, attribute_list_03, marginal_weights_01, marginal_weights_03)
rm(candidates_design_01, candidates_design_03)

##########################################################################################################################################
#STEP 3: SUBSETS OF DATASETS

## Identify Cases without variance in the responses

### Wave 1, Vote Choice
conjoint_01 %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% select(idperson) %>% pull() -> cases_filter_01

### Wave 1, Evaluation of Competence
conjoint_01 %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% select(idperson) %>% pull() -> cases_filter_02

### Wave 3, Vote Choice
conjoint_03 %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% select(idperson) %>% pull() -> cases_filter_03

### Wave 3, Evaluation of Competence
conjoint_03 %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% select(idperson) %>% pull() -> cases_filter_04

### Wave 1, Perception of Representation
conjoint_03 %>%
  group_by(idperson) %>%
  summarise(sd_repr = sd(repr)) %>%
  filter(sd_repr!=0) %>% select(idperson) %>% pull() -> cases_filter_05


## Empty List to store subsets of data.frame
conjoint_w01 <- list()
conjoint_w03 <- list()


## Full Sample (Waves 1 and 3)
conjoint_01 -> conjoint_w01$conjoint_w01_global
conjoint_03 -> conjoint_w03$conjoint_w03_global


## Full Sample and Screeners

### Wave 1, Screener 1
conjoint_01 %>% 
  mutate(check1 = paste0(group_const,attention1)) %>% 
  filter(check1 == "03"|check1 == "11") %>% select(-check1) -> conjoint_w01$conjoint_w01_global_att1

### Wave 1, Screener 1 and Screener 2
conjoint_01 %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% select(-check2) -> conjoint_w01$conjoint_w01_global_att12

### Wave 3, Screener
conjoint_03 %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_global_att1

 
## Full Sample, Variance in Vote (Waves 1 and 3)
conjoint_01 %>% filter(idperson %in% cases_filter_01) -> conjoint_w01$conjoint_w01_vote
conjoint_03 %>% filter(idperson %in% cases_filter_03) -> conjoint_w03$conjoint_w03_vote


## Full Sample, Variance in Vote and Screeners

### Wave 1, Screener 1
conjoint_w01$conjoint_w01_vote %>% 
  mutate(check1 = paste0(group_const,attention1)) %>% 
  filter(check1 == "03"|check1 == "11") %>% select(-check1) -> conjoint_w01$conjoint_w01_vote_att1

### Wave 1, Screener 1 and Screener 2
conjoint_w01$conjoint_w01_vote %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% select(-check2) -> conjoint_w01$conjoint_w01_vote_att12

### Wave 3, Screener
conjoint_w03$conjoint_w03_vote %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_vote_att1


## Full Sample, Variance in Evaluation (Waves 1 and 3)
conjoint_01 %>% filter(idperson %in% cases_filter_02) -> conjoint_w01$conjoint_w01_eval
conjoint_03 %>% filter(idperson %in% cases_filter_04) -> conjoint_w03$conjoint_w03_eval


## Full Sample, Variance in Evaluation and Screeners

### Wave 1, Screener 1
conjoint_w01$conjoint_w01_eval %>% 
  mutate(check1 = paste0(group_const,attention1)) %>% 
  filter(check1 == "03"|check1 == "11") %>% select(-check1) -> conjoint_w01$conjoint_w01_eval_att1

### Wave 1, Screener 1 and Screener 2
conjoint_w01$conjoint_w01_eval %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% select(-check2) -> conjoint_w01$conjoint_w01_eval_att12

### Wave 3, Screener
conjoint_w03$conjoint_w03_eval %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_eval_att1


## Full Sample, Variance in Representation (Wave 3)
conjoint_03 %>% filter(idperson %in% cases_filter_05) -> conjoint_w03$conjoint_w03_repr


## Full Sample, Variance in Representation and Screeners (Wave 3)
conjoint_w03$conjoint_w03_repr %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_repr_att1


## Full Sample, Variance in Vote Choice and Evaluation (Wave 1)
conjoint_01 %>% filter(idperson %in% cases_filter_01) %>%
  filter(idperson %in% cases_filter_02) -> conjoint_w01$conjoint_w01_filt

## Full Sample, Variance in Vote Choice, Evaluation and Representation (Wave 3)
conjoint_03 %>% filter(idperson %in% cases_filter_03) %>%
  filter(idperson %in% cases_filter_04) %>% filter(idperson %in% cases_filter_05) -> conjoint_w03$conjoint_w03_filt


## Full Sample, Variance in Vote Choice and Evaluation and Screener 1 (Wave 1)
conjoint_w01$conjoint_w01_filt %>% 
  mutate(check1 = paste0(group_const,attention1)) %>% 
  filter(check1 == "03"|check1 == "11") %>% select(-check1) -> conjoint_w01$conjoint_w01_filt_att1

## Full Sample, Variance in Vote Choice and Evaluation and Screeners 1 and 2 (Wave 1)
conjoint_w01$conjoint_w01_filt %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% select(-check2) -> conjoint_w01$conjoint_w01_filt_att12

## Full Sample, Variance in Vote Choice, Evaluation and Representation and Screener (Wave 3)
conjoint_w03$conjoint_w03_filt %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_filt_att1


## Repeat the same splitting for Sub-sample: Mayor (Wave 1)
conjoint_w01$conjoint_w01_global       %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_global__m         
conjoint_w01$conjoint_w01_global_att1  %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_global_att1__m
conjoint_w01$conjoint_w01_global_att12 %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_global_att12__m
conjoint_w01$conjoint_w01_vote         %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_vote__m
conjoint_w01$conjoint_w01_vote_att1    %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_vote_att1__m
conjoint_w01$conjoint_w01_vote_att12   %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_vote_att12__m
conjoint_w01$conjoint_w01_eval         %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_eval__m
conjoint_w01$conjoint_w01_eval_att1    %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_eval_att1__m
conjoint_w01$conjoint_w01_eval_att12   %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_eval_att12__m
conjoint_w01$conjoint_w01_filt         %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_filt__m
conjoint_w01$conjoint_w01_filt_att1    %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_filt_att1__m
conjoint_w01$conjoint_w01_filt_att12   %>% filter(group_const == 0) -> conjoint_w01$conjoint_w01_filt_att12__m


## Repeat the same splitting for Sub-sample: Member of the Constituent Convention (Wave 1)
conjoint_w01$conjoint_w01_global       %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_global__c
conjoint_w01$conjoint_w01_global_att1  %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_global_att1__c
conjoint_w01$conjoint_w01_global_att12 %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_global_att12__c
conjoint_w01$conjoint_w01_vote         %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_vote__c
conjoint_w01$conjoint_w01_vote_att1    %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_vote_att1__c
conjoint_w01$conjoint_w01_vote_att12   %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_vote_att12__c
conjoint_w01$conjoint_w01_eval         %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_eval__c
conjoint_w01$conjoint_w01_eval_att1    %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_eval_att1__c
conjoint_w01$conjoint_w01_eval_att12   %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_eval_att12__c
conjoint_w01$conjoint_w01_filt         %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_filt__c
conjoint_w01$conjoint_w01_filt_att1    %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_filt_att1__c
conjoint_w01$conjoint_w01_filt_att12   %>% filter(group_const == 1) -> conjoint_w01$conjoint_w01_filt_att12__c


## Repeat the same splitting for Sub-sample: Deputy 1 (Wave 3)
conjoint_w03$conjoint_w03_global       %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_global__d1       
conjoint_w03$conjoint_w03_global_att1  %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_global_att1__d1
conjoint_w03$conjoint_w03_vote         %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_vote__d1
conjoint_w03$conjoint_w03_vote_att1    %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_vote_att1__d1
conjoint_w03$conjoint_w03_eval         %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_eval__d1
conjoint_w03$conjoint_w03_eval_att1    %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_eval_att1__d1
conjoint_w03$conjoint_w03_repr         %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_repr__d1
conjoint_w03$conjoint_w03_repr_att1    %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_repr_att1__d1
conjoint_w03$conjoint_w03_filt         %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_filt__d1
conjoint_w03$conjoint_w03_filt_att1    %>% filter(group_dip == 0) -> conjoint_w03$conjoint_w03_filt_att1__d1

## Repeat the same splitting for Sub-sample: Deputy 2 (Wave 3)
conjoint_w03$conjoint_w03_global       %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_global__d2
conjoint_w03$conjoint_w03_global_att1  %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_global_att1__d2
conjoint_w03$conjoint_w03_vote         %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_vote__d2
conjoint_w03$conjoint_w03_vote_att1    %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_vote_att1__d2
conjoint_w03$conjoint_w03_eval         %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_eval__d2
conjoint_w03$conjoint_w03_eval_att1    %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_eval_att1__d2
conjoint_w03$conjoint_w03_repr         %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_repr__d2
conjoint_w03$conjoint_w03_repr_att1    %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_repr_att1__d2
conjoint_w03$conjoint_w03_filt         %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_filt__d2
conjoint_w03$conjoint_w03_filt_att1    %>% filter(group_dip == 1) -> conjoint_w03$conjoint_w03_filt_att1__d2

## Remove objects
rm(cases_filter_01,cases_filter_02,cases_filter_03,cases_filter_04,cases_filter_05)


##########################################################################################################################################
#STEP 4: ESTIMATION OF AMCE'S AND MM'S

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


## Create empty lists to store results

### Social Class, Wave 1
result_class_w01_vote_AMCE <- list()
result_class_w01_vote_MM   <- list()
result_class_w01_eval_AMCE <- list()
result_class_w01_eval_MM   <- list()

### Occupation, Wave 1
result_occup_w01_vote_AMCE <- list()
result_occup_w01_vote_MM   <- list()
result_occup_w01_eval_AMCE <- list()
result_occup_w01_eval_MM   <- list()

### Social Class, Wave 3
result_class_w03_vote_AMCE <- list()
result_class_w03_vote_MM   <- list()
result_class_w03_eval_AMCE <- list()
result_class_w03_eval_MM   <- list()
result_class_w03_repr_AMCE <- list()
result_class_w03_repr_MM   <- list()

### Occupation, Wave 3
result_occup_w03_vote_AMCE <- list()
result_occup_w03_vote_MM   <- list()
result_occup_w03_eval_AMCE <- list()
result_occup_w03_eval_MM   <- list()
result_occup_w03_repr_AMCE <- list()
result_occup_w03_repr_MM   <- list()


## For Loops for Wave 01
for(i in 1:length(conjoint_w01)){
  
  ### VI: Class, VD: Vote, QoI: AMCE
  cregg::amce(data = conjoint_w01[[i]], formula = formula_1, id = ~ idperson) -> temp1
  ### Store in the list
  bind_cols(type = rep("A",dim(temp1)[1]),sample = rep(i,dim(temp1)[1]), 
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp1)[1]),
            temp1) -> result_class_w01_vote_AMCE[[i]]
  
  ### VI: Class, VD: Vote, QoI: MM
  cregg::mm(data = conjoint_w01[[i]], formula = formula_1, id = ~ idperson, h0 = 0.5) -> temp2
  ### Store in the list
  bind_cols(type = rep("B",dim(temp2)[1]),sample = rep(i,dim(temp2)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp2)[1]),
            temp2) -> result_class_w01_vote_MM[[i]]
  
  
  ### VI: Class, VD: Evaluation, QoI: AMCE
  cregg::amce(data = conjoint_w01[[i]], formula = formula_3, id = ~ idperson) -> temp3
  ### Store in the list
  bind_cols(type = rep("C",dim(temp3)[1]),sample = rep(i,dim(temp3)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp3)[1]),
            temp3) -> result_class_w01_eval_AMCE[[i]]
  
  ### VI: Class, VD: Evaluation, QoI: MM
  cregg::mm(data = conjoint_w01[[i]], formula = formula_3, id = ~ idperson, h0 = 50) -> temp4
  ### Store in the list
  bind_cols(type = rep("D",dim(temp4)[1]),sample = rep(i,dim(temp4)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp4)[1]),
            temp4) -> result_class_w01_eval_MM[[i]]
  
  
  ### VI: Occupation, VD: Vote, QoI: AMCE
  cregg::amce(data = conjoint_w01[[i]], formula = formula_2, id = ~ idperson) -> temp5
  ### Store in the list
  bind_cols(type = rep("E",dim(temp5)[1]),sample = rep(i,dim(temp5)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp5)[1]),
            temp5) -> result_occup_w01_vote_AMCE[[i]]
  
  ### VI: Occupation, VD: Vote, QoI: MM
  cregg::mm(data = conjoint_w01[[i]], formula = formula_2, id = ~ idperson, h0 = 0.5) -> temp6
  ### Store in the list
  bind_cols(type = rep("F",dim(temp6)[1]),sample = rep(i,dim(temp6)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp6)[1]),
            temp6) -> result_occup_w01_vote_MM[[i]]
  
  
  ### VI: Occupation, VD: Evaluation, QoI: AMCE
  cregg::amce(data = conjoint_w01[[i]], formula = formula_4, id = ~ idperson) -> temp7
  ### Store in the list
  bind_cols(type = rep("G",dim(temp7)[1]),sample = rep(i,dim(temp7)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp7)[1]),
            temp7) -> result_occup_w01_eval_AMCE[[i]]
  
  ### VI: Occupation, VD: Evaluation, QoI: MM
  cregg::mm(data = conjoint_w01[[i]], formula = formula_4, id = ~ idperson, h0 = 50) -> temp8
  ### Store in the list
  bind_cols(type = rep("H",dim(temp8)[1]),sample = rep(i,dim(temp8)[1]),
            size = rep(dim(conjoint_w01[[i]])[1]/dim(conjoint_01)[1]*100,dim(temp8)[1]),
            temp8) -> result_occup_w01_eval_MM[[i]]
  
}


## For Loops for Wave 03
for(i in 1:length(conjoint_w03)){
  
  ### VI: Class, VD: Vote, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_1, id = ~ idperson) -> temp1
  ### Store in the list
  bind_cols(type = rep("A",dim(temp1)[1]),sample = rep(i,dim(temp1)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp1)[1]),
            temp1) -> result_class_w03_vote_AMCE[[i]]
  
  ### VI: Class, VD: Vote, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_1, id = ~ idperson, h0 = 0.5) -> temp2
  ### Store in the list
  bind_cols(type = rep("B",dim(temp2)[1]),sample = rep(i,dim(temp2)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp2)[1]),
            temp2) -> result_class_w03_vote_MM[[i]]
  
  
  ### VI: Class, VD: Evaluation, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_3, id = ~ idperson) -> temp3
  ### Store in the list
  bind_cols(type = rep("C",dim(temp3)[1]),sample = rep(i,dim(temp3)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp3)[1]),
            temp3) -> result_class_w03_eval_AMCE[[i]]
  
  ### VI: Class, VD: Evaluation, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_3, id = ~ idperson, h0 = 50) -> temp4
  ### Store in the list
  bind_cols(type = rep("D",dim(temp4)[1]),sample = rep(i,dim(temp4)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp4)[1]),
            temp4) -> result_class_w03_eval_MM[[i]]
  
  
  ### VI: Class, VD: Representation, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_5, id = ~ idperson) -> temp5
  ### Store in the list
  bind_cols(type = rep("E",dim(temp5)[1]),sample = rep(i,dim(temp5)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp5)[1]),
            temp5) -> result_class_w03_repr_AMCE[[i]]
  
  ### VI: Class, VD: Representation, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_5, id = ~ idperson, h0 = 50) -> temp6
  ### Store in the list
  bind_cols(type = rep("F",dim(temp6)[1]),sample = rep(i,dim(temp6)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp6)[1]),
            temp6) -> result_class_w03_repr_MM[[i]]
  
  
  ### VI: Occupation, VD: Vote, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_2, id = ~ idperson) -> temp7
  ### Store in the list
  bind_cols(type = rep("G",dim(temp7)[1]),sample = rep(i,dim(temp7)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp7)[1]),
            temp7) -> result_occup_w03_vote_AMCE[[i]]
  
  ### VI: Occupation, VD: Vote, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_2, id = ~ idperson, h0 = 0.5) -> temp8
  ### Store in the list
  bind_cols(type = rep("H",dim(temp8)[1]),sample = rep(i,dim(temp8)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp8)[1]),
            temp8) -> result_occup_w03_vote_MM[[i]]
  
  
  ### VI: Occupation, VD: Evaluation, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_4, id = ~ idperson) -> temp9
  ### Store in the list
  bind_cols(type = rep("I",dim(temp9)[1]),sample = rep(i,dim(temp9)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp9)[1]),
            temp9) -> result_occup_w03_eval_AMCE[[i]]
  
  ### VI: Occupation, VD: Evaluation, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_4, id = ~ idperson, h0 = 50) -> temp10
  ### Store in the list
  bind_cols(type = rep("J",dim(temp10)[1]),sample = rep(i,dim(temp10)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp10)[1]),
            temp10) -> result_occup_w03_eval_MM[[i]]
  
  
  ### VI: Occupation, VD: Representation, QoI: AMCE
  cregg::amce(data = conjoint_w03[[i]], formula = formula_6, id = ~ idperson) -> temp11
  ### Store in the list
  bind_cols(type = rep("K",dim(temp11)[1]),sample = rep(i,dim(temp11)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp11)[1]),
            temp11) -> result_occup_w03_repr_AMCE[[i]]
  
  ### VI: Occupation, VD: Representation, QoI: MM
  cregg::mm(data = conjoint_w03[[i]], formula = formula_6, id = ~ idperson, h0 = 50) -> temp12
  ### Store in the list
  bind_cols(type = rep("L",dim(temp12)[1]),sample = rep(i,dim(temp12)[1]),
            size = rep(dim(conjoint_w03[[i]])[1]/dim(conjoint_03)[1]*100,dim(temp12)[1]),
            temp12) -> result_occup_w03_repr_MM[[i]]
}


##########################################################################################################################################
#STEP 5: DEFINITIVE STRUCTURE

## Transform into a data.frame

### Wave 1
bind_rows(do.call("rbind.data.frame", result_class_w01_vote_AMCE),
          do.call("rbind.data.frame", result_class_w01_vote_MM),
          do.call("rbind.data.frame", result_class_w01_eval_AMCE),
          do.call("rbind.data.frame", result_class_w01_eval_MM),
          do.call("rbind.data.frame", result_occup_w01_vote_AMCE),
          do.call("rbind.data.frame", result_occup_w01_vote_MM),
          do.call("rbind.data.frame", result_occup_w01_eval_AMCE),
          do.call("rbind.data.frame", result_occup_w01_eval_MM)) -> wave_01

### Wave 3
bind_rows(do.call("rbind.data.frame", result_class_w03_vote_AMCE),
          do.call("rbind.data.frame", result_class_w03_vote_MM),
          do.call("rbind.data.frame", result_class_w03_eval_AMCE),
          do.call("rbind.data.frame", result_class_w03_eval_MM),
          do.call("rbind.data.frame", result_class_w03_repr_AMCE),
          do.call("rbind.data.frame", result_class_w03_repr_MM),
          do.call("rbind.data.frame", result_occup_w03_vote_AMCE),
          do.call("rbind.data.frame", result_occup_w03_vote_MM),
          do.call("rbind.data.frame", result_occup_w03_eval_AMCE),
          do.call("rbind.data.frame", result_occup_w03_eval_MM),
          do.call("rbind.data.frame", result_occup_w03_repr_AMCE),
          do.call("rbind.data.frame", result_occup_w03_repr_MM)) -> wave_03


## Recode variable (Sub-Sample ID)

### Wave 1
wave_01 %>% 
  mutate(sample_esp = car::recode(sample, 
  "'1' ='Full, All' ; '2' ='Full, All, Attention 1' ; '3' ='Full, All, Attention 1+2';
   '4' ='Full, Vote'; '5' ='Full, Vote, Attention 1'; '6' ='Full, Vote, Attention 1+2';
   '7' ='Full, Eval'; '8' ='Full, Eval, Attention 1'; '9' ='Full, Eval, Attention 1+2';
   '10'='Full, Comb';'11' ='Full, Comb, Attention 1';'12' ='Full, Comb, Attention 1+2';
   '13' ='Mayors, All' ; '14' ='Mayors, All, Attention 1' ; '15' ='Mayors, All, Attention 1+2';
   '16' ='Mayors, Vote'; '17' ='Mayors, Vote, Attention 1'; '18' ='Mayors, Vote, Attention 1+2';
   '19' ='Mayors, Eval'; '20' ='Mayors, Eval, Attention 1'; '21' ='Mayors, Eval, Attention 1+2';
   '22' ='Mayors, Comb'; '23' ='Mayors, Comb, Attention 1'; '24' ='Mayors, Comb, Attention 1+2';
   '25' ='Assembly Members, All' ; '26' ='Assembly Members, All, Attention 1' ; '27' ='Assembly Members, All, Attention 1+2';
   '28' ='Assembly Members, Vote'; '29' ='Assembly Members, Vote, Attention 1'; '30' ='Assembly Members, Vote, Attention 1+2';
   '31' ='Assembly Members, Eval'; '32' ='Assembly Members, Eval, Attention 1'; '33' ='Assembly Members, Eval, Attention 1+2';
   '34' ='Assembly Members, Comb'; '35' ='Assembly Members, Comb, Attention 1'; '36' ='Assembly Members, Comb, Attention 1+2'")) %>%
  ### Remove irrelevant variables and reorder the resulting data.frame
  select(-type,-sample) %>% rename(sample = sample_esp) %>%
  select(sample, size:upper)-> wave_01

### Wave 3
wave_03 %>% 
  mutate(sample_esp = car::recode(sample, 
  "'1' ='Full, All' ; '2' ='Full, All, Attention' ;
   '3' ='Full, Vote'; '4' ='Full, Vote, Attention';
   '5' ='Full, Eval'; '6' ='Full, Eval, Attention';
   '7' ='Full, Repr'; '8' ='Full, Repr, Attention';
   '9' ='Full, Comb';'10' ='Full, Comb, Attention';
   '11' ='Deputies 1, All' ; '12' ='Deputies 1, All, Attention' ;
   '13' ='Deputies 1, Vote'; '14' ='Deputies 1, Vote, Attention';
   '15' ='Deputies 1, Eval'; '16' ='Deputies 1, Eval, Attention';
   '17' ='Deputies 1, Repr'; '18' ='Deputies 1, Repr, Attention';
   '19' ='Deputies 1, Comb'; '20' ='Deputies 1, Comb, Attention';
   '21' ='Deputies 2, All' ; '22' ='Deputies 2, All, Attention' ;
   '23' ='Deputies 2, Vote'; '24' ='Deputies 2, Vote, Attention';
   '25' ='Deputies 2, Eval'; '26' ='Deputies 2, Eval, Attention';
   '27' ='Deputies 2, Repr'; '28' ='Deputies 2, Repr, Attention';
   '29' ='Deputies 2, Comb'; '30' ='Deputies 2, Comb, Attention'"))  %>%
  ### Remove irrelevant variables and reorder the resulting data.frame
  select(-type,-sample) %>% rename(sample = sample_esp) %>%
  select(sample, size:upper)-> wave_03


### Store the results
write_rds(x = wave_01, file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01.rds")
write_rds(x = wave_03, file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03.rds")

### Remove objects
rm(formula_1, formula_2, formula_3, formula_4, formula_5, formula_6,i,
   temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12,
   result_class_w01_eval_AMCE, result_class_w01_eval_MM, 
   result_class_w01_vote_AMCE, result_class_w01_vote_MM,
   result_class_w03_eval_AMCE, result_class_w03_eval_MM,
   result_class_w03_repr_AMCE, result_class_w03_repr_MM,
   result_class_w03_vote_AMCE, result_class_w03_vote_MM,
   result_occup_w01_eval_AMCE, result_occup_w01_eval_MM, 
   result_occup_w01_vote_AMCE, result_occup_w01_vote_MM,
   result_occup_w03_eval_AMCE, result_occup_w03_eval_MM,
   result_occup_w03_repr_AMCE, result_occup_w03_repr_MM,
   result_occup_w03_vote_AMCE, result_occup_w03_vote_MM)


##########################################################################################################################################
#STEP 6: MULTIPLE HYPOTHESIS

## Import estimated results
wave_01 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01.rds")
wave_03 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03.rds")

## Correct P-Value and Confidence Intervals

### Wave 1
wave_01 %>% mutate(group = paste0(sample,"-",statistic,"-",outcome)) %>%
  group_by(group) %>%
  mutate(temp = row_number(),
         treatment = ifelse(temp<25, "Social Class","Occupation"),
         class = paste0(group,"-",treatment)) %>% ungroup() %>% 
  dplyr::select(sample,size,treatment,outcome:upper,class) %>% 
  group_by(class) %>%
  mutate(p_bonff     = p.adjust(p, method = "bonferroni"),
         p_fdr       = p.adjust(p, method = "fdr"),
         t1          = size*dim(conjoint_01)[1]/100,
         t2          = ifelse(treatment == "Social Class", 
                              qt(1 - 0.05/24, df = t1, lower.tail = TRUE),
                              qt(1 - 0.05/27, df = t1, lower.tail = TRUE)),
         lower_bonff = estimate - t2*std.error,
         upper_bonff = estimate + t2*std.error) -> wave_01


### Wave 3
wave_03 %>% mutate(group = paste0(sample,"-",statistic,"-",outcome)) %>%
  group_by(group) %>%
  mutate(temp = row_number(),
         treatment = ifelse(temp<25, "Social Class","Occupation"),
         class = paste0(group,"-",treatment)) %>% ungroup() %>% 
  dplyr::select(sample,size,treatment,outcome:upper,class) %>% 
  group_by(class) %>%
  mutate(p_bonff     = p.adjust(p, method = "bonferroni"),
         p_fdr       = p.adjust(p, method = "fdr"),
         t1          = size*dim(conjoint_03)[1]/100,
         t2          = ifelse(treatment == "Social Class", 
                              qt(1 - 0.05/24, df = t1, lower.tail = TRUE),
                              qt(1 - 0.05/30, df = t1, lower.tail = TRUE)),
         lower_bonff = estimate - t2*std.error,
         upper_bonff = estimate + t2*std.error) -> wave_03

### Store the results
write_rds(x = wave_01, file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01_corr.rds")
write_rds(x = wave_03, file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03_corr.rds")


##########################################################################################################################################
##TECHNICAL DETAILS FOR REPLICATION

#Macbook Pro 13 inch 2017
#mac OS Big Sur 11.2.3
#
#A .Rproj file was used in the development of this Code.
#> sessionInfo() 
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Monterey 12.3.1
#
#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#  [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4     readr_2.1.1     tidyr_1.2.0     tibble_3.1.6    tidyverse_1.3.1
#[9] cregg_0.4.0     cjoint_2.1.0    survey_4.1-1    survival_3.2-13 Matrix_1.4-0    ggplot2_3.3.5   lmtest_0.9-39   zoo_1.8-9      
#[17] sandwich_3.0-1 
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.8.3     lubridate_1.8.0  lattice_0.20-45  assertthat_0.2.1 digest_0.6.29    utf8_1.2.2       mime_0.12        R6_2.5.1        
#[9] cellranger_1.1.0 backports_1.4.1  reprex_2.0.1     ggstance_0.3.5   httr_1.4.2       pillar_1.7.0     rlang_1.0.2      readxl_1.3.1    
#[17] rstudioapi_0.13  car_3.0-12       splines_4.1.2    munsell_0.5.0    shiny_1.7.1      broom_0.7.12     compiler_4.1.2   httpuv_1.6.5    
#[25] modelr_0.1.8     pkgconfig_2.0.3  htmltools_0.5.2  mitools_2.4      tidyselect_1.1.2 fansi_1.0.3      crayon_1.5.1     tzdb_0.2.0      
#[33] dbplyr_2.1.1     withr_2.4.3      later_1.3.0      jsonlite_1.8.0   xtable_1.8-4     gtable_0.3.0     lifecycle_1.0.1  DBI_1.1.2       
#[41] magrittr_2.0.2   scales_1.1.1     stringi_1.7.6    cli_3.2.0        carData_3.0-5    fs_1.5.2         promises_1.2.0.1 xml2_1.3.3      
#[49] ellipsis_0.3.2   generics_0.1.2   vctrs_0.3.8      tools_4.1.2      glue_1.6.2       hms_1.1.1        abind_1.4-5      fastmap_1.1.0   
#[57] colorspace_2.0-2 rvest_1.0.2      haven_2.4.3 