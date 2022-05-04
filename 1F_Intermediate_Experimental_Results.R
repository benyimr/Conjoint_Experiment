##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº6:  Experimental Results (Intermediate: ACPs)
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

## Load functions
source("Functions/conjacp.R")             ### Function that calculates ACPs
#source("Functions/FunctionVCOVCluster.R") ### Function that calculates clustered SEs
#source("Functions/FunctionTableGraph.R")  ### Function that prepare the data frame for result graphs
#source("Functions/theme_graph.R")


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


##########################################################################################################################################
#STEP 2: FORMATTING VARIABLES 

## Coercing to factors (Wave 1)
conjoint_01$attr_sex      <- factor(conjoint_01$sex_c,     levels = c("Male","Female"))
conjoint_01$attr_age      <- factor(conjoint_01$age_c,     levels = c("25 years","50 years","70 years"))
conjoint_01$attr_ideol    <- factor(conjoint_01$ideol_c,   levels = c("Right","Center-Right","Center","Center-Left","Left"))
conjoint_01$attr_pol_exp  <- factor(conjoint_01$pol_exp_c, levels = c("None","Activist","Elected Official"))
conjoint_01$attr_pty_mem  <- factor(conjoint_01$party_c,   levels = c("Member of Party","Independent supported by Party","Independent"))
conjoint_01$attr_res      <- factor(conjoint_01$resid_c,   levels = c("Live in another County","Same County"))
conjoint_01$attr_rel      <- factor(conjoint_01$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_01$attr_class    <- factor(conjoint_01$class_c,   levels = c("Working Class","Middle Class","Upper Class"))
conjoint_01$attr_occup    <- factor(conjoint_01$occup_c,   levels = c("Construction worker","Salesperson","Accountant","Teacher",
                                                                      "Civil Engineer in Mining","Physician"))

## Coercing to factors (Wave 3)
conjoint_03$attr_sex      <- factor(conjoint_03$sex_c,     levels = c("Male","Female"))
conjoint_03$attr_age      <- factor(conjoint_03$age_c,     levels = c("25 years","50 years","70 years"))
conjoint_03$attr_ideol    <- factor(conjoint_03$ideol_c,   levels = c("Right","Center-Right","Center","Center-Left","Left"))
conjoint_03$attr_pol_exp  <- factor(conjoint_03$pol_exp_c, levels = c("None","Activist","Deputy"))
conjoint_03$attr_pty_mem  <- factor(conjoint_03$party_c,   levels = c("Member of Party","Independent supported by Party","Independent"))
conjoint_03$attr_res      <- factor(conjoint_03$resid_c,   levels = c("Live in another County","Same County"))
conjoint_03$attr_rel      <- factor(conjoint_03$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_03$attr_class    <- factor(conjoint_03$class_c,   levels = c("Working Class","Middle Class","Upper Class"))
conjoint_03$attr_occup    <- factor(conjoint_03$occup_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                      "Accountant","Teacher","Nurse Technician",
                                                                      "Lawyer","Civil Engineer in Mining","Physician"))
conjoint_03$classf1_c     <- car::recode(conjoint_03$classf_c,"'Check'=NA")
conjoint_03$occupf1_c     <- car::recode(conjoint_03$occupf_c,"'Check'=NA")
conjoint_03$attr_class_f  <- factor(conjoint_03$classf1_c,   levels = c("Working Class","Middle Class","Upper Class"))
conjoint_03$attr_occup_f  <- factor(conjoint_03$occupf1_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                        "Accountant","Teacher","Nurse Technician",
                                                                        "Lawyer","Civil Engineer in Mining","Physician"))


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
  filter(check1 == "03"|check1 == "11") %>% dplyr::select(-check1) -> conjoint_w01$conjoint_w01_global_att1

### Wave 1, Screener 1 and Screener 2
conjoint_01 %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_w01$conjoint_w01_global_att12

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
  filter(check1 == "03"|check1 == "11") %>% dplyr::select(-check1) -> conjoint_w01$conjoint_w01_vote_att1

### Wave 1, Screener 1 and Screener 2
conjoint_w01$conjoint_w01_vote %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_w01$conjoint_w01_vote_att12

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
  filter(check1 == "03"|check1 == "11") %>% dplyr::select(-check1) -> conjoint_w01$conjoint_w01_eval_att1

### Wave 1, Screener 1 and Screener 2
conjoint_w01$conjoint_w01_eval %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_w01$conjoint_w01_eval_att12

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
  filter(check1 == "03"|check1 == "11") %>% dplyr::select(-check1) -> conjoint_w01$conjoint_w01_filt_att1

## Full Sample, Variance in Vote Choice and Evaluation and Screeners 1 and 2 (Wave 1)
conjoint_w01$conjoint_w01_filt %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_w01$conjoint_w01_filt_att12

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


## Create special task variable (Wave 1)
for(i in 1: length(conjoint_w01)){
  
  conjoint_w01[[i]]$task_alt <- cumsum(!duplicated(conjoint_w01[[i]][, c("task","idperson")]))
}

## Create special task variable (Wave 3)
for(i in 1: length(conjoint_w03)){
  
  conjoint_w03[[i]]$task_alt <- cumsum(!duplicated(conjoint_w03[[i]][, c("task","idperson")]))
}


##########################################################################################################################################
#STEP 4: CONJACP DATA MANAGEMENT

## Empty List to store processed data.frames

### Wave 1
conjacp_class_b_w01 <- list()
conjacp_class_w_w01 <- list()
conjacp_occup_b_w01 <- list()
conjacp_occup_w_w01 <- list()

### Wave 3
conjacp_class_b_w03 <- list()
conjacp_class_w_w03 <- list()
conjacp_occup_b_w03 <- list()
conjacp_occup_w_w03 <- list()


## Create CONJACP object for ACP and C-ACP estimation. 

### For Loop, Wave 1
for(i in 1:length(conjoint_w01)){
  
  ### Social Class, no weights
  conjacp_class_b_w01[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_class,
                                               data = conjoint_w01[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson")
  ### Social Class, weights
  conjacp_class_w_w01[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_class,
                                               data = conjoint_w01[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson",
                                               level_weights = list(list("attr_sex",      c(1/2,1/2)),
                                                                    list("attr_age",      c(1/3,1/3,1/3)),
                                                                    list("attr_ideol",    c(2/8,1/8,2/8,2/8,1/8)),
                                                                    list("attr_pol_exp",  c(2/4,1/4,1/4)),
                                                                    list("attr_pty_mem",  c(1/3,1/3,1/3)),
                                                                    list("attr_res",      c(1/2,1/2)),
                                                                    list("attr_rel",      c(5/10,2/10,3/10)),
                                                                    list("attr_class",    c(1/3,1/3,1/3))))
  
  ### Occupation, no weights
  conjacp_occup_b_w01[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_occup,
                                               data = conjoint_w01[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson")
  ### Occupation, weights
  conjacp_occup_w_w01[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_occup,
                                               data = conjoint_w01[[i]],  tasks = "task_alt", subgroups = NULL, id = "idperson",
                                               level_weights = list(list("attr_sex",     c(1/2,1/2)),
                                                                    list("attr_age",     c(1/3,1/3,1/3)),
                                                                    list("attr_ideol",   c(2/8,1/8,2/8,2/8,1/8)),
                                                                    list("attr_pol_exp", c(2/4,1/4,1/4)),
                                                                    list("attr_pty_mem", c(1/3,1/3,1/3)),
                                                                    list("attr_res",     c(1/2,1/2)),
                                                                    list("attr_rel",     c(5/10,2/10,3/10)),
                                                                    list("attr_occup",   c(1/9,1/9,1/9,1/9,1/9,1/9))))
}


### For Loop, Wave 3
for(i in 1:length(conjoint_w03)){
  
  ### Social Class, no weights
  conjacp_class_b_w03[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_class,
                                               data = conjoint_w03[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson")
  ### Social Class, weights
  conjacp_class_w_w03[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_class,
                                               data = conjoint_w03[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson",
                                               level_weights = list(list("attr_sex",      c(1/2,1/2)),
                                                                    list("attr_age",      c(1/3,1/3,1/3)),
                                                                    list("attr_ideol",    c(2/8,1/8,2/8,2/8,1/8)),
                                                                    list("attr_pol_exp",  c(2/4,1/4,1/4)),
                                                                    list("attr_pty_mem",  c(1/3,1/3,1/3)),
                                                                    list("attr_res",      c(1/2,1/2)),
                                                                    list("attr_rel",      c(5/10,2/10,3/10)),
                                                                    list("attr_class",    c(1/3,1/3,1/3))))
  
  ### Occupation, no weights
  conjacp_occup_b_w03[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_occup,
                                               data = conjoint_w03[[i]], tasks = "task_alt", subgroups = NULL, id = "idperson")
  ### Occupation, weights
  conjacp_occup_w_w03[[i]] <- conjacp.prepdata(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                                 attr_pty_mem + attr_res + attr_rel + attr_occup,
                                               data = conjoint_w03[[i]],  tasks = "task_alt", subgroups = NULL, id = "idperson",
                                               level_weights = list(list("attr_sex",     c(1/2,1/2)),
                                                                    list("attr_age",     c(1/3,1/3,1/3)),
                                                                    list("attr_ideol",   c(2/8,1/8,2/8,2/8,1/8)),
                                                                    list("attr_pol_exp", c(2/4,1/4,1/4)),
                                                                    list("attr_pty_mem", c(1/3,1/3,1/3)),
                                                                    list("attr_res",     c(1/2,1/2)),
                                                                    list("attr_rel",     c(5/10,2/10,3/10)),
                                                                    list("attr_occup",   c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9))))
}


##########################################################################################################################################
#STEP 5: ESTIMATION OF ACP

## Empty List to store results

### Wave 1
conjacp_estimation_class_b_w01 <- list()
conjacp_estimation_class_w_w01 <- list()
conjacp_estimation_occup_b_w01 <- list()
conjacp_estimation_occup_w_w01 <- list()

### Wave 3
conjacp_estimation_class_b_w03 <- list()
conjacp_estimation_class_w_w03 <- list()
conjacp_estimation_occup_b_w03 <- list()
conjacp_estimation_occup_w_w03 <- list()

 
## Estimation of ACPs

### For Loop, Wave 1
for(i in 1:36){
  
  ### Social Class. Unweighted. Wave 1
  conjacp.estimation(conjacp_class_b_w01[[i]],   estimand = "acp", adjust = FALSE) -> temp1
  summary(temp1)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 1, Unweighted, Class", sample = i) -> conjacp_estimation_class_b_w01[[i]]
  
  ### Social Class. Weighted. Wave 1
  conjacp.estimation(conjacp_class_w_w01[[i]],   estimand = "acp", adjust = FALSE) -> temp2
  summary(temp2)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 1, Weighted, Class", sample = i) -> conjacp_estimation_class_w_w01[[i]]
  
  ### Occupation. Unweighted. Wave 1
  conjacp.estimation(conjacp_occup_b_w01[[i]],   estimand = "acp", adjust = FALSE) -> temp3
  summary(temp3)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 1, Unweighted, Occupation", sample = i) -> conjacp_estimation_occup_b_w01[[i]]
  
  ### Occupation. Weighted. Wave 1
  conjacp.estimation(conjacp_occup_w_w01[[i]],   estimand = "acp", adjust = FALSE) -> temp4
  summary(temp4)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 1, Weighted, Occupation", sample = i) -> conjacp_estimation_occup_w_w01[[i]]

}


### For Loop, Wave 3
for(i in 1:30){
  
  ### Social Class. Unweighted. Wave 3
  conjacp.estimation(conjacp_class_b_w03[[i]],   estimand = "acp", adjust = FALSE) -> temp5
  summary(temp5)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 3, Unweighted, Class", sample = i) -> conjacp_estimation_class_b_w03[[i]]
  
  ### Social Class. Weighted. Wave 3
  conjacp.estimation(conjacp_class_w_w03[[i]],   estimand = "acp", adjust = FALSE) -> temp6
  summary(temp6)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 3, Weighted, Class", sample = i) -> conjacp_estimation_class_w_w03[[i]]
  
  ### Occupation. Unweighted. Wave 3
  conjacp.estimation(conjacp_occup_b_w03[[i]],   estimand = "acp", adjust = FALSE) -> temp7
  summary(temp7)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 3, Unweighted, Occupation", sample = i) -> conjacp_estimation_occup_b_w03[[i]]
  
  ### Occupation. Weighted. Wave 3
  conjacp.estimation(conjacp_occup_w_w03[[i]],   estimand = "acp", adjust = FALSE) -> temp8
  summary(temp8)$coefficients %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(type = "Wave 3, Weighted, Occupation", sample = i) -> conjacp_estimation_occup_w_w03[[i]]
  
}

## Transform into a single data.frame

### Wave 1
bind_rows(
  do.call("rbind.data.frame", conjacp_estimation_class_b_w01),
  do.call("rbind.data.frame", conjacp_estimation_class_w_w01),
  do.call("rbind.data.frame", conjacp_estimation_occup_b_w01),
  do.call("rbind.data.frame", conjacp_estimation_occup_w_w01)) -> temp_w01_df

### Wave 3
bind_rows(
  do.call("rbind.data.frame", conjacp_estimation_class_b_w03),
  do.call("rbind.data.frame", conjacp_estimation_class_w_w03),
  do.call("rbind.data.frame", conjacp_estimation_occup_b_w03),
  do.call("rbind.data.frame", conjacp_estimation_occup_w_w03)) -> temp_w03_df


## Definitive Format

### Wave 1
temp_w01_df %>% 
  ### Recode type of sub-sample
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
  ### Split attribute and levels
  mutate(attribute = str_split(string = rowname, pattern = "\\.", simplify = TRUE)[,1],
         level     = str_split(string = rowname, pattern = "\\.", simplify = TRUE)[,2]) %>% 
  ### Remove irrelevant variables and reorder the resulting data.frame
  dplyr::select(-sample,-rowname) %>% rename(sample = sample_esp) %>%
  dplyr::select(type,sample, attribute, level,Estimate,`Std. Error`)-> wave_01

### Wave 3
temp_w03_df %>% 
  ### Recode type of sub-sample
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
  ### Split attribute and levels
  mutate(attribute = str_split(string = rowname, pattern = "\\.", simplify = TRUE)[,1],
         level     = str_split(string = rowname, pattern = "\\.", simplify = TRUE)[,2]) %>% 
  ### Remove irrelevant variables and reorder the resulting data.frame
  dplyr::select(-sample,-rowname) %>% rename(sample = sample_esp) %>%
  dplyr::select(type,sample, attribute, level,Estimate,`Std. Error`)-> wave_03


## Store results
write_rds(x = wave_01, file = "0C_Objects/ACP_Intermediate_Analysis_Wave_01.rds")
write_rds(x = wave_03, file = "0C_Objects/ACP_Intermediate_Analysis_Wave_03.rds")


## Remove objects
rm(conjacp_class_b_w01,conjacp_class_b_w03,conjacp_class_w_w01,conjacp_class_w_w03,
   conjacp_occup_b_w01,conjacp_occup_b_w03,conjacp_occup_w_w01,conjacp_occup_w_w03,
   conjacp_estimation_class_b_w01,conjacp_estimation_class_b_w03,
   conjacp_estimation_class_w_w01,conjacp_estimation_class_w_w03,
   conjacp_estimation_occup_b_w01,conjacp_estimation_occup_b_w03,
   conjacp_estimation_occup_w_w01,conjacp_estimation_occup_w_w03,
   conjoint_w01,conjoint_w03,i,temp_df,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,
   temp_w01_df,temp_w03_df)
rm(conjacp, conjacp.diff, conjacp.estimation, conjacp.prepdata, conjacp.var, print.conjacp,
   print.summary.conjacp, summary.conjacp)

## Restart session
.rs.restartR()

acp_df %>% filter(attribute == "attrclass")
acp_df %>% filter(attribute == "attroccup")


##########################################################################################################################################
#STEP 6: MULTIPLE HYPOTHESIS

## Import estimated results
wave_01 <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_01.rds")
wave_03 <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_03.rds")

## Import datasets with sample size information
sample_01 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01_corr.rds")
sample_03 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03_corr.rds")

## Extract sample size
sample_01 %>% group_by(sample) %>% summarise(size = mean(size)) -> sample_01f
sample_03 %>% group_by(sample) %>% summarise(size = mean(size)) -> sample_03f

## Add sample size information
wave_01f <- left_join(x = wave_01, y = sample_01f, by = "sample")
wave_03f <- left_join(x = wave_03, y = sample_03f, by = "sample")


## Correct P-Value and Confidence Intervals

### Wave 1
wave_01f %>% mutate(class = paste0(type,"-",sample)) %>% 
  mutate(treatment = ifelse(str_detect(class, "Class")==TRUE,1,0)) %>% 
  group_by(class) %>% 
  mutate(t1          = size*dim(conjoint_01)[1]/100,
         t2          = ifelse(treatment == 1, 
                              qt(1 - 0.05/24, df = t1, lower.tail = TRUE),
                              qt(1 - 0.05/27, df = t1, lower.tail = TRUE)),
         lower_bonff = Estimate - t2*`Std. Error`,
         upper_bonff = Estimate + t2*`Std. Error`) -> wave_01f

### Wave 3
wave_03f %>% mutate(class = paste0(type,"-",sample)) %>% 
  mutate(treatment = ifelse(str_detect(class, "Class")==TRUE,1,0)) %>% 
  group_by(class) %>% 
  mutate(t1          = size*dim(conjoint_03)[1]/100,
         t2          = ifelse(treatment == 1, 
                              qt(1 - 0.05/24, df = t1, lower.tail = TRUE),
                              qt(1 - 0.05/30, df = t1, lower.tail = TRUE)),
         lower_bonff = Estimate - t2*`Std. Error`,
         upper_bonff = Estimate + t2*`Std. Error`) -> wave_03f

### Store the results
write_rds(x = wave_01f, file = "0C_Objects/ACP_Intermediate_Analysis_Wave_01_corr.rds")
write_rds(x = wave_03f, file = "0C_Objects/ACP_Intermediate_Analysis_Wave_03_corr.rds")


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
#  [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] factorEx_1.0.1          genlasso_1.5            igraph_1.2.11           arm_1.12-2              lme4_1.1-27.1          
#[6] MASS_7.3-55             cregg_0.4.0             cjoint_2.1.0            survey_4.1-1            survival_3.2-13        
#[11] Matrix_1.4-0            lmtest_0.9-39           zoo_1.8-9               sandwich_3.0-1          forcats_0.5.1          
#[16] stringr_1.4.0           dplyr_1.0.8             purrr_0.3.4             readr_2.1.1             tidyr_1.2.0            
#[21] tibble_3.1.6            tidyverse_1.3.1         sf_1.0-5                sjmisc_2.8.9            sjlabelled_1.1.8       
#[26] rnaturalearthdata_0.1.0 rnaturalearth_0.1.0     tayloRswift_0.1.0       ggthemes_4.2.4          ggcorrplot_0.1.3       
#[31] ggplot2_3.3.5          
#
#loaded via a namespace (and not attached):
#  [1] TH.data_1.1-0      minqa_1.2.4        colorspace_2.0-2   ellipsis_0.3.2     class_7.3-20       estimability_1.3   ggstance_0.3.5    
#[8] parameters_0.16.0  fs_1.5.2           rstudioapi_0.13    proxy_0.4-26       listenv_0.8.0      prodlim_2019.11.13 fansi_1.0.2       
#[15] mvtnorm_1.1-3      lubridate_1.8.0    xml2_1.3.3         codetools_0.2-18   splines_4.1.2      doParallel_1.0.16  knitr_1.37        
#[22] texreg_1.37.5      Formula_1.2-4      jsonlite_1.7.3     nloptr_2.0.0       ggeffects_1.1.1    broom_0.7.12       dbplyr_2.1.1      
#[29] effectsize_0.6.0.1 shiny_1.7.1        compiler_4.1.2     httr_1.4.2         sjstats_0.18.1     emmeans_1.7.2      backports_1.4.1   
#[36] assertthat_0.2.1   fastmap_1.1.0      cli_3.2.0          later_1.3.0        htmltools_0.5.2    tools_4.1.2        coda_0.19-4       
#[43] gtable_0.3.0       glue_1.6.1         Rcpp_1.0.8         carData_3.0-5      cellranger_1.1.0   vctrs_0.3.8        sjPlot_2.8.10     
#[50] nlme_3.1-155       iterators_1.0.13   insight_0.15.0     xfun_0.29          globals_0.14.0     rvest_1.0.2        mime_0.12         
#[57] lifecycle_1.0.1    estimatr_0.30.4    future_1.23.0      scales_1.1.1       hms_1.1.1          promises_1.2.0.1   parallel_4.1.2    
#[64] pbapply_1.5-0      stringi_1.7.6      bayestestR_0.11.5  foreach_1.5.1      e1071_1.7-9        boot_1.3-28        lava_1.6.10       
#[71] rlang_1.0.1        pkgconfig_2.0.3    lattice_0.20-45    tidyselect_1.1.1   parallelly_1.30.0  magrittr_2.0.2     R6_2.5.1          
#[78] generics_0.1.2     multcomp_1.4-18    DBI_1.1.2          pillar_1.7.0       haven_2.4.3        withr_2.4.3        units_0.7-2       
#[85] datawizard_0.2.3   abind_1.4-5        sp_1.4-6           performance_0.8.0  future.apply_1.8.1 car_3.0-12         modelr_0.1.8      
#[92] crayon_1.5.0       KernSmooth_2.23-20 utf8_1.2.2         tzdb_0.2.0         readxl_1.3.1       reprex_2.0.1       digest_0.6.29     
#[99] classInt_0.4-3     pbmcapply_1.5.0    xtable_1.8-4       httpuv_1.6.5       munsell_0.5.0      mitools_2.4  