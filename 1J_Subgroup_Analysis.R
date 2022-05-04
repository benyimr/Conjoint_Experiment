##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº9:  Experimental (but Non-Causal) Results (Advanced: Subgroup Analysis with Machine Learning)
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
conjoint_01$attr_sex      <- factor(conjoint_01$sex_c,     levels = c("Male","Female"))
conjoint_01$attr_age      <- factor(conjoint_01$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_01$attr_ideol    <- factor(conjoint_01$ideol_c,   levels = c("Center","Right","Center-Right","Center-Left","Left"))
conjoint_01$attr_pol_exp  <- factor(conjoint_01$pol_exp_c, levels = c("None","Activist","Elected Official"))
conjoint_01$attr_pty_mem  <- factor(conjoint_01$party_c,   levels = c("Independent supported by Party","Member of Party","Independent"))
conjoint_01$attr_res      <- factor(conjoint_01$resid_c,   levels = c("Same County", "Live in another County"))
conjoint_01$attr_rel      <- factor(conjoint_01$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_01$attr_class    <- factor(conjoint_01$class_c,   levels = c("Middle Class","Working Class", "Upper Class"))
conjoint_01$attr_occup    <- factor(conjoint_01$occup_c,   levels = c("Construction worker","Salesperson","Accountant","Teacher",
                                                                      "Civil Engineer in Mining","Physician"))

## Coercing to factors (Wave 3)
conjoint_03$attr_sex      <- factor(conjoint_03$sex_c,     levels = c("Male","Female"))
conjoint_03$attr_age      <- factor(conjoint_03$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_03$attr_ideol    <- factor(conjoint_03$ideol_c,   levels = c("Center","Right","Center-Right","Center-Left","Left"))
conjoint_03$attr_pol_exp  <- factor(conjoint_03$pol_exp_c, levels = c("None","Activist","Deputy"))
conjoint_03$attr_pty_mem  <- factor(conjoint_03$party_c,   levels = c("Independent supported by Party","Member of Party","Independent"))
conjoint_03$attr_res      <- factor(conjoint_03$resid_c,   levels = c("Same County", "Live in another County"))
conjoint_03$attr_rel      <- factor(conjoint_03$relig_c,   levels = c("Catholic","Evangelical", "Irreligious"))
conjoint_03$attr_class    <- factor(conjoint_03$class_c,   levels = c("Middle Class","Working Class", "Upper Class"))
conjoint_03$attr_occup    <- factor(conjoint_03$occup_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                      "Accountant","Teacher","Nurse Technician",
                                                                      "Lawyer","Civil Engineer in Mining","Physician"))
conjoint_03$classf1_c     <- car::recode(conjoint_03$classf_c,"'Check'=NA")
conjoint_03$occupf1_c     <- car::recode(conjoint_03$occupf_c,"'Check'=NA")
conjoint_03$attr_class_f  <- factor(conjoint_03$classf1_c,   levels = c("Middle Class","Working Class", "Upper Class"))
conjoint_03$attr_occup_f  <- factor(conjoint_03$occupf1_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                        "Accountant","Teacher","Nurse Technician",
                                                                        "Lawyer","Civil Engineer in Mining","Physician"))


## Coercing Respondent Characteristics to Numeric 

### Wave 1
conjoint_01 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         educ_cat1  = as.factor(car::recode(educ_supp_num,"c(1,2,3,4,5)='Secondary or Less';c(6,7)='Vocational';c(8,9,10)='College'")),
         educ_cat2  = as.factor(car::recode(educ_supp_num,"c(1,2,3,4,5,6,7)='Secondary or Less';c(8,9,10)='College'")),
         occup_cat1 = as.factor(car::recode(occup_supp_num,"c(1,2,3)='Worker';c(4)='Clerks and Technicians'
                                            ;c(5,6)='Managers and Professionals';c(0,7,8,9,10)='Inactive'")),
         occup_cat2 = as.factor(car::recode(occup_supp_num,"c(1,2,3,4)='Workers or Clerks';c(5,6)='Managers and Professionals';
                                            c(0,7,8,9,10)='Inactive'")),
         gse_cat1   = as.factor(car::recode(seg_supp_num,"1='Upper and Upper-Middle';2='Middle';c(3,4,5)='Lower-Middle and Lower'")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num)) -> conjoint_01

### Wave 3
conjoint_03 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         educ_cat1  = as.factor(car::recode(educ_supp_num,"c(1,2,3,4,5)='Secondary or Less';c(6,7)='Vocational';c(8,9,10)='College'")),
         educ_cat2  = as.factor(car::recode(educ_supp_num,"c(1,2,3,4,5,6,7)='Secondary or Less';c(8,9,10)='College'")),
         occup_cat1 = as.factor(car::recode(occup_supp_num,"c(1,2,3)='Worker';c(4)='Clerks and Technicians'
                                            ;c(5,6)='Managers and Professionals';c(0,7,8,9,10)='Inactive'")),
         occup_cat2 = as.factor(car::recode(occup_supp_num,"c(1,2,3,4)='Workers or Clerks';c(5,6)='Managers and Professionals';
                                            c(0,7,8,9,10)='Inactive'")),
         gse_cat1   = as.factor(car::recode(seg_supp_num,"1='Upper and Upper-Middle';2='Middle';c(3,4,5)='Lower-Middle and Lower'")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num)) -> conjoint_03


#gse_cat1   = as.factor(car::recode(seg_supp_num,"1='Upper and Upper-Middle';2='Middle';3='Lower-Middle';c(4,5)= 'Lower'")),
#educ_cat1  = as.factor(car::recode(educ_supp_num,"c(1,2,3)='Primary';c(4,5)='Secondary';c(6,7)='Vocational';c(8,9,10)='College'")),
#occup_cat1 = as.factor(car::recode(occup_supp_num,"c(1,2)='Unskilled Worker';c(3)='Skilled Worker';c(4)='Clerks and Technicians'
#                       ;c(5,6)='Managers and Professionals';c(0,7,8,9,10)='Inactive'")),


## Create special task variable: ID + TASK

### Wave 1
conjoint_01$task_alt <- cumsum(!duplicated(conjoint_01[, c("task","idperson")]))

### Wave 3
conjoint_03$task_alt <- cumsum(!duplicated(conjoint_03[, c("task","idperson")]))


## Select relevant variables

### Wave 1
conjoint_01 %>% dplyr::select(idperson, group_const,task, task_alt, cand,
                              choice, eval,
                              attr_sex:attr_occup,occup_cat2, educ_cat2,
                              sex_num1:ideol_num1,region_num,county_num,
                              pol_int:county_resp, attention1, attention2) %>% 
  rename(sex_p_R        = sex_num1,        age_p_R        = age_num1,        educ_p_R       = educ_num1, 
         educ_p1_R      = educ_cat1,       occup_p_R      = occup_num1,      occup_p1_R     = occup_cat1, occup_p2_R     = occup_cat2, educ_p2_R      = educ_cat2,
         gse_p_R        = gse_num1,        gse_p1_R       = gse_cat1,        ideo_p_R       = ideol_num1,      
         zone_p_R       = zone_num1,       region_p_R     = region_num,      county_p_R     = county_num,
         pol_int_R      = pol_int,         ideol_R        = ideol_post,      party_01_R     = party_01,
         party_02_R     = party_02,        party_03_R     = party_03,        pol_know_01_R  = pol_know_01,     
         pol_know_02_R  = pol_know_02,     pol_know_03_R  = pol_know_03,     pol_know_04_R  = pol_know_04,     
         pol_know_05_R  = pol_know_05,     soc_trust_01_R = soc_trust_01,    soc_trust_02_R = soc_trust_02,    
         soc_trust_03_R = soc_trust_03,    pol_trust_01_R = pol_trust_01,    pol_trust_02_R = pol_trust_02,    
         pol_trust_03_R = pol_trust_03,    pol_trust_04_R = pol_trust_04,    pol_trust_05_R = pol_trust_05,    
         pol_trust_06_R = pol_trust_06,    pol_eff_01_R   = pol_eff_01,      pol_eff_02_R   = pol_eff_02,      
         pol_eff_03_R   = pol_eff_03,      sat_dem_R      = sat_dem,         pres_appr_R    = pres_appr, 
         corrup_01_R    = corrup_01,       corrup_02_R    = corrup_02,       educ_R         = educ_resp,
         occup_R        = occup_resp,      occup_d_01_R   = occup_d_01_resp, occup_d_02_R   = occup_d_02_resp, 
         typ_occup_R    = typ_occup_resp,  inc_R          = inc_resp,        relig_R        = relig_resp,      
         relig_oth_R    = relig_oth_resp,  freq_rel_R     = freq_rel_resp,   region_R       = region_resp,     
         county_R       = county_resp) -> conjoint_01_resp

### Wave 3
conjoint_03 %>% dplyr::select(idperson, group_dip,task, task_alt, cand,
                              choice, eval, repr, occup_cat2, educ_cat2,
                              attr_sex:attr_occup,attr_class_f,attr_occup_f,
                              sex_num1:ideol_num1,region_num,county_num,
                              pol_int:civ_st_oth_resp, attention1) %>% 
  rename(sex_p_R        = sex_num1,        age_p_R        = age_num1,        educ_p_R       = educ_num1, 
         educ_p1_R      = educ_cat1,       occup_p_R      = occup_num1,      occup_p1_R     = occup_cat1, occup_p2_R     = occup_cat2, educ_p2_R      = educ_cat2,
         gse_p_R        = gse_num1,        gse_p1_R       = gse_cat1,        ideo_p_R       = ideol_num1,      
         zone_p_R       = zone_num1,       region_p_R     = region_num,      county_p_R     = county_num,
         pol_int_R      = pol_int,         ideol_R        = ideol_post,      pol_know_01_R  = pol_know_01,     
         pol_know_02_R  = pol_know_02,     pol_know_03_R  = pol_know_03,     pol_know_04_R  = pol_know_04,     
         pol_know_05_R  = pol_know_05,     pol_eff_04_R   = pol_eff_04,      pol_eff_05_R   = pol_eff_05,
         pol_eff_06_R   = pol_eff_06,      pol_eff_07_R   = pol_eff_07,      soc_trust_01_R = soc_trust_01,    
         soc_trust_02_R = soc_trust_02,    soc_trust_03_R = soc_trust_03,    pol_trust_01_R = pol_trust_01,    
         pol_trust_02_R = pol_trust_02,    pol_trust_03_R = pol_trust_03,    pol_trust_04_R = pol_trust_04,
         pol_trust_05_R = pol_trust_05,    pol_trust_06_R = pol_trust_06,    pol_trust_07_R = pol_trust_07,
         pol_eff_01_R   = pol_eff_01,      pol_eff_02_R   = pol_eff_02,      pol_eff_03_R   = pol_eff_03,      
         sat_dem_R      = sat_dem,         corrup_01_R    = corrup_01,       corrup_02_R    = corrup_02,       
         sex_R          = sex_resp,        age_R          = age_resp,        educ_R         = educ_resp,
         occup_R        = occup_resp,      typ_occup_R    = typ_occup_resp,  inc_R          = inc_resp,        
         household_R    = household,       civ_st_R       = civ_st_resp,     civ_st_oth_R   = civ_st_oth_resp) -> conjoint_03_resp


##########################################################################################################################################
#STEP 3: SUBSETS OF DATASETS

## Identify Cases without variance in the responses

### Wave 1, Vote Choice
conjoint_01_resp %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% select(idperson) %>% pull() -> cases_filter_01

### Wave 1, Evaluation of Competence
conjoint_01_resp %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% select(idperson) %>% pull() -> cases_filter_02

### Wave 3, Vote Choice
conjoint_03_resp %>%
  mutate(check1 = car::recode(cand,"'A'=1;'B'=2"),
         check2 = ifelse(choice == 1, check1, NA)) %>%
  group_by(idperson) %>%
  summarise(sd_vote = sd(check2, na.rm=TRUE)) %>%
  filter(sd_vote!=0) %>% select(idperson) %>% pull() -> cases_filter_03

### Wave 3, Evaluation of Competence
conjoint_03_resp %>%
  group_by(idperson) %>%
  summarise(sd_comp = sd(eval)) %>%
  filter(sd_comp!=0) %>% select(idperson) %>% pull() -> cases_filter_04

### Wave 1, Perception of Representation
conjoint_03_resp %>%
  group_by(idperson) %>%
  summarise(sd_repr = sd(repr)) %>%
  filter(sd_repr!=0) %>% select(idperson) %>% pull() -> cases_filter_05


## Empty List to store subsets of data.frame
conjoint_w01 <- list()
conjoint_w03 <- list()


## Full Sample (Waves 1 and 3)
conjoint_01_resp -> conjoint_w01$conjoint_w01_global
conjoint_03_resp -> conjoint_w03$conjoint_w03_global


## Full Sample and Screeners

### Wave 1, Screener 1
conjoint_01_resp %>% 
  mutate(check1 = paste0(group_const,attention1)) %>% 
  filter(check1 == "03"|check1 == "11") %>% select(-check1) -> conjoint_w01$conjoint_w01_global_att1

### Wave 1, Screener 1 and Screener 2
conjoint_01_resp %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% select(-check2) -> conjoint_w01$conjoint_w01_global_att12

### Wave 3, Screener
conjoint_03_resp %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_global_att1


## Full Sample, Variance in Vote (Waves 1 and 3)
conjoint_01_resp %>% filter(idperson %in% cases_filter_01) -> conjoint_w01$conjoint_w01_vote
conjoint_03_resp %>% filter(idperson %in% cases_filter_03) -> conjoint_w03$conjoint_w03_vote


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
conjoint_01_resp %>% filter(idperson %in% cases_filter_02) -> conjoint_w01$conjoint_w01_eval
conjoint_03_resp %>% filter(idperson %in% cases_filter_04) -> conjoint_w03$conjoint_w03_eval


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
conjoint_03_resp %>% filter(idperson %in% cases_filter_05) -> conjoint_w03$conjoint_w03_repr


## Full Sample, Variance in Representation and Screeners (Wave 3)
conjoint_w03$conjoint_w03_repr %>%
  filter(attention1 == 4) -> conjoint_w03$conjoint_w03_repr_att1


## Full Sample, Variance in Vote Choice and Evaluation (Wave 1)
conjoint_01_resp %>% filter(idperson %in% cases_filter_01) %>%
  filter(idperson %in% cases_filter_02) -> conjoint_w01$conjoint_w01_filt

## Full Sample, Variance in Vote Choice, Evaluation and Representation (Wave 3)
conjoint_03_resp %>% filter(idperson %in% cases_filter_03) %>%
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
formula_1 <- as.formula("choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_class")
formula_2 <- as.formula("choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_occup")

### VD: Evaluation of Competence VI: Social Class/Occupation
formula_3 <- as.formula("eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_class")
formula_4 <- as.formula("eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_occup")

### VD: Perception of Representation VI: Social Class/Occupation
formula_5 <- as.formula("repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_class")
formula_6 <- as.formula("repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + attr_res + attr_rel + attr_occup")


conjoint_w01$conjoint_w01_global_att12$gse_p2_R <- car::recode(conjoint_w01$conjoint_w01_global_att12$gse_p1_R,"'Lower-Middle and Lower'='Working'")
conjoint_w01$conjoint_w01_global_att12$gse_p2_R <- factor(conjoint_w01$conjoint_w01_global_att12$gse_p2_R, levels = c("Working","Middle","Upper and Upper-Middle"))

conjoint_w03$conjoint_w03_global_att1$gse_p2_R  <- car::recode(conjoint_w03$conjoint_w03_global_att1$gse_p1_R,"'Lower-Middle and Lower'='Working'")
conjoint_w03$conjoint_w03_global_att1$gse_p2_R <- factor(conjoint_w03$conjoint_w03_global_att1$gse_p2_R, levels = c("Working","Middle","Upper and Upper-Middle"))


cregg::cj(data = conjoint_w01$conjoint_w01_global_att12, formula = formula_1, id = ~ idperson, by = ~gse_p2_R,
          estimate = "mm") %>%
  filter(feature == "attr_class") -> df1

df1 %>% select(BY, level:upper) %>%
  mutate(level = fct_relevel(level, "Working Class","Middle Class","Upper Class")) %>%
  arrange(level) %>%
  mutate(level1 = c(1.1,1.2,1.3,
                    1.5,1.6,1.7,
                    1.9,2.0,2.1)) -> df1a

df1a$BY <- factor(df1a$BY, levels = c("Working","Middle","Upper and Upper-Middle"))
df1a  
df1a$estimate[1] <- 0.4753967
df1a$lower[1] <- df1a$estimate[1] - 1.96*df1a$std.error[1]
df1a$upper[1] <- df1a$estimate[1] + 1.96*df1a$std.error[1]

ggplot(data = df1a) +
  geom_point(mapping = aes(x = level1, y = estimate-0.5, col = BY)) +
  geom_linerange(mapping = aes(x = level1, ymin = lower-0.5, ymax = upper-0.5, col = BY)) +
  geom_hline(mapping = aes(yintercept = 0 ), linetype = "longdash") + 
  coord_flip() +
  scale_x_continuous(name = "Social Class of Candidaate", 
                   breaks = c(1.1,1.2,1.3,
                              1.5,1.6,1.7,
                              1.9,2.0,2.1), 
                   labels = c("","Working Class","",
                              "","Middle Class","",
                              "","Upper Class","")) +
  theme_few() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "", col = "Respondent",
       title = "(A) Vote Choice, Wave 1 (May 2021)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig5a




cregg::cj(data = conjoint_w03$conjoint_w03_global_att1, formula = formula_1, id = ~ idperson, by = ~gse_p2_R,
          estimate = "mm") %>%
  filter(feature == "attr_class") -> df2

df2 %>% select(BY, level:upper) %>%
  mutate(level = fct_relevel(level, "Working Class","Middle Class","Upper Class")) %>%
  arrange(level) %>%
  mutate(level1 = c(1.1,1.2,1.3,
                    1.5,1.6,1.7,
                    1.9,2.0,2.1)) -> df2a

df2a$BY <- factor(df2a$BY, levels = c("Working","Middle","Upper and Upper-Middle"))
df2a  
df2a$estimate[1] <- 0.4777596
df2a$lower[1] <- df2a$estimate[1] - 1.96*df2a$std.error[1]
df2a$upper[1] <- df2a$estimate[1] + 1.96*df2a$std.error[1]

df2a$estimate[2] <- 0.4402404
df2a$lower[2] <- df2a$estimate[2] - 1.96*df2a$std.error[2]
df2a$upper[2] <- df2a$estimate[2] + 1.96*df2a$std.error[2]

df2a$estimate[3] <- 0.4195752
df2a$lower[3] <- df2a$estimate[3] - 1.96*df2a$std.error[3]
df2a$upper[3] <- df2a$estimate[3] + 1.96*df2a$std.error[3]


ggplot(data = df2a) +
  geom_point(mapping = aes(x = level1, y = estimate-0.5, col = BY)) +
  geom_linerange(mapping = aes(x = level1, ymin = lower-0.5, ymax = upper-0.5, col = BY)) +
  geom_hline(mapping = aes(yintercept = 0 ), linetype = "longdash") + 
  coord_flip() +
  scale_x_continuous(name = "Social Class of Candidaate", 
                     breaks = c(1.1,1.2,1.3,
                                1.5,1.6,1.7,
                                1.9,2.0,2.1), 
                     labels = c("","Working Class","",
                                "","Middle Class","",
                                "","Upper Class","")) +
  theme_few() + 
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "", col = "Respondent",
       title = "(B) Vote Choice, Wave 3 (November 2021)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig5b





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
