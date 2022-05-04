##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº9:  Experimental (but Non-Causal) Results (Advanced: Temporal Stability)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(causalToolbox)
library(cjbart)
library(grf)
library(ranger)       
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


##########################################################################################################################################
#STEP 2: FORMATTING VARIABLES 

## Coercing to factors (Wave 1)
conjoint_01$attr_sex      <- factor(conjoint_01$sex_c,     levels = c("Female","Male"))
conjoint_01$attr_age      <- factor(conjoint_01$age_c,     levels = c("25 years","70 years","50 years"))
conjoint_01$attr_ideol    <- factor(conjoint_01$ideol_c,   levels = c("Right","Center-Right","Center-Left","Left","Center"))
conjoint_01$attr_pol_exp  <- factor(conjoint_01$pol_exp_c, levels = c("Activist","Elected Official","None"))
conjoint_01$attr_pty_mem  <- factor(conjoint_01$party_c,   levels = c("Member of Party","Independent", "Independent supported by Party"))
conjoint_01$attr_res      <- factor(conjoint_01$resid_c,   levels = c("Same County", "Live in another County"))
conjoint_01$attr_rel      <- factor(conjoint_01$relig_c,   levels = c("Evangelical", "Irreligious", "Catholic"))
conjoint_01$attr_class    <- factor(conjoint_01$class_c,   levels = c("Upper Class","Working Class", "Middle Class"))
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


## Coercing Respondent Characteristics to Numeric 

### Wave 1
conjoint_01 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num)) -> conjoint_01

### Wave 3
conjoint_03 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"1=1;2=2;3=3;c(4,5)=4")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num)) -> conjoint_03


## Create special task variable: ID + TASK

### Wave 1
conjoint_01$task_alt <- cumsum(!duplicated(conjoint_01[, c("task","idperson")]))

### Wave 3
conjoint_03$task_alt <- cumsum(!duplicated(conjoint_03[, c("task","idperson")]))


## Select relevant variables

### Wave 1
conjoint_01 %>% dplyr::select(idperson, group_const,task, task_alt, cand,
                              choice, eval,
                              attr_sex:attr_occup,
                              sex_num1:ideol_num1,region_num,county_num,
                              pol_int:county_resp, attention1, attention2) %>% 
  rename(sex_p_R        = sex_num1,        age_p_R        = age_num1,        educ_p_R       = educ_num1, 
         occup_p_R      = occup_num1,      gse_p_R        = gse_num1,        ideo_p_R       = ideol_num1,      
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
                              choice, eval, repr,
                              attr_sex:attr_occup,attr_class_f,attr_occup_f,
                              sex_num1:ideol_num1,region_num,county_num,
                              pol_int:civ_st_oth_resp, attention1) %>% 
  rename(sex_p_R        = sex_num1,        age_p_R        = age_num1,        educ_p_R       = educ_num1, 
         occup_p_R      = occup_num1,      gse_p_R        = gse_num1,        ideo_p_R       = ideol_num1,      
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
conjoint_01_resp %>% filter(idperson %in% cases_filter_01) %>%
  filter(idperson %in% cases_filter_02) %>% 
  mutate(check2 = paste0(group_const,attention1,attention2)) %>% 
  filter(check2 == "034"|check2 == "114") %>% dplyr::select(-check2) -> conjoint_01_filtered

## Full Sample, Variance in Vote Choice, Evaluation, and Representation and Screener (Wave 3)
conjoint_03_resp %>% filter(idperson %in% cases_filter_03) %>%
  filter(idperson %in% cases_filter_04) %>% filter(idperson %in% cases_filter_05) %>%
  filter(attention1 == 4) -> conjoint_03_filtered

## Remove objects
rm(cases_filter_01,cases_filter_02,cases_filter_03,cases_filter_04,cases_filter_05,
   conjoint_01, conjoint_03)


##########################################################################################################################################
#STEP 4: DATA MANAGEMENT FOR ESTIMATION OF IMCEs (OLS)

## Recode Attributes as numeric variables (ideally dichotomic)

### Wave: 1, Sample: Filtered
conjoint_01_filtered %>% 
  mutate(attr_sex_D     = car::recode(attr_sex,    "'Female'=0;'Male'=1"),
         attr_age_D     = car::recode(attr_age,    "c('25 years','70 years')=0;'50 years'=1"),
         attr_ideol_T   = car::recode(attr_ideol,  "c('Right','Center-Right')=0;'Center'=1;c('Center-Left','Left')=2"),
         attr_pol_exp_D = car::recode(attr_pol_exp,"'None'=0;c('Activist','Elected Official')=1"),
         attr_pty_mem_D = car::recode(attr_pty_mem, "c('Independent','Independent supported by Party')=0;'Member of Party'=1"),
         attr_rel_D     = car::recode(attr_rel,     "c('Catholic','Evangelical')=0;'Irreligious'=1"),
         attr_res_D     = car::recode(attr_res,     "'Live in another County'=0;'Same County'=1"),
         attr_class_D   = car::recode(attr_class,   "c('Middle Class','Upper Class')=0;'Working Class'=1")) -> conjoint_01_filtered

### Wave: 3, Sample: Filtered
conjoint_03_filtered %>% 
  mutate(attr_sex_D     = car::recode(attr_sex,    "'Female'=0;'Male'=1"),
         attr_age_D     = car::recode(attr_age,    "c('25 years','70 years')=0;'50 years'=1"),
         attr_ideol_T   = car::recode(attr_ideol,  "c('Right','Center-Right')=0;'Center'=1;c('Center-Left','Left')=2"),
         attr_pol_exp_D = car::recode(attr_pol_exp,"'None'=0;c('Activist','Deputy')=1"),
         attr_pty_mem_D = car::recode(attr_pty_mem, "c('Independent','Independent supported by Party')=0;'Member of Party'=1"),
         attr_rel_D     = car::recode(attr_rel,     "c('Catholic','Evangelical')=0;'Irreligious'=1"),
         attr_res_D     = car::recode(attr_res,     "'Live in another County'=0;'Same County'=1"),
         attr_class_D   = car::recode(attr_class,   "c('Middle Class','Upper Class')=0;'Working Class'=1")) -> conjoint_03_filtered


## Identify cases with no variance in the dichotomized attributes

### Wave 1
conjoint_01_filtered %>%
  group_by(idperson) %>%
  summarise(sd_sex     = sd(as.numeric(as.character(attr_sex_D)),     na.rm=TRUE),
            sd_age     = sd(as.numeric(as.character(attr_age_D)),     na.rm=TRUE),
            sd_ideol   = sd(as.numeric(as.character(attr_ideol_T)),   na.rm=TRUE),
            sd_pol_exp = sd(as.numeric(as.character(attr_pol_exp_D)), na.rm=TRUE),
            sd_pty_mem = sd(as.numeric(as.character(attr_pty_mem_D)), na.rm=TRUE),
            sd_rel     = sd(as.numeric(as.character(attr_rel_D)),     na.rm=TRUE),
            sd_res     = sd(as.numeric(as.character(attr_res_D)),     na.rm=TRUE),
            sd_class   = sd(as.numeric(as.character(attr_class_D)),   na.rm=TRUE)) %>% 
  filter(sd_sex != 0, sd_age != 0, sd_ideol !=0, sd_pol_exp !=0, sd_pty_mem !=0,
         sd_rel !=0, sd_res !=0, sd_class !=0)  %>% dplyr::select(idperson) %>% pull() -> cases_sd_filter_01

### Wave 3
conjoint_03_filtered %>%
  group_by(idperson) %>%
  summarise(sd_sex     = sd(as.numeric(as.character(attr_sex_D)),     na.rm=TRUE),
            sd_age     = sd(as.numeric(as.character(attr_age_D)),     na.rm=TRUE),
            sd_ideol   = sd(as.numeric(as.character(attr_ideol_T)),   na.rm=TRUE),
            sd_pol_exp = sd(as.numeric(as.character(attr_pol_exp_D)), na.rm=TRUE),
            sd_pty_mem = sd(as.numeric(as.character(attr_pty_mem_D)), na.rm=TRUE),
            sd_rel     = sd(as.numeric(as.character(attr_rel_D)),     na.rm=TRUE),
            sd_res     = sd(as.numeric(as.character(attr_res_D)),     na.rm=TRUE),
            sd_class   = sd(as.numeric(as.character(attr_class_D)),   na.rm=TRUE)) %>% 
  filter(sd_sex != 0, sd_age != 0, sd_ideol !=0, sd_pol_exp !=0, sd_pty_mem !=0,
         sd_rel !=0, sd_res !=0, sd_class !=0)  %>% dplyr::select(idperson) %>% pull() -> cases_sd_filter_03


## Filter datasets
conjoint_01_filtered %>% filter(idperson %in% cases_sd_filter_01) %>% 
  filter(idperson != 5971, idperson != 2843, idperson != 1440, idperson != 2177,
         idperson != 4245, idperson != 5272, idperson != 5261)  -> conjoint_01_filtered_esp 
conjoint_03_filtered %>% filter(idperson %in% cases_sd_filter_03) %>% 
  filter(idperson != 2703, idperson != 1779, idperson != 1080, idperson != 3769) -> conjoint_03_filtered_esp 


## Identify cases in both waves
intersect(conjoint_01_filtered_esp$idperson, conjoint_03_filtered_esp$idperson) -> vctr_cases

## Filter datasets
conjoint_01_filtered_esp %>% filter(idperson %in% vctr_cases) %>% mutate(idperson = as.factor(idperson)) -> conjoint_01_filtered_esp
conjoint_03_filtered_esp %>% filter(idperson %in% vctr_cases) %>% mutate(idperson = as.factor(idperson)) -> conjoint_03_filtered_esp 

## Remove objects
rm(cases_sd_filter_01,cases_sd_filter_03, vctr_cases)


##########################################################################################################################################
#STEP 5: INDIVIDUAL MARGINAL COMPONENT EFFECT (ZHIRKOV, OLS): POINT ESTIMATES

## Create temporal objects to store results

### Number of Rows
n01 <- length(levels(conjoint_01_filtered_esp$idperson)) # Wave: 1, Sample: Filtered
n02 <- length(levels(conjoint_03_filtered_esp$idperson)) # Wave: 3, Sample: Filtered

### List of Matrices for estimated coefficients and standard errors
coefs <- list(coefs_01 = matrix(0, nrow=n01, ncol=8), coefs_02 = matrix(0, nrow=n01, ncol=8),
              coefs_03 = matrix(0, nrow=n02, ncol=8), coefs_04 = matrix(0, nrow=n02, ncol=8),
              coefs_05 = matrix(0, nrow=n02, ncol=8))

ses <- list(ses_01 = matrix(0, nrow=n01, ncol=8), ses_02 = matrix(0, nrow=n01, ncol=8),
            ses_03 = matrix(0, nrow=n02, ncol=8), ses_04 = matrix(0, nrow=n02, ncol=8),
            ses_05 = matrix(0, nrow=n02, ncol=8))


## Estimate IMCEs for each respondent/attribute

### Wave: 1, Sample: Filtered, DV: Competence
for (i in 1:n01) {
  
  ### Attribute: Sex
  mod <- lm(eval ~ attr_sex_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,1] <- summ$coefficients[2,1]
  ses[[1]][i,1]   <- summ$coefficients[2,2]
  
  ### Attribute: Age
  mod <- lm(eval ~ attr_age_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,2] <- summ$coefficients[2,1]
  ses[[1]][i,2]   <- summ$coefficients[2,2]
  
  ### Attribute: Ideology
  mod <- lm(eval ~ attr_ideol_T,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,3] <- summ$coefficients[2,1]
  ses[[1]][i,3]   <- summ$coefficients[2,2]
  
  ### Attribute: Political Experience
  mod <- lm(eval ~ attr_pol_exp_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )  
  summ            <- summary(mod)
  coefs[[1]][i,4] <- summ$coefficients[2,1]
  ses[[1]][i,4]   <- summ$coefficients[2,2]
  
  ### Attribute: Party Membership
  mod <- lm(eval ~ attr_pty_mem_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,5] <- summ$coefficients[2,1]
  ses[[1]][i,5]   <- summ$coefficients[2,2]
  
  ### Attribute: Residence
  mod <- lm(eval ~ attr_res_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,6] <- summ$coefficients[2,1]
  ses[[1]][i,6]   <- summ$coefficients[2,2]
  
  ### Attribute: Religion
  mod <- lm(eval ~ attr_rel_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,7] <- summ$coefficients[2,1]
  ses[[1]][i,7]   <- summ$coefficients[2,2]
  
  ### Attribute: Social Class
  mod <- lm(eval ~ attr_class_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[1]][i,8] <- summ$coefficients[2,1]
  ses[[1]][i,8]   <- summ$coefficients[2,2]
}

### Transform into a data.frame
coef.exp_01 <- cbind(levels(conjoint_01_filtered_esp$idperson), coefs[[1]], ses[[1]])
coef.exp_01 %>% as.data.frame() %>% sapply(., as.numeric) -> coef.exp_01

### Assign names
colnames(coef.exp_01) <- c('idperson',
                           'imce_sex_eval_w01',       'imce_age_eval_w01',     'imce_ideol_eval_w01',
                           'imce_pol_exp_eval_w01',   'imce_pty_mem_eval_w01', 'imce_rel_eval_w01',
                           'imce_res_eval_w01',       'imce_class_eval_w01',   'imce_sex_se_eval_w01',
                           'imce_age_se_eval_w01',    'imce_ideol_se_eval_w01','imce_pol_exp_se_eval_w01',
                           'imce_pty_mem_se_eval_w01','imce_rel_se_eval_w01',   'imce_res_se_eval_w01',
                           'imce_class_se_eval_w01')


### Wave: 1, Sample: Filtered, DV: Vote Choice
for (i in 1:n01) {
  
  ### Attribute: Sex
  mod <- lm(choice ~ attr_sex_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,1] <- summ$coefficients[2,1]
  ses[[2]][i,1]   <- summ$coefficients[2,2]
  
  ### Attribute: Age
  mod <- lm(choice ~ attr_age_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,2] <- summ$coefficients[2,1]
  ses[[2]][i,2]   <- summ$coefficients[2,2]
  
  ### Attribute: Ideology
  mod <- lm(choice ~ attr_ideol_T,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,3] <- summ$coefficients[2,1]
  ses[[2]][i,3]   <- summ$coefficients[2,2]
  
  ### Attribute: Political Experience
  mod <- lm(choice ~ attr_pol_exp_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )  
  summ            <- summary(mod)
  coefs[[2]][i,4] <- summ$coefficients[2,1]
  ses[[2]][i,4]   <- summ$coefficients[2,2]
  
  ### Attribute: Party Membership
  mod <- lm(choice ~ attr_pty_mem_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,5] <- summ$coefficients[2,1]
  ses[[2]][i,5]   <- summ$coefficients[2,2]
  
  ### Attribute: Residence
  mod <- lm(choice ~ attr_res_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,6] <- summ$coefficients[2,1]
  ses[[2]][i,6]   <- summ$coefficients[2,2]
  
  ### Attribute: Religion
  mod <- lm(choice ~ attr_rel_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,7] <- summ$coefficients[2,1]
  ses[[2]][i,7]   <- summ$coefficients[2,2]
  
  ### Attribute: Social Class
  mod <- lm(choice ~ attr_class_D,
            data = conjoint_01_filtered_esp,
            subset = (idperson==levels(conjoint_01_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[2]][i,8] <- summ$coefficients[2,1]
  ses[[2]][i,8]   <- summ$coefficients[2,2]
}

### Transform into a data.frame
coef.exp_02 <- cbind(levels(conjoint_01_filtered_esp$idperson), coefs[[2]], ses[[2]])
coef.exp_02 %>% as.data.frame() %>% sapply(., as.numeric) -> coef.exp_02

### Assign names
colnames(coef.exp_02) <- c('idperson',
                           'imce_sex_choice_w01',       'imce_age_choice_w01',     'imce_ideol_choice_w01',
                           'imce_pol_exp_choice_w01',   'imce_pty_mem_choice_w01', 'imce_rel_choice_w01',
                           'imce_res_choice_w01',       'imce_class_choice_w01',   'imce_sex_se_choice_w01',
                           'imce_age_se_choice_w01',    'imce_ideol_se_choice_w01','imce_pol_exp_se_choice_w01',
                           'imce_pty_mem_se_choice_w01','imce_rel_se_choice_w01',   'imce_res_se_choice_w01',
                           'imce_class_se_choice_w01')


### Wave: 3, Sample: Filtered, DV: Competence
for (i in 1:n02) {
  
  ### Attribute: Sex
  mod <- lm(eval ~ attr_sex_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,1] <- summ$coefficients[2,1]
  ses[[3]][i,1]   <- summ$coefficients[2,2]
  
  ### Attribute: Age
  mod <- lm(eval ~ attr_age_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,2] <- summ$coefficients[2,1]
  ses[[3]][i,2]   <- summ$coefficients[2,2]
  
  ### Attribute: Ideology
  mod <- lm(eval ~ attr_ideol_T,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,3] <- summ$coefficients[2,1]
  ses[[3]][i,3]   <- summ$coefficients[2,2]
  
  ### Attribute: Political Experience
  mod <- lm(eval ~ attr_pol_exp_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )  
  summ            <- summary(mod)
  coefs[[3]][i,4] <- summ$coefficients[2,1]
  ses[[3]][i,4]   <- summ$coefficients[2,2]
  
  ### Attribute: Party Membership
  mod <- lm(eval ~ attr_pty_mem_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,5] <- summ$coefficients[2,1]
  ses[[3]][i,5]   <- summ$coefficients[2,2]
  
  ### Attribute: Residence
  mod <- lm(eval ~ attr_res_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,6] <- summ$coefficients[2,1]
  ses[[3]][i,6]   <- summ$coefficients[2,2]
  
  ### Attribute: Religion
  mod <- lm(eval ~ attr_rel_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,7] <- summ$coefficients[2,1]
  ses[[3]][i,7]   <- summ$coefficients[2,2]
  
  ### Attribute: Social Class
  mod <- lm(eval ~ attr_class_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[3]][i,8] <- summ$coefficients[2,1]
  ses[[3]][i,8]   <- summ$coefficients[2,2]
}

### Transform into a data.frame
coef.exp_03 <- cbind(levels(conjoint_03_filtered_esp$idperson), coefs[[3]], ses[[3]])
coef.exp_03 %>% as.data.frame() %>% sapply(., as.numeric) -> coef.exp_03

### Assign names
colnames(coef.exp_03) <- c('idperson',
                           'imce_sex_eval_w03',       'imce_age_eval_w03',     'imce_ideol_eval_w03',
                           'imce_pol_exp_eval_w03',   'imce_pty_mem_eval_w03', 'imce_rel_eval_w03',
                           'imce_res_eval_w03',       'imce_class_eval_w03',   'imce_sex_se_eval_w03',
                           'imce_age_se_eval_w03',    'imce_ideol_se_eval_w03','imce_pol_exp_se_eval_w03',
                           'imce_pty_mem_se_eval_w03','imce_rel_se_eval_w03',   'imce_res_se_eval_w03',
                           'imce_class_se_eval_w03')


### Wave: 3, Sample: Filtered, DV: Vote Choice
for (i in 1:n02) {
  
  ### Attribute: Sex
  mod <- lm(choice ~ attr_sex_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,1] <- summ$coefficients[2,1]
  ses[[4]][i,1]   <- summ$coefficients[2,2]
  
  ### Attribute: Age
  mod <- lm(choice ~ attr_age_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,2] <- summ$coefficients[2,1]
  ses[[4]][i,2]   <- summ$coefficients[2,2]
  
  ### Attribute: Ideology
  mod <- lm(choice ~ attr_ideol_T,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,3] <- summ$coefficients[2,1]
  ses[[4]][i,3]   <- summ$coefficients[2,2]
  
  ### Attribute: Political Experience
  mod <- lm(choice ~ attr_pol_exp_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )  
  summ            <- summary(mod)
  coefs[[4]][i,4] <- summ$coefficients[2,1]
  ses[[4]][i,4]   <- summ$coefficients[2,2]
  
  ### Attribute: Party Membership
  mod <- lm(choice ~ attr_pty_mem_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,5] <- summ$coefficients[2,1]
  ses[[4]][i,5]   <- summ$coefficients[2,2]
  
  ### Attribute: Residence
  mod <- lm(choice ~ attr_res_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,6] <- summ$coefficients[2,1]
  ses[[4]][i,6]   <- summ$coefficients[2,2]
  
  ### Attribute: Religion
  mod <- lm(choice ~ attr_rel_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,7] <- summ$coefficients[2,1]
  ses[[4]][i,7]   <- summ$coefficients[2,2]
  
  ### Attribute: Social Class
  mod <- lm(choice ~ attr_class_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[4]][i,8] <- summ$coefficients[2,1]
  ses[[4]][i,8]   <- summ$coefficients[2,2]
}

### Transform into a data.frame
coef.exp_04 <- cbind(levels(conjoint_03_filtered_esp$idperson), coefs[[4]], ses[[4]])
coef.exp_04 %>% as.data.frame() %>% sapply(., as.numeric) -> coef.exp_04

### Assign names
colnames(coef.exp_04) <- c('idperson',
                           'imce_sex_choice_w03',       'imce_age_choice_w03',     'imce_ideol_choice_w03',
                           'imce_pol_exp_choice_w03',   'imce_pty_mem_choice_w03', 'imce_rel_choice_w03',
                           'imce_res_choice_w03',       'imce_class_choice_w03',   'imce_sex_se_choice_w03',
                           'imce_age_se_choice_w03',    'imce_ideol_se_choice_w03','imce_pol_exp_se_choice_w03',
                           'imce_pty_mem_se_choice_w03','imce_rel_se_choice_w03',   'imce_res_se_choice_w03',
                           'imce_class_se_choice_w03')


### Wave: 3, Sample: Filtered, DV: Representation
for (i in 1:n02) {
  
  ### Attribute: Sex
  mod <- lm(repr ~ attr_sex_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,1] <- summ$coefficients[2,1]
  ses[[5]][i,1]   <- summ$coefficients[2,2]
  
  ### Attribute: Age
  mod <- lm(repr ~ attr_age_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,2] <- summ$coefficients[2,1]
  ses[[5]][i,2]   <- summ$coefficients[2,2]
  
  ### Attribute: Ideology
  mod <- lm(repr ~ attr_ideol_T,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,3] <- summ$coefficients[2,1]
  ses[[5]][i,3]   <- summ$coefficients[2,2]
  
  ### Attribute: Political Experience
  mod <- lm(repr ~ attr_pol_exp_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )  
  summ            <- summary(mod)
  coefs[[5]][i,4] <- summ$coefficients[2,1]
  ses[[5]][i,4]   <- summ$coefficients[2,2]
  
  ### Attribute: Party Membership
  mod <- lm(repr ~ attr_pty_mem_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,5] <- summ$coefficients[2,1]
  ses[[5]][i,5]   <- summ$coefficients[2,2]
  
  ### Attribute: Residence
  mod <- lm(repr ~ attr_res_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,6] <- summ$coefficients[2,1]
  ses[[5]][i,6]   <- summ$coefficients[2,2]
  
  ### Attribute: Religion
  mod <- lm(repr ~ attr_rel_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,7] <- summ$coefficients[2,1]
  ses[[5]][i,7]   <- summ$coefficients[2,2]
  
  ### Attribute: Social Class
  mod <- lm(repr ~ attr_class_D,
            data = conjoint_03_filtered_esp,
            subset = (idperson==levels(conjoint_03_filtered_esp$idperson)[i])
  )
  summ            <- summary(mod)
  coefs[[5]][i,8] <- summ$coefficients[2,1]
  ses[[5]][i,8]   <- summ$coefficients[2,2]
}

### Transform into a data.frame
coef.exp_05 <- cbind(levels(conjoint_03_filtered_esp$idperson), coefs[[5]], ses[[5]])
coef.exp_05 %>% as.data.frame() %>% sapply(., as.numeric) -> coef.exp_05

### Assign names
colnames(coef.exp_05) <- c('idperson',
                           'imce_sex_repr_w03',       'imce_age_repr_w03',     'imce_ideol_repr_w03',
                           'imce_pol_exp_repr_w03',   'imce_pty_mem_repr_w03', 'imce_rel_repr_w03',
                           'imce_res_repr_w03',       'imce_class_repr_w03',   'imce_sex_se_repr_w03',
                           'imce_age_se_repr_w03',    'imce_ideol_se_repr_w03','imce_pol_exp_se_repr_w03',
                           'imce_pty_mem_se_repr_w03','imce_rel_se_repr_w03',   'imce_res_se_repr_w03',
                           'imce_class_se_repr_w03')

## Remove irrelevant objects
rm(i, mod, n01, n02, summ)


## Combine into one dataset
temp_01 <- full_join(x= as.data.frame(coef.exp_01), y = as.data.frame(coef.exp_02), by = "idperson")
temp_02 <- full_join(x= as.data.frame(coef.exp_03), y = as.data.frame(coef.exp_04), by = "idperson")
temp_03 <- full_join(x= temp_02,                    y = as.data.frame(coef.exp_05), by = "idperson")
imce    <- full_join(x= temp_01,                    y = temp_03, by = "idperson")


## Correlation tests
cor.test(imce$imce_class_eval_w01,  imce$imce_class_eval_w03)
cor.test(imce$imce_class_choice_w01,imce$imce_class_choice_w03)

cor.test(imce$imce_class_choice_w03,imce$imce_class_repr_w03)
cor.test(imce$imce_class_choice_w01,imce$imce_class_repr_w03)

## Remove irrelevant objects
rm(coef.exp_01,coef.exp_02,coef.exp_03,coef.exp_04,coef.exp_05,coefs,ses,temp_01,temp_02,temp_03)
rm(conjoint_01_filtered, conjoint_01_filtered_esp, conjoint_03_filtered, conjoint_03_filtered_esp)

##########################################################################################################################################
#STEP 6: INDIVIDUAL MARGINAL COMPONENT EFFECT (ZHIRKOV, OLS): BOOTSTRAPPING

##### for future work


##########################################################################################################################################
#STEP 7: DATA MANAGEMENT FOR MACHINE LEARNING

## Identify cases in both waves
intersect(conjoint_01_resp$idperson, conjoint_03_resp$idperson) -> vctr_cases

## Recode attributes into numeric variables and Filter datasets

### Wave 1
conjoint_01_resp %>% 
  mutate(attr_sex1     = as.numeric(attr_sex),
         attr_age1     = as.numeric(attr_age),
         attr_ideol1   = as.numeric(attr_ideol),
         attr_pol_exp1 = as.numeric(attr_pol_exp),
         attr_pty_mem1 = as.numeric(attr_pty_mem),
         attr_res1     = as.numeric(attr_res),
         attr_rel1     = as.numeric(attr_rel),
         attr_class1   = as.numeric(if_else(attr_class=="Working Class",1,0)),
         region_p_R    = as.numeric(region_p_R),
         county_p_R    = as.numeric(car::recode(county_p_R,"''='17000'")),
         typ_occup1_R = ifelse(is.na(typ_occup_R)==TRUE,0, typ_occup_R)) -> conjoint_01_choice

### Sample for BART
conjoint_01_choice %>% 
  dplyr::select(idperson, group_const, task, choice,attr_sex:attr_class,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_01_choice_01

### Sample for Causal Forest and X-Learner
conjoint_01_choice %>% 
  dplyr::select(idperson, group_const, task, choice,attr_sex1:attr_class1,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_01_choice_02

### Sample for BART
conjoint_01_choice %>% 
  dplyr::select(idperson, group_const, task, eval,attr_sex:attr_class,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_01_choice_03

### Sample for Causal Forest and X-Learner
conjoint_01_choice %>% 
  dplyr::select(idperson, group_const, task, eval,attr_sex1:attr_class1,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_01_choice_04

### Wave 3
conjoint_03_resp %>% 
  mutate(attr_sex1     = as.numeric(attr_sex),
         attr_age1     = as.numeric(attr_age),
         attr_ideol1   = as.numeric(attr_ideol),
         attr_pol_exp1 = as.numeric(attr_pol_exp),
         attr_pty_mem1 = as.numeric(attr_pty_mem),
         attr_res1     = as.numeric(attr_res),
         attr_rel1     = as.numeric(attr_rel),
         attr_class1   = as.numeric(if_else(attr_class=="Working Class",1,0)),
         region_p_R    = as.numeric(region_p_R),
         county_p_R    = as.numeric(car::recode(county_p_R,"' '='17000'")),
         typ_occup1_R = ifelse(is.na(typ_occup_R)==TRUE,0, typ_occup_R)) -> conjoint_03_choice

### Sample for BART
conjoint_03_choice %>% 
  dplyr::select(idperson, group_dip, task, choice,attr_sex:attr_class,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_03_choice_01

### Sample for Causal Forest and X-Learner
conjoint_03_choice %>% 
  dplyr::select(idperson, group_dip, task, choice,attr_sex1:attr_class1,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_03_choice_02

### Sample for BART
conjoint_03_choice %>% 
  dplyr::select(idperson, group_dip, task, eval,attr_sex:attr_class,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_03_choice_03

### Sample for Causal Forest and X-Learner
conjoint_03_choice %>% 
  dplyr::select(idperson, group_dip, task, eval,attr_sex1:attr_class1,
                sex_p_R:county_p_R,pol_int_R, ideol_R, educ_R,occup_R, typ_occup1_R, inc_R) %>% 
  filter(idperson %in% vctr_cases) -> conjoint_03_choice_04


## Honest (Sample-Splitting) Procedure

### Random Split
set.seed(7303)
conjoint_id_train <- sample(unique(conjoint_01_choice_01$idperson), length(table(conjoint_01_choice_01$idperson))/2, replace=FALSE)
conjoint_id_test  <- setdiff(unique(conjoint_01_choice_01$idperson), conjoint_id_train)

### VERSION Nº 1
### Wave 1
conjoint_w01_t01_train <- conjoint_01_choice_01[is.element(conjoint_01_choice_01$idperson,conjoint_id_train), ]
conjoint_w01_t01_test  <- conjoint_01_choice_01[is.element(conjoint_01_choice_01$idperson,conjoint_id_test[-1]), ]

### Wave 3
conjoint_w03_t01_train <- conjoint_03_choice_01[is.element(conjoint_03_choice_01$idperson,conjoint_id_train), ]
conjoint_w03_t01_test  <- conjoint_03_choice_01[is.element(conjoint_03_choice_01$idperson,conjoint_id_test[-1]), ]

### VERSION Nº 2
### Wave 1
conjoint_w01_t02_train <- conjoint_01_choice_02[is.element(conjoint_01_choice_02$idperson,conjoint_id_train), ]
conjoint_w01_t02_test  <- conjoint_01_choice_02[is.element(conjoint_01_choice_02$idperson,conjoint_id_test[-1]), ]

### Wave 3
conjoint_w03_t02_train <- conjoint_03_choice_02[is.element(conjoint_03_choice_02$idperson,conjoint_id_train), ]
conjoint_w03_t02_test  <- conjoint_03_choice_02[is.element(conjoint_03_choice_02$idperson,conjoint_id_test[-1]), ]

### VERSION Nº 3
### Wave 1
conjoint_w01_t03_train <- conjoint_01_choice_03[is.element(conjoint_01_choice_03$idperson,conjoint_id_train), ]
conjoint_w01_t03_test  <- conjoint_01_choice_03[is.element(conjoint_01_choice_03$idperson,conjoint_id_test[-1]), ]

### Wave 3
conjoint_w03_t03_train <- conjoint_03_choice_03[is.element(conjoint_03_choice_03$idperson,conjoint_id_train), ]
conjoint_w03_t03_test  <- conjoint_03_choice_03[is.element(conjoint_03_choice_03$idperson,conjoint_id_test[-1]), ]

### VERSION Nº 4
### Wave 1
conjoint_w01_t04_train <- conjoint_01_choice_04[is.element(conjoint_01_choice_04$idperson,conjoint_id_train), ]
conjoint_w01_t04_test  <- conjoint_01_choice_04[is.element(conjoint_01_choice_04$idperson,conjoint_id_test[-1]), ]

### Wave 3
conjoint_w03_t04_train <- conjoint_03_choice_04[is.element(conjoint_03_choice_04$idperson,conjoint_id_train), ]
conjoint_w03_t04_test  <- conjoint_03_choice_04[is.element(conjoint_03_choice_04$idperson,conjoint_id_test[-1]), ]


## Remove objects
rm(conjoint_01_resp, conjoint_01_choice, conjoint_03_resp, conjoint_03_choice,
   conjoint_id_test, conjoint_id_train, vctr_cases)

### List of relevant covariates
covars <- c("attr_sex1","attr_age1","attr_ideol1","attr_pol_exp1","attr_pty_mem1","attr_res1","attr_rel1",
            "sex_p_R","age_p_R", "educ_p_R","occup_p_R","gse_p_R","zone_p_R","ideo_p_R","region_p_R",
            "county_p_R", "pol_int_R","ideol_R","educ_R","occup_R","typ_occup1_R","inc_R")


##########################################################################################################################################
#STEP 8: INDIVIDUAL-LEVEL MARGINAL COMPONENT EFFECT (ROBINSON AND DUCH, BAYESIAN ADDITIVE REGRESSION TREES: BART)

## BART Models
cj_model_w01 <- cjbart(data = conjoint_01_choice_01, Y = "choice",id = "idperson", round = "task", use_round = TRUE, cores = 2)
cj_model_w03 <- cjbart(data = conjoint_03_choice_01, Y = "choice",id = "idperson", round = "task", use_round = TRUE, cores = 2)

## Estimation of IMCEs

### Wave 1
het_effects_01a <- IMCE(data = conjoint_01_choice_01, model = cj_model_w01,attribs = c("attr_class"),
                        ref_levels = c("Working Class"),cores = 2)
het_effects_01b <- IMCE(data = conjoint_01_choice_01, model = cj_model_w01,attribs = c("attr_class"),
                        ref_levels = c("Middle Class"),cores = 2)
het_effects_01c <- IMCE(data = conjoint_01_choice_01, model = cj_model_w01,attribs = c("attr_class"),
                        ref_levels = c("Upper Class"),cores = 2)

### Wave 3
het_effects_03a <- IMCE(data = conjoint_03_choice_01, model = cj_model_w03,attribs = c("attr_class"),
                        ref_levels = c("Working Class"),cores = 2)
het_effects_03b <- IMCE(data = conjoint_03_choice_01, model = cj_model_w03,attribs = c("attr_class"),
                        ref_levels = c("Middle Class"),cores = 2)
het_effects_03c <- IMCE(data = conjoint_03_choice_01, model = cj_model_w03,attribs = c("attr_class"),
                        ref_levels = c("Upper Class"),cores = 2)

## Extract estimated IMCEs

### Wave 1
het_effects_01a$imce %>% group_by(idperson) %>% summarise(md_rfwk_w1 = mean(`Middle Class`),  up_rfwk_w1 = mean(`Upper Class`))  -> summ_01
het_effects_01b$imce %>% group_by(idperson) %>% summarise(wk_rfmd_w1 = mean(`Working Class`), up_rfmd_w1 = mean(`Upper Class`))  -> summ_02
het_effects_01c$imce %>% group_by(idperson) %>% summarise(wk_rfup_w1 = mean(`Working Class`), md_rfup_w1 = mean(`Middle Class`)) -> summ_03

### Wave 3
het_effects_03a$imce %>% group_by(idperson) %>% summarise(md_rfwk_w3 = mean(`Middle Class`),  up_rfwk_w3 = mean(`Upper Class`))  -> summ_04
het_effects_03b$imce %>% group_by(idperson) %>% summarise(wk_rfmd_w3 = mean(`Working Class`), up_rfmd_w3 = mean(`Upper Class`))  -> summ_05
het_effects_03c$imce %>% group_by(idperson) %>% summarise(wk_rfup_w3 = mean(`Working Class`), md_rfup_w3 = mean(`Middle Class`)) -> summ_06

## Merge results
temp_01   <- full_join(x = summ_01, y = summ_02, by = "idperson")
temp_02   <- full_join(x = temp_01, y = summ_03, by = "idperson")
temp_03   <- full_join(x = summ_04, y = summ_05, by = "idperson")
temp_04   <- full_join(x = temp_03, y = summ_06, by = "idperson")
imce_bart <- full_join(x = temp_02, y = temp_04, by = "idperson")

## Correlation tests
cor.test(imce_bart$md_rfwk_w1, imce_bart$md_rfwk_w3)
cor.test(imce_bart$up_rfwk_w1, imce_bart$up_rfwk_w3)

cor.test(imce_bart$wk_rfmd_w1, imce_bart$wk_rfmd_w3) #
cor.test(imce_bart$up_rfmd_w1, imce_bart$up_rfmd_w3)

cor.test(imce_bart$wk_rfup_w1, imce_bart$wk_rfup_w3) #
cor.test(imce_bart$md_rfup_w1, imce_bart$md_rfup_w3)


## Remove irrelevant objects
rm(cj_model_w01, cj_model_w03, het_effects_01a, het_effects_01b, het_effects_01c,
   het_effects_03a, het_effects_03b, het_effects_03c, summ_01, summ_02, summ_03,
   summ_04, summ_05, summ_06, temp_01, temp_02, temp_03, temp_04)


## Honest (Sample-Splitting) Procedure

## BART Models
cj_model_w01h <- cjbart(data = conjoint_w01_t01_train, Y = "choice",id = "idperson", round = "task", use_round = TRUE, cores = 2)
cj_model_w03h <- cjbart(data = conjoint_w03_t01_train, Y = "choice",id = "idperson", round = "task", use_round = TRUE, cores = 2)

## Estimation of IMCEs

### Wave 1
het_effects_01a <- IMCE(data = conjoint_w01_t01_test, model = cj_model_w01h,attribs = c("attr_class"),
                        ref_levels = c("Working Class"),cores = 2)
het_effects_01b <- IMCE(data = conjoint_w01_t01_test, model = cj_model_w01h,attribs = c("attr_class"),
                        ref_levels = c("Middle Class"),cores = 2)
het_effects_01c <- IMCE(data = conjoint_w01_t01_test, model = cj_model_w01h,attribs = c("attr_class"),
                        ref_levels = c("Upper Class"),cores = 2)

### Wave 3
het_effects_03a <- IMCE(data = conjoint_w03_t01_test, model = cj_model_w03h,attribs = c("attr_class"),
                        ref_levels = c("Working Class"),cores = 2)
het_effects_03b <- IMCE(data = conjoint_w03_t01_test, model = cj_model_w03h,attribs = c("attr_class"),
                        ref_levels = c("Middle Class"),cores = 2)
het_effects_03c <- IMCE(data = conjoint_w03_t01_test, model = cj_model_w03h,attribs = c("attr_class"),
                        ref_levels = c("Upper Class"),cores = 2)


## Extract estimated IMCEs

### Wave 1
het_effects_01a$imce %>% group_by(idperson) %>% summarise(md_rfwk_w1 = mean(`Middle Class`),  up_rfwk_w1 = mean(`Upper Class`))  -> summ_01
het_effects_01b$imce %>% group_by(idperson) %>% summarise(wk_rfmd_w1 = mean(`Working Class`), up_rfmd_w1 = mean(`Upper Class`))  -> summ_02
het_effects_01c$imce %>% group_by(idperson) %>% summarise(wk_rfup_w1 = mean(`Working Class`), md_rfup_w1 = mean(`Middle Class`)) -> summ_03

### Wave 3
het_effects_03a$imce %>% group_by(idperson) %>% summarise(md_rfwk_w3 = mean(`Middle Class`),  up_rfwk_w3 = mean(`Upper Class`))  -> summ_04
het_effects_03b$imce %>% group_by(idperson) %>% summarise(wk_rfmd_w3 = mean(`Working Class`), up_rfmd_w3 = mean(`Upper Class`))  -> summ_05
het_effects_03c$imce %>% group_by(idperson) %>% summarise(wk_rfup_w3 = mean(`Working Class`), md_rfup_w3 = mean(`Middle Class`)) -> summ_06


## Merge results
temp_01          <- full_join(x = summ_01, y = summ_02, by = "idperson")
temp_02          <- full_join(x = temp_01, y = summ_03, by = "idperson")
temp_03          <- full_join(x = summ_04, y = summ_05, by = "idperson")
temp_04          <- full_join(x = temp_03, y = summ_06, by = "idperson")
imce_bart_honest <- full_join(x = temp_02, y = temp_04, by = "idperson")


## Correlation tests
cor.test(imce_bart_honest$md_rfwk_w1, imce_bart_honest$md_rfwk_w3)
cor.test(imce_bart_honest$up_rfwk_w1, imce_bart_honest$up_rfwk_w3)

cor.test(imce_bart_honest$wk_rfmd_w1, imce_bart_honest$wk_rfmd_w3) #
cor.test(imce_bart_honest$up_rfmd_w1, imce_bart_honest$up_rfmd_w3)

cor.test(imce_bart_honest$wk_rfup_w1, imce_bart_honest$wk_rfup_w3) #
cor.test(imce_bart_honest$md_rfup_w1, imce_bart_honest$md_rfup_w3)


## Remove irrelevant objects
rm(cj_model_w01h, cj_model_w03h, het_effects_01a, het_effects_01b, het_effects_01c,
   het_effects_03a, het_effects_03b, het_effects_03c, summ_01, summ_02, summ_03,
   summ_04, summ_05, summ_06, temp_01, temp_02, temp_03, temp_04)

##########################################################################################################################################
#STEP 9: INDIVIDUAL-LEVEL MARGINAL COMPONENT EFFECT (ABRAMSON, KOCAK, MAGAZINNIK AND STREZHNEV, CAUSAL FOREST)

## Wave 1, Full Sample

### Treatment model
tr_model_01 <- regression_forest(X = conjoint_01_choice_02[,covars], 
                                 Y = conjoint_01_choice_02$attr_class1, num.trees = 2000, 
                                 clusters = conjoint_01_choice_02$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_01    <- predict (tr_model_01)$predictions

### Outcome model
out_model_01 <- regression_forest(X = conjoint_01_choice_02[,covars],  
                                  Y = conjoint_01_choice_02$choice, num.trees = 2000, 
                                  clusters = conjoint_01_choice_02$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_01    <- predict (out_model_01)$predictions

### Estimate raw causal forest
cf_raw_01 <- causal_forest(X = conjoint_01_choice_02[,covars],  
                           Y = conjoint_01_choice_02$choice, 
                           W = conjoint_01_choice_02$attr_class, 
                           clusters = conjoint_01_choice_02$idperson,
                           Y.hat = Y_hat_01, W.hat = W_hat_01, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_01, file = "0C_Objects/Raw_Causal_Forest_01.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_01      <- variable_importance(cf_raw_01)
selected_vars_01 <- which(imp_vars_01 > mean(imp_vars_01))

### Estimate causal forest
cf_new_01 <- causal_forest(X = conjoint_01_choice_02[,covars] %>% dplyr::select(covars[selected_vars_01]), 
                           Y = conjoint_01_choice_02$choice, 
                           W = conjoint_01_choice_02$attr_class, 
                           clusters = conjoint_01_choice_02$idperson,
                           Y.hat = Y_hat_01, W.hat = W_hat_01, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_01,   file = "0C_Objects/Pred_Causal_Forest_01.rds")


## Wave 3, Full Sample

### Treatment model
tr_model_02 <- regression_forest(X = conjoint_03_choice_02[,covars], 
                                 Y = conjoint_03_choice_02$attr_class, num.trees = 2000, 
                                 clusters = conjoint_03_choice_02$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_02    <- predict (tr_model_02)$predictions

### Outcome model
out_model_02 <- regression_forest(X = conjoint_03_choice_02[,covars],  
                                  Y = conjoint_03_choice_02$choice, num.trees = 2000, 
                                  clusters = conjoint_03_choice_02$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_02    <- predict (out_model_02)$predictions

### Estimate raw causal forest
cf_raw_02 <- causal_forest(X = conjoint_03_choice_02[,covars],  
                           Y = conjoint_03_choice_02$choice, 
                           W = conjoint_03_choice_02$attr_class, 
                           clusters = conjoint_03_choice_02$idperson,
                           Y.hat = Y_hat_02, W.hat = W_hat_02, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_02, file = "0C_Objects/Raw_Causal_Forest_02.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_02      <- variable_importance(cf_raw_02)
selected_vars_02 <- which(imp_vars_02 > mean(imp_vars_02))

### Estimate causal forest
cf_new_02 <- causal_forest(X = conjoint_03_choice_02[,covars] %>% dplyr::select(covars[selected_vars_02]), 
                           Y = conjoint_03_choice_02$choice, 
                           W = conjoint_03_choice_02$attr_class, 
                           clusters = conjoint_03_choice_02$idperson,
                           Y.hat = Y_hat_02, W.hat = W_hat_02, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_02,   file = "0C_Objects/Pred_Causal_Forest_02.rds")


## Honest (Sample-Splitting) Procedure
## Wave 1, Filtered Sample

### Treatment model
tr_model_03 <- regression_forest(X = conjoint_w01_t02_train[,covars], 
                                 Y = conjoint_w01_t02_train$attr_class, num.trees = 2000, 
                                 clusters = conjoint_w01_t02_train$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_03    <- predict (tr_model_03)$predictions

### Outcome model
out_model_03 <- regression_forest(X = conjoint_w01_t02_train[,covars],  
                                  Y = conjoint_w01_t02_train$choice, num.trees = 2000, 
                                  clusters = conjoint_w01_t02_train$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_03    <- predict (out_model_03)$predictions

### Estimate raw causal forest
cf_raw_03 <- causal_forest(X = conjoint_w01_t02_train[,covars],  
                           Y = conjoint_w01_t02_train$choice, 
                           W = conjoint_w01_t02_train$attr_class, 
                           clusters = conjoint_w01_t02_train$idperson,
                           Y.hat = Y_hat_03, W.hat = W_hat_03, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_03, file = "0C_Objects/Raw_Causal_Forest_03.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_03      <- variable_importance(cf_raw_03)
selected_vars_03 <- which(imp_vars_03 > mean(imp_vars_03))

### Estimate causal forest
cf_new_03 <- causal_forest(X = conjoint_w01_t02_test[,covars] %>% dplyr::select(covars[selected_vars_03]), 
                           Y = conjoint_w01_t02_test$choice, 
                           W = conjoint_w01_t02_test$attr_class, 
                           clusters = conjoint_w01_t02_test$idperson,
                           Y.hat = Y_hat_03, W.hat = W_hat_03, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_03,   file = "0C_Objects/Pred_Causal_Forest_03.rds")


## Wave 3, Filtered Sample

### Treatment model
tr_model_04 <- regression_forest(X = conjoint_w03_t02_train[,covars], 
                                 Y = conjoint_w03_t02_train$attr_class, num.trees = 2000, 
                                 clusters = conjoint_w03_t02_train$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_04    <- predict (tr_model_04)$predictions

### Outcome model
out_model_04 <- regression_forest(X = conjoint_w03_t02_train[,covars],  
                                  Y = conjoint_w03_t02_train$choice, num.trees = 2000, 
                                  clusters = conjoint_w03_t02_train$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_04    <- predict (out_model_04)$predictions

### Estimate raw causal forest
cf_raw_04 <- causal_forest(X = conjoint_w03_t02_train[,covars],  
                           Y = conjoint_w03_t02_train$choice, 
                           W = conjoint_w03_t02_train$attr_class, 
                           clusters = conjoint_w03_t02_train$idperson,
                           Y.hat = Y_hat_04, W.hat = W_hat_04, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_04, file = "0C_Objects/Raw_Causal_Forest_04.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_04      <- variable_importance(cf_raw_04)
selected_vars_04 <- which(imp_vars_04 > mean(imp_vars_04))

### Estimate causal forest
cf_new_04 <- causal_forest(X = conjoint_w03_t02_test[,covars] %>% dplyr::select(covars[selected_vars_04]), 
                           Y = conjoint_w03_t02_test$choice, 
                           W = conjoint_w03_t02_test$attr_class, 
                           clusters = conjoint_w03_t02_test$idperson,
                           Y.hat = Y_hat_04, W.hat = W_hat_04, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_04,   file = "0C_Objects/Pred_Causal_Forest_04.rds")


## Transform to individual level

### Wave 1
data.frame(id = conjoint_01_choice_02$idperson, omce = cf_new_01$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_01

### Wave 3
data.frame(id = conjoint_03_choice_02$idperson, omce = cf_new_02$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_02

### Wave 1, Honest Procedure
data.frame(id = conjoint_w01_t02_test$idperson, omce = cf_new_03$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_03

### Wave 3, Honest Procedure
data.frame(id = conjoint_w03_t02_test$idperson, omce = cf_new_04$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_04

## Merge results
imce_cf        <- full_join(x = summ_01, y = summ_02, by = "id")
imce_cf_honest <- full_join(x = summ_03, y = summ_04, by = "id")

## Correlation tests
cor.test(imce_cf$imce.x, imce_cf$imce.y)
cor.test(imce_cf_honest$imce.x, imce_cf_honest$imce.y)

## Remove objects
rm(cf_new_01, cf_new_02, cf_new_03, cf_new_04,
   cf_raw_01, cf_raw_02, cf_raw_03, cf_raw_04,
   imp_vars_01, imp_vars_02, imp_vars_03, imp_vars_04,
   selected_vars_01, selected_vars_02, selected_vars_03, selected_vars_04,
   out_model_01, out_model_02, out_model_03, out_model_04,
   tr_model_01, tr_model_02, tr_model_03, tr_model_04,
   summ_01, summ_02, summ_03, summ_04,
   W_hat_01, W_hat_02, W_hat_03, W_hat_04, 
   Y_hat_01, Y_hat_02, Y_hat_03, Y_hat_04)


##########################################################################################################################################
#STEP 10: META-LEARNER

## Wave 1, Random Forest (Filtered Sample)
xl_01 <- X_RF(feat = conjoint_01_choice_02[,c("idperson",covars)], 
              tr   = conjoint_01_choice_02$attr_class1,
              yobs = conjoint_01_choice_02$choice, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_01, file = "0C_Objects/X_Learner_RF_resp_Sample_W01.rds")

## Wave 3, Random Forest (Filtered Sample)
xl_02 <- X_RF(feat = conjoint_03_choice_02[,c("idperson",covars)], 
              tr   = conjoint_03_choice_02$attr_class1,
              yobs = conjoint_03_choice_02$choice, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_02, file = "0C_Objects/X_Learner_RF_resp_Sample_W03.rds")

## Wave 1, Random Forest (Test Sample)
xl_03 <- X_RF(feat = conjoint_w01_t02_train[,c("idperson",covars)], 
              tr   = conjoint_w01_t02_train$attr_class1,
              yobs = conjoint_w01_t02_train$choice, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_03, file = "0C_Objects/X_Learner_RF_Honest_Sample_W01.rds")

## Wave 3, Random Forest (Test Sample)
xl_04 <- X_RF(feat = conjoint_w03_t02_train[,c("idperson",covars)], 
              tr   = conjoint_w03_t02_train$attr_class1,
              yobs = conjoint_w03_t02_train$choice, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_04, file = "0C_Objects/X_Learner_RF_Honest_Sample_W03.rds")

### Predicted values
pred_xl_01 <- EstimateCate(xl_01, conjoint_01_choice_02[,c("idperson",covars)])
pred_xl_02 <- EstimateCate(xl_02, conjoint_03_choice_02[,c("idperson",covars)])
pred_xl_03 <- EstimateCate(xl_03, conjoint_w01_t02_test[,c("idperson",covars)])
pred_xl_04 <- EstimateCate(xl_04, conjoint_w03_t02_test[,c("idperson",covars)])

## Transform to individual level

### Wave 1
data.frame(id = conjoint_01_choice_01$idperson, omce = pred_xl_01) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_01

### Wave 3
data.frame(id = conjoint_03_choice_01$idperson, omce = pred_xl_02) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_02

### Wave 1, Honest Procedure
data.frame(id = conjoint_w01_t02_test$idperson, omce = pred_xl_03) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_03

### Wave 3, Honest Procedure
data.frame(id = conjoint_w03_t02_test$idperson, omce = pred_xl_04) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_04


### Merge information
imce_xl_cf        <- full_join(x = summ_01, y = summ_02, by = "id")
imce_xl_cf_honest <- full_join(x = summ_03, y = summ_04, by = "id")

### Correlation
cor.test(imce_xl_cf$imce.x, imce_xl_cf$imce.y)
cor.test(imce_xl_cf_honest$imce.x, imce_xl_cf_honest$imce.y)

### Remove objects
rm(summ_01, summ_02, summ_03, summ_04, covars,
   xl_01, xl_02, xl_03, xl_04,
   pred_xl_01, pred_xl_02, pred_xl_03, pred_xl_04)


##########################################################################################################################################
#STEP 11: REPEAT FOR EVALUATION OF COMPETENCE 

#INDIVIDUAL-LEVEL MARGINAL COMPONENT EFFECT (ABRAMSON, KOCAK, MAGAZINNIK AND STREZHNEV, CAUSAL FOREST)

## Wave 1, Full Sample

### Treatment model
tr_model_01 <- regression_forest(X = conjoint_01_choice_04[,covars], 
                                 Y = conjoint_01_choice_04$attr_class1, num.trees = 2000, 
                                 clusters = conjoint_01_choice_04$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_01    <- predict (tr_model_01)$predictions

### Outcome model
out_model_01 <- regression_forest(X = conjoint_01_choice_04[,covars],  
                                  Y = conjoint_01_choice_04$eval, num.trees = 2000, 
                                  clusters = conjoint_01_choice_04$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_01    <- predict (out_model_01)$predictions

### Estimate raw causal forest
cf_raw_01 <- causal_forest(X = conjoint_01_choice_04[,covars],  
                           Y = conjoint_01_choice_04$eval, 
                           W = conjoint_01_choice_04$attr_class, 
                           clusters = conjoint_01_choice_04$idperson,
                           Y.hat = Y_hat_01, W.hat = W_hat_01, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_01, file = "0C_Objects/Raw_Causal_Forest_05.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_01      <- variable_importance(cf_raw_01)
selected_vars_01 <- which(imp_vars_01 > mean(imp_vars_01))

### Estimate causal forest
cf_new_01 <- causal_forest(X = conjoint_01_choice_04[,covars] %>% dplyr::select(covars[selected_vars_01]), 
                           Y = conjoint_01_choice_04$eval, 
                           W = conjoint_01_choice_04$attr_class, 
                           clusters = conjoint_01_choice_04$idperson,
                           Y.hat = Y_hat_01, W.hat = W_hat_01, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_01,   file = "0C_Objects/Pred_Causal_Forest_05.rds")


## Wave 3, Full Sample

### Treatment model
tr_model_02 <- regression_forest(X = conjoint_03_choice_04[,covars], 
                                 Y = conjoint_03_choice_04$attr_class, num.trees = 2000, 
                                 clusters = conjoint_03_choice_04$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_02    <- predict (tr_model_02)$predictions

### Outcome model
out_model_02 <- regression_forest(X = conjoint_03_choice_04[,covars],  
                                  Y = conjoint_03_choice_04$eval, num.trees = 2000, 
                                  clusters = conjoint_03_choice_04$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_02    <- predict (out_model_02)$predictions

### Estimate raw causal forest
cf_raw_02 <- causal_forest(X = conjoint_03_choice_04[,covars],  
                           Y = conjoint_03_choice_04$eval, 
                           W = conjoint_03_choice_04$attr_class, 
                           clusters = conjoint_03_choice_04$idperson,
                           Y.hat = Y_hat_02, W.hat = W_hat_02, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_02, file = "0C_Objects/Raw_Causal_Forest_06.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_02      <- variable_importance(cf_raw_02)
selected_vars_02 <- which(imp_vars_02 > mean(imp_vars_02))

### Estimate causal forest
cf_new_02 <- causal_forest(X = conjoint_03_choice_04[,covars] %>% dplyr::select(covars[selected_vars_02]), 
                           Y = conjoint_03_choice_04$eval, 
                           W = conjoint_03_choice_04$attr_class, 
                           clusters = conjoint_03_choice_04$idperson,
                           Y.hat = Y_hat_02, W.hat = W_hat_02, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_02,   file = "0C_Objects/Pred_Causal_Forest_06.rds")


## Honest (Sample-Splitting) Procedure
## Wave 1, Filtered Sample

### Treatment model
tr_model_03 <- regression_forest(X = conjoint_w01_t04_train[,covars], 
                                 Y = conjoint_w01_t04_train$attr_class, num.trees = 2000, 
                                 clusters = conjoint_w01_t04_train$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_03    <- predict (tr_model_03)$predictions

### Outcome model
out_model_03 <- regression_forest(X = conjoint_w01_t04_train[,covars],  
                                  Y = conjoint_w01_t04_train$eval, num.trees = 2000, 
                                  clusters = conjoint_w01_t04_train$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_03    <- predict (out_model_03)$predictions

### Estimate raw causal forest
cf_raw_03 <- causal_forest(X = conjoint_w01_t04_train[,covars],  
                           Y = conjoint_w01_t04_train$eval, 
                           W = conjoint_w01_t04_train$attr_class, 
                           clusters = conjoint_w01_t04_train$idperson,
                           Y.hat = Y_hat_03, W.hat = W_hat_03, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_03, file = "0C_Objects/Raw_Causal_Forest_07.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_03      <- variable_importance(cf_raw_03)
selected_vars_03 <- which(imp_vars_03 > mean(imp_vars_03))

### Estimate causal forest
cf_new_03 <- causal_forest(X = conjoint_w01_t04_test[,covars] %>% dplyr::select(covars[selected_vars_03]), 
                           Y = conjoint_w01_t04_test$eval, 
                           W = conjoint_w01_t04_test$attr_class, 
                           clusters = conjoint_w01_t04_test$idperson,
                           Y.hat = Y_hat_03, W.hat = W_hat_03, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_03,   file = "0C_Objects/Pred_Causal_Forest_07.rds")


## Wave 3, Filtered Sample

### Treatment model
tr_model_04 <- regression_forest(X = conjoint_w03_t04_train[,covars], 
                                 Y = conjoint_w03_t04_train$attr_class, num.trees = 2000, 
                                 clusters = conjoint_w03_t04_train$idperson,
                                 honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                 num.threads = 3, seed = 7303)
W_hat_04    <- predict (tr_model_04)$predictions

### Outcome model
out_model_04 <- regression_forest(X = conjoint_w03_t04_train[,covars],  
                                  Y = conjoint_w03_t04_train$eval, num.trees = 2000, 
                                  clusters = conjoint_w03_t04_train$idperson,
                                  honesty = TRUE, honesty.fraction = 0.5, tune.parameters = "all",
                                  num.threads = 3, seed = 7303)
Y_hat_04    <- predict (out_model_04)$predictions

### Estimate raw causal forest
cf_raw_04 <- causal_forest(X = conjoint_w03_t04_train[,covars],  
                           Y = conjoint_w03_t04_train$eval, 
                           W = conjoint_w03_t04_train$attr_class, 
                           clusters = conjoint_w03_t04_train$idperson,
                           Y.hat = Y_hat_04, W.hat = W_hat_04, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_raw_04, file = "0C_Objects/Raw_Causal_Forest_08.rds")

### Choose variables whose importance is larger than one fifth of the mean
imp_vars_04      <- variable_importance(cf_raw_04)
selected_vars_04 <- which(imp_vars_04 > mean(imp_vars_04))

### Estimate causal forest
cf_new_04 <- causal_forest(X = conjoint_w03_t04_test[,covars] %>% dplyr::select(covars[selected_vars_04]), 
                           Y = conjoint_w03_t04_test$eval, 
                           W = conjoint_w03_t04_test$attr_class, 
                           clusters = conjoint_w03_t04_test$idperson,
                           Y.hat = Y_hat_04, W.hat = W_hat_04, num.trees = 2000,
                           tune.parameters = "all", honesty = TRUE, honesty.fraction = 0.5, 
                           num.threads = 3, seed = 7303)

### Store the results
write_rds(x = cf_new_04,   file = "0C_Objects/Pred_Causal_Forest_08.rds")


## Transform to individual level

### Wave 1
data.frame(id = conjoint_01_choice_04$idperson, omce = cf_new_01$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_01

### Wave 3
data.frame(id = conjoint_03_choice_04$idperson, omce = cf_new_02$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_02

### Wave 1, Honest Procedure
data.frame(id = conjoint_w01_t04_test$idperson, omce = cf_new_03$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_03

### Wave 3, Honest Procedure
data.frame(id = conjoint_w03_t04_test$idperson, omce = cf_new_04$predictions) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_04

## Merge results
imce_eval_cf        <- full_join(x = summ_01, y = summ_02, by = "id")
imce_eval_cf_honest <- full_join(x = summ_03, y = summ_04, by = "id")

## Correlation tests
cor.test(imce_eval_cf$imce.x, imce_eval_cf$imce.y)
cor.test(imce_eval_cf_honest$imce.x, imce_eval_cf_honest$imce.y)

## Remove objects
rm(cf_new_01, cf_new_02, cf_new_03, cf_new_04,
   cf_raw_01, cf_raw_02, cf_raw_03, cf_raw_04,
   imp_vars_01, imp_vars_02, imp_vars_03, imp_vars_04,
   selected_vars_01, selected_vars_02, selected_vars_03, selected_vars_04,
   out_model_01, out_model_02, out_model_03, out_model_04,
   tr_model_01, tr_model_02, tr_model_03, tr_model_04,
   summ_01, summ_02, summ_03, summ_04,
   W_hat_01, W_hat_02, W_hat_03, W_hat_04, 
   Y_hat_01, Y_hat_02, Y_hat_03, Y_hat_04)


##########################################################################################################################################
#STEP 10: META-LEARNER

## Wave 1, Random Forest (Filtered Sample)
xl_01 <- X_RF(feat = conjoint_01_choice_04[,c("idperson",covars)], 
              tr   = conjoint_01_choice_04$attr_class1,
              yobs = conjoint_01_choice_04$eval, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_01, file = "0C_Objects/X_Learner_RF_eval_Sample_W01.rds")

## Wave 3, Random Forest (Filtered Sample)
xl_02 <- X_RF(feat = conjoint_03_choice_04[,c("idperson",covars)], 
              tr   = conjoint_03_choice_04$attr_class1,
              yobs = conjoint_03_choice_04$eval, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_02, file = "0C_Objects/X_Learner_RF_eval_Sample_W03.rds")

## Wave 1, Random Forest (Test Sample)
xl_03 <- X_RF(feat = conjoint_w01_t04_train[,c("idperson",covars)], 
              tr   = conjoint_w01_t04_train$attr_class1,
              yobs = conjoint_w01_t04_train$eval, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_03, file = "0C_Objects/X_Learner_RF_eval_Honest_Sample_W01.rds")

## Wave 3, Random Forest (Test Sample)
xl_04 <- X_RF(feat = conjoint_w03_t04_train[,c("idperson",covars)], 
              tr   = conjoint_w03_t04_train$attr_class1,
              yobs = conjoint_w03_t04_train$eval, 
              nthread = 3,
              verbose = TRUE)
write_rds(x = xl_04, file = "0C_Objects/X_Learner_RF_eval_Honest_Sample_W03.rds")

### Predicted values
pred_xl_01 <- EstimateCate(xl_01, conjoint_01_choice_04[,c("idperson",covars)])
pred_xl_02 <- EstimateCate(xl_02, conjoint_03_choice_04[,c("idperson",covars)])
pred_xl_03 <- EstimateCate(xl_03, conjoint_w01_t04_test[,c("idperson",covars)])
pred_xl_04 <- EstimateCate(xl_04, conjoint_w03_t04_test[,c("idperson",covars)])

## Transform to individual level

### Wave 1
data.frame(id = conjoint_01_choice_03$idperson, omce = pred_xl_01) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_01

### Wave 3
data.frame(id = conjoint_03_choice_03$idperson, omce = pred_xl_02) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_02

### Wave 1, Honest Procedure
data.frame(id = conjoint_w01_t04_test$idperson, omce = pred_xl_03) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_03

### Wave 3, Honest Procedure
data.frame(id = conjoint_w03_t04_test$idperson, omce = pred_xl_04) %>%
  group_by(id) %>% summarise(imce = mean(omce)) -> summ_04


### Merge information
imce_eval_xl_cf        <- full_join(x = summ_01, y = summ_02, by = "id")
imce_eval_xl_cf_honest <- full_join(x = summ_03, y = summ_04, by = "id")

### Correlation
cor.test(imce_eval_xl_cf$imce.x, imce_eval_xl_cf$imce.y)
cor.test(imce_eval_xl_cf_honest$imce.x, imce_eval_xl_cf_honest$imce.y)

### Remove objects
rm(summ_01, summ_02, summ_03, summ_04, covars,
   xl_01, xl_02, xl_03, xl_04,
   pred_xl_01, pred_xl_02, pred_xl_03, pred_xl_04)


##########################################################################################################################################
#STEP 11: STORE RESULTS

## Create objects
write_rds(x = imce,                   file = "0C_Objects/IMCE_OLS.rds")
write_rds(x = imce_bart,              file = "0C_Objects/IMCE_Bart.rds")
write_rds(x = imce_bart_honest,       file = "0C_Objects/IMCE_Bart_Honest.rds")
write_rds(x = imce_cf,                file = "0C_Objects/IMCE_CF.rds")
write_rds(x = imce_cf_honest,         file = "0C_Objects/IMCE_CF_Honest.rds")
write_rds(x = imce_xl_cf,             file = "0C_Objects/IMCE_XL_CF.rds")
write_rds(x = imce_xl_cf_honest,      file = "0C_Objects/IMCE_XL_CF_Honest.rds")
write_rds(x = imce_eval_cf,           file = "0C_Objects/IMCE_eval_CF.rds")
write_rds(x = imce_eval_cf_honest,    file = "0C_Objects/IMCE_eval_CF_Honest.rds")
write_rds(x = imce_eval_xl_cf,        file = "0C_Objects/IMCE_eval_XL_CF.rds")
write_rds(x = imce_eval_xl_cf_honest, file = "0C_Objects/IMCE_eval_XL_CF_Honest.rds")

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
#Random number generation:
#  RNG:     L'Ecuyer-CMRG 
# Normal:  Inversion 
# Sample:  Rejection 
# 
#locale:
#[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
# [1] ranger_0.13.1         grf_2.1.0             cjbart_0.2.2          BART_2.9              survival_3.2-13      
# [6] nnet_7.3-17           nlme_3.1-155          causalToolbox_0.0.2.1 xtable_1.8-4          forcats_0.5.1        
#[11] stringr_1.4.0         dplyr_1.0.8           purrr_0.3.4           readr_2.1.1           tidyr_1.2.0          
#[16] tibble_3.1.6          tidyverse_1.3.1       sjmisc_2.8.9          MetBrewer_0.2.0       ggthemes_4.2.4       
#[21] ggpubr_0.4.0          ggplot2_3.3.5        
#
#loaded via a namespace (and not attached):
# [1] fs_1.5.2           lubridate_1.8.0    insight_0.17.0     httr_1.4.2         Rforestry_0.9.0.97 tools_4.1.2       
# [7] backports_1.4.1    utf8_1.2.2         R6_2.5.1           sjlabelled_1.1.8   DBI_1.1.2          colorspace_2.0-2  
#[13] withr_2.4.3        tidyselect_1.1.2   gridExtra_2.3      compiler_4.1.2     DiceKriging_1.6.0  glmnet_4.1-3      
#[19] cli_3.2.0          rvest_1.0.2        xml2_1.3.3         labeling_0.4.2     scales_1.1.1       digest_0.6.29     
#[25] pkgconfig_2.0.3    htmltools_0.5.2    dbplyr_2.1.1       fastmap_1.1.0      htmlwidgets_1.5.4  rlang_1.0.2       
#[31] readxl_1.3.1       rstudioapi_0.13    shape_1.4.6        visNetwork_2.1.0   farver_2.1.0       generics_0.1.2    
#[37] jsonlite_1.8.0     car_3.0-12         magrittr_2.0.2     Matrix_1.4-0       Rcpp_1.0.8.3       munsell_0.5.0     
#[43] fansi_1.0.3        abind_1.4-5        lifecycle_1.0.1    stringi_1.7.6      carData_3.0-5      MASS_7.3-55       
#[49] grid_4.1.2         parallel_4.1.2     crayon_1.5.1       lattice_0.20-45    haven_2.4.3        cowplot_1.1.1     
#[55] splines_4.1.2      hms_1.1.1          pillar_1.7.0       ggsignif_0.6.3     codetools_0.2-18   reprex_2.0.1      
#[61] glue_1.6.2         modelr_0.1.8       vctrs_0.3.8        tzdb_0.2.0         Rdpack_2.1.3       foreach_1.5.2     
#[67] cellranger_1.1.0   gtable_0.3.0       assertthat_0.2.1   rbibutils_2.2.7    broom_0.7.12       rstatix_0.7.0     
#[73] iterators_1.0.14   ellipsis_0.3.2    