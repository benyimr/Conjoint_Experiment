##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº8:  Experimental Results (Advanced: Causal Interaction with Machine Learning)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(FindIt)
library(glinternet)
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
conjoint_01$attr_pol_exp  <- factor(conjoint_01$pol_exp_c, levels = c("None", "Activist","Elected Official"))
conjoint_01$attr_pty_mem  <- factor(conjoint_01$party_c,   levels = c("Member of Party","Independent", "Independent supported by Party"))
conjoint_01$attr_res      <- factor(conjoint_01$resid_c,   levels = c("Same County", "Live in another County"))
conjoint_01$attr_rel      <- factor(conjoint_01$relig_c,   levels = c("Catholic", "Evangelical", "Irreligious"))
conjoint_01$attr_class    <- factor(conjoint_01$class_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_01$attr_occup    <- factor(conjoint_01$occup_c,   levels = c("Construction worker","Salesperson","Accountant","Teacher",
                                                                      "Civil Engineer in Mining","Physician"))

## Coercing to factors (Wave 3)
conjoint_03$attr_sex      <- factor(conjoint_03$sex_c,     levels = c("Male","Female"))
conjoint_03$attr_age      <- factor(conjoint_03$age_c,     levels = c("50 years","25 years","70 years"))
conjoint_03$attr_ideol    <- factor(conjoint_03$ideol_c,   levels = c("Center","Right","Center-Right","Center-Left","Left"))
conjoint_03$attr_pol_exp  <- factor(conjoint_03$pol_exp_c, levels = c("None", "Activist","Deputy"))
conjoint_03$attr_pty_mem  <- factor(conjoint_03$party_c,   levels = c("Member of Party","Independent", "Independent supported by Party"))
conjoint_03$attr_res      <- factor(conjoint_03$resid_c,   levels = c("Same County", "Live in another County"))
conjoint_03$attr_rel      <- factor(conjoint_03$relig_c,   levels = c("Catholic", "Evangelical", "Irreligious"))
conjoint_03$attr_class    <- factor(conjoint_03$class_c,   levels = c("Middle Class","Working Class","Upper Class"))
conjoint_03$attr_occup    <- factor(conjoint_03$occup_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                      "Accountant","Teacher","Nurse Technician",
                                                                      "Lawyer","Civil Engineer in Mining","Physician"))
conjoint_03$classf1_c     <- car::recode(conjoint_03$classf_c,"'Check'=NA")
conjoint_03$occupf1_c     <- car::recode(conjoint_03$occupf_c,"'Check'=NA")
conjoint_03$attr_class_f  <- factor(conjoint_03$classf1_c,   levels = c("Working Class","Middle Class","Upper Class"))
conjoint_03$attr_occup_f  <- factor(conjoint_03$occupf1_c,   levels = c("Construction worker","Salesperson","Janitor",
                                                                        "Accountant","Teacher","Nurse Technician",
                                                                        "Lawyer","Civil Engineer in Mining","Physician"))


## Coercing Respondent Characteristics to Numeric (Wave 1)
conjoint_01 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"c(2,3)=2;c(4)=3;c(5)=4")),
         zone_num1  = as.numeric(zone_num),
         ideol_num1 = as.numeric(ideol_num)) -> conjoint_01

## Coercing Respondent Characteristics to Numeric (Wave 1)
conjoint_03 %>%
  mutate(sex_num1   = as.numeric(sex_num),
         age_num1   = as.numeric(age),
         educ_num1  = as.numeric(car::recode(educ_supp_num,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")),
         occup_num1 = as.numeric(car::recode(occup_supp_num,"c(1,2)=2;c(3)=3;c(4)=4;c(5,6)=5;c(0,7,8,9,10)=1")),
         gse_num1   = as.numeric(car::recode(seg_supp_num,"c(2,3)=2;c(4)=3;c(5)=4")),
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
         household_R    = household,       civ_st_R       = civ_st_resp,     civ_st_oth_R   = civ_st_oth_resp)  -> conjoint_03_resp

#%>% 
#filter(group_dip == 1)

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
#STEP 4: CAUSAL INTERACTION ANALYSIS (SPECIFIED INTERACTIONS)

## Empty object to store results
causal_anova_00 <- list()

## Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class

### N-Way = 1
causal_anova_00$vote_n1_w1o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_01_resp,
                                           nway = 1,
                                           pair.id = conjoint_01_resp$task_alt, diff = TRUE,
                                           screen = FALSE, screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$vote_n2_w1o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_01_resp,
                                           nway = 2,
                                           pair.id = conjoint_01_resp$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$vote_n3_w1o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_01_resp,
                                           nway = 3,
                                           pair.id = conjoint_01_resp$task_alt, diff = TRUE,
                                           screen = FALSE, screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class

### N-Way = 1
causal_anova_00$vote_n1_w1f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_01_filtered,
                                           nway = 1,
                                           pair.id = conjoint_01_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$vote_n2_w1f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_01_filtered,
                                           nway = 2,
                                           pair.id = conjoint_01_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$vote_n3_w1f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_01_filtered,
                                           nway = 3,
                                           pair.id = conjoint_01_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 1, Sample: Full, DV: Evaluation of Competence, IV: Social Class

### N-Way = 1
causal_anova_00$eval_n1_w1o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_01_resp,
                                           nway = 1,
                                           pair.id = conjoint_01_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$eval_n2_w1o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_01_resp,
                                           nway = 2,
                                           pair.id = conjoint_01_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$eval_n3_w1o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_01_resp,
                                           nway = 3,
                                           pair.id = conjoint_01_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 1, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class

### N-Way = 1
causal_anova_00$eval_n1_w1f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_01_filtered,
                                           nway = 1,
                                           pair.id = conjoint_01_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$eval_n2_w1f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_01_filtered,
                                           nway = 2,
                                           pair.id = conjoint_01_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$eval_n3_w1f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_01_filtered,
                                           nway = 3,
                                           pair.id = conjoint_01_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_01_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class

### N-Way = 1
causal_anova_00$vote_n1_w3o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 1,
                                           pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$vote_n2_w3o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 2,
                                           pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$vote_n3_w3o <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_resp,
                                           nway = 3,
                                           pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class

### N-Way = 1
causal_anova_00$vote_n1_w3f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 1,
                                           pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$vote_n2_w3f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 2,
                                           pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$vote_n3_w3f <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_filtered,
                                           nway = 3,
                                           pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "binomial",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Full, DV: Evaluation of Competence, IV: Social Class

### N-Way = 1
causal_anova_00$eval_n1_w3o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 1,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$eval_n2_w3o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 2,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$eval_n3_w3o <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_resp,
                                           nway = 3,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class

### N-Way = 1
causal_anova_00$eval_n1_w3f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 1,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$eval_n2_w3f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 2,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$eval_n3_w3f <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_filtered,
                                           nway = 3,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Full, DV: Perception of Representation, IV: Social Class

### N-Way = 1
causal_anova_00$repr_n1_w3o <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 1,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$repr_n2_w3o <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_resp,
                                           nway = 2,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$repr_n3_w3o <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_resp,
                                           nway = 3,
                                           pair.id = conjoint_03_resp$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_resp$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


## Wave: 3, Sample: Filtered, DV: Perception of Representation, IV: Social Class

### N-Way = 1
causal_anova_00$repr_n1_w3f <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = NULL, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 1,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 2 (Specified Interactions)
causal_anova_00$repr_n2_w3f <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_pol_exp:attr_class, int3.formula = NULL,
                                           data = conjoint_03_filtered,
                                           nway = 2,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)

### N-Way = 3 (Specified Interactions)
causal_anova_00$repr_n3_w3f <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                                             attr_pty_mem + attr_res + attr_rel + attr_class,
                                           int2.formula = ~ attr_ideol:attr_class + attr_pol_exp:attr_class +
                                             attr_ideol:attr_pol_exp, int3.formula = ~ attr_ideol:attr_pol_exp:attr_class,
                                           data = conjoint_03_filtered,
                                           nway = 3,
                                           pair.id = conjoint_03_filtered$task_alt, diff = FALSE,
                                           screen = FALSE,   screen.type = "fixed", screen.num.int = 4, 
                                           collapse = FALSE, collapse.type = "fixed", collapse.cost = 0.3,
                                           family = "gaussian",
                                           cluster = conjoint_03_filtered$idperson,
                                           maxIter = 150,eps = 1e-05,
                                           fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                                           ord.fac   = c(F, T, T, F, F, F, F, T),
                                           select.prob = TRUE,
                                           boot = 5000,
                                           seed = 7303,
                                           verbose = TRUE)


##########################################################################################################################################
#STEP 5: CAUSAL INTERACTION ANALYSIS (MACHINE LEARNING INTERACTIONS)

## Sample Splitting

### Wave: 1, Sample: Full
set.seed(7303)
conjoint_01_resp_id_train <- sample(unique(conjoint_01_resp$idperson), length(table(conjoint_01_resp$idperson))/2, replace=FALSE)
conjoint_01_resp_id_test  <- setdiff(unique(conjoint_01_resp$idperson), conjoint_01_resp_id_train)
conjoint_01_resp_train     <- conjoint_01_resp[is.element(conjoint_01_resp$idperson,conjoint_01_resp_id_train), ]
conjoint_01_resp_test      <- conjoint_01_resp[is.element(conjoint_01_resp$idperson,conjoint_01_resp_id_test), ]

### Wave: 1, Sample: Filtered
set.seed(7303)
conjoint_01_filt_id_train  <- sample(unique(conjoint_01_filtered$idperson), length(table(conjoint_01_filtered$idperson))/2, replace=FALSE)
conjoint_01_filt_id_test   <- setdiff(unique(conjoint_01_filtered$idperson), conjoint_01_filt_id_train)
conjoint_01_filtered_train <- conjoint_01_filtered[is.element(conjoint_01_filtered$idperson,conjoint_01_filt_id_train), ]
conjoint_01_filtered_test  <- conjoint_01_filtered[is.element(conjoint_01_filtered$idperson,conjoint_01_filt_id_test), ]

### Wave: 3, Sample: Full
set.seed(7303)
conjoint_03_resp_id_train <- sample(unique(conjoint_03_resp$idperson), length(table(conjoint_03_resp$idperson))/2, replace=FALSE)
conjoint_03_resp_id_test  <- setdiff(unique(conjoint_03_resp$idperson), conjoint_03_resp_id_train)
conjoint_03_resp_train    <- conjoint_03_resp[is.element(conjoint_03_resp$idperson,conjoint_03_resp_id_train), ]
conjoint_03_resp_test     <- conjoint_03_resp[is.element(conjoint_03_resp$idperson,conjoint_03_resp_id_test), ]

### Wave: 3, Sample: Filtered
set.seed(7303)
conjoint_03_filt_id_train  <- sample(unique(conjoint_03_filtered$idperson), length(table(conjoint_03_filtered$idperson))/2, replace=FALSE)
conjoint_03_filt_id_test   <- setdiff(unique(conjoint_03_filtered$idperson), conjoint_03_filt_id_train)
conjoint_03_filtered_train <- conjoint_03_filtered[is.element(conjoint_03_filtered$idperson,conjoint_03_filt_id_train), ]
conjoint_03_filtered_test  <- conjoint_03_filtered[is.element(conjoint_03_filtered$idperson,conjoint_03_filt_id_test), ]

## Create empty objects to store results
causal_anova_01 <- list()
causal_anova_02 <- list()

## For Loop to estimate interactions

### Two-Way Interactions
for(i in 1:5){
  ### Train, 2-Way Interaction, Wave: 1, Sample: Full, DV: Choice
  temp01 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_resp_train, nway = 2,
                        pair.id = conjoint_01_resp_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "binomial",cluster = conjoint_01_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Full, DV: Choice
  causal_anova_01[[i]] <- test.CausalANOVA(fit = temp01,newdata = conjoint_01_resp_test,collapse.level = TRUE,
                          pair.id = conjoint_01_resp_test$task_alt, diff = TRUE,cluster = conjoint_01_resp_test$idperson) 
  
  ### Train, 2-Way Interaction Wave: 1, Sample: Filtered, DV: Choice
  temp02 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_filtered_train, nway = 2,
                        pair.id = conjoint_01_filtered_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "binomial",cluster = conjoint_01_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Choice
  causal_anova_01[[i+5]] <- test.CausalANOVA(fit = temp02,newdata = conjoint_01_filtered_test,collapse.level = TRUE,
                            pair.id = conjoint_01_filtered_test$task_alt, diff = TRUE,cluster = conjoint_01_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 1, Sample: Full, DV: Competence
  temp03 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_resp_train, nway = 2,
                        pair.id = conjoint_01_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_01_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Full, DV: Competence
  causal_anova_01[[i+10]] <- test.CausalANOVA(fit = temp03,newdata = conjoint_01_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_01_resp_test$task_alt, diff = FALSE,cluster = conjoint_01_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Competence
  temp04 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_filtered_train, nway = 2,
                        pair.id = conjoint_01_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_01_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Competence
  causal_anova_01[[i+15]] <- test.CausalANOVA(fit = temp04,newdata = conjoint_01_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_01_filtered_test$task_alt, diff = FALSE,cluster = conjoint_01_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Choice
  temp05 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 2,
                        pair.id = conjoint_03_resp_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "binomial",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Choice
  causal_anova_01[[i+20]] <- test.CausalANOVA(fit = temp05,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = TRUE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Choice
  temp06 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 2,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "binomial",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Choice
  causal_anova_01[[i+25]] <- test.CausalANOVA(fit = temp06,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = TRUE,cluster = conjoint_03_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Competence
  temp07 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 2,
                        pair.id = conjoint_03_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Competence
  causal_anova_01[[i+30]] <- test.CausalANOVA(fit = temp07,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = FALSE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Competence
  temp08 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 2,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Competence
  causal_anova_01[[i+35]] <- test.CausalANOVA(fit = temp08,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = FALSE,cluster = conjoint_03_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Representation
  temp09 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 2,
                        pair.id = conjoint_03_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Representation
  causal_anova_01[[i+40]] <- test.CausalANOVA(fit = temp07,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = FALSE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Representation
  temp10 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 2,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i, collapse = FALSE,family = "gaussian",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Representation
  causal_anova_01[[i+45]] <- test.CausalANOVA(fit = temp08,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = FALSE,cluster = conjoint_03_filtered_test$idperson) 
  
}


### Three-Way Interactions
for(i in 1:3){
  ### Train, 2-Way Interaction, Wave: 1, Sample: Full, DV: Choice
  temp01 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_resp_train, nway = 3,
                        pair.id = conjoint_01_resp_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "binomial",cluster = conjoint_01_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Full, DV: Choice
  causal_anova_02[[i]] <- test.CausalANOVA(fit = temp01,newdata = conjoint_01_resp_test,collapse.level = TRUE,
                          pair.id = conjoint_01_resp_test$task_alt, diff = TRUE,cluster = conjoint_01_resp_test$idperson) 
  
  ### Train, 2-Way Interaction Wave: 1, Sample: Filtered, DV: Choice
  temp02 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_filtered_train, nway = 3,
                        pair.id = conjoint_01_filtered_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "binomial",cluster = conjoint_01_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Choice
  causal_anova_02[[i+3]] <- test.CausalANOVA(fit = temp02,newdata = conjoint_01_filtered_test,collapse.level = TRUE,
                            pair.id = conjoint_01_filtered_test$task_alt, diff = TRUE,cluster = conjoint_01_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 1, Sample: Full, DV: Competence
  temp03 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_resp_train, nway = 3,
                        pair.id = conjoint_01_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_01_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Full, DV: Competence
  causal_anova_02[[i+6]] <- test.CausalANOVA(fit = temp03,newdata = conjoint_01_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_01_resp_test$task_alt, diff = FALSE,cluster = conjoint_01_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Competence
  temp04 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_01_filtered_train, nway = 3,
                        pair.id = conjoint_01_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_01_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 1, Sample: Filtered, DV: Competence
  causal_anova_02[[i+9]] <- test.CausalANOVA(fit = temp04,newdata = conjoint_01_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_01_filtered_test$task_alt, diff = FALSE,cluster = conjoint_01_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Choice
  temp05 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 3,
                        pair.id = conjoint_03_resp_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "binomial",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Choice
  causal_anova_02[[i+12]] <- test.CausalANOVA(fit = temp05,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = TRUE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Choice
  temp06 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 3,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = TRUE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "binomial",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Choice
  causal_anova_02[[i+15]] <- test.CausalANOVA(fit = temp06,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = TRUE,cluster = conjoint_03_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Competence
  temp07 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 3,
                        pair.id = conjoint_03_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Competence
  causal_anova_02[[i+18]] <- test.CausalANOVA(fit = temp07,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = FALSE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Competence
  temp08 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 3,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Competence
  causal_anova_02[[i+21]] <- test.CausalANOVA(fit = temp08,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = FALSE,cluster = conjoint_03_filtered_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Full, DV: Representation
  temp09 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_resp_train, nway = 3,
                        pair.id = conjoint_03_resp_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_03_resp_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Full, DV: Representation
  causal_anova_02[[i+24]] <- test.CausalANOVA(fit = temp07,newdata = conjoint_03_resp_test,collapse.level = TRUE,
                             pair.id = conjoint_03_resp_test$task_alt, diff = FALSE,cluster = conjoint_03_resp_test$idperson) 
  
  ### Train, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Representation
  temp10 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,data = conjoint_03_filtered_train, nway = 3,
                        pair.id = conjoint_03_filtered_train$task_alt, diff = FALSE,screen   = TRUE, screen.type = "fixed", 
                        screen.num.int = i+2, collapse = FALSE,family = "gaussian",cluster = conjoint_03_filtered_train$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),ord.fac   = c(F, T, T, F, F, F, F, T),verbose = TRUE) 
  
  ### Test, 2-Way Interaction, Wave: 3, Sample: Filtered, DV: Representation
  causal_anova_02[[i+27]] <- test.CausalANOVA(fit = temp08,newdata = conjoint_03_filtered_test,collapse.level = TRUE,
                             pair.id = conjoint_03_filtered_test$task_alt, diff = FALSE,cluster = conjoint_03_filtered_test$idperson) 
  
}


##########################################################################################################################################
#STEP 6: CAUSAL INTERACTION ANALYSIS (GROUP-LASSO INTERACTION-NET, K-FOLD CROSS-VALIDATION)

## prepare the data for screening (Randomized Attributes)

##Wave 1, Full Sample 
data.s01 <- matrix(NA, ncol=ncol(conjoint_01_resp_train[,c(8:15)]),nrow=nrow(conjoint_01_resp_train[,c(8:15)]))
for(i in 1:ncol(data.s01)){
  data.s01[,i] <- as.numeric(conjoint_01_resp_train[,c(8:15)][,i]) - 1            
}

##Wave 1, Filtered Sample 
data.s02 <- matrix(NA, ncol=ncol(conjoint_01_filtered_train[,c(8:15)]),nrow=nrow(conjoint_01_filtered_train[,c(8:15)]))
for(i in 1:ncol(data.s02)){
  data.s02[,i] <- as.numeric(conjoint_01_filtered_train[,c(8:15)][,i]) - 1            
}

##Wave 3, Full Sample 
data.s03 <- matrix(NA, ncol=ncol(conjoint_03_resp_train[,c(9:16)]),nrow=nrow(conjoint_03_resp_train[,c(9:16)]))
for(i in 1:ncol(data.s03)){
  data.s03[,i] <- as.numeric(conjoint_03_resp_train[,c(9:16)][,i]) - 1            
}

##Wave 3, Filtered Sample 
data.s04 <- matrix(NA, ncol=ncol(conjoint_03_filtered_train[,c(9:16)]),nrow=nrow(conjoint_03_filtered_train[,c(9:16)]))
for(i in 1:ncol(data.s04)){
  data.s04[,i] <- as.numeric(conjoint_03_filtered_train[,c(9:16)][,i]) - 1            
}

## Empty objects to store results
causal_anova_03 <- list()
causal_anova_04 <- list()
causal_anova_05 <- list()

## 10-Fold Cross-Validation, Group-LASSO Interaction-Net

### Wave: 1, Sample: Full, DV: Choice
causal_anova_03$gl.screen01 <- glinternet.cv(X=data.s01, Y=conjoint_01_resp_train[,6], family="binomial", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 1, Sample: Full, DV: Competence
causal_anova_03$gl.screen02 <- glinternet.cv(X=data.s01, Y=conjoint_01_resp_train[,7], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 1, Sample: Filtered, DV: Choice
causal_anova_03$gl.screen03 <- glinternet.cv(X=data.s02, Y=conjoint_01_filtered_train[,6], family="binomial", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)   

### Wave: 1, Sample: Full, Filtered: Competence
causal_anova_03$gl.screen04 <- glinternet.cv(X=data.s02, Y=conjoint_01_filtered_train[,7], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 3, Sample: Full, DV: Choice
causal_anova_03$gl.screen05 <- glinternet.cv(X=data.s03, Y=conjoint_03_resp_train[,6], family="binomial", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)   

### Wave: 3, Sample: Full, DV: Competence
causal_anova_03$gl.screen06 <- glinternet.cv(X=data.s03, Y=conjoint_03_resp_train[,7], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 3, Sample: Full, DV: Representation
causal_anova_03$gl.screen07 <- glinternet.cv(X=data.s03, Y=conjoint_03_resp_train[,8], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),lambdaMinRatio=0.45,
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Choice
causal_anova_03$gl.screen08 <- glinternet.cv(X=data.s04, Y=conjoint_03_filtered_train[,6], family="binomial", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)   

### Wave: 3, Sample: Filtered, DV: Competence
causal_anova_03$gl.screen09 <- glinternet.cv(X=data.s04, Y=conjoint_03_filtered_train[,7], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Representation
causal_anova_03$gl.screen10 <- glinternet.cv(X=data.s04, Y=conjoint_03_filtered_train[,8], family="gaussian", 
                                         numLevels = c(2, 3, 5, 3, 3, 2, 3, 3), lambdaMinRatio=0.45,
                                         nLambda=5, nFolds=5,verbose=FALSE, maxIter=150, numCores = 2)


## Explore the selected coefficients

### Wave: 1, Sample: Full, DV: Choice
causal_anova_04$explore01 <- glinternet(X=data.s01, Y=conjoint_01_resp_train[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen01$lambdaHat1Std, family="binomial", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Full, DV: Competence
causal_anova_04$explore02 <- glinternet(X=data.s01, Y=conjoint_01_resp_test[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen02$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Filtered, DV: Choice
causal_anova_04$explore03 <- glinternet(X=data.s02, Y=conjoint_01_filtered_test[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen03$lambdaHat1Std, family="binomial", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Full, Filtered: Competence
causal_anova_04$explore04 <- glinternet(X=data.s02, Y=conjoint_01_filtered_test[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen04$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Choice
causal_anova_04$explore05 <- glinternet(X=data.s03, Y=conjoint_03_resp_test[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen05$lambdaHat1Std, family="binomial", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Competence
causal_anova_04$explore06 <- glinternet(X=data.s03, Y=conjoint_03_resp_test[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen06$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Representation
causal_anova_04$explore07 <- glinternet(X=data.s03, Y=conjoint_03_resp_test[,8], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen07$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Choice
causal_anova_04$explore08 <- glinternet(X=data.s04, Y=conjoint_03_filtered_test[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen08$lambdaHat1Std, family="binomial", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Competence
causal_anova_04$explore09 <- glinternet(X=data.s04, Y=conjoint_03_filtered_test[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen09$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Representation
causal_anova_04$explore10 <- glinternet(X=data.s04, Y=conjoint_03_filtered_test[,8], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                    lambda = causal_anova_03$gl.screen10$lambdaHat1Std, family="gaussian", 
                                    verbose=FALSE, maxIter=5000, numCores = 2)


causal_anova_04$explore03$activeSet   #Wave: 1, Sample: Filtered, DV: Vote            | 0
causal_anova_04$explore04$activeSet   #Wave: 1, Sample: Filtered, DV: Competence      | 0
causal_anova_04$explore08$activeSet   #Wave: 3, Sample: Filtered, DV: Vote            | 1 pol_exp, rel
causal_anova_04$explore09$activeSet   #Wave: 3, Sample: Filtered, DV: Competence      | 1 pol_exp, rel
causal_anova_04$explore10$activeSet   #Wave: 3, Sample: Filtered, DV: Representation  | 2 pol_exp, rel, age

causal_anova_04$explore01$activeSet   #Wave: 1, Sample: Full,     DV: Vote            | 0
causal_anova_04$explore02$activeSet   #Wave: 1, Sample: Full,     DV: Competence      | 0
causal_anova_04$explore05$activeSet   #Wave: 3, Sample: Full,     DV: Vote            | 2 pol_exp
causal_anova_04$explore06$activeSet   #Wave: 3, Sample: Full,     DV: Competence      | 2 pol_exp
causal_anova_04$explore07$activeSet   #Wave: 3, Sample: Full,     DV: Representation  | 2 pol_exp, res, rel

### Wave: 1, Sample: Full, DV: Choice
causal_anova_05$explore01 <- glinternet(X=data.s01, Y=conjoint_01_resp_train[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen01$lambdaHat, family="binomial", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Full, DV: Competence
causal_anova_05$explore02 <- glinternet(X=data.s01, Y=conjoint_01_resp_train[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen02$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Filtered, DV: Choice
causal_anova_05$explore03 <- glinternet(X=data.s02, Y=conjoint_01_filtered_train[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen03$lambdaHat, family="binomial", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 1, Sample: Full, Filtered: Competence
causal_anova_05$explore04 <- glinternet(X=data.s02, Y=conjoint_01_filtered_train[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen04$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Choice
causal_anova_05$explore05 <- glinternet(X=data.s03, Y=conjoint_03_resp_train[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen05$lambdaHat, family="binomial", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Competence
causal_anova_05$explore06 <- glinternet(X=data.s03, Y=conjoint_03_resp_train[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen06$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Full, DV: Representation
causal_anova_05$explore07 <- glinternet(X=data.s03, Y=conjoint_03_resp_train[,8], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen07$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Choice
causal_anova_05$explore08 <- glinternet(X=data.s04, Y=conjoint_03_filtered_train[,6], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen08$lambdaHat, family="binomial", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Competence
causal_anova_05$explore09 <- glinternet(X=data.s04, Y=conjoint_03_filtered_train[,7], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen09$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

### Wave: 3, Sample: Filtered, DV: Representation
causal_anova_05$explore10 <- glinternet(X=data.s04, Y=conjoint_03_filtered_train[,8], numLevels = c(2, 3, 5, 3, 3, 2, 3, 3),
                                        lambda = causal_anova_03$gl.screen10$lambdaHat, family="gaussian", 
                                        verbose=FALSE, maxIter=5000, numCores = 2)

##########################################################################################################################################
#STEP 7: STORE RESULTS

## Create objects
write_rds(x = causal_anova_00, file = "0C_Objects/Causal_Anova_Wave_01_and_03_Specified_Interactions.rds")
write_rds(x = causal_anova_01, file = "0C_Objects/Causal_Anova_Wave_01_and_03_2W_ML_Interactions.rds")
write_rds(x = causal_anova_02, file = "0C_Objects/Causal_Anova_Wave_01_and_03_3W_ML_Interactions.rds")
write_rds(x = causal_anova_03, file = "0C_Objects/Causal_Anova_Wave_01_and_03_K_Fold_Cross_Validation.rds")
write_rds(x = causal_anova_04, file = "0C_Objects/Causal_Anova_Wave_01_and_03_ML_Identification_of_Interactions.rds")
write_rds(x = causal_anova_05, file = "0C_Objects/Causal_Anova_Wave_01_and_03_ML_Identification_of_Interactions_Alt_Lambda.rds")


## Extract interactions

### Create empty objects to store interactions
df1 <- data.frame(group = rep(NA,50), int = rep(NA,50))
df2 <- data.frame(group = rep(NA,30), int = rep(NA,30))

### For loop, Two-Way Interactions
for(i in 1:50){
  df1$group[i] <- i
  df1$int[i]   <- paste(str_subset(string = attr(terms(causal_anova_01[[i]]$formula),"term.labels") ,pattern = "\\:"), 
                        sep = ",", collapse = ",")
}

### For loop, Three-Way Interactions
for(i in 1:30){
  df2$group[i] <- i
  df2$int[i]   <- paste(str_subset(string = attr(terms(causal_anova_02[[i]]$formula),"term.labels") ,pattern = "\\:"), 
                        sep = ",", collapse = ",")
}

## Separate Columns
df1 %>% separate(int, sep = ",", c("I1","I2","I3","I4","I5","I6","I7")) -> df1a
df2 %>% separate(int, sep = ",", c("I1","I2","I3","I4","I5","I6","I7","I8")) -> df2a

## Include original row
df1b <- full_join(x = df1, y = df1a, by = "group")
df2b <- full_join(x = df2, y = df2a, by = "group")

## Store the results
write_csv(x = df1b, file = "Table1.csv")
write_csv(x = df2b, file = "Table2.csv")


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
#[9] tidyverse_1.3.1 FindIt_1.2.0    arm_1.12-2      lme4_1.1-27.1   Matrix_1.4-0    MASS_7.3-55    
#
#loaded via a namespace (and not attached):
#  [1] nlme_3.1-155       fs_1.5.2           lubridate_1.8.0    httr_1.4.2         insight_0.15.0     tools_4.1.2        backports_1.4.1   
#[8] utf8_1.2.2         R6_2.5.1           sjlabelled_1.1.8   DBI_1.1.2          colorspace_2.0-2   withr_2.4.3        tidyselect_1.1.1  
#[15] emmeans_1.7.2      compiler_4.1.2     rvest_1.0.2        performance_0.8.0  glmnet_4.1-3       cli_3.2.0          xml2_1.3.3        
#[22] sandwich_3.0-1     bayestestR_0.11.5  scales_1.1.1       lmtest_0.9-39      mvtnorm_1.1-3      quadprog_1.5-8     digest_0.6.29     
#[29] minqa_1.2.4        pkgconfig_2.0.3    htmltools_0.5.2    dbplyr_2.1.1       fastmap_1.1.0      readxl_1.3.1       rlang_1.0.1       
#[36] rstudioapi_0.13    cjoint_2.1.0       shiny_1.7.1        shape_1.4.6        generics_0.1.2     jsonlite_1.7.3     zoo_1.8-9         
#[43] car_3.0-12         sjPlot_2.8.10      magrittr_2.0.2     lars_1.2           parameters_0.16.0  Rcpp_1.0.8         munsell_0.5.0     
#[50] fansi_1.0.2        abind_1.4-5        lifecycle_1.0.1    stringi_1.7.6      multcomp_1.4-18    carData_3.0-5      ggstance_0.3.5    
#[57] grid_4.1.2         promises_1.2.0.1   sjmisc_2.8.9       crayon_1.5.0       lattice_0.20-45    ggeffects_1.1.1    haven_2.4.3       
#[64] splines_4.1.2      hms_1.1.1          sjstats_0.18.1     knitr_1.37         pillar_1.7.0       igraph_1.2.11      boot_1.3-28       
#[71] estimability_1.3   effectsize_0.6.0.1 codetools_0.2-18   lpSolve_5.6.15     cregg_0.4.0        reprex_2.0.1       glue_1.6.1        
#[78] mitools_2.4        modelr_0.1.8       tzdb_0.2.0         vctrs_0.3.8        nloptr_2.0.0       httpuv_1.6.5       foreach_1.5.1     
#[85] cellranger_1.1.0   gtable_0.3.0       assertthat_0.2.1   datawizard_0.2.3   xfun_0.29          limSolve_1.5.6     mime_0.12         
#[92] xtable_1.8-4       broom_0.7.12       survey_4.1-1       coda_0.19-4        later_1.3.0        survival_3.2-13    glinternet_1.0.12 
#[99] iterators_1.0.13   TH.data_1.1-0      ellipsis_0.3.2 