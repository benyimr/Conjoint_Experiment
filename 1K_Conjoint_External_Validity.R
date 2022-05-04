##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº10:  Experimental Results (Advanced: External Validity)
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)


library(factorEx)


conjoint_01 %>% group_by(idperson) %>% summarise(id = first(idperson)) %>% sample_frac(size = 0.05) %>% 
  dplyr::select(id) %>% pull() -> sample_cases
conjoint_01 %>% filter(idperson %in% sample_cases) %>% dplyr::select(-typ_occup_resp,-relig_resp,-party_02) %>%
  group_by(idperson) %>% mutate(id = cur_group_id()) %>% 
  mutate(task_esp = paste0(id,"-",task)) %>% as.data.frame() -> sample

alpha <- list(attr_sex = c("Male" = 0.55, "Female" = 0.45),
              attr_age = c("50 years" = 0.50, "25 years" = 0.20, "70 years" = 0.3),
              attr_ideol = c("Center" = 0.3, "Right" = 0.1, "Center-Right" = 0.2, "Center-Left" = 0.25, "Left" = 0.15),
              attr_pol_exp = c("None" = 0.3, "Activist" = 0.2, "Elected Official" = 0.5),
              attr_pty_mem = c("Independent supported by Party" = 0.4, "Member of Party" = 0.35, "Independent" = 0.25),
              attr_res = c("Live in another County" = 0.40, "Same County" = 0.6),
              attr_rel = c("Catholic" = 0.55, "Evangelical" = 0.25, "Irreligious" = 0.20),
              attr_class = c("Middle Class" = 0.3, "Working Class" = 0.2, "Upper Class" = 0.5))

# All Candidates
amce_model_pop_01 <- model_pAMCE(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp + attr_pty_mem + 
                                   attr_res + attr_rel + attr_class,
                                 formula_three = NULL,
                                 data = sample,
                                 reg = TRUE, 
                                 
                                 pair = FALSE,
                                 
                                 cross_int = FALSE,
                                 cluster_id = sample$id,
                                 target_dist = alpha,
                                 target_type = c("marginal"),
                                 difference = FALSE,
                                 cv_type = "cv.1Std",
                                 nfolds = 5,
                                 boot = 500,
                                 seed = 7303,
                                 numCores = NULL)
                                 
                                 #ord_fac = NULL,
                                 #pair_id = sample$task_esp, 
                                 
                                 
                                 
                                 
                                 
      
                                 
                                 
                                  
                                 
                                 )

try <- model_pAMCE(formula = choice ~ Sex + Age + Ideology,,
                          data = sample, reg = TRUE, cross_int = TRUE,
                          pair_id = sample$task_esp, cluster_id = sample$idperson,
                          target_dist = alpha, target_type = c("marginal"),
                          boot = 500, seed = 1234)

