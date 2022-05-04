##########################################################################################################################################
#THE HETEROGENEOUS EFFECT OF SOCIAL CLASS ON VOTE CHOICE
##R Code Nº 4: Data Management Conjoint Datasets
#Author: Benjamín Muñoz
#Date: March 18, 2022
##########################################################################################################################################
#STEP 0: WORKING SPACE

## Remove objects from working environment
rm(list=ls())

## Set options
options(scipen = 1000000)

## Load packages
library(sjlabelled)
library(tidyverse)


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Pre-Electoral Wave
wave_pre <- read_data(path = "0A_Datasets/4_Panel/BBDD_FONDECYT_OLA1.dta")

## Post-Electoral Wave
wave_post <- read_data(path = "0A_Datasets/4_Panel/BBDD_Ola3_FONDECYT.dta")


##########################################################################################################################################
#STEP 2: GENERAL INFORMATION ABOUT VARIABLES

## Change variable names
names(wave_pre)  <- stringr::str_to_lower(names(wave_pre))
names(wave_post) <- stringr::str_to_lower(names(wave_post))


## FOR A FUTURE VERSION: WIDE TO LONG WITH cregg::cj_tidy

## WAVE Nº1

#folio                            ID

# attribute_1    : attribute_16   First   Task
# attribute_17   : attribute_32   Second  Task
# attribute_33   : attribute_48   Third   Task
# attribute_49   : attribute_64   Fourth  Task
# attribute_65   : attribute_80   Fifth   Task
# attribute_81   : attribute_96   Sixth   Task
# attribute_97   : attribute_112  Seventh Task
# attribute_113  : attribute_128  Eighth  Task
# attribute_129                   Code
# attribute_130  : attribute_137  Attribute
# grupo                           Experimental Group
# codigo_region                   Region
# nombre_region                   Region
# codigo_comuna                   Comuna
# nombre_comuna                   Comuna
# codigo_sexo                     Sex
# nombre_sexo                     Sex
# codigo_gse_jefehogar            Socio-Economic
# nombre_gse_jefehogar            Socio-Economic
# codigo_ocupacion_jefehogar      Occupation
# nombre_ocupacion_jefehogar      Occupation
# codigo_educacion_jefehogar      Education
# nombre_educacion_jefehogar      Education
# registro                        Registry
# codigo_zona                     Zone
# nombre_zona                     Zone
# nombre_panelista_cati           Nombre
# fecha_nacimiento_panelista      Fecha de Nacimiento
# codigo_identificacion_politica  Ideology
# nombre_identificacion_politica  Ideology
# edad_panelista                  Age
# nombre_rango_edad_panelista     Age
# cod_rango_edad_panelista        Age 

#e1x1_1                           DV Competetent A First   Task
#e1x1_2                           DV Competetent B First   Task
#e2x1	                            DV Vote          First   Task  
#e1x2_1                           DV Competetent A Second  Task
#e1x2_2                           DV Competetent B Second  Task
#e2x2	                            DV Vote          Second  Task
#e1x3_1                           DV Competetent A Third   Task
#e1x3_2                           DV Competetent B Third   Task
#e2x3	                            DV Vote          Third   Task
#e1x4_1                           DV Competetent A Fourth  Task
#e1x4_2                           DV Competetent B Fourth  Task
#e2x4	                            DV Vote          Fourth  Task
#e1x5_1                           DV Competetent A Fifth   Task
#e1x5_2                           DV Competetent B Fifth   Task
#e2x5	                            DV Vote          Fifth   Task
#e1x6_1                           DV Competetent A Sixth   Task
#e1x6_2                           DV Competetent B Sixth   Task
#e2x6	                            DV Vote          Sixth   Task
#e1x7_1                           DV Competetent A Seventh Task
#e1x7_2                           DV Competetent B Seventh Task
#e2x7	                            DV Vote          Seventh Task
#e1x8_1                           DV Competetent A Eighth  Task
#e1x8_2                           DV Competetent B Eighth  Task
#e2x8	                            DV Vote          Eighth  Task
#e3                               Screener

#p14                              Political Interest
#p15_sq001                        Ideology
#p16, p17, p17_other              Partisanship
#p18a : p18_e                     Political Knowledge
#p18_sq001: p18_sq003             Social Trust
#p19                              Political Trust
#p20                              Other Political Participation
#p1 : p7                          Retrospective Political Participation
#p8 : p10                         Intention to vote
#p21                              Political Efficacy/Attitudes towards vote
#p22                              Satisfaction with Democracy
#p23                              Presidential Approval
#p24 : p25                        Corruption
#p28                              Screener
#c1                               Sex
#c2                               Age
#c3                               Education
#c4                               Occupation
#c5                               Occupation 
#c6                               Occupation
#c7                               Household Income
#c8                               Religion
#region                           Region
#comuna                           Comuna


## WAVE Nº 3

# id_original                     ID
# id                              ID

# attribute_1    : attribute_18   First   Task
# attribute_19   : attribute_36   Second  Task
# attribute_37   : attribute_54   Third   Task
# attribute_55   : attribute_72   Fourth  Task
# attribute_73   : attribute_90   Fifth   Task
# attribute_91   : attribute_108  Sixth   Task
# attribute_109  : attribute_126  Seventh Task
# attribute_127  : attribute_144  Eighth  Task
# attribute_146                   Code
# attribute_147  : attribute_155  Attribute
# grupo                           Experimental Group
# codigo_region                   Region
# nombre_region                   Region
# codigo_comuna                   Comuna
# nombre_comuna                   Comuna
# codigo_sexo                     Sex
# nombre_sexo                     Sex
# codigo_gse_jefehogar            Socio-Economic
# nombre_gse_jefehogar            Socio-Economic
# codigo_ocupacion_jefehogar      Occupation
# nombre_ocupacion_jefehogar      Occupation
# codigo_educacion_jefehogar      Education
# nombre_educacion_jefehogar      Education
# codigo_zona                     Zone
# nombre_zona                     Zone
# fecha_nacimiento_panelista      Fecha de Nacimiento
# codigo_identificacion_politica  Ideology
# nombre_identificacion_politica  Ideology
# edad_panelista                  Age
# nombre_rango_edad_panelista     Age
# cod_rango_edad_panelista        Age 
# nacionalidad                    Nationality

#e1x1	                            DV Vote             First     Task  
#e1x2_1                           DV Competetent A    First     Task
#e1x2_2                           DV Competetent B    First     Task
#e1x3_1                           DV Representation A First     Task
#e1x3_2                           DV Representation B First     Task
#e2x1	                            DV Vote             Second    Task  
#e2x2_1                           DV Competetent A    Second    Task
#e2x2_2                           DV Competetent B    Second    Task
#e2x3_1                           DV Representation A Second    Task
#e2x3_2                           DV Representation B Second    Task
#e3x1	                            DV Vote             Third     Task  
#e3x2_1                           DV Competetent A    Third     Task
#e3x2_2                           DV Competetent B    Third     Task
#e3x3_1                           DV Representation A Third     Task
#e3x3_2                           DV Representation B Third     Task
#e4x1	                            DV Vote             Fourth    Task  
#e4x2_1                           DV Competetent A    Fourth    Task
#e4x2_2                           DV Competetent B    Fourth    Task
#e4x3_1                           DV Representation A Fourth    Task
#e4x3_2                           DV Representation B Fourth    Task
#e5x1	                            DV Vote             Fifth     Task  
#e5x2_1                           DV Competetent A    Fifth     Task
#e5x2_2                           DV Competetent B    Fifth     Task
#e5x3_1                           DV Representation A Fifth     Task
#e5x3_2                           DV Representation B Fifth     Task
#e6x1	                            DV Vote             Sixth     Task  
#e6x2_1                           DV Competetent A    Sixth     Task
#e6x2_2                           DV Competetent B    Sixth     Task
#e6x3_1                           DV Representation A Sixth     Task
#e6x3_2                           DV Representation B Sixth     Task
#e7x1	                            DV Vote             Seventh   Task  
#e7x2_1                           DV Competetent A    Seventh   Task
#e7x2_2                           DV Competetent B    Seventh   Task
#e7x3_1                           DV Representation A Seventh   Task
#e7x3_2                           DV Representation B Seventh   Task
#e8x1	                            DV Vote             Eighth    Task  
#e8x2_1                           DV Competetent A    Eighth    Task
#e8x2_2                           DV Competetent B    Eighth    Task
#e8x3_1                           DV Representation A Eighth    Task
#e8x3_2                           DV Representation B Eighth    Task
#vd1                              Screener

#p13                              Political Interest
#p14_1                            Ideology
#p17_a : p17_e                    Political Knowledge
#p19_1 : p19_4                    Efficacy 
#p21_1: p21_3                     Social Trust
#p22                              Political Trust
#p1 : p4                          Retrospective Political Participation
#p5 : p7                          Intention to vote
#p24                              Political Efficacy/Attitudes towards vote
#p26                              Satisfaction with Democracy
#p27 : p28                        Corruption
#c1                               Sex
#c2                               Age
#c3                               Education
#c4                               Occupation
#c6                               Occupation
#c7                               Household Income
#c8                               Household Size
#c9, c9_other                     Civil Status


##########################################################################################################################################
#STEP 3: DATA PROCESSING OF ATTRIBUTES

## Variables related to hypothetical profiles

### Eight Tasks, 2 profiles per task. 8 Attributes. Wave 1
v01a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(1))
v01b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(1))
v02a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(2))
v02b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(2))
v03a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(3))
v03b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(3))
v04a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(4))
v04b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(4))
v05a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(5))
v05b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(5))
v06a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(6))
v06b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(6))
v07a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(7))
v07b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(7))
v08a <- paste0("attribute_",seq(from = -15, to = -1, by = 2) + 16*(8))
v08b <- paste0("attribute_",seq(from = -14, to =  0, by = 2) + 16*(8))

### Eight Tasks, 2 profiles per task. 8/9 Attributes. Wave 3
v01c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(1))
v01d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(1))
v02c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(2))
v02d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(2))
v03c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(3))
v03d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(3))
v04c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(4))
v04d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(4))
v05c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(5))
v05d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(5))
v06c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(6))
v06d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(6))
v07c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(7))
v07d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(7))
v08c <- paste0("attribute_",seq(from = -17, to = -1, by = 2) + 18*(8))
v08d <- paste0("attribute_",seq(from = -16, to =  0, by = 2) + 18*(8))


## Variables identifying the name of the attributes
esp_01 <- paste0("attribute_",seq(from = 130, to =  137, by = 1))
esp_02 <- paste0("attribute_",seq(from = 147, to =  155, by = 1))


## Filtering datasets. Select relevant variables (one data.frame per profile)

### Wave 1
wave_pre %>% select(folio, all_of(v01a), all_of(esp_01), e1x1_1, e2x1) %>% mutate(candidate = "A", task = 1) -> data_01a
wave_pre %>% select(folio, all_of(v01b), all_of(esp_01), e1x1_2, e2x1) %>% mutate(candidate = "B", task = 1) -> data_01b
wave_pre %>% select(folio, all_of(v02a), all_of(esp_01), e1x2_1, e2x2) %>% mutate(candidate = "A", task = 2) -> data_02a
wave_pre %>% select(folio, all_of(v02b), all_of(esp_01), e1x2_2, e2x2) %>% mutate(candidate = "B", task = 2) -> data_02b
wave_pre %>% select(folio, all_of(v03a), all_of(esp_01), e1x3_1, e2x3) %>% mutate(candidate = "A", task = 3) -> data_03a
wave_pre %>% select(folio, all_of(v03b), all_of(esp_01), e1x3_2, e2x3) %>% mutate(candidate = "B", task = 3) -> data_03b
wave_pre %>% select(folio, all_of(v04a), all_of(esp_01), e1x4_1, e2x4) %>% mutate(candidate = "A", task = 4) -> data_04a
wave_pre %>% select(folio, all_of(v04b), all_of(esp_01), e1x4_2, e2x4) %>% mutate(candidate = "B", task = 4) -> data_04b
wave_pre %>% select(folio, all_of(v05a), all_of(esp_01), e1x5_1, e2x5) %>% mutate(candidate = "A", task = 5) -> data_05a
wave_pre %>% select(folio, all_of(v05b), all_of(esp_01), e1x5_2, e2x5) %>% mutate(candidate = "B", task = 5) -> data_05b
wave_pre %>% select(folio, all_of(v06a), all_of(esp_01), e1x6_1, e2x6) %>% mutate(candidate = "A", task = 6) -> data_06a
wave_pre %>% select(folio, all_of(v06b), all_of(esp_01), e1x6_2, e2x6) %>% mutate(candidate = "B", task = 6) -> data_06b
wave_pre %>% select(folio, all_of(v07a), all_of(esp_01), e1x7_1, e2x7) %>% mutate(candidate = "A", task = 7) -> data_07a
wave_pre %>% select(folio, all_of(v07b), all_of(esp_01), e1x7_2, e2x7) %>% mutate(candidate = "B", task = 7) -> data_07b
wave_pre %>% select(folio, all_of(v08a), all_of(esp_01), e1x8_1, e2x8) %>% mutate(candidate = "A", task = 8) -> data_08a
wave_pre %>% select(folio, all_of(v08b), all_of(esp_01), e1x8_2, e2x8) %>% mutate(candidate = "B", task = 8) -> data_08b

### Wave 3
wave_post %>% select(folio, all_of(v01c), all_of(esp_02), e1x1, e1x2_1, e1x3_1) %>% mutate(candidate = "A", task = 1) -> data_01c
wave_post %>% select(folio, all_of(v01d), all_of(esp_02), e1x1, e1x2_2, e1x3_2) %>% mutate(candidate = "B", task = 1) -> data_01d
wave_post %>% select(folio, all_of(v02c), all_of(esp_02), e2x1, e2x2_1, e2x3_1) %>% mutate(candidate = "A", task = 2) -> data_02c
wave_post %>% select(folio, all_of(v02d), all_of(esp_02), e2x1, e2x2_2, e2x3_2) %>% mutate(candidate = "B", task = 2) -> data_02d
wave_post %>% select(folio, all_of(v03c), all_of(esp_02), e3x1, e3x2_1, e3x3_1) %>% mutate(candidate = "A", task = 3) -> data_03c
wave_post %>% select(folio, all_of(v03d), all_of(esp_02), e3x1, e3x2_2, e3x3_2) %>% mutate(candidate = "B", task = 3) -> data_03d
wave_post %>% select(folio, all_of(v04c), all_of(esp_02), e4x1, e4x2_1, e4x3_1) %>% mutate(candidate = "A", task = 4) -> data_04c
wave_post %>% select(folio, all_of(v04d), all_of(esp_02), e4x1, e4x2_2, e4x3_2) %>% mutate(candidate = "B", task = 4) -> data_04d
wave_post %>% select(folio, all_of(v05c), all_of(esp_02), e5x1, e5x2_1, e5x3_1) %>% mutate(candidate = "A", task = 5) -> data_05c
wave_post %>% select(folio, all_of(v05d), all_of(esp_02), e5x1, e5x2_2, e5x3_2) %>% mutate(candidate = "B", task = 5) -> data_05d
wave_post %>% select(folio, all_of(v06c), all_of(esp_02), e6x1, e6x2_1, e6x3_1) %>% mutate(candidate = "A", task = 6) -> data_06c
wave_post %>% select(folio, all_of(v06d), all_of(esp_02), e6x1, e6x2_2, e6x3_2) %>% mutate(candidate = "B", task = 6) -> data_06d
wave_post %>% select(folio, all_of(v07c), all_of(esp_02), e7x1, e7x2_1, e7x3_1) %>% mutate(candidate = "A", task = 7) -> data_07c
wave_post %>% select(folio, all_of(v07d), all_of(esp_02), e7x1, e7x2_2, e7x3_2) %>% mutate(candidate = "B", task = 7) -> data_07d
wave_post %>% select(folio, all_of(v08c), all_of(esp_02), e8x1, e8x2_1, e8x3_1) %>% mutate(candidate = "A", task = 8) -> data_08c
wave_post %>% select(folio, all_of(v08d), all_of(esp_02), e8x1, e8x2_2, e8x3_2) %>% mutate(candidate = "B", task = 8) -> data_08d


## Set Variable Names

### Wave 1
names(data_01a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_01b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_02a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_02b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_03a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_03b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_04a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_04b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_05a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_05b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_06a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_06b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_07a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_07b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_08a) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")
names(data_08b) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","b1","b2","b3","b4","b5","b6","b7","b8","eval","vote","cand","task")

### Wave 3
names(data_01c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_01d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_02c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_02d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_03c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_03d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_04c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_04d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_05c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_05d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_06c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_06d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_07c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_07d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_08c) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")
names(data_08d) <- c("idperson","a1","a2","a3","a4","a5","a6","a7","a8","a9","b1","b2","b3","b4","b5","b6","b7","b8","b9","vote","eval","repr","cand","task")


## Create and select variables relevant variables (attribute:value)

### Wave 1
data_01a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_01e
data_01b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_01f
data_02a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_02e
data_02b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_02f
data_03a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_03e
data_03b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_03f
data_04a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_04e
data_04b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_04f
data_05a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_05e
data_05b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_05f
data_06a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_06e
data_06b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_06f
data_07a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_07e
data_07b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_07f
data_08a %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_08e
data_08b %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8)) %>% 
  select(idperson,c1:c8,eval:task) -> data_08f

### Wave 3
data_01c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_01g
data_01d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_01h
data_02c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_02g
data_02d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_02h
data_03c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_03g
data_03d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_03h
data_04c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_04g
data_04d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_04h
data_05c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_05g
data_05d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_05h
data_06c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_06g
data_06d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_06h
data_07c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_07g
data_07d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_07h
data_08c %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_08g
data_08d %>% mutate(c1 = paste0(a1,";",b1),c2 = paste0(a2,";",b2),c3 = paste0(a3,";",b3),c4 = paste0(a4,";",b4),
                    c5 = paste0(a5,";",b5),c6 = paste0(a6,";",b6),c7 = paste0(a7,";",b7),c8 = paste0(a8,";",b8),
                    c9 = paste0(a9,";",b9)) %>% select(idperson,c1:c9,vote:task) -> data_08h


##########################################################################################################################################
#STEP 4: STRUCTURE OF DATASET 

## Empty list to store results
empty_list1 <- list()
empty_list2 <- list()

## Loop to create dataframes (Element = Respondent). Wave 1
for(i in 1:2851){
  
  ### Combine different profiles evaluated by one respondent
  empty_list1[[i]] <- bind_rows(data_01e[i,],data_01f[i,],
                                data_02e[i,],data_02f[i,],
                                data_03e[i,],data_03f[i,],
                                data_04e[i,],data_04f[i,],
                                data_05e[i,],data_05f[i,],
                                data_06e[i,],data_06f[i,],
                                data_07e[i,],data_07f[i,],
                                data_08e[i,],data_08f[i,])
}


## Loop to create dataframes (Element = Respondent). Wave 3
for(i in 1:1637){
  
  ### Combine different profiles evaluated by one respondent
  empty_list2[[i]] <- bind_rows(data_01g[i,],data_01h[i,],
                                data_02g[i,],data_02h[i,],
                                data_03g[i,],data_03h[i,],
                                data_04g[i,],data_04h[i,],
                                data_05g[i,],data_05h[i,],
                                data_06g[i,],data_06h[i,],
                                data_07g[i,],data_07h[i,],
                                data_08g[i,],data_08h[i,])
}


## Empty data.frame to store results
empty_df1 <- data.frame()
empty_df2 <- data.frame()


### Create a single data.frame. Wave 1
for(i in 1:2851){
  
  empty_df1 <- bind_rows(empty_df1,empty_list1[[i]])
}

### Create a single data.frame. Wave 3
for(i in 1:1637){
  
  empty_df2 <- bind_rows(empty_df2,empty_list2[[i]])
}


##########################################################################################################################################
#STEP 5: RECODE OF ATTRIBUTES AND SELECTION OF CONJOINT'S VARIABLES

## Create combination of attributes. Wave 1
empty_df1$temp <- paste(empty_df1$c1,empty_df1$c2,empty_df1$c3,empty_df1$c4,empty_df1$c5,
                        empty_df1$c6,empty_df1$c7,empty_df1$c8,empty_df1$c9,sep = ",")

### Sex, Wave 1
empty_df1$sex_c <-    if_else(condition = str_detect(string = empty_df1$temp, pattern = "Hombre;Sexo") == TRUE, 
                              true = "Male", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Mujer;Sexo") == TRUE, 
                                        true = "Female", false = "Check"))

### Age, Wave 1
empty_df1$age_c <-    if_else(condition = str_detect(string = empty_df1$temp, pattern = "25;Edad") == TRUE, 
                              true = "25 years", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "50;Edad") == TRUE, 
                                        true = "50 years", false = 
                                          if_else(condition = str_detect(string = empty_df1$temp, pattern = "70;Edad") == TRUE, 
                                                  true = "70 years", false = "Check")))

### Ideology, Wave 1
empty_df1$ideol_c <-  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Derecha;Ideología Política") == TRUE, 
                              true = "Right", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Centro-derecha;Ideología Política") == TRUE, 
                                        true = "Center-Right", false = 
                                          if_else(condition = str_detect(string = empty_df1$temp, pattern = "Centro-izquierda;Ideología Política") == TRUE, 
                                                  true = "Center-Left", false = 
                                                    if_else(condition = str_detect(string = empty_df1$temp, pattern = "Izquierda;Ideología Política") == TRUE, 
                                                            true = "Left", false = 
                                                              if_else(condition = str_detect(string = empty_df1$temp, pattern = "Centro;Ideología Política") == TRUE, 
                                                                      true = "Center", false = "Check")))))

### Political Experience, Wave 1
empty_df1$pol_exp_c <- if_else(condition = str_detect(string = empty_df1$temp, pattern = "Activista en organización de la sociedad civil;Experiencia Política") == TRUE, 
                               true = "Activist", false = 
                                 if_else(condition = str_detect(string = empty_df1$temp, pattern = "Cargo de elección popular;Experiencia Política") == TRUE, 
                                         true = "Elected Official", false = 
                                           if_else(condition = str_detect(string = empty_df1$temp, pattern = "Ninguna;Experiencia Política") == TRUE, 
                                                   true = "None", false = "Check")))

### Party Membership, Wave 1
empty_df1$party_c <-   if_else(condition = str_detect(string = empty_df1$temp, pattern = "Milita en un partido político;Militancia") == TRUE, 
                               true = "Member of Party", false = 
                                 if_else(condition = str_detect(string = empty_df1$temp, pattern = "Independiente con el apoyo de partidos;Militancia") == TRUE, 
                                         true = "Independent supported by Party", false = 
                                           if_else(condition = str_detect(string = empty_df1$temp, pattern = "Independiente sin el apoyo de partidos;Militancia") == TRUE, 
                                                   true = "Independent", false = "Check")))

### Residence, Wave 1
empty_df1$resid_c <-  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Vive en tu comuna;Residencia") == TRUE, 
                              true = "Same County", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Vive en otra comuna;Residencia") == TRUE, 
                                        true = "Live in another County", false = "Check"))

### Religion, Wave 1
empty_df1$relig_c <-  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Católico/a;Religión") == TRUE, 
                              true = "Catholic", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Evangélico/a;Religión") == TRUE, 
                                        true = "Evangelical", false = 
                                          if_else(condition = str_detect(string = empty_df1$temp, pattern = "Irreligioso/a;Religión") == TRUE, 
                                                  true = "Irreligious", false = "Check")))

### Occupation, Wave 1
empty_df1$occup_c <-  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Obrero/a de la construcción;Ocupación") == TRUE, 
                              true = "Construction worker", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Médico/a;Ocupación") == TRUE, 
                                        true = "Physician", false = 
                                          if_else(condition = str_detect(string = empty_df1$temp, pattern = "Vendedor/a en retail;Ocupación") == TRUE, 
                                                  true = "Salesperson", false = 
                                                    if_else(condition = str_detect(string = empty_df1$temp, pattern = "Contador/a;Ocupación") == TRUE, 
                                                            true = "Accountant",false = 
                                                              if_else(condition = str_detect(string = empty_df1$temp, pattern = "Ingeniero/a civil en minas;Ocupación") == TRUE, 
                                                                      true = "Civil Engineer in Mining", false = 
                                                                        if_else(condition = str_detect(string = empty_df1$temp, pattern = "Profesor/a de colegio;Ocupación") == TRUE, 
                                                                                true = "Teacher", false = 
                                                                                  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Abogado/a;Ocupación") == TRUE, 
                                                                                          true = "Lawyer", false = 
                                                                                            if_else(condition = str_detect(string = empty_df1$temp, pattern = "Conserje;Ocupación") == TRUE,  
                                                                                                    true = "Janitor", false = 
                                                                                                      if_else(condition = str_detect(string = empty_df1$temp, pattern = "Técnico en Enfermería;Ocupación") == TRUE,  
                                                                                                              true = "Nurse Technician", false = "Check")))))))))

### Social Class, Wave 1
empty_df1$class_c <-  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Obrero/a de la construcción;Ocupación") == TRUE, 
                              true = "Working Class", false = 
                                if_else(condition = str_detect(string = empty_df1$temp, pattern = "Médico/a;Ocupación") == TRUE, 
                                        true = "Upper Class", false = 
                                          if_else(condition = str_detect(string = empty_df1$temp, pattern = "Vendedor/a en retail;Ocupación") == TRUE, 
                                                  true = "Working Class", false = 
                                                    if_else(condition = str_detect(string = empty_df1$temp, pattern = "Contador/a;Ocupación") == TRUE, 
                                                            true = "Middle Class",false = 
                                                              if_else(condition = str_detect(string = empty_df1$temp, pattern = "Ingeniero/a civil en minas;Ocupación") == TRUE, 
                                                                      true = "Upper Class", false = 
                                                                        if_else(condition = str_detect(string = empty_df1$temp, pattern = "Profesor/a de colegio;Ocupación") == TRUE, 
                                                                                true = "Middle Class", false = 
                                                                                  if_else(condition = str_detect(string = empty_df1$temp, pattern = "Abogado/a;Ocupación") == TRUE, 
                                                                                          true = "Upper Class", false = 
                                                                                            if_else(condition = str_detect(string = empty_df1$temp, pattern = "Conserje;Ocupación") == TRUE,  
                                                                                                    true = "Working Class", false = 
                                                                                                      if_else(condition = str_detect(string = empty_df1$temp, pattern = "Técnico en Enfermería;Ocupación") == TRUE,  
                                                                                                              true = "Middle Class", false = "Check")))))))))

## Create Outcome (Vote Choice). Wave 1
empty_df1$deptemp <- paste0(empty_df1$vote,empty_df1$cand)
empty_df1$choice  <- if_else(condition = empty_df1$deptemp == "1A", true = 1, false = 
                               if_else(condition = empty_df1$deptemp == "1B", true = 0, false = 
                                         if_else(condition = empty_df1$deptemp == "2A", true = 0, false = 
                                                   if_else(condition = empty_df1$deptemp == "2B", true = 1, false = 2))))


## Create combination of attributes. Wave 3
empty_df2$temp <- paste(empty_df2$c1,empty_df2$c2,empty_df2$c3,empty_df2$c4,empty_df2$c5,
                        empty_df2$c6,empty_df2$c7,empty_df2$c8,empty_df2$c9,sep = ",")

### Sex, Wave 3
empty_df2$sex_c <-    if_else(condition = str_detect(string = empty_df2$temp, pattern = "Hombre;Sexo") == TRUE, 
                              true = "Male", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Mujer;Sexo") == TRUE, 
                                        true = "Female", false = "Check"))

### Age, Wave 3
empty_df2$age_c <-    if_else(condition = str_detect(string = empty_df2$temp, pattern = "25;Edad") == TRUE, 
                              true = "25 years", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "50;Edad") == TRUE, 
                                        true = "50 years", false = 
                                          if_else(condition = str_detect(string = empty_df2$temp, pattern = "70;Edad") == TRUE, 
                                                  true = "70 years", false = "Check")))

### Ideology, Wave 3
empty_df2$ideol_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Derecha;Ideología Política") == TRUE, 
                              true = "Right", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Centro-derecha;Ideología Política") == TRUE, 
                                        true = "Center-Right", false = 
                                          if_else(condition = str_detect(string = empty_df2$temp, pattern = "Centro-izquierda;Ideología Política") == TRUE, 
                                                  true = "Center-Left", false = 
                                                    if_else(condition = str_detect(string = empty_df2$temp, pattern = "Izquierda;Ideología Política") == TRUE, 
                                                            true = "Left", false = 
                                                              if_else(condition = str_detect(string = empty_df2$temp, pattern = "Centro;Ideología Política") == TRUE, 
                                                                      true = "Center", false = "Check")))))

### Political Experience, Wave 3
empty_df2$pol_exp_c <- if_else(condition = str_detect(string = empty_df2$temp, pattern = "Activista en organización de la sociedad civil;Experiencia Política") == TRUE, 
                               true = "Activist", false = 
                                 if_else(condition = str_detect(string = empty_df2$temp, pattern = "Diputado durante los últimos 4 años;Experiencia Política") == TRUE, 
                                         true = "Deputy", false = 
                                           if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ninguna;Experiencia Política") == TRUE, 
                                                   true = "None", false = "Check")))

### Party Membership, Wave 3
empty_df2$party_c <-   if_else(condition = str_detect(string = empty_df2$temp, pattern = "Milita en un partido político;Militancia") == TRUE, 
                               true = "Member of Party", false = 
                                 if_else(condition = str_detect(string = empty_df2$temp, pattern = "Independiente con el apoyo de partidos;Militancia") == TRUE, 
                                         true = "Independent supported by Party", false = 
                                           if_else(condition = str_detect(string = empty_df2$temp, pattern = "Independiente sin el apoyo de partidos;Militancia") == TRUE, 
                                                   true = "Independent", false = "Check")))

### Residence, Wave 3
empty_df2$resid_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vive en tu comuna;Residencia") == TRUE, 
                              true = "Same County", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vive en otra comuna;Residencia") == TRUE, 
                                        true = "Live in another County", false = "Check"))

### Religion, Wave 3
empty_df2$relig_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Católico/a;Religión") == TRUE, 
                              true = "Catholic", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Evangélico/a;Religión") == TRUE, 
                                        true = "Evangelical", false = 
                                          if_else(condition = str_detect(string = empty_df2$temp, pattern = "Irreligioso/a;Religión") == TRUE, 
                                                  true = "Irreligious", false = "Check")))

### Occupation, Wave 3
empty_df2$occup_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación") == TRUE, 
                              true = "Construction worker", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación") == TRUE, 
                                        true = "Physician", false = 
                                          if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación") == TRUE, 
                                                  true = "Salesperson", false = 
                                                    if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación") == TRUE, 
                                                            true = "Accountant",false = 
                                                              if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación") == TRUE, 
                                                                      true = "Civil Engineer in Mining", false = 
                                                                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación") == TRUE, 
                                                                                true = "Teacher", false = 
                                                                                  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación") == TRUE, 
                                                                                          true = "Lawyer", false = 
                                                                                            if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación") == TRUE,  
                                                                                                    true = "Janitor", false = 
                                                                                                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación") == TRUE,  
                                                                                                              true = "Nurse Technician", false = "Check")))))))))

### Father's Occupation, Wave 3
empty_df2$occupf_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación del Padre") == TRUE, 
                               true = "Construction worker", false = 
                                 if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación del Padre") == TRUE, 
                                         true = "Physician", false = 
                                           if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación del Padre") == TRUE, 
                                                   true = "Salesperson", false = 
                                                     if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación del Padre") == TRUE, 
                                                             true = "Accountant",false = 
                                                               if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación del Padre") == TRUE, 
                                                                       true = "Civil Engineer in Mining", false = 
                                                                         if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación del Padre") == TRUE, 
                                                                                 true = "Teacher", false = 
                                                                                   if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación del Padre") == TRUE, 
                                                                                           true = "Lawyer", false = 
                                                                                             if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación del Padre") == TRUE,  
                                                                                                     true = "Janitor", false = 
                                                                                                       if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación del Padre") == TRUE,  
                                                                                                               true = "Nurse Technician", false = "Check")))))))))

### Social Class, Wave 3
empty_df2$class_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación") == TRUE, 
                              true = "Working Class", false = 
                                if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación") == TRUE, 
                                        true = "Upper Class", false = 
                                          if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación") == TRUE, 
                                                  true = "Working Class", false = 
                                                    if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación") == TRUE, 
                                                            true = "Middle Class",false = 
                                                              if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación") == TRUE, 
                                                                      true = "Upper Class", false = 
                                                                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación") == TRUE, 
                                                                                true = "Middle Class", false = 
                                                                                  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación") == TRUE, 
                                                                                          true = "Upper Class", false = 
                                                                                            if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación") == TRUE,  
                                                                                                    true = "Working Class", false = 
                                                                                                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación") == TRUE,  
                                                                                                              true = "Middle Class", false = "Check")))))))))

### Father's Social Class, Wave 3
empty_df2$classf_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación del Padre") == TRUE, 
                               true = "Working Class", false = 
                                 if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación del Padre") == TRUE, 
                                         true = "Upper Class", false = 
                                           if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación del Padre") == TRUE, 
                                                   true = "Working Class", false = 
                                                     if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación del Padre") == TRUE, 
                                                             true = "Middle Class",false = 
                                                               if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación del Padre") == TRUE, 
                                                                       true = "Upper Class", false = 
                                                                         if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación del Padre") == TRUE, 
                                                                                 true = "Middle Class", false = 
                                                                                   if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación del Padre") == TRUE, 
                                                                                           true = "Upper Class", false = 
                                                                                             if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación del Padre") == TRUE,  
                                                                                                     true = "Working Class", false = 
                                                                                                       if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación del Padre") == TRUE,  
                                                                                                               true = "Middle Class", false = "Check")))))))))

### Social Class, Wave 3 (Re-check)
empty_df2$class_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación") == TRUE, 
                              true = "Working Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación") == TRUE, 
                              true = "Upper Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación") == TRUE, 
                              true = "Working Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación") == TRUE, 
                              true = "Middle Class",false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación") == TRUE, 
                              true = "Upper Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación") == TRUE, 
                              true = "Middle Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación") == TRUE, 
                              true = "Upper Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación") == TRUE,  
                              true = "Working Class", false = 
                      if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación") == TRUE,  
                              true = "Middle Class", false = "Check")))))))))

### Father's Social Class, Wave 3 (Re-check)
empty_df2$classf_c <-  if_else(condition = str_detect(string = empty_df2$temp, pattern = "Obrero/a de la construcción;Ocupación del Padre") == TRUE, 
                               true = "Working Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Médico/a;Ocupación del Padre") == TRUE, 
                                true = "Upper Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Vendedor/a en retail;Ocupación del Padre") == TRUE, 
                                true = "Working Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Contador/a;Ocupación del Padre") == TRUE, 
                                true = "Middle Class",false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Ingeniero/a civil en minas;Ocupación del Padre") == TRUE, 
                                true = "Upper Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Profesor/a de colegio;Ocupación del Padre") == TRUE, 
                                true = "Middle Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Abogado/a;Ocupación del Padre") == TRUE, 
                                true = "Upper Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Conserje;Ocupación del Padre") == TRUE,  
                                true = "Working Class", false = 
                        if_else(condition = str_detect(string = empty_df2$temp, pattern = "Técnico en Enfermería;Ocupación del Padre") == TRUE,  
                                true = "Middle Class", false = "Check")))))))))

## Create Outcome (Vote Choice). Wave 3
empty_df2$deptemp <- paste0(empty_df2$vote,empty_df2$cand)
empty_df2$choice  <- if_else(condition = empty_df2$deptemp == "1A", true = 1, false = 
                               if_else(condition = empty_df2$deptemp == "1B", true = 0, false = 
                                         if_else(condition = empty_df2$deptemp == "2A", true = 0, false = 
                                                   if_else(condition = empty_df2$deptemp == "2B", true = 1, false = 2))))


## Remove objects
rm(data_01a,data_01b,data_01c,data_01d,data_01e,data_01f,data_01g,data_01h,
   data_02a,data_02b,data_02c,data_02d,data_02e,data_02f,data_02g,data_02h,
   data_03a,data_03b,data_03c,data_03d,data_03e,data_03f,data_03g,data_03h,
   data_04a,data_04b,data_04c,data_04d,data_04e,data_04f,data_04g,data_04h,
   data_05a,data_05b,data_05c,data_05d,data_05e,data_05f,data_05g,data_05h,
   data_06a,data_06b,data_06c,data_06d,data_06e,data_06f,data_06g,data_06h,
   data_07a,data_07b,data_07c,data_07d,data_07e,data_07f,data_07g,data_07h,
   data_08a,data_08b,data_08c,data_08d,data_08e,data_08f,data_08g,data_08h,
   empty_list1,empty_list2,i,esp_01,esp_02,
   v01a,v01b,v01c,v01d,
   v02a,v02b,v02c,v02d,
   v03a,v03b,v03c,v03d,
   v04a,v04b,v04c,v04d,
   v05a,v05b,v05c,v05d,
   v06a,v06b,v06c,v06d,
   v07a,v07b,v07c,v07d,
   v08a,v08b,v08c,v08d)


## Select processed variables
empty_df1 %>% select(idperson,task,cand,sex_c:class_c,choice,eval)       -> basic_df1
empty_df2 %>% select(idperson,task,cand,sex_c:classf_c,choice,eval,repr) -> basic_df2


##########################################################################################################################################
#STEP 4: RESPONDENT COVARIATES AND DEPENDENT VARIABLES

## Select covariates (respondent level). Wave 1
wave_pre %>% mutate(temp1 = ifelse(grupo==1,1,0)) %>%
  select(folio, codigo_region:cod_rango_edad_panelista,e3,
         p14,p15_sq001,p16, p17, p17_other,p18a,p18b,p18c,p18d,p18e,
         p18_sq001,p18_sq002,p18_sq003,
         p19_sq001:p19_sq006,
         p21_sq001:p21_sq003,
         p22,p23,p24,p25,p28,
         c3,c4,c5_sq001,c5_sq002,c6,c7,c8,c8_other,c9,region,comuna,temp1
         ) %>%  
  ### Rename variables (harmonic with Wave 3)
  rename(idperson        = folio,                          group_const     = temp1, 
         sex_num         = codigo_sexo,                    sex_cat         = nombre_sexo,
         date_born       = fecha_nacimiento_panelista,     age             = edad_panelista, 
         range_age_num   = cod_rango_edad_panelista,       range_age_cat   = nombre_rango_edad_panelista,
         seg_supp_num    = codigo_gse_jefehogar,           seg_supp_cat    = nombre_gse_jefehogar, 
         occup_supp_num  = codigo_ocupacion_jefehogar,     occup_supp_cat  = nombre_ocupacion_jefehogar,
         educ_supp_num   = codigo_educacion_jefehogar,     educ_supp_cat   = nombre_educacion_jefehogar, 
         ideol_num       = codigo_identificacion_politica, ideol_cat       = nombre_identificacion_politica,
         zone_num        = codigo_zona,                    zone_cat        = nombre_zona, 
         region_num      = codigo_region,                  region_cat      = nombre_region,
         county_num      = codigo_comuna,                  county_cat      = nombre_comuna,
         attention1      = e3,                             attention2      = p28,
         pol_int         = p14,                            ideol_post      = p15_sq001,
         party_01        = p16,                            party_02        = p17,
         party_03        = p17_other,                      pol_know_01     = p18a,
         pol_know_02     = p18b,                           pol_know_03     = p18c,
         pol_know_04     = p18d,                           pol_know_05     = p18e,
         soc_trust_01    = p18_sq001,                      soc_trust_02    = p18_sq002,
         soc_trust_03    = p18_sq003,                      pol_trust_01    = p19_sq001,
         pol_trust_02    = p19_sq002,                      pol_trust_03    = p19_sq003,
         pol_trust_04    = p19_sq004,                      pol_trust_05    = p19_sq005,
         pol_trust_06    = p19_sq006,                      pol_eff_01      = p21_sq001, 
         pol_eff_02      = p21_sq002,                      pol_eff_03      = p21_sq003,
         sat_dem         = p22,                            pres_appr       = p23,
         corrup_01       = p24,                            corrup_02       = p25,  
         educ_resp       = c3,                             occup_resp      = c4,
         occup_d_01_resp = c5_sq001,                       occup_d_02_resp = c5_sq002,
         typ_occup_resp  = c6,                             inc_resp        = c7,
         relig_resp      = c8,                             relig_oth_resp  = c8_other,
         freq_rel_resp   = c9,                             region_resp     = region,
         county_resp     = comuna) %>%
  ### Remove irrelevant variables
  select(-registro,-nombre_panelista_cati) -> covariates_pre


## Select covariates (respondent level). Wave 3
wave_post %>% mutate(temp1 = ifelse(grupo==1,1,0)) %>%
  select(folio, codigo_region:cod_rango_edad_panelista,nacionalidad, vd1,
         p13,p14_1,p17a, p17b, p17c,p17d,p17e,p19_1,p19_2,p19_3,p19_4,
         p21_1,p21_2,p21_3,
         p22_1:p22_7,
         p24_1:p24_3,
         p26,p27,p28,
         c1,c2,c3,c4,c6,c7,c8,c9,c9_other,temp1
  ) %>%  
  ### Rename variables (harmonic with Wave 1)
  rename(idperson       = folio,                          group_dip       = temp1, 
         sex_num        = codigo_sexo,                    sex_cat         = nombre_sexo,
         date_born      = fecha_nacimiento_panelista,     age             = edad_panelista, 
         range_age_num  = cod_rango_edad_panelista,       range_age_cat   = nombre_rango_edad_panelista,
         seg_supp_num   = codigo_gse_jefehogar,           seg_supp_cat    = nombre_gse_jefehogar, 
         occup_supp_num = codigo_ocupacion_jefehogar,     occup_supp_cat  = nombre_ocupacion_jefehogar,
         educ_supp_num  = codigo_educacion_jefehogar,     educ_supp_cat   = nombre_educacion_jefehogar, 
         ideol_num      = codigo_identificacion_politica, ideol_cat       = nombre_identificacion_politica,
         zone_num       = codigo_zona,                    zone_cat        = nombre_zona, 
         region_num     = codigo_region,                  region_cat      = nombre_region,
         county_num     = codigo_comuna,                  county_cat      = nombre_comuna,
         nationality    = nacionalidad,                   attention1      = vd1,
         pol_int        = p13,                            ideol_post      = p14_1,
         pol_know_01    = p17a,                           pol_know_02     = p17b,       
         pol_know_03    = p17c,                           pol_know_04     = p17d,
         pol_know_05    = p17e,                           pol_eff_04      = p19_1,
         pol_eff_05     = p19_2,                          pol_eff_06      = p19_3,
         pol_eff_07     = p19_4,                          soc_trust_01    = p21_1,               
         soc_trust_02   = p21_2,                          soc_trust_03    = p21_3,            
         pol_trust_01   = p22_1,                          pol_trust_02    = p22_2,
         pol_trust_03   = p22_3,                          pol_trust_04    = p22_4,
         pol_trust_05   = p22_5,                          pol_trust_06    = p22_6,
         pol_trust_07   = p22_7,                          pol_eff_01      = p24_1, 
         pol_eff_02     = p24_2,                          pol_eff_03      = p24_3,
         sat_dem        = p26,                            corrup_01       = p27,                 
         corrup_02      = p28,                            sex_resp        = c1,
         age_resp       = c2,                             educ_resp       = c3,                      
         occup_resp     = c4,                             typ_occup_resp  = c6,
         inc_resp       = c7,                             household       = c8,
         civ_st_resp    = c9,                             civ_st_oth_resp = c9_other) %>%
  ### Remove irrelevant variables
  select(-registro,-nombre_panelista_cati) -> covariates_post


## Merge data.frames
conjoint_wave_01 <- left_join(x = basic_df1, y = covariates_pre,  by = "idperson")
conjoint_wave_03 <- left_join(x = basic_df2, y = covariates_post, by = "idperson")


## Save data.frame
write_rds(x = conjoint_wave_01, file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
write_rds(x = conjoint_wave_03, file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


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