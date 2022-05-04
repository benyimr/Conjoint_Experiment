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
library(FindIt)
library(ggpubr)
library(ggthemes)
library(MetBrewer)
library(sjmisc)
library(tidyverse)
library(xtable)


##########################################################################################################################################
#STEP 1: LOADING DATASET

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


##########################################################################################################################################
#STEP 2: DESCRIPTIVE STATISTICS

### Create function for conjoint table
resume <- function(data, var,digits,label,div){
  
  #### Step 1: Select Variable
  data %>% select(all_of(var)) -> df
  
  #### Step 2: Create Tables
  t1 <- table(df)/div
  t2 <- round(prop.table(table(df)/div)*100,digits)

  #### Step 3: Transform into Data.frame  
  t1a <- as.data.frame(t1)
  t2a <- as.data.frame(t2)
  
  #### Step 4: Merge information
  t3 <- full_join(x = t1a, y = t2a, by = "df")
  
  #### Step 5: Final format
  t3 %>% mutate(lab = label) %>%
    select(lab,df,Freq.x,Freq.y) %>%
    rename(Attribute = lab, Levels = df,
           Frequency = Freq.x,
           Percentage = Freq.y)-> t3
  
  #### Step 6: Define output
  return(t3)
}

### Create vector of variable names
conjoint_01 %>% select(ends_with("_c")) %>% names() -> vctr_01

### Create labels
vctr_02 <- c("Sex","Age","Ideology","Political Experience","Party Membership",
             "Residence","Religion","Occupation","Class")

### Create empty objects to store results
temp_01 <- list()
temp_02 <- list()

### For loopp to calculate descriptive statistics
for(i in 1:9){
  
  temp_01[[i]] <- resume(data = conjoint_01, var = vctr_01[i], digits = 3, label = vctr_02[i], div = 1)
  temp_02[[i]] <- resume(data = conjoint_03, var = vctr_01[i], digits = 3, label = vctr_02[i], div = 1)
}

### Transform into a LaTeX table
xtable::xtable(do.call("rbind.data.frame", temp_01))
xtable::xtable(do.call("rbind.data.frame", temp_02))


### Descriptive Statistics (DV: Vote Choice)
resume(data = conjoint_01, var = "choice", digits = 3, label = "Vote", div = 1)
resume(data = conjoint_03, var = "choice", digits = 3, label = "Vote", div = 1)

### Descriptive Statistics (DV: Compeptence and Representation)
bind_rows(
descr(conjoint_01$eval),
descr(conjoint_03$eval),
descr(conjoint_03$repr)
) %>% mutate(wave = c("Wave 1","Wave 3", "Wave 3"),
             label = c("Competence","Competence","Representation")) %>%
  select(wave,label,mean:md,range:skew) %>% xtable()


### Descriptive Statistics (Respondent Characteristics)

### Recode variables
conjoint_01 %>%
  mutate(seg_supp_cat1 = car::recode(seg_supp_cat,"'C1'='AB-C1';c('D','E')='D-E' "),
         occup_cat1    = car::recode(occup_supp_num,"c(1,2)='Obrero No Calificado';
                                     c(3)='Obrero Calificado';c(4)='Administrativo medio y bajo';
                                     c(5,6)='Ejecutivo medio y alto';c(0,7,8,9,10)='Inactivo'"),
         educ_cat1     = car::recode(educ_supp_num,"c(1,2,3)='Básica';c(4,5)='Media';
                                     c(6,7)='Técnica';c(8,9,10)='Superior'"),
         zone_cat1     = str_to_title(zone_cat),
         ideol_cat1    = car::recode(ideol_cat, "'NS-NR'='Ninguna-Independiente'")) -> conjoint_01
   
conjoint_03 %>%
  mutate(seg_supp_cat1 = car::recode(seg_supp_cat,"'C1'='AB-C1';c('D','E')='D-E' "),
         occup_cat1    = car::recode(occup_supp_num,"c(1,2)='Obrero No Calificado';
                                     c(3)='Obrero Calificado';c(4)='Administrativo medio y bajo';
                                     c(5,6)='Ejecutivo medio y alto';c(0,7,8,9,10)='Inactivo'"),
         educ_cat1     = car::recode(educ_supp_num,"c(1,2,3)='Básica';c(4,5)='Media';
                                     c(6,7)='Técnica';c(8,9,10)='Superior'"),
         zone_cat1     = str_to_title(zone_cat),
         ideol_cat1    = car::recode(ideol_cat, "'NS-NR'='Ninguna-Independiente'")) -> conjoint_03



### Create vector of variable names
vctr_03 <- c("sex_cat","range_age_cat","seg_supp_cat1","occup_cat1","educ_cat1","zone_cat1","ideol_cat1")

### Create labels
vctr_04 <- c("Sex","Age","Socio-Economic Level","Occupation","Education", "Zone","Ideology")


### Create empty objects to store results
temp_03 <- list()
temp_04 <- list()

### For loopp to calculate descriptive statistics
for(i in 1:7){
  
  temp_03[[i]] <- resume(data = conjoint_01, var = vctr_03[i], digits = 3, label = vctr_04[i], div = 16)
  temp_04[[i]] <- resume(data = conjoint_03, var = vctr_03[i], digits = 3, label = vctr_04[i], div = 16)
}

### Transform into a LaTeX table
xtable::xtable(do.call("rbind.data.frame", temp_03))
xtable::xtable(do.call("rbind.data.frame", temp_03))

### Remove objects
rm(list = ls())

##########################################################################################################################################
#STEP 3: FIGURES NEGATIVE BIAS

### Load the results
wave_01 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01_corr.rds")
wave_03 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03_corr.rds")

wave_01f <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_01_corr.rds")
wave_03f <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_03_corr.rds")


## FIGURE 1: SOCIAL CLASS

#### Figure 1, Subplot A
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Social Class",
                   outcome == "choice", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1a

#### Figure 1, Subplot B
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Social Class",
                   outcome == "choice", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1b 

#### Figure 1, Subplot C
wave_01f %>% filter(type == "Wave 1, Unweighted, Class", 
                    sample == "Full, Comb, Attention 1+2",
                    attribute == "attrclass") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1c 



#### Figure 1, Subplot D
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "repr", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-5,5) + 
  labs(x = "Attribute", y = "Level of Representation", title = "(D) Representation (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1d

#### Figure 1, Subplot E
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "repr", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 43), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(30,50) +
  labs(x = "", y = "Level of Representation", title = "(E) Representation (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1e

#### Figure 1, Subplot F
wave_03f %>% filter(type == "Wave 3, Unweighted, Class", 
                    sample == "Full, Comb, Attention",
                    attribute == "attrclass") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = "Level of Representation", title = "(F) Representation (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1f


### Create Figure 1
annotate_figure(
  ggarrange(fig_1a,fig_1b,fig_1c, fig_1d, fig_1e, fig_1f, 
            ncol = 3, nrow = 2, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Social Class (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## FIGURE 1.A: OCCUPPATION

#### Figure 1.A, Subplot A
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Occupation",
                   outcome == "choice", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician", "Teacher", "Accountant", 
                             "Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1a_esp

#### Figure 1.A, Subplot B
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Occupation",
                   outcome == "choice", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician", "Teacher", "Accountant", 
                             "Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1b_esp 

#### Figure 1.A, Subplot C
wave_01f %>% filter(type == "Wave 1, Unweighted, Occupation", 
                    sample == "Full, Comb, Attention 1+2",
                    attribute == "attroccup") %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician", "Teacher", "Accountant", 
                             "Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1c_esp 

#### Figure 1.A, Subplot D
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "repr", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-10,10) + 
  labs(x = "Attribute", y = "Level of Representation", title = "(D) Representation (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1d_esp

#### Figure 1.A, Subplot E
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "repr", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 43), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(30,50) +
  labs(x = "", y = "Level of Representation", title = "(E) Representation (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1e_esp

#### Figure 1.A, Subplot F
wave_03f %>% filter(type == "Wave 3, Unweighted, Occupation", 
                    sample == "Full, Comb, Attention",
                    attribute == "attroccup") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = "Level of Representation", title = "(F) Representation (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_1f_esp

### Create Figure 1.A
annotate_figure(
  ggarrange(fig_1a_esp,fig_1b_esp,fig_1c_esp, fig_1d_esp, fig_1e_esp, fig_1f_esp, 
            ncol = 3, nrow = 2, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Previous Occupation (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_01A.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## FIGURE 2: SOCIAL CLASS

#### Figure 2, Subplot A
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "choice", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2a

#### Figure 2, Subplot B
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "choice", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2b 

#### Figure 2, Subplot C
wave_03f %>% filter(type == "Wave 3, Unweighted, Class", 
                    sample == "Full, Comb, Attention",
                    attribute == "attrclass") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        #axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2c 


### Create Figure 2
annotate_figure(
  ggarrange(fig_2a,fig_2b,fig_2c, 
            ncol = 3, nrow = 1, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Social Class (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

## FIGURE 2.A: OCCUPATION

#### Figure 2.A, Subplot A
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "choice", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2a_esp

#### Figure 2.A, Subplot B
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "choice", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2b_esp 

#### Figure 2.A, Subplot C
wave_03f %>% filter(type == "Wave 3, Unweighted, Occupation", 
                    sample == "Full, Comb, Attention",
                    attribute == "attroccup") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2c_esp 


### Create Figure 2
annotate_figure(
  ggarrange(fig_2a_esp,fig_2b_esp,fig_2c_esp, 
            ncol = 3, nrow = 1, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Previous Occupation (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_02A.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

### Remove objects
rm(list = ls())


##########################################################################################################################################
#STEP 4: FIGURES PERSISTENCE OF BIAS

### Load the results
imce_bart_h <- read_rds(file = "0C_Objects/IMCE_Bart_Honest.rds")
imce_cf_h   <- read_rds(file = "0C_Objects/IMCE_CF_Honest.rds")
imce_xl_h   <- read_rds(file = "0C_Objects/IMCE_XL_CF_Honest.rds")

### FIGURE 3: INTER-WAVE CORRELATION

### Correlation tests
cor.test(imce_cf_h$imce.x, imce_cf_h$imce.y)

### Figure 3
imce_cf_h %>%
  ggplot() + 
  geom_point(mapping = aes(x = imce.x, y = imce.y), col = "gray64") + 
  geom_smooth(mapping = aes(x = imce.x, y = imce.y), method = "lm", se = FALSE, col = "red") + 
  geom_smooth(mapping = aes(x = imce.x, y = imce.y), method = "loess", se = FALSE, col = "blue") + 
  geom_text(x=-0.24, y=-0.027, label="N = 818") + 
  geom_text(x=-0.24, y=-0.033, label= expression(hat(rho[13])~"=0.55") ) + 
  theme_few() +
  labs(x = expression(hat(tau[1])),
       y = expression(hat(tau[3])),
       caption = "Source: Muñoz, Barsted, Somma and Staniak (2021).",
       title = "Inter-Wave Correlation between Individual Marginal Component Effect of Working Class.",
       subtitle = "Algorithm: Causal Forests. Honest Estimates. Red line (OLS) and blue line (LOESS)."
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"))
ggsave(filename = "Figure_03.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


### FIGURE 3.A: INTER-WAVE CORRELATION (ALTERNATIVE ALGORITHMS)

### Correlation tests
cor.test(imce_bart_h$wk_rfmd_w1, imce_bart_h$wk_rfmd_w3)
cor.test(imce_xl_h$imce.x, imce_xl_h$imce.y)

### Figure 3, Subplot A
bind_cols(
  data.frame(var1 = as.factor(c(rep("Working Class",2),rep("Middle Class",2),rep("Upper Class",2))),
             var2 = as.factor(c("Reference: Middle","Reference: Upper",
                                "Reference: Working","Reference: Upper",
                                "Reference: Working","Reference: Middle")),
             col = c("1","1","0","0","0","0")),
  bind_rows(
    cor.test(imce_bart_h$wk_rfmd_w1, imce_bart_h$wk_rfmd_w3) %>% broom::tidy(),
    cor.test(imce_bart_h$wk_rfup_w1, imce_bart_h$wk_rfup_w3) %>% broom::tidy(),  
    cor.test(imce_bart_h$md_rfwk_w1, imce_bart_h$md_rfwk_w3) %>% broom::tidy(),
    cor.test(imce_bart_h$md_rfup_w1, imce_bart_h$md_rfup_w3) %>% broom::tidy(),
    cor.test(imce_bart_h$up_rfwk_w1, imce_bart_h$up_rfwk_w3) %>% broom::tidy(),
    cor.test(imce_bart_h$up_rfmd_w1, imce_bart_h$up_rfmd_w3) %>% broom::tidy())) %>%
  select(var1:estimate,p.value,conf.low,conf.high) %>%
  mutate(var1 = fct_relevel(var1, "Working Class", "Middle Class","Upper Class"),
         var2 = fct_relevel(var2, "Reference: Working", "Reference: Middle", 
                            "Reference: Upper")) %>%
  ggplot() + 
  geom_point(mapping = aes(x = var1 , y = estimate, col = col), size = 3) +
  geom_linerange(mapping = aes(x = var1, ymin = conf.low, ymax = conf.high, col = col), size = 1.5) +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  facet_wrap(~var2, ncol = 1) + ylim(0,0.6) +
  labs(x = "Candidate's Social Class", 
       y = expression(hat(rho)), 
       title = "(A) Bayesian Additive Regression Trees.") + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_3a_esp

### Figure 3, Subplot B
imce_xl_h %>%
  ggplot() + 
  geom_point(mapping = aes(x = imce.x, y = imce.y), col = "gray64") + 
  geom_smooth(mapping = aes(x = imce.x, y = imce.y), method = "lm", se = FALSE, col = "red") + 
  geom_smooth(mapping = aes(x = imce.x, y = imce.y), method = "loess", se = FALSE, col = "blue") + 
  geom_text(x=-0.24, y=-0.027, label="N = 818") + 
  geom_text(x=-0.24, y=-0.033, label= expression(hat(rho[13])~"=0.55") ) + 
  theme_few() +
  labs(x = expression(hat(tau[1])),
       y = expression(hat(tau[3])),
       title = "(B) X-Learner."
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_3b_esp

### Create Figure 3.A
annotate_figure(
  ggarrange(fig_3a_esp,fig_3b_esp, 
            ncol = 2, nrow = 1),
  top = text_grob("Inter-Wave Correlation between Individual Marginal Component Effect of Working Class.",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_03A.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

### Remove objects
rm(list=ls())

##########################################################################################################################################
#STEP 5: INDEPENDENCE OF BIAS

## Importing pre-processed datasets
conjoint_01 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_01.rds")
conjoint_03 <- read_rds(file = "0A_Datasets/4_Panel/Conjoint_Candidates_Chile_Wave_03.rds")


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


## Estimate Basic Models

### Wave: 1, Sample: Full, DV: Vote Choice, IV: Social Class
model_01 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_01_resp, nway = 2,
                        pair.id = conjoint_01_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_01_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 1, Sample: Full, DV: Evaluation of Competence, IV: Social Class
model_02 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_01_resp, nway = 2,
                        pair.id = conjoint_01_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_01_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
model_03 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_01_filtered, nway = 2,
                        pair.id = conjoint_01_filtered$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_01_filtered$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 1, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class
model_04 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_01_filtered, nway = 2,
                        pair.id = conjoint_01_filtered$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_01_filtered$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Full, DV: Vote Choice, IV: Social Class
model_05 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_resp, nway = 2,
                        pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_03_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Full, DV: Evaluation of Competence, IV: Social Class
model_06 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_resp, nway = 2,
                        pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Full, DV: Perception of Representativeness, IV: Social Class
model_07 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_resp, nway = 2,
                        pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
model_08 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_filtered, nway = 2,
                        pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_03_filtered$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class
model_09 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_filtered, nway = 2,
                        pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_filtered$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Perception of Representativeness, IV: Social Class
model_10 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_filtered, nway = 2,
                        pair.id = conjoint_03_filtered$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_filtered$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)


## Function
model_clean <- function(model){
  
  ### Extract AMIEs
  temp <- summary(model)$AMIE2
  
  ### Process the AMIEs
  temp %>% 
    rename(stderr = `Std.Err`) %>%
    mutate(lwr = round(AMIE - stderr*qt(p = (1- 0.05/(2*7)), df = dim(model$data)[1] - length(unlist(model$coefs))),4),
           upr = round(AMIE + stderr*qt(p = (1- 0.05/(2*7)), df = dim(model$data)[1] - length(unlist(model$coefs))),4),
           check = ifelse(0>= lwr & 0 <= upr, 0, 1))  %>%
    select(Level1,Level2, base,AMIE,stderr,check) %>%
    remove_rownames() %>%  
    mutate(reference = ifelse(base=="***",1,0)) %>%
    select(-base) -> temp1
  
  ### Creeate values
  temp1 %>% 
    mutate(AMIE_lab = ifelse(check == 0 & reference == 0,yes = "", no =
                               ifelse(check == 0 & reference == 1,
                                      yes = "R", no = AMIE)),
           AMIE_fill = ifelse(reference == 1, yes = NA, no = AMIE) 
                   ) -> temp2
  
  ### Define output
  return(temp2)
}

#dim(summary_01)[1]-7)

## Extract AMIEs
summary_01 <- model_clean(model_01) # Wave: 1, Sample: Full,     DV: choice       
summary_02 <- model_clean(model_02) # Wave: 1, Sample: Full,     DV: eval       
summary_03 <- model_clean(model_03) # Wave: 1, Sample: Filtered, DV: choice  ****          
summary_04 <- model_clean(model_04) # Wave: 1, Sample: Filtered, DV: eval    ****                 
summary_05 <- model_clean(model_05) # Wave: 3, Sample: Full,     DV: choice       
summary_06 <- model_clean(model_06) # Wave: 3, Sample: Full,     DV: eval       
summary_07 <- model_clean(model_07) # Wave: 3, Sample: Full,     DV: repr       
summary_08 <- model_clean(model_08) # Wave: 3, Sample: Filtered, DV: choice  ****           
summary_09 <- model_clean(model_09) # Wave: 3, Sample: Filtered, DV: eval    ****     
summary_10 <- model_clean(model_10) # Wave: 3, Sample: Filtered, DV: repr    ****


summary_05 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_05
summary_06 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_06
summary_07 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_07
summary_08 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_08
summary_09 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_09
summary_10 %>% mutate(Level1 = car::recode(Level1,"'Deputy'='Elected Official'")) -> summary_10


summary_01 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_1oc")) -> summary_01
summary_02 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_1oe")) -> summary_02
summary_03 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_1fc")) -> summary_03
summary_04 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_1fe")) -> summary_04
summary_05 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3oc")) -> summary_05
summary_06 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3oe")) -> summary_06
summary_07 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3or")) -> summary_07
summary_08 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3fc")) -> summary_08
summary_09 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3fe")) -> summary_09
summary_10 %>% rename_at(vars(-Level1,-Level2), ~ paste0(., "_3fr")) -> summary_10


temp1 <- left_join(x = summary_01, y = summary_02, by = c("Level1","Level2"))
temp2 <- left_join(x = temp1,      y = summary_03, by = c("Level1","Level2"))
temp3 <- left_join(x = temp2,      y = summary_04, by = c("Level1","Level2"))
temp4 <- left_join(x = temp3,      y = summary_05, by = c("Level1","Level2"))
temp5 <- left_join(x = temp4,      y = summary_06, by = c("Level1","Level2"))
temp6 <- left_join(x = temp5,      y = summary_07, by = c("Level1","Level2"))
temp7 <- left_join(x = temp6,      y = summary_08, by = c("Level1","Level2"))
temp8 <- left_join(x = temp7,      y = summary_09, by = c("Level1","Level2"))
AMIEs <- left_join(x = temp8,      y = summary_10, by = c("Level1","Level2"))

rm(model_01, model_02, model_03, model_04, model_05, model_06, model_07, model_08, model_09, model_10,
   summary_01, summary_02, summary_03, summary_04, summary_05, summary_06, summary_07, summary_08, 
   summary_09, summary_10, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8) 


# Wave: 1, Sample: Full,     DV: choice 
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_1oc), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_1oc), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 1, Sample: Full,     DV: eval
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_1oe), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_1oe), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 1, Sample: Filtered, DV: choice
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_1fc), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_1fc), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 1, Sample: Filtered, DV: eval
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_1fe), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_1fe), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Full,     DV: choice
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3oc), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3oc), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Full,     DV: eval
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3oe), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3oe), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Full,     DV: repr
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3or), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3or), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Filtered, DV: choice    
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3fc), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3fc), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Filtered, DV: eval     
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3fe), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3fe), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


# Wave: 3, Sample: Filtered, DV: repr
ggplot(data = AMIEs) + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_3fr), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab_3fr), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(AMIEs$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-3,3)) +
  labs(x = "Moderator", y = "Treatment", fill = "AMIE") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) 


## Sample Splitting

### Wave: 1, Sample: Full
set.seed(17806)
conjoint_01_resp_id_train <- sample(unique(conjoint_01_resp$idperson), length(table(conjoint_01_resp$idperson))/2, replace=FALSE)
conjoint_01_resp_id_test  <- setdiff(unique(conjoint_01_resp$idperson), conjoint_01_resp_id_train)
conjoint_01_resp_train     <- conjoint_01_resp[is.element(conjoint_01_resp$idperson,conjoint_01_resp_id_train), ]
conjoint_01_resp_test      <- conjoint_01_resp[is.element(conjoint_01_resp$idperson,conjoint_01_resp_id_test), ]

### Wave: 1, Sample: Filtered
set.seed(17806)
conjoint_01_filt_id_train  <- sample(unique(conjoint_01_filtered$idperson), length(table(conjoint_01_filtered$idperson))/2, replace=FALSE)
conjoint_01_filt_id_test   <- setdiff(unique(conjoint_01_filtered$idperson), conjoint_01_filt_id_train)
conjoint_01_filtered_train <- conjoint_01_filtered[is.element(conjoint_01_filtered$idperson,conjoint_01_filt_id_train), ]
conjoint_01_filtered_test  <- conjoint_01_filtered[is.element(conjoint_01_filtered$idperson,conjoint_01_filt_id_test), ]

### Wave: 3, Sample: Full
set.seed(17806)
conjoint_03_resp_id_train <- sample(unique(conjoint_03_resp$idperson), length(table(conjoint_03_resp$idperson))/2, replace=FALSE)
conjoint_03_resp_id_test  <- setdiff(unique(conjoint_03_resp$idperson), conjoint_03_resp_id_train)
conjoint_03_resp_train    <- conjoint_03_resp[is.element(conjoint_03_resp$idperson,conjoint_03_resp_id_train), ]
conjoint_03_resp_test     <- conjoint_03_resp[is.element(conjoint_03_resp$idperson,conjoint_03_resp_id_test), ]

### Wave: 3, Sample: Filtered
set.seed(17806)
conjoint_03_filt_id_train  <- sample(unique(conjoint_03_filtered$idperson), length(table(conjoint_03_filtered$idperson))/2, replace=FALSE)
conjoint_03_filt_id_test   <- setdiff(unique(conjoint_03_filtered$idperson), conjoint_03_filt_id_train)
conjoint_03_filtered_train <- conjoint_03_filtered[is.element(conjoint_03_filtered$idperson,conjoint_03_filt_id_train), ]
conjoint_03_filtered_test  <- conjoint_03_filtered[is.element(conjoint_03_filtered$idperson,conjoint_03_filt_id_test), ]


## Basic Model
model_00 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_class:attr_sex + attr_class:attr_age + 
                          attr_class:attr_ideol + attr_class:attr_pol_exp +
                          attr_class:attr_pty_mem + attr_class:attr_res + attr_class:attr_rel, 
                        int3.formula = NULL,data = conjoint_01_resp_test, nway = 2,
                        pair.id = conjoint_01_resp_test$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_01_resp_test$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 1, Sample: Filtered, DV: Vote Choice, IV: Social Class
model_01 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_sex:attr_res + attr_pty_mem:attr_res + 
                          attr_pty_mem:attr_rel + attr_pol_exp:attr_class, 
                        int3.formula = NULL,data = conjoint_01_resp_test, nway = 2,
                        pair.id = conjoint_01_resp_test$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_01_resp_test$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 1, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class
model_02 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_pty_mem:attr_rel + attr_pol_exp:attr_class, 
                        int3.formula = NULL,data = conjoint_01_resp_test, nway = 2,
                        pair.id = conjoint_01_resp_test$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_01_resp_test$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Vote Choice, IV: Social Class
model_03 <- CausalANOVA(formula = choice ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_sex:attr_res + attr_sex:attr_rel + 
                          attr_age:attr_res + attr_pol_exp:attr_class + attr_res:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_resp_test, nway = 2,
                        pair.id = conjoint_03_resp_test$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "binomial", cluster = conjoint_03_resp_test$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Evaluation of Competence, IV: Social Class
model_04 <- CausalANOVA(formula = eval ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_age:attr_pty_mem + attr_pol_exp:attr_class + 
                          attr_res:attr_rel, 
                        int3.formula = NULL,data = conjoint_03_resp_test, nway = 2,
                        pair.id = conjoint_03_resp_test$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_resp_test$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)

### Wave: 3, Sample: Filtered, DV: Perception of Representativeness, IV: Social Class
model_05 <- CausalANOVA(formula = repr ~ attr_sex + attr_age + attr_ideol + attr_pol_exp +
                          attr_pty_mem + attr_res + attr_rel + attr_class,
                        int2.formula = ~attr_sex:attr_res + attr_age:attr_pty_mem + 
                          attr_age:attr_res + attr_res:attr_rel + attr_pol_exp:attr_class,
                        int3.formula = NULL,data = conjoint_03_resp, nway = 2,
                        pair.id = conjoint_03_resp$task_alt, diff = TRUE,
                        screen = FALSE, collapse = FALSE,
                        family = "gaussian", cluster = conjoint_03_resp$idperson,
                        fac.level = c(2, 3, 5, 3, 3, 2, 3, 3),
                        ord.fac   = c(F, T, T, F, F, F, F, T), verbose = TRUE)



## Extract AMIEs
summary_00 <- model_clean(model_00)

## Fake Plot Subplot A
summary_00 %>% mutate(AMIE_esp = 0) %>% 
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_esp), col ="gray", lwd = 0.5) + 
  #geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "", fill = "AMIE",
       title = "(A) Vote Choice, Wave 1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_a

## Fake Plot Subplot B
summary_00 %>% mutate(AMIE_esp = 0) %>% 
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_esp), col ="gray", lwd = 0.5) + 
  #geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "", y = "Treatment", fill = "AMIE",
       title = "(B) Vote Choice, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_b

## Fake Plot Subplot C
summary_00 %>% mutate(AMIE_esp = 0) %>% 
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = AMIE_esp), col ="gray", lwd = 0.5) + 
  #geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-5,5)) +
  labs(x = "", y = "", fill = "AMIE",
       title = "(C) Representation, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_c


### Create Figure 4 Fake
annotate_figure(
  ggarrange(fig0_a,fig0_b,fig0_c, 
            ncol = 3, nrow = 1, widths = c(1.3,0.85,0.85),
            common.legend = TRUE, legend = "bottom"),
  top = text_grob("Causal Interactions with Social Class (K-Fold CV Group LASSO)",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_04Fake.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## Selection 01 Plot Subplot A
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official')='X';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "", fill = "AMIE",
       title = "(A) Vote Choice, Wave 1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_d

## Selection 01 Plot Subplot B
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official')='X';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "", y = "Treatment", fill = "AMIE",
       title = "(B) Vote Choice, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_e

## Selection 01 Plot Subplot C
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official')='X';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-5,5)) +
  labs(x = "", y = "", fill = "AMIE",
       title = "(C) Representation, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_f


### Create Figure 4 Selection 01
annotate_figure(
  ggarrange(fig0_d,fig0_e,fig0_f, 
            ncol = 3, nrow = 1, widths = c(1.3,0.85,0.85),
            common.legend = TRUE, legend = "bottom"),
  top = text_grob("Causal Interactions with Social Class (K-Fold CV Group LASSO)",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_04Selected_01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## Selection 02 Plot Subplot A
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official')='';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "Moderator", y = "", fill = "AMIE",
       title = "(A) Vote Choice, Wave 1") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_g

## Selection 02 Plot Subplot B
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official')='X';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-0.1,0.1)) +
  labs(x = "", y = "Treatment", fill = "AMIE",
       title = "(B) Vote Choice, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_h

## Selection 02 Plot Subplot C
summary_00 %>% mutate(AMIE_lab = car::recode(Level1, "c('None','Activist','Elected Official','Catholic',
                                             'Evangelical','Irreligious')='X';else=''")) %>%  
  ggplot() + 
  geom_tile(mapping = aes(x = Level1, y = Level2, fill = 0), col ="gray", lwd = 0.5) + 
  geom_text(mapping = aes(x = Level1, y = Level2, label = AMIE_lab), color = "grey40", size = 4) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(summary_00$Level1))) + 
  scale_y_discrete(limits = c("Working Class","Middle Class","Upper Class")) +
  theme_few() +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue", limits = c(-5,5)) +
  labs(x = "", y = "", fill = "AMIE",
       title = "(C) Representation, Wave 3") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face="bold"),
        legend.key.width=unit(1.5, "cm")) -> fig0_i


### Create Figure 4 Selection 02
annotate_figure(
  ggarrange(fig0_g,fig0_h,fig0_i, 
            ncol = 3, nrow = 1, widths = c(1.3,0.85,0.85),
            common.legend = TRUE, legend = "bottom"),
  top = text_grob("Causal Interactions with Social Class (K-Fold CV Group LASSO, Filtered Sample)",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_04Selected_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)



plot(model_03, type = "ConditionalEffect", fac.name = c("attr_class","attr_pol_exp"))
#plot(model_04, type = "ConditionalEffect", fac.name = c("attr_class","attr_pol_exp"))
plot(model_05, type = "ConditionalEffect", fac.name = c("attr_class","attr_pol_exp"))


ConditionalEffect(object = model_03, treat.fac = "attr_class", cond.fac = "attr_pol_exp", base.ind = 3) -> t1
ConditionalEffect(object = model_05, treat.fac = "attr_class", cond.fac = "attr_pol_exp", base.ind = 3) -> t2



bind_rows(
  t1$ConditionalEffects$`attr_pol_exp=None` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "None") %>% remove_rownames(),
  t1$ConditionalEffects$`attr_pol_exp=Activist` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "Activist") %>% remove_rownames(),
  t1$ConditionalEffects$`attr_pol_exp=Deputy` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "Deputy") %>% remove_rownames()) %>%
  
  mutate(rowname = fct_relevel(rowname, "Upper Class", "Middle Class","Working Class"),
         cond    = fct_relevel(cond, "None","Activist","Deputy"),
         col     = car::recode(rowname,"'Working Class'='1';else='0'")) %>% 
  
  ggplot() +
  geom_point(mapping = aes(x = rowname, y = ConditionalEffect, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = rowname, ymin = `2.5% CI`, ymax = `97.5% CI`, col = col), size = 1.5) + 
  geom_hline(mapping = aes(yintercept=0), linetype = "dashed", col = "gray64", size = 1) + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  facet_wrap(~cond, ncol = 1) + 
  labs(x = "Treatment", y = expression("Pr(Vote)"),
       title = "(A) Vote Choice, Wave 3") +
  ylim(-0.15,0.15) + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig4_a



bind_rows(
  t2$ConditionalEffects$`attr_pol_exp=None` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "None") %>% remove_rownames(),
  t2$ConditionalEffects$`attr_pol_exp=Activist` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "Activist") %>% remove_rownames(),
  t2$ConditionalEffects$`attr_pol_exp=Deputy` %>% 
    as.data.frame() %>% mutate(rownames_to_column(.), cond = "Deputy") %>% remove_rownames()) %>%
  
  mutate(rowname = fct_relevel(rowname, "Upper Class", "Middle Class","Working Class"),
         cond    = fct_relevel(cond, "None","Activist","Deputy"),
         col     = car::recode(rowname,"'Working Class'='1';else='0'")) %>%
  ggplot() +
  geom_point(mapping = aes(x = rowname, y = ConditionalEffect, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = rowname, ymin = `2.5% CI`, ymax = `97.5% CI`, col = col), size = 1.5) + 
  geom_hline(mapping = aes(yintercept=0), linetype = "dashed", col = "gray64", size = 1) + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  facet_wrap(~cond, ncol = 1) + 
  labs(x = "Treatment", y = expression("Level of Representation"),
       title = "(B) Representation, Wave 3") +
  ylim(-5,5) + 
  coord_flip() + 
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig4_b


### Create Figure 4 Selection 02
annotate_figure(
  ggarrange(fig4_a,fig4_b, 
            ncol = 2, nrow = 1,
            common.legend = TRUE, legend = "none"),
  top = text_grob("Conditional Effect of Social Class",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_04Results01.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

## Remove objects
rm(list=ls())

##########################################################################################################################################
#STEP 6: MECHANISM

### Load the results
wave_01 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_01_corr.rds")
wave_03 <- read_rds(file = "0C_Objects/AMCE_MM_Basic_Analysis_Wave_03_corr.rds")

wave_01f <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_01_corr.rds")
wave_03f <- read_rds(file = "0C_Objects/ACP_Intermediate_Analysis_Wave_03_corr.rds")


## FIGURE 5: SOCIAL CLASS

#### Figure 5, Subplot A
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Social Class",
                   outcome == "eval", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>%
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-8,3) + 
  labs(x = "Attribute", y = expression(Delta~"Evaluation"), title = "(A) Competence (AMCE), Wave 1 (May 2021)") +
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5a

#### Figure 5, Subplot B
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Social Class",
                   outcome == "eval", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate-48.09, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff-48.09, ymax = upper_bonff-48.09, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) +
  ylim(-6,6) +
  labs(x = "", y = expression(Delta~"Evaluation"), title = "(B) Competence (MM), Wave 1 (May 2021)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5b 


#### Figure 5, Subplot C
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "eval", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>%  
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) +
  ylim(-8,3) + 
  labs(x = "Attribute", y = "Level of Representation", title = "(C) Competence (AMCE), Wave 3 (November 2021)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5c

#### Figure 5, Subplot D
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "eval", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate-46.31, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff-46.31, ymax = upper_bonff-46.31, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-5,5) +
  labs(x = "", y = "Level of Representation", title = "(D) Competence (MM), Wave 3 (November 2021)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5d

### Create Figure 5
annotate_figure(
  ggarrange(fig_5a,fig_5b,fig_5c, fig_5d, 
            ncol = 2, nrow = 2),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_05.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## FIGURE 5.A: OCCUPPATION

#### Figure 5.A, Subplot A
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Occupation",
                   outcome == "eval", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician", "Teacher", "Accountant", 
                             "Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker')='1';else='0'")) %>%  
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) +
  ylim(-10,5) + 
  labs(x = "Attribute", y = expression(Delta~"Evaluation"), title = "(A) Competence (AMCE), Wave 1.") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5a_esp

#### Figure 5.A, Subplot B
wave_01 %>% filter(sample == "Full, Comb, Attention 1+2", treatment == "Occupation",
                   outcome == "eval", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician", "Teacher", "Accountant", 
                             "Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 48.09), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(38,60) + 
  labs(x = "", y = expression(Delta~"Evaluation"), title = "(B) Competence (MM), Wave 1.") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5b_esp 

#### Figure 5.A, Subplot C
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "eval", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-10,5) + 
  labs(x = "Attribute", y = expression(Delta~"Evaluation"), title = "(C) Competence (AMCE), Wave 3.") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5c_esp

#### Figure 5.A, Subplot D
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "eval", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 46.31), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(38,60) +
  labs(x = "", y = expression(Delta~"Evaluation"), title = "(d) Competence (MM), Wave 3.") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_5d_esp


### Create Figure 5.A
annotate_figure(
  ggarrange(fig_5a_esp,fig_5b_esp,fig_5c_esp, fig_5d_esp, 
            ncol = 2, nrow = 2),
  top = text_grob("Effect of Candidate's Previous Occupation (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_05A.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)


## FIGURE 2: SOCIAL CLASS

#### Figure 2, Subplot A
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "choice", statistic == "amce", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2a

#### Figure 2, Subplot B
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Social Class",
                   outcome == "choice", statistic == "mm", feature == "Social Class") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2b 

#### Figure 2, Subplot C
wave_03f %>% filter(type == "Wave 3, Unweighted, Class", 
                    sample == "Full, Comb, Attention",
                    attribute == "attrclass") %>% 
  mutate(level = fct_relevel(level, "Upper Class", "Middle Class","Working Class"),
         col  = ifelse(level == "Working Class","1","0")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2c 


### Create Figure 2
annotate_figure(
  ggarrange(fig_2a,fig_2b,fig_2c, 
            ncol = 3, nrow = 1, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Social Class (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_02.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

## FIGURE 2.A: OCCUPATION

#### Figure 2.A, Subplot A
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "choice", statistic == "amce", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") + 
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "Attribute", y = expression(Delta~"Pr(Vote)"), title = "(A) Vote Choice (AMCE)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2a_esp

#### Figure 2.A, Subplot B
wave_03 %>% filter(sample == "Full, Comb, Attention", treatment == "Occupation",
                   outcome == "choice", statistic == "mm", feature == "Occupation") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = estimate, col = col), size = 3) + 
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0.5), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(0.4,0.6) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(B) Vote Choice (MM)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2b_esp 

#### Figure 2.A, Subplot C
wave_03f %>% filter(type == "Wave 3, Unweighted, Occupation", 
                    sample == "Full, Comb, Attention",
                    attribute == "attroccup") %>% 
  mutate(level = car::recode(level, "'Janitor'='Concierge'")) %>% 
  mutate(level = fct_relevel(level, "Civil Engineer in Mining", "Physician","Lawyer",
                             "Teacher", "Accountant", "Nurse Technician",
                             "Concierge","Salesperson","Construction worker"),
         col  = car::recode(level,"c('Salesperson','Construction worker','Concierge')='1';else='0'")) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = level, y = Estimate, col = col), size = 2.5) +
  geom_linerange(mapping = aes(x = level, ymin = lower_bonff, ymax = upper_bonff, col = col), size = 1) + 
  geom_hline(mapping = aes(yintercept = 0), linetype = "longdash", col = "gray64") +
  scale_color_manual(values = met.brewer(name = "Egypt", n = 2, type = "discrete", direction = -1)) + 
  ylim(-0.2,0.2) + 
  labs(x = "", y = expression(Delta~"Pr(Vote)"), title = "(C) Vote Choice (ACP)") + 
  coord_flip() + 
  theme_few() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.y = element_blank(),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold")) -> fig_2c_esp 


### Create Figure 2
annotate_figure(
  ggarrange(fig_2a_esp,fig_2b_esp,fig_2c_esp, 
            ncol = 3, nrow = 1, widths = c(1,0.85,0.85)),
  top = text_grob("Effect of Candidate's Previous Occupation (95% CI, Clustered SE + Bonferroni Correction).",
                  color = "black", face = "bold.italic", 
                  size = 18,),
  bottom = text_grob("Source: Muñoz, Bargsted, Somma and Staniak (2021).", 
                     color = "black",
                     hjust = 1, x = 1, 
                     face = "italic", size = 10))
ggsave(filename = "Figure_02A.png", plot = last_plot(),
       device = "png", path = "0B_Figures/4_Conjoint/", 
       width = 45, height = 20, units = "cm", scale = 0.9)

### Remove objects
rm(list = ls())

