## Fixed effects Model
## PBH
## February 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich) # for robust and clustered standard errors

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load Panel Data ----
df <- read.delim("Data/panelData.csv",sep=";")


df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago


# Model ----
mod_base <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                     offset(log(pop75)), na.action=na.omit,
                   data = df) 
rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param=="pm25Exp_10ug")

mod_rm <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                     offset(log(pop75)), na.action=na.omit,
                   data = filter(df,REGION==13)) 
rr_rm <-  getModelInfo(mod_rm,"RM",data_df =  filter(df,REGION==13)) %>% 
  filter(param=="pm25Exp_10ug")


# Estimate avoided deaths in the whole period -----
limit <- 12
# limit <- 20

## Estimate annual proportional reduction needed per commune and year -----
red_needed <- df %>% 
  group_by(year,commune) %>% 
  reframe(pm25=mean(pm25_exposure)) %>% ungroup() %>% 
  mutate(red=if_else(pm25<limit,0,(pm25-limit)/pm25)) %>%  # reduction in percentage
  dplyr::select(-pm25)

## Apply Reduction -----

df_avoided <- df %>% left_join(red_needed) %>% 
  mutate(new_pm25=pm25_exposure*(1-red))

# check
df_avoided %>% 
  group_by(year,commune) %>% 
  reframe(pm25=mean(new_pm25),
          old=mean(pm25_exposure)) %>% arrange(desc(pm25))

## Avoided deaths -----

# rr to use

# rr_all <- rr_base
rr_all <- rr_rm
(rr <- rr_all$rr/100)
(rr <- rr_all$rr_low/100) 
(rr <- rr_all$rr_high/100)

df_avoided %>% ungroup() %>% 
  filter(REGION==13) %>% 
  dplyr::select(-codigo_comuna,-REGION,-PROVINCIA,
                -quarter,-commune,-year,-month,-total_pop) %>% 
  mutate(reduction=(pm25_exposure-new_pm25)/10) %>% # get reduction in terms of 10 ug/m2
  mutate(red_perc_mortality=rr*reduction) %>% # get relative decrease in mortality rate
  mutate(new_MR=MR_all_cause*(1-red_perc_mortality),
         new_death_counts=pop75*new_MR/1000,
         avoided_deaths=death_count_all_cause-new_death_counts) %>% 
  # head() %>% 
  pull(avoided_deaths) %>% sum()/18/12 # per month



sum(df$death_count_all_cause)

