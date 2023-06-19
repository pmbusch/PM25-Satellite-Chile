## Regressions at Commune Level
## PBH
## June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)


theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")

# Change some parameters
df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

# Loop --------
# Communes
communes <- df$commune %>% unique()


all_mods <- data.frame()

# one model for each commune
for (x in communes){
  # print(x)
  mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+
                    offset(log(pop75)), na.action=na.omit,
                data = filter(df,commune==x)) # filter by commune
  
  coef <- summary(mod)$coefficients
  
  # all
  out <- data.frame(commune=x,param=rownames(coef),est=coef[,1],se=coef[,2],N=nobs(mod),
                    bic=BIC(mod),
                    pop75=mean(filter(df,commune==x)$pop75))

  out <- out %>% 
      mutate(rr=exp(est)*100-100,
             rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
             rr_high=exp(est+1.96*se)*100-100)  
  
  all_mods <- rbind(all_mods,out)
  rm(out)
  
}

# Figure ------
all_mods %>% 
  filter(param=="pm25Exp_10ug") %>%
  ggplot(aes(rr))+
  geom_histogram()

all_mods_pm25 <- all_mods %>% 
  filter(param=="pm25Exp_10ug")
# Median
weighted.mean(all_mods_pm25$rr,all_mods_pm25$pop75)
matrixStats::weightedMedian(all_mods_pm25$rr,all_mods_pm25$pop75)

# Share bigger than 1
all_mods_pm25 %>% filter(rr>0) %>% nrow()/327 # 73%

# Share of significant
all_mods_pm25 %>% 
  filter(sign(rr_low)==sign(rr_high)) %>% 
  filter(rr>0) %>%
  nrow()/327 # 12%

