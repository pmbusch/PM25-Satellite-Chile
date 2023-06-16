# Analysis on Exposure to PM2.5 and socioeconomic levels
# PBH June 2023

library(tidyverse)
library(chilemapas)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load data ----
df <- read.delim("Data/panelData.csv",sep=";")
socioeconomic <-  read.csv("Data/socioeconomic.csv")


# get average over 2017 year
df_socio <- df %>% 
  filter(year==2017) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pm25_exposure=mean(pm25_exposure),
            landTemp=mean(landTemp),
            pop75=mean(pop75)) %>% 
  left_join(socioeconomic) %>% mutate(codigo_comuna=as.character(codigo_comuna)) %>% 
  left_join(codigos_territoriales) %>% 
  filter(!is.na(income_mean))


# Figures -----

## all pairs
df_socio %>% 
  select_if(is.numeric) %>% 
  GGally::ggpairs()

# income
df_socio %>% 
  ggplot(aes(income_mean,pm25_exposure,size=pop75))+
  geom_point()

# high school
df_socio %>% 
  ggplot(aes(perc_less_highschool,pm25_exposure,size=pop75))+
  geom_point()

# wood burning for heat
df_socio %>% 
  ggplot(aes(perc_woodHeating,pm25_exposure,size=pop75))+
  geom_point()

df_socio %>% 
  ggplot(aes(perc_woodCooking,pm25_exposure,size=pop75))+
  geom_point()

df_socio %>% 
  ggplot(aes(perc_woodWarmWater,pm25_exposure,size=pop75))+
  geom_point()


# EoF