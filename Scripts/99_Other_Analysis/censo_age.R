# Analysis on age distribution among the 2017 census data


library(tidyverse)
library(readxl)

# censo <- read.csv2("C:/Users/pmbus/Downloads/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv")

censo <- read_excel("C:/Users/pmbus/Downloads/censo2017_edad.xlsx",sheet="Pasted")

censo$Casos %>% sum()
censo$Edad %>% unique()


censo <- censo %>% filter(!(Edad %in% paste0("",0:74)))

censo <- censo %>% 
  mutate(age=as.numeric(Edad),
         age=if_else(is.na(age),100,age))



# average age per commune on the 75+ group
censo %>% 
  mutate(popAge=age*Casos) %>% 
  group_by(Comuna) %>% 
  reframe(popAge=sum(popAge),pop=sum(Casos)) %>%
  mutate(age=popAge/pop) %>% arrange(desc(age)) %>% 
  skimr::skim()
  
head(censo)

# weighted median
library(spatstat)
censo %>% 
  group_by(Comuna) %>%
  dplyr::summarise(MedianAge = weighted.median(age, Casos),
                   .groups = 'drop') %>% 
  pull(MedianAge) %>% median()
  ggplot(aes(MedianAge))+geom_histogram()

