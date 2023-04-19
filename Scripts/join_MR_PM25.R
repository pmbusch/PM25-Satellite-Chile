## Join the PM25 exposure data to the mortality rates data
## PBH
## April 2023


library(tidyverse)
library(readxl)

# Load required data -----
# rate is per 1000 birhts
# mortality <- read.delim("Data/mortality_data.csv")
# mortality <- mortality %>% rename(year=Year,quarter=Quarter)

# 75+ years death data----
# death_75 <- read.delim("Data/chile_elderly_mortality_count_comuna_level_year_quarter.csv",sep=",")
death_75 <- read.delim("Data/chile_elderly_75+_all_cause_mortality_count_comuna_level_year_1990_2019_month.csv",
                       sep=",")
# cardio and respiratory causes
death_75_cdp <- read.delim("Data/chile_elderly_75+_circulatory_respiratory_mortality_count_comuna_level_year_1997_2019_month.csv",
                       sep=",") %>% rename(Mortality_Count_CDP=Mortality_Count)

# join causes of death
death_75 <- death_75 %>% full_join(death_75_cdp)
rm(death_75_cdp)
# a lot of NA due to the age range.
# death_75 %>% group_by(Year) %>% summarise(sum(is.na(Mortality_Count_CDP))/n()) %>% view() 

death_75 <- death_75 %>% rename(year=Year,
                                # quarter=Quarter,
                                month=Month,
                                sex=Gender,
                                codigo_comuna=CODIGO_COMUNA_RESIDENCIA)

death_75$Mortality_Count %>% sum()
death_75$Mortality_Count_CDP %>% sum(na.rm=T)

# Add sex inmediately - IDEA: duplicate DF with new "TOTAL" value to show in the group by

death_75_aux <- death_75
death_75_aux$sex <- "TOTAL"
death_75 <- rbind(death_75,death_75_aux)
rm(death_75_aux)

table(death_75$sex)

death_75 <- death_75 %>% group_by(codigo_comuna,sex,year,month) %>%
  summarise(Mortality_Count=sum(Mortality_Count,na.rm=T),
            Mortality_Count_CDP=sum(Mortality_Count_CDP,na.rm=T),)

# get population 75+ by year and commune
pop <- read_excel("Data/estimaciones-y-proyecciones-2002-2035-comunas.xlsx",
                  sheet="Est. y Proy. de Pob. Comunal")
# flat
pop <- pop %>% pivot_longer(c(-1:-8), names_to = "year", values_to = "pop") %>% 
  mutate(year=year %>% str_remove_all("Poblacion ") %>% as.numeric())

# same as before to get sex at the same moment
# first filter to reduce computational burden
names(pop)[7] <- "Sexo"
pop <- pop %>% 
  filter(Edad>74) %>% 
  mutate(sex=if_else(Sexo==1,"Hombre","Mujer"))

pop_aux <- pop
pop_aux$sex <- "TOTAL"
pop <- rbind(pop,pop_aux)
rm(pop_aux)
table(pop$sex)

pop_75 <- pop %>% 
  group_by(Comuna,sex,year) %>% summarise(pop75=sum(pop,na.rm=T)) %>% ungroup() %>% 
  rename(codigo_comuna=Comuna)
# rm(pop)


# library(ggforce)
# ggplot(pop_75,aes(pop75))+geom_histogram(bins=100)+
#   facet_zoom(xlim(0,500))

# join death and pop
death_75 <- death_75 %>% left_join(pop_75)

# death_75 %>% group_by(year) %>% summarise(sum(is.na(pop75)/n()))
# only starting from 2002 for now
death_75 <- death_75 %>% filter(year>2001)

# use only counties with at least 50 people in the age group
death_75 <- death_75 %>% filter(pop75>50)


death_75 <- death_75 %>% mutate(mortality=Mortality_Count/pop75*1000,
                                mortality_CDP=Mortality_Count_CDP/pop75*1000)

# pm25 pollution data exposure
pm25_exp <- read.delim("Data/pm25exposure_commune.csv",sep = ";")

# ALREADY AT COMMUNE LEVEL!
# group to region and quarters
# pm25_exp <- pm25_exp %>% 
#   mutate(quarter=ceiling(month/3) %>% as.integer()) %>% # quarters by months
#   # mutate(codigo_region=as.factor(codigo_region)) %>% 
#   mutate(pop_pm25=total_pop*pm25_exposure) %>% 
#   group_by(REGION,PROVINCIA,codigo_comuna,year,month) %>% 
#   summarise(pop_pm25=sum(pop_pm25,na.rm=T),
#             total_pop=sum(total_pop,na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(pm25_exposure=pop_pm25/total_pop) %>% 
#   mutate(pm25Exp_10ug=pm25_exposure/10)

# remove below 1 exposure
pm25_exp <- pm25_exp %>% filter(pm25_exposure>1)

# Join ----
# names(mortality)
names(pm25_exp)
names(death_75)

df <- death_75 %>% left_join(pm25_exp)
sum(is.na(df$pm25_exposure))

# df <- df %>% na.omit() # lost 20K, why? Commune with no rural or urban pop


df <- df %>% 
  mutate(quarter=ceiling(month/3) %>% as.integer()) # quarters by months

df <- df %>% mutate(pm25Exp_10ug=pm25_exposure/10) %>% 
  mutate(commune=as.factor(codigo_comuna))

# save results separetely, to avoid confusions
df_all <- df %>% filter(sex=="TOTAL")
df_all$sex <- NULL

write.table(df_all,"Data/panelData.csv",sep = ";",row.names = F)



df_sex <- df %>% filter(sex!="TOTAL")
df_sex$sex %>% table()

write.table(df_sex,"Data/panelData_Sex.csv",sep = ";",row.names = F)


# EoF