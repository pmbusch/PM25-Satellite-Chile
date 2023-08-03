## Join the PM25 exposure data to the mortality rates data
## PBH
## April 2023

library(tidyverse)
library(readxl)

# Load required data -----
# rate is per 1000 birhts
# mortality <- read.delim("Data/mortality_data.csv")
# mortality <- mortality %>% rename(year=Year,quarter=Quarter)

# 75+ or 65+ years death data----
# make sure to save the data with names
# all causes

death_75 <- read.delim(paste0("Data/chile_elderly_",
                              "75+",
                              # "65+",
                              "_all_cause_external_cardio_respiratory_mortality_count_commune_level_year_1990_2019_quarter_month.csv"),
                       sep=",")
death_75$Age <- NULL

names(death_75)
# all causes no CDP
death_75 <- death_75 %>% 
  mutate(Death_count_all_cause_NoCDP=Death_count_all_cause-
           ifelse(is.na(Death_count_cardioRespiratory), 0, Death_count_cardioRespiratory))


death_75 <- death_75 %>% rename(year=Year,
                                quarter=Quarter,
                                month=Month,
                                sex=Gender,
                                codigo_comuna=CODIGO_COMUNA_RESIDENCIA)

death_75 %>% 
  pivot_longer(c(6:11), names_to = "cause", values_to = "value") %>% 
  group_by(cause) %>% summarise(death_count=sum(value,na.rm = T))


# Add sex inmediately - IDEA: duplicate DF with new "TOTAL" value to show in the group by
death_75_aux <- death_75
death_75_aux$sex <- "TOTAL"
death_75 <- rbind(death_75,death_75_aux)
rm(death_75_aux)

table(death_75$sex)

death_75 <- death_75 %>%
  pivot_longer(c(6:11), names_to = "cause", values_to = "death_count") %>% 
  group_by(codigo_comuna,sex,year,month,cause) %>%
  summarise(death_count=sum(death_count,na.rm=T))

# add complete records, 0 when there is no death in that month
death_75 <- death_75 %>% ungroup() %>% 
  complete(codigo_comuna,sex,year, month, cause, fill = list(death_count = 0))

nrow(death_75)
length(unique(death_75$codigo_comuna))*length(unique(death_75$sex))* # 2242080 records
  length(unique(death_75$year))*length(unique(death_75$month))*length(unique(death_75$cause))


# get population 75+ by year and commune
pop <- read_excel("Data/estimaciones-y-proyecciones-2002-2035-comunas.xlsx",
                  sheet="Est. y Proy. de Pob. Comunal")
# flat
pop <- pop %>% pivot_longer(c(-1:-8), names_to = "year", values_to = "pop") %>% 
  mutate(year=year %>% str_remove_all("Poblacion ") %>% as.numeric())


# pop share above 75+ by region - 2019
pop %>% 
  filter(year==2019) %>% 
  mutate(above_75=if_else(Edad>74,"Above75","Below75")) %>% 
  group_by(Region,above_75) %>% 
  summarise(pop=sum(pop)) %>% ungroup() %>% 
  pivot_wider(names_from = above_75, values_from = pop) %>% 
  mutate(share_75=Above75/(Above75+Below75))

# commnune
pop %>% 
  filter(year==2019) %>% 
  mutate(above_75=if_else(Edad>74,"Above75","Below75")) %>% 
  group_by(Region,`Nombre Comuna`,above_75) %>% 
  summarise(pop=sum(pop)) %>% ungroup() %>% 
  pivot_wider(names_from = above_75, values_from = pop) %>% 
  mutate(share_75=Above75/(Above75+Below75)) %>% arrange(desc(share_75))


# same as before to get sex at the same moment
# first filter to reduce computational burden
names(pop)[7] <- "Sexo"
pop <- pop %>% 
  mutate(sex=if_else(Sexo==1,"Hombre","Mujer"))

pop_aux <- pop
pop_aux$sex <- "TOTAL"
pop <- rbind(pop,pop_aux)
rm(pop_aux)
table(pop$sex)

pop_75 <- pop %>% 
  filter(Edad>74) %>%
  # filter(Edad>64) %>% 
  group_by(Comuna,sex,year) %>% summarise(pop75=sum(pop,na.rm=T)) %>% ungroup() %>% 
  rename(codigo_comuna=Comuna)
# rm(pop)

# pop total
pop_total <- pop %>% group_by(Comuna,sex,year) %>% 
  summarise(pop=sum(pop,na.rm=T)) %>% ungroup() %>% 
  rename(codigo_comuna=Comuna)

pop_total %>% filter(year==2017,sex=="TOTAL") %>% pull(pop) %>% sum() 

# 75+ share
pop_75 <- pop_75 %>% left_join(pop_total) %>% 
  mutate(pop75_share=pop75/pop)


# library(ggforce)
# ggplot(pop_75,aes(pop75))+geom_histogram(bins=100)+
#   facet_zoom(xlim(0,500))

# join death and pop
death_75 <- death_75 %>% left_join(pop_75)

# death_75 %>% group_by(year) %>% summarise(sum(is.na(pop75)/n()))
# only starting from 2002 for now
death_75 <- death_75 %>% filter(year>2001)

# remove commune 9999
death_75 <- death_75 %>% filter(codigo_comuna!=99999)

death_75 <- death_75 %>% mutate(mortality=death_count/pop75*1000)

data_save <- death_75 %>% filter(sex=="TOTAL") %>% 
  filter(cause=="Death_count_all_cause") %>% dplyr::select(-sex)
data_save$codigo_comuna %>% unique() %>% length()
write.csv(data_save,"Data/mortality_data.csv",row.names = F)
rm(data_save)


# use only communes with at least 50 people in the age group (on average)
pop_counties <- death_75 %>% group_by(codigo_comuna) %>% summarise(pop75=mean(pop75))
pop_counties <- pop_counties %>% filter(pop75>50) %>% pull(codigo_comuna) # 328 for 75+, 333 for 65+

death_75 <- death_75 %>% filter(codigo_comuna %in% pop_counties)


# are complete records?
nrow(death_75)
length(unique(death_75$codigo_comuna))*length(unique(death_75$sex))* # 1275264 records
  length(unique(death_75$year))*length(unique(death_75$month))*length(unique(death_75$cause))


# Spread
death_75 <- death_75 %>% 
  pivot_wider(names_from = cause, values_from = c(death_count,mortality),values_fill = 0)
names(death_75) <- names(death_75) %>% 
  str_replace_all("death_count_Death_count","death_count") %>% 
  str_replace_all("mortality_Death_count","MR")


# pm25 pollution data exposure ------
pm25_exp <- read.delim("Data/pm25exposure_commune.csv",sep = ";")
pm25_exp$codigo_comuna %>% unique() %>% length()

# remove below 1 exposure
# pm25_exp <- pm25_exp %>% filter(pm25_exposure>1)

landTemp <-  read.delim("Data/landTemp_commune.csv",sep = ";")
landTemp$codigo_comuna %>% unique() %>% length()


names(landTemp) <- names(landTemp) %>% str_replace_all("total_pop","total_pop_T")


# Join ----
# names(mortality)
names(pm25_exp)
names(death_75)
names(landTemp)


df <- death_75 %>% left_join(pm25_exp) %>% left_join(landTemp) %>% rename(pm25_exposure=pm25_Exposure)

# remove Isla Pascua
df <- df %>% filter(codigo_comuna!=5201)

sum(is.na(df$pm25_exposure))
sum(is.na(df$landTemp))


# are complete records?
nrow(df)
length(unique(df$codigo_comuna))*length(unique(df$sex))* # 211896 records
  length(unique(df$year))*length(unique(df$month))
length(unique(df$codigo_comuna))
# 327 communes for 75+, 332 for 65+


df <- df %>% 
  mutate(quarter=ceiling(month/3) %>% as.integer()) # quarters by months

df <- df %>% mutate(pm25Exp_10ug=pm25_exposure/10) %>% 
  mutate(commune=as.factor(codigo_comuna))

# save results separately, to avoid confusions
df_all <- df %>% filter(sex=="TOTAL")
df_all$sex <- NULL

write.table(df_all,"Data/Panel Data/panelData.csv",sep = ";",row.names = F)
# write.table(df_all,"Data/Panel Data/panelData_65.csv",sep = ";",row.names = F)

df_sex <- df %>% filter(sex!="TOTAL")
df_sex$sex %>% table()

write.table(df_sex,"Data/Panel Data/panelData_Sex.csv",sep = ";",row.names = F)
# write.table(df_sex,"Data/Panel Data/panelData_Sex_65.csv",sep = ";",row.names = F)


# Check communes
df %>% group_by(NOM_COMUNA) %>% summarise(pm25_exposure=mean(pm25_exposure)) %>% arrange(pm25_exposure)
df %>% group_by(NOM_COMUNA) %>% summarise(pm25_exposure=mean(pm25_exposure)) %>% arrange(desc(pm25_exposure))
df %>% group_by(NOM_COMUNA) %>% summarise(landTemp=mean(landTemp)) %>% arrange(landTemp)
df %>% group_by(NOM_COMUNA) %>% summarise(landTemp=mean(landTemp)) %>% arrange(desc(landTemp))

# EoF