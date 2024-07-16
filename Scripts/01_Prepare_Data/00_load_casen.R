# Load Socioeconomic data from CASEN 2017 Survey
# http://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2017
# Code is simply used to load socioeconomic data for further analysis
# PBH June 2023

library(tidyverse)
library(haven)
library(chilemapas)
library(readxl)

df_casen <-read_sav("Data/Data_Original/Casen 2017.sav")
df_casen %>% names()

nrow(df_casen) # 216,439

## Unify codigo_comuna: add 0 to regions (01)
df_casen <- df_casen %>% 
  mutate(comuna=paste(if_else(str_length(comuna)==4,"0",""),
                      comuna,sep=""))

# Expansion Factor: commune and region
df_casen$expc %>% sum()


## INCOME -------
# ytotcor: monthly income per capita (corrected). (Ingreso total corregido)
df_income <- df_casen %>% 
  group_by(comuna) %>% 
  summarise(income_mean=sum(ytotcor*expc,na.rm=T)/sum(expc,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(income_mean=income_mean/649*12) %>% # 2017: 1 USD = 649 CLP
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

## EDUCATION ------------
# e6a: Cuál fue el nivel educacional más alto alcanzado o el nivel educacional actual
# e6a: , highest education level
df_codigoEducacion <- read_excel("Data/Data_Original/Codigos_CASEN.xlsx", sheet = "e6a")
df_education <- df_casen %>% 
  group_by(comuna,e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)


# create variable: % less than high school education
df_education$e6a %>% unique()
df_education <- df_education %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(codigo_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  dplyr::select(codigo_comuna,perc) %>% 
  rename(perc_less_highschool=perc)


## HEALTH CARE PROVIDER -------
# s12: A qué sistema previsional de salud pertenece usted
# s12: affiliation to health care provider (private or state level
df_codigoSalud <- read_excel("Data/Data_Original/Codigos_CASEN.xlsx", sheet = "s12")
df_healthProvider <- df_casen %>% 
  group_by(comuna,s12) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoSalud, by=c("s12"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

# Method is done to include commune with nobody with isapre
df_healthProvider$s12 %>% unique()
df_healthProvider <-  df_healthProvider %>% 
  filter(s12!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(prev=case_when(
    s12==1 ~ "perc_fonasa_A",
    s12==2 ~ "perc_fonasa_B",
    s12==3 ~ "perc_fonasa_C",
    s12==4 ~ "perc_fonasa_D",
    s12==6 ~ "perc_FFAA",
    s12==7 ~ "perc_isapre",
    T~"otro")) %>% 
  group_by(codigo_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% dplyr::select(-hab) %>% 
  pivot_wider(names_from = prev, values_from = perc, values_fill = 0) %>% dplyr::select(-otro)


## occupancy rate -----------
# o9a: o9a. ¿Cuál es su ocupación u oficio?
# o1: La semana pasada, ¿trabajó al menos una hora, sin considerar los quehaceres del hogar?
# o9a: occupancy rate (last week)
df_occupancy <- df_casen %>% 
  filter(!is.na(o1)) %>% 
  group_by(comuna,o1) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100,
         o1=if_else(o1==1,"Si","No")) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)
# Leave only percentaje of occupancy
df_occupancy <- df_occupancy %>% filter(o1=="Si") %>% rename(perc_occupancy=perc)


## WOOD -----------
# v36a: Qué combustible o fuente de energía usa habitualmente para: Cocinar 
# v36b: Idem Calefacción
# v36c: Idem Sistema de Agua Caliente
# Opción 4: Leña o derivados (pellets, astillas o briquetas)
# Main fuel used for: cooking (v36a), heating (v36b), warm water (v36c)
# Sum if to wood options
df_wood_casen <- df_casen %>% 
  group_by(comuna) %>% 
  summarise(hab=sum(expc, na.rm=T),
            lena_cocina=sum(if_else(v36a==4,expc,0),na.rm=T),
            lena_calefaccion=sum(if_else(v36b==4,expc,0),na.rm=T),
            lena_agua=sum(if_else(v36c==4,expc,0),na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

df_wood_casen <- df_wood_casen %>% 
  mutate(perc_woodCooking=lena_cocina/hab*100,
         perc_woodHeating=lena_calefaccion/hab*100,
         perc_woodWarmWater=lena_agua/hab*100) %>% 
  dplyr::select(codigo_comuna, perc_woodCooking, perc_woodHeating, perc_woodWarmWater)



## JOIN ALL -------
df_casen <- left_join(df_income, 
                      df_occupancy %>% dplyr::select(codigo_comuna, perc_occupancy)) %>% 
  left_join(df_education) %>% 
  left_join(df_healthProvider) %>% 
  left_join(df_wood_casen) %>% 
  dplyr::select(-nombre_comuna,-codigo_provincia,-nombre_provincia,
         -codigo_region,-nombre_region)

# Save intermediate data -----
write.csv(df_casen,"Data/socioeconomic.csv",row.names = F)

# EoF