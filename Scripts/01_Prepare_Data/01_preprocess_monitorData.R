## Script to pre-process monitor data for PM2.5
# Load historical monitor data from SINCA Chile
# https://sinca.mma.gob.cl/
# It proceess the data into a monthly data
## PBH
## February 2023

library(tidyverse)
theme_set(theme_bw(16)+ 
            theme(panel.grid.major = element_blank(),
                  axis.title.y=element_text(angle=0,margin=margin(r=0))))


# read data obtained through web scraping for the whole period-----
df <- readRDS("Data/Data_Original/pm25_daily.rds")

# Filters -----

# only consider validated data
df$tipo_dato %>% table()
df <- df %>% filter(tipo_dato=="validados")

# years, since 2015
df$year %>% table()
df <- df %>% filter(year>2014 & year<2022) # satellite data is up to 2021

df <- df %>% filter(unidad!="s/i")

# change names
df <- df %>% rename(province=provincia,
                    commune=comuna,
                    longitude=longitud,latitude=latitud)


# Yearly data
df_year <- df %>% group_by(region,province,commune,site,longitude,latitude,year,pollutant) %>% 
  summarise(value=mean(valor,na.rm=T),
            n=n(), 
            completeness=n/365*100)

ggplot(df_year,aes(completeness))+stat_ecdf()

# initial filter, more than 75% of days with data in a given year
df_year <- df_year %>% filter(completeness>=75)

# number of stations
df_year$site %>% unique() %>% length() # monitors
df_year$year %>% table()

write.table(df_year,"Data/pm25_year.csv",sep=";",row.names = F)

# Monthly data
df_month <- df %>% group_by(region,province,commune,site,longitude,latitude,year,month,pollutant) %>% 
  summarise(value=mean(valor,na.rm=T),
            n=n(), 
            completeness=n/31*100)
ggplot(df_month,aes(completeness))+stat_ecdf()


# monitor %>% left_join(satellite)
# origianl 4019, then 3858, 161 lost
df_month <- df_month %>% filter(completeness>=75)

# save intermediate monthly data
write.table(df_month,"Data/pm25_month.csv",sep=";",row.names = F)

# EoF