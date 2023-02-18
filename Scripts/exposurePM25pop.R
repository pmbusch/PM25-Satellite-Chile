## Estimate PM25 Population exposure
## PBH
## February 2023

library(tidyverse)
library(chilemapas)
library(raster)
library(rasterVis)
library(mapview)
library(sf)

# load data -----
# population Census 2017
zones <- chilemapas::censo_2017_zonas
zones <- zones %>% group_by(geocodigo) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T)) %>% ungroup()
zones$poblacion %>% sum() # total pop
map_pop <- st_as_sf(mapa_zonas) # convert to geospatial object
map_pop <- map_pop %>% 
  left_join(zones) # add pop
map_pop$poblacion %>% sum(na.rm=T)

# not all population was here, that's weird, let's compare by region
zones_pop <- zones %>% mutate(codigo_region=substr(geocodigo,1,2)) %>% 
  group_by(codigo_region) %>% summarise(pop=sum(poblacion,na.rm=T)) %>% ungroup()
zones_pop$key <- "Zone"
map_pop2 <- map_pop %>% group_by(codigo_region) %>% 
  summarise(pop=sum(poblacion,na.rm=T)) %>% ungroup()
map_pop2$geometry <- NULL
map_pop2$key <- "Map"
rbind(zones_pop,map_pop2) %>% 
  ggplot(aes(codigo_region,pop,fill=key))+
  geom_bar (stat="identity", position = position_dodge())+
  theme_bw()
rm(map_pop2,zones_pop)
# not too bad, at least not concentrated in a particular region


## Loop through monhtly files
file_url <- "D:/PM25 Satellite Data/%s/%s"
file_nc_monthly <- "V5GL03.HybridPM25.Global.%s-%s.nc"

data_storage <- map_pop
data_storage$geometry <- NULL

months <- 1:12
months <- paste0(ifelse(str_length(months)==1,"0",""),months)
for (y in 2015:2019){
  for (m in months){
    cat("Period: ",y,"-",m,"\n")
    x <- paste0(y,m)
    file <- sprintf(file_url,"Monthly",sprintf(file_nc_monthly,x,x))
    sat <- raster(file,varname = "GWRPM25",band = 1)
    sat <- crop(sat, extent(-76,-65, -56, -17.5)) # chile area

    ### extract PM25 for each population polygon based on area cross  -----
    # for each polygon it gets the weighted average by the area
    # with this method I can calculate PM2.5 for each polygon of population
    pm25 <- extract(sat,map_pop,weights=T,fun=mean,na.rm=T) # na for parts of map without PM2.5
    data_storage$pm25_exposure <- pm25[,1] # takes around 6 min
    # some NA come from island (Rapa Nui)- Some values are 0 by satellite, for some reason
    
    colName <- paste0("pm25Exp",y,"_",m)
    # save intermediate results
    write.table(data_storage,paste0("Data/Exposure/",colName,".csv"),
                sep=";",row.names = F)
    # data_storage <- data_storage %>% rename(!!colName:=pm25_exposure)
  }
}
rm(y,m,x)

data_storage <- map_pop
data_storage$geometry <- NULL

## get all csv ----
for (y in 2015:2019){
  for (m in months){
    cat("Period: ",y,"-",m,"\n")
    colName <- paste0("pm25Exp",y,"_",m)
    pm25 <- read_delim(paste0("Data/Exposure/",colName,".csv"),delim=";") %>% 
      dplyr::select(geocodigo,pm25_exposure)
    
    data_storage <- data_storage %>% left_join(pm25)
    
    # rename data
    data_storage <- data_storage %>% rename(!!colName:=pm25_exposure)
  }
}
rm(y,m,x)

# flat table
# a <- data_storage
data_storage <- data_storage %>% 
  pivot_longer(c(-geocodigo,-codigo_comuna,-codigo_provincia,
                 -codigo_region,-poblacion), 
               names_to = "period", values_to = "pm25_Exposure")

data_storage <- data_storage %>% 
  mutate(period=period %>% str_remove_all("pm25Exp"),
         year=substr(period,1,4),
         month=substr(period,6,7))


write.table(data_storage,"Data/pm25exposure.csv",sep=";",row.names = F)


# figures ---
ggplot(data_storage,aes(pm25_Exposure,fill=year))+
  geom_density(alpha=.5)+
  facet_wrap(~month)+
  labs(x="PM2.5 Exposure",y="")+
  theme_bw()

# how to aggregate at region level?

pm25_exp_region <- data_storage %>% 
  group_by(codigo_region,year,month) %>% 
  summarise(pm25_exposure=stats::weighted.mean(pm25_Exposure,poblacion,na.rm=T,)) %>% 
  ungroup()

ggplot(pm25_exp_region,aes(month,pm25_exposure,
                           col=codigo_region,group=codigo_region))+
  geom_line()+
  facet_wrap(~year)+
  labs(x="Month",y="PM2.5 Exposure",col="Region")+
  theme_bw()


# Playground Example -----
# valdivia
codigos_territoriales %>% 
  filter(nombre_comuna=="Valdivia")
mapa_valdivia <- mapa_zonas %>% filter(codigo_comuna=="14101")
mapa_valdivia <- st_as_sf(mapa_valdivia) # convert to geospatial object
mapa_valdivia <- mapa_valdivia %>% 
  left_join(zones) # add pop
mapa_valdivia$area <- st_area(mapa_valdivia)

# plot(mapa_valdivia)
# mapview(mapa_valdivia)

sat <- crop(sat, extent(-73.3,-73.1, -39.9, -39.7))
crs(sat) <- crs(mapa_valdivia)

# plot(sat)
mapview(sat)+mapview(mapa_valdivia)

### extract PM25 for each population polygon based on area cross ---
# for each polygon it gets the weighted average by the area
# with this method I can calculate PM2.5 for each polygon of population
d <- extract(sat,mapa_valdivia,weights=T,fun=mean) 
d

# EoF