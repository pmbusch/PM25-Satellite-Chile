## Estimate PM25 Population exposure
## PBH
## February 2023
## New Census data: April 2023


library(tidyverse)
library(scales)
library(chilemapas)
library(raster)
library(rasterVis)
library(mapview)
library(sf)
library(rgdal)

# load population data for all regions -----

fig_name <- "Figures/PM25_Exposure/%s"

regions <- 1:16
regions <- paste0("R",ifelse(str_length(regions)==1,"0",""),regions)

url_file_reg <- "D:/CENSO 2017/%s/%s"
first <- T

# Loop to all regions
for (r in regions){
  cat(r,"\n")
  
  # load data
  # urban
  manzana <- st_read(sprintf(url_file_reg,r,"MANZANA_IND_C17.shp"))
  # rural
  entidad <- st_read(sprintf(url_file_reg,r,"ENTIDAD_IND_C17.shp"))
  
    # names(manzana);names(entidad); # same name
  # join both
  manzana$type <- "urban"
  entidad$type <- "rural"
  if (first){
    zones <- rbind(manzana,entidad)
    first <- F
  } else {
    zones <- rbind(zones,manzana,entidad)
  }
}
rm(first,r,entidad,manzana)

# map
# mapview(zones,zcol ="type")


# see population coverage by region
# national
sum(zones$TOTAL_PERS)/sum(censo_2017_comunas$poblacion) # 99.48%

# pop by region
pop_reg <- censo_2017_comunas %>% left_join(codigos_territoriales) %>% 
  group_by(codigo_region) %>% summarise(pob=sum(poblacion))

df <- data.frame(zones)
pop_map <- df %>% mutate(REGION=as.integer(REGION)) %>% 
  group_by(REGION) %>% summarise(pob_map=sum(TOTAL_PERS))

pop_reg$pob_map <- pop_map$pob_map
pop_reg <- pop_reg %>% mutate(perc=pob_map/pob)
pop_reg # lowest is 98.2% in R12

# pop by commune
pop_com <- censo_2017_comunas %>% left_join(codigos_territoriales) %>% 
  group_by(codigo_comuna) %>% summarise(pob=sum(poblacion))

pop_map <- df %>% mutate(COMUNA=as.integer(COMUNA)) %>% 
  group_by(COMUNA) %>% summarise(pob_map=sum(TOTAL_PERS)) %>% ungroup() %>% 
  mutate(codigo_comuna=paste0(if_else(str_length(COMUNA)==4,"0",""),COMUNA))
  

pop_com <- pop_com %>% left_join(pop_map)
pop_com <- pop_com %>% mutate(perc=pob_map/pob)
pop_com %>% arrange(perc) %>% head(10) # lowest is 76%% in 01403
# cdf
ggplot(pop_com,aes(perc))+geom_histogram(bins=50)+theme_bw(20)+labs(y="Count of Coomunes",
                                                        x="Percentage of total population captured")
ggsave(sprintf(fig_name,"pop_capture_commune.png"))



# Percentage of rural population
df %>% group_by(type) %>% summarise(pop=sum(TOTAL_PERS)) %>% 
  ungroup() %>% mutate(perc=pop/sum(pop)) # 12.3% Rural

pop_rural <- df %>% group_by(REGION,type) %>% summarise(pop=sum(TOTAL_PERS)) %>% 
  ungroup() %>% group_by(REGION) %>% mutate(perc=pop/sum(pop))

pop_rural <- pop_rural %>% 
  mutate(region=factor(REGION,levels=rev(c("15","1","2","3","4","5","13",
                                     "6","7","16","8","9","14","10","11","12"))))
ggplot(pop_rural,aes(region,perc,fill=type))+
  geom_col()+
  coord_flip(expand = F)+
  labs(x="Region",y="% Population",fill="")+
  geom_hline(yintercept = 0.877,linetype="dashed",linewidth=1.4)+
  scale_y_continuous(labels=percent)+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(20)+
    theme(legend.position = "bottom")
ggsave(sprintf(fig_name,"pop_share.png"))

rm(df,pop_reg,pop_map,pop_rural,pop_com)

# clean some communes from outside continental Chile 
zones$NOM_COMUNA %>% unique()
zones <- zones %>% 
  filter(!(NOM_COMUNA %in% c("ISLA DE PASCUA","JUAN FERNÁNDEZ")))




## Load  PM2.5  monthly files ---------
file_url <- "D:/PM25 Satellite Data/%s/%s"
file_nc_monthly <- "V5GL03.HybridPM25.Global.%s-%s.nc"

# extent of Chile
extent_pop <- extent(zones)

months <- 1:12
months <- paste0(ifelse(str_length(months)==1,"0",""),months)

# Files to read all at once
x <- expand.grid(months,1998:2019)
x <- paste0(x$Var2,x$Var1)
raster_files <- sprintf(file_url,"Monthly",sprintf(file_nc_monthly,x,x))

# create raster brick with all PM25 monthly data
raster_list <- lapply(raster_files, raster) # read raster
raster_list<- lapply(raster_list,function(x) crop(x,extent_pop)) # crop to map extent
# sat <- crop(sat, extent(-76,-65, -56, -17.5)) # chile area
raster_brick <- brick(raster_list) # brick
rm(raster_list)

### extract PM25 for each population polygon based on area cross  -----

# Need to do a loop per region to avoid overflowing and reasonable times

# Loop to all regions
rm(zones)
for (r in regions){
  cat("Region: ",r,"\n")
  
  # load geospatial population data one region at the time
  manzana <- st_read(sprintf(url_file_reg,r,"MANZANA_IND_C17.shp"))  # urban
  entidad <- st_read(sprintf(url_file_reg,r,"ENTIDAD_IND_C17.shp"))  # rural
  # join both
  manzana$type <- "urban"
  entidad$type <- "rural"
  zones <- rbind(manzana,entidad); rm(manzana,entidad)
  
  # crop PM2.5 data to make it more smoothly
  extent_zone <- extent(zones)
  raster_brick_zones <- crop(raster_brick,extent_zone)
  
  # for each polygon it gets the weighted average by the area
  # with this method I can calculate PM2.5 for each polygon of population
  pm25 <- extract(raster_brick_zones,zones,weights=T,fun=mean,na.rm=T) # na for parts of map without PM2.5
  
  # clean
  rm(raster_brick_zones)
  
  # dataframe
  df_pm25 <- data.frame(pm25)
  names(df_pm25) <- x
  
  zones$geometry <- NULL
  
  # add to data frame
  zones <- cbind(zones,df_pm25) 
  
  
  # save intermediate results
  write.table(zones,paste0("Data/Exposure/rawExposure",r,".csv"),
              sep=";",row.names = F)
  
  # flat table
  zones <- zones %>% 
    pivot_longer(c(-1:-17), 
                 names_to = "period", values_to = "pm25_Exposure")
  
  zones <- zones %>% 
    mutate(year=substr(period,1,4),
           month=substr(period,5,6))
  
  
  # check total pop
  # zones %>% group_by(year,month) %>% summarise(pop=sum(TOTAL_PERS))
  # zones %>% group_by(year,month) %>% summarise(pm25=mean(pm25_Exposure))
  
  write.table(zones,paste0("Data/Exposure/pm25exposure",r,".csv"),sep=";",row.names = F)
  
  
  # estimate by commune
  # zones$year %>% table()
  pm25_exp_commune <- zones %>% 
    mutate(pop_pm25=TOTAL_PERS*pm25_Exposure) %>% 
    group_by(REGION,NOM_REGION,PROVINCIA,NOM_PROVIN,COMUNA,NOM_COMUNA,
             year,month) %>% 
    summarise(pop_pm25=sum(pop_pm25,na.rm=T),
              total_pop=sum(TOTAL_PERS,na.rm = T)) %>% 
    # summarise(pm25_exposure=stats::weighted.mean(pm25_Exposure,poblacion,na.rm=T,)) %>% 
    ungroup() %>% 
    mutate(pm25_exposure=pop_pm25/total_pop)
  pm25_exp_commune$pop_pm25 <- NULL
  
  
  # same but with urban_rural
  pm25_exp_commune_type <- zones %>% 
    mutate(pop_pm25=TOTAL_PERS*pm25_Exposure) %>% 
    group_by(REGION,NOM_REGION,PROVINCIA,NOM_PROVIN,COMUNA,NOM_COMUNA,
             type,year,month) %>% 
    summarise(pop_pm25=sum(pop_pm25,na.rm=T),
              total_pop=sum(TOTAL_PERS,na.rm = T)) %>% 
    # summarise(pm25_exposure=stats::weighted.mean(pm25_Exposure,poblacion,na.rm=T,)) %>% 
    ungroup() %>% 
    mutate(pm25_exposure=pop_pm25/total_pop)
  # spread
  pm25_exp_commune_type$pop_pm25 <- NULL
  pm25_exp_commune_type <- pm25_exp_commune_type %>% 
    pivot_wider(names_from = type, values_from = c(pm25_exposure,total_pop))
  pm25_exp_commune_type$pm25_exposure_ <- NULL; pm25_exp_commune_type$total_pop_ <- NULL
  
  
  # join
  pm25_exp_commune <- pm25_exp_commune %>% 
    left_join(pm25_exp_commune_type)
  

  # save
  write.table(pm25_exp_commune,paste0("Data/Exposure/pm25exposure_commune",r,".csv"),
  sep=";",row.names = F)

  # clean
  rm(zones,pm25_exp_commune,pm25_exp_commune_type)

}
rm(raster_brick)

# Next Loop to join all 16 regions

csv_files <- paste0("Data/Exposure/pm25exposure_commune",regions,".csv")
pm25_exp_commune <- lapply(csv_files, read.csv2)
pm25_exp_commune <-  do.call("rbind",pm25_exp_commune)

pm25_exp_commune <- pm25_exp_commune %>% 
  mutate(codigo_comuna=paste0(if_else(str_length(COMUNA)==4,"0",""),COMUNA),
         pm25_exposure=as.numeric(pm25_exposure),
         pm25_exposure_rural=as.numeric(pm25_exposure_rural),
         pm25_exposure_urban=as.numeric(pm25_exposure_urban)) %>% 
  relocate(codigo_comuna,.after=COMUNA)

# checks
pm25_exp_commune$REGION %>% table()
pm25_exp_commune$COMUNA %>% unique() %>% length() # 345 Communes
pm25_exp_commune$pm25_exposure %>% range() # 0 to 90
pm25_exp_commune$year %>% table()
pm25_exp_commune$month %>% table()
pm25_exp_commune %>% group_by(year,month) %>% summarise(sum(total_pop))

# save
write.table(pm25_exp_commune,"Data/pm25exposure_commune.csv",
            sep=";",row.names = F)


# Playground Example OLD-----
# valdivia
# codigos_territoriales %>% 
#   filter(nombre_comuna=="Valdivia")
# mapa_valdivia <- mapa_zonas %>% filter(codigo_comuna=="14101")
# mapa_valdivia <- st_as_sf(mapa_valdivia) # convert to geospatial object
# mapa_valdivia <- mapa_valdivia %>% 
#   left_join(zones) # add pop
# mapa_valdivia$area <- st_area(mapa_valdivia)
# 
# # plot(mapa_valdivia)
# # mapview(mapa_valdivia)
# 
# file <- sprintf(file_url,"Monthly",sprintf(file_nc_monthly,"201908","201908"))
# sat <- raster(file,varname = "GWRPM25",band = 1)
# sat <- crop(sat, extent(-73.3,-73.1, -39.9, -39.7))
# sat <- crop(sat, extent(-76,-65, -56, -17.5)) # chile area
# crs(sat) <- crs(mapa_valdivia) # just to map, does not have effect on extraction
# 
# # plot(sat)
# mapview(sqrt(sat),maxpixels =  4125000)+mapview(mapa_valdivia)
# 
# ### extract PM25 for each population polygon based on area cross ---
# # for each polygon it gets the weighted average by the area
# # with this method I can calculate PM2.5 for each polygon of population
# d <- extract(sat,mapa_valdivia,weights=T,fun=mean,na.rm=TRUE) 
# d

# EoF