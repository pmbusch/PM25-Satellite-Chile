## Get satellite estimations at monitor coordinates
## Source: https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.03
## PBH
## February 2023

## libraries ------
library(tidyverse)
library(raster)
library(rasterVis)
library(ncdf4)
library(RColorBrewer)


file_url <- "Data/Satellite/%s/pm25Chile_%s.rds"

## Extract data on monitor sites ----------

## Monitor coordinates
monitors <- read_delim("Data/pm25_year.csv",delim = ";")
monitors <- monitors %>% group_by(site,latitude,longitude) %>% tally() %>% 
  ungroup() %>% dplyr::select(-n)
# to sf
monitors <- sf::st_as_sf(monitors, 
                       coords = c("longitude","latitude"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")
# mapview::mapview(monitors)


# Loop year
monitor_year <- monitors
monitor_year$geometry <- NULL
for (y in 2015:2021){
  cat("Year :",y,"\n")
  # satellite data
  mp_chile <- readRDS(sprintf(file_url,"Annual",y))
  ## Spatial join satellite data  (raster) with  lat long coordinates of monitors
  extracted_values <- raster::extract(mp_chile, monitors, method="bilinear") # bilinear interpolates from the four nearest cell
  colName <- paste0("",y)
  monitor_year <- cbind(monitor_year,extracted_values) %>% rename(!!colName:=extracted_values)
}
# to long format
monitor_year <- monitor_year %>% pivot_longer(c(-site,-latitude,-longitude), 
                                              names_to = "year", values_to = "pm25_satellite")
# monitor_year$year %>% unique()
write.table(monitor_year,"Data/pm25Satellite_year.csv",sep=";",row.names = F)


# Loop month
months <- 1:12
months <- paste0(ifelse(str_length(months)==1,"0",""),months)
monitor_month <- monitors
monitor_month$geometry <- NULL
for (y in 2015:2021){
  for (m in months){
    cat("Period :",y,"-",m,"\n")
    # satellite data
    mp_chile <- readRDS(sprintf(file_url,"Monthly",paste0(y,"_",m)))
    ## Spatial join satellite data  (raster) with  lat long coordinates of monitors
    extracted_values <- raster::extract(mp_chile, monitors, method="bilinear") # bilinear interpolates from the four nearest cell
    colName <- paste0(y,"_",m)
    monitor_month <- cbind(monitor_month,extracted_values) %>% rename(!!colName:=extracted_values)
  }
}
# to long format
monitor_month <- monitor_month %>% pivot_longer(c(-site,-latitude,-longitude), 
                                              names_to = "period", values_to = "pm25_satellite")

monitor_month <- monitor_month %>% mutate(year=substr(period,start=1,stop=4),
                         month=substr(period,start=6,stop=7))
monitor_month$period <- NULL
# monitor_month$year %>% unique()
# monitor_month$month %>% unique()

write.table(monitor_month,"Data/pm25Satellite_month.csv",sep=";",row.names = F)

# EoF