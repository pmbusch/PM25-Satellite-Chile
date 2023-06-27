## Pre Process Satellite data to Chile
## Source: https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V5.GL.03
## PBH
## February 2023

## libraries ------
library(tidyverse)
library(raster)
library(rasterVis)
library(ncdf4)
library(RColorBrewer)


# Load data -----
file_url <- "D:/PM25 Satellite Data/%s/%s"
file_nc_anual <- "V5GL03.HybridPM25.Global.%s01-%s12.nc"
file_nc_monthly <- "V5GL03.HybridPM25.Global.%s-%s.nc"
save_file_url <- "Data/Satellite/%s/pm25Chile_%s.rds"

# Loop year
for (y in 2015:2021){
  cat("Year: ",y,"\n")
  file <- sprintf(file_url,"Annual",sprintf(file_nc_anual,y,y))
  mp <- raster(file,varname = "GWRPM25",band = 1)
  # Filter Chile extent
  mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))
  saveRDS(mp_chile,sprintf(save_file_url,"Annual",y))
}

# Loop year-month

months <- 1:12
months <- paste0(ifelse(str_length(months)==1,"0",""),months)
for (y in 2015:2021){
  for (m in months){
    cat("Period: ",y,"-",m,"\n")
    x <- paste0(y,m)
    file <- sprintf(file_url,"Monthly",sprintf(file_nc_monthly,x,x))
    mp <- raster(file,varname = "GWRPM25",band = 1)
    # Filter Chile extent
    mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))
    saveRDS(mp_chile,sprintf(save_file_url,"Monthly",paste0(y,"_",m)))
  }
}
rm(y,m,x)

#To output a quick view for the dataset
# png("D:\\plot2021.png",
#     height = 15,
#     width = 20,
#     units = 'cm',
#     res = 1000)
# print(levelplot(mp))
# dev.off()



# png("D:\\plot2021Chile.png",
#     height = 15,
#     width = 20,
#     units = 'cm',
#     res = 1000)
# print(levelplot(mp_chile))
# dev.off()

# EoF