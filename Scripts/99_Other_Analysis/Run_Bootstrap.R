## Run Bootstrap for main Model
## PBH
## May 2023


library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)
# library(boot)


# Code takes a lot to run

# Load Panel Data
df <- read.delim("Data/panelData.csv",sep=";")
# Directly from GitHub
# df <- read.delim("https://raw.githubusercontent.com/pmbusch/PM25-Satelite-Chile/main/Data/panelData.csv",sep=";")


df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago

# to debug, select 50 random communes
df$commune %>% unique() %>% length()
set.seed(1)
coms <- df$commune %>% unique() %>% sample(50,replace = F)

df <- df %>% filter(commune %in% coms)


# run bootstrap of base model.
# SOURCE: https://github.com/echolab-stanford/NCC2018/blob/master/scripts/MakeFigure1.R

# Note: Bootstrap should be made by choosing communes with their full time series data, if not, the model does not make
# sense as it loses the time series data for each commune


# BOOTSTRAP -----

# TAKES FOREVEEEEEEER TO RUN, NEED TO DO IT IN FASTER MACHINE
coms<- unique(df$commune)
set.seed(1)
# bootsize <- length(coms)
bootsize <- 50 # to debug faster
# bootreps <- 1000
bootreps <- 100 
outPM25 <- c()
outTemp <- c()

# Main Model -----

for (i in 1:bootreps) {
  tryCatch( { samp <- data.frame(commune=sample(coms,bootsize,replace=T))
      subdata <- inner_join(df,samp)    
      reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                              offset(log(pop75)), data = subdata, 
                            na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
      outPM25 <- c(outPM25,reg[1])
      outTemp <- c(outTemp,reg[2])
      }, error=function(e){})
      print(i)
    }

write_csv(data.frame(x = 1:length(outPM25), pm25Exp_10ug = outPM25,landTemp=outTemp),
            file = "Data/Bootstrap/Bootstrap_Main.csv")

# Metropolitan vs Rest of Country -----

## For Metropolitan region subsample ------

df_met <- filter(df,REGION ==13)
coms<- unique(df_met$commune)
set.seed(1)
outPM25 <- c()
outTemp <- c()

for (i in 1:bootreps) {
  tryCatch( { samp <- data.frame(commune=sample(coms,length(coms),replace=T))
  subdata <- inner_join(df_met,samp)    
  reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                          offset(log(pop75)), data = subdata, 
                        na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
  outPM25 <- c(outPM25,reg[1])
  outTemp <- c(outTemp,reg[2])
  }, error=function(e){})
  print(i)
}
write_csv(data.frame(x = 1:length(outPM25), pm25Exp_10ug = outPM25,landTemp=outTemp),
          file = "Data/Bootstrap/Bootstrap_Metropolitan.csv")

## Rest of Country -----

df_met <- filter(df,REGION !=13)
coms<- unique(df_met$commune)
set.seed(1)
outPM25 <- c()
outTemp <- c()

for (i in 1:bootreps) {
  tryCatch( { samp <- data.frame(commune=sample(coms,bootsize,replace=T))
  subdata <- inner_join(df_met,samp)    
  reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                          offset(log(pop75)), data = subdata, 
                        na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
  outPM25 <- c(outPM25,reg[1])
  outTemp <- c(outTemp,reg[2])
  }, error=function(e){})
  print(i)
}
write_csv(data.frame(x = 1:length(outPM25), pm25Exp_10ug = outPM25,landTemp=outTemp),
          file = "Data/Bootstrap/Bootstrap_NoMetropolitan.csv")


## NEED TO ADD BOOTSTRAP FOR HALF OF YEAR AND FOR URBAN-RURAL





# EoF