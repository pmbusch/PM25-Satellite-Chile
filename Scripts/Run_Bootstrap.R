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


df <- df %>% mutate(quarter=factor(quarter),
                    year=as.factor(year),
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

# Define a function to fit the glm.nb model
fit_glm_nb <- function(data,i) {
  subset_data <- data[i,]
  reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                          offset(log(pop75)), data = subset_data, 
                        na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
  return(reg)  
}


start_time <- proc.time() 
# Perform fast bootstrap using boot function
boot_results <- boot(data = df, statistic = fit_glm_nb, R = 100, 
                     sim = "ordinary",strata = df$commune)
end_time <- proc.time() 
print(end_time - start_time) # 400 seg, 6 min for 100 with 50 coms


boot_results$data




for (i in 1:bootreps) {
  tryCatch( { samp <- data.frame(commune=sample(coms,bootsize,replace=T))
      subdata <- inner_join(df,samp)    
      reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                              offset(log(pop75)), data = subdata, 
                            na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
      outPM25 <- c(outPM25,reg[1])
      outTemp <- c(outTemp,reg[2])
      }, error=function(e){})
      print(i)
    }

write_csv(data.frame(x = 1:length(outPM25), pm25Exp_10ug = outPM25,landTemp=outTemp),
            file = "Data/Bootstrap/Bootstrap_Main.csv")


## For Metropolitan region subsample ------

df_met <- filter(df,REGION ==13)
coms<- unique(df_met$commune)
set.seed(1)
outPM25 <- c()
outTemp <- c()

for (i in 1:bootreps) {
  tryCatch( { samp <- data.frame(commune=sample(coms,length(coms),replace=T))
  subdata <- inner_join(df_met,samp)    
  reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
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
  reg <- summary(glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                          offset(log(pop75)), data = subdata, 
                        na.action=na.omit))$coefficients[c("pm25Exp_10ug","landTemp"),"Estimate"]
  outPM25 <- c(outPM25,reg[1])
  outTemp <- c(outTemp,reg[2])
  }, error=function(e){})
  print(i)
}
write_csv(data.frame(x = 1:length(outPM25), pm25Exp_10ug = outPM25,landTemp=outTemp),
          file = "Data/Bootstrap/Bootstrap_NoMetropolitan.csv")



# Figure -----

#calculate weighted mean for base rate
br <- weighted.mean(df$MR_all_cause,df$pop75)  
pm <- weighted.mean(df$pm25_exposure,df$pop75) 
boot <- read.csv2("Data/Bootstrap/Bootstrap_Main.csv")
boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);

# convert to RR
boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()

df$pm25_exposure %>% range()
df$pm25_exposure %>% quantile(c(0.01,0.99))
pm_range <- 0:60 ## 99 percentile

mean_rr <- mean(boot$pm25Exp_10ug)
ci_rr <- quantile(boot$pm25Exp_10ug,c(0.025,0.975))
rr <- c(mean_rr,ci_rr)

# slope assumed linear (constant). Slipe is per 10 ug/m3 change
slope <- (rr-1)*br

# create dataframe with points for lines
response_pm <- tibble(x=pm_range)
response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)/10*slope[1], # Check: Simple linear slope starting from means
         y_low=br+(x-pm)/10*slope[2],
         y_high=br+(x-pm)/10*slope[3])


ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
  ymax = y_high),
  alpha = 0.4,fill="#77AADD")+
  geom_line(aes(y=y),linewidth=1,col="#114477")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 alpha=0.4,fill="#114477",col="white")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  labs(x=expression(paste("PM2.5 Concetration [",mu,"g/",m^3,"]")),
       y=expression(paste("All Cause MR [deaths per 1,000 pop.]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/Effect.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


# same for temp
#calculate weighted mean for base rate
br <- weighted.mean(df$MR_all_cause,df$pop75)  
temp <- weighted.mean(df$landTemp,df$pop75) 
# convert to RR
boot$landTemp <- boot$landTemp %>% exp()

df$landTemp %>% range()
df$landTemp %>% quantile(c(0.01,0.99))
pm_range <- 0:45 ## 99 percentile

mean_rr <- mean(boot$landTemp)
ci_rr <- quantile(boot$landTemp,c(0.025,0.975))
rr <- c(mean_rr,ci_rr)

slope <- (rr-1)*br

# create dataframe with points for lines
response_pm <- tibble(x=pm_range)
response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)*slope[1],
         y_low=br+(x-pm)*slope[2],
         y_high=br+(x-pm)*slope[3])

ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high),
              alpha = 0.4,fill="#AA7744")+
  geom_line(aes(y=y),linewidth=1,col="#DDAA77")+
  geom_histogram(aes(landTemp,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 alpha=0.4,fill="#DDAA77",col="white")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,9,2)),limits = c(0,9)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("All Cause MR [deaths per 1,000 pop.]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/EffectTemp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Figure for Metropolitan region and rest of country ----

#calculate weighted mean for base rate
df <- df %>% mutate(met=REGION ==13)

# order if False (no met) and true (met)
br <- df %>% group_by(met) %>% summarize(x = weighted.mean(MR_all_cause,w=pop75)) %>% pull(x)
pm <-  df %>% group_by(met) %>% summarize(x = weighted.mean(pm25_exposure,w=pop75)) %>% pull(x)

boot <- read.csv("Data/Bootstrap/Bootstrap_Metropolitan.csv")
boot2 <- read.csv("Data/Bootstrap/Bootstrap_NoMetropolitan.csv")
boot$met <- T; boot2$met <- F;
boot <- rbind(boot,boot2); rm(boot2)

boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);

# convert to RR
boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()

df %>% group_by(met) %>% summarise(range(pm25_exposure))
df %>% group_by(met) %>% summarise(quantile(pm25_exposure,c(0.01,0.99)))

pm_range <- 5:59 ## 99 percentile
pm_range_met <- 10:49

rr <- boot %>% group_by(met) %>% summarise(mean_rr=mean(pm25Exp_10ug),
                                           ci_low=quantile(pm25Exp_10ug,c(0.025)),
                                           ci_high=quantile(pm25Exp_10ug,c(0.975)))

# slope assumed linear (constant). Slope is per 10 ug/m3 change
rr$br <- br
rr$pm <- pm
rr <- rr %>% pivot_longer(c(-met,-br,-pm), names_to = "est", values_to = "rr") %>% 
  mutate(slope=(rr-1)*br) %>% dplyr::select(-rr) %>% 
  pivot_wider(names_from = est, values_from = slope)


# create dataframe with points for lines
response_pm <- tibble(x=pm_range);response_pm$met <- F;
response_pm2 <- tibble(x=pm_range_met);response_pm2$met <- T;
response_pm <- rbind(response_pm,response_pm2);rm(response_pm2)

response_pm <- response_pm %>% left_join(rr)

response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)/10*mean_rr, # Check: Simple linear slope starting from means
         y_low=br+(x-pm)/10*ci_low,
         y_high=br+(x-pm)/10*ci_high)


ggplot(response_pm,aes(x,group=met))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=met),
              alpha = 0.4)+
  geom_line(aes(y=y,col=met),linewidth=1)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,met==F),binwidth = 0.5,
                 alpha=0.4,fill="#AA4488",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,met==T),binwidth = 0.5,
                 alpha=0.4,fill="#DDAA77",col="white")+
  annotate("text", x = 30, y = 1.2, label = "Rest of Country", color = "#AA4488")+
  annotate("text", x = 30, y = 3.2, label = "Metropolitan Region", color = "#DDAA77")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#DDAA77", "FALSE" = "#AA4488"))+
  scale_fill_manual(values = c("TRUE" = "#AA7744", "FALSE" = "#CC99BB"))+
  labs(x=expression(paste("PM2.5 Concetration [",mu,"g/",m^3,"]")),
       y=expression(paste("All Cause MR [deaths per 1,000 pop.]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Met.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# Same for Temperature

# order if False (no met) and true (met)
br <- df %>% group_by(met) %>% summarize(x = weighted.mean(MR_all_cause,w=pop75)) %>% pull(x)
temp <-  df %>% group_by(met) %>% summarize(x = weighted.mean(landTemp,w=pop75)) %>% pull(x)

# convert to RR
boot$landTemp <- boot$landTemp %>% exp()

df %>% group_by(met) %>% summarise(range(landTemp))
df %>% group_by(met) %>% summarise(quantile(landTemp,c(0.01,0.99)))

pm_range <- 3:44 ## 99 percentile
pm_range_met <- 11:42

rr <- boot %>% group_by(met) %>% summarise(mean_rr=mean(landTemp),
                                           ci_low=quantile(landTemp,c(0.025)),
                                           ci_high=quantile(landTemp,c(0.975)))

# slope assumed linear (constant). Slope is per 10 ug/m3 change
rr$br <- br
rr$pm <- temp
rr <- rr %>% pivot_longer(c(-met,-br,-pm), names_to = "est", values_to = "rr") %>% 
  mutate(slope=(rr-1)*br) %>% dplyr::select(-rr) %>% 
  pivot_wider(names_from = est, values_from = slope)


# create dataframe with points for lines
response_pm <- tibble(x=pm_range);response_pm$met <- F;
response_pm2 <- tibble(x=pm_range_met);response_pm2$met <- T;
response_pm <- rbind(response_pm,response_pm2);rm(response_pm2)

response_pm <- response_pm %>% left_join(rr)

response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)*mean_rr, # Check: Simple linear slope starting from means
         y_low=br+(x-pm)*ci_low,
         y_high=br+(x-pm)*ci_high)


ggplot(response_pm,aes(x,group=met))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=met),
              alpha = 0.4)+
  geom_line(aes(y=y,col=met),linewidth=1)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,met==F),binwidth = 0.5,
                 alpha=0.4,fill="#117744",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,met==T),binwidth = 0.5,
                 alpha=0.4,fill="#114477",col="white")+
  annotate("text", x = 30, y = 1.2, label = "Rest of Country", color = "#117744")+
  annotate("text", x = 30, y = 3.2, label = "Metropolitan Region", color = "#114477")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#114477", "FALSE" = "#117744"))+
  scale_fill_manual(values = c("TRUE" = "#77AADD", "FALSE" = "#44AA77"))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("All Cause MR [deaths per 1,000 pop.]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Temp_Met.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)



# Source: https://stackoverflow.com/questions/54749641/bootstrapping-with-glm-model
# data structure for results
nboot <- 3
bres <- matrix(NA,
               nrow=nboot,
               ncol=length(coef(model_nb)),
               dimnames=list(rep=seq(nboot),
                             coef=names(coef(model_nb))[2:3])) # save T and PM2.5
# bootstrap
set.seed(1)
bootsize <- nobs(model_nb)
df_boot <- df
start_time <- proc.time() 
for (i in seq(nboot)) {
  bdat <- df_boot[sample(nrow(df_boot),size=bootsize,replace=TRUE),]
  bfit <- update(model_nb, data=bdat)  ## refit with new data
  bres[i,] <- coef(bfit)[2:3]
}
end_time <- proc.time()
print(end_time - start_time)  # 11 min for 3 runs

write_csv(bres,path = "Data/Bootstrap/bootstrap_model.csv")






# EoF