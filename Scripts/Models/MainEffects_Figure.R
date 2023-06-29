# Main Effects figure
#
# PBH June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

source("Scripts/Functions.R",encoding = "UTF-8")

fig_name <- "Figures/Effects/%s.png"

# Load data -----

# Two options: 
# (1) Load previously saved data coming from bootstraps runs
# (2) Use the standard errors from the models fitted


# Load Panel Data
df <- read.delim("Data/panelData.csv",sep=";")
df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101"))

# Function to fit model  - Option 2 ----
fitmod <- function(data_){
  mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                  year_quarter+commune+
                       offset(log(pop75)), 
                     data = data_,
                     na.action=na.omit)
  return(mod)
}

# Function to calculate risk slope and line for the plot ----
getRiskSlope <- function(data_, mod_info,pm_range=0:60,temp=F){
  #calculate weighted mean for base rate
  br <- weighted.mean(data_$MR_all_cause,data_$pop75)  
  pm <- weighted.mean(data_$pm25_exposure,data_$pop75) 
  adj <- if (temp) 1 else 10 # present per 10 PM2.5 or per 1 °C
  if (temp) {
    pm <- weighted.mean(data_$landTemp,data_$pop75)
  }
  
  # slope assumed linear (constant). Slope is per 10 ug/m3 change
  rr <- c(mod_info$rr,mod_info$rr_low, mod_info$rr_high)
  slope <- (rr/100)*br
  
  # create dataframe with points for lines
  response_pm <- tibble(x=pm_range)
  response_pm <- response_pm %>% 
    mutate(y=br+(x-pm)/adj*slope[1], # Check: Simple linear slope starting from means
           y_low=br+(x-pm)/adj*slope[2],
           y_high=br+(x-pm)/adj*slope[3])
  return(response_pm)
}

# Figure Main Effect Full Model -----

# Option 1
# boot <- read.csv2("Data/Bootstrap/Bootstrap_Main.csv")

## Option 2 - Use SE from Regression Model
model_nb <- fitmod(df)
# get model info - Robust SE
out <- getModelInfo(model_nb,"Main")


# bootstrap option
# boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);
# boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()
# mean_rr <- mean(boot$pm25Exp_10ug)
# ci_rr <- quantile(boot$pm25Exp_10ug,c(0.025,0.975))

## PM2.5 Figure -----
df$pm25_exposure %>% quantile(c(0.01,0.99)) # 99 percentile

# get line with CI
out_pm25 <- out %>% filter(param=="pm25Exp_10ug")
response_pm <- getRiskSlope(df, out_pm25, pm_range = 0:60)

# Figure
ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high),
              alpha = 0.4,fill="#8B451380")+
  geom_line(aes(y=y),linewidth=0.5,col="#8B4513")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#8B4513",col="white")+
  annotate("text", x = 40, y = 1.2, size=10*5/14 * 0.8,
           label = "PM2.5 Exposure distribution", color = "#8B4513")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(sprintf(fig_name,"Effect_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Temperature Figure -----

df$landTemp %>% range()
df$landTemp %>% quantile(c(0.01,0.99))

response_pm <- getRiskSlope(df, filter(out,param=="landTemp"), 
                            pm_range = 0:45,temp = T)
ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high),
              alpha = 0.4,fill="#8000008B")+
  geom_line(aes(y=y),linewidth=0.5,col="#00008B")+
  geom_histogram(aes(landTemp,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#00008B",col="white")+
  annotate("text", x = 30, y = 2.2, size=10*5/14 * 0.8,
           label = "Land Temperature distribution", color = "#00008B")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,9,2)),limits = c(0,9)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(sprintf(fig_name,"EffectTemp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####

# Figure for Metropolitan region and rest of country ----

# Option 2
# Met
model_nb1 <- fitmod(filter(df,REGION==13))
out1 <- getModelInfo(model_nb1,"Met",data_df = filter(df,REGION==13))
  
# No MET
model_nb2 <- fitmod(filter(df,REGION!=13))
out2 <- getModelInfo(model_nb2,"NoMet",data_df = filter(df,REGION!=13))


## PM2.5 Figure -----
df <- df %>% mutate(met= REGION==13)
df %>% group_by(met) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))

response_pm1 <- getRiskSlope(filter(df,REGION==13), 
                            filter(out1,param=="pm25Exp_10ug"), 
                            pm_range = 10:49)
response_pm2 <- getRiskSlope(filter(df,REGION!=13), 
                             filter(out2,param=="pm25Exp_10ug"), 
                            pm_range = 5:59)
response_pm1$met <- T; response_pm2$met <- F
response_pm <- rbind(response_pm1,response_pm2)

ggplot(response_pm,aes(x,group=met))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=met),
              alpha = 0.4)+
  geom_line(aes(y=y,col=met),linewidth=0.5)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,met==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#2ecc71",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,met==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#9b59b6",col="white")+
  annotate("text", x = 30, y = 1.2, size=10*5/14 * 0.8,
           label = "Rest of Country", color = "#2ecc71")+
  annotate("text", x = 30, y = 3.2, size=10*5/14 * 0.8,
           label = "Metropolitan Region", color = "#9b59b6")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#9b59b6", "FALSE" = "#2ecc71"))+
  scale_fill_manual(values = c("TRUE" = "#9b59b680", "FALSE" = "#2ecc7180"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Met_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Temperature Figure -----

df %>% group_by(met) %>% reframe(quantile(landTemp,c(0.01,0.99)))

response_pm1 <- getRiskSlope(filter(df,REGION==13), 
                             filter(out1,param=="landTemp"), 
                             pm_range = 11:42,temp=T)
response_pm2 <- getRiskSlope(filter(df,REGION!=13), 
                             filter(out2,param=="landTemp"), 
                             pm_range = 3:44,temp=T)
response_pm1$met <- T; response_pm2$met <- F
response_pm <- rbind(response_pm1,response_pm2)


ggplot(response_pm,aes(x,group=met))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=met),
              alpha = 0.4)+
  geom_line(aes(y=y,col=met),linewidth=0.5)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,met==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#117744",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,met==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#114477",col="white")+
  annotate("text", x = 30, y = 1.2,size=10*5/14 * 0.8, 
           label = "Rest of Country", color = "#117744")+
  annotate("text", x = 30, y = 3.2,size=10*5/14 * 0.8, 
           label = "Metropolitan Region", color = "#114477")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#114477", "FALSE" = "#117744"))+
  scale_fill_manual(values = c("TRUE" = "#77AADD", "FALSE" = "#44AA77"))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Temp_Met"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####


# Figure by Half of year -----

df %>% group_by(month) %>% summarise(mean(pm25_exposure))
df <- df %>% mutate(cold = quarter %in% c("2","3"))
rm(model_nb1,model_nb2)
# Cold
model_nb1 <- fitmod(filter(df,cold==T))
out1 <- getModelInfo(model_nb1,"Cold",data_df = filter(df,cold==T))
# Hot
model_nb2 <- fitmod(filter(df,cold==F))
out2 <- getModelInfo(model_nb2,"Hot",data_df = filter(df,cold==F))


## PM2.5 Figure -----

df %>% group_by(cold) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,cold==T), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 10:62)
response_pm2 <- getRiskSlope(filter(df,cold==F), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 5:24)
response_pm1$cold <- T; response_pm2$cold <- F
response_pm <- rbind(response_pm1,response_pm2)

ggplot(response_pm,aes(x,group=cold))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=cold),
              alpha = 0.4)+
  geom_line(aes(y=y,col=cold),linewidth=0.5)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,cold==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#8B0000",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,cold==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#1A237E",col="white")+
  annotate("text", x = 30, y = 1.2,size=10*5/14 * 0.8, 
           label = "Summer & Spring", color = "#8B0000")+
  annotate("text", x = 30, y = 3.6,size=10*5/14 * 0.8, 
           label = "Winter & Fall", color = "#1A237E")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#1A237E", "FALSE" = "#8B0000"))+
  scale_fill_manual(values = c("TRUE" = "#1A237E80", "FALSE" = "#8B000080"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Season_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Temperature Figure -----

df %>% group_by(cold) %>% reframe(quantile(landTemp,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,cold==T), 
                             filter(out1,param=="landTemp"), 
                             pm_range = 1:35,temp=T)
response_pm2 <- getRiskSlope(filter(df,cold==F), 
                             filter(out2,param=="landTemp"), 
                             pm_range = 12:46,temp=T)
response_pm1$cold <- T; response_pm2$cold <- F
response_pm <- rbind(response_pm1,response_pm2)


ggplot(response_pm,aes(x,group=cold))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=cold),
              alpha = 0.4)+
  geom_line(aes(y=y,col=cold),linewidth=0.5)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,cold==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#8B0000",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,cold==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#1A237E",col="white")+
  annotate("text", x = 20, y = 1.2,size=10*5/14 * 0.8, 
           label = "Summer & Spring", color = "#8B0000")+
  annotate("text", x = 20, y = 3.6,size=10*5/14 * 0.8, 
           label = "Winter & Fall", color = "#1A237E")+
   scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8.1)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,46))+
  scale_color_manual(values = c("TRUE" = "#1A237E", "FALSE" = "#8B0000"))+
  scale_fill_manual(values = c("TRUE" = "#1A237E80", "FALSE" = "#8B000080"))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Temp_Season_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


# Figure by Urban and Rural --------

# Urban Share
com_rural <- df %>% group_by(commune) %>% 
  summarise(total_pop_urban=sum(total_pop_urban,na.rm=T)/12/18,
            total_pop_rural=sum(total_pop_rural,na.rm=T)/12/18) %>% ungroup() %>% 
  mutate(rural_share=total_pop_rural/(total_pop_rural+total_pop_urban)) %>% 
  arrange(desc(rural_share))
nrow(filter(com_rural,rural_share>0.5))/nrow(com_rural) # 28%
# Communes with more than 50% of rural habitants
com_rural <- com_rural %>% filter(rural_share>0.5) %>% pull(commune) # 93

df <- df %>% mutate(comRural=(commune %in% com_rural))

df %>% group_by(comRural) %>% summarise(mean(pm25_exposure))


rm(model_nb1,model_nb2)
# Urban
model_nb1 <- fitmod(filter(df,comRural==F))
out1 <- getModelInfo(model_nb1,"Urban",data_df = filter(df,comRural==F))
# Rural
model_nb2 <- fitmod(filter(df,comRural==T))
out2 <- getModelInfo(model_nb2,"Rural",data_df = filter(df,comRural==T))


## PM2.5 Figure -----
df %>% group_by(comRural) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,comRural==F), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 6:58)
response_pm2 <- getRiskSlope(filter(df,comRural==T), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 5:58)
response_pm1$comRural <- F; response_pm2$comRural <- T
response_pm <- rbind(response_pm1,response_pm2)


ggplot(response_pm,aes(x,group=comRural))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=comRural),
              alpha = 0.4)+
  geom_line(aes(y=y,col=comRural),linewidth=0.5)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,comRural==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#333333",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,comRural==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#F4A460",col="white")+
  annotate("text", x = 30, y = 1.2,size=10*5/14 * 0.8, 
           label = "Urban", color = "#333333")+
  annotate("text", x = 30, y = 3.6, size=10*5/14 * 0.8,
           label = "Rural", color = "#F4A460")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#F4A460", "FALSE" = "#333333"))+
  scale_fill_manual(values = c("TRUE" = "#F4A46080", "FALSE" = "#33333380"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Urban2"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Temperature Figure -----

df %>% group_by(comRural) %>% reframe(quantile(landTemp,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,comRural==F), 
                             filter(out1,param=="landTemp"), 
                             pm_range = 3:43,temp=T)
response_pm2 <- getRiskSlope(filter(df,comRural==T), 
                             filter(out2,param=="landTemp"), 
                             pm_range = 3:44,temp=T)
response_pm1$comRural <- F; response_pm2$comRural <- T
response_pm <- rbind(response_pm1,response_pm2)


ggplot(response_pm,aes(x,group=comRural))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=comRural),
              alpha = 0.4)+
  geom_line(aes(y=y,col=comRural),linewidth=0.5)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,comRural==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#333333",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,comRural==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#F4A460",col="white")+
  annotate("text", x = 20, y = 1.2,size=10*5/14 * 0.8, 
           label = "Urban", color = "#333333")+
  annotate("text", x = 20, y = 3.6, size=10*5/14 * 0.8,
           label = "Rural", color = "#F4A460")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#F4A460", "FALSE" = "#333333"))+
  scale_fill_manual(values = c("TRUE" = "#F4A46080", "FALSE" = "#33333380"))+
  labs(x=expression(paste("Land Temperature [°C]","")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Urban_temp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# EoF