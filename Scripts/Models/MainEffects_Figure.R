# Main Effects figure
#
# PBH June 2023


library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

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



# Figure Main Effect Full Model -----

# Option 1
# boot <- read.csv2("Data/Bootstrap/Bootstrap_Main.csv")

## Option 2 - Use SE from Regression Model
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
# Robust SE
cluster_se <- vcovCL(model_nb, cluster = df$commune)
coef_est <- coef(model_nb)
coef_se <- sqrt(diag(cluster_se)) 
ci_lower <- coef_est - 1.96 * coef_se
ci_upper <- coef_est + 1.96 * coef_se

## PM2.5 Figure -----

#calculate weighted mean for base rate
br <- weighted.mean(df$MR_all_cause,df$pop75)  
pm <- weighted.mean(df$pm25_exposure,df$pop75) 


# boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);

# convert to RR
boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()

df$pm25_exposure %>% range()
df$pm25_exposure %>% quantile(c(0.01,0.99))
pm_range <- 0:60 ## 99 percentile

# mean_rr <- mean(boot$pm25Exp_10ug)
mean_rr <- mean(exp(coef_est["pm25Exp_10ug"]))

# ci_rr <- quantile(boot$pm25Exp_10ug,c(0.025,0.975))
ci_rr <- c(exp(ci_lower["pm25Exp_10ug"]),exp(ci_upper["pm25Exp_10ug"]))
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
              alpha = 0.4,fill="#8B451380")+
  geom_line(aes(y=y),linewidth=1,col="#8B4513")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 alpha=0.4,fill="#8B4513",col="white")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/Effect_se.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Temperature Figure -----

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
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/EffectTemp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####

# Figure for Metropolitan region and rest of country ----

# Option 2
# Met
model_nb1 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                      offset(log(pop75)), 
                    data = filter(df,REGION==13),
                    na.action=na.omit)
cluster_se <- vcovCL(model_nb1, cluster = filter(df,REGION==13)$commune)
coef_est <- coef(model_nb1)
coef_se <- sqrt(diag(cluster_se)) 
ci_lower <- coef_est - 1.96 * coef_se
ci_upper <- coef_est + 1.96 * coef_se

# No MET
model_nb2 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                      offset(log(pop75)), 
                    data = filter(df,REGION!=13),
                    na.action=na.omit)
cluster_se2 <- vcovCL(model_nb2, cluster = filter(df,REGION!=13)$commune)
coef_est2 <- coef(model_nb2)
coef_se2 <- sqrt(diag(cluster_se2)) 
ci_lower2 <- coef_est2 - 1.96 * coef_se2
ci_upper2 <- coef_est2 + 1.96 * coef_se2


#calculate weighted mean for base rate
df <- df %>% mutate(met= REGION==13)

## PM2.5 Figure -----

# order if False (no met) and true (met)
br <- df %>% group_by(met) %>% summarize(x = weighted.mean(MR_all_cause,w=pop75)) %>% pull(x)
pm <-  df %>% group_by(met) %>% summarize(x = weighted.mean(pm25_exposure,w=pop75)) %>% pull(x)

# boot <- read.csv("Data/Bootstrap/Bootstrap_Metropolitan.csv")
# boot2 <- read.csv("Data/Bootstrap/Bootstrap_NoMetropolitan.csv")
# boot$met <- T; boot2$met <- F;
# boot <- rbind(boot,boot2); rm(boot2)

# boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);

# convert to RR
# boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()

df %>% group_by(met) %>% summarise(range(pm25_exposure))
df %>% group_by(met) %>% summarise(quantile(pm25_exposure,c(0.01,0.99)))

pm_range <- 5:59 ## 99 percentile
pm_range_met <- 10:49

# rr <- boot %>% group_by(met) %>% summarise(mean_rr=mean(pm25Exp_10ug),
#                                            ci_low=quantile(pm25Exp_10ug,c(0.025)),
#                                            ci_high=quantile(pm25Exp_10ug,c(0.975)))

# option 2
rr <- data.frame(met=c(F,T),
                 mean_rr=c(exp(coef_est2["pm25Exp_10ug"]),exp(coef_est["pm25Exp_10ug"])),
                 ci_low=c(exp(ci_lower2["pm25Exp_10ug"]),exp(ci_lower["pm25Exp_10ug"])),
                 ci_high=c(exp(ci_upper2["pm25Exp_10ug"]),exp(ci_upper["pm25Exp_10ug"])))


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
                 alpha=0.4,fill="#2ecc71",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,met==T),binwidth = 0.5,
                 alpha=0.4,fill="#9b59b6",col="white")+
  annotate("text", x = 30, y = 1.2, label = "Rest of Country", color = "#2ecc71")+
  annotate("text", x = 30, y = 3.2, label = "Metropolitan Region", color = "#9b59b6")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#9b59b6", "FALSE" = "#2ecc71"))+
  scale_fill_manual(values = c("TRUE" = "#9b59b680", "FALSE" = "#2ecc7180"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Met_se.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Temperature Figure -----
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
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Temp_Met.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####


# Figure by Half of year -----

df %>% group_by(month) %>% summarise(mean(pm25_exposure))

df <- df %>% mutate(cold = quarter %in% c("2","3"))
df %>% group_by(quarter,cold) %>% tally()

# Cold
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                     offset(log(pop75)), 
                   data = filter(df,cold==T),
                   na.action=na.omit)

cluster_se <- vcovCL(model_nb, cluster = filter(df,cold==T)$commune)
coef_est <- coef(model_nb)
coef_se <- sqrt(diag(cluster_se)) 
ci_lower <- coef_est - 1.96 * coef_se
ci_upper <- coef_est + 1.96 * coef_se

# Hot
model_nb2 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                      offset(log(pop75)), 
                    data = filter(df,cold==F),
                    na.action=na.omit)
cluster_se2 <- vcovCL(model_nb2, cluster =filter(df,cold==F)$commune)
coef_est2 <- coef(model_nb2)
coef_se2 <- sqrt(diag(cluster_se2)) 
ci_lower2 <- coef_est2 - 1.96 * coef_se2
ci_upper2 <- coef_est2 + 1.96 * coef_se2

## PM2.5 Figure -----
# order if False (summer and spring) and true (fall and winter)
br <- df %>% group_by(cold) %>% summarize(x = weighted.mean(MR_all_cause,w=pop75)) %>% pull(x)
pm <-  df %>% group_by(cold) %>% summarize(x = weighted.mean(pm25_exposure,w=pop75)) %>% pull(x)


df %>% group_by(cold) %>% summarise(range(pm25_exposure))
df %>% group_by(cold) %>% summarise(quantile(pm25_exposure,c(0.01,0.99)))

pm_range <- 5:24 ## 99 percentile
pm_range_cold <- 10:62


# option 2
rr <- data.frame(cold=c(F,T),
                 mean_rr=c(exp(coef_est2["pm25Exp_10ug"]),exp(coef_est["pm25Exp_10ug"])),
                 ci_low=c(exp(ci_lower2["pm25Exp_10ug"]),exp(ci_lower["pm25Exp_10ug"])),
                 ci_high=c(exp(ci_upper2["pm25Exp_10ug"]),exp(ci_upper["pm25Exp_10ug"])))


# slope assumed linear (constant). Slope is per 10 ug/m3 change
rr$br <- br
rr$pm <- pm
rr <- rr %>% pivot_longer(c(-cold,-br,-pm), names_to = "est", values_to = "rr") %>% 
  mutate(slope=(rr-1)*br) %>% dplyr::select(-rr) %>% 
  pivot_wider(names_from = est, values_from = slope)


# create dataframe with points for lines
response_pm <- tibble(x=pm_range);response_pm$cold <- F;
response_pm2 <- tibble(x=pm_range_cold);response_pm2$cold <- T;
response_pm <- rbind(response_pm,response_pm2);rm(response_pm2)

response_pm <- response_pm %>% left_join(rr)

response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)/10*mean_rr, # Check: Simple linear slope starting from means
         y_low=br+(x-pm)/10*ci_low,
         y_high=br+(x-pm)/10*ci_high)


ggplot(response_pm,aes(x,group=cold))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=cold),
              alpha = 0.4)+
  geom_line(aes(y=y,col=cold),linewidth=1)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,cold==F),binwidth = 0.5,
                 alpha=0.4,fill="#8B0000",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,cold==T),binwidth = 0.5,
                 alpha=0.4,fill="#1A237E",col="white")+
  annotate("text", x = 30, y = 1.2, label = "Summer & Spring", color = "#8B0000")+
  annotate("text", x = 30, y = 3.6, label = "Winter & Fall", color = "#1A237E")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#1A237E", "FALSE" = "#8B0000"))+
  scale_fill_manual(values = c("TRUE" = "#1A237E80", "FALSE" = "#8B000080"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Season_se.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Temperature Figure -----

# TO DO


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


# Urban
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                     offset(log(pop75)), 
                   data = filter(df,comRural==F),
                   na.action=na.omit)

cluster_se <- vcovCL(model_nb, cluster = filter(df,comRural==F)$commune)
coef_est <- coef(model_nb)
coef_se <- sqrt(diag(cluster_se)) 
ci_lower <- coef_est - 1.96 * coef_se
ci_upper <- coef_est + 1.96 * coef_se

# Rural
model_nb2 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                      offset(log(pop75)), 
                    data = filter(df,comRural==T),
                    na.action=na.omit)
cluster_se2 <- vcovCL(model_nb2, cluster =filter(df,comRural==T)$commune)
coef_est2 <- coef(model_nb2)
coef_se2 <- sqrt(diag(cluster_se2)) 
ci_lower2 <- coef_est2 - 1.96 * coef_se2
ci_upper2 <- coef_est2 + 1.96 * coef_se2

## PM2.5 Figure -----
# order if False (urban) and true (rural)
br <- df %>% group_by(comRural) %>% summarize(x = weighted.mean(MR_all_cause,w=pop75)) %>% pull(x)
pm <-  df %>% group_by(comRural) %>% summarize(x = weighted.mean(pm25_exposure,w=pop75)) %>% pull(x)


df %>% group_by(comRural) %>% reframe(range(pm25_exposure))
df %>% group_by(comRural) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))

pm_range <- 6:58 ## 99 percentile
pm_range_rural <- 5:58


# option 2
rr <- data.frame(comRural=c(F,T),
                 mean_rr=c(exp(coef_est["pm25Exp_10ug"]),exp(coef_est2["pm25Exp_10ug"])),
                 ci_low=c(exp(ci_lower["pm25Exp_10ug"]),exp(ci_lower2["pm25Exp_10ug"])),
                 ci_high=c(exp(ci_upper["pm25Exp_10ug"]),exp(ci_upper2["pm25Exp_10ug"])))


# slope assumed linear (constant). Slope is per 10 ug/m3 change
rr$br <- br
rr$pm <- pm
rr <- rr %>% pivot_longer(c(-comRural,-br,-pm), names_to = "est", values_to = "rr") %>% 
  mutate(slope=(rr-1)*br) %>% dplyr::select(-rr) %>% 
  pivot_wider(names_from = est, values_from = slope)


# create dataframe with points for lines
response_pm <- tibble(x=pm_range);response_pm$comRural <- F;
response_pm2 <- tibble(x=pm_range_rural);response_pm2$comRural <- T;
response_pm <- rbind(response_pm,response_pm2);rm(response_pm2)

response_pm <- response_pm %>% left_join(rr)

response_pm <- response_pm %>% 
  mutate(y=br+(x-pm)/10*mean_rr, # Check: Simple linear slope starting from means
         y_low=br+(x-pm)/10*ci_low,
         y_high=br+(x-pm)/10*ci_high)


ggplot(response_pm,aes(x,group=comRural))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=comRural),
              alpha = 0.4)+
  geom_line(aes(y=y,col=comRural),linewidth=1)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,comRural==F),binwidth = 0.5,
                 alpha=0.4,fill="#333333",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,comRural==T),binwidth = 0.5,
                 alpha=0.4,fill="#F4A460",col="white")+
  annotate("text", x = 30, y = 1.2, label = "Urban", color = "#333333")+
  annotate("text", x = 30, y = 3.6, label = "Rural", color = "#F4A460")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#F4A460", "FALSE" = "#333333"))+
  scale_fill_manual(values = c("TRUE" = "#F4A46080", "FALSE" = "#33333380"))+
  labs(x=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]")),
       y=expression(paste("75+ Mortality Rate All-Cause [per 1,000 habs]","")))+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Effect_Urban.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Temperature Figure -----

# TO DO




# EoF