# Main Effects figure
#
# PBH June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

source("Scripts/Functions.R",encoding = "UTF-8")

fig_name <- "Figures/Effects/%s.png"

fig_fontsize <- 12
# fig_fontsize <- 6 # 6 for smaller panel

# guidelines
nat <- 20
who <- 5

# Load data -----

# Load Panel Data
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")
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


# RUN MODELS -----------
## Code to run models, figure can be generated with previously saved models

# SKIP TO FIGURES IF MODELS ARE ALREADY LOADED

# Two options: 
# (1) Load previously saved data coming from bootstraps runs - TAKES DAYS TO RUN
# (2) Use the standard errors from the models fitted

## Full Model -----

# Option 1
# boot <- read.csv2("Data/Bootstrap/Bootstrap_Main.csv")

# bootstrap option
# boot$pm25Exp_10ug <- as.numeric(boot$pm25Exp_10ug);boot$landTemp <- as.numeric(boot$landTemp);
# boot$pm25Exp_10ug <- boot$pm25Exp_10ug %>% exp()
# mean_rr <- mean(boot$pm25Exp_10ug)
# ci_rr <- quantile(boot$pm25Exp_10ug,c(0.025,0.975))


## Option 2 - Use SE from Regression Model
model_nb <- fitmod(df)
# get model info - Robust SE
out <- getModelInfo(model_nb,"Main")
# save model
write.csv(out,"Data/Main Effects/fullModel.csv",row.names = F)

## Metropolitan region and rest of country ----

# Met
model_nb1 <- fitmod(filter(df,REGION==13))
out1 <- getModelInfo(model_nb1,"Met",data_df = filter(df,REGION==13))
write.csv(out1,"Data/Main Effects/MetModel.csv",row.names = F)
# No MET
model_nb2 <- fitmod(filter(df,REGION!=13))
out2 <- getModelInfo(model_nb2,"NoMet",data_df = filter(df,REGION!=13))
write.csv(out2,"Data/Main Effects/NoMetModel.csv",row.names = F)

## Half of Year ----

rm(model_nb1,model_nb2)
df %>% group_by(month) %>% summarise(mean(pm25_exposure))
df <- df %>% mutate(cold = quarter %in% c("2","3"))
# Cold
model_nb1 <- fitmod(filter(df,cold==T))
out1 <- getModelInfo(model_nb1,"Cold",data_df = filter(df,cold==T))
write.csv(out1,"Data/Main Effects/ColdModel.csv",row.names = F)
# Hot
model_nb2 <- fitmod(filter(df,cold==F))
out2 <- getModelInfo(model_nb2,"Hot",data_df = filter(df,cold==F))
write.csv(out2,"Data/Main Effects/HotModel.csv",row.names = F)

## Urban and Rural --------

# Urban Share
com_rural <- df %>% group_by(commune) %>% 
  summarise(total_pop_urban=sum(total_pop_urban,na.rm=T)/12/18,
            total_pop_rural=sum(total_pop_rural,na.rm=T)/12/18) %>% ungroup() %>% 
  mutate(rural_share=total_pop_rural/(total_pop_rural+total_pop_urban)) %>% 
  arrange(desc(rural_share))
nrow(filter(com_rural,rural_share>0.3))/nrow(com_rural) # 
# Communes with more than 50% of rural habitants
com_rural <- com_rural %>% filter(rural_share>0.3) %>% pull(commune) # 
df <- df %>% mutate(comRural=(commune %in% com_rural))
df %>% group_by(comRural) %>% summarise(mean(pm25_exposure))
df  %>% group_by(comRural,REGION) %>% summarise(n=n()/12/18) %>% 
  pivot_wider(names_from = comRural, values_from = n)



rm(model_nb1,model_nb2)
# Urban
model_nb1 <- fitmod(filter(df,comRural==F))
out1 <- getModelInfo(model_nb1,"Urban",data_df = filter(df,comRural==F))
write.csv(out1,"Data/Main Effects/UrbanModel.csv",row.names = F)
# Rural
model_nb2 <- fitmod(filter(df,comRural==T))
out2 <- getModelInfo(model_nb2,"Rural",data_df = filter(df,comRural==T))
write.csv(out2,"Data/Main Effects/RuralModel.csv",row.names = F)

## PM2.5 Levels --------

# Average over the whole period
com_pm25 <- df %>% group_by(commune) %>% 
  summarise(pm25=mean(pm25_exposure), n=n()) %>% ungroup() %>% 
 arrange(desc(pm25))
# Cut-off - 20ug/m3 above and below
nrow(filter(com_pm25,pm25>20))/nrow(com_pm25) # 50.46%
com_pm25 <- com_pm25 %>% filter(pm25>20) %>% pull(commune) # 165
df <- df %>% mutate(comPM25=(commune %in% com_pm25))
df %>% group_by(comPM25) %>% summarise(mean(pm25_exposure))

rm(model_nb1,model_nb2)
# Below 20
model_nb1 <- fitmod(filter(df,comPM25==F))
out1 <- getModelInfo(model_nb1,"Below 20",data_df = filter(df,comPM25==F))
write.csv(out1,"Data/Main Effects/Belowpm25Model.csv",row.names = F)
# Above 20
model_nb2 <- fitmod(filter(df,comPM25==T))
out2 <- getModelInfo(model_nb2,"Above 20",data_df = filter(df,comPM25==T))
write.csv(out2,"Data/Main Effects/Abovepm25Model.csv",row.names = F)

## POP75+ Share --------

# Average over the whole period
com_75 <- df %>% group_by(REGION,commune) %>% 
  summarise(pop75_share=mean(pop75_share), n=n()) %>% ungroup() %>% 
  arrange(desc(pop75_share))
# ggplot(com_75,aes(pop75_share,col=factor(REGION)))+stat_ecdf()
# Cut-off - 4.5% POP75+ population is the MEDIAN!
median(com_75$pop75_share)
nrow(filter(com_75,pop75_share>0.045))/nrow(com_75) # 49.85%
com_75 <- com_75 %>% filter(pop75_share>0.045) %>% pull(commune) # 
df <- df %>% mutate(com75=(commune %in% com_75))
df %>% group_by(com75) %>% summarise(mean(pm25_exposure))

rm(model_nb1,model_nb2)
# Below 
model_nb1 <- fitmod(filter(df,com75==F))
out1 <- getModelInfo(model_nb1,"Below 4.5 pop",data_df = filter(df,com75==F))
write.csv(out1,"Data/Main Effects/Below75Model.csv",row.names = F)
# Above 
model_nb2 <- fitmod(filter(df,com75==T))
out2 <- getModelInfo(model_nb2,"Above 4.5 pop",data_df = filter(df,com75==T))
write.csv(out2,"Data/Main Effects/Above75Model.csv",row.names = F)


####
####
####
####
####


# FIGURES -----

## Full Model -----

# load model
out <- read.csv("Data/Main Effects/fullModel.csv")

### PM2.5 Figure -----
df$pm25_exposure %>% quantile(c(0.01,0.99)) # 99 percentile

# get line with CI
out_pm25 <- out %>% filter(param=="pm25Exp_10ug")
response_pm <- getRiskSlope(df, out_pm25, pm_range = 0:60)

# Figure
p1 <- ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high),
              alpha = 0.4,fill="#8B451380")+
  geom_line(aes(y=y),linewidth=0.5,col="#8B4513")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#8B4513",col="white")+
  annotate("text", x = 40, y = 1.2, size=10*5/14 * 0.8,
           label = "PM"[2.5] ~ " exposure distribution", color = "#8B4513")+
  # guidelines
  annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=3.6, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=3,label="Chile annual standard",angle = 90,size=9*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=3.6, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=3,label="WHO guidelines",angle = 90,size=9*5/14 * 0.8)+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1
ggsave(sprintf(fig_name,"Effect_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

p1 <- last_plot()+labs(y="75+ all-cause monthly MR [per 1,000]")
p1
ggsave(sprintf(fig_name,"Effect_se2"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7/2)


### Temperature Figure -----

df$landTemp %>% range()
df$landTemp %>% quantile(c(0.01,0.99))

response_pm <- getRiskSlope(df, filter(out,param=="landTemp"), 
                            pm_range = 0:45,temp = T)
ggplot(response_pm,aes(x))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high),
              alpha = 0.4,fill="#00008B80")+
  geom_line(aes(y=y),linewidth=0.5,col="#00008B")+
  geom_histogram(aes(landTemp,y=after_stat(density)*50,weight=pop75),
                 data=df,binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#00008B",col="white")+
  annotate("text", x = 30, y = 2.2, size=10*5/14 * 0.8,
           label = "Land temperature distribution", color = "#00008B")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,9,2)),limits = c(0,9)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(sprintf(fig_name,"EffectTemp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####

## Metropolitan region and rest of country ----

# load model
out1 <- read.csv("Data/Main Effects/MetModel.csv")
out2 <- read.csv("Data/Main Effects/NoMetModel.csv")

### PM2.5 Figure -----
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

p2 <- ggplot(response_pm,aes(x,group=met))+
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
  annotate("text", x = 35, y = 1.2, size=fig_fontsize*5/14 * 0.8,
           label = "Rest of country", color = "#2ecc71")+
  annotate("text", x = 35, y = 3.2, size=fig_fontsize*5/14 * 0.8,
           label = "Metropolitan region", color = "#9b59b6")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#9b59b6", "FALSE" = "#2ecc71"))+
  scale_fill_manual(values = c("TRUE" = "#9b59b680", "FALSE" = "#2ecc7180"))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p2 <- p2+labs(y="75+ all-cause monthly MR [per 1,000]")
p2
ggsave(sprintf(fig_name,"Effect_Met_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)
p2+
  annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=3.5,label="Chile annual standard",angle = 90,size=fig_fontsize*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=3.5,label="WHO guidelines",angle = 90,size=fig_fontsize*5/14 * 0.8)
ggsave(sprintf(fig_name,"Effect_Met_se_guide"), ggplot2::last_plot(),
         units="cm",dpi=500,
         width=8.7,height=8.7)

### Temperature Figure -----

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
  annotate("text", x = 30, y = 1.2,size=fig_fontsize*5/14 * 0.8, 
           label = "Rest of country", color = "#117744")+
  annotate("text", x = 30, y = 3.2,size=fig_fontsize*5/14 * 0.8, 
           label = "Metropolitan region", color = "#114477")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#114477", "FALSE" = "#117744"))+
  scale_fill_manual(values = c("TRUE" = "#77AADD", "FALSE" = "#44AA77"))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Temp_Met"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

####
####
####


## Half of year -----

df %>% group_by(month) %>% summarise(mean(pm25_exposure))
df <- df %>% mutate(cold = quarter %in% c("2","3"))

# load model
out1 <- read.csv("Data/Main Effects/ColdModel.csv")
out2 <- read.csv("Data/Main Effects/HotModel.csv")

### PM2.5 Figure -----

df %>% group_by(cold) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,cold==T), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 10:62)
response_pm2 <- getRiskSlope(filter(df,cold==F), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 5:24)
response_pm1$cold <- T; response_pm2$cold <- F
response_pm <- rbind(response_pm1,response_pm2)

p_season <- ggplot(response_pm,aes(x,group=cold))+
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
  annotate("text", x = 32, y = 0.7,size=fig_fontsize*5/14 * 0.8, 
           label = "Summer & Spring", color = "#8B0000")+
  annotate("text", x = 30, y = 3.6,size=fig_fontsize*5/14 * 0.8, 
           label = "Winter & Fall", color = "#1A237E")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#1A237E", "FALSE" = "#8B0000"))+
  scale_fill_manual(values = c("TRUE" = "#1A237E80", "FALSE" = "#8B000080"))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p_season
ggsave(sprintf(fig_name,"Effect_Season_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

p_season+annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=1.4,label="Chile annual standard",angle = 90,size=fig_fontsize*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=1.4,label="WHO guidelines",angle = 90,size=fig_fontsize*5/14 * 0.8)
ggsave(sprintf(fig_name,"Effect_Season_se_guide"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


### Temperature Figure -----

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
  annotate("text", x = 20, y = 1.2,size=fig_fontsize*5/14 * 0.8, 
           label = "Summer & Spring", color = "#8B0000")+
  annotate("text", x = 20, y = 3.6,size=fig_fontsize*5/14 * 0.8, 
           label = "Winter & Fall", color = "#1A237E")+
   scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8.1)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,46))+
  scale_color_manual(values = c("TRUE" = "#1A237E", "FALSE" = "#8B0000"))+
  scale_fill_manual(values = c("TRUE" = "#1A237E80", "FALSE" = "#8B000080"))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Temp_Season_se"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Urban and Rural -----

# Urban Share
com_rural <- df %>% group_by(commune) %>% 
  summarise(total_pop_urban=sum(total_pop_urban,na.rm=T)/12/18,
            total_pop_rural=sum(total_pop_rural,na.rm=T)/12/18) %>% ungroup() %>% 
  mutate(rural_share=total_pop_rural/(total_pop_rural+total_pop_urban)) %>% 
  arrange(desc(rural_share))
nrow(filter(com_rural,rural_share>0.3))/nrow(com_rural) # 
# Communes with more than 50% of rural habitants
com_rural <- com_rural %>% filter(rural_share>0.3) %>% pull(commune) # 

df <- df %>% mutate(comRural=(commune %in% com_rural))
df %>% group_by(comRural) %>% summarise(mean(pm25_exposure))

# load model
out1 <- read.csv("Data/Main Effects/UrbanModel.csv")
out2 <- read.csv("Data/Main Effects/RuralModel.csv")


### PM2.5 Figure -----
df %>% group_by(comRural) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,comRural==F), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 6:58)
response_pm2 <- getRiskSlope(filter(df,comRural==T), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 5:58)
response_pm1$comRural <- F; response_pm2$comRural <- T
response_pm <- rbind(response_pm1,response_pm2)


p3 <- ggplot(response_pm,aes(x,group=comRural))+
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
  annotate("text", x = 26, y = 1.2,size=fig_fontsize*5/14 * 0.8, 
           label = "Urban", color = "#333333")+
  annotate("text", x = 26, y = 3.6, size=fig_fontsize*5/14 * 0.8,
           label = "Rural", color = "#F4A460")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#F4A460", "FALSE" = "#333333"))+
  scale_fill_manual(values = c("TRUE" = "#F4A46080", "FALSE" = "#33333380"))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p3 <- p3+labs(y="75+ all-cause monthly MR [per 1,000]")
p3
ggsave(sprintf(fig_name,"Effect_Urban"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

p3+annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=3.5, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=1.5,label="Chile annual standard",angle = 90,size=fig_fontsize*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=3.5, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=1.5,label="WHO guidelines",angle = 90,size=fig_fontsize*5/14 * 0.8)
ggsave(sprintf(fig_name,"Effect_Urban_guide"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


### Temperature Figure -----

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
  annotate("text", x = 20, y = 1.2,size=fig_fontsize*5/14 * 0.8, 
           label = "Urban", color = "#333333")+
  annotate("text", x = 20, y = 3.6, size=fig_fontsize*5/14 * 0.8,
           label = "Rural", color = "#F4A460")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#F4A460", "FALSE" = "#333333"))+
  scale_fill_manual(values = c("TRUE" = "#F4A46080", "FALSE" = "#33333380"))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_Urban_temp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## PM2.5 Levels --------

# Average over the whole period
com_pm25 <- df %>% group_by(commune) %>% 
  summarise(pm25=mean(pm25_exposure), n=n()) %>% ungroup() %>% 
  arrange(desc(pm25))
# Cut-off - 20ug/m3 above and below
nrow(filter(com_pm25,pm25>20))/nrow(com_pm25) # 50.46%
com_pm25 <- com_pm25 %>% filter(pm25>20) %>% pull(commune) # 165
df <- df %>% mutate(comPM25=(commune %in% com_pm25))

# load model
out1 <- read.csv("Data/Main Effects/Belowpm25Model.csv")
out2 <- read.csv("Data/Main Effects/Abovepm25Model.csv")

### PM2.5 Figure -----
df %>% group_by(comPM25) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,comPM25==F), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 6:38)
response_pm2 <- getRiskSlope(filter(df,comPM25==T), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 6:62)
response_pm1$comPM25 <- F; response_pm2$comPM25 <- T
response_pm <- rbind(response_pm1,response_pm2)

p4 <- ggplot(response_pm,aes(x,group=comPM25))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=comPM25),
              alpha = 0.4)+
  geom_line(aes(y=y,col=comPM25),linewidth=0.5)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,comPM25==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#CC9900",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,comPM25==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#A80000",col="white")+
  # had to divide to get the two lines
  # annotate("text", x = 36, y = 1.2,size=fig_fontsize*5/14 * 0.8, 
  #          label =expression(paste("Communes with a mean ")), 
  #          color = "#CC9900")+
  annotate("text", x = 36, y = 1,size=fig_fontsize*5/14 * 0.8, 
           label =expression(paste(PM[2.5]," below 20 ",mu,"g/",m^3,"","")), 
           color = "#CC9900")+
  # annotate("text", x = 36, y = 3.7, size=fig_fontsize*5/14 * 0.8,
  #          label = expression(paste("Communes with a mean ")),
  #          color = "#A80000")+
  annotate("text", x = 36, y = 3.5, size=fig_fontsize*5/14 * 0.8,
           label = expression(paste(PM[2.5]," above 20 ",mu,"g/",m^3,"","")),
           color = "#A80000")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#A80000", "FALSE" = "#CC9900"))+
  scale_fill_manual(values = c("TRUE" = "#A80000AA", "FALSE" = "#CC9900AA"))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
p4 <- p4+labs(y="75+ all-cause monthly MR [per 1,000]")
p4
ggsave(sprintf(fig_name,"Effect_AbovePM25"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

p4+annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=1.5,label="Chile annual standard",angle = 90,size=fig_fontsize*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=1.5,label="WHO guidelines",angle = 90,size=fig_fontsize*5/14 * 0.8)
ggsave(sprintf(fig_name,"Effect_AbovePM25_guide"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


### Temperature Figure -----

df %>% group_by(comPM25) %>% reframe(quantile(landTemp,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,comPM25==F), 
                             filter(out1,param=="landTemp"), 
                             pm_range = 6:46,temp=T)
response_pm2 <- getRiskSlope(filter(df,comPM25==T), 
                             filter(out2,param=="landTemp"), 
                             pm_range = 3:41,temp=T)
response_pm1$comPM25 <- F; response_pm2$comPM25 <- T
response_pm <- rbind(response_pm1,response_pm2)


ggplot(response_pm,aes(x,group=comPM25))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=comPM25),
              alpha = 0.4)+
  geom_line(aes(y=y,col=comPM25),linewidth=0.5)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,comPM25==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#CC9900",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,comPM25==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#A80000",col="white")+
  annotate("text", x = 20, y = 1.2,size=(fig_fontsize-1)*5/14 * 0.8, 
           label = expression(paste("",PM[2.5]," below 20 ",mu,"g/",m^3,"","")), 
           color = "#CC9900")+
  annotate("text", x = 20, y = 3.6, size=(fig_fontsize-1)*5/14 * 0.8,
           label = expression(paste("",PM[2.5]," above 20 ",mu,"g/",m^3,"","")), 
           color = "#A80000")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#A80000", "FALSE" = "#CC9900"))+
  scale_fill_manual(values = c("TRUE" = "#A80000AA", "FALSE" = "#CC9900AA"))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_AbovePM25_temp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

## Above and below 75+ population share --------

# Average over the whole period
com_75 <- df %>% group_by(REGION,commune) %>% 
  summarise(pop75_share=mean(pop75_share), n=n()) %>% ungroup() %>% 
  arrange(desc(pop75_share))
# ggplot(com_75,aes(pop75_share,col=factor(REGION)))+stat_ecdf()
# Cut-off - 4.5% POP75+ population is the MEDIAN!
median(com_75$pop75_share)
nrow(filter(com_75,pop75_share>0.045))/nrow(com_75) # 49.85%
com_75 <- com_75 %>% filter(pop75_share>0.045) %>% pull(commune) # 
df <- df %>% mutate(com75=(commune %in% com_75))


# load model
out1 <- read.csv("Data/Main Effects/Below75Model.csv")
out2 <- read.csv("Data/Main Effects/Above75Model.csv")

### PM2.5 Figure -----
df %>% group_by(com75) %>% reframe(mean(pm25_exposure))
df %>% group_by(com75) %>% reframe(quantile(pm25_exposure,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,com75==F), 
                             filter(out1,param=="pm25Exp_10ug"), 
                             pm_range = 6:57)
response_pm2 <- getRiskSlope(filter(df,com75==T), 
                             filter(out2,param=="pm25Exp_10ug"), 
                             pm_range = 5:59)
response_pm1$com75 <- F; response_pm2$com75 <- T
response_pm <- rbind(response_pm1,response_pm2)

p5 <- ggplot(response_pm,aes(x,group=com75))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=com75),
              alpha = 0.4)+
  geom_line(aes(y=y,col=com75),linewidth=0.5)+
  # histograms
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,com75==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#004E7A",col="white")+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2.4),
                 data=filter(df,com75==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#FF6B6B",col="white")+
  annotate("text", x = 37, y = 1,size=fig_fontsize*5/14 * 0.8, 
           label ="75+ pop. share below 4.5%", 
           color = "#004E7A")+
  annotate("text", x = 37, y = 3.4, size=fig_fontsize*5/14 * 0.8,
           label = "75+ pop. share above 4.5%", 
           color = "#FF6B6B")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,60,10)),limits = c(0,60))+
  scale_color_manual(values = c("TRUE" = "#FF6B6B", "FALSE" = "#004E7A"))+
  scale_fill_manual(values = c("TRUE" = "#FF6B6BAA", "FALSE" = "#004E7AAA"))+
  labs(x=lab_pm25,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

p5 <- p5+labs(y="75+ all-cause monthly MR [per 1,000]")
p5
ggsave(sprintf(fig_name,"Effect_AbovePop75"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

p5+annotate("rect", xmin=nat, xmax=nat, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=nat+1,y=1.5,label="Chile annual standard",angle = 90,size=fig_fontsize*5/14 * 0.8)+
  annotate("rect", xmin=who, xmax=who, ymin=0, ymax=4, color = "black", linetype="dashed",linewidth=0.1)+ 
  annotate("text",x=who+1,y=1.5,label="WHO guidelines",angle = 90,size=fig_fontsize*5/14 * 0.8)
ggsave(sprintf(fig_name,"Effect_AbovePop75_guide"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


### Temperature Figure -----

df %>% group_by(com75) %>% reframe(quantile(landTemp,c(0.01,0.99)))
response_pm1 <- getRiskSlope(filter(df,com75==F), 
                             filter(out1,param=="landTemp"), 
                             pm_range = 2:45,temp=T)
response_pm2 <- getRiskSlope(filter(df,com75==T), 
                             filter(out2,param=="landTemp"), 
                             pm_range = 4:42,temp=T)
response_pm1$com75 <- F; response_pm2$com75 <- T
response_pm <- rbind(response_pm1,response_pm2)

ggplot(response_pm,aes(x,group=com75))+
  geom_ribbon(aes(ymin = y_low,
                  ymax = y_high,fill=com75),
              alpha = 0.4)+
  geom_line(aes(y=y,col=com75),linewidth=0.5)+
  # histograms
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75),
                 data=filter(df,com75==F),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#004E7A",col="white")+
  geom_histogram(aes(landTemp,y=after_stat(density)*20,weight=pop75), position = position_nudge(y=2),
                 data=filter(df,com75==T),binwidth = 0.5,
                 linewidth=0.1,center=0,
                 alpha=0.4,fill="#FF6B6B",col="white")+
  annotate("text", x = 20, y = 1.2,size=(fig_fontsize-1)*5/14 * 0.8, 
           label = "Below 4.5% (median) 75+ population share", 
           color = "#004E7A")+
  annotate("text", x = 20, y = 3.6, size=(fig_fontsize-1)*5/14 * 0.8,
           label ="Above 4.5% (median) 75+ population share", 
           color = "#FF6B6B")+
  scale_y_continuous(expand = c(0,0),breaks = c(seq(0,8,2)),limits = c(0,8)) +
  scale_x_continuous(expand = c(0,0),breaks = c(seq(0,40,10)),limits = c(0,45))+
  scale_color_manual(values = c("TRUE" = "#FF6B6B", "FALSE" = "#004E7A"))+
  scale_fill_manual(values = c("TRUE" = "#FF6B6BAA", "FALSE" = "#004E7AAA"))+
  labs(x=lab_temp,y=lab_mr2)+
  theme_bw(fig_fontsize)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(sprintf(fig_name,"Effect_AbovePop75_temp"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)



# COMBINE PANEL FIGURES -----------

# Need to Run Figure_Demeaning script to get p_dem
# need to run figures code to get those figures

library(gridExtra)
# 
# # Combine them and label them
# p <- grid.arrange(arrangeGrob(p_dem+ # left side
#                                 annotate("text", x = 1, y = 41, size=14*5/14 * 0.8,label = "A"),nrow = 1), 
#              arrangeGrob(p1 + annotate("text", x = 4, y = 7, size=14*5/14 * 0.8,label = "B"), # right side top
#                          arrangeGrob(p2+annotate("text", x = 4, y = 7, size=14*5/14 * 0.8,label = "C"),
#                                      p3+annotate("text", x = 4, y = 7, size=14*5/14 * 0.8,label = "D"),
#                                      ncol = 2), # right side bottom
#                          nrow = 2),
#              ncol = 2)
# p
# ggsave(sprintf(fig_name,"AllEffect"), p,
#        units="cm",dpi=500,
#        width=8.7*2,height=8.7)

# EoF