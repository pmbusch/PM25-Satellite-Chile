## Models with Lag terms
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")


# Load required data -----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

df <- df %>% filter(!is.na(pm25Exp_10ug))

df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago


# LAGS --------

## Model with lags -----
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  group_by(codigo_comuna) %>% 
  mutate(pm25Exp_lag12=lag(pm25Exp_10ug,12,order_by=count_month),
         pm25Exp_lag6=lag(pm25Exp_10ug,6,order_by=count_month),
         pm25Exp_lag5=lag(pm25Exp_10ug,5,order_by=count_month),
         pm25Exp_lag4=lag(pm25Exp_10ug,4,order_by=count_month),
         pm25Exp_lag3=lag(pm25Exp_10ug,3,order_by=count_month),
         pm25Exp_lag2=lag(pm25Exp_10ug,2,order_by=count_month),
         pm25Exp_lag1=lag(pm25Exp_10ug,1,order_by=count_month),
         pm25Exp_lead1=lead(pm25Exp_10ug,1,order_by=count_month),
         pm25Exp_lead6=lead(pm25Exp_10ug,6,order_by=count_month),
         pm25Exp_lead12=lead(pm25Exp_10ug,12,order_by=count_month))

model_nb_lags <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                          pm25Exp_lag1+
                          pm25Exp_lag2+pm25Exp_lag3+pm25Exp_lag4+
                          pm25Exp_lag5+pm25Exp_lag6+
                          # pm25Exp_lag12+pm25Exp_lag6+pm25Exp_lag3+pm25Exp_lag1+
                          year_quarter+
                          commune+
                          offset(log(pop75)), 
                        data = df,
                        na.action=na.omit)

x <- getModelInfo(model_nb_lags,"Lags")

x[c(2,4:9),] %>%
  # x[c(2,4),] %>%
  # x[c(2,4:7),] %>% 
  mutate(order_id=case_when(
    str_detect(param,"lag") ~ 10,
    str_detect(param,"lead") ~ 1000,
    T ~ 100)) %>% 
  arrange(desc(param)) %>% 
  mutate(param=param %>% str_remove("pm25Exp_") %>% str_replace("10ug","Same Period")) %>%
  mutate(row_id = row_number(),
         row_id=row_id+order_id) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(reorder(param,row_id,decreasing = T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # geom_point(size=1,col="red")+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # Modify theme to look good
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Model_Lags.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)



## Model with lags and leads -----
model_nb_lagsLeads <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                               pm25Exp_lag1+pm25Exp_lead1+
                               # pm25Exp_lag12+pm25Exp_lag6+pm25Exp_lag3+pm25Exp_lag1+
                               # pm25Exp_lead1+pm25Exp_lead6+pm25Exp_lead12+
                               # year+quarter+
                               year_quarter+
                               commune+
                               offset(log(pop75)), 
                             data = df,
                             na.action=na.omit)

x <- getModelInfo(model_nb_lagsLeads,"Lags")

x[c(2,4:5),] %>% 
  # x[c(2,4:10),] %>% 
  mutate(order_id=case_when(
    str_detect(param,"lag") ~ 10,
    str_detect(param,"lead") ~ 1000,
    T ~ 100)) %>% 
  mutate(param=param %>% str_remove("pm25Exp_") %>% str_replace("10ug","Same Period")) %>%
  mutate(row_id = row_number(),
         row_id=row_id+order_id) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(reorder(param,row_id,decreasing = T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # Modify theme to look good
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Model_LagsLeads.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)


# Model with 3 years of lags
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  group_by(codigo_comuna) %>% 
  mutate(
    pm25Exp_lag36=lag(pm25Exp_10ug,36,order_by=count_month),
    pm25Exp_lag35=lag(pm25Exp_10ug,35,order_by=count_month),
    pm25Exp_lag34=lag(pm25Exp_10ug,34,order_by=count_month),
    pm25Exp_lag33=lag(pm25Exp_10ug,33,order_by=count_month),
    pm25Exp_lag32=lag(pm25Exp_10ug,32,order_by=count_month),
    pm25Exp_lag31=lag(pm25Exp_10ug,31,order_by=count_month),
    pm25Exp_lag30=lag(pm25Exp_10ug,30,order_by=count_month),
    pm25Exp_lag29=lag(pm25Exp_10ug,29,order_by=count_month),
    pm25Exp_lag28=lag(pm25Exp_10ug,28,order_by=count_month),
    pm25Exp_lag27=lag(pm25Exp_10ug,27,order_by=count_month),
    pm25Exp_lag26=lag(pm25Exp_10ug,26,order_by=count_month),
    pm25Exp_lag25=lag(pm25Exp_10ug,25,order_by=count_month),
    pm25Exp_lag24=lag(pm25Exp_10ug,24,order_by=count_month),
    pm25Exp_lag23=lag(pm25Exp_10ug,23,order_by=count_month),
    pm25Exp_lag22=lag(pm25Exp_10ug,22,order_by=count_month),
    pm25Exp_lag21=lag(pm25Exp_10ug,21,order_by=count_month),
    pm25Exp_lag20=lag(pm25Exp_10ug,20,order_by=count_month),
    pm25Exp_lag19=lag(pm25Exp_10ug,19,order_by=count_month),
    pm25Exp_lag18=lag(pm25Exp_10ug,18,order_by=count_month),
    pm25Exp_lag17=lag(pm25Exp_10ug,17,order_by=count_month),
    pm25Exp_lag16=lag(pm25Exp_10ug,16,order_by=count_month),
    pm25Exp_lag15=lag(pm25Exp_10ug,15,order_by=count_month),
    pm25Exp_lag14=lag(pm25Exp_10ug,14,order_by=count_month),
    pm25Exp_lag13=lag(pm25Exp_10ug,13,order_by=count_month),
    pm25Exp_lag12=lag(pm25Exp_10ug,12,order_by=count_month),
    pm25Exp_lag11=lag(pm25Exp_10ug,11,order_by=count_month),
    pm25Exp_lag10=lag(pm25Exp_10ug,10,order_by=count_month),
    pm25Exp_lag9=lag(pm25Exp_10ug,9,order_by=count_month),
    pm25Exp_lag8=lag(pm25Exp_10ug,8,order_by=count_month),
    pm25Exp_lag7=lag(pm25Exp_10ug,7,order_by=count_month),
    pm25Exp_lag6=lag(pm25Exp_10ug,6,order_by=count_month),
    pm25Exp_lag5=lag(pm25Exp_10ug,5,order_by=count_month),
    pm25Exp_lag4=lag(pm25Exp_10ug,4,order_by=count_month),
    pm25Exp_lag3=lag(pm25Exp_10ug,3,order_by=count_month),
    pm25Exp_lag2=lag(pm25Exp_10ug,2,order_by=count_month),
    pm25Exp_lag1=lag(pm25Exp_10ug,1,order_by=count_month),
    pm25Exp_lead1=lead(pm25Exp_10ug,1,order_by=count_month),
    pm25Exp_lead2=lead(pm25Exp_10ug,2,order_by=count_month),
    pm25Exp_lead3=lead(pm25Exp_10ug,3,order_by=count_month),
    pm25Exp_lead4=lead(pm25Exp_10ug,4,order_by=count_month),
    pm25Exp_lead5=lead(pm25Exp_10ug,5,order_by=count_month),
    pm25Exp_lead6=lead(pm25Exp_10ug,6,order_by=count_month),
    pm25Exp_lead7=lead(pm25Exp_10ug,7,order_by=count_month),
    pm25Exp_lead8=lead(pm25Exp_10ug,8,order_by=count_month),
    pm25Exp_lead9=lead(pm25Exp_10ug,9,order_by=count_month),
    pm25Exp_lead10=lead(pm25Exp_10ug,10,order_by=count_month),
    pm25Exp_lead11=lead(pm25Exp_10ug,11,order_by=count_month),
    pm25Exp_lead12=lead(pm25Exp_10ug,12,order_by=count_month))

model_nb_lags <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                          pm25Exp_lag36+pm25Exp_lag35+pm25Exp_lag34+pm25Exp_lag33+pm25Exp_lag32+
                          pm25Exp_lag31+pm25Exp_lag30+pm25Exp_lag29+pm25Exp_lag28+pm25Exp_lag27+
                          pm25Exp_lag26+pm25Exp_lag25+pm25Exp_lag24+pm25Exp_lag23+pm25Exp_lag22+
                          pm25Exp_lag21+pm25Exp_lag20+pm25Exp_lag19+pm25Exp_lag18+pm25Exp_lag17+
                          pm25Exp_lag16+pm25Exp_lag15+pm25Exp_lag14+pm25Exp_lag13+pm25Exp_lag12+
                          pm25Exp_lag11+pm25Exp_lag10+pm25Exp_lag9+pm25Exp_lag8+pm25Exp_lag7+
                          pm25Exp_lag6+pm25Exp_lag5+pm25Exp_lag4+pm25Exp_lag3+pm25Exp_lag2+
                          pm25Exp_lag1+
                          year_quarter+commune+
                          offset(log(pop75)), 
                        data = df,
                        na.action=na.omit)

x <- getModelInfo(model_nb_lags,"AllLags")

xx <- x[c(2,4:39),] %>% 
  mutate(order_id=case_when(
    str_detect(param,"lag") ~ 10,
    str_detect(param,"lead") ~ 1000,
    T ~ 100)) %>% 
  mutate(param=param %>% str_remove("pm25Exp_") %>% str_replace("10ug","Same Period")) %>%
  mutate(row_id = row_number(),
         row_id=row_id+order_id) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) # significant at 5%

xx %>% 
  ggplot(aes(reorder(param,row_id,decreasing = F),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  geom_point(size=3,col="black",x="Same Period",y=xx[1,]$rr)+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  geom_vline(xintercept = c(12.5,24.5,36.5,37.5),col="grey")+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # Modify theme to look good
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90,hjust = 0.5, vjust = 0.5))

ggsave("Figures/Model/Model_AllLags.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)


model_nb_lagsLeads <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                               pm25Exp_lag36+pm25Exp_lag35+pm25Exp_lag34+pm25Exp_lag33+pm25Exp_lag32+
                               pm25Exp_lag31+pm25Exp_lag30+pm25Exp_lag29+pm25Exp_lag28+pm25Exp_lag27+
                               pm25Exp_lag26+pm25Exp_lag25+pm25Exp_lag24+pm25Exp_lag23+pm25Exp_lag22+
                               pm25Exp_lag21+pm25Exp_lag20+pm25Exp_lag19+pm25Exp_lag18+pm25Exp_lag17+
                               pm25Exp_lag16+pm25Exp_lag15+pm25Exp_lag14+pm25Exp_lag13+pm25Exp_lag12+
                               pm25Exp_lag11+pm25Exp_lag10+pm25Exp_lag9+pm25Exp_lag8+pm25Exp_lag7+
                               pm25Exp_lag6+pm25Exp_lag5+pm25Exp_lag4+pm25Exp_lag3+pm25Exp_lag2+
                               pm25Exp_lag1+
                               pm25Exp_lead1+pm25Exp_lead2+pm25Exp_lead3+pm25Exp_lead4+pm25Exp_lead5+
                               pm25Exp_lead6+pm25Exp_lead7+pm25Exp_lead8+pm25Exp_lead9+pm25Exp_lead10+
                               pm25Exp_lead11+pm25Exp_lead12+
                               year_quarter+commune+
                               offset(log(pop75)), 
                             data = df,
                             na.action=na.omit)

x <- getModelInfo(model_nb_lagsLeads,"AllLags")

xx <- x[c(2,4:51),] %>% 
  mutate(order_id=case_when(
    str_detect(param,"lag") ~ 10,
    str_detect(param,"lead") ~ 1000,
    T ~ 100)) %>% 
  mutate(param=param %>% str_remove("pm25Exp_") %>% str_replace("10ug","Same Period")) %>%
  mutate(row_id = row_number(),
         row_id=row_id+order_id) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) # significant at 5%
xx %>% 
  ggplot(aes(reorder(param,row_id,decreasing = F),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  geom_point(size=3,col="red",x="Same Period",y=xx[1,]$rr)+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  geom_vline(xintercept = c(12.5,24.5,36.5,37.5),col="grey")+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # Modify theme to look good
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90,hjust = 0.5, vjust = 0.5))

ggsave("Figures/Model/Model_AllLagsLeads.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)


# EoF