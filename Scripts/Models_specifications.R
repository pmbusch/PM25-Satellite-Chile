## Different Models comparisons by Model specification
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Fucntions.R",encoding="UTF-8")


# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")

df <- df %>% filter(!is.na(pm25Exp_10ug))

df <- df %>% mutate(quarter=factor(quarter),
                    year=as.factor(year),
                    region=as.factor(REGION),
                    month=as.factor(month),
                    commune=as.factor(codigo_comuna),
                    commune=relevel(commune,ref="13101")) # Santiago



## Negative Binomial forms ----
# year+quarter
model_nb<- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
models_nb_res <- getModelInfo(model_nb,"Year+Quarter")


# year*month
mod_nb1 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb1,"Year*Month"))

# year+month
mod_nb2 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb2,"Year+Month"))

# year+quarter (no month)
mod_nb3 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb3,"Year+Quarter"))

# year*quarter (no month)
mod_nb4 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb4,"Year*Quarter"))

# year*quarter + month
mod_nb5 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb5,"Year*Quarter+Month"))

# year+quarter+month
mod_nb6 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb6,"Year+Quarter+Month"))

# region*quarter
mod_nb7 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+region*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7,"Region*Quarter"))

mod_nb7a <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year*month+region*quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7a,"Region*Quarter+Year*Month"))

mod_nb7b <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+month+region*quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7b,"Region*Quarter+Year+Month"))


# region*year
mod_nb8 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+region*year+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb8,"Region*Year"))

# year + quarter -landTemp
mod_nb9 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+region*year+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb9,"No Temperature (Year+Quarter)"))

# Land Temp Squared year +quarter
mod_nb10 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+I(landTemp^2)+region*year+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb10,"T + T^2 (Year+Quarter)"))


# save results
write.csv(models_nb_res,"Data/modelSpecResults.csv",row.names = F)
models_nb_res <- read.csv("Data/modelSpecResults.csv")

# Figure 
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
ggplot(aes(reorder(name,rowname,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(title="Base Model: MR ~ PM2.5+T°+Commune+...",x="Additional Terms",
       y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
       # y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Model_Specifications.png", ggplot2::last_plot(),
# ggsave("Figures//Model/Model_Specifications_Temp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# Month coefficients
month_levels <- paste0("month",12:2)
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(str_detect(param,"month"),!str_detect(param,":")) %>% 
  mutate(param=factor(param,levels=month_levels)) %>% 
  ggplot(aes(param,rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  facet_wrap(~name)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",y=expression(paste("Percentage increase in Mortality w.r.t. Month 1")))+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures//Model/Model_Specifications_MonthEffect.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)

# Model with lags -----
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  group_by(codigo_comuna) %>% 
  mutate(pm25Exp_lag12=lag(pm25Exp_10ug,12,order_by=count_month),
         pm25Exp_lag6=lag(pm25Exp_10ug,6,order_by=count_month),
         pm25Exp_lag3=lag(pm25Exp_10ug,3,order_by=count_month),
         pm25Exp_lag1=lag(pm25Exp_10ug,1,order_by=count_month),
         pm25Exp_lead1=lead(pm25Exp_10ug,1,order_by=count_month),
         pm25Exp_lead6=lead(pm25Exp_10ug,6,order_by=count_month),
         pm25Exp_lead12=lead(pm25Exp_10ug,12,order_by=count_month))

model_nb_lags <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                          pm25Exp_lag12+pm25Exp_lag6+pm25Exp_lag3+pm25Exp_lag1+
                          pm25Exp_lead1+pm25Exp_lead6+pm25Exp_lead12+
                          year+quarter+commune+
                          offset(log(pop75)), 
                        data = df,
                        na.action=na.omit)

x <- getModelInfo(model_nb_lags,"Lags")

x[c(2,4:10),] %>% 
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

ggsave("Figures//Model/Model_Lags.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)


# Model with all lags
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
                          pm25Exp_lead1+pm25Exp_lead2+pm25Exp_lead3+pm25Exp_lead4+pm25Exp_lead5+
                          pm25Exp_lead6+pm25Exp_lead7+pm25Exp_lead8+pm25Exp_lead9+pm25Exp_lead10+
                          pm25Exp_lead11+pm25Exp_lead12+
                          year+quarter+commune+
                          offset(log(pop75)), 
                        data = df,
                        na.action=na.omit)

x <- getModelInfo(model_nb_lags,"AllLags")

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

ggsave("Figures//Model/Model_AllLags.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)



# # LM ----
model_lm <- glm(mortality ~ pm25Exp_10ug+year+region*quarter, 
                weights = pop75,
                data = df,
                family =  gaussian(link = "log"),
                na.action=na.omit)

summary(model_lm)
nobs(model_lm)
BIC(model_lm)
coef(model_lm) %>% exp()
confint(model_lm) %>% exp()
autoplot(model_lm)

## Gamma regression ----
model_gamma <- glm(mortality ~ pm25Exp_10ug+year+region*quarter, 
                   weights = pop75,
                   data = df,
                   family =  Gamma(link = "log"),
                   na.action=na.omit)

summary(model_gamma)
nobs(model_gamma)
BIC(model_gamma)
coef(model_gamma) %>% exp()
confint(model_gamma) %>% exp()
autoplot(model_gamma)


## Random Effects Region-----
model_random <- glmer.nb(Mortality_Count ~ pm25Exp_10ug+year+(1 | region)+
                           offset(log(pop75)), 
                         data = df,
                         na.action=na.omit)

summary(model_random)
nobs(model_random)
BIC(model_random)
fixef(model_random) %>% exp()
confint(model_random, method="Wald") %>% exp()

## Random Effects Comune-----
model_random_com <- glmer.nb(Mortality_Count ~ pm25Exp_10ug+year+(1 | commune)+
                               offset(log(pop75)), 
                             data = df,
                             na.action=na.omit)

summary(model_random_com)
nobs(model_random_com)
BIC(model_random_com)
fixef(model_random_com) %>% exp()
confint(model_random_com, method="Wald") %>% exp()





# EoF