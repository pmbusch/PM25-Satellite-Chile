## Different Models comparisons by Model specification
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")

df <- df %>% filter(!is.na(pm25Exp_10ug))

df <- df %>% mutate(quarter=factor(quarter),
                    year=as.factor(year),
                    commune=as.factor(codigo_comuna),
                    commune=relevel(commune,ref="13101")) # Santiago


# Function to get data from fitted models ----
getModelInfo <- function(mod,name){
  
  coef <- summary(mod)$coefficients
 
  # clustered standard errors by commune
  cluster_se <- vcovCL(mod, cluster = df$commune)
  cluster_se <- sqrt(diag(cluster_se))
  
  # all
  out <- data.frame(name=name,est=coef[,1],se=cluster_se,N=nobs(mod),bic=BIC(mod)) %>% 
    mutate(rr=exp(est)*100-100,
           rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
           rr_high=exp(est+1.96*se)*100-100)
  
  return(out)
}


## Negative Binomial forms ----
# year+quarter
model_nb<- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
models_nb_res <- getModelInfo(model_nb,"Year+Quarter")


# year*month: 1.006 (0.999-1.013)
mod_nb1 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year*month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb1,"Year*Month"))

# year+month: 1.006 (1.000-1.013)
mod_nb2 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb2,"Year+Month"))

# year+quarter (no month): 1.054 (1.044-1.065)
mod_nb3 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb3,"Year+Quarter"))

# year*quarter (no month): 1.056 (1.045-1.067)
mod_nb4 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb4,"Year*Quarter"))

# year*quarter + month: 1.008 (1.001-1.015)
mod_nb5 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year*quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb5,"Year*Quarter+Month"))

# year+quarter+month: 1.006 (1.000-1.013)
mod_nb6 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+quarter+month+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb6,"Year+Quarter+Month"))

# region*quarter
mod_nb7 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+REGION*quarter+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb7,"Region*Quarter"))

# region*year
mod_nb8 <- glm.nb(Mortality_Count ~ pm25Exp_10ug+REGION*year+commune+
                         offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb8,"Region*Year"))


# Figure 


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

model_nb_lags <- glm.nb(Mortality_Count ~ pm25Exp_10ug+
                          pm25Exp_lag12+pm25Exp_lag6+pm25Exp_lag3+pm25Exp_lag1+
                          pm25Exp_lead1+pm25Exp_lead6+pm25Exp_lead12+
                          year+quarter+commune+
                          offset(log(pop75)), 
                        data = df,
                        na.action=na.omit)

getModelInfo(model_nb_lags,"Lags") %>% view()
summary(model_nb_lags)
nobs(model_nb_lags)
BIC(model_nb_lags)
coef(model_nb_lags) %>% exp()
confint(model_nb_lags) %>% exp()
autoplot(model_nb_lags)


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