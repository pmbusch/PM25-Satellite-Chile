## Spline with PM2.5 Models
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

# PM25 spline ----------


## Prediction function to see effect ----
# Better to plot to see the effect
# https://stats.stackexchange.com/questions/503985/interpretation-of-cubic-spline-coefficients-in-r

# get predictions with a function
data_used <- df %>% dplyr::select(death_count_all_cause,
                                  pm25_exposure,
                                  pm25Exp_10ug,landTemp,year_quarter,
                                  commune,pop75,MR_all_cause) %>% na.omit()

# Support base
pm25_support <- seq(0.5,6,0.1)
br <- weighted.mean(data_used$MR_all_cause,data_used$pop75)  
pm <- weighted.mean(data_used$pm25Exp_10ug,data_used$pop75) 
pop <- sum(data_used$pop75)
deat <- sum(data_used$death_count_all_cause)  
deat/pop*1000

# Key idea: make predictions with new dataset changing only the PM2.5. base data
# partially based on: 
# https://github.com/sheftneal/HBBB2018/blob/master/scripts/07_FigureED3.R
f.getSplinePredictions <- function(new_base,model,se=F){
  
  N <- length(new_base)
  # create base data for prediction, there is no interaction with PM2.5
  x <- data_used[which.max(complete.cases(data_used)), ]
  df_new <- do.call(rbind, lapply(1:N, function(i) x))
  df_new$pm25Exp_10ug <- with(data_used,seq(min(pm25Exp_10ug, na.rm=TRUE), 
                                            max(pm25Exp_10ug, na.rm=TRUE), length.out=N))
  
  # predictions  
  ind <- which(new_base==round(pm,1)) # mean of pm2.5
  if (se){
    y <- predict(model, newdata=df_new,type = "response",se.fit = T) # death counts
    y_low = y$fit - y$se.fit*1.96
    y_high = y$fit + y$se.fit*1.96
    y = y$fit
    y = y/x$pop75*1000;y_low = y_low/x$pop75*1000;y_high = y_high/x$pop75*1000;
    y = y - y[ind] + br;y_low = y_low - y_low[ind] + br;y_high = y_high - y_high[ind] + br;    
    return(list(y=y,y_low=y_low,y_high=y_high))
    
  } else {
    y <- predict(model, newdata=df_new,type = "response",se.fit = F) # death counts
    y = y/x$pop75*1000
    # Re-center to base of MR
    y = y - y[ind] + br
    return(y)
  }
}


## NB Models Spline PM2.5 ----------

# run models
mod_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                   offset(log(pop75)), data = df,na.action=na.omit)
nobs(mod_nb)


# Numbers in model names are not meaningful

# 3 knots equally spaced (quantiles)
mod_nb12 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# Knots
# df$pm25Exp_10ug %>% range()
# attr(terms(mod_nb12),"predvars") 
# Compare to base model
lmtest::lrtest(model_nb, mod_nb12) # Spline is better is better

# Knto at 20 ug/m3
mod_nb13 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = 20/10)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# mod_nb14 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25,35)/10)+landTemp+year_quarter+commune+
#                      offset(log(pop75)), data = df,na.action=na.omit)
# mod_nb15 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp+year_quarter+commune+
#                      offset(log(pop75)), data = df,na.action=na.omit)

# 3 knots equally spaced (quantiles) for PM2.5 and Land Temp.
mod_nb16 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+ns(landTemp, df = 3)+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# PM2.5 with quadratic term
mod_nb17 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)

# mod_nb18 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+I(pm25Exp_10ug^3)+
#                      landTemp+year_quarter+commune+offset(log(pop75)), data = df,na.action=na.omit)


# get effect of PM2.5 on MR over the range of PM2.5 values

response_a <- tibble::tibble(x =pm25_support)
y=f.getSplinePredictions(pm25_support,mod_nb,se=T)
y_low=y$y_low
y_high=y$y_high
y=y$y
y12 = f.getSplinePredictions(pm25_support,mod_nb12)
y13 = f.getSplinePredictions(pm25_support,mod_nb13)
# y14 = f.getSplinePredictions(pm25_support,mod_nb14)
# y15 = f.getSplinePredictions(pm25_support,mod_nb15)
y16 = f.getSplinePredictions(pm25_support,mod_nb16)
y17 = f.getSplinePredictions(pm25_support,mod_nb17)
# y18 = f.getSplinePredictions(pm25_support,mod_nb18)


# proportion of observations above 40 ug/m3
nrow(filter(data_used,pm25_exposure>40))/nrow(data_used) # 10%
# ggplot(data_used,aes(pm25_exposure))+stat_ecdf()

# Pm2.5 to ug/m3
ci_plot <- response_a %>% 
  mutate(x=x*10)

# Plot response
response_a %>%
  mutate(Base=y,y_low=y_low,y_high=y_high,
         `spline (3 df)`=y12,`spline (1 knot at 20)`=y13,
         # `spline (4 knots)`=y14,`spline (4 df)`=y15,
         `spline (3 df)+ Temp (3 df)`=y16,
         # `x+x^2+x^3`=y18,
         `x+x^2`=y17) %>% 
  dplyr::select(-y_low,-y_high) %>% 
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  mutate(x=x*10) %>% 
  ggplot(aes(x))+
  geom_ribbon(data=ci_plot,
              aes(ymin = y_low,ymax = y_high),fill="grey",alpha = 0.3)+
  geom_line(aes(y=value,col=key),linewidth=1)+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*10,weight=pop75),
                 data=data_used,binwidth = 0.5,
                 linewidth=0.1,center=0,position = position_nudge(y=4.5),
                 alpha=0.4,fill="darkred",col="white")+
  coord_cartesian(ylim=c(4.5,6.5),xlim=c(0,60))+
  # theme(legend.position = c(0.5,0.3))+
  labs(x=lab_pm25,y=lab_mr2,col="")
  # labs(x="PM2.5 [ug/m3]",y="75+ MR [per 1,000]",col="")
ggsave("Figures/Model/Splines.png")


## Linear models -------
## Analysis by Nature paper
# https://github.com/sheftneal/HBBB2018/blob/master/scripts/07_FigureED3.R


model_ols <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+landTemp | year_quarter+commune,
                       data=df,weights=df$pop75)
model_12 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, df = 3)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_13 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, knots = 16/10)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_14 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25,35)/10)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_15 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_16 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, df = 3)+ns(landTemp, df = 3) | year_quarter+commune,
                      data=df,weights=df$pop75)
model_17 <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_18 <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+I(pm25Exp_10ug^3)+
                        landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)


# get predictions
data_used <- df %>% dplyr::select(death_count_all_cause,
                                  pm25_exposure,
                                  pm25Exp_10ug,landTemp,year_quarter,
                                  commune,pop75,MR_all_cause) %>% na.omit()
pm25_support <- seq(0.1,6,0.1)
br <- weighted.mean(data_used$MR_all_cause,data_used$pop75)  
pm <- weighted.mean(data_used$pm25Exp_10ug,data_used$pop75) 

summary(model_ols)$coefficients
slope <- coefficients(model_ols)[1]
se_slope <- summary(model_ols)$coefficients[1,2]

response_a <- tibble::tibble(x =pm25_support)

# Get from other models
x <- response_a$x
ind <- which(x==round(pm,1))
y = slope*x
y_low = x*(slope-1.96*se_slope)
y_high = x*(slope+1.96*se_slope)
y12 = as.numeric(t(as.matrix((model_12)$coefficients[1:3]))%*%t(ns(x,3)))
y13 = as.numeric(t(as.matrix((model_13)$coefficients[1:2]))%*%t(ns(x, knots = 16/10)))
y14 = as.numeric(t(as.matrix((model_14)$coefficients[1:5]))%*%t(ns(x, knots = c(10,15,25,35)/10)))				
y15 = as.numeric(t(as.matrix((model_15)$coefficients[1:4]))%*%t(ns(x,4)))				
y16 = as.numeric(t(as.matrix((model_16)$coefficients[1:3]))%*%t(ns(x,3)))				
y17 = x*model_17$coefficients[1]+x^2*model_17$coefficients[2]
y18 = x*model_18$coefficients[1]+x^2*model_18$coefficients[2]+x^3*model_18$coefficients[3]

# Re-center
y = y - y[ind] + br
y_low = y_low - y_low[ind] + br
y_high = y_high - y_high[ind] + br
y12 = y12 - y12[ind] + br
y13 = y13 - y13[ind] + br
y14 = y14 - y14[ind] + br
y15 = y15 - y15[ind] + br
y16 = y16 - y16[ind] + br
y17 = y17 - y17[ind] + br
y18 = y18 - y18[ind] + br


ci_plot <- response_a %>% 
  mutate(x=x*10)

# Plot
response_a %>%
  mutate(Base=y,y_low=y_low,y_high=y_high,
         `spline (3 df)`=y12,`spline (1 knot at 16)`=y13,
         `spline (4 knots)`=y14,`spline (4 df)`=y15,
         `spline (3 df)+ Temp (3 df)`=y16,
         `x+x^2`=y17,`x+x^2+x^3`=y18) %>% 
  dplyr::select(-y_low,-y_high) %>% 
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  mutate(x=x*10) %>% 
  ggplot(aes(x))+
  geom_ribbon(data=ci_plot,
              aes(ymin = y_low,ymax = y_high),fill="grey",alpha = 0.3)+
  geom_line(aes(y=value,col=key),linewidth=1)+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*20,weight=pop75),
                 data=data_used,binwidth = 0.5,
                 linewidth=0.1,center=0,position = position_nudge(y=3),
                 alpha=0.4,fill="darkred",col="white")+
  ylim(3,6.5)+
  # ylim(0,6.5)+
  xlim(0,60)+
  # theme(legend.position = c(0.5,0.3))+
  labs(x="PM2.5 [ug/m3]",y="75+ MR (per 1,000]",col="")
ggsave("Figures/Model/SplinesLinear.png")



# EoF