## Spline with PM2.5 Models
## PBH
## Figure S12
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
(br <- weighted.mean(data_used$MR_all_cause,data_used$pop75)) 
(pm <- weighted.mean(data_used$pm25Exp_10ug,data_used$pop75))
matrixStats::weightedMedian(data_used$pm25Exp_10ug,
                            data_used$pop75)
# pop <- sum(data_used$pop75)
# deat <- sum(data_used$death_count_all_cause)  
# deat/pop*1000

# Key idea: make predictions with new dataset changing only the PM2.5. base data
# partially based on: 
# https://github.com/sheftneal/HBBB2018/blob/master/scripts/07_FigureED3.R
# Does not work with SE, for that we use the linear approach
f.getSplinePredictions <- function(new_base,data_,model){
  
  N <- length(new_base)
  
  br <- weighted.mean(data_$MR_all_cause,data_$pop75) 
  pm <- weighted.mean(data_$pm25Exp_10ug,data_$pop75)
  
  # Calculate mean values for the continuous predictors
  mean_temp <- mean(data_$landTemp,weights=data_$pop75)
  mean_pop <- mean(data_$pop75)
  
  # create base data for prediction, there is no interaction with PM2.5
  
  # get mean data for continous variable, for rest wathever is fine
  # we adjust intercept to match mean later
  df_new <- tibble(pm25Exp_10ug = new_base) %>% 
    mutate(commune = "13101",
           year_quarter = "2002-1",
           landTemp=mean_temp,
           pop75=mean_pop)
  
  # predictions  
  y <- predict(model, newdata=df_new,type = "response") # death counts
  
  # adjust curve to match mean MR when mean PM2.5 
  ind <- which(new_base==round(pm,1)) # position of mean of pm2.5
  y = y/df_new$pop75*1000
  # Re-center to base of MR
  y = y - y[ind] + br
  return(y)
}

# test
# x <- f.getSplinePredictions(pm25_support,data_used,mod_nb)

## NB Models Spline PM2.5 ----------

# run models
mod_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                   offset(log(pop75)), data = df,na.action=na.omit)
nobs(mod_nb)

library(splines)

# Knot at 20 ug/m3
mod_knot20 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(20)/10)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# Knot at 10, 15 and 25
mod_knots3 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25)/10)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# DF 4 
mod_df4 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp+year_quarter+
                     commune+offset(log(pop75)), data = df,na.action=na.omit)
# PM2.5 with quadratic term
mod_quadratic <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)


# get effect of PM2.5 on MR over the range of PM2.5 values
response_a <- tibble::tibble(x =pm25_support)
# NOTE: linear response we need the SE, use function "getRiskSlope" 
# created in Figure2_and_3.R

# get line with CI
out_pm25 <- getModelInfo(mod_nb,"Base",data_df = data_used) %>% 
  filter(param=="pm25Exp_10ug")
response_pm <- getRiskSlope(data_used, out_pm25, pm_range = pm25_support*10)

y_low=response_pm$y_low
y_high=response_pm$y_high
y=response_pm$y

yknot20 = f.getSplinePredictions(pm25_support,data_used,mod_knot20)
yknot3 = f.getSplinePredictions(pm25_support,data_used,mod_knots3)
ydf4 = f.getSplinePredictions(pm25_support,data_used,mod_df4)
y_quadratic = f.getSplinePredictions(pm25_support,data_used,mod_quadratic)

# proportion of observations above 40 ug/m3
nrow(filter(data_used,pm25_exposure>40))/nrow(data_used) # 10%
nrow(filter(data_used,pm25_exposure<10))/nrow(data_used) # 16%
nrow(filter(data_used,pm25_exposure<5))/nrow(data_used) # 0.2%
nrow(filter(data_used,pm25_exposure<15))/nrow(data_used) # 41%
# ggplot(data_used,aes(pm25_exposure))+stat_ecdf()

# Pm2.5 to ug/m3
ci_plot <- response_a %>% 
  mutate(x=x*10)

# Plot response
data_fig <- response_a %>%
  mutate(Base=y,
         y_low=y_low,y_high=y_high,
         `spline (1 knot at 20)`=yknot20,
         `spline (3 knots at 10, 15 & 25)`=yknot3,
         `spline (4 df)`=ydf4,
         `quadratic term`=y_quadratic) %>% 
  dplyr::select(-y_low,-y_high) %>%
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  mutate(x=x*10)

ggplot(data_fig,aes(x))+
  geom_ribbon(data=ci_plot,
              aes(ymin = y_low,ymax = y_high),fill="#FF474C",
              alpha = 0.2)+
  geom_line(aes(y=value,col=key),linewidth=0.5,alpha=.9)+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*50,weight=pop75),
                 data=data_used,binwidth = 0.5,
                 linewidth=0.1,center=0,position = position_nudge(y=0),
                 alpha=0.4,fill="darkred",col="white")+
  coord_cartesian(ylim=c(0,8),xlim=c(0,60),expand=F)+
  # geom_text(aes(y=value,label=key,col=key),
  #                 data=filter(data_fig,x==60),
  #                 nudge_x = 1,hjust=0,nudge_y = c(1,2,-1,0,-0.5,0)/20,
  #                 size=8*5/14 * 0.8)+
  labs(x=lab_pm25,y=lab_mr2,
       col=expression(paste(PM[2.5]," functional form","")))+
  scale_x_continuous(breaks=c(0,20,40,60))+
  theme_bw(10)+
  # add bottom and vertical bar
  geom_segment(x = 0.01, xend = 0.01, yend = 8,y=0,col="black",linewidth=0.3)+
  geom_segment(y = 0.01, yend = 0.01, xend = 60,x=3,col="black",linewidth=0.3)+
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.72,0.4),
        legend.spacing.y = unit(0.01, 'cm'),
        legend.text = element_text(size=8),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/FigureS12.png",
       units="cm",dpi=600,
       width=8.7,height=8.7)
pdf("Figures/Model/FigureS12.pdf",
    width = 8.7/2.54, height =8.7/2.54)
ggplot2::last_plot()
dev.off()


# Key idea: make predictions with new dataset changing only the PM2.5. base data
# We calcualte all the levels (commune and quarter) and then average based on pop
# Idea is to get an "average" of categorical variables
# note: Effect of PM2.5 does not have an interaction with categorical variables
# so it does not matter, this results in the same figure
f.getSplinePredictionsAvg <- function(new_base,data_,model,se=F){
  
  # Calculate mean values for the continuous predictors
  mean_temp <- mean(data_$landTemp,weights=data_$pop75)
  mean_pop <- mean(data_$pop75)
  
  # Initialize an empty data frame to store predictions
  prediction_data <- data.frame()
  
  # Calculate population (weights) for each commune - year
  prop_commune <- data_ %>% 
    # filter(year_quarter=="2002-1",commune=="13101") %>% 
    group_by(commune,year_quarter) %>% 
    reframe(pop_weight=mean(pop75))
  # reframe(pop_weight=n())
  
  # Create data frame with all combinations
  prediction_grid <- expand.grid(
    pm25Exp_10ug = new_base,
    # all levels
    commune = unique(prop_commune$commune),
    year_quarter = unique(prop_commune$year_quarter)) %>% 
    mutate(landTemp=mean_temp,pop75=mean_pop) # average vaues
  
  if(se==F){
    
    # Predict the values 
    predictions <- predict(model, newdata=prediction_grid, type="response")
    
    # Add the proportions as weights
    prediction_grid <- prediction_grid %>% 
      left_join(prop_commune) %>% 
      group_by(pm25Exp_10ug) %>% 
      mutate(pop_weight=pop_weight/sum(pop_weight)) %>% ungroup()
    
    # add predictions
    prediction_grid$pred <- predictions
    
    # calculate Mortality Rate (predictions are death counts)
    summary_predictions <- prediction_grid %>%
      mutate(y=pred/pop75*1000)
    
    # filter by category closer to average base rate 
    pm_level = round(pm,1)
    level_sel <- summary_predictions %>% 
      filter(pm25Exp_10ug==pm_level) %>% 
      mutate(dif=abs(y-br)) %>% arrange(dif) %>% 
      head(1)
    
    # filter
    summary_predictions <- summary_predictions %>%
      mutate(x=pm25Exp_10ug*10) %>% 
      filter(commune==level_sel$commune,year_quarter==level_sel$year_quarter)
    
    # summary_predictions <- summary_predictions %>% 
    #   mutate(weighted_fit=y*pop_weight) %>% 
    # # calculate average of preductions across levels
    #   group_by(pm25Exp_10ug) %>%
    #   summarise(y = sum(weighted_fit)) %>% ungroup() %>% 
    # mutate(x=pm25Exp_10ug*10)
    
    return(summary_predictions)
    
  } 
  else {
    # Same but with SE, avoid using response and se.fit at the same time
    predictions <- predict(model, newdata=prediction_grid, type="link",se.fit = T)
    
    # Add the proportions as weights
    prediction_grid <- prediction_grid %>% 
      left_join(prop_commune) %>% 
      group_by(pm25Exp_10ug) %>% 
      mutate(pop_weight=pop_weight/sum(pop_weight)) %>% ungroup()
    
    # add predictions - link function is log
    prediction_grid$pred <- exp(predictions$fit)
    prediction_grid$y_low <- exp(predictions$fit-predictions$se.fit*1.96)
    prediction_grid$y_high <- exp(predictions$fit+predictions$se.fit*1.96)
    
    # calculate average of predictions across levels
    summary_predictions <- prediction_grid %>%
      mutate(y=pred/pop75*1000,y_low=y_low/pop75*1000,y_high=y_high/pop75*1000) %>% 
      mutate(y=y*pop_weight,y_low=y_low*pop_weight,y_high=y_high*pop_weight) %>% 
      group_by(pm25Exp_10ug) %>%
      summarise(y = sum(y),y_low = sum(y_low),y_high = sum(y_high)) %>% ungroup() %>% 
      mutate(x=pm25Exp_10ug*10)
    
    return(summary_predictions)
  }
}

x = f.getSplinePredictions(pm25_support,data_used,mod_nb,se = F)


## test - category levels had the same shape, only the intercept with Y changes
# all_combs <- summary_predictions %>% 
#   mutate(key=paste0(commune,year_quarter)) %>% pull(key) %>% unique()
# all_combs = all_combs[1:50]

# summary_predictions %>% 
#   mutate(key=paste0(commune,year_quarter)) %>% 
#   filter(key %in% all_combs) %>% 
#   mutate(x=pm25Exp_10ug*10) %>% 
#   ggplot(aes(x,y))+
#     geom_line(aes(group=key),alpha=.5)+
#   ylim(4.5,6.5)




## Linear models -------
# OUR MODEL IS NOT OLS, so it not valid

## Analysis by Nature paper
# https://github.com/sheftneal/HBBB2018/blob/master/scripts/07_FigureED3.R


model_ols <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+landTemp | year_quarter+commune,
                       data=df,weights=df$pop75)
# model_12 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, df = 3)+landTemp | year_quarter+commune,
#                       data=df,weights=df$pop75)
model_13 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, knots = 20/10)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_14 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25)/10)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_15 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
model_16 <- lfe::felm(MR_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25)/10)+
                        ns(landTemp, df = 4) | year_quarter+commune,
                      data=df,weights=df$pop75)
model_17 <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp | year_quarter+commune,
                      data=df,weights=df$pop75)
# model_18 <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+I(pm25Exp_10ug^3)+
#                         landTemp | year_quarter+commune,
                      # data=df,weights=df$pop75)


# get predictions
data_used <- df %>% dplyr::select(death_count_all_cause,
                                  pm25_exposure,
                                  pm25Exp_10ug,landTemp,year_quarter,
                                  commune,pop75,MR_all_cause) %>% na.omit()
pm25_support <- seq(0.5,6,0.1)
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
# y12 = as.numeric(t(as.matrix((model_12)$coefficients[1:3]))%*%t(ns(x,3)))
y13 = as.numeric(t(as.matrix((model_13)$coefficients[1:2]))%*%t(ns(x, knots = 20/10)))
y14 = as.numeric(t(as.matrix((model_14)$coefficients[1:4]))%*%t(ns(x, knots = c(10,15,25)/10)))				
y15 = as.numeric(t(as.matrix((model_15)$coefficients[1:4]))%*%t(ns(x,4)))				
# y16 = as.numeric(t(as.matrix((model_16)$coefficients[1:3]))%*%t(ns(x,3)))				
y17 = x*model_17$coefficients[1]+x^2*model_17$coefficients[2]
# y18 = x*model_18$coefficients[1]+x^2*model_18$coefficients[2]+x^3*model_18$coefficients[3]

# Re-center
y = y - y[ind] + br
y_low = y_low - y_low[ind] + br
y_high = y_high - y_high[ind] + br
# y12 = y12 - y12[ind] + br
y13 = y13 - y13[ind] + br
y14 = y14 - y14[ind] + br
y15 = y15 - y15[ind] + br
# y16 = y16 - y16[ind] + br
y17 = y17 - y17[ind] + br
# y18 = y18 - y18[ind] + br


ci_plot <- response_a %>% 
  mutate(x=x*10)

# Plot
data_fig <- response_a %>%
  mutate(Base=y,y_low=y_low,y_high=y_high,
         # `spline (3 df)`=y12,
         `spline (1 knot at 20)`=y13,
         `spline (3 knots at 10, 15 & 25)`=y14,`spline (4 df)`=y15,
         # `spline (3 knots at 10, 15 & 25)+ Temp (4 df)`=y16,
         `quadratic term`=y17) %>% 
  dplyr::select(-y_low,-y_high) %>% 
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  mutate(x=x*10)

ggplot(data_fig,aes(x))+
  geom_ribbon(data=ci_plot,
              aes(ymin = y_low,ymax = y_high),fill="#FF474C",alpha = 0.3)+
  geom_line(aes(y=value,col=key),linewidth=0.5)+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*10,weight=pop75),
                 data=data_used,binwidth = 0.5,
                 linewidth=0.1,center=0,position = position_nudge(y=4.5),
                 alpha=0.4,fill="darkred",col="white")+
  coord_cartesian(ylim=c(4.5,6.5),xlim=c(0,75))+
  geom_text(aes(y=value,label=key,col=key),
            data=filter(data_fig,x==60),
            nudge_x = 1,hjust=0,nudge_y = c(0,0,0,0,0,-0.05),
            size=8*5/14 * 0.8)+
  labs(x=lab_pm25,y=lab_mr2,col="")+
  scale_x_continuous(breaks=c(0,20,40,60))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        # panel.border = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())

ggsave("Figures/Model/SplinesLinear.png")



# EoF