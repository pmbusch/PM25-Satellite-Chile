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
(br <- weighted.mean(data_used$MR_all_cause,data_used$pop75)) 
(pm <- weighted.mean(data_used$pm25Exp_10ug,data_used$pop75))
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
# Numbers in model names are not meaningful

# 3 knots equally spaced (quantiles)
# mod_nb12 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+landTemp+
#                      year_quarter+commune+
#                      offset(log(pop75)), data = df,na.action=na.omit)
# Knots
# df$pm25Exp_10ug %>% range()
# attr(terms(mod_nb15),"predvars")
# Compare to base model
# lmtest::lrtest(model_nb, mod_nb12) # Spline is better is better

# Knto at 20 ug/m3
mod_nb13 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(20)/10)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
mod_nb14 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25)/10)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
mod_nb15 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp+year_quarter+
                     commune+offset(log(pop75)), data = df,na.action=na.omit)

# 3 knots equally spaced (quantiles) for PM2.5 and Land Temp.
# mod_nb16 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+ns(landTemp, df = 3)+
#                      year_quarter+commune+
#                      offset(log(pop75)), data = df,na.action=na.omit)
# PM2.5 with quadratic term
mod_nb17 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)


# mod_nb18 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+I(pm25Exp_10ug^3)+
#                      landTemp+year_quarter+commune+offset(log(pop75)), data = df,na.action=na.omit)

# 4 df equally spaced (quantiles) for PM2.5 and Land Temp.
quantile(data_used$pm25Exp_10ug,c(0.1,0.75,0.9),weights=data_used$pop75)
quantile(data_used$landTemp,c(0.1,0.75,0.9),weights=data_used$pop75)

# with quantiles
mod_nb19 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, 
                                              knots = c(10,15,25)/10)+
                                              # knots = unname(quantile(data_used$pm25Exp_10ug,c(0.1,0.75,0.9),weights=data_used$pop75)))+
                     ns(landTemp, df = 4)+
                        # knots = unname(quantile(data_used$landTemp,c(0.1,0.75,0.9),weights=data_used$pop75)))+
                     year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
attr(terms(mod_nb19),"predvars")



# just df
# mod_nb19 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 4)+ns(landTemp, df = 4)+
#                      year_quarter+commune+
#                      offset(log(pop75)), data = df,na.action=na.omit)


# get effect of PM2.5 on MR over the range of PM2.5 values
response_a <- tibble::tibble(x =pm25_support)
# linear response we need the SE, use function from MainEffects_Figure.R
# get line with CI
out_pm25 <- getModelInfo(mod_nb,"Base",data_df = data_used) %>% 
  filter(param=="pm25Exp_10ug")
response_pm <- getRiskSlope(data_used, out_pm25, pm_range = pm25_support*10)

y_low=response_pm$y_low
y_high=response_pm$y_high
y=response_pm$y

# y12 = f.getSplinePredictions(pm25_support,data_used,mod_nb12)
y13 = f.getSplinePredictions(pm25_support,data_used,mod_nb13)
y14 = f.getSplinePredictions(pm25_support,data_used,mod_nb14)
y15 = f.getSplinePredictions(pm25_support,data_used,mod_nb15)
# y16 = f.getSplinePredictions(pm25_support,data_used,mod_nb16)
y17 = f.getSplinePredictions(pm25_support,data_used,mod_nb17)
# y18 = f.getSplinePredictions(pm25_support,data_used,mod_nb18)
y19 = f.getSplinePredictions(pm25_support,data_used,mod_nb19)


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
         # `spline (3 df)`=y12,
         `spline (1 knot at 20)`=y13,
         `spline (3 knots at 10, 15 & 25)`=y14,
         `spline (4 df)`=y15,
         # `spline (3 df)+ Temp (3 df)`=y16,
         # `x+x^2+x^3`=y18,
         `spline (3 knots at 10, 30 & 50) + Temp (4 df)`=y19,
         `quadratic term`=y17) %>% 
  dplyr::select(-y_low,-y_high) %>%
  pivot_longer(c(-x), names_to = "key", values_to = "value") %>% 
  mutate(x=x*10)

ggplot(data_fig,aes(x))+
  geom_ribbon(data=ci_plot,
              aes(ymin = y_low,ymax = y_high),fill="#FF474C",alpha = 0.3)+
  geom_line(aes(y=value,col=key),linewidth=0.5)+
  geom_histogram(aes(pm25_exposure,y=after_stat(density)*8,weight=pop75),
                 data=data_used,binwidth = 0.5,
                 linewidth=0.1,center=0,position = position_nudge(y=5),
                 alpha=0.4,fill="darkred",col="white")+
  coord_cartesian(ylim=c(5,6.5),xlim=c(3,82))+
  geom_text(aes(y=value,label=key,col=key),
                  data=filter(data_fig,x==60),
                  nudge_x = 1,hjust=0,nudge_y = c(1,2,-1,0,-0.5,0)/20,
                  size=8*5/14 * 0.8)+
  labs(x=lab_pm25,y=lab_mr2,col="")+
  scale_x_continuous(breaks=c(0,20,40,60))+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        # panel.border = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())

ggsave("Figures/Model/Splines.png",
       units="cm",dpi=600,
       width=8.7*2,height=8.7)
pdf("Figures/Model/Splines.pdf",
    width = 8.7*2/2.54, height =8.7/2.54)
ggplot2::last_plot()
dev.off()


# Key idea: make predictions with new dataset changing only the PM2.5. base data
# We calcualte all the levels (commune and quarter) and then average based on pop
# Idea is to get an "average" of categorical variables
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