## Different Models comparisons by Model specification
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


## Negative Binomial forms ----
# year+quarter
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
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


# region*year+quarter
mod_nb8 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+region*year+quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb8,"Region*Year+Quarter"))


# year + quarter -landTemp
mod_nb9 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+year_quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb9,"No Temperature+Year-Quarter"))

# Land Temp Squared year +quarter
mod_nb10 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+I(landTemp^2)+year_quarter+commune+
                    offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb10,"T + T^2+Year-Quarter"))

# Temp spline
library(splines)
mod_nb11 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+ns(landTemp, df = 3)+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
models_nb_res <- rbind(models_nb_res,getModelInfo(mod_nb11,"Spline Temperature+Year-Quarter"))
df$landTemp %>% range()
attr(terms(mod_nb11),"predvars") # 16.5 & 27
lmtest::lrtest(model_nb, mod_nb11) # Spline is better is better


# PM25 spline
mod_nb12 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
summary(mod_nb12)
getModelInfo(mod_nb12,"Spline PM2.5") %>% head()
AIC(mod_nb12)
# Knots
df$pm25Exp_10ug %>% range()
attr(terms(mod_nb12),"predvars") # 13.4 & 22.8
# so knots are 2.9-13.4; 13.4-22.8; 22.8-90.2
# Compare to base model
lmtest::lrtest(model_nb, mod_nb12) # Spline is better is better


# Better to plot to see the effect
# https://stats.stackexchange.com/questions/503985/interpretation-of-cubic-spline-coefficients-in-r
# Create a data frame for prediction: only `Age` will vary.
#
N <- 101
x <- df[which.max(complete.cases(df)), ]
df_new <- do.call(rbind, lapply(1:N, function(i) x))
df_new$pm25Exp_10ug <- with(df, seq(min(pm25Exp_10ug, na.rm=TRUE), 
                                    max(pm25Exp_10ug, na.rm=TRUE), length.out=N))
# Predict and plot.
df_new$predDeath <- predict(mod_nb12, newdata=df_new) # The predicted *link,* by default
with(df_new, plot(pm25Exp_10ug*10, predDeath, type="l", lwd=2, 
                  xlab="PM2.5",ylab="", main="Relative spline term"))
mtext("Spline contribution\nto the link function", side=2, line=2)

# Plot numerical derivatives.
dpm25Exp_10ug <- diff(df_new$pm25Exp_10ug[1:2])
delta <- diff(df_new$predDeath)/dpm25Exp_10ug
pm25 <- (df_new$pm25Exp_10ug[-N] + df_new$pm25Exp_10ug[-1]) / 2
plot(pm25*10, delta, type="l", lwd=2, ylab="Change per PM2.5", xlab="PM2.5",
     main="Spline Slope (Effective Coefficient)")


# get predictions with a function
data_used <- df %>% dplyr::select(death_count_all_cause,
                                  pm25_exposure,
                                  pm25Exp_10ug,landTemp,year_quarter,
                                  commune,pop75,MR_all_cause) %>% na.omit()
pm25_support <- seq(0.1,6,0.1)
br <- weighted.mean(data_used$MR_all_cause,data_used$pop75)  
pm <- weighted.mean(data_used$pm25Exp_10ug,data_used$pop75) 
pop <- sum(data_used$pop75)
deat <- sum(data_used$death_count_all_cause)  
deat/pop*1000

# Key idea: make predictions with new dataset changing only the PM2.5. base data
f.getSplinePredictions <- function(new_base,model,se=F){
  
  N <- length(new_base)
  # create base data for prediction, there is no interaction with PM2.5
  x <- data_used[which.max(complete.cases(data_used)), ]
  df_new <- do.call(rbind, lapply(1:N, function(i) x))
  df_new$pm25Exp_10ug <- with(data_used,seq(min(pm25Exp_10ug, na.rm=TRUE), 
                                      max(pm25Exp_10ug, na.rm=TRUE), length.out=N))

  # predictions  
  ind <- which(new_base==round(pm,1))
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
    # Re-center
    y = y - y[ind] + br
    return(y)
  }
}



mod_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
nobs(mod_nb)
# attr(terms(mod_nb12),"predvars") 
mod_nb13 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = 16/10)+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# attr(terms(mod_nb13),"predvars") 
mod_nb14 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, knots = c(10,15,25,35)/10)+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
# attr(terms(mod_nb14),"predvars") 
# summary(mod_nb13)
# summary(mod_nb14)
mod_nb15 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 4)+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
mod_nb16 <- glm.nb(death_count_all_cause ~ ns(pm25Exp_10ug, df = 3)+ns(landTemp, df = 3)+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
mod_nb17 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+landTemp+year_quarter+commune+
                     offset(log(pop75)), data = df,na.action=na.omit)
mod_nb18 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+I(pm25Exp_10ug^2)+I(pm25Exp_10ug^3)+
                     landTemp+year_quarter+commune+offset(log(pop75)), data = df,na.action=na.omit)

# get data on models

response_a <- tibble::tibble(x =pm25_support)


y=f.getSplinePredictions(pm25_support,mod_nb,se=T)
y_low=y$y_low
y_high=y$y_high
y=y$y
y12 = f.getSplinePredictions(pm25_support,mod_nb12)
y13 = f.getSplinePredictions(pm25_support,mod_nb13)
y14 = f.getSplinePredictions(pm25_support,mod_nb14)
y15 = f.getSplinePredictions(pm25_support,mod_nb15)
y16 = f.getSplinePredictions(pm25_support,mod_nb16)
y17 = f.getSplinePredictions(pm25_support,mod_nb17)
y18 = f.getSplinePredictions(pm25_support,mod_nb18)


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
ggsave("Figures/Model/Splines.png")

###### Analysis by Nature paper
# https://github.com/sheftneal/HBBB2018/blob/master/scripts/07_FigureED3.R

# Need to do it with linear models?

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





#####


# save results
write.csv(models_nb_res,"Data/Models/modelSpecResults.csv",row.names = F)
models_nb_res <- read.csv("Data/Models/modelSpecResults.csv")

models_nb_res$name %>% unique()
cat_levels <- c("Time fixed effects",
                "Region interaction",
                "Temperature term")

order_models <- tibble(name=c(
  "Year*Month",
  "Year+Month",
  "Year*Quarter",
  "Year*Quarter+Month",
  "Year+Quarter",
  "Year+Quarter+Month",
  "Region*Quarter",
  "Region*Quarter+Year*Month",
  "Region*Quarter+Year+Month",
  "Region*Year+Quarter",
  "No Temperature+Year-Quarter",
  "T + T^2+Year-Quarter",
  "Spline Temperature+Year-Quarter"),
  category=c(rep(cat_levels[1],6),
             rep(cat_levels[2],4),
             rep(cat_levels[3],3)))
order_models$order_n <- 1:nrow(order_models)


# put title top
# https://stackoverflow.com/questions/70369558/two-column-facet-grid-with-strip-labels-on-top
library(gtable)
library(grid)
library(gridExtra)
f.TitleTop <- function(ggObject){
  
  gt <- ggplotGrob(ggObject)
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  for(i in rev(panels$t-1)) {
    gt = gtable_add_rows(gt, unit(1.2, "lines"), i)
  }
  panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
  strips <- c(subset(gt$layout, grepl("strip-r", gt$layout$name), se=t:r))
  stripText = gtable_filter(gt, "strip-r")
  for(i in 1:length(strips$t)) {
    gt = gtable_add_grob(gt, stripText$grobs[[i]]$grobs[[1]], t=panels$t[i]-1, l=5)
  }
  gt = gt[,-6]
  # for(i in panels$t) { # remove for the all figure for the article, for some reason it interfer with the sec. axis
  #   gt$heights[i-1] = unit(1.2, "lines")
  #   gt$heights[i-2] = unit(0.2, "lines")
  # }
  return(gt)
}


# Figure 
p <- models_nb_res %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>% 
  left_join(order_models) %>% 
  mutate(category=factor(category,levels=cat_levels)) %>% 
  # name modification
  mutate(name=name %>% 
           str_replace_all("Year\\*Month","Year-Month") %>% 
           str_replace_all("Year\\*Quarter","Year-Quarter") %>% 
           str_replace("T \\+ T\\^2","T°+T° squared") %>% 
           str_replace_all("\\+"," + ")) %>% 
  # mutate(rowname=1:nrow(models_nb_res)) %>%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
ggplot(aes(reorder(name,order_n,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # geom_point(size=1,col="red")+ # all T are significant
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  # geom_vline(xintercept = c(3.5,7.5), linetype="dashed",col="grey",linewidth=0.3)+
  coord_flip()+
  # facet_wrap(~category,ncol=1,scales = "free_y")+
  facet_grid(category~.,scales = "free_y",space = "free")+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(title=expression(paste("Base Model: MR ~ ",PM[2.5],"+T°+Commune+...","")),
       x="Additional term",
       y=lab_rr)+
       # y=lab_rr_temp)+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(angle=0,vjust=1.025,margin=margin(r=-80)),
        axis.title.x = element_text(size=10,hjust=1),
        # strip.text = element_text(size = 8,angle=90),
        strip.text.y = element_text(angle = 0),
        legend.position = "none")



gt <- f.TitleTop(p)
grid.newpage();grid.draw(gt)

ggsave("Figures/Model/Model_Specifications.png",gt,
# ggsave("Figures/Model/Model_Specifications_Temp.png",gt,
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# Month coefficients
month_levels <- paste0("",12:2)
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(str_detect(param,"month"),!str_detect(param,":")) %>% 
  mutate(param=factor(str_remove(param,"month"),levels=month_levels)) %>% 
  ggplot(aes(param,rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=1,aes(col=signif))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.4)+
  coord_flip()+
  facet_wrap(~name)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="Month Fixed Effect",y=expression(paste("Percentage increase in Mortality w.r.t. Month 1")))+
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