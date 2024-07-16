## modelo PMB paper 
## updates LAC 8 may 2024



## Fixed effects Model

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich) # for robust and clustered standard errors
library(gam)

# Load Panel Data ----
# df <- read.delim("panelData.csv",sep=";")
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

names(df)
head(df)


df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago

head(df)

## we create a sequential month number

df$n_month = 12*(as.numeric(df$year)-1) + as.numeric(df$month)

yy = as.numeric(df$year)
mm = as.numeric(df$month)
cbind(yy,mm)



## Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression



## modelo base PMB
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+
                     year_quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

## modelo base PMB
model_ym <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+
                     year+ month+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
plot.Gam(model_ym, se=T)
summary(df$pm25Exp_10ug)



pdf("testmodelos.pdf", paper="letter", w=7, h=10)

mod.s0 <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+ offset(log(pop75)), 
                   data = df, na.action=na.omit)
desc.mod(mod.s0)
mod.s1 = update(mod.s0, . ~ . + offset(log(pop75)) ); desc.mod(mod.s1)
mod.s2 = update(mod.s1, . ~ . + landTemp ); desc.mod(mod.s2)
mod.s3 = update(mod.s2, . ~ . + commune );  desc.mod(mod.s3)

# temporal controls
mod.s4 = update(mod.s3, . ~ . + year );     desc.mod(mod.s4)
mod.s5 = update(mod.s4, . ~ . + month );    desc.mod(mod.s5)
mod.s6 = update(mod.s3, . ~ . + year_quarter );  desc.mod(mod.s6)
mod.s7 = update(mod.s3, . ~ . + ns(n_month,18*3) );  desc.mod(mod.s7)

dev.off()


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

mod.s8 = update(mod.s7, . ~ . + pm25Exp_lag1);  desc.mod(mod.s8)
mod.s9 = update(mod.s8, . ~ . + pm25Exp_lead1);  desc.mod(mod.s9)




# function to test models
# --------------------------
desc.mod = function(mod) {
    
  # models coefs
    print( paste("Model ", formula(mod)[2]," ~ " ,formula(mod)[3], sep=" "))
    print(summary(mod))
    print(nobs(mod))
    print(BIC(mod))
    coefPM = coef(mod)[2];  print(coefPM)
    #ci_pm25 <- confint(mod,method="Wald",parm="pm25Exp_10ug") %>% exp()


  # plot  of model coefficients
    par(mfrow=c(3,2))
    plot.Gam(mod, se=T)
    par(mfrow=c(1,1))
    title(main=list(paste(formula(mod)[2], " ~ ", formula(mod)[3], sep=" "), cex=0.7, col="red"  ))

    # coefPM  # we pass on the coef of PM
}
