## Fixed effects Model
## PBH
## February 2023

library(tidyverse)
library(MASS)
library(lme4)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
# rate is per 1000 birhts
# mortality <- read.delim("Data/mortality_data.csv")
# mortality <- mortality %>% rename(year=Year,quarter=Quarter)

# 75+ years death data----
# death_75 <- read.delim("Data/chile_elderly_mortality_count_comuna_level_year_quarter.csv",sep=",")
death_75 <- read.delim("Data/chile_elderly_mortality_count_comuna_level_year_month.csv",sep=",")
death_75 <- death_75 %>% rename(year=Year,
                                # quarter=Quarter,
                                month=Month,
                                codigo_comuna=CODIGO_COMUNA_RESIDENCIA)
death_75$Mortality_Count %>% sum()
death_75 <- death_75 %>% group_by(codigo_comuna,year,month) %>%
  summarise(Mortality_Count=sum(Mortality_Count,na.rm=T))

# get population
chilemapas::censo_2017_comunas$edad %>% unique()
pop_75 <- chilemapas::censo_2017_comunas %>% 
  filter(edad %in% c("75 a 79","80 a 84","85 a 89","90 a 94","95 a 99")) %>% 
  group_by(codigo_comuna) %>% summarise(pop75=sum(poblacion,na.rm=T)) %>% 
  ungroup() %>% mutate(codigo_comuna=as.integer(codigo_comuna))

# use only counties with at least 50 people in the age group
pop_75 <- pop_75 %>% filter(pop75>50)

# library(ggforce)
# ggplot(pop_75,aes(pop75))+geom_histogram(bins=100)+
#   facet_zoom(xlim(0,500))

# join death and pop
death_75 <- death_75 %>% left_join(pop_75)

# death_75$codigo_comuna <- as.character(death_75$codigo_comuna)
# a <- death_75 %>% left_join(chilemapas::codigos_territoriales)
# 
# a %>% group_by(year,month,codigo_region) %>% 
#   summarise(Mortality_Count=sum(Mortality_Count,na.rm=T),
#             pop75=sum(pop75,na.rm=T)) %>% 
#   mutate(a=paste0(year,month)) %>%
#   mutate(mortality=Mortality_Count/pop75*1000) %>% 
#   ggplot(aes(a,mortality,group=codigo_region))+
#   theme(axis.text.x = element_text(angle=90))+
#   geom_line()+
#   facet_wrap(~codigo_region)

death_75 <- death_75 %>% mutate(mortality=Mortality_Count/pop75*1000)

# pm25 pollution data exposure
pm25 <- read.delim("Data/pm25exposure.csv",sep = ";")

# group to region and quarters
pm25_exp <- pm25 %>% 
  mutate(quarter=ceiling(month/3) %>% as.integer()) %>% # quarters by months
  # mutate(codigo_region=as.factor(codigo_region)) %>% 
  mutate(pop_pm25=poblacion*pm25_Exposure) %>% 
  group_by(codigo_region,codigo_provincia,codigo_comuna,year,month) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(poblacion,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) %>% 
  mutate(pm25Exp_10ug=pm25_exposure/10)

# remove below 1 exposure
pm25_exp <- pm25_exp %>% filter(pm25_exposure>1)

# Join ----
# names(mortality)
names(pm25_exp)
names(death_75)
# df <- mortality %>% left_join(pm25_exp)
df <- death_75 %>% left_join(pm25_exp)
# df %>% filter(is.na(pm25_exposure)) %>%
#   mutate(codigo_comuna=as.character(codigo_comuna)) %>% 
#   left_join(chilemapas::codigos_territoriales,by="codigo_comuna") %>% 
#   view
df <- df %>% na.omit()

df <- df %>% 
  mutate(year=as.factor(year),
         region=as.factor(codigo_region),
         commune=as.factor(codigo_comuna),
         month=as.factor(month)
         # quarter=as.factor(quarter)
         )

# write.table(df,"Data/panelData.csv",sep = ";",row.names = F)


## Correlation ---
cor(df$mortality,df$pm25_exposure)
cor(df$mortality,df$pm25_exposure,method = "spearman")

df %>% group_by(month) %>% 
  summarise(cor(mortality,pm25_exposure))


# Some figures -----
# df %>% 
#   mutate(a=paste0(year,month)) %>%
#   ggplot(aes(a,mortality,group=commune))+
#   geom_line(alpha=.5)
# 
# ggplot(df,aes(month,mortality))+
#   geom_boxplot()
# 
# ggplot(df,aes(pm25_exposure,mortality,col=quarter))+
#   geom_point(alpha=.5, data=dplyr::select(df,-quarter),col="grey")+
#   geom_point(alpha=.5)+
#   geom_smooth(method="lm")+
#   geom_smooth(method="lm", data=dplyr::select(df,-quarter),col="grey")+
#   facet_wrap(~quarter)


## Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

df <- df %>% mutate(quarter=ceiling(as.numeric(month)/3) %>% factor())

model_nb <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

summary(model_nb)
nobs(model_nb)
BIC(model_nb)
coef(model_nb) %>% exp()
confint(model_nb,method="Wald") %>% exp()
autoplot(model_nb)
plot(df$mortality,predict(model_nb,type="response"))

# NB with interaction
model_nb_inter <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+region*quarter+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

summary(model_nb_inter)
nobs(model_nb_inter)
BIC(model_nb_inter)
coef(model_nb_inter) %>% exp()
confint(model_nb_inter,method="Wald") %>% exp()
autoplot(model_nb_inter)
plot(df$mortality,predict(model_nb_inter,type="response"))


# LM
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

## Gamma regression -----
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


# Model with lags -----
df <- df %>% 
  mutate(pm25Exp_lag1=lag(pm25Exp_10ug,1),
         pm25Exp_lag2=lag(pm25Exp_10ug,2),
         pm25Exp_lag3=lag(pm25Exp_10ug,3),
         pm25Exp_lag4=lag(pm25Exp_10ug,4))


model_nb_lags <- glm.nb(Mortality_Count ~ pm25Exp_10ug+pm25Exp_lag1+
                          pm25Exp_lag2+pm25Exp_lag3+pm25Exp_lag4+
                          year+region+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

summary(model_nb_lags)
nobs(model_nb_lags)
BIC(model_nb_lags)
coef(model_nb_lags) %>% exp()
confint(model_nb_lags) %>% exp()
autoplot(model_nb_lags)



## Panel estimators
# https://rdrr.io/cran/pglm/man/pglm.html

library(pglm)
la <- pglm(deathsum ~ pm25Exp_10ug+offset(log(Birth_count)), 
           data=df,
           family = negbin, model = "within", 
           effect="twoways",
           print.level = 3, method = "nr",
           index = c('region', 'quarter'))
summary(la)


# EoF