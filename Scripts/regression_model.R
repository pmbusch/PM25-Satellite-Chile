## Fixed effects Model
## PBH
## February 2023

library(tidyverse)
library(MASS)
library(lme4)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
# rate is per 1000 birhts
mortality <- read.delim("Data/mortality_data.csv")
mortality <- mortality %>% rename(year=Year,quarter=Quarter)

# 75+ years death data----
death_75 <- read.delim("Data/chile_elderly_mortality_count_comuna_level_year_quarter.csv",sep=",")
death_75 <- death_75 %>% rename(year=Year,quarter=Quarter,
                                codigo_comuna=CODIGO_COMUNA_RESIDENCIA)

# get population
chilemapas::censo_2017_comunas$edad %>% unique()
pop_75 <- chilemapas::censo_2017_comunas %>% 
  filter(edad %in% c("75 a 79","80 a 84","85 a 89","90 a 94","95 a 99")) %>% 
  group_by(codigo_comuna) %>% summarise(pop75=sum(poblacion,na.rm=T)) %>% 
  ungroup() %>% mutate(codigo_comuna=as.integer(codigo_comuna))

pop_75 <- pop_75 %>% filter(pop75>50)

# library(ggforce)
# ggplot(pop_75,aes(pop75))+geom_histogram(bins=100)+
#   facet_zoom(xlim(0,500))


death_75 <- death_75 %>% left_join(pop_75)
death_75 <- death_75 %>% mutate(mortality=Mortality_Count/pop75*1000)

# pm25 pollution data exposure
pm25 <- read.delim("Data/pm25exposure.csv",sep = ";")

# group to region and quarters
pm25_exp <- pm25 %>% 
  mutate(quarter=ceiling(month/3) %>% as.integer()) %>% # quarters by months
  # mutate(codigo_region=as.factor(codigo_region)) %>% 
  mutate(pop_pm25=poblacion*pm25_Exposure) %>% 
  # group_by(codigo_region,year,quarter) %>% 
  group_by(codigo_region,codigo_provincia,codigo_comuna,year,quarter) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(poblacion,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) %>% 
  mutate(pm25Exp_10ug=pm25_exposure/10)

pm25_exp <- pm25_exp %>% filter(pm25_exposure>1)

# Join ----
names(mortality)
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
         quarter=as.factor(quarter))

write.table(df,"Data/panelData.csv",sep = ";",row.names = F)


## Correlation ---
cor(df$mortality,df$pm25_exposure)
cor(df$mortality,df$pm25_exposure,method = "spearman")

df %>% group_by(quarter) %>% 
  summarise(cor(mortality,pm25_exposure))


# Some figures -----
df %>% 
  mutate(a=paste0(year,quarter)) %>% 
  ggplot(aes(a,mortality,group=commune))+
  geom_line()

ggplot(df,aes(quarter,mortality))+
  geom_boxplot()

ggplot(df,aes(pm25_exposure,mortality,col=quarter))+
  geom_point(alpha=.5)+
  geom_smooth()+
  facet_wrap(~quarter)


## Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression
model_nb <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+region+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

summary(model_nb)
nobs(model_nb)
confint(model_nb) %>% exp()
autoplot(model_nb)

# LM
model_lm <- glm(mortality ~ pm25Exp_10ug+year+region, 
                   data = df,
                family = "gaussian",
                   na.action=na.omit)

summary(model_lm)
nobs(model_lm)
confint(model_lm)
autoplot(model_lm)

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