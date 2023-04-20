## Fixed effects Model
## PBH
## February 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich) # for robust and clustered standard errors

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

# LOAD pANEL DATA ----
df <- read.delim("Data/panelData.csv",sep=";")

df$year %>% range()
df$codigo_comuna %>% unique() %>% length() # 331
df$pm25_exposure %>% range(na.rm=T)
df$mortality %>% range(na.rm=T)


## Correlation ---
cor(df$mortality,df$pm25_exposure,use = "complete.obs")
cor(df$mortality,df$pm25_exposure,method = "spearman",use = "complete.obs")

df %>% group_by(month) %>% 
  summarise(cor(mortality,pm25_exposure,use = "complete.obs"))

df %>% group_by(quarter) %>% 
  summarise(cor(mortality,pm25_exposure,use = "complete.obs"))



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

df <- df %>% mutate(quarter=factor(quarter),
                    year=as.factor(year),
                    commune=as.factor(codigo_comuna),
                    commune=relevel(commune,ref="13101")) # Santiago

# year month interaction

# year*month: 1.006 (0.999-1.013)
# year*quarter + month: 1.008 (1.001-1.015)
# year*quarter (no month): 1.056 (1.045-1.067)
# year+quarter (no month): 1.054 (1.044-1.065)
# year+quarter+month: 1.006 (1.000-1.013)
# year+month: 1.006 (1.000-1.013)

model_nb <- glm.nb(Mortality_Count ~ pm25Exp_10ug+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)

summary(model_nb)
nobs(model_nb)
BIC(model_nb)
coef(model_nb) %>% exp()
exp(coef(model_nb))[2]
# ci_pm25 <- confint(model_nb,method="Wald",parm="pm25Exp_10ug") %>% exp()
autoplot(model_nb)
plot(df$mortality,predict(model_nb,type="response"))


# cluster standard errors to commune

# Compute the cluster-robust variance-covariance matrix using the sandwich package
cluster_se <- vcovCL(model_nb, cluster = df$commune)
lmtest::coeftest(model_nb, vcov = cluster_se)

# both take super long to run and give the same results
# confint(model_nb, level = 0.95, vcov = cluster_se,parm="pm25Exp_10ug")
# confint(model_nb, level = 0.95,parm="pm25Exp_10ug")

# manually calculate SE
alpha <- 0.05
# Calculate the critical value of the t-distribution
t_crit <- qt(1 - alpha/2, df = nobs(model_nb)- length(unique(df$commune))) # 1.96 practically

coef_est <- coef(model_nb)
coef_se <- sqrt(diag(cluster_se)) # get SE from the clusterred v-cov. Bigger than with normal
# coef_se <- summary(model_nb)$coefficients[,2] # larger but unadjusted

ci_lower <- coef_est - t_crit * coef_se
ci_upper <- coef_est + t_crit * coef_se
coef_est[1:8] %>% exp()
cbind(ci_lower, ci_upper)[1:8,] %>% exp()

# new p-value
# calculate the z-statistics and p-values for the coefficients
# assume asymptotic normality
z <- coef_est/coef_se
p_val <- 2 * (1 - pnorm(abs(z)))

# estimates
mod_est <- tibble(
  param=names(coef_est),
  coef=(exp(coef_est)-1)*100, # percentage increase
  ci_low=(exp(ci_lower)-1)*100,
  ci_high=(exp(ci_upper)-1)*100,
  t_value=z,
  p_value=p_val
)


# Figure - Year Effects ----
mod_est %>% 
  filter(param %>% str_detect("year")) %>% 
  mutate(param=str_remove(param,"year")) %>% 
  ggplot(aes(param,coef))+
  geom_point(size=1)+
  geom_linerange(aes(ymin=ci_low,ymax=ci_high))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  labs(x="",y="Percentage increase in Mortality relative to 2002")+
  coord_flip()+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
ggsave("Figures//Model/YearEffects.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


# Figure - Map Commune Effects ----
# Relative to STGO
library(chilemapas)

est_commune <- mod_est %>% 
  filter(param %>% str_detect("commune")) %>% 
  mutate(param=str_remove(param,"commune")) %>% 
  rename(codigo_comuna=param)

# join to Chile mapas
est_commune <- est_commune %>% 
  mutate(codigo_comuna=if_else(str_length(codigo_comuna)==4,
                               paste0("0",codigo_comuna),
                               codigo_comuna)) %>%  # add extra 0
  left_join(mapa_comunas)

# sign
est_commune <- est_commune %>% 
  # filter(coef<50) %>% 
  mutate(signif=p_value<0.05) 
table(est_commune$signif) # only 3 not significant

# map - need to make colors more visible
ggplot(est_commune) + 
  geom_sf(aes(fill = coef, geometry = geometry,color = signif),
          size=0.1) +
  scale_color_manual(values=c("transparent","black"),
                     name="Sign. at 5% level")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0,
                       trans="log",
                       name = "Percentage increase in Mortality \n relative to Santiago commune") +
  labs(title = "") +
  theme_void(10)

ggsave("Figures/Model/CommuneEffects.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7*2)


# estimate avoided deaths in the whole period -----
limit <- 12
names(df)
df %>% ungroup() %>% 
  dplyr::select(-codigo_comuna,-codigo_region,-codigo_provincia,
                -region,-quarter,-commune,-year,-month,-total_pop) %>% 
  mutate(reduction=if_else(pm25_exposure>limit,pm25Exp_10ug-limit/10,0)) %>% # get reduction in terms of 10 ug/m2
  mutate(red_perc_mortality=exp(coef(model_nb))[2]^(-reduction)) %>% # get relative decrease in mortality rate
  # mutate(red_perc_mortality=ci_pm25[1]^(-reduction)) %>% # low
  # mutate(red_perc_mortality=ci_pm25[2]^(-reduction)) %>% # high
  mutate(new_MR=mortality*red_perc_mortality,
         new_death_counts=pop75*new_MR/1000,
         avoided_deaths=Mortality_Count-new_death_counts) %>% 
  # head() %>% 
  pull(avoided_deaths) %>% sum()/60


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