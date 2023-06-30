## Fixed effects Model
## PBH
## February 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich) # for robust and clustered standard errors

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

# Load Panel Data ----
df <- read.delim("Data/panelData.csv",sep=";")
# df <- read.delim("Data/panelData_65.csv",sep=";") # 65+ deaths

df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago

df$codigo_comuna %>% unique() %>% length() # 327
df$pm25_exposure %>% range(na.rm=T)
df$MR_all_cause %>% range(na.rm=T)
df$landTemp %>% range(na.rm=T)

# Percentage of 0 counts in data
df %>% pivot_longer(c(death_count_all_cause,death_count_all_cause_NoCDP,death_count_cardio,
                      death_count_cardioRespiratory,death_count_respiratory,death_count_external), 
                    names_to = "cause", values_to = "value") %>% 
  mutate(zero=value==0) %>% 
  group_by(cause,zero) %>% tally() %>% ungroup() %>% group_by(cause) %>% 
  mutate(perc=n/sum(n)) %>% pivot_wider(names_from = zero, values_from = c(n,perc))


df %>% filter(commune=="13101") %>% group_by(year) %>% 
  summarise(t=quantile(landTemp,c(0.05,0.5,0.95)),
            pm=quantile(pm25_exposure,c(0.05,0.5,0.95))) %>% 
  mutate(qtl=c("p5","p50","p95")) %>% ungroup() %>% 
  pivot_wider(names_from = qtl, values_from = c(t,pm))

# Correlation ---
cor(df$MR_all_cause,df$pm25_exposure,use = "complete.obs")
cor(df$MR_all_cause,df$pm25_exposure,method = "spearman",use = "complete.obs")

df %>% group_by(month) %>% 
  summarise(cor(MR_all_cause,pm25_exposure,use = "complete.obs"))

df %>% group_by(quarter) %>% 
  summarise(cor(MR_all_cause,pm25_exposure,use = "complete.obs"))

# temp
cor(df$MR_all_cause,df$landTemp,use = "complete.obs")
cor(df$landTemp,df$pm25_exposure,use = "complete.obs")

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




# Month to predict PM2.5 -------

# R2 squared
summary(lm(pm25_exposure~month,data=df))$adj.r.squared # 0.5582
summary(lm(pm25_exposure~month+landTemp,data=df))$adj.r.squared # 0.5768
summary(lm(pm25_exposure~commune,data=df))$adj.r.squared # 0.1878
summary(lm(pm25_exposure~month+commune,data=df))$adj.r.squared # 0.7487
summary(lm(pm25_exposure~year+month+commune,data=df))$adj.r.squared # 0.7538
summary(lm(pm25_exposure~year+month,data=df))$adj.r.squared # 0.7538

summary(lm(pm25_exposure~quarter,data=df))$adj.r.squared # 0.4723
summary(lm(pm25_exposure~quarter+month,data=df))$adj.r.squared # 0.5582, obviously
summary(lm(pm25_exposure~quarter+commune,data=df))$adj.r.squared # 0.6624
summary(lm(pm25_exposure~year+quarter+commune,data=df))$adj.r.squared # 0.6675
summary(lm(pm25_exposure~year+quarter,data=df))$adj.r.squared # 0.4773


# For Temperature
summary(lm(landTemp~month,data=df))$adj.r.squared # 0.5657
summary(lm(landTemp~commune,data=df))$adj.r.squared # 0.3819
summary(lm(landTemp~month+commune,data=df))$adj.r.squared # 0.9504
summary(lm(landTemp~year+month+commune,data=df))$adj.r.squared # 0.9518
summary(lm(landTemp~year+month,data=df))$adj.r.squared # 0.5670

summary(lm(landTemp~quarter,data=df))$adj.r.squared # 0.4618
summary(lm(landTemp~quarter+commune,data=df))$adj.r.squared # 0.84599
summary(lm(landTemp~year+quarter+commune,data=df))$adj.r.squared # 0.8474
summary(lm(landTemp~year+quarter,data=df))$adj.r.squared # 0.4631

# For MR
summary(lm(MR_all_cause~month,data=df))$adj.r.squared # 0.0474
summary(lm(MR_all_cause~commune,data=df))$adj.r.squared # 0.0597
summary(lm(MR_all_cause~month+commune,data=df))$adj.r.squared # 0.1073
summary(lm(MR_all_cause~year+month+commune,data=df))$adj.r.squared # 0.1197
summary(lm(MR_all_cause~year+month,data=df))$adj.r.squared # 0.0598

summary(lm(MR_all_cause~quarter,data=df))$adj.r.squared # 0.0348
summary(lm(MR_all_cause~quarter+commune,data=df))$adj.r.squared # 0.0947
summary(lm(MR_all_cause~year+quarter+commune,data=df))$adj.r.squared # 0.1071
summary(lm(MR_all_cause~year+quarter,data=df))$adj.r.squared # 0.0471



# MAIN MODEL ---------

## Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression

# model for yearly data
df2 <- df %>% group_by(year, commune,NOM_COMUNA) %>% 
  summarise(death_count_all_cause=sum(death_count_all_cause),
            pm25Exp_10ug=mean(pm25Exp_10ug),
            landTemp=mean(landTemp),
            pop75=mean(pop75)) %>% ungroup()


df$met <- df$REGION=="13"
df$met <- df$met*df$pm25Exp_10ug

model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+
                     landTemp+
                     year_quarter
                     commune+
                     # met+ # interaction for met
                     # I(landTemp^2)+
                     # month+
                     # quarter:pm25Exp_10ug+
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
plot(df$MR_all_cause,predict(model_nb,type="response"))


# CI for interatcion
mod <- lm(MR_all_cause ~ pm25Exp_10ug+met+landTemp+year+quarter+commune, data=df)
lincom <- summary(multcomp::glht(mod,linfct="pm25Exp_10ug + met = 0"))$test
lc <- c(lincom$coefficients, lincom$sigma)
lc[1]+1.96*lc[2]
lc[1]-1.96*lc[2]
# significant


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

coef_est[340:352] %>% exp()
cbind(ci_lower, ci_upper)[340:352,] %>% exp()


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


# Test -----
library(DHARMa)
# tests if the simulated dispersion is equal to the observed dispersion
testDispersion(model_nb)

# testZeroinflation() - tests if there are more zeros in the data than expected from the simulations
testZeroInflation(model_nb)

# testTemporalAutocorrelation() - tests for temporal autocorrelation in the residuals
simulationOutput <- simulateResiduals(fittedModel = model_nb)
testTemporalAutocorrelation(simulationOutput,
                            df$)

# Figure - Residuals -----
df$predDeathCount <- predict(model_nb,type = "response")

df %>% 
  # filter(region==13) %>% 
  mutate(mr_pred=predDeathCount/pop75*1000,
         res=MR_all_cause-mr_pred) %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
# by region
    mutate(popRes=res*pop75) %>%
  group_by(date,REGION) %>%
  summarise(popRes=sum(popRes,na.rm=T),
            pop75=sum(pop75,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(res=popRes/pop75) %>% 
  ggplot(aes(date,res,group=REGION))+
  geom_line(alpha=.2,linewidth=.5)+ 
  facet_wrap(~REGION)+
  coord_cartesian(expand = F)+
  labs(x="",y=expression(paste("Residual Mortality Rate")))+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank())


## Figure - Year Effects ----
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
  
ggsave("Figures/Model/YearEffects.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


## Figure - Map Commune Effects ----
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
  scale_color_manual(values=c("#D3D3D3","black"),
                     name="Sign. at 5% level")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0,
                       # trans="log",
                       name = "Percentage increase in Mortality \n relative to Santiago commune") +
  labs(title = "") +
  theme_void(10)

ggsave("Figures/Model/CommuneEffects.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7*2)




# Comparison of Models ---------
# start_time <- proc.time() # Capture the starting time
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
# end_time <- proc.time() # Capture the ending time
# print(end_time - start_time) # 286 sec, almost 5 min

model_poi <- glm(death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+
                   offset(log(pop75)), 
                 data = df,family ="poisson",na.action=na.omit)
# OLS with grouped fixed effects
model_ols <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+landTemp | year+quarter+commune,
                       data=df,weights=df$pop75)
summary(model_ols)

model_ols_lm <- lm(MR_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune,
                   data=df,weights=df$pop75) # identical


# comparison of models
library(glmmTMB)
model_nb2 <- glmmTMB(formula = death_count_all_cause ~ pm25Exp_10ug + landTemp + year + quarter + commune +
                       offset(log(pop75)),data = df,family = "nbinom1")
model_nb3 <- glmmTMB(formula = death_count_all_cause ~ pm25Exp_10ug + landTemp + year + quarter + commune +
                       offset(log(pop75)),data = df,family = "nbinom2")


# Zero-Inflated Poisson and Negative Binomial Model 
library(pscl)
# Key idea: structural zeros and zero-inflation (account for prob. of getting a zero)

zip <- zeroinfl(death_count_all_cause ~ pm25Exp_10ug + landTemp + year + quarter + commune +
                  offset(log(pop75)) | pm25Exp_10ug + landTemp,  # no regressor for zero prob.
                data = df, dist = 'poisson')

zinb <- zeroinfl(death_count_all_cause ~ pm25Exp_10ug + landTemp + year + quarter + commune +
                   offset(log(pop75)) | pm25Exp_10ug + landTemp , 
                 data = df, dist = 'negbin')

# Show effect of PM2.5 on MR of a similar magnitude
# Negative effect of PM2.5 and T on the probability of having a 0 count event

# Test for the Existence of Zero-Inflation in R
# likelihood ratio test
# 2(l(full) - l(null)) converges in distribution to \chi^2_{df(full) - df(null)} under the null.
1 - pchisq(as.numeric(2*(logLik(zip) - logLik(model_poi))),
           df = length(coef(zip)) - length(coef(model_poi)))


# coefficients
coef(model_nb)[1:8] %>% exp() # NB using MASS
coef(model_poi)[1:8] %>% exp() # poisson
1+coef(model_ols)/mean(df$MR_all_cause) # OLS
summary(model_nb2)$coefficients$cond[1:8,1] %>% exp() # NB using glmmTMB with nbinom1
summary(model_nb3)$coefficients$cond[1:8,1] %>% exp() # NB using glmmTMB with nbinom2

# Comparison figure

source("Scripts/Functions.R",encoding="UTF-8")

br <- weighted.mean(df$MR_all_cause,df$pop75,na.rm=T)

models_nb_res <- getModelInfo(model_nb,"Negative Binomial")
models_nb_res <- rbind(models_nb_res,getModelInfo(model_poi,"Poisson"))
models_nb_res <- rbind(models_nb_res,getModelInfo(model_ols,"OLS",ols=T,baseRate = br))
models_nb_res <- rbind(models_nb_res,getModelInfo(model_nb2,"NB glmmTMB 1",robustSE = F))
models_nb_res <- rbind(models_nb_res,getModelInfo(model_nb3,"NB glmmTMB 2",robustSE = F))

# save results
write.csv(models_nb_res,"Data/modelMethodsResults.csv",row.names = F)
models_nb_res <- read.csv("Data/modelMethodsResults.csv")

# Figure 
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
  ggplot(aes(reorder(name,rowname,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  # geom_point(size=1,aes(col=signif))+
  geom_point(size=1,col="red")+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  # scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(title="Model: MR ~ PM2.5+T°+Commune+Year+Quarter",x="Model Used",
       y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  # Modify theme to look good
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("Figures/Model/Model_Method.png", ggplot2::last_plot(),
       # ggsave("Figures//Model/Model_Specifications_Temp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)





# comparing NB to Poisson - https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
pchisq(2 * (logLik(model_nb) - logLik(model_poi)), df = 1, lower.tail = FALSE) # NB is much more appropiate
lmtest::lrtest(model_poi, model_nb) # NB is better


df %>%
  group_by(year,month) %>%
  summarise(
    means = mean(MR_all_cause),
    variances = var(MR_all_cause),
    ratio = variances/means)




# PLAYGROUND --------------



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