## Different model specifications: NB, OLS, Poisson
## PBH
## June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich) # for robust and clustered standard errors

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load Panel Data ----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")
# df <- read.delim("Data/Panel Data/panelData_65.csv",sep=";") # 65+ deaths

df <- df %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(quarter=factor(quarter),
         year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         month=as.factor(month),
         commune=as.factor(codigo_comuna),
         commune=relevel(commune,ref="13101")) # Santiago

# Fit Models -----

# Negative Binomial
model_nb <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                     offset(log(pop75)), 
                   data = df,
                   na.action=na.omit)
# Poisson
model_poi <- glm(death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+
                   offset(log(pop75)), 
                 data = df,family ="poisson",na.action=na.omit)


# comparing NB to Poisson - https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
pchisq(2 * (logLik(model_nb) - logLik(model_poi)), df = 1, lower.tail = FALSE) # NB is much more appropiate
lmtest::lrtest(model_poi, model_nb) # NB is better

# OLS with grouped fixed effects
model_ols <- lfe::felm(MR_all_cause ~ pm25Exp_10ug+landTemp | year_quarter+commune,
                       data=df,weights=df$pop75)
# summary(model_ols)

# model_ols_lm <- lm(MR_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune,
#                    data=df,weights=df$pop75) # identical

# Zero-Inflated Poisson and Negative Binomial Model 
library(pscl)
# Key idea: structural zeros and zero-inflation (account for prob. of getting a zero)

zip <- zeroinfl(death_count_all_cause ~ pm25Exp_10ug + landTemp + year_quarter + commune +
                  offset(log(pop75)) | commune,  # commune as regressor for zero prob.
                data = df, dist = 'poisson')

zinb <- zeroinfl(death_count_all_cause ~ pm25Exp_10ug + landTemp + year_quarter + commune +
                   offset(log(pop75)) | commune , 
                 data = df, dist = 'negbin')

# Show effect of PM2.5 on MR of a similar magnitude
# Negative effect of PM2.5 and T on the probability of having a 0 count event

# Test for the Existence of Zero-Inflation in R
# likelihood ratio test
# 2(l(full) - l(null)) converges in distribution to \chi^2_{df(full) - df(null)} under the null.
1 - pchisq(as.numeric(2*(logLik(zip) - logLik(model_poi))),
           df = length(coef(zip)) - length(coef(model_poi)))



# Comparison figure -----
br <- weighted.mean(df$MR_all_cause,df$pop75,na.rm=T)

models_nb_res <- getModelInfo(model_nb,"Negative Binomial")
models_nb_res <- rbind(models_nb_res,getModelInfo(model_poi,"Poisson"))
models_nb_res <- rbind(models_nb_res,getModelInfo(model_ols,"OLS",ols=T,baseRate = br))


# save results
write.csv(models_nb_res,"Data/modelMethodsResults.csv",row.names = F)
# models_nb_res <- read.csv("Data/modelMethodsResults.csv")

# Figure 
models_nb_res %>% 
  mutate(rowname=1:nrow(models_nb_res)) %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
  mutate(name=name %>% str_replace("Negative Binomial","Negative \n Binomial")) %>% 
  ggplot(aes(reorder(name,rowname,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  # geom_point(size=1,aes(col=signif))+
  geom_point(size=1,col="red")+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  coord_flip()+
  # scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  labs(x="",
       y=lab_rr)+
  # Modify theme to look good
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=7,hjust=1),
        legend.position = "none")

ggsave("Figures/Model/Model_Method.png", ggplot2::last_plot(),
       # ggsave("Figures/Model/Model_Specifications_Temp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# EoF