## Different Models comparisons by Sub-samples
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

source("Scripts/Functions.R",encoding="UTF-8")

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

# Load required data -----

## Panel data -----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")
# df <- read.delim("Data/Panel Data/panelData_65.csv",sep=";") # 65+ deaths
# df <- df %>% rename(pop75_share=pop65_share)

# Change some parameters
df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

# order appropiately 
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna)

## ROBUSTNESS -----

### Sex data -------------
df_sex <- read.delim("Data/Panel Data/panelData_sex.csv",sep=";")
# df_sex <- read.delim("Data/Panel Data/panelData_sex_65.csv",sep=";") # 65+
# df_sex <- df_sex %>% rename(pop75_share=pop65_share)


df_sex <- df_sex %>% filter(!is.na(pm25Exp_10ug))

# Change some parameters
df_sex <- df_sex %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

### Population ----- 
# use only communes with at least 500 people in the age group (on average)
pop_commune <- df %>% group_by(commune) %>% summarise(pop75=mean(pop75))
# ggplot(pop_commune,aes(pop75))+stat_ecdf()
nrow(filter(pop_commune,pop75>500))/nrow(pop_commune) # 75% of communes
pop_commune <- pop_commune %>% filter(pop75>500) %>% pull(commune) # 246 for 75+,

df <- df %>% mutate(pop500=(commune %in% pop_commune))

### Bad regions identification ------
df <- df %>% mutate(bad_region=if_else(region %in% c("15","3","12","11"),1,0)) # XI for overprediction


## HETEROGENEITY -----


### PM2.5 above 20 ----------
# Average over the whole period
com_pm25 <- df %>% group_by(commune) %>% 
  summarise(pm25=mean(pm25_exposure), n=n()) %>% ungroup() %>% 
  arrange(desc(pm25))
# Cut-off - 20ug/m3 above and below
nrow(filter(com_pm25,pm25>20))/nrow(com_pm25) # 50.46%
com_pm25 <- com_pm25 %>% filter(pm25>20) %>% pull(commune) # 165
df <- df %>% mutate(comPM25=(commune %in% com_pm25))


### Pop 75 share above median -------
# Above and below median Pop 75 - Need to sample communes
commune_pop <- df %>% group_by(commune) %>% summarise(pop75_share=mean(pop75_share))
median_pop <-  median(commune_pop$pop75_share) # 0.04496193
commune_pop <- commune_pop %>% filter(pop75_share>0.045) %>% pull(commune) # 163
# commune_pop <- commune_pop %>% filter(pop75_share>0.108) %>% pull(commune) # for 65+
df <- df %>% mutate(pop_case=if_else(commune %in% commune_pop,"Above Median","Below Median"))
rm(commune_pop)


### Urban Share -----
com_rural <- df %>% group_by(commune) %>% 
  summarise(total_pop_urban=sum(total_pop_urban,na.rm=T)/12/18,
            total_pop_rural=sum(total_pop_rural,na.rm=T)/12/18) %>% ungroup() %>% 
  mutate(rural_share=total_pop_rural/(total_pop_rural+total_pop_urban)) %>% 
  arrange(desc(rural_share))
# ggplot(com_rural,aes(rural_share))+stat_ecdf()
nrow(filter(com_rural,rural_share>0.5))/nrow(com_rural) # 28%
nrow(filter(com_rural,rural_share>0.6))/nrow(com_rural) # 16%
nrow(filter(com_rural,rural_share>0.7))/nrow(com_rural) # 8.5%
nrow(filter(com_rural,rural_share>0.8))/nrow(com_rural) # 4%
nrow(filter(com_rural,rural_share>0.4))/nrow(com_rural) # 40%
nrow(filter(com_rural,rural_share>0.3))/nrow(com_rural) # 51%
nrow(filter(com_rural,rural_share>0.2))/nrow(com_rural) # 63%

# Communes with more than x% of rural habitants
com_rural <- com_rural %>% filter(rural_share>0.3) %>% pull(commune) # 93 for 50%, 52 for 60%
df <- df %>% mutate(comRural=(commune %in% com_rural))

### income -----
income <-  read.csv("Data/socioeconomic.csv")
income_percentil <- quantile(income$income_mean,
                               probs = c(0.3,0.65,0.95),na.rm=T)

# ggplot(income,aes(income_mean))+
#   geom_histogram(bins=50,fill="grey",col="black")+
#   geom_vline(xintercept = income_percentil,col="red")+
#   annotate("text",x=income_percentil+30,y=50,label=paste0("P",names(income_percentil)),
#            angle = 90,col="red")+
#   coord_cartesian(expand = F)+
#   labs(x="Mean Annual Income [USD]",y="Number of Communes")+
#   theme_bw(20)+
#   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
# ggsave("Figures/Other/Income_Distribution.png")

df <- df %>% left_join(income)
rm(income)

# income
df <- df %>% mutate(income_group=case_when(
  income_mean<income_percentil[1] ~ "Below P30 (less than $3,352)",
  income_mean<income_percentil[2] ~ "Between P30-P65 ($3,352 to $4,320)",
  income_mean<income_percentil[3] ~ "Between P65-P95 ($4,320 to $6,990)",
  T ~ "Above P95 (more than $6,990)"))
# df %>% group_by(commune,income_group) %>% tally() %>% view()


# share of 75 plus per income group
df %>% 
  group_by(income_group) %>% 
  reframe(n=n(),
          pop75=sum(pop75)/12,pop=sum(pop)/12) %>% ungroup() %>% 
  mutate(share_75=pop75/pop*100)


# see communes by incom
df %>% group_by(income_group,region) %>% tally() %>% 
  mutate(n=n/18/12) %>% 
  group_by(region) %>% 
  mutate(n=n/sum(n)) %>% #share 
  pivot_wider(names_from = income_group, values_from = n) %>% arrange(region)
# RM: 11 communes of 23
df %>% dplyr::select(region,NOM_COMUNA,income_group) %>% 
  filter(income_group=="Above P95 (more than $6,990)") %>% 
  group_by(region,NOM_COMUNA) %>% tally()


## OTHERS (not used) -------------

### PM2.5 -----
# Above and below median PM2.5 for each communes
commune_mp25 <- df %>% group_by(commune) %>% summarise(pm25_exposure=mean(pm25_exposure),
                                                       pop75=mean(pop75))
median_pm25 <- matrixStats::weightedMedian(commune_mp25$pm25_exposure,commune_mp25$pop75)
median_pm25
# Sampling should be for the entire commune over the whole period
commune_mp25 <- commune_mp25 %>% filter(pm25_exposure>median_pm25) %>% pull(commune)

df <- df %>% mutate(pm25_case=if_else(commune %in% commune_mp25,"Above Median","Below Median"))
# df %>% group_by(commune,pm25_case) %>% tally() %>% view()

rm(commune_mp25)

commune_mp25 <- df %>% group_by(commune) %>% summarise(pm25_exposure=mean(pm25_exposure))

# pm.25 level
commune_mp25 <- commune_mp25 %>% mutate(pm25_level=case_when(
  pm25_exposure<15 ~ "PM2.5 below 15",
  pm25_exposure<20 ~ "PM2.5 15-20",
  pm25_exposure<25 ~ "PM2.5 20-25",
  pm25_exposure<30 ~ "PM2.5 25-30",
  T ~ "PM2.5 Above 30"))
commune_mp25 %>% group_by(pm25_level) %>% tally()
commune_mp25$pm25_exposure <- NULL

df <- df %>% left_join(commune_mp25)
rm(commune_mp25)

### Regions by zone -----
df <- df %>% mutate(zone=case_when(
  region %in% c("15","1","2","3","4") ~ "North",
  region %in% c("5","13","6","7") ~ "Center",
  region %in% c("11","12") ~ "Patagonia",
  T ~ "South"))
df %>% group_by(zone,region) %>% tally()
# pm.25 by zone
df %>%
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  group_by(zone) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(total_pop,na.rm = T),
            n=n()) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) 

### only polluted regions -----
df <- df %>% mutate(centersouth_regions=region %in% c("5","13","6","7","8","16","9","14"))

### Season -----
df <- df %>% 
  mutate(quarter_text=case_when(
    quarter==1 ~ "Summer",
    quarter==2 ~ "Fall",
    quarter==3 ~ "Winter",
    T ~ "Spring") %>% factor(levels=c("Fall","Spring","Winter","Summer")))

### Temperature quartiles -----
commune_temp <- df %>% group_by(commune) %>% summarise(landTemp=mean(landTemp))
temp_qt <- quantile(commune_temp$landTemp,c(0.25,0.5,0.75))

commune_temp <- commune_temp %>% mutate(temp_qt=case_when(
  landTemp<temp_qt[1] ~ "Temp Q I",
  landTemp<temp_qt[2] ~ "Temp Q II",
  landTemp<temp_qt[3] ~ "Temp Q III",
  T ~ "Temp Q IV"))
commune_temp %>% group_by(temp_qt) %>% tally()
commune_temp$landTemp <- NULL

df <- df %>% left_join(commune_temp)

### Income quantiles -----

commune_income <- df %>% group_by(commune) %>% summarise(income_mean=mean(income_mean)) %>% na.omit()
income_qt <- quantile(commune_income$income_mean,c(0.2,0.4,0.6,0.8))

commune_income <- commune_income %>% mutate(income_qt=case_when(
  income_mean<income_qt[1] ~ "Income Q I",
  income_mean<income_qt[2] ~ "Income Q II",
  income_mean<income_qt[3] ~ "Income Q III",
  income_mean<income_qt[4] ~ "Income Q IV",
  income_mean<1e6 ~ "Income Q V",
  T ~ "NA"))
commune_income %>% group_by(income_qt) %>% tally()
commune_income$income_mean <- NULL

df <- df %>% left_join(commune_income)
df %>% group_by(income_qt) %>% tally()


# Function to run Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression
# see: https://github.com/echolab-stanford/NCC2018/blob/master/scripts/functions.R
runModel <- function(data_,
                     name,
                     formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year_quarter+commune+offset(log(pop75))"){
  
  mod <- glm.nb(as.formula(formula),
                     data = data_,
                     na.action=na.omit)

  coef <- summary(mod)$coefficients
  mean <- weighted.mean(data_$MR_all_cause,data_$pop75,na.rm=T)
  mean_pm25 <- weighted.mean(data_$pm25_exposure,data_$pop75,na.rm=T)
  mean_temp <- weighted.mean(data_$landTemp,data_$pop75,na.rm=T)

  # clustered standard errors by commune
  cluster_se <- vcovCL(mod, cluster = data_$commune)
  cluster_se <- sqrt(diag(cluster_se))

  # only PM2.5
  out <- data.frame(var=name,param=rownames(coef),est=coef[,1],se=cluster_se,mean_MR=mean,
                    N=nobs(mod),bic=BIC(mod),mean_pm25=mean_pm25,mean_temp=mean_temp)
  return(out)
}

# Run Models ------
results <- list()  #lists to save results

# Final Figure Models
results[[1]] <- runModel(data=df,name="Full Sample")  # full sample
results[[2]] <- runModel(data=filter(df_sex,sex=="Hombre"),name="Sex: Male") 
results[[3]] <- runModel(data=filter(df_sex,sex=="Mujer"),name="Sex: Female") 
results[[4]] <- runModel(data=filter(df,pop500==T),name="Pop. 75+ Above 500")
# results[[4]] <- runModel(data=filter(df,pop500==T),name="Pop. 65+ Above 500") # 65+
results[[5]] <- runModel(data=filter(df,bad_region==0),name="Excluding regions with low satellite accuracy")
results[[6]] <- runModel(data=filter(df,REGION ==13),name="Only Metropolitan region")
results[[7]] <- runModel(data=filter(df,REGION !=13),name="Excluding Metropolitan region")
results[[8]] <- runModel(data=filter(df,comPM25==F),name="PM2.5 below 20 ug/m3")
results[[9]] <- runModel(data=filter(df,comPM25==T),name="PM2.5 above 20 ug/m3")
results[[10]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 75+ share below median (<4.5%)")
results[[11]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 75+ share above median (>4.5%)")
# 65+ case
# results[[10]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 65+ share below median (<10.8%)")
# results[[11]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 65+ share above median (>10.8%)")
results[[12]] <- runModel(data=filter(df,comRural==F),name="Urban Commune")
results[[13]] <- runModel(data=filter(df,comRural==T),name="Rural Commune (>30% share poulation)")
results[[14]] <- runModel(data=filter(df,income_qt=="Income Q I"),name="I Quintile Income (below $3,182)")
results[[15]] <- runModel(data=filter(df,income_qt=="Income Q II"),name="II Quintile Income ($3,182-3,577$)")
results[[16]] <- runModel(data=filter(df,income_qt=="Income Q III"),name="III Quintile Income ($3,577-$4,138)")
results[[17]] <- runModel(data=filter(df,income_qt=="Income Q IV"),name="IV Quintile Income ($4,138-$4,961)")
results[[18]] <- runModel(data=filter(df,income_qt=="Income Q V"),name="V Quintile Income (above $4,961)")
results[[19]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardioRespiratory,
                                      MR_all_cause=MR_cardioRespiratory),name="Cardiorespiratory cause")
results[[20]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardio,
                                      MR_all_cause=MR_cardio),name="Cardiovascular cause")
results[[21]] <- runModel(data=mutate(df,death_count_all_cause=death_count_respiratory,
                                      MR_all_cause=MR_respiratory),name="Respiratory cause")
results[[22]] <- runModel(data=mutate(df,death_count_all_cause=death_count_all_cause_NoCDP,
                                      MR_all_cause=MR_all_cause_NoCDP),name="All-cause excluding Cardiorespiratory")
results[[23]] <- runModel(data=mutate(df,death_count_all_cause=death_count_external,
                                      MR_all_cause=MR_external),name="External cause")
#Others
# results[[4]] <- runModel(data=filter(df,pm25_case=="Above Median"),name="PM2.5: Above Median") 
# results[[5]] <- runModel(data=filter(df,pm25_case=="Below Median"),name="PM2.5: Below Median") 
# results[[6]] <- runModel(data=filter(df,pm25_level=="PM2.5 below 15"),name="PM2.5 Below 15") 
# results[[7]] <- runModel(data=filter(df,pm25_level=="PM2.5 15-20"),name="PM2.5 15-20") 
# results[[8]] <- runModel(data=filter(df,pm25_level=="PM2.5 20-25"),name="PM2.5 20-25") 
# results[[9]] <- runModel(data=filter(df,pm25_level=="PM2.5 25-30"),name="PM2.5 25-30") 
# results[[10]] <- runModel(data=filter(df,pm25_level=="PM2.5 Above 30"),name="PM2.5 Above 30") 
# results[[17]] <- runModel(data=filter(df,zone=="North"),name="Zone: North")
# results[[18]] <- runModel(data=filter(df,zone=="Center"),name="Zone: Center")
# results[[19]] <- runModel(data=filter(df,zone=="South"),name="Zone: South")
# results[[20]] <- runModel(data=filter(df,zone=="Patagonia"),name="Zone: Patagonia")
# # by season
# results[[27]] <- runModel(data=filter(df,quarter_text =="Summer"),name="Quarter: Summer",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# results[[28]] <- runModel(data=filter(df,quarter_text =="Fall"),name="Quarter: Fall",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# results[[29]] <- runModel(data=filter(df,quarter_text =="Winter"),name="Quarter: Winter",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# results[[30]] <- runModel(data=filter(df,quarter_text =="Spring"),name="Quarter: Spring",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# # by periods of year
# results[[31]] <- runModel(data=filter(df,year %in% c("2002","2003","2004","2005")),name="Period: 2002-2005")
# results[[32]] <- runModel(data=filter(df,year %in% c("2006","2007","2008","2009","2010")),name="Period: 2006-2010")
# results[[33]] <- runModel(data=filter(df,year %in% c("2011","2012","2013","2014","2015")),name="Period: 2011-2015")
# results[[34]] <- runModel(data=filter(df,year %in% c("2016","2017","20018","2019")),name="Period: 2016-2019")
# results[[40]] <- runModel(data=filter(df,centersouth_regions==T),name="Center-South Regions")
# temperature quantiles
# results[[1]] <- runModel(data=filter(df,temp_qt=="Temp Q I"),name="Quantile T° I (below 17.8)") 
# results[[2]] <- runModel(data=filter(df,temp_qt=="Temp Q II"),name="Quantile T° II (17.8-22.0)") 
# results[[3]] <- runModel(data=filter(df,temp_qt=="Temp Q III"),name="Quantile T° III (22.0-26.1)") 
# results[[4]] <- runModel(data=filter(df,temp_qt=="Temp Q IV"),name="Quantile T° IV (above 26.1)") 
# temperature quintiles
# results[[14]] <- runModel(data=filter(df,income_group =="Below P30 (less than $3,352)"),
#                          name="Income less $3,352 (P30)")
# results[[15]] <- runModel(data=filter(df,income_group =="Between P30-P65 ($3,352 to $4,320)"),
#                          name="Income $3,352-$4,320 (P30-P65)")
# results[[16]] <- runModel(data=filter(df,income_group =="Between P65-P95 ($4,320 to $6,990)"),
#                          name="Income $4,320-$6,990 (P65-P95)")
# results[[17]] <- runModel(data=filter(df,income_group =="Above P95 (more than $6,990)"),
#                          name="Above $6,990 (P95)")


# merge results
res <- do.call("rbind",results)

# save data
write.csv(res,"Data/Models/modelResults.csv",row.names = F)
# write.csv(res,"Data/Models/modelResults_65.csv",row.names = F)

res <- read.csv("Data/Models/modelResults.csv")
# res <- read.csv("Data/Models/modelResults_65.csv")

fig_name <- "Figures/Model/%s.png"
# fig_name <- sprintf(fig_name,"Models_Subsample")
# fig_name <- sprintf(fig_name,"Models_Subsample65")
fig_name <- sprintf(fig_name,"Models_Subsample_Temp")
# fig_name <- sprintf(fig_name,"Models_Subsample65_Temp")

# Calculate RR and C.I. -----
rows <- res %>% filter(param=="pm25Exp_10ug") %>% nrow()
x <- res %>% 
  # filter(param=="pm25Exp_10ug") %>%
  filter(param=="landTemp") %>%
  mutate(rr=exp(est)*100-100,
         rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
         rr_high=exp(est+1.96*se)*100-100) %>% 
  mutate(N=formatC(N,0, big.mark=",")) %>% 
  mutate(mean_MR=format(round(mean_MR,2),nsmall=2),
         mean_pm25=format(round(mean_pm25,2),nsmall=2),
         mean_temp=format(round(mean_temp,2),nsmall=2)) %>% 
  mutate(ci=paste0(format(round(rr,1),nsmall=1)," (",
                   format(round(rr_low,1),nsmall=1),", ",
                   format(round(rr_high,1),nsmall=1),")")) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  mutate(rowname=1:rows) %>% 
  mutate(label=paste0(var," (n=",N,")"))


# Figure for article ------
x$var %>% unique()

# select relevant to show
robustness <- c( "Full Sample","Robustness",
                 "Sex: Male","Sex: Female",
                 "Pop. 75+ Above 500",
                 # "Pop. 65+ Above 500",
                 "Excluding regions with low satellite accuracy")
heterogen <- c("Heterogeneity",
               "Only Metropolitan region","Excluding Metropolitan region",
               "PM2.5 below 20 ug/m3","PM2.5 above 20 ug/m3",
               "Pop. 75+ share below median (<4.5%)","Pop. 75+ share above median (>4.5%)",
               # "Pop. 65+ share below median (<10.8%)","Pop. 65+ share above median (>10.8%)", #65+ case
               "Urban Commune","Rural Commune (>30% share poulation)",
               "I Quintile Income (below $3,182)","II Quintile Income ($3,182-3,577$)",
               "III Quintile Income ($3,577-$4,138)","IV Quintile Income ($4,138-$4,961)",
               "V Quintile Income (above $4,961)")
other_causes <- c("Other Mortality Causes",
                  "Cardiorespiratory cause","Cardiovascular cause",
                  "Respiratory cause","All-cause excluding Cardiorespiratory",
                  "External cause")

# add rows
y <- rbind(x,c("Robustness",rep(NA,ncol(x)-1)),
           c("Heterogeneity",rep(NA,ncol(x)-1)),
           c("Other Mortality Causes",rep(NA,ncol(x)-1)))

y <- y %>% 
  filter(var %in% c(robustness,heterogen,other_causes)) %>%
  mutate(var=factor(var,levels=rev(c(robustness,heterogen,other_causes)))) %>%
  mutate(rr=as.numeric(rr),
         rr_low=as.numeric(rr_low),
         rr_high=as.numeric(rr_high)) %>% 
  mutate(title=var %in% c("Robustness","Heterogeneity","Other Mortality Causes")) %>% 
  mutate(pm25_var=str_detect(var,"PM2.5")) # special text for this
rows <- y %>% nrow()


# Figure
# Draw figure with Table Incorporated
# Key Idea: Expand limits of graph and put text in each row, then do some formating

# Need to comment/uncomment to recreate the Temperature and 65+ version
font_size <- 7.5
range(y$rr_low,na.rm=T);range(y$rr_high,na.rm=T)
max_value <- ceiling(max(y$rr_high,na.rm=T))
temp_adj <- 0
# temp_adj <- 2 # for temp
ggplot(y,aes(var,rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=0.6,aes(col=signif))+
  # geom_point(size=0.6,col="red")+ # all T are significant
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_vline(xintercept = c(6.5,20.5,25.5),
             col="grey",linewidth=0.3)+
  geom_vline(xintercept = c(11.5,13.5,15.5,17.5,22.5),
             col="grey",linewidth=0.15,linetype="dashed")+
  labs(x="",y=lab_rr)+
  # labs(x="",y=lab_rr_temp)+
  # add bottom bar
  geom_segment(x = 0.01, xend = 0.01, yend = max_value,
               y=-1,
               # y = -2.5, # temp
               col="black",linewidth=0.5)+
  # adjust range of axis
  coord_flip(xlim=c(0,rows+2),expand = F)+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(seq(0,5,2.5)),
                     # breaks = c(seq(-2,0,1)), # temp
                     limits = c(-14,9.3)) +
                     # limits = c(-16,4)) + # temp
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  theme_bw(font_size)+
  # add text data
  geom_text(y=-14-temp_adj,x=rows+1,label="Sample",hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(data=filter(y,!title,!pm25_var),y=-14-temp_adj,aes(label=var),
            hjust = 0,size=font_size*5/14 * 0.8)+
  # special rows for pm2.5
  geom_text(data=filter(y,pm25_var,str_detect(var,"above")),y=-14-temp_adj,hjust = 0,
            size=font_size*5/14 * 0.8,label=expression(paste("PM"[2.5], " above 20 ", mu, "g/m"^3, "")))+
  geom_text(data=filter(y,pm25_var,str_detect(var,"below")),y=-14-temp_adj,hjust = 0,
            size=font_size*5/14 * 0.8,label=expression(paste("PM"[2.5], " below 20 ", mu, "g/m"^3, "")))+
  # titles in bold
  geom_text(data=filter(y,title),y=-14-temp_adj,aes(label=var),
            hjust = 0,size=font_size*5/14 * 0.8,fontface = "bold")+
  geom_text(y=-7-temp_adj,x=rows+1,label="n",size=font_size*5/14 * 0.8)+
  geom_text(y=-7-temp_adj,aes(label=N),size=font_size*5/14 * 0.8)+
  geom_text(y=-5-temp_adj,x=rows+1,label="Monthly MR",size=font_size*5/14 * 0.8)+
  geom_text(y=-5-temp_adj,aes(label=mean_MR),size=font_size*5/14 * 0.8)+
  geom_text(y=-2.5,x=rows+1,size=font_size*5/14 * 0.8,
  label=expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "]")))+
  geom_text(y=-2.5,aes(label=mean_pm25),size=font_size*5/14 * 0.8)+
  # geom_text(y=-4.5,x=rows+1,label="Mean T [°C]",size=font_size*5/14 * 0.8)+
  # geom_text(y=-4.5,aes(label=mean_temp),size=font_size*5/14 * 0.8)+
  geom_text(y=max_value+1,x=rows+1,label="Effect C.I. 95%",size=font_size*5/14 * 0.8)+
  geom_text(y=max_value+1,aes(label=ci),size=font_size*5/14 * 0.8)+
  # Modify theme to look good
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border=element_blank(),
        axis.line.y=element_blank(),
        # axis.line.x = element_line(colour = "black"),
        axis.text.y=element_blank(),
        axis.title.x = element_text(hjust = 0.9),
        axis.ticks.y = element_blank())

ggsave(fig_name, ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7*2,height=8.7)

# EoF