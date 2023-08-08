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
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")
# df <- read.delim("Data/Panel Data/panelData_65.csv",sep=";") # 65+ deaths

# income data
income <-  read.csv("Data/socioeconomic.csv")

income_percentil <- quantile(income$income_mean,
                               probs = c(0.3,0.65,0.95),na.rm=T)

ggplot(income,aes(income_mean))+
  geom_histogram(bins=50,fill="grey",col="black")+
  geom_vline(xintercept = income_percentil,col="red")+
  annotate("text",x=income_percentil+30,y=50,label=paste0("P",names(income_percentil)),
           angle = 90,col="red")+
  coord_cartesian(expand = F)+
  labs(x="Mean Annual Income [USD]",y="Number of Communes")+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave("Figures/Other/Income_Distribution.png")

df <- df %>% left_join(income)
rm(income)

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

df <- df %>% 
  mutate(quarter_text=case_when(
    quarter==1 ~ "Summer",
    quarter==2 ~ "Fall",
    quarter==3 ~ "Winter",
    T ~ "Spring") %>% factor(levels=c("Fall","Spring","Winter","Summer")))

# sex data
df_sex <- read.delim("Data/Panel Data/panelData_sex.csv",sep=";")
# df_sex <- read.delim("Data/Panel Data/panelData_sex_65.csv",sep=";") # 65+

df_sex <- df_sex %>% filter(!is.na(pm25Exp_10ug))

# Change some paramters
df_sex <- df_sex %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))


# ggplot(df,aes(pm25_exposure,mortality,col=quarter_text))+
#   # geom_point(alpha=.5, data=dplyr::select(df,-quarter),col="grey")+
#   geom_point(alpha=.5)+
#   geom_smooth(method="lm")+
#   geom_smooth(method="lm", data=dplyr::select(df,-quarter_text),col="grey")+
#   facet_wrap(~quarter_text)+
#   labs(x="PM2.5 Exposure [ug/m3]",
#        y="75+ All Cause Mortality Rate, per 1,000",col="")+
#   theme(legend.position = "none")


## Prepare data for analysis -----

# Above and below median PM2.5 for each communes
commune_mp25 <- df %>% group_by(commune) %>% summarise(pm25_exposure=mean(pm25_exposure),
                                                       pop75=mean(pop75))
median_pm25 <- matrixStats::weightedMedian(commune_mp25$pm25_exposure,commune_mp25$pop75)
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


# Above and below median Pop 75 - Need to sample communes
commune_pop <- df %>% group_by(commune) %>% summarise(pop75=mean(pm25_exposure))
median_pop <-  median(commune_pop$pop75)
commune_pop <- commune_pop %>% filter(pop75>median_pop) %>% pull(commune)
df <- df %>% mutate(pop_case=if_else(commune %in% commune_pop,"Above Median","Below Median"))
rm(commune_pop)
# df %>% group_by(commune,pop_case) %>% tally() %>% view()

# Population 
# use only communes with at least 500 people in the age group (on average)
pop_commune <- df %>% group_by(commune) %>% summarise(pop75=mean(pop75))
ggplot(pop_commune,aes(pop75))+stat_ecdf()
nrow(filter(pop_commune,pop75>500))/nrow(pop_commune) # 75% of communes
pop_commune <- pop_commune %>% filter(pop75>500) %>% pull(commune) # 246 for 75+,

df <- df %>% mutate(pop500=(commune %in% pop_commune))

# Urban Share
com_rural <- df %>% group_by(commune) %>% 
  summarise(total_pop_urban=sum(total_pop_urban,na.rm=T)/12/18,
            total_pop_rural=sum(total_pop_rural,na.rm=T)/12/18) %>% ungroup() %>% 
  mutate(rural_share=total_pop_rural/(total_pop_rural+total_pop_urban)) %>% 
  arrange(desc(rural_share))
ggplot(com_rural,aes(rural_share))+stat_ecdf()
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


# Bad regions identification
df <- df %>% mutate(bad_region=if_else(region %in% c("15","3","12"),1,0))

# Regions by zone
df <- df %>% mutate(zone=case_when(
  region %in% c("15","1","2","3","4") ~ "North",
  region %in% c("5","13","6","7") ~ "Center",
  region %in% c("11","12") ~ "Patagonia",
  T ~ "South"))
df %>% group_by(zone,region) %>% tally()

# only polluted regions
df <- df %>% mutate(centersouth_regions=region %in% c("5","13","6","7","8","16","9","14"))


# pm.25 by zone
df %>%
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  group_by(zone) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(total_pop,na.rm = T),
            n=n()) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) 


# order appropiately to ensure lags are well calculated
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna)

# income
df <- df %>% mutate(income_group=case_when(
  income_mean<income_percentil[1] ~ "Below P30 (less than $3,352)",
  income_mean<income_percentil[2] ~ "Between P30-P65 ($3,352 to $4,320)",
  income_mean<income_percentil[3] ~ "Between P65-P95 ($4,320 to $6,990)",
  T ~ "Above P95 (more than $6,990)"))
# df %>% group_by(commune,income_group) %>% tally() %>% view()

# share of 75 plus per commune
df %>% 
  group_by(income_group) %>% 
  reframe(n=n(),
          pop75=sum(pop75)/12,pop=sum(pop)/12) %>% ungroup() %>% 
  mutate(share_75=pop75/pop*100)


# see communes by incom
df %>% group_by(income_group,region) %>% tally() %>% 
  mutate(n=n/18/12) %>% 
  pivot_wider(names_from = income_group, values_from = n)
# RM: 11 communes of 23
df %>% dplyr::select(region,NOM_COMUNA,income_group) %>% 
  filter(income_group=="Above P95 (more than $6,990)") %>% 
  group_by(region,NOM_COMUNA) %>% tally()



## Function to run Negative Binomial Model ----- 
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

results[[1]] <- runModel(data=df,name="Full Sample")  # full sample
results[[2]] <- runModel(data=filter(df_sex,sex=="Hombre"),name="Sex: Male") 
results[[3]] <- runModel(data=filter(df_sex,sex=="Mujer"),name="Sex: Female") 
results[[4]] <- runModel(data=filter(df,pm25_case=="Above Median"),name="PM2.5: Above Median") 
results[[5]] <- runModel(data=filter(df,pm25_case=="Below Median"),name="PM2.5: Below Median") 
results[[6]] <- runModel(data=filter(df,pm25_level=="PM2.5 below 15"),name="PM2.5 Below 15") 
results[[7]] <- runModel(data=filter(df,pm25_level=="PM2.5 15-20"),name="PM2.5 15-20") 
results[[8]] <- runModel(data=filter(df,pm25_level=="PM2.5 20-25"),name="PM2.5 20-25") 
results[[9]] <- runModel(data=filter(df,pm25_level=="PM2.5 25-30"),name="PM2.5 25-30") 
results[[10]] <- runModel(data=filter(df,pm25_level=="PM2.5 Above 30"),name="PM2.5 Above 30") 
results[[11]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 75+: Above Median") 
results[[12]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 75+: Below Median")
results[[13]] <- runModel(data=filter(df,pop500==T),name="Pop. 75+ Above 500")
results[[14]] <- runModel(data=filter(df,comRural==T),name="Rural Commune (>30% share poulation)")
results[[15]] <- runModel(data=filter(df,comRural==F),name="Urban Commune")
results[[16]] <- runModel(data=filter(df,bad_region==0),name="Excluding regions with low satellite accuracy")
results[[17]] <- runModel(data=filter(df,zone=="North"),name="Zone: North")
results[[18]] <- runModel(data=filter(df,zone=="Center"),name="Zone: Center")
results[[19]] <- runModel(data=filter(df,zone=="South"),name="Zone: South")
results[[20]] <- runModel(data=filter(df,zone=="Patagonia"),name="Zone: Patagonia")
results[[21]] <- runModel(data=filter(df,REGION ==13),name="Only Metropolitan region")
results[[22]] <- runModel(data=filter(df,REGION !=13),name="Excluding Metropolitan region")
# by Income
results[[23]] <- runModel(data=filter(df,income_group =="Below P30 (less than $3,352)"),
                         name="Income less $3,352 (P30)")
results[[24]] <- runModel(data=filter(df,income_group =="Between P30-P65 ($3,352 to $4,320)"),
                         name="Income $3,352-$4,320 (P30-P65)")
results[[25]] <- runModel(data=filter(df,income_group =="Between P65-P95 ($4,320 to $6,990)"),
                         name="Income $4,320-$6,990 (P65-P95)")
results[[26]] <- runModel(data=filter(df,income_group =="Above P95 (more than $6,990)"),
                         name="Above $6,990 (P95)")
# by season
results[[27]] <- runModel(data=filter(df,quarter_text =="Summer"),name="Quarter: Summer",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[28]] <- runModel(data=filter(df,quarter_text =="Fall"),name="Quarter: Fall",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[29]] <- runModel(data=filter(df,quarter_text =="Winter"),name="Quarter: Winter",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[30]] <- runModel(data=filter(df,quarter_text =="Spring"),name="Quarter: Spring",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# by periods of year
results[[31]] <- runModel(data=filter(df,year %in% c("2002","2003","2004","2005")),name="Period: 2002-2005")
results[[32]] <- runModel(data=filter(df,year %in% c("2006","2007","2008","2009","2010")),name="Period: 2006-2010")
results[[33]] <- runModel(data=filter(df,year %in% c("2011","2012","2013","2014","2015")),name="Period: 2011-2015")
results[[34]] <- runModel(data=filter(df,year %in% c("2016","2017","20018","2019")),name="Period: 2016-2019")
# other Endpoints Full Model
results[[35]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardioRespiratory,
                                      MR_all_cause=MR_cardioRespiratory),name="Cardiorespiratory cause")
results[[36]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardio,
                                      MR_all_cause=MR_cardio),name="Cardiovascular cause")
results[[37]] <- runModel(data=mutate(df,death_count_all_cause=death_count_respiratory,
                                      MR_all_cause=MR_respiratory),name="Respiratory cause")
results[[38]] <- runModel(data=mutate(df,death_count_all_cause=death_count_all_cause_NoCDP,
                                      MR_all_cause=MR_all_cause_NoCDP),name="All-cause excluding Cardiorespiratory")
results[[39]] <- runModel(data=mutate(df,death_count_all_cause=death_count_external,
                                      MR_all_cause=MR_external),name="External cause")
results[[40]] <- runModel(data=filter(df,centersouth_regions==T),name="Center-South Regions")

# merge results
res <- do.call("rbind",results)
write.csv(res,"Data/Models/modelResults.csv",row.names = F)
# write.csv(res,"Data/Models/modelResults_65.csv",row.names = F)

res <- read.csv("Data/Models/modelResults.csv")
# res <- read.csv("Data/Models/modelResults_65.csv")

fig_name <- "Figures/Model/%s.png"
fig_name <- sprintf(fig_name,"Models_Subsample")
# fig_name <- sprintf(fig_name,"Models_Subsample65")
# fig_name <- sprintf(fig_name,"Models_Subsample_Temp")

# Summary Figure -----

# Calculate RR and C.I.
rows <- res %>% filter(param=="pm25Exp_10ug") %>% nrow()
x <- res %>% 
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
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

# Draw figure with Table Incorporated
# Key Idea: Expand limits of graph and put text in each row, then do some formating
font_size <- 7.5
range(x$rr_low);range(x$rr_high)
max_value <- ceiling(max(x$rr_high))
ggplot(x,aes(reorder(var,rowname,decreasing=T),rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_point(size=0.6,aes(col=signif))+
  # geom_point(size=0.6,col="red")+ # all T are significant
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_vline(xintercept = c(5.5,5.5,9.5,13.5,17.5,19.5,24.5,27.5,32.5,34.5,36.5),
             col="grey",linewidth=0.2)+
  labs(x="",y=lab_rr)+
  # labs(x="",y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  # add bottom bar
  geom_segment(x = 0.01, xend = 0.01, yend = max_value,
               y=-3,
               # y = -2.5, # temp
               col="black",linewidth=0.5)+
  # adjust range of axis
  coord_flip(xlim=c(0,rows+2),expand = F)+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(seq(-2.5,5,2.5)),
                     # breaks = c(seq(-2,1,1)), # temp
                     limits = c(-16,9.3)) +
                     # limits = c(-16,4)) + # temp
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  theme_bw(font_size)+
  # add text data
  geom_text(y=-16,x=rows+1,label="Sample",hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-16,aes(label=var),hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-9,x=rows+1,label="n",size=font_size*5/14 * 0.8)+
  geom_text(y=-9,aes(label=N),size=font_size*5/14 * 0.8)+
  geom_text(y=-7,x=rows+1,label="Base rate",size=font_size*5/14 * 0.8)+
  geom_text(y=-7,aes(label=mean_MR),size=font_size*5/14 * 0.8)+
  geom_text(y=-4.5,x=rows+1,size=font_size*5/14 * 0.8,
            label=expression(paste("Mean PM"[2.5], " [", mu, "g/m"^3, "]")))+
  geom_text(y=-4.5,aes(label=mean_pm25),size=font_size*5/14 * 0.8)+
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
        axis.ticks.y = element_blank())

ggsave("Figures/Model/AllModels.png", ggplot2::last_plot(),
# ggsave("Figures//Model/AllModels_65.png", ggplot2::last_plot(),
# ggsave("Figures//Model/AllModels_Temp.png", ggplot2::last_plot(),
# ggsave("Figures//Model/AllModels_Temp_65.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)

# Figure for article ------

x$var %>% unique()

# select relevant to show
robustness <- c( "Full Sample","Robustness",
                 "Sex: Male","Sex: Female",
                   "Pop. 75+: Below Median","Pop. 75+: Above Median",
                   "Pop. 75+ Above 500","Excluding regions with low satellite accuracy")
heterogen <- c("Heterogeneity",
               "Only Metropolitan region","Excluding Metropolitan region",
               "Urban Commune","Rural Commune (>30% share poulation)",
               "Income less $3,352 (P30)","Income $3,352-$4,320 (P30-P65)",
               "Income $4,320-$6,990 (P65-P95)","Above $6,990 (P95)")
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
  mutate(title=var %in% c("Robustness","Heterogeneity","Other Mortality Causes"))
rows <- y %>% nrow()

# Figure
# Need to comment/uncomment to recreate the Temperature and 65+ version
font_size <- 7.5
range(y$rr_low,na.rm=T);range(y$rr_high,na.rm=T)
max_value <- ceiling(max(y$rr_high,na.rm=T))
temp_adj <- 0
# temp_adj <- 2 # for temp
ggplot(y,aes(var,rr))+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  # geom_point(size=0.6,aes(col=signif))+
  geom_point(size=0.6,col="red")+ # all T are significant
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_vline(xintercept = c(6.5,15.5,23.5),
             col="grey",linewidth=0.3)+
  geom_vline(xintercept = c(10.5,12.5,16.5,19.5,21.5),
             col="grey",linewidth=0.15,linetype="dashed")+
  labs(x="",y=lab_rr)+
  # labs(x="",y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
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
  geom_text(data=filter(y,!title),y=-14-temp_adj,aes(label=var),
            hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(data=filter(y,title),y=-14-temp_adj,aes(label=var),
            hjust = 0,size=font_size*5/14 * 0.8,fontface = "bold")+
  geom_text(y=-7-temp_adj,x=rows+1,label="n",size=font_size*5/14 * 0.8)+
  geom_text(y=-7-temp_adj,aes(label=N),size=font_size*5/14 * 0.8)+
  geom_text(y=-5-temp_adj,x=rows+1,label="Base rate",size=font_size*5/14 * 0.8)+
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