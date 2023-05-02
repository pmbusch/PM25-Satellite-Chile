## Different Models comparisons by Sub-samples
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")

df <- df %>% filter(!is.na(pm25Exp_10ug))


# income data
income <-  read.delim("Data/Data_Original/income.csv",sep=";")

income_percentil <- quantile(income$income_mean_usd,
                               probs = c(0.3,0.65,0.95),na.rm=T)

ggplot(income,aes(income_mean_usd))+
  geom_histogram(bins=50,fill="grey",col="black")+
  geom_vline(xintercept = income_percentil,col="red")+
  annotate("text",x=income_percentil+30,y=50,label=paste0("P",names(income_percentil)),
           angle = 90,col="red")+
  coord_cartesian(expand = F)+
  labs(x="Mean Income [USD]",y="Number of Communes")+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave("Figures/Other/Income_Distribution.png")

df <- df %>% left_join(income)
rm(income)

# Change some parameters
df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year=as.factor(year),
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
df_sex <- read.delim("Data/panelData_sex.csv",sep=";")

df_sex <- df_sex %>% filter(!is.na(pm25Exp_10ug))

# Change some paramters
df_sex <- df_sex %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year=as.factor(year),
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
#        y="75+ All Cause Mortality Rate, per 1000 hab",col="")+
#   theme(legend.position = "none")


## Prepare data for analysis -----

# Above and below median PM2.5
median_pm25 <- matrixStats::weightedMedian(df$pm25_exposure,df$pop75)
df <- df %>% mutate(pm25_case=if_else(pm25_exposure>median_pm25,"Above Median","Below Median"))

# Above and below median Pop 75
median_pop <-  median(df$pop75)
df <- df %>% mutate(pop_case=if_else(pop75>median_pop,"Above Median","Below Median"))

# Bad regions identification
df <- df %>% mutate(bad_region=if_else(region %in% c("15","3","12"),1,0))

# Regions by zone
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


# order appropiately to ensure lags are well calculated
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna)

# income
df <- df %>% mutate(income_group=case_when(
  income_mean_usd<income_percentil[1] ~ "Below P30 (less than $386)",
  income_mean_usd<income_percentil[2] ~ "Between P30-P65 ($386 to $522)",
  income_mean_usd<income_percentil[3] ~ "Between P65-P95 ($522 to $898)",
  T ~ "Above P95 (more than $898)"))
df %>% group_by(income_group) %>% tally()

# pm.25 level
df <- df %>% mutate(pm25_level=case_when(
  pm25_exposure<10 ~ "PM2.5 below 10",
  pm25_exposure<20 ~ "PM2.5 10-20",
  pm25_exposure<30 ~ "PM2.5 20-30",
  pm25_exposure<40 ~ "PM2.5 30-40",
  pm25_exposure<50 ~ "PM2.5 40-50",
  T ~ "PM2.5>50"))
df %>% group_by(pm25_level) %>% tally()



## Function to run Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression
# see: https://github.com/echolab-stanford/NCC2018/blob/master/scripts/functions.R
runModel <- function(data_,
                     name,
                     formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+quarter+commune+offset(log(pop75))"){
  
  mod <- glm.nb(as.formula(formula), 
                     data = data_,
                     na.action=na.omit)
  
  coef <- summary(mod)$coefficients
  mean <- weighted.mean(data_$MR_all_cause,data_$pop75,na.rm=T)
  mean_pm25 <- weighted.mean(data_$pm25_exposure,data_$pop75,na.rm=T)
  
  # clustered standard errors by commune
  cluster_se <- vcovCL(mod, cluster = data_$commune)
  cluster_se <- sqrt(diag(cluster_se))
  
  # only PM2.5
  out <- data.frame(var=name,param=rownames(coef),est=coef[,1],se=cluster_se,mean_MR=mean,
                    N=nobs(mod),bic=BIC(mod),mean_pm25=mean_pm25)
  return(out)
}

# Run Models ------
results <- list()  #lists to save results

results[[1]] <- runModel(data=df,name="Full Sample")  # full sample
results[[2]] <- runModel(data=filter(df_sex,sex=="Hombre"),name="Sex: Male") 
results[[3]] <- runModel(data=filter(df_sex,sex=="Mujer"),name="Sex: Female") 
results[[4]] <- runModel(data=filter(df,pm25_case=="Above Median"),name="PM2.5: Above Median") 
results[[5]] <- runModel(data=filter(df,pm25_case=="Below Median"),name="PM2.5: Below Median") 
results[[6]] <- runModel(data=filter(df,pm25_exposure<10),name="PM2.5 below 10") 
results[[7]] <- runModel(data=filter(df,pm25_exposure>10,pm25_exposure<20),name="PM2.5 10-20") 
results[[8]] <- runModel(data=filter(df,pm25_exposure>20,pm25_exposure<30),name="PM2.5 20-30") 
results[[9]] <- runModel(data=filter(df,pm25_exposure>30,pm25_exposure<40),name="PM2.5 30-40") 
results[[10]] <- runModel(data=filter(df,pm25_exposure>40,pm25_exposure<50),name="PM2.5 40-50") 
results[[11]] <- runModel(data=filter(df,pm25_exposure>50),name="PM2.5 above 50") 
results[[12]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 75+: Above Median") 
results[[13]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 75+: Below Median") 
results[[14]] <- runModel(data=filter(df,bad_region==0),name="No bad regions (satellite)")
results[[15]] <- runModel(data=filter(df,zone=="North"),name="Zone: North")
results[[16]] <- runModel(data=filter(df,zone=="Center"),name="Zone: Center")
results[[17]] <- runModel(data=filter(df,zone=="South"),name="Zone: South")
results[[18]] <- runModel(data=filter(df,zone=="Patagonia"),name="Zone: Patagonia")
results[[19]] <- runModel(data=filter(df,REGION ==13),name="Only Metropolitan region")
results[[20]] <- runModel(data=filter(df,REGION !=13),name="No Metropolitan region")
# by Income
results[[21]] <- runModel(data=filter(df,income_group =="Below P30 (less than $386)"),
                         name="Income less $386 (P30)")
results[[22]] <- runModel(data=filter(df,income_group =="Between P30-P65 ($386 to $522)"),
                         name="Income $386-$522 (P30-P65)")
results[[23]] <- runModel(data=filter(df,income_group =="Between P65-P95 ($522 to $898)"),
                         name="Income $522-$898 (P65-P95)")
results[[24]] <- runModel(data=filter(df,income_group =="Above P95 (more than $898)"),
                         name="Above $898 (P95)")
# by season
results[[25]] <- runModel(data=filter(df,quarter_text =="Summer"),name="Quarter: Summer",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[26]] <- runModel(data=filter(df,quarter_text =="Fall"),name="Quarter: Fall",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[27]] <- runModel(data=filter(df,quarter_text =="Winter"),name="Quarter: Winter",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
results[[28]] <- runModel(data=filter(df,quarter_text =="Spring"),name="Quarter: Spring",formula="death_count_all_cause ~ pm25Exp_10ug+landTemp+year+commune+offset(log(pop75))")
# by periods of year
results[[29]] <- runModel(data=filter(df,year %in% c("2002","2003","2004","2005")),name="Period: 2002-2005")
results[[30]] <- runModel(data=filter(df,year %in% c("2006","2007","2008","2009","2010")),name="Period: 2006-2010")
results[[31]] <- runModel(data=filter(df,year %in% c("2011","2012","2013","2014","2015")),name="Period: 2011-2015")
results[[32]] <- runModel(data=filter(df,year %in% c("2016","2017","20018","2019")),name="Period: 2016-2019")
# other Endpoints Full Model
results[[33]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardioRespiratory,
                                      MR_all_cause=MR_cardioRespiratory),name="Cardiorespiratory cause")
results[[34]] <- runModel(data=mutate(df,death_count_all_cause=death_count_cardio,
                                      MR_all_cause=MR_cardio),name="Cardiovascular cause")
results[[35]] <- runModel(data=mutate(df,death_count_all_cause=death_count_respiratory,
                                      MR_all_cause=MR_respiratory),name="Respiratory cause")
results[[36]] <- runModel(data=mutate(df,death_count_all_cause=death_count_all_cause_NoCDP,
                                      MR_all_cause=MR_all_cause_NoCDP),name="All cause no Cardiorespiratory")
results[[37]] <- runModel(data=mutate(df,death_count_all_cause=death_count_external,
                                      MR_all_cause=MR_external),name="External cause")



# merge results
res <- do.call("rbind",results)
write.csv(res,"Data/modelResults.csv",row.names = F)
res <- read.csv("Data/modelResults.csv")


# Summary Figure -----

# Calculate RR and C.I.
x <- res %>% 
  mutate(rr=exp(est)*100-100,
         rr_low=exp(est-1.96*se)*100-100, # by the huge number of n, the t-stat converges to 1.96 for 5%
         rr_high=exp(est+1.96*se)*100-100) %>% 
  mutate(N=formatC(N,0, big.mark=",")) %>% 
  mutate(mean_MR=format(round(mean_MR,2),nsmall=2),
         mean_pm25=format(round(mean_pm25,2),nsmall=2)) %>% 
  mutate(ci=paste0(format(round(rr,1),nsmall=1)," (",
                   format(round(rr_low,1),nsmall=1),", ",
                   format(round(rr_high,1),nsmall=1),")")) %>% 
  mutate(rowname=1:nrow(res)) %>% 
  mutate(label=paste0(var," (n=",N,")"))

# Draw figure with Table Incorporated
# Key Idea: Expand limits of graph and put text in each row, then do some formattin
font_size <- 7.5
rows <- res %>% filter(param=="pm25Exp_10ug") %>% nrow()
x %>%
  filter(param=="pm25Exp_10ug") %>%
  # filter(param=="landTemp") %>%
  ggplot(aes(reorder(var,rowname,decreasing=T),rr))+
  geom_point(size=1)+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  geom_vline(xintercept = c(5.5,5.5,9.5,13.5,17.5,19.5,24.5,26.5,32.5,34.5,36.5),
             col="grey",linewidth=0.2)+
  labs(x="",y=expression(paste("Percentage change in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # labs(x="",y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  # add bottom bar
  geom_segment(x = 0.01, y = -2.5,xend = 0.01, yend = 15,
               col="black",linewidth=0.5)+
  # adjust range of axis
  coord_flip(xlim=c(0,rows+2),expand = F)+
  scale_y_continuous(breaks = c(seq(0,15,5)), 
                     expand = c(0,0),
                     limits = c(-16,18.5)) +
  theme_bw(font_size)+
  # add text data
  geom_text(y=-16,x=rows+1,label="Sample",hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-16,aes(label=var),hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-9,x=rows+1,label="n",size=font_size*5/14 * 0.8)+
  geom_text(y=-9,aes(label=N),size=font_size*5/14 * 0.8)+
  geom_text(y=-7,x=rows+1,label="Base rate",size=font_size*5/14 * 0.8)+
  geom_text(y=-7,aes(label=mean_MR),size=font_size*5/14 * 0.8)+
  geom_text(y=-4.5,x=rows+1,label="Mean PM2.5",size=font_size*5/14 * 0.8)+
  geom_text(y=-4.5,aes(label=mean_pm25),size=font_size*5/14 * 0.8)+
  geom_text(y=17,x=rows+1,label="Effect C.I. 95%",size=font_size*5/14 * 0.8)+
  geom_text(y=17,aes(label=ci),size=font_size*5/14 * 0.8)+
  # Modify theme to look good
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        axis.line.y=element_blank(),
        # axis.line.x = element_line(colour = "black"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())

ggsave("Figures//Model/AllModels.png", ggplot2::last_plot(),
# ggsave("Figures//Model/AllModels_Temp.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# EoF