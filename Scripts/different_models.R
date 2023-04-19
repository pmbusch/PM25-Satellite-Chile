## Different Models comparisons
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

# Change some paramters
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
  region %in% c("5","13","6","7","16") ~ "Center",
  T ~ "South"))
df %>% group_by(zone,region) %>% tally()

# pm.25 by zone
df %>%
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  group_by(zone) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(total_pop,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) 

# order appropiately to ensure lags are well calculated
df <- df %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% 
  arrange(count_month) %>% arrange(codigo_comuna)

## Function to run Negative Binomial Model ----- 
# for offset, see https://stats.stackexchange.com/questions/66791/where-does-the-offset-go-in-poisson-negative-binomial-regression
# seE: https://github.com/echolab-stanford/NCC2018/blob/master/scripts/functions.R
runModel <- function(data_,
                     name,
                     formula="Mortality_Count ~ pm25Exp_10ug+year+quarter+commune+offset(log(pop75))"){
  
  mod <- glm.nb(as.formula(formula), 
                     data = data_,
                     na.action=na.omit)
  
  coef <- summary(mod)$coefficients
  mean <- weighted.mean(data_$mortality,data_$pop75,na.rm=T)
  
  # clustered standard errors by commune
  cluster_se <- vcovCL(mod, cluster = data_$commune)
  cluster_se <- sqrt(diag(cluster_se))
  
  # only PM2.5
  out <- data.frame(est=coef[2,1],se=cluster_se[2],mean=mean,
                    var=name,N=nobs(mod),bic=BIC(mod))
  return(out)
}

# Run Models ------
results <- list()  #lists to save results

results[[1]] <- runModel(data=df,name="Full Sample")  # full sample
results[[2]] <- runModel(data=df,name="Region-Quarter Interaction",
                         formula="Mortality_Count ~ pm25Exp_10ug+year+quarter*region+offset(log(pop75))")  # full sample
results[[3]] <- runModel(data=filter(df_sex,sex=="Hombre"),name="Sex: Male") 
results[[4]] <- runModel(data=filter(df_sex,sex=="Mujer"),name="Sex: Female") 
results[[5]] <- runModel(data=filter(df,pm25_case=="Above Median"),name="PM2.5: Above Median") 
results[[6]] <- runModel(data=filter(df,pm25_case=="Below Median"),name="PM2.5: Below Median") 
results[[7]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 75+: Above Median") 
results[[8]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 75+: Below Median") 
results[[9]] <- runModel(data=filter(df,bad_region==0),name="No bad regions (satellite)")
results[[10]] <- runModel(data=filter(df,zone=="North"),name="Zone: North")
results[[11]] <- runModel(data=filter(df,zone=="Center"),name="Zone: Center")
results[[12]] <- runModel(data=filter(df,zone=="South"),name="Zone: South")
results[[13]] <- runModel(data=filter(df,quarter_text =="Summer"),name="Quarter: Summer",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[14]] <- runModel(data=filter(df,quarter_text =="Fall"),name="Quarter: Fall",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[15]] <- runModel(data=filter(df,quarter_text =="Winter"),name="Quarter: Winter",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[16]] <- runModel(data=filter(df,quarter_text =="Spring"),name="Quarter: Spring",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
# lags
results[[17]] <- runModel(data=mutate(group_by(df,codigo_comuna), # need to group by by commune to ensure lag does not cross among communes
                                      pm25Exp_10ug=lag(pm25Exp_10ug,12,order_by=count_month)),name="Lag: -1 Year")
results[[18]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lag(pm25Exp_10ug,6,order_by=count_month)),name="Lag: -6 Month")
results[[19]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lag(pm25Exp_10ug,3,order_by=count_month)),name="Lag: -3 Month")
results[[20]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lag(pm25Exp_10ug,1,order_by=count_month)),name="Lag: -1 Month")
results[[21]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lead(pm25Exp_10ug,1,order_by=count_month)),name="Lead: +1 Month")
results[[22]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lead(pm25Exp_10ug,6,order_by=count_month)),name="Lead: +6 Month")
results[[23]] <- runModel(data=mutate(group_by(df,codigo_comuna),
                                      pm25Exp_10ug=lead(pm25Exp_10ug,12,order_by=count_month)),name="Lead: +1 Year")
# by periods of year
results[[24]] <- runModel(data=filter(df,year %in% c("2002","2003","2004","2005")),name="Period: 2002-2005")
results[[25]] <- runModel(data=filter(df,year %in% c("2006","2007","2008","2009","2010")),name="Period: 2006-2010")
results[[26]] <- runModel(data=filter(df,year %in% c("2011","2012","2013","2014","2015")),name="Period: 2011-2015")
results[[27]] <- runModel(data=filter(df,year %in% c("2016","2017","20018","2019")),name="Period: 2016-2019")
# other Endpoints Full Model
results[[28]] <- runModel(data=df,name="Cardiopulmonary cause",
                          formula="Mortality_Count_CDP ~ pm25Exp_10ug+year+quarter+commune+offset(log(pop75))")


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
  # mutate(mean=formatC(mean,3)) %>% 
  mutate(mean=format(round(mean,2),nsmall=2)) %>% 
  mutate(ci=paste0(format(round(rr,1),nsmall=1)," (",
                   format(round(rr_low,1),nsmall=1),", ",
                   format(round(rr_high,1),nsmall=1),")")) %>% 
  mutate(rowname=1:nrow(res)) %>% 
  mutate(label=paste0(var," (n=",N,")"))

# Draw figure with Table Incorporated
# Key Idea: Expand limits of graph and put text in each row, then do some formattin
font_size <- 7.5
rows <- nrow(res)
ggplot(x,aes(reorder(var,rowname,decreasing=T),rr))+
  geom_point(size=1)+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  # add separating lines
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  geom_vline(xintercept = c(1.5,5.5,12.5,16.5,19.5,20.5,22.5,24.5,26.5),
             col="grey",linewidth=0.2)+
  labs(x="",y=expression(paste("Percentage increase in Mortality rate by 10 ",mu,"g/",m^3," PM2.5","")))+
  # add bottom bar
  geom_segment(x = 0.01, y = -2.5,xend = 0.01, yend = 15,
               col="black",linewidth=0.5)+
  # adjust range of axis
  coord_flip(xlim=c(0,rows+2),expand = F)+
  scale_y_continuous(breaks = c(seq(0,15,5)), 
                     expand = c(0,0),
                     limits = c(-13,18.5)) +
  theme_bw(font_size)+
  # add text data
  geom_text(y=-13,x=rows+1,label="Sample",hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-13,aes(label=var),hjust = 0,size=font_size*5/14 * 0.8)+
  geom_text(y=-7,x=rows+1,label="n",size=font_size*5/14 * 0.8)+
  geom_text(y=-7,aes(label=N),size=font_size*5/14 * 0.8)+
  geom_text(y=-5,x=rows+1,label="Base rate",size=font_size*5/14 * 0.8)+
  geom_text(y=-5,aes(label=mean),size=font_size*5/14 * 0.8)+
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
       units="cm",dpi=500,
       # width = 1068/3.7795275591, # pixel to mm under dpi=300
       # height = 664/3.7795275591)
       width=8.7*2,height=8.7)


# EoF