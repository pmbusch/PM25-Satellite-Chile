## Different Models comparisons
## PBH
## March 2023

library(tidyverse)
library(MASS)
library(lme4)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
# 75+ years death data
death_75 <- read.delim("Data/chile_elderly_mortality_count_comuna_level_year_month.csv",sep=",")
death_75 <- death_75 %>% rename(year=Year,
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

# join death and pop
death_75 <- death_75 %>% left_join(pop_75)
death_75 <- death_75 %>% mutate(mortality=Mortality_Count/pop75*1000)

# pm25 pollution data exposure
pm25 <- read.delim("Data/pm25exposure.csv",sep = ";")

# group to region and quarters
pm25_exp <- pm25 %>% 
  mutate(quarter=ceiling(month/3) %>% as.integer()) %>% # quarters by months
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
names(pm25_exp)
names(death_75)
df <- death_75 %>% left_join(pm25_exp)
df <- df %>% na.omit()

df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year=as.factor(year),
         region=as.factor(codigo_region),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

df <- df %>% 
  mutate(quarter_text=case_when(
    quarter==1 ~ "Summer",
    quarter==2 ~ "Fall",
    quarter==3 ~ "Winter",
    T ~ "Spring") %>% factor(levels=c("Fall","Spring","Winter","Summer")))

# write.table(df,"Data/panelData.csv",sep = ";",row.names = F)


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
  # only PM2.5
  out <- data.frame(est=coef[2,1],se=coef[2,2],mean=mean,
                    var=name,N=nobs(mod),bic=BIC(mod))
  return(out)
}

# Run Models ------
results <- list()  #lists to save results

results[[1]] <- runModel(data=df,name="Full Sample")  # full sample
results[[2]] <- runModel(data=df,name="Region-Quarter Interaction",
                         formula="Mortality_Count ~ pm25Exp_10ug+year+quarter*region+offset(log(pop75))")  # full sample
results[[3]] <- runModel(data=filter(df,pm25_case=="Above Median"),name="PM2.5: Above Median") 
results[[4]] <- runModel(data=filter(df,pm25_case=="Below Median"),name="PM2.5: Below Median") 
results[[5]] <- runModel(data=filter(df,pop_case=="Above Median"),name="Pop. 75+: Above Median") 
results[[6]] <- runModel(data=filter(df,pop_case=="Below Median"),name="Pop. 75+: Below Median") 
results[[7]] <- runModel(data=filter(df,bad_region==0),name="No bad regions (satellite)")
results[[8]] <- runModel(data=filter(df,zone=="North"),name="Zone: North")
results[[9]] <- runModel(data=filter(df,zone=="Center"),name="Zone: Center")
results[[10]] <- runModel(data=filter(df,zone=="South"),name="Zone: South")
results[[11]] <- runModel(data=filter(df,quarter_text =="Summer"),name="Quarter: Summer",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[12]] <- runModel(data=filter(df,quarter_text =="Fall"),name="Quarter: Fall",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[13]] <- runModel(data=filter(df,quarter_text =="Winter"),name="Quarter: Winter",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")
results[[14]] <- runModel(data=filter(df,quarter_text =="Spring"),name="Quarter: Spring",formula="Mortality_Count ~ pm25Exp_10ug+year+commune+offset(log(pop75))")

# merge results
res <- do.call("rbind",results)
write.csv(res,"Data/modelResults.csv",row.names = F)

# Summary Figure -----

# Calculate RR and C.I.
x <- res %>% 
  mutate(rr=exp(est)*100-100,
         rr_low=exp(est-1.96*se)*100-100,
         rr_high=exp(est+1.96*se)*100-100) %>% 
  rownames_to_column() %>% mutate(rowname=as.numeric(rowname)) %>% 
  mutate(label=paste0(var," (n=",N,")"))

ggplot(x,aes(reorder(label,rowname,decreasing=T),rr))+
  geom_point(size=3)+
  geom_linerange(aes(ymin=rr_low,ymax=rr_high))+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=1)+
  geom_vline(xintercept = c(4.5,7.5,8.5,10.5,12.5),
             col="grey",linewidth=0.1)+
  coord_flip()+
  labs(x="",y="Percentage increase in Mortality rate by 10 ug/m3 PM2.5")

ggsave("Figures/AllModels.png", ggplot2::last_plot(),units="mm",dpi=300,
       width = 1586/3.7795275591, # pixel to mm under dpi=300
       height = 950/3.7795275591)


# EoF