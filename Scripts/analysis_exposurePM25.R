## Analysis on PM25 Population exposure
## PBH
## February 2023

library(tidyverse)
library(flextable)
theme_set(theme_bw(16)+ 
            theme(panel.grid.major = element_blank(),
                  axis.title.y=element_text(angle=0,margin=margin(r=-70))))


# load data 
pm25_exp_commune <- read.delim("Data/pm25exposure_commune.csv",sep=";")


# Region level
pm25_exp_region <- pm25_exp_commune %>% 
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  group_by(REGION,year,month) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(total_pop,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop)


## table ------

table <- pm25_exp_region %>% group_by(REGION) %>% 
  summarise(Mean=mean(pm25_exposure),
            `S.D.`=sd(pm25_exposure),
            Min=min(pm25_exposure),
            Median=median(pm25_exposure),
            Max=max(pm25_exposure)) %>% 
  rename(Region=REGION)

total_table <- pm25_exp_region %>% 
  summarise(Mean=mean(pm25_exposure),
            `S.D.`=sd(pm25_exposure),
            Min=min(pm25_exposure),
            Median=median(pm25_exposure),
            Max=max(pm25_exposure))
total_table$Region <- "Total"
table <- rbind(table,total_table)

flextable(table) %>% 
  autofit() %>% 
  colformat_double(j=2:6,digits=1) %>% 
  bold(i=17) %>% hline(i=16)



# figure density ---

pm25_exp_dens <- pm25_exp %>% 
  mutate(pop_pm25=poblacion*pm25_Exposure) %>% 
  mutate(season=case_when(
    month>3 & month<7 ~ "Fall",
    month>3 & month < 10 ~ "Winter",
    month>3 & month <13 ~ "Spring",
    T ~ "Summer") %>% factor(levels=c("Fall","Spring","Winter","Summer"))) %>% 
  group_by(codigo_comuna,season) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(poblacion,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop)
pm25_exp_dens$pop_pm25 <- NULL; pm25_exp_dens$total_pop <- NULL

pop_commune <- pm25_exp %>% group_by(codigo_comuna,geocodigo) %>% 
  summarise(pop=mean(poblacion,na.rm=T)) %>% ungroup() %>% 
  group_by(codigo_comuna) %>% 
  summarise(pop=sum(pop,na.rm=T))
pop_commune$pop %>% sum()
pm25_exp_dens <- pm25_exp_dens %>% left_join(pop_commune)  

# % district zones
library(chilemapas)
zonas_2017 <- censo_2017_zonas %>% group_by(geocodigo) %>% 
  summarise(poblacion=sum(poblacion))
mapa_zona <-chilemapas::mapa_zonas %>% left_join(zonas_2017) 
sum(mapa_zona$poblacion,na.rm=T)/sum(censo_2017_comunas$poblacion)


pm25_exp_dens %>% 
  ggplot(aes(pm25_exposure))+
  geom_histogram(aes( y = ..density.., weight = pop), fill="brown",bins=50)+
  facet_wrap(~season)+
  geom_vline(xintercept = 12, linetype="dashed")+
  annotate("text", x = 11, y = 0.15, angle = 90,label = "WHO Guideline")+
  labs(x="PM2.5 Exposure [ug/m3]",y="")+
  coord_cartesian(expand = F)+
  theme(legend.position = "none")


# figure time series ------
pm25_exp_region %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  ggplot(aes(date,pm25_exposure,
                           col=codigo_region,group=codigo_region))+
  geom_line()+
  labs(x="",y="PM2.5 Exposure",col="Region")+
  theme_bw()

pm25_exp_commune %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  ggplot(aes(date,pm25_exposure,group=COMUNA))+
  geom_line(alpha=.2,linewidth=.5)+
  coord_cartesian(expand = F)+
  labs(x="",y=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]","")))+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y-%b")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank())

ggsave("Figures/PM25Exp_timeseries.png",last_plot(),
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 7.4)

pm25_exp_commune$codigo_comuna %>% unique() %>% length() # 345

# EoF