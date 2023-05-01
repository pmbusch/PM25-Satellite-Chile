## Analysis on PM25 Population exposure
## PBH
## February 2023

library(tidyverse)
library(flextable)
theme_set(theme_bw(16)+ 
            theme(panel.grid.major = element_blank(),
                  axis.title.y=element_text(angle=0,margin=margin(r=-70))))
fig_name <- "Figures/PM25_Exposure/%s"


# load data 
pm25_exp_commune <- read.delim("Data/pm25exposure_commune.csv",sep=";")
landTemp <-  read.delim("Data/landTemp_commune.csv",sep = ";")
names(landTemp) <- names(landTemp) %>% str_replace_all("total_pop","total_pop_T")

pm25_exp_commune <- pm25_exp_commune %>% left_join(landTemp)

 # Region level
region_levels <- c("15","1","2","3","4","5","13","6","7","8","16","9","14","10","11","12")
pm25_exp_region <- pm25_exp_commune %>% 
  # mutate(pm25_exposure=landTemp,
  #        total_pop=total_pop_T) %>% # to see T results
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  group_by(REGION,year,month) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            total_pop=sum(total_pop,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/total_pop) %>% 
  rename(Region=REGION) %>% 
  mutate(Region=factor(Region,levels=region_levels)) %>% 
  arrange(Region)


## table ------
table <- pm25_exp_region %>% group_by(Region) %>% 
  summarise(Mean=mean(pm25_exposure,na.rm=T),
            `S.D.`=sd(pm25_exposure,na.rm=T),
            Min=min(pm25_exposure,na.rm=T),
            Median=median(pm25_exposure,na.rm=T),
            Max=max(pm25_exposure,na.rm=T)) 
  

total_table <- pm25_exp_region %>% 
  summarise(Mean=mean(pm25_exposure,na.rm=T),
            `S.D.`=sd(pm25_exposure,na.rm=T),
            Min=min(pm25_exposure,na.rm=T),
            Median=median(pm25_exposure,na.rm=T),
            Max=max(pm25_exposure,na.rm=T))
total_table$Region <- "Total"
table <- rbind(table,total_table)

flextable(table) %>% 
  autofit() %>% 
  colformat_double(j=2:6,digits=1) %>% 
  bold(i=17) %>% hline(i=16)


# figure density ---

pm25_exp_dens <- pm25_exp_commune %>% 
  mutate(pop_pm25=total_pop*pm25_exposure) %>% 
  mutate(season=case_when(
    month>3 & month<7 ~ "Fall",
    month>3 & month < 10 ~ "Winter",
    month>3 & month <13 ~ "Spring",
    T ~ "Summer") %>% factor(levels=c("Fall","Spring","Winter","Summer"))) %>% 
  group_by(codigo_comuna,season) %>% 
  summarise(pop_pm25=sum(pop_pm25,na.rm=T),
            sum_pop=sum(total_pop,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pm25_exposure=pop_pm25/sum_pop)
pm25_exp_dens$pop_pm25 <- NULL;

# pop_commune <- pm25_exp_commune %>% group_by(codigo_comuna,geocodigo) %>% 
#   summarise(pop=mean(poblacion,na.rm=T)) %>% ungroup() %>% 
#   group_by(codigo_comuna) %>% 
#   summarise(pop=sum(pop,na.rm=T))
# pop_commune$pop %>% sum()
# pm25_exp_dens <- pm25_exp_dens %>% left_join(pop_commune)  
# 
# # % district zones
# library(chilemapas)
# zonas_2017 <- censo_2017_zonas %>% group_by(geocodigo) %>% 
#   summarise(poblacion=sum(poblacion))
# mapa_zona <-chilemapas::mapa_zonas %>% left_join(zonas_2017) 
# sum(mapa_zona$poblacion,na.rm=T)/sum(censo_2017_comunas$poblacion)


pm25_exp_dens %>% 
  ggplot(aes(pm25_exposure))+
  geom_histogram(aes( y = ..density.., weight = sum_pop), fill="brown",bins=50)+
  facet_wrap(~season)+
  geom_vline(xintercept = 10, linetype="dashed")+
  annotate("text", x = 9, y = 0.15, angle = 90,label = "WHO Guideline")+
  labs(x="Commune PM2.5 Exposure [ug/m3]",y="")+
  coord_cartesian(expand = F)+
  theme(legend.position = "none")

ggsave(sprintf(fig_name,"histogram.png"))


# figure time series ------
pm25_exp_region %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(zone=case_when(
    Region %in% c("15","1","2","3","4") ~ "North",
    Region %in% c("5","13","6","7") ~ "Center",
    Region %in% c("8","16","9","14") ~ "Center-South",
    Region %in% c("10","11","12") ~ "South") %>% 
      factor(levels=c("North","Center","Center-South","South"))) %>% 
  ggplot(aes(date,pm25_exposure,
                           col=Region,group=Region))+
  geom_line(linewidth=0.5)+
  facet_wrap(~zone)+
  labs(x="",y="PM2.5 Exposure",col="Region")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=7),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"))
  

ggsave(sprintf(fig_name,"PM25Exp_timeseries_reg.png"),last_plot(),
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 7.4)

pm25_exp_commune %>% 
  # filter(year>2010) %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  ggplot(aes(date,pm25_exposure,group=COMUNA))+
  geom_line(alpha=.2,linewidth=.5)+
  # geom_line(alpha=.2,linewidth=.5,col="red",aes(y=landTemp))+ # Temperature
  coord_cartesian(expand = F)+
  labs(x="",y=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]","")))+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank())

ggsave(sprintf(fig_name,"PM25Exp_timeseries.png"),last_plot(),
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 7.4)

pm25_exp_commune$codigo_comuna %>% unique() %>% length() # 345




# Slope Graph  -------------

# Filter max and min values

pm_exp_anual <- pm25_exp_region %>% 
  group_by(Region,year) %>% 
  summarise(pm25_exposure=mean(pm25_exposure,na.rm=T)) %>% ungroup() %>% 
  mutate(zone=case_when(
    Region %in% c("15","1","2","3","4") ~ "North",
    Region %in% c("5","13","6","7") ~ "Center",
    Region %in% c("8","16","9","14") ~ "Center-South",
    Region %in% c("10","11","12") ~ "South") %>% 
      factor(levels=c("North","Center","Center-South","South"))) %>% 
  mutate(reg=paste0("R",Region),
         lab=paste0("",format(pm25_exposure,digits=2)))

library(ggrepel)
slope_graph <- pm_exp_anual %>% ggplot(aes(x = year, y = pm25_exposure)) +
  geom_line(aes(group = Region, color = zone), linewidth=0.5) +
  geom_point(color = "white", size = 1) +
  geom_point(color = "#0072B2", size = 0.5) +
  geom_label_repel(data = filter(pm_exp_anual,year==1998), 
                   aes(label = reg, color=zone) , 
                   hjust = "left", 
                   size = 8*5/14 * 0.8, 
                   nudge_x = -1.5,
                   direction = "y")+
  geom_label_repel(data = filter(pm_exp_anual,year==2019), 
                   aes(label = reg, color=zone),
                   hjust = "right", 
                   size = 8*5/14 * 0.8, 
                   nudge_x = 1.5, 
                   direction = "y")+
  geom_label(data = filter(pm_exp_anual,year %in% c(2000,2005,2010,2015,2019)), 
             aes(label = lab),
             size = 8*5/14 * 0.8,
             label.padding = unit(0.05, "lines"),
             label.size = 0.0)+
  geom_hline(yintercept = 10, col="red", linetype = "dashed", linewidth=0.5)+
  geom_text(x=2008,y=10, vjust=-1, label="WHO Guidelines", col="red",size=6*5/14 * 0.8)+
  scale_color_viridis_d()+
  scale_y_continuous(expression(paste("Annual PM2.5 Exposure [",mu,"g/",m^3,"]","")))+
  labs(x="",col="")+
  expand_limits(x=c(1998-.5,2019+.5))+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# slope_graph

ggsave(sprintf(fig_name,"PM25Exp_yearSlope.png"),slope_graph,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 7.4)


# Urban vs Rural ------
p <- pm25_exp_commune %>% 
  mutate(zone=case_when(
    REGION %in% c(15,1,2,3,4) ~ "North",
    REGION %in% c(5,13,6,7) ~ "Center",
    REGION %in% c(8,16,9,14) ~ "Center-South",
    REGION %in% c(10,11,12) ~ "South") %>% 
      factor(levels=c("North","Center","Center-South","South"))) %>% 
  mutate(period=case_when(
    year<2005 ~ "1998-2004",
    year<2010 ~ "2005-2009",
    year<2015 ~ "2010-2014",
    year<2020 ~ "2015-2019")) %>% 
  # sample_n(100) %>%  # reduce compuilation time to try format stuff
  ggplot(aes(pm25_exposure_urban,pm25_exposure_rural,col=zone))+
  geom_point(alpha=0.5,size=0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x=expression(paste("Urban PM2.5 Exposure [",mu,"g/",m^3,"]","")),
       y=expression(paste("Rural PM2.5 Exposure [",mu,"g/",m^3,"]","")),
       col="")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position=c(0.75,0.2),
        legend.text = element_text(size=7),
        legend.key.height = unit(0.1, "cm"),
        legend.spacing.x = unit(0.001,"cm"))
        # legend.box.background = element_rect(colour = "black"))
  # guides(color = guide_legend(label.vjust = 0))
p

ggsave(sprintf(fig_name,
               "PM25Exp_urbanRural.png"),
               # "PM25Exp_urbanRural_period.png"), # for year as color
       p,
       units="cm",dpi=500,
       width = 7.4, # half-width width
       height = 7.4)


# EoF