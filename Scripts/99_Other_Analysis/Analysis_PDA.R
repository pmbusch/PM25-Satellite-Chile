# Analysis on cities with PDA
# 23 cities from https://www.sciencedirect.com/science/article/pii/S2212095520306787
# PBH July 2023


library(tidyverse)
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))
source("Scripts/Functions.R",encoding="UTF-8")
df <- read.delim("Data/panelData.csv",sep=";")


# 23 Cities ------
# communes where the 33 monitors site used where located
coms <- c("5105", "5109", "5101", "13114", "13124", "13101", "13601", "6101", 
          "6301", "7301", "7101", "7401", "16101", "8110", "8101", "8102", "8301", 
          "9101", "9112", "14101", "14201", "10301", "10101", "11101")

df <- df %>% filter(commune %in% coms)

df %>% pull(NOM_COMUNA) %>% unique()

# Analysis time series -----

# average by year
df %>% 
  filter(year %in% c(2002,2005,2010,2015,2019)) %>% 
  group_by(commune,year) %>% 
  summarise(value=mean(pm25_exposure)) %>% 
  pivot_wider(names_from = year, values_from = value)


# all time series
df %>% 
  # filter(year>2015) %>%
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  ggplot(aes(date,pm25_exposure,group=commune))+
  geom_line(alpha=.2,linewidth=.1,col="#800000")+ 
  coord_cartesian(expand = F)+
  labs(x="",y=lab_pm25)+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank())

ggsave(sprintf("Figures/PDA/Timeseries.png"),
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

# average for 2002-2004 and 2017-2019
avg_2002 <- df %>% 
  # filter(year %in% c(2002:2004)) %>%
  filter(year %in% c(2010)) %>%
  group_by(NOM_COMUNA,commune) %>% 
  summarise(pm_2002=mean(pm25_exposure))

avg_2019 <- df %>% 
  # filter(year %in% c(2017:2019)) %>%
  filter(year %in% c(2019)) %>%
  group_by(NOM_COMUNA,commune) %>% 
  summarise(pm_2019=mean(pm25_exposure))


# slope graph
df %>% 
  # filter(year %in% c(2002,2005,2010,2015,2019)) %>% 
  group_by(NOM_COMUNA,commune,year) %>% 
  summarise(value=mean(pm25_exposure)) %>% 
  left_join(avg_2002) %>% left_join(avg_2019) %>% 
  mutate(AQ_improve=if_else(pm_2002-pm_2019>0,"Yes","No")) %>% # did Air quality improve
  ggplot(aes(x = year, y = value,group=commune)) +
  geom_line(linewidth=0.5) +
  geom_point(size=0.5) +
  geom_text(data = . %>% filter(year == 2019),size=8*5/14 * 0.8,
            aes(label = NOM_COMUNA,col=AQ_improve), x=2020.5) +  
  labs(x="",y=lab_pm25,col="Did Air Quality improve \n from 2010 to 2019")+
  xlim(2002,2021)+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.9,0.9))


ggsave(sprintf("Figures/PDA/slopeGraph.png"),
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)


