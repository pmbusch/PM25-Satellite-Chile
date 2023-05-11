## Time series figures of data
## PBH
## May 2023

library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

# Load Panel Data ----
df <- read.delim("Data/panelData.csv",sep=";")

# Figure ------
names(df)
# create 3 plots
p2 <- df %>% 
  # filter(year>2015) %>%
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  ggplot(aes(date,pm25_exposure,group=commune))+
  geom_line(alpha=.2,linewidth=.1,col="#800000")+ 
  coord_cartesian(expand = F)+
  labs(x="",y=expression(paste("PM2.5 Exposure [",mu,"g/",m^3,"]","")))+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank())
# p2
p3 <- p2+aes(y=landTemp)+labs(y="Land Temperature [°C]")+geom_line(alpha=.2,linewidth=.1,col="#000080")
# p3
p1 <- p2+aes(y=MR_all_cause)+labs(y="75+ Mortality Rate \n All-Cause [per 1,000 habs]")+
  geom_line(alpha=.2,linewidth=.1,col="black")
# p1

# Arrange in row format
library(gridExtra)

p <- grid.arrange(arrangeGrob(p1, top="A: 75+ Mortality Rate All-Cause"),
                  arrangeGrob(p2, top="B: PM2.5 Exposure"),
                  arrangeGrob(p3, top="C: Land Temperature"),
                  ncol=1)
# p

ggsave(sprintf("Figures/Timeseries.png"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

df$codigo_comuna %>% unique() %>% length() # 327
