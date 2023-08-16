## Time series figures of data
## PBH
## May 2023

library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load Panel Data ----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

# Figure ------
names(df)
# create 3 plots
library(RColorBrewer)
brewer.pal(n=3,"Reds") # color same as map
p2 <- df %>% 
  # filter(year>2015) %>%
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  ggplot(aes(date,pm25_exposure,group=commune))+
  geom_line(alpha=.2,linewidth=.1,col="#DE2D26")+
  # geom_boxplot(aes(group=date),alpha=.3,col="#DE2D26")+
  coord_cartesian(expand = F)+
  labs(x="",y=lab_pm25)+
  scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank())
# p2
brewer.pal(n=3,"Blues") 
p3 <- p2+aes(y=landTemp)+labs(y=lab_temp)+
  geom_line(alpha=.2,linewidth=.1,col="#3182BD")
# p3
brewer.pal(n=3,"Oranges") 
p1 <- p2+aes(y=MR_all_cause)+labs(y=lab_mr)+
  geom_line(alpha=.2,linewidth=.1,col="#E6550D")
# p1

# Arrange in row format
library(gridExtra)

# Add titles
p1 <- p1 +   ggtitle("A: 75+ Mortality Rate All-Cause") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
p2 <- p2 +   ggtitle(expression("B: PM"[2.5]~"Exposure")) +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
p3 <- p3 +   ggtitle("C: Land Temperature") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))


p <- grid.arrange(arrangeGrob(p1),
                  arrangeGrob(p2),
                  arrangeGrob(p3),
                  ncol=1)
p
# 
ggsave(sprintf("Figures/Timeseries.png"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

df$codigo_comuna %>% unique() %>% length() # 327


# Just PM2.5 and MR
p2 <- p2 +   ggtitle(expression("PM"[2.5]~"Exposure"))
p1 <- p1 +   ggtitle("75+ Mortality Rate All-Cause")

p <- grid.arrange(arrangeGrob(p2),
                  arrangeGrob(p1),
                  ncol=1)
p
# 
ggsave(sprintf("Figures/Timeseries_2.png"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

p3 <- p3 +   ggtitle("Land Temperature") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(sprintf("Figures/Timeseries_3.png"),p3,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 8.7)


# EoF