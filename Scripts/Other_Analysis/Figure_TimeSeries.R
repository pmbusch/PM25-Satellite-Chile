## Time series figures of data
## PBH
## May 2023

library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load Panel Data ----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

# Some stats
df %>% dplyr::select(pm25_exposure,landTemp) %>% skimr::skim()
df$pm25_exposure %>% quantile(c(0.05,0.95))
df$landTemp %>% quantile(c(0.05,0.95))


# Figure v1: Spaghietti ------
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
  theme_bw(12)+
  theme(panel.grid.major = element_blank())
p2
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
p1 <- p1 +   ggtitle("A: 75+ All-Cause Mortality Rate") +
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
p2 <- p2 +   ggtitle(expression("PM"[2.5]~" exposure"))
p1 <- p1 +   ggtitle("75+ all-cause monthly mortality rate")

p <- grid.arrange(arrangeGrob(p2),
                  arrangeGrob(p1),
                  ncol=1)
p
# 
ggsave(sprintf("Figures/Timeseries_2.png"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

p3 <- p3 +   ggtitle("Land temperature") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(sprintf("Figures/Timeseries_3.png"),p3,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 8.7)


# Figure v2: Highlighting ------------

df_fig <- df %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  mutate(nombre=str_to_title(NOM_COMUNA) %>% 
           str_replace("í","i") %>% str_replace("ó","o"))

# get national mean
df_mean <- df_fig %>% 
  mutate(pm_pop=pm25_exposure*pop75) %>% 
  group_by(date) %>% 
  reframe(pm_pop=sum(pm_pop),pop75=sum(pop75)) %>% 
  mutate(pm25_exposure=pm_pop/pop75, nombre="Chile mean",
         commune=999,
         pm_pop=NULL,pop75=NULL) %>% ungroup()

# highlight communes
selected_coms <- c(
  2101, # Antofagasta
  5101, # Valparaiso
  13101, # Santiago
  9101, # Temuco
  8101, # Concepcion
  12101 # Punta Areas
)

category_colors <- c("Other communes" = "#CCCCCC",
                     "Chile mean" = "#DE2D26",
                     "Antofagasta" = "#e377c2", 
                     "Santiago" = "#2ca02c",
                     "Valparaiso" = "#9467bd",
                     "Temuco" = "#ff7f0e",
                     "Concepcion" = "#8c564b",
                     "Punta Arenas"="#1f77b4")
legend_order <- c("Other communes",
                  "Antofagasta", 
                  "Santiago",
                  "Valparaiso",
                  "Temuco",
                  "Concepcion",
                  "Punta Arena",
                  "Chile mean")


brewer.pal(n=3,"Reds") # color same as map
df_fig %>% 
  mutate(nombre="Other communes") %>% 
  ggplot(aes(date,pm25_exposure,group=commune))+
  geom_line(alpha=.25,linewidth=.05,
            aes(col=nombre)
            )+
  geom_line(data=filter(df_fig, commune %in% selected_coms),
            aes(col=nombre),linewidth=0.15)+
  geom_line(data=df_mean,linewidth=0.4,aes(col=nombre))+
  coord_cartesian(expand = F)+
  labs(x="",y=lab_pm25,col="")+
  scale_x_date(date_labels = "%Y",
               breaks = seq(ymd("2002-01-01"), ymd("2020-12-31"), by = "2 year"),  # Set desired date breaks
               limits = c(ymd("2002-01-01"), ymd("2023-12-31")))+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  scale_color_manual(values = category_colors,
                     breaks = legend_order,
                     labels = legend_order) + # Manually set colors
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.9,0.3),
        legend.text = element_text(size=6))+
  guides(col=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="cm")
  )

ggsave(sprintf("Figures/Timeseries_highlight_2.png"),
       last_plot(),
       units="cm",dpi=500,
       width = 14.8, # full width
       height =6.1)

## Same but for MR -----

df_fig <- df %>% 
  mutate(pm25_exposure=MR_all_cause) %>% 
  mutate(date=as.Date(paste(year,month,"01",sep="-"),"%Y-%m-%d")) %>% 
  mutate(count_month=as.numeric(year)*12+as.numeric(month)) %>% # order by month
  arrange(count_month) %>% arrange(codigo_comuna) %>% 
  mutate(nombre=str_to_title(NOM_COMUNA) %>% 
           str_replace("í","i") %>% str_replace("ó","o"))

# get national mean
df_mean <- df_fig %>% 
  mutate(pm_pop=pm25_exposure*pop75) %>% 
  group_by(date) %>% 
  reframe(pm_pop=sum(pm_pop),pop75=sum(pop75)) %>% 
  mutate(pm25_exposure=pm_pop/pop75, nombre="Chile mean",
         commune=999,
         pm_pop=NULL,pop75=NULL) %>% ungroup()

# highlight communes
selected_coms <- c(
  2101, # Antofagasta
  5101, # Valparaiso
  13101, # Santiago
  9101, # Temuco
  8101, # Concepcion
  12101 # Punta Areas
)

category_colors <- c("Other communes" = "#CCCCCC",
                     "Chile mean" = "#DE2D26",
                     "Antofagasta" = "#e377c2", 
                     "Santiago" = "#2ca02c",
                     "Valparaiso" = "#9467bd",
                     "Temuco" = "#ff7f0e",
                     "Concepcion" = "#8c564b",
                     "Punta Arenas"="#1f77b4")
legend_order <- c("Other communes",
                  "Antofagasta", 
                  "Santiago",
                  "Valparaiso",
                  "Temuco",
                  "Concepcion",
                  "Punta Arena",
                  "Chile mean")


df_fig %>% 
  mutate(nombre="Other communes") %>% 
  ggplot(aes(date,pm25_exposure,group=commune))+
  geom_line(alpha=.25,linewidth=.05,
            aes(col=nombre)
  )+
  geom_line(data=filter(df_fig, commune %in% selected_coms),
            aes(col=nombre),linewidth=0.15)+
  geom_line(data=df_mean,linewidth=0.4,aes(col=nombre))+
  coord_cartesian(expand = F,ylim=c(0,20))+
  labs(x="",y=lab_mr,col="")+
  scale_x_date(date_labels = "%Y",
               breaks = seq(ymd("2002-01-01"), ymd("2020-12-31"), by = "2 year"),  # Set desired date breaks
               limits = c(ymd("2002-01-01"), ymd("2023-12-31")))+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  scale_color_manual(values = category_colors,
                     breaks = legend_order,
                     labels = legend_order) + # Manually set colors
  theme_bw(12)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.9,0.3),
        legend.text = element_text(size=6))+
  guides(col=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="cm")
  )

ggsave(sprintf("Figures/Timeseries_highlight_mr.png"),
       last_plot(),
       units="cm",dpi=500,
       width = 14.8, # full width
       height =6.1)



# EoF