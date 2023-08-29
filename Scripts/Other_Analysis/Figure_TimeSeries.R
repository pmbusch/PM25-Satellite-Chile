## Time series figures of data
## PBH
## May 2023

library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

fig_name <- "Figures/Time Series/%s.png"

# Load Panel Data ----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

# Some stats
df %>% dplyr::select(pm25_exposure,landTemp) %>% skimr::skim()
df$pm25_exposure %>% quantile(c(0.05,0.95))
df$landTemp %>% quantile(c(0.05,0.95))


# Figure v1: Spagetti ------
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
ggsave(sprintf(fig_name,"Timeseries"),p,
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
ggsave(sprintf(fig_name,"Timeseries2"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height = 12.2)

p3 <- p3 +   ggtitle("Land temperature") +
  theme(plot.title = element_text(size = 12, hjust = 0.5))
ggsave(sprintf(fig_name,"Timeseries3"),p3,
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
  mutate(pm_pop=pm25_exposure*pop75,
         mr_pop=MR_all_cause*pop75,
         temp_pop=landTemp*pop75) %>% 
  group_by(date) %>% 
  reframe(pm_pop=sum(pm_pop),pop75=sum(pop75),
          mr_pop=sum(mr_pop),temp_pop=sum(temp_pop)) %>% 
  mutate(pm25_exposure=pm_pop/pop75, 
         MR_all_cause=mr_pop/pop75,
         landTemp=temp_pop/pop75,
         nombre="Chile mean",
         commune=999,
         pm_pop=NULL,pop75=NULL,mr_pop=NULL,temp_pop=NULL) %>% ungroup()


# highlight communes
selected_coms <- c(
  2101, # Antofagasta
  5101, # Valparaiso
  13101, # Santiago
  9101, # Temuco
  8101, # Concepcion
  11101, # Coyhaique
  12101 # Punta Areas
)


# colors_cat <- pals::glasbey(7)
# colors_cat <- viridis::cividis(7)
colors_cat <- viridis::turbo(7)
# colors_cat <- viridis::magma(7)
category_colors <- c("Other communes" = "#B0B0B0",
                     "Chile mean" = "#1A1A1A",
                     "Antofagasta" = colors_cat[1], 
                     "Valparaiso" = colors_cat[2],
                     "Santiago" = colors_cat[3],
                     "Concepcion" = colors_cat[4],
                     "Temuco" = colors_cat[5],
                     "Coyhaique" = colors_cat[6],
                     "Punta Arenas"=colors_cat[7])




legend_order <- c("Chile mean",
                  "Antofagasta", 
                  "Valparaiso",
                  "Santiago",
                  "Concepcion",
                  "Temuco",
                  "Coyhaique",
                  "Punta Arenas",
                  "Other communes")

p <- df_fig %>% 
  mutate(nombre="Other communes") %>% 
  ggplot(aes(date,pm25_exposure,group=commune,col=nombre))+
  geom_line(alpha=.25,linewidth=0.1)+
  geom_line(data=filter(df_fig, commune %in% selected_coms),linewidth=0.2)+
  geom_line(data=df_mean,linewidth=0.4)+
  coord_cartesian(expand = F)+
  labs(x="",y=lab_pm25,col="  Communes (N to S)")+
  scale_x_date(date_labels = "%Y",
               breaks = seq(ymd("2002-01-01"), ymd("2020-12-31"), by = "2 year"),  # Set desired date breaks
               limits = c(ymd("2002-01-01"), ymd("2024-06-30")))+
  # scale_x_date(date_breaks = "6 month",date_labels = "%Y-%b")+
  scale_color_manual(values = category_colors,
                     breaks = legend_order,
                     labels = legend_order) + # Manually set colors
  theme_bw(12)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.92,0.5),
        legend.title = element_text(size=7, vjust=-24,face = "bold"), # title pos
        legend.text = element_text(size=7))+
  guides(col=guide_legend(
    keywidth=0.1,
    # keyheight=0.1,
    keyheight = c(1.2,rep(0.1,7),0.6),
    default.unit="cm",
    override.aes = list(alpha=1, linewidth=c(0.8,rep(0.35,7),0.25))) # linewidth for legend
  )
p
ggsave(sprintf(fig_name,"highlight_pm"),p,
       units="cm",dpi=500,
       width = 14.8, # full width
       height =6.1)

## Same but for MR and temp-----

p_mr <- p+aes(y=MR_all_cause)+labs(y=lab_mr)
p_mr
ggsave(sprintf(fig_name,"highlight_mr"),p_mr,
       units="cm",dpi=500,
       width = 14.8, # full width
       height =6.1)

# p_mr2 <- p_mr+ylim(0,20)
# ggsave(sprintf(fig_name,"highlight_mr2"),p_mr2,
#        units="cm",dpi=500,
#        width = 14.8, # full width
#        height =6.1)

p_temp <- p+aes(y=landTemp)+labs(y=lab_temp)
p_temp
ggsave(sprintf(fig_name,"highlight_temp"),p_temp,
       units="cm",dpi=500,
       width = 14.8, # full width
       height =6.1)




# EoF