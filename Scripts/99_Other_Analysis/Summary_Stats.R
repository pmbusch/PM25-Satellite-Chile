## Summary Statistic for the Panel Data used
# PBH September 2023

library(tidyverse)
theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load required data -----
df <- read.delim("Data/Panel Data/panelData.csv",sep=";")

# Change some parameters
df <- df %>% 
  mutate(quarter=ceiling(as.numeric(month)/3) %>% factor()) %>% 
  mutate(year_quarter=paste0(year,"-",quarter)) %>% 
  mutate(year=as.factor(year),
         year_quarter=as.factor(year_quarter),
         region=as.factor(REGION),
         commune=as.factor(codigo_comuna),
         month=as.factor(month),
         quarter=as.factor(quarter))

# Summary Table ----

(n <- nrow(df))
(n_com <- df$commune %>% unique() %>% length())

library(DescTools)

# raw averages
table <- df %>% dplyr::select(MR_all_cause,pop75_share,pop75,
                     pm25_exposure,landTemp)
cols_name <- colnames(table)

# skimr::skim_without_charts(table)
table %>% 
  rownames_to_column(var = "var") %>% 
  pivot_longer(c(-1,-pop75), names_to = "key", values_to = "value") %>% mutate(var=NULL) %>% 
  group_by(key) %>% 
  # summarise_all(list(mean = mean, sd = sd, min = min, median = median, max = max))
  reframe(mean=weighted.mean(value,pop75),
          median=weighted.median(value, pop75))
  


# Average across communes
df %>% dplyr::select(commune,MR_all_cause,pop75_share,pop75,
                              pm25_exposure,landTemp) %>% 
  group_by(commune) %>% 
  summarise_all(list(mean)) %>% ungroup() %>% 
  pivot_longer(c(-commune,-pop75), names_to = "key", values_to = "value") %>% 
  # mutate(mult=pop75*value) %>% 
  group_by(key) %>% 
  reframe(mean=weighted.mean(value,pop75))
  # reframe(sumCum=sum(mult),pop75=sum(pop75)) %>% ungroup() %>% mutate(mean=sumCum/pop75)
  

weighted.mean(df$pm25_exposure,df$pop75,na.rm=T)


# Geom rdiges --------

library(ggridges)
theme_set(theme_minimal())
df_month <- df %>% group_by(commune,month) %>% 
  reframe(pm25_exposure=mean(pm25_exposure),
          # landTemp=mean(landTemp),
          MR_all_cause=mean(MR_all_cause)) %>% ungroup() %>% 
  mutate(month_name=month.abb[month]) %>% 
  pivot_longer(c(pm25_exposure,MR_all_cause), names_to = "key", values_to = "value")
  

ggplot(df_month,aes(x = value, y = month_name, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = lab_pm25, option = "C")+
  labs(y="",x=lab_pm25)+
  facet_wrap(~key,scales = "free_x")+
  scale_y_discrete(limits = month.abb[1:12])+
  theme_ridges(grid = T, center_axis_labels = TRUE)+
  theme(legend.position = "none")

ggsave("Figures/Month_dist.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)

df_year <- df %>% group_by(commune,year) %>% 
  reframe(pm25_exposure=mean(pm25_exposure)) %>% ungroup()

ggplot(df_year,aes(x = pm25_exposure, y = year, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = lab_pm25, option = "C")+
  labs(y="",x=lab_pm25)+
  theme(legend.position = "none")

ggsave("Figures/Year_dist.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)




# EoF