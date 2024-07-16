# Age of Death time Series Analysis
# Figure S8
# PBH September 2023


library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

age_death <- read.csv("Data/chile_all_cause_mortality_count_mean_age_commune_level_year_1990_2019.csv")

age_death$Year %>% unique()
age_death <- age_death %>% filter(Age.group=="75+",Year>2001) %>% 
  rename(commune=CODIGO_COMUNA_RESIDENCIA )


# overall increase
age_death %>% group_by(Year,Gender) %>% 
  summarise(Mean_age=weighted.mean(Mean_age,Death_count_all_cause)) %>% 
  # pivot_wider(names_from = Gender, values_from = Mean_age)
  mutate(sex=if_else(Gender=="Hombre","Male","Female")) %>% 
  ggplot(aes(Year,Mean_age,col=sex))+
  geom_line()+
  scale_x_continuous(breaks = c(2002, 2005, 2010, 2015, 2019)) +
  scale_y_continuous(breaks=83:86)+
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x="",y="Mean age of death for 75+ group",col="")+
  theme_bw(10)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.6))

ggsave("Figures/AgeDeath_national.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7,height=8.7)

pdf("Figures/FigureS8.pdf", 
       width=8.7/2.54,height=8.7/2.54)
ggplot2::last_plot()
dev.off()



# Year variations -----
library(ggridges)
age_death %>% 
  mutate(sex=if_else(Gender=="Hombre","Male","Female")) %>% 
  mutate(Year=factor(Year,levels=2019:2002)) %>% 
  ggplot(aes(x = Mean_age, y = Year,fill=sex)) +
  # geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  # scale_fill_viridis_c(name = "Age of death for 75+ age group", option = "C") +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5,alpha=0.7)+
  scale_y_discrete(breaks = c(2002, 2005, 2010, 2015, 2019))+
  coord_cartesian(xlim=c(80,90),expand = F)+ 
  scale_x_continuous(breaks=seq(80,90,2))+
  # facet_wrap(~sex,ncol=2)+
  labs(x = "Age of death for 75+ group",y="",fill="")+
  # guides(fill=guide_legend(nrow = 1))+
  theme_bw(8)
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
  #       legend.position = c(0.9,0.9))

ggsave("Figures/AgeDeath_monthly.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)

## Boxplot

age_death %>% 
  mutate(sex=if_else(Gender=="Hombre","Male","Female")) %>% 
  mutate(Year=factor(Year,levels=2002:2019)) %>% 
  ggplot(aes(x = Mean_age, y = Year,fill=sex)) +
  geom_boxplot(alpha=.7)+
  scale_y_discrete(breaks = c(2002, 2005, 2010, 2015, 2019))+
  coord_flip(xlim=c(80,90),expand = F)+ 
  scale_x_continuous(breaks=seq(80,90,2))+
  # facet_wrap(~sex,ncol=2)+
  labs(x = "Age of death for 75+ group",y="",fill="")+
  # guides(fill=guide_legend(nrow = 1))+
  theme_bw(8)
# theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
#       legend.position = c(0.9,0.9))

ggsave("Figures/AgeDeath_monthly_boxplot.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=8.7)



# EoF