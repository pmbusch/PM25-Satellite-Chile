##  Figure - Show Demeaning effect of PM2.5 on Mortality 75+ in Chile
## PBH
## June 2023

library(tidyverse)

theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))


# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")


# Summary Statistics -----

weighted.mean(df$pm25_exposure,df$pop75)
weighted.mean(df$landTemp,df$pop75)
sum(df$death_count_all_cause)
sum(df$death_count_all_cause)/18 # nationally by year
sum(df$death_count_all_cause)/(nrow(df)) # average by commune per month


# Manipulate data - Remove Means -----
# see: https://stats.stackexchange.com/questions/409755/demeaning-with-two-n-fixed-effects-in-panel-regressions/565199#565199

df <- df %>% 
  group_by(commune) %>% # Mean by Commune
  mutate(c_mean_mr=mean(MR_all_cause),
         c_mean_pm25=mean(pm25_exposure)) %>% ungroup() %>% 
  group_by(year,quarter) %>% # Mean by Year-Quarter
  mutate(y_mean_mr=mean(MR_all_cause),
         y_mean_pm25=mean(pm25_exposure)) %>% ungroup() %>% 
  mutate(all_mean_mr=mean(MR_all_cause), # overall mean
         all_mean_pm25=mean(pm25_exposure)) %>% 
  mutate(diff_mr=MR_all_cause-c_mean_mr-y_mean_mr+all_mean_mr,
         diff_pm25=pm25_exposure-c_mean_pm25-y_mean_pm25+all_mean_pm25)


cor(df$diff_mr,df$diff_pm25)

range(df$diff_pm25)
breaks <- seq(-25, 50, by = 10) # by 
breaks[8]=50 # change end
labels <- paste0(breaks[-length(breaks)], " to ", breaks[-1])
# Create the new categorical variable
df <- df %>%
  mutate(pm25_category = cut(diff_pm25, 
                             breaks = breaks, 
                             labels = labels, right = FALSE))

# Figure -----
p_dem <- df %>% 
  mutate(met=if_else(REGION=="13","Metropolitan Region","Rest of Country")) %>% 
  # sample_n(1000) %>% 
  ggplot(aes(pm25_category,diff_mr))+
  # geom_point(alpha=0.5, aes(fill=met))+
  # geom_violin(aes(fill=met)) +
  # geom_boxplot(aes(fill=met),outlier.size = 0.1,linewidth=0.1)+
  geom_boxplot(outlier.size = 0.1,linewidth=0.1)+ # no met
  geom_hline(yintercept = 0,linetype="dashed")+
  scale_fill_manual(values = c("#9b59b680", "#2ecc7180")) +
  labs(y="75+ Mortality Rate All-Cause \n deviation* [per 1,000]",
       fill="",
       caption="(*): Commune and Year-Quarter Fixed Effects removed",
       x=expression(paste("",PM[2.5] ," Exposure deviation* [",mu,"g/",m^3,"]","")))+
  # ylim(-10,10)+
  theme_bw(8)+
  theme(legend.position = c(0.78,0.78),
        legend.text = element_text(size = 6),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_dem

# ggsave("Figures/MR_PM25_met.png", 
ggsave("Figures/MR_PM25.png",
       ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# Other -----

df_1719 <- df %>% 
  filter(year %in% c(2017,2018,2019)) %>% 
  group_by(commune) %>% 
  summarise(pm25=mean(pm25_exposure),
            temp=mean(landTemp))
head(df_1719)

write.table(df_1719,"Data/pm25_2017_2019.csv",sep = ";",row.names = F)

