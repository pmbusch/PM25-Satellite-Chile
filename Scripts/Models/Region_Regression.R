## Regressions at Region regression Level
## PBH
## June 2023

library(tidyverse)
library(MASS)
library(lme4)
library(sandwich)


theme_set(theme_bw(16)+ theme(panel.grid.major = element_blank()))

source("Scripts/Functions.R",encoding="UTF-8")

# Load required data -----
df <- read.delim("Data/panelData.csv",sep=";")

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

# Base model ----
mod_base <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                     offset(log(pop75)), na.action=na.omit,
                   data = df) 
rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param=="pm25Exp_10ug") %>% pull(rr)

# Loop --------
# year
regs <- df$region %>% unique()
all_mods <- data.frame()

# one model for each commune
for (x in regs){
  mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                  offset(log(pop75)), na.action=na.omit,
                data = filter(df,region==x)) # filter by region
  out <- getModelInfo(mod,x,data_df =filter(df,region==x))
  all_mods <- rbind(all_mods,out)
  rm(out)
}

# Figure ------

df_fig <- all_mods %>% 
  filter(param=="pm25Exp_10ug")

region_levels <- c("15","1","2","3","4","5","M","6","7","8","16","9","14","10","11","12")

df_fig %>%
  mutate(name=name %>% str_replace("13","M")) %>% 
  mutate(region=factor(name,levels=rev(region_levels))) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = region, y = rr)) +
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_hline(yintercept = rr_base, linetype="dashed",col="red",linewidth=0.5)+
  coord_flip()+
  # coord_flip(ylim = c(-10,10))+
  # scale_y_continuous(breaks = seq(-12.5, 18, by = 2.5))+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 1, y = rr_base+7, label = "Pooled estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 1, y = rr_base+3.2, xend = 1, yend = rr_base+1.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x = "",y =lab_rr)+
  theme_bw(9)+
  theme(legend.position = "none",
        # axis.title.y = element_text(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures//Model/RegionModels.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# Average exposure by region
region_levels <- c("15","1","2","3","4","5","13","6","7","8","16","9","14","10","11","12")
df_stats <- df %>% 
  mutate(region=factor(region,levels=region_levels)) %>% 
  mutate(pm25_pop=pop75*pm25_exposure) %>% 
  group_by(region) %>% 
  summarise(pm25=sum(pm25_pop),
            pop75=sum(pop75),
            n=n()) %>% ungroup() %>% 
  mutate(pm25=pm25/pop75,
         pop75=pop75/18/12) # pop in total

## IDEA: Create table as other sub samples

df_fig %>%
  rename(region=name) %>% 
  left_join(df_stats) %>% 
  mutate(region=paste0("R",region)) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = pm25, y = rr)) +
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif))+
  ggrepel::geom_text_repel(aes(label=region),size=6*5/14 * 0.8)+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_hline(yintercept = rr_base, linetype="dashed",col="red",linewidth=0.5)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 8, y = rr_base+7, label = "Pooled estimate",size=8*5/14 * 0.8, hjust=0) +
  geom_segment(aes(x = 10, y = rr_base+5.2, xend = 10, yend = rr_base+1.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x =lab_pm25,
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/RegionModelsPM25.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)
