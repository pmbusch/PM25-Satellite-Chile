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

write.csv(all_mods,"Data/Models/modelResults_region.csv",row.names = F)
all_mods <- read.csv("Data/Models/modelResults_region.csv")


# Figure ------

param_int <- "pm25Exp_10ug"
# param_int <- "landTemp"  # temp

df_fig <- all_mods %>% 
  filter(param==param_int)

rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% pull(rr)
rr_ci <- getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% dplyr::select(rr_low,rr_high)

region_levels <- c("15","1","2","3","4","5","M","6","7","16","8","9","14","10","11","12")
region_levels2 <- c("XV","I","II","III","IV","V","M","VI","VII","XVI","VIII","IX","XIV","X","XI","XII")

reg_codes <- data.frame(name=region_levels, region=region_levels2)

# Change regions name
df_fig <- df_fig %>%
  mutate(name=name %>% str_replace("13","M")) %>% 
  left_join(reg_codes, by="name") %>% 
  mutate(region=factor(region,levels=rev(region_levels2)))

# Figure
df_fig %>%
  mutate(rr_base_low=rr_ci[1],rr_base_high=rr_ci[2]) %>% 
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = region, y = rr)) +
  # base model
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=0,xmax=17,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  coord_flip()+
  # coord_flip(ylim = c(-8,10))+
  scale_y_continuous(breaks = seq(-10, 15, by = 5))+
  # scale_y_continuous(breaks = seq(-3, 0, by = 1))+ #temp
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # annotation
  annotate("text", x = 1, y = rr_base+7.5, label = "Pooled estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 1, y = rr_base+3.2, xend = 1, yend = rr_base+1.3),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text", x = 16, y = -12, size=14*5/14 * 0.8,label = "B")+
  # temp - uncomment
  # annotate("text", x = 5, y = rr_base-1, label = "Pooled \n estimate",size=8*5/14 * 0.8) +
  # geom_segment(aes(x = 5, y = rr_base-1.2, xend = 5, yend = rr_base-0.3),
  #              arrow = arrow(length = unit(0.3, "cm"))) +
  # annotate("text", x = 16, y = -3, size=14*5/14 * 0.8,label = "B")+
  labs(x = "Region",y =lab_rr)+
  # labs(x="Region",y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  theme_bw(8.3)+
  theme(legend.position = "none",
        # axis.title.y = element_text(size = 1),
        axis.title.y=element_text(angle=0,vjust = -0.05,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

fig_name <- "RegionModels"
# fig_name <- "RegionModelsTemp"
ggsave(paste0("Figures/Model/",fig_name,".png"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)


# Average effect of sign. regions
df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>% 
  group_by(signif) %>% 
  summarise(rr=mean(rr)) #4.8%


# Average exposure by region
df_stats <- df %>% 
  mutate(region=region %>% str_replace("13","M")) %>%
  left_join(reg_codes, by=c("region"="name")) %>% 
  mutate(region=factor(region,levels=rev(region_levels2))) %>% 
  mutate(pm25_pop=pop75*pm25_exposure) %>% 
  group_by(region.y) %>% 
  summarise(pm25=sum(pm25_pop),
            pop75=sum(pop75),
            death=sum(death_count_all_cause),
            n=n()) %>% ungroup() %>% 
  rename(region=region.y) %>% 
  mutate(pm25=pm25/pop75,
         mr=death/pop75*1000,
         pop75=pop75/18/12) # pop in total

## IDEA: Create table as other sub samples

df_fig %>%
  # dplyr::select(-region) %>% 
  # rename(region=name) %>% 
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

# EoF