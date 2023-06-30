## Regressions at Year regression Level
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
yrs <- df$year %>% unique()
all_mods <- data.frame()

# one model for each commune
for (x in yrs){
  mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                  offset(log(pop75)), na.action=na.omit,
                data = filter(df,year==x)) # filter by year
  
  out <- getModelInfo(mod,x,data_df =filter(df,year==x))
  all_mods <- rbind(all_mods,out)
  rm(out)
}

# Figure ------

df_fig <- all_mods %>% 
  filter(param=="pm25Exp_10ug")

df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  mutate(year=as.numeric(name)) %>% 
  ggplot(aes(x = year, y = rr)) +
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  scale_x_continuous(breaks = c(2002, 2005, 2010, 2015, 2019)) +
  # annotation
  annotate("text", x = 2017.5, y = rr_base+1.5, label = "Pooled estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 2018, y = rr_base+1.2, xend = 2019, yend = rr_base+0.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x = "",
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures//Model/YearModels.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# PM2.5 pollution by year (pop weighted average)

df %>% 
  mutate(pm25_pop=pm25_exposure*pop75) %>% 
  group_by(year) %>%
  reframe(pm25_pop=sum(pm25_pop),
          pop75=sum(pop75)) %>% 
  mutate(pm25=pm25_pop/pop75)

# df %>% 
#   group_by(year) %>% 
#   reframe(pm25=mean(pm25_exposure),
#           pm25_sd=sd(pm25_exposure))

# check quarter effects
names(all_mods)
all_mods %>% 
  filter(str_detect(param,"year")) %>% 
  mutate(qt=str_sub(param, start= 18)) %>% 
  dplyr::select(name,qt,est) %>% 
  # pivot_wider(names_from = qt, values_from = est)
  ggplot(aes(name,est,col=qt))+geom_point()

# Year 2017 Analysis in detail -------


df$region %>% table

mods_2017 <- data.frame()
# regions with severe burning 6 to 8 (Rancagua to Concepcion)
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, region %in% c(6,7,8))) # filter by year
out <- getModelInfo(mod,"2017 Region VI, VII, VIII",
                    data_df =filter(df,year==2017, region %in% c(6,7,8)))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, !(region %in% c(6,7,8)))) # filter by year
out <- getModelInfo(mod,"2017 Rest of Country",
                    data_df =filter(df,year==2017, !(region %in% c(6,7,8))))
mods_2017 <- rbind(mods_2017,out)


# all years or pooled
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,region %in% c(6,7,8))) 
out <- getModelInfo(mod,"Region VI, VII, VIII",
                    data_df =filter(df,year!=2017,region %in% c(6,7,8)))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,!(region %in% c(6,7,8)))) 
out <- getModelInfo(mod,"Rest of Country",
                    data_df =filter(df,year!=2017,!(region %in% c(6,7,8))))
mods_2017 <- rbind(mods_2017,out)

df_fig <- mods_2017 %>% 
  filter(param=="pm25Exp_10ug")

df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = name, y = rr)) +
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  # geom_point(size=1, aes(col=signif)) +
  geom_point(size=1,col="red")+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  # geom_hline(yintercept = rr_base, linetype="dashed",col="red",linewidth=0.5)+
  # scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  # scale_x_continuous(breaks = c(2002, 2005, 2010, 2015, 2019)) +
  # annotation
  # annotate("text", x = 2017.5, y = rr_base+1.5, label = "Pooled estimate",size=8*5/14 * 0.8) +
  # geom_segment(aes(x = 2018, y = rr_base+1.2, xend = 2019, yend = rr_base+0.2),
  #              arrow = arrow(length = unit(0.3, "cm"))) +
  coord_flip()+
  labs(x = "",
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures//Model/YearModels2017.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# EoF