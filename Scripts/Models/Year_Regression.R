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

write.csv(all_mods,"Data/Models/modelResults_year.csv",row.names = F)
all_mods <- read.csv("Data/Models/modelResults_year.csv")

# Figure ------

param_int <- "pm25Exp_10ug"
# param_int <- "landTemp"  # temp

df_fig <- all_mods %>% 
  filter(param==param_int)

rr_base <-  getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% pull(rr)
rr_ci <- getModelInfo(mod_base,"Base") %>% 
  filter(param==param_int) %>% dplyr::select(rr_low,rr_high)

# Figure
df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  mutate(year=as.numeric(name)) %>% 
  ggplot(aes(x = year, y = rr)) +
  # base RR
  geom_hline(yintercept = rr_base, linetype="dashed",col="brown",linewidth=0.5)+
  geom_rect(xmin=2001,xmax=2020,ymin = as.numeric(rr_ci[1]), ymax = as.numeric(rr_ci[2]), 
            fill = "brown",alpha=0.01)+
  # by year
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  geom_point(size=1, aes(col=signif)) +
  # geom_point(size=1,col="red")+ # all T are significant
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  scale_color_manual(values = c("black", "red"), labels = c(F, T))+
  scale_x_continuous(breaks = c(2002, 2005, 2010, 2015, 2019)) +
  # annotation
  annotate("text", x = 2017.5, y = rr_base+1.5, label = "Pooled estimate",size=8*5/14 * 0.8) +
  geom_segment(aes(x = 2018, y = rr_base+1.2, xend = 2019, yend = rr_base+0.2),
               arrow = arrow(length = unit(0.3, "cm"))) +
  # annotate("text", x = 2002, y = 8, size=14*5/14 * 0.8,label = "A")+
  # temp - uncomment
  # annotate("text", x = 2017.5, y = rr_base+0.7, label = "Pooled estimate",size=8*5/14 * 0.8) +
  # geom_segment(aes(x = 2018, y = rr_base+0.6, xend = 2018, yend = rr_base+0.05),
  #              arrow = arrow(length = unit(0.3, "cm"))) +
  # annotate("text", x = 2002, y = 0.3, size=14*5/14 * 0.8,label = "A")+
  labs(x = "Year",y = lab_rr)+
  # labs(x="Year",y=expression(paste("Percentage change in Mortality rate by 1° Celsius")))+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
fig_name <- "YearModels"
# fig_name <- "YearModels_Temp"

ggsave(paste0("Figures/Model/",fig_name,".png"), ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# PM2.5 pollution by year (pop weighted average)

df %>% 
  mutate(pm25_pop=pm25_exposure*pop75) %>% 
  group_by(year) %>%
  reframe(pm25_pop=sum(pm25_pop),
          death=sum(death_count_all_cause),
          pop75=sum(pop75)) %>% 
  mutate(pm25=pm25_pop/pop75,
         MR=death/pop75*1000) %>% 
  dplyr::select(-pm25_pop)



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

# Which regions have higher PM2.5 in January 2017?
df %>% 
  filter(month %in% c(1)) %>% # January
  mutate(y2017=if_else(year==2017,"y2017","Other_years")) %>% 
  group_by(REGION,y2017) %>% 
  summarise(pm25=mean(pm25_exposure)) %>% 
  pivot_wider(names_from = y2017, values_from = pm25) %>% 
  mutate(relative_diff=(y2017-Other_years)/Other_years*100) 

# regions with severe burning  - more than 70% increase in January 2017
regs_burning <- c(4,5,6,7,8,13,16)

mods_2017 <- data.frame()
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, region %in% regs_burning)) # filter by year
out <- getModelInfo(mod,"2017 Region IV, V, M, \n VI, VII, VIII, XVI",
                    data_df =filter(df,year==2017, region %in% regs_burning))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, !(region %in% regs_burning))) # filter by year
out <- getModelInfo(mod,"2017 Rest of Country",
                    data_df =filter(df,year==2017, !(region %in% regs_burning)))
mods_2017 <- rbind(mods_2017,out)


# all years or pooled
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,region %in% regs_burning))
out <- getModelInfo(mod,"Region IV, V, M, \n VI, VII, VIII, XVI",
                    data_df =filter(df,year!=2017,region %in% regs_burning))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,!(region %in% regs_burning)))
out <- getModelInfo(mod,"Rest of Country",
                    data_df =filter(df,year!=2017,!(region %in% regs_burning)))
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
  coord_flip()+
  labs(x = "",
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/YearModels2017.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# By commune
# which communes? 
comunes_burning <- df %>% 
  filter(month %in% c(1)) %>% # January
  mutate(y2017=if_else(year==2017,"y2017","Other_years")) %>% 
  group_by(REGION,commune,y2017) %>% 
  summarise(pm25=mean(pm25_exposure)) %>% 
  pivot_wider(names_from = y2017, values_from = pm25) %>% ungroup() %>% 
  mutate(relative_diff=(y2017-Other_years)/Other_years*100) %>% 
  arrange(desc(relative_diff)) %>% 
  filter(relative_diff>100) %>%  # at least PM2.5 doubled in January
  pull(commune)
length(comunes_burning) #141

df %>% 
  filter(month %in% c(1)) %>% # January
  mutate(y2017=if_else(year==2017,"y2017","Other_years")) %>% 
  group_by(REGION,commune,y2017) %>% 
  summarise(pm25=mean(pm25_exposure)) %>% 
  pivot_wider(names_from = y2017, values_from = pm25) %>% ungroup() %>% 
  mutate(relative_diff=(y2017-Other_years)/Other_years*100) %>% 
  arrange(desc(relative_diff)) %>% 
  ggplot(aes(relative_diff))+stat_ecdf(aes(y = ..y..*327)) +
  labs(x="Relative difference % of January 2017 w.t.r other Januaries",
       y="Number of communes")


# which regions got more communes?
df %>% 
  group_by(region,commune) %>% tally() %>% ungroup() %>% 
  filter(commune %in% comunes_burning) %>% 
  group_by(region) %>% tally()

mods_2017 <- data.frame()
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, commune %in% comunes_burning)) # filter by year
out <- getModelInfo(mod,"2017 Communes \n affected by burning",
                    data_df =filter(df,year==2017, commune %in% comunes_burning))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year==2017, !(commune %in% comunes_burning))) # filter by year
out <- getModelInfo(mod,"2017 Rest of Country",
                    data_df =filter(df,year==2017, !(commune %in% comunes_burning)))
mods_2017 <- rbind(mods_2017,out)

# all years or pooled
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,commune %in% comunes_burning))
out <- getModelInfo(mod,"Communes affected \n by burning",
                    data_df =filter(df,year!=2017,commune %in% comunes_burning))
mods_2017 <- rbind(mods_2017,out)

# rest
mod <- glm.nb(death_count_all_cause ~ pm25Exp_10ug+landTemp+commune+year_quarter+
                offset(log(pop75)), na.action=na.omit,
              data = filter(df,year!=2017,!(commune %in% comunes_burning)))
out <- getModelInfo(mod,"Rest of Country",
                    data_df =filter(df,year!=2017,!(commune %in% comunes_burning)))
mods_2017 <- rbind(mods_2017,out)



df_fig <- mods_2017 %>% 
  mutate(name=str_replace(name,"Communes affected","Communes \n affected")) %>% 
  filter(param=="pm25Exp_10ug")

df_fig

df_fig %>%
  mutate(signif=sign(rr_low)==sign(rr_high)) %>%  # significant at 5%
  ggplot(aes(x = name, y = rr)) +
  geom_linerange(aes(ymin = rr_low, ymax = rr_high), linewidth = 0.2) +
  # geom_point(size=1, aes(col=signif)) +
  geom_point(size=1,col="red")+
  geom_hline(yintercept = 0, linetype="dashed",col="grey",linewidth=0.5)+
  coord_flip()+
  labs(x = "",caption="Communes affected by burning: >100% of PM2.5 \n increase in January 2017 w.r.t other Januaries",
       y = lab_rr)+
  theme_bw(10)+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Figures/Model/YearModels2017_commune.png", ggplot2::last_plot(),
       units="cm",dpi=500,
       width=8.7,height=8.7)

# EoF